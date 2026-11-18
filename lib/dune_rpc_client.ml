(** Dune RPC client speaking the dune 3.20 csexp RPC protocol, using Eio.

    The protocol is a two-step handshake followed by long-polling:

    1. [initialize]    -- establish the session
    2. [version_menu]  -- negotiate the version of each method
    3. long-poll [poll/progress] and [poll/diagnostic] to observe the build

    Every message is a csexp "packet": a record with optional fields
    [id], [result], [method] and [params]. A request carries [id]/[method]/[params],
    a response carries [id]/[result], a notification carries [method]/[params]. *)

open Eio

(* Setup logging *)
let src = Logs.Src.create "dune-rpc-client" ~doc:"Dune RPC Client logging"

module Log = (val Logs.src_log src : Logs.LOG)

(* Request ID counter for the handshake requests. *)
module Request_id = struct
  type t = { mutable counter : int; mutex : Eio.Mutex.t }

  let create () = { counter = 0; mutex = Eio.Mutex.create () }

  let next t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        let id = t.counter in
        t.counter <- t.counter + 1;
        Printf.sprintf "dune-rpc-%d" id)
end

(* Registry polling module following OCaml LSP pattern *)
module Registry_poll = struct
  type instance = {
    root : string;
    pid : int option; [@warning "-69"]
    socket : string option;
  }

  let read_file ~fs path =
    try
      let content = Eio.Path.load Eio.Path.(fs / path) in
      Ok content
    with
    | Eio.Io (Eio.Fs.E (Not_found _), _) -> Error "File not found"
    | exn -> Error (Printexc.to_string exn)

  let parse_registry_entry content =
    try
      match Csexp.parse_string content with
      | Ok (Csexp.List [ Csexp.Atom "3"; Csexp.List fields ]) ->
          (* Version 3 format *)
          let root = ref None in
          let pid = ref None in
          List.iter
            (function
              | Csexp.List [ Csexp.Atom "root"; Csexp.Atom r ] -> root := Some r
              | Csexp.List [ Csexp.Atom "pid"; Csexp.Atom p ] -> (
                  try pid := Some (int_of_string p) with
                  | Failure _ -> () (* Invalid integer format *)
                  | Invalid_argument _ -> ())
              | _ -> ())
            fields;
          Option.map (fun r -> { root = r; pid = !pid; socket = None }) !root
      | Ok
          (Csexp.List
             [
               Csexp.Atom "2";
               Csexp.List [ Csexp.Atom "root"; Csexp.Atom root ];
               Csexp.List [ Csexp.Atom "pid"; Csexp.Atom pid_str ];
             ]) ->
          (* Version 2 format *)
          let pid =
            try Some (int_of_string pid_str)
            with Failure _ | Invalid_argument _ -> None
          in
          Some { root; pid; socket = None }
      | _ -> None
    with
    | Csexp.Parser.Parse_error _ -> None
    | Invalid_argument _ -> None
    | _ -> None

  let poll ~fs registry_path =
    try
      let entries = Eio.Path.read_dir Eio.Path.(fs / registry_path) in
      let instances = ref [] in
      List.iter
        (fun entry ->
          if
            (not (String.starts_with ~prefix:"." entry))
            && not (String.ends_with ~suffix:".socket" entry)
          then
            let entry_path = Filename.concat registry_path entry in
            match read_file ~fs entry_path with
            | Ok content -> (
                match parse_registry_entry content with
                | Some inst ->
                    let socket_path =
                      Filename.concat registry_path (entry ^ ".socket")
                    in
                    let inst =
                      try
                        ignore (Eio.Path.load Eio.Path.(fs / socket_path));
                        { inst with socket = Some socket_path }
                      with
                      | Eio.Io (Eio.Fs.E (Not_found _), _) -> inst
                      | _ -> inst
                    in
                    instances := inst :: !instances
                | None -> ())
            | Error _ -> ())
        entries;
      Ok !instances
    with
    | Eio.Io (Eio.Fs.E (Not_found _), _) -> Ok []
    | exn -> Error (Printexc.to_string exn)
end

type diagnostic = {
  file : string;
  line : int;
  column : int;
  message : string;
  severity : [ `Error | `Warning ];
}

type progress =
  | Waiting
  | In_progress of { complete : int; remaining : int; failed : int }
  | Failed
  | Interrupted
  | Success

type session = {
  flow : Eio.Flow.two_way_ty Eio.Resource.t;
  buf_read : Buf_read.t;
  (* Session identifier. Kept as a plain string so the client does not depend
     on a specific dune-rpc [Id] API (which moved/changed between versions). *)
  id : string;
  request_id : Request_id.t;
}

type connection_state = Disconnected | Connected of session | Closed

type t = {
  sw : Switch.t;
  root : string;
  mutable state : connection_state;
  registry_path : string option;
  diagnostics : (string, diagnostic list) Hashtbl.t;
  mutable progress : progress;
  mutex : Eio.Mutex.t;
  env : Eio_unix.Stdenv.base;
  mutable last_poll_error : string option;
  (* Long-poll request ids and the poller (subscription) ids. These are only
     ever touched by the single [run] fiber, so they need no locking. *)
  mutable progress_rid : Csexp.t;
  mutable diagnostic_rid : Csexp.t;
  mutable next_poll_id : int;
}

let default_registry_path () =
  match Sys.getenv_opt "XDG_RUNTIME_DIR" with
  | Some dir -> Some (Filename.concat dir "dune-rpc")
  | None -> (
      match Sys.getenv_opt "HOME" with
      | Some home -> Some (Filename.concat home ".cache/dune/rpc")
      | None -> None)

let create ~sw ~env ~root =
  let registry_path = default_registry_path () in
  let diagnostics = Hashtbl.create 16 in
  let mutex = Eio.Mutex.create () in
  {
    sw;
    root;
    state = Disconnected;
    registry_path;
    diagnostics;
    progress = Waiting;
    mutex;
    env;
    last_poll_error = None;
    progress_rid = Csexp.Atom "poll-progress-0";
    diagnostic_rid = Csexp.Atom "poll-diagnostic-0";
    next_poll_id = 0;
  }

(* Find matching Dune instance for our root *)
let find_matching_instance instances root =
  (* First try exact match *)
  match
    List.find_opt (fun inst -> inst.Registry_poll.root = root) instances
  with
  | Some _ as result -> result
  | None ->
      (* Then try parent directories *)
      List.find_opt
        (fun inst ->
          let inst_root = inst.Registry_poll.root in
          String.starts_with ~prefix:inst_root root
          && (String.length root = String.length inst_root
             || root.[String.length inst_root] = '/'))
        instances

(* Write a Csexp to the buffer *)
let write_csexp buf_write csexp =
  let s = Csexp.to_string csexp in
  Buf_write.string buf_write s

(* Read a Csexp from the buffer using the Parser module for robust parsing.
   Blocks (yields) until a complete sexp is available; returns an error only
   when the connection is closed. *)
let read_csexp buf_read =
  let module Lexer = Csexp.Parser.Lexer in
  let module Stack = Csexp.Parser.Stack in
  let lexer = Lexer.create () in
  let rec loop stack =
    match Buf_read.peek_char buf_read with
    | None ->
        Lexer.feed_eoi lexer;
        Error "Unexpected end of input"
    | Some _ -> (
        let c = Buf_read.any_char buf_read in
        let token = Lexer.feed lexer c in
        match token with
        | Atom n -> (
            (* Read n bytes for the atom *)
            let atom_bytes = Buf_read.take n buf_read in
            let stack = Stack.add_atom atom_bytes stack in
            match stack with Sexp (sexp, Empty) -> Ok sexp | _ -> loop stack)
        | (Lparen | Rparen | Await) as token -> (
            let stack = Stack.add_token token stack in
            match stack with Sexp (sexp, Empty) -> Ok sexp | _ -> loop stack))
  in

  try loop Stack.Empty with
  | End_of_file -> Error "Connection closed"
  | exn -> Error (Printf.sprintf "Parse error: %s" (Printexc.to_string exn))

(* ---------------------------------------------------------------------------
   Protocol helpers
   ------------------------------------------------------------------------- *)

(* Compare two csexp values for equality via their canonical string form. The
   csexp library does not expose an [equal] function. *)
let same_csexp a b = Csexp.to_string a = Csexp.to_string b

(* Allocate a fresh long-poll request id. Only called from the [run] fiber. *)
let alloc_poll_id t prefix =
  let n = t.next_poll_id in
  t.next_poll_id <- n + 1;
  Csexp.Atom (Printf.sprintf "%s-%d" prefix n)

(* Stable poller (subscription) ids, sent as the params of poll requests. *)
let progress_poller_id = Csexp.Atom "ocaml-mcp-progress"
let diagnostic_poller_id = Csexp.Atom "ocaml-mcp-diagnostic"

(* Send a request packet: ((id ID) (method M) (params P)). *)
let send_request session id method_ params =
  let packet =
    Csexp.List
      [
        Csexp.List [ Csexp.Atom "id"; id ];
        Csexp.List [ Csexp.Atom "method"; Csexp.Atom method_ ];
        Csexp.List [ Csexp.Atom "params"; params ];
      ]
  in
  Buf_write.with_flow session.flow (fun w ->
      write_csexp w packet;
      Buf_write.flush w)

(* The methods and versions this client advertises during version negotiation.
   The server replies with the greatest common version for each method. *)
let supported_menu =
  Csexp.List
    [
      Csexp.List
        [ Csexp.Atom "poll/progress"; Csexp.List [ Csexp.Atom "1"; Csexp.Atom "2" ] ];
      Csexp.List
        [ Csexp.Atom "poll/diagnostic"; Csexp.List [ Csexp.Atom "1"; Csexp.Atom "2" ] ];
      Csexp.List
        [ Csexp.Atom "diagnostics"; Csexp.List [ Csexp.Atom "1"; Csexp.Atom "2" ] ];
      Csexp.List [ Csexp.Atom "ping"; Csexp.List [ Csexp.Atom "1" ] ];
      Csexp.List [ Csexp.Atom "build_dir"; Csexp.List [ Csexp.Atom "1" ] ];
      Csexp.List
        [ Csexp.Atom "cancel-poll/progress"; Csexp.List [ Csexp.Atom "1" ] ];
      Csexp.List
        [ Csexp.Atom "cancel-poll/diagnostic"; Csexp.List [ Csexp.Atom "1" ] ];
      Csexp.List [ Csexp.Atom "shutdown"; Csexp.List [ Csexp.Atom "1" ] ];
    ]

(* Perform the two-step handshake: [initialize] then [version_menu]. On success
   returns a ready-to-use session. *)
let initialize_session ~flow ~buf_read =
  let id = "ocaml-mcp" in
  let request_id = Request_id.create () in
  let session_id = Csexp.Atom "ocaml-mcp" in

  (* Step 1: initialize *)
  let init_params =
    Csexp.List
      [
        Csexp.List
          [
            Csexp.Atom "dune_version";
            Csexp.List [ Csexp.Atom "3"; Csexp.Atom "20" ];
          ];
        Csexp.List [ Csexp.Atom "protocol_version"; Csexp.Atom "0" ];
        Csexp.List [ Csexp.Atom "id"; session_id ];
      ]
  in
  let init_packet =
    Csexp.List
      [
        Csexp.List [ Csexp.Atom "id"; Csexp.Atom "initialize" ];
        Csexp.List [ Csexp.Atom "method"; Csexp.Atom "initialize" ];
        Csexp.List [ Csexp.Atom "params"; init_params ];
      ]
  in
  Buf_write.with_flow flow (fun w ->
      write_csexp w init_packet;
      Buf_write.flush w);

  (* Read the initialize response, then send the version negotiation. *)
  match read_csexp buf_read with
  | Ok (Csexp.List [ _; Csexp.List [ Csexp.Atom "result"; Csexp.List [ Csexp.Atom "ok"; _ ] ] ])
    ->
      let vn_packet =
        Csexp.List
          [
            Csexp.List [ Csexp.Atom "id"; Csexp.Atom "version-menu" ];
            Csexp.List [ Csexp.Atom "method"; Csexp.Atom "version_menu" ];
            Csexp.List [ Csexp.Atom "params"; supported_menu ];
          ]
      in
      Buf_write.with_flow flow (fun w ->
          write_csexp w vn_packet;
          Buf_write.flush w);
      match read_csexp buf_read with
      | Ok (Csexp.List [ _; Csexp.List [ Csexp.Atom "result"; Csexp.List [ Csexp.Atom "ok"; _ ] ] ])
        -> Ok { flow; buf_read; id; request_id }
      | Ok sexp ->
          Error
            (Printf.sprintf "Invalid version_menu response: %s" (Csexp.to_string sexp))
      | Error msg -> Error msg
  | Ok (Csexp.List [ _; Csexp.List [ Csexp.Atom "result"; Csexp.List [ Csexp.Atom "error"; err ] ] ])
    -> Error (Printf.sprintf "Initialize failed: %s" (Csexp.to_string err))
  | Ok sexp ->
      Error (Printf.sprintf "Invalid initialize response: %s" (Csexp.to_string sexp))
  | Error msg -> Error msg

(* Connect to a Dune instance *)
let connect_to_instance ~sw ~net instance =
  match instance.Registry_poll.socket with
  | None -> Error "No socket path available"
  | Some socket_path -> (
      try
        let addr = `Unix socket_path in
        let socket = Eio.Net.connect ~sw net addr in
        let flow = (socket :> Eio.Flow.two_way_ty Eio.Resource.t) in
        let buf_read = Buf_read.of_flow ~max_size:Int.max_int flow in
        match initialize_session ~flow ~buf_read with
        | Ok session -> Ok session
        | Error msg ->
            Eio.Flow.close socket;
            Error msg
      with
      | Eio.Io _ as exn ->
          Error
            (Printf.sprintf "Connection failed: %s" (Printexc.to_string exn))
      | exn -> Error (Printexc.to_string exn))

(* Kick off the long-polls for progress and diagnostics. The first poll of each
   returns the current state immediately; subsequent polls block until a change. *)
let subscribe_to_events t session =
  t.progress_rid <- alloc_poll_id t "poll-progress";
  t.diagnostic_rid <- alloc_poll_id t "poll-diagnostic";
  send_request session t.progress_rid "poll/progress" progress_poller_id;
  send_request session t.diagnostic_rid "poll/diagnostic" diagnostic_poller_id

(* ---------------------------------------------------------------------------
   Response decoding
   ------------------------------------------------------------------------- *)

(* Extract human-readable text from a dune Pp (pretty-printer) sexp. The Pp is a
   recursive AST; we walk it collecting the visible text. *)
let extract_pp_text =
  let rec aux = function
    (* dune encodes each Pp constructor as [(Name <arg-sexp>)]. A [pair a b]
       argument is itself a nested list [(a b)], so compound constructors carry
       their arguments inside one extra list level. See dune's
       [exported_types.Pp.sexp]. *)
    | Csexp.List [ Csexp.Atom "Nop"; _ ] -> ""
    | Csexp.List [ Csexp.Atom "Newline"; _ ] -> "\n"
    | Csexp.List [ Csexp.Atom "Verbatim"; Csexp.Atom s ] -> s
    | Csexp.List [ Csexp.Atom "Text"; Csexp.Atom s ] -> s
    | Csexp.List [ Csexp.Atom "Char"; Csexp.Atom c ] -> c
    | Csexp.List [ Csexp.Atom "Seq"; Csexp.List [ a; b ] ] -> aux a ^ aux b
    | Csexp.List [ Csexp.Atom "Concat"; Csexp.List [ a; Csexp.List bs ] ] ->
        aux a ^ String.concat "" (List.map aux bs)
    | Csexp.List [ Csexp.Atom "Box"; Csexp.List [ _; a ] ] -> aux a
    | Csexp.List [ Csexp.Atom "Vbox"; Csexp.List [ _; a ] ] -> aux a
    | Csexp.List [ Csexp.Atom "Hbox"; a ] -> aux a
    | Csexp.List [ Csexp.Atom "Hvbox"; Csexp.List [ _; a ] ] -> aux a
    | Csexp.List [ Csexp.Atom "Hovbox"; Csexp.List [ _; a ] ] -> aux a
    | Csexp.List [ Csexp.Atom "Tag"; Csexp.List [ _; a ] ] -> aux a
    | Csexp.List [ Csexp.Atom "Break"; _ ] -> " "
    | Csexp.Atom s -> s (* fallback: a bare atom is treated as plain text *)
    | _ -> ""
  in
  aux

(* Parse a dune Progress sexp (v1 or v2). *)
let parse_progress = function
  | Csexp.Atom "waiting" | Csexp.List [ Csexp.Atom "waiting"; _ ] -> Waiting
  | Csexp.Atom "failed" | Csexp.List [ Csexp.Atom "failed"; _ ] -> Failed
  | Csexp.Atom "interrupted" | Csexp.List [ Csexp.Atom "interrupted"; _ ] -> Interrupted
  | Csexp.Atom "success" | Csexp.List [ Csexp.Atom "success"; _ ] -> Success
  | Csexp.List [ Csexp.Atom "in_progress"; Csexp.List fields ] ->
      let find_int name fields =
        let rec find = function
          | [] -> None
          | Csexp.List [ Csexp.Atom n; Csexp.Atom v ] :: _ when n = name -> (
              try Some (int_of_string v) with
              | Failure _ | Invalid_argument _ -> None)
          | _ :: rest -> find rest
        in
        find fields
      in
      let complete = find_int "complete" fields |> Option.value ~default:0 in
      let remaining = find_int "remaining" fields |> Option.value ~default:0 in
      let failed = find_int "failed" fields |> Option.value ~default:0 in
      In_progress { complete; remaining; failed }
  | _ -> Waiting

(* Parse a dune Diagnostic sexp (v1 or v2) into our simplified diagnostic. *)
let parse_diagnostic csexp =
  let open Csexp in
  (* Helper to find a field in an association list of (name value) pairs. *)
  let find_field name fields =
    let rec find = function
      | [] -> None
      | List [ Atom n; value ] :: _ when n = name -> Some value
      | _ :: rest -> find rest
    in
    find fields
  in

  let parse_severity = function
    | Atom "error" -> Some `Error
    | Atom "warning" -> Some `Warning
    | _ -> None
  in

  let parse_pos pos_fields =
    let fname = find_field "pos_fname" pos_fields in
    let lnum = find_field "pos_lnum" pos_fields in
    let cnum = find_field "pos_cnum" pos_fields in
    let bol = find_field "pos_bol" pos_fields in
    match (fname, lnum, cnum, bol) with
    | Some (Atom file), Some (Atom line_str), Some (Atom cnum_str), Some (Atom bol_str)
      -> (
        try
          let line = int_of_string line_str in
          let cnum = int_of_string cnum_str in
          let bol = int_of_string bol_str in
          Some (file, line, cnum - bol)
        with _ -> None)
    | _ -> None
  in

  let parse_loc = function
    | List fields -> (
        match find_field "start" fields with
        | Some (List pos_fields) -> parse_pos pos_fields
        | _ -> None)
    | _ -> None
  in

  match csexp with
  | List fields -> (
      let message = find_field "message" fields in
      let severity = Option.bind (find_field "severity" fields) parse_severity in
      let loc = Option.bind (find_field "loc" fields) parse_loc in
      match (message, loc) with
      | Some msg_sexp, Some (file, line, column) ->
          Some
            {
              file;
              line;
              column;
              message = extract_pp_text msg_sexp;
              severity = Option.value severity ~default:`Warning;
            }
      | _ -> None)
  | _ -> None

(* Apply a single diagnostic event (Add/Remove) to the diagnostics table. *)
let apply_diagnostic_event t = function
  | Csexp.List [ Csexp.Atom "Add"; diag_sexp ] -> (
      match parse_diagnostic diag_sexp with
      | Some diag ->
          Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
              let current =
                Hashtbl.find_opt t.diagnostics diag.file |> Option.value ~default:[]
              in
              let is_dup =
                List.exists
                  (fun d ->
                    d.line = diag.line && d.column = diag.column
                    && d.message = diag.message)
                  current
              in
              if not is_dup then
                Hashtbl.replace t.diagnostics diag.file (diag :: current))
      | None -> ())
  | Csexp.List [ Csexp.Atom "Remove"; diag_sexp ] -> (
      match parse_diagnostic diag_sexp with
      | Some diag ->
          Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
              let current =
                Hashtbl.find_opt t.diagnostics diag.file |> Option.value ~default:[]
              in
              let filtered =
                List.filter
                  (fun d -> not (d.line = diag.line && d.column = diag.column))
                  current
              in
              if filtered = [] then Hashtbl.remove t.diagnostics diag.file
              else Hashtbl.replace t.diagnostics diag.file filtered)
      | None -> ())
  | _ -> ()

(* Handle the data payload of a poll/progress response: (Some <progress>) or None. *)
let handle_progress_data t = function
  | Csexp.List [ Csexp.Atom "Some"; progress_sexp ] ->
      let progress = parse_progress progress_sexp in
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.progress <- progress)
  | _ -> ()

(* Handle the data payload of a poll/diagnostic response: (Some [<event>...]) or None. *)
let handle_diagnostic_data t data =
  match data with
  | Csexp.List [ Csexp.Atom "Some"; Csexp.List events ] ->
      List.iter (apply_diagnostic_event t) events
  | _ -> ()

(* Process one incoming packet. Returns false if the connection is closed.
   When a poll response arrives, the corresponding poll is re-issued so that we
   always have exactly one outstanding progress poll and one diagnostic poll. *)
let process_messages t session =
  try
    match read_csexp session.buf_read with
    | Error _ -> false (* Connection closed *)
    | Ok csexp ->
        (match csexp with
        | Csexp.List
            [
              Csexp.List [ Csexp.Atom "id"; rid ];
              Csexp.List [ Csexp.Atom "result"; result ];
            ] -> (
            let is_progress = same_csexp rid t.progress_rid in
            let is_diagnostic = same_csexp rid t.diagnostic_rid in
            (match result with
            | Csexp.List [ Csexp.Atom "ok"; data ] ->
                if is_progress then handle_progress_data t data
                else if is_diagnostic then handle_diagnostic_data t data
                else ()
            | Csexp.List [ Csexp.Atom "error"; err ] ->
                Log.debug (fun m -> m "RPC error: %s" (Csexp.to_string err))
            | _ -> ());
            (* Re-issue the corresponding poll so we always keep exactly one
               outstanding progress poll and one diagnostic poll. Re-issuing on
               error too means a transient failure cannot silently drop a
               subscription. *)
            if is_progress then (
              t.progress_rid <- alloc_poll_id t "poll-progress";
              send_request session t.progress_rid "poll/progress" progress_poller_id)
            else if is_diagnostic then (
              t.diagnostic_rid <- alloc_poll_id t "poll-diagnostic";
              send_request session t.diagnostic_rid "poll/diagnostic"
                diagnostic_poller_id))
        | _ -> ());
        true
  with
  | End_of_file -> false
  | Failure _ | Invalid_argument _ -> true
  | exn ->
      Log.debug (fun m ->
          m "Unexpected error in process_messages: %s" (Printexc.to_string exn));
      true

(* Attempt to connect to [instance]. On success, subscribe to events and mark
   the client Connected. Returns true iff a connection was established. *)
let try_connect_instance t ~net (instance : Registry_poll.instance) =
  match connect_to_instance ~sw:t.sw ~net instance with
  | Ok session ->
      subscribe_to_events t session;
      t.state <- Connected session;
      true
  | Error _msg ->
      (* Ignore connection errors for resilience *)
      false

(* Dune exposes its RPC server as a single unix socket at
   <root>/_build/.rpc/dune, without writing a registry entry. Build an
   [instance] pointing at that well-known path so we can probe it directly
   when the registry-based discovery finds no matching instance. *)
let direct_socket_instance (t : t) : Registry_poll.instance =
  { root = t.root;
    pid = None;
    socket = Some (t.root ^ "/_build/.rpc/dune"); }

(* Poll for Dune instances and connect if found. Falls back to dune's
   well-known in-build-dir RPC socket when the registry has no match. *)
let poll_and_connect t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match t.state with
      | Connected _ | Closed -> ()
      | Disconnected ->
          let net = Eio.Stdenv.net t.env in
          let connected_via_registry =
            match t.registry_path with
            | None -> false
            | Some registry_path -> (
                let fs = Eio.Stdenv.fs t.env in
                match Registry_poll.poll ~fs registry_path with
                | Error msg ->
                    if t.last_poll_error <> Some msg then
                      t.last_poll_error <- Some msg (* Log error if needed *) ;
                    false
                | Ok instances -> (
                    t.last_poll_error <- None;
                    match find_matching_instance instances t.root with
                    | None -> false
                    | Some instance -> try_connect_instance t ~net instance))
          in
          if not connected_via_registry then
            ignore (try_connect_instance t ~net (direct_socket_instance t)))

(* Main polling loop *)
let run t =
  let clock = Eio.Stdenv.clock t.env in
  let rec loop () =
    poll_and_connect t;

    (* Process messages if connected. With long-polling this blocks until the
       next progress/diagnostic change, so the loop self-paces. *)
    (match t.state with
    | Connected session ->
        if not (process_messages t session) then
          (* Connection lost *)
          Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
              Eio.Flow.shutdown session.flow `All;
              t.state <- Disconnected)
    | _ -> ());

    (* Sleep before the next iteration. When connected this is a small extra
       delay after each update; when disconnected it paces reconnection. *)
    Eio.Time.sleep clock 0.25;

    match t.state with Closed -> () | _ -> loop ()
  in
  loop ()

(* Get diagnostics for a file *)
let get_diagnostics t ~file =
  Eio.Mutex.use_ro t.mutex (fun () ->
      if file = "" then
        (* Return all diagnostics *)
        Hashtbl.fold (fun _file diags acc -> diags @ acc) t.diagnostics []
      else Hashtbl.find_opt t.diagnostics file |> Option.value ~default:[])

(* Get current build progress *)
let get_progress t = Eio.Mutex.use_ro t.mutex (fun () -> t.progress)

(* Close the client *)
let close t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match t.state with
      | Connected session ->
          Eio.Flow.shutdown session.flow `All;
          t.state <- Closed
      | _ -> t.state <- Closed)
