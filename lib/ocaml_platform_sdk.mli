(** A programmatic interface to OCaml Platform tools.

    This SDK provides a unified, type-safe API for interacting with core OCaml
    development tools like Dune, Merlin, and OCamlformat.

    The main entry point is the {!t} type, which represents an active session
    for a specific project. It must be created and used within an Eio event
    loop.

    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let sdk = Ocaml_platform_sdk.create ~sw ~env ~project_root:"." () in
      (* ... use the sdk ... *)
      Ocaml_platform_sdk.close sdk
    ]} *)

(** {1 Session Management} *)

type t
(** The main SDK session handle. It encapsulates clients for all platform tools
    and must be used within an Eio context. *)

val create :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  project_root:string ->
  ?enable_dune:bool ->
  unit ->
  t
(** [create ~sw ~env ~project_root ()] initializes a new SDK session for the
    project at [project_root].

    This function sets up clients for Merlin and OCamlformat. If [enable_dune]
    is true (the default), it also starts a background fiber to poll for and
    connect to a Dune RPC server.

    @param sw The Eio switch to manage background fibers (e.g., Dune RPC).
    @param env The Eio environment.
    @param project_root The root directory of the OCaml project.
    @param enable_dune
      Defaults to [true]. If false, the Dune RPC client will not be initialized.
*)

val close : t -> unit
(** [close t] gracefully shuts down the session and releases resources, such as
    disconnecting from the Dune RPC server. *)

(** {1 Dune Integration} *)
module Dune : sig
  type diagnostic = Dune_rpc_client.diagnostic = {
    file : string;
    line : int;
    column : int;
    message : string;
    severity : [ `Error | `Warning ];
  }
  (** A diagnostic message (error or warning) from Dune. *)

  type progress = Dune_rpc_client.progress =
    | Waiting
    | In_progress of { complete : int; remaining : int; failed : int }
    | Failed
    | Interrupted
    | Success  (** The current build progress reported by Dune. *)

  val diagnostics :
    t -> file:string -> (diagnostic list, [> `Dune_not_initialized ]) result
  (** [diagnostics t ~file] gets the current list of diagnostics from Dune. If
      [file] is an empty string, it returns all diagnostics for the project.
      Returns an error if the Dune client was not initialized. *)

  val progress : t -> (progress, [> `Dune_not_initialized ]) result
  (** [progress t] gets the current build progress from Dune. Returns an error
      if the Dune client was not initialized. *)
end

(** {1 Merlin Integration} *)
module Merlin : sig
  val find_definition :
    t ->
    source_path:string ->
    source_text:string ->
    line:int ->
    col:int ->
    (string * Lexing.position, string) result
  (** [find_definition t ~source_path ~source_text ~line ~col] locates the
      definition of the symbol at the given position. *)

  val find_references :
    t ->
    source_path:string ->
    source_text:string ->
    line:int ->
    col:int ->
    ((Ocaml_utils.Warnings.loc * string) list, string) result
  (** [find_references t ~source_path ~source_text ~line ~col] finds all
      references to the symbol at the given position. *)

  val type_at_pos :
    t ->
    source_path:string ->
    source_text:string ->
    line:int ->
    col:int ->
    (Ocaml_utils.Warnings.loc * string, string) result
  (** [type_at_pos t ~source_path ~source_text ~line ~col] gets the type of the
      expression at the given position. *)

  val document_symbols :
    t ->
    source_path:string ->
    source_text:string ->
    (Query_protocol.item list, string) result
  (** [document_symbols t ~source_path ~source_text] returns the outline (e.g.,
      values, types, modules) of the given source file. *)

  val diagnostics :
    t ->
    source_path:string ->
    source_text:string ->
    (Ocaml_parsing.Location.report list, string) result
  (** [diagnostics t ~source_path ~source_text] gets syntax and type errors from
      Merlin for the given source file. *)
end

(** {1 OCamlformat Integration} *)
module Ocamlformat : sig
  val format_doc : t -> path:string -> content:string -> (string, string) result
  (** [format_doc t ~path ~content] formats an OCaml source file. It
      automatically detects whether it is an implementation (.ml) or interface
      (.mli). *)

  val format_type : t -> typ:string -> (string, [> `Msg of string ]) result
  (** [format_type t ~typ] formats a string containing an OCaml type expression.
  *)
end

(** {1 Code Analysis} *)
module Analysis : sig
  val module_signature : t -> module_path:string list -> (string, string) result
  (** [module_signature t ~module_path] reads the build artifacts (.cmi or .cmt)
      to extract the signature of a compiled module. Example:
      [module_signature t ~module_path:["My_lib"; "Utils"]] *)
end
