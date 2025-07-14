type t = {
  dune_rpc : Dune_rpc_client.t option;
  merlin : Merlin_client.t;
  ocamlformat : Ocamlformat_client.t;
  project_root : string;
  env : Eio_unix.Stdenv.base;
}

let create ~sw ~env ~project_root ?(enable_dune = true) () =
  let dune_rpc =
    if enable_dune then (
      let client = Dune_rpc_client.create ~sw ~env ~root:project_root in
      (* Start the polling loop in the background *)
      Eio.Fiber.fork ~sw (fun () ->
          try Dune_rpc_client.run client
          with Eio.Cancel.Cancelled _ -> Dune_rpc_client.close client);
      Some client)
    else None
  in
  let merlin = Merlin_client.create ~project_root in
  let ocamlformat = Ocamlformat_client.create () in
  { dune_rpc; merlin; ocamlformat; project_root; env }

let close t =
  match t.dune_rpc with
  | Some client -> Dune_rpc_client.close client
  | None -> ()

module Dune = struct
  type diagnostic = Dune_rpc_client.diagnostic = {
    file : string;
    line : int;
    column : int;
    message : string;
    severity : [ `Error | `Warning ];
  }

  type progress = Dune_rpc_client.progress =
    | Waiting
    | In_progress of { complete : int; remaining : int; failed : int }
    | Failed
    | Interrupted
    | Success

  let diagnostics t ~file =
    match t.dune_rpc with
    | Some client -> Ok (Dune_rpc_client.get_diagnostics client ~file)
    | None -> Error `Dune_not_initialized

  let progress t =
    match t.dune_rpc with
    | Some client -> Ok (Dune_rpc_client.get_progress client)
    | None -> Error `Dune_not_initialized
end

module Merlin = struct
  let find_definition t ~source_path ~source_text ~line ~col =
    Merlin_client.find_definition t.merlin ~source_path ~source_text ~line ~col

  let find_references t ~source_path ~source_text ~line ~col =
    Merlin_client.find_references t.merlin ~source_path ~source_text ~line ~col

  let type_at_pos t ~source_path ~source_text ~line ~col =
    Merlin_client.type_at_pos t.merlin ~source_path ~source_text ~line ~col

  let document_symbols t ~source_path ~source_text =
    Merlin_client.document_symbols t.merlin ~source_path ~source_text

  let diagnostics t ~source_path ~source_text =
    Merlin_client.diagnostics t.merlin ~source_path ~source_text
end

module OCamlformat = struct
  let format_doc t ~path ~content =
    Ocamlformat_client.format_doc t.ocamlformat ~path ~content

  let format_type t ~typ = Ocamlformat_client.format_type t.ocamlformat ~typ
end

module Analysis = struct
  let module_signature t ~module_path =
    Ocaml_analysis.get_module_signature ~env:t.env ~project_root:t.project_root
      ~module_path
end
