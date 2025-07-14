open Ocaml_platform_sdk

let name = "fs_write"

let description =
  "Write content to a file. For OCaml files (.ml/.mli), automatically formats \
   the code and returns diagnostics."

module Args = struct
  type t = { file_path : string; content : string } [@@deriving yojson]

  let schema () =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "file_path",
                `Assoc
                  [
                    ("type", `String "string");
                    ("description", `String "The path to the file to write");
                  ] );
              ( "content",
                `Assoc
                  [
                    ("type", `String "string");
                    ("description", `String "The content to write to the file");
                  ] );
            ] );
        ("required", `List [ `String "file_path"; `String "content" ]);
      ]
end

module Output = struct
  type diagnostic = {
    message : string;
    severity : string;
    start_line : int;
    start_col : int;
    end_line : int;
    end_col : int;
  }
  [@@deriving yojson]

  type t = {
    path : string;
    formatted : bool;
    diagnostics : diagnostic list option; [@yojson.option]
  }
  [@@deriving yojson]
end

module Error = struct
  type t = Permission_denied of string | IO_error of string

  let to_string = function
    | Permission_denied path -> Printf.sprintf "Permission denied: %s" path
    | IO_error msg -> Printf.sprintf "I/O error: %s" msg
end

let execute (context : Tool.Context.t) (args : Args.t) =
  let { Args.file_path; content } = args in

  (* Resolve relative paths against project root *)
  let file_path =
    if Filename.is_relative file_path then
      Filename.concat context.project_root file_path
    else file_path
  in

  (* Ensure parent directories exist using Eio *)
  let dir = Filename.dirname file_path in
  (try Eio.Path.mkdirs ~perm:0o755 Eio.Path.(context.fs / dir)
   with _exn ->
     (* Directory might already exist or we're in a normal case where parent exists *)
     ());

  (* Check if it's an OCaml file *)
  let is_ocaml_file =
    Filename.check_suffix file_path ".ml"
    || Filename.check_suffix file_path ".mli"
  in

  let final_content, formatted, diagnostics =
    if is_ocaml_file then
      (* Format with ocamlformat *)
      match
        OCamlformat.format_doc context.sdk ~path:file_path
          ~content
      with
      | Ok formatted_content ->
          (* Get diagnostics from Merlin *)
          let diagnostics =
            match
              Merlin.diagnostics context.sdk ~source_path:file_path
                ~source_text:formatted_content
            with
            | Ok reports ->
                Some
                  (List.map
                     (fun report ->
                       let module Loc = Ocaml_parsing.Location in
                       let loc = Loc.loc_of_report report in
                       let message =
                         Format.asprintf "%a" Loc.print_main report
                       in
                       let severity =
                         match report.source with
                         | Warning -> "warning"
                         | _ -> "error"
                       in
                       {
                         Output.message;
                         severity;
                         start_line = loc.loc_start.pos_lnum;
                         start_col =
                           loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
                         end_line = loc.loc_end.pos_lnum;
                         end_col = loc.loc_end.pos_cnum - loc.loc_end.pos_bol;
                       })
                     reports)
            | Error _ -> None
          in
          (formatted_content, true, diagnostics)
      | Error _ -> (content, false, None)
    else (content, false, None)
  in

  (* Write the file *)
  try
    Eio.Path.save
      Eio.Path.(context.fs / file_path)
      final_content ~create:(`Or_truncate 0o644);
    Ok { Output.path = file_path; formatted; diagnostics }
  with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) ->
      Error (Error.Permission_denied file_path)
  | exn -> Error (Error.IO_error (Printexc.to_string exn))
