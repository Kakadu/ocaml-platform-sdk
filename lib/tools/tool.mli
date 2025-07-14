module Context : sig
  type t = {
    sdk : Ocaml_platform_sdk.t;
    sw : Eio.Switch.t;
    fs : Eio.Fs.dir_ty Eio.Path.t;
    process_mgr : Eio_unix.Process.mgr_ty Eio.Process.mgr;
    project_root : string;
  }

  val create :
    sw:Eio.Switch.t ->
    env:Eio_unix.Stdenv.base ->
    project_root:string ->
    ?enable_dune:bool ->
    unit ->
    t
end

module type S = sig
  val name : string
  val description : string

  module Args : sig
    type t

    val of_yojson : Yojson.Safe.t -> (t, string) Result.t
    val to_yojson : t -> Yojson.Safe.t
    val schema : unit -> Yojson.Safe.t
  end

  module Output : sig
    type t

    val to_yojson : t -> Yojson.Safe.t
  end

  module Error : sig
    type t

    val to_string : t -> string
  end

  val execute : Context.t -> Args.t -> (Output.t, Error.t) Result.t
end
