open Containers
open Board_types
open Api_js.Requests.Json_request
open Api_js.Api_types
open Common

let get_base_path () = Uri.Path.Format.(
    Boards_js.Requests.get_board_path () / ("streams" @/ empty))

module WS = struct

  open Common.Uri
  open Streams.TS

  let get_streams ?(inputs=[]) ?(ids=[]) control =
    WS.get ~from:(Json.List.of_yojson Stream.of_yojson)
      ~path:Path.Format.(get_base_path ())
      ~query:Query.[ "id",    (module List(Int32))
                   ; "input", (module List(Topology.Show_topo_input)) ]
      control (List.map Stream.id_to_int32 ids) inputs

  let get_bitrate ~id control =
    WS.get ~from:bitrate_of_yojson
      ~path:Path.Format.(get_base_path () / (Int32 ^/ "bitrate" @/ empty))
      ~query:Query.empty
      control (Stream.id_to_int32 id)

  let get_structure ~id control =
    WS.get ~from:structure_of_yojson
      ~path:Path.Format.(get_base_path () / (Int32 ^/ "structure" @/ empty))
      ~query:Query.empty
      control (Stream.id_to_int32 id)

  module T2MI = struct

    open Streams.T2MI

    let get_structure ~id control =
      WS.get ~from:structure_of_yojson
        ~path:Path.Format.(get_base_path () / (Int32 ^/ "t2mi/structure" @/ empty))
        ~query:Query.empty
        control id

  end

  module Errors = struct

    open Errors

    let get_errors ?(errors=[]) ?(priority=[]) ?(pids=[]) ~id control =
      WS.get ~from:(Json.List.of_yojson of_yojson)
        ~path:Path.Format.(get_base_path () / (Int32 ^/ "errors" @/ empty))
        ~query:Query.[ "errors",   (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid",      (module List(Int))]
        control id errors priority pids

  end

end

module HTTP = struct

  open Common.Uri
  open Streams.TS

  let get_streams ?(ids=[]) ?(inputs=[]) ?limit ?compress ?from ?till ?duration control =
    get_result ~from:(Api_js.Api_types.rows_of_yojson
                        streams_states_of_yojson
                        streams_unique_of_yojson)
      ~path:(get_base_path ())
      ~query:Query.[ "id",       (module List(Int32))
                   ; "input",    (module List(Topology.Show_topo_input))
                   ; "limit",    (module Option(Int))
                   ; "compress", (module Option(Bool))
                   ; "from",     (module Option(Time.Show))
                   ; "to",       (module Option(Time.Show))
                   ; "duration", (module Option(Time.Relative)) ]
      control (List.map Stream.id_to_int32 ids)
      inputs limit compress from till duration

  let get_si_psi_section ?section ?table_id_ext ?eit_ts_id ?eit_orig_nw_id ~id ~table_id control =
    get_result ~from:section_of_yojson
      ~from_err:section_error_of_yojson
      ~path:Path.Format.(get_base_path () / (Int32 ^/ "section" @/ Int ^/ empty))
      ~query:Query.[ "section",        (module Option(Int))
                   ; "table-id-ext",   (module Option(Int))
                   ; "eit-ts-id",      (module Option(Int))
                   ; "eit-orig-nw-id", (module Option(Int)) ]
      control (Stream.id_to_int32 id) table_id section table_id_ext eit_ts_id eit_orig_nw_id

  let get_bitrate ?limit ?compress ?from ?till ?duration ~id control =
    get_result ~from:(fun _ -> Error "FIXME Not implemented")
      ~path:Path.Format.(get_base_path () / (Int32 ^/ "bitrate" @/ empty))
      ~query:Query.[ "limit",    (module Option(Int))
                   ; "compress", (module Option(Bool))
                   ; "from",     (module Option(Time.Show))
                   ; "to",       (module Option(Time.Show))
                   ; "duration", (module Option(Time.Relative)) ]
      control (Stream.id_to_int32 id) limit compress from till duration

  let get_structure ?limit ?from ?till ?duration ~id control =
    get_result
      ~from:(Api_js.Api_types.rows_of_yojson
               Json.(List.of_yojson (Pair.of_yojson
                                       Stream.id_of_yojson
                                       structure_of_yojson))
               (fun _ -> Error "cannot be compressed"))
      ~path:Path.Format.(get_base_path () / (Int32 ^/ "structure" @/ empty))
      ~query:Query.[ "limit",    (module Option(Int))
                   ; "from",     (module Option(Time.Show))
                   ; "to",       (module Option(Time.Show))
                   ; "duration", (module Option(Time.Relative)) ]
      control (Stream.id_to_int32 id) limit from till duration

  module T2MI = struct

    open Streams.T2MI

    let get_sequence ?duration ~id control =
      get_result ~from:sequence_of_yojson
        ~path:Path.Format.(get_base_path () / (Int32 ^/ "sequence" @/ empty))
        ~query:Query.[ "duration", (module Option(Time.Relative)) ]
        control (Stream.id_to_int32 id) duration

    let get_structure ?limit ?from ?till ?duration ~id control =
      get_result ~from:(fun _ -> Error "FIXME Not implemented")
        ~path:Path.Format.(get_base_path () / (Int32 ^/ "structure" @/ empty))
        ~query:Query.[ "limit",    (module Option(Int))
                     ; "from",     (module Option(Time.Show))
                     ; "to",       (module Option(Time.Show))
                     ; "duration", (module Option(Time.Relative)) ]
        control (Stream.id_to_int32 id) limit from till duration

  end

  module Errors = struct

    open Errors

    let get_errors?(errors=[]) ?(priority=[]) ?(pids=[])
          ?limit ?compress ?from ?till ?duration ~id control =
      get_result ~from:(rows_of_yojson raw_of_yojson compressed_of_yojson)
        ~path:Path.Format.(get_base_path () / (Int32 ^/ "errors" @/ empty))
        ~query:Query.[ "errors",   (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid",      (module List(Int))
                     ; "limit",    (module Option(Int))
                     ; "compress", (module Option(Bool))
                     ; "from",     (module Option(Time.Show))
                     ; "to",       (module Option(Time.Show))
                     ; "duration", (module Option(Time.Relative)) ]
        control (Stream.id_to_int32 id) errors priority pids limit
        compress from till duration

    let get_errors_percent ?(errors=[]) ?(priority=[]) ?(pids=[])
          ?from ?till ?duration ~id control =
      get_result ~from:(fun _ -> Error "not implemented")
        ~path:Path.Format.(get_base_path () / (Int32 ^/ "errors/percent" @/ empty))
        ~query:Query.[ "errors",   (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid",      (module List(Int))
                     ; "from",     (module Option(Time.Show))
                     ; "to",       (module Option(Time.Show))
                     ; "duration", (module Option(Time.Relative)) ]
        control (Stream.id_to_int32 id) errors priority pids from till duration

    let get_errors_has_any ?(errors=[]) ?(priority=[]) ?(pids=[])
          ?from ?till ?duration ~id control =
      get_result ~from:(fun _ -> Error "not implemented")
        ~path:Path.Format.(get_base_path () / (Int32 ^/ "errors/has-any" @/ empty))
        ~query:Query.[ "errors",   (module List(Int))
                     ; "priority", (module List(Int))
                     ; "pid",      (module List(Int))
                     ; "from",     (module Option(Time.Show))
                     ; "to",       (module Option(Time.Show))
                     ; "duration", (module Option(Time.Relative)) ]
        control (Stream.id_to_int32 id) errors priority pids from till duration

  end

end
