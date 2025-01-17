open Application_types

type t = Board_niitv_tsan_types.config

let equal = Board_niitv_tsan_types.equal_config

let to_yojson = Board_niitv_tsan_types.config_to_yojson

let of_yojson = Board_niitv_tsan_types.config_of_yojson

let default =
  { Board_niitv_tsan_types.
    input = ASI
  ; input_source = 1
  ; t2mi_source = 2
  ; t2mi_mode =
      { pid = 0x1FFF
      ; enabled = false
      ; t2mi_stream_id = 0
      ; stream = ID Stream.Multi_TS_ID.forbidden
      }
  ; jitter_mode =
      { pid = 0x1FFF
      ; stream = Stream.Multi_TS_ID.of_int32_pure 0l
      ; stream_id = None
      }
  }

let to_string x =
  Yojson.Safe.pretty_to_string @@ to_yojson x

let of_string x =
  Yojson.Safe.from_string x
  |> of_yojson
  |> function Ok v -> v | Error e -> failwith e
