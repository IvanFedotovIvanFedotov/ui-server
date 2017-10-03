let prefix = 0x55AA

[@@@ocaml.warning "-32"]

[%%cstruct
 type common_header =
   { prefix   : uint16_t
   ; msg_code : uint16_t
   } [@@little_endian]]

[%%cstruct
 type complex_req_header =
   { client_id  : uint16_t
   ; length     : uint16_t
   ; request_id : uint16_t
   } [@@little_endian]]

[%%cstruct
 type complex_rsp_header =
   { length     : uint16_t
   ; client_id  : uint16_t
   ; code_ext   : uint16_t
   ; request_id : uint16_t
   ; param      : uint16_t
   } [@@little_endian]]

[%%cstruct
 type complex_rsp_header_ext =
   { length     : uint16_t
   ; client_id  : uint16_t
   ; code_ext   : uint16_t
   ; request_id : uint16_t
   ; param      : uint32_t
   } [@@little_endian]]

[%%cstruct
 type board_info =
   { board_type    : uint8_t
   ; board_version : uint8_t
   ; rfu           : uint16_t
   } [@@little_endian]]

[%%cstruct
 type board_mode =
   { mode           : uint8_t
   ; rfu            : uint8_t
   ; t2mi_pid       : uint16_t
   ; t2mi_stream_id : uint32_t
   } [@@little_endian]]

[%%cstruct
 type req_get_t2mi_frame_seq =
   { time : uint16_t
   } [@@little_endian]]

[%%cstruct
 type req_get_t2mi_info =
   { rfu       : uint16_t
   ; stream_id : uint16_t
   } [@@little_endian]]

(* Status *)

[%%cstruct
 type status =
   { rfu                 : uint32_t
   ; ts_num              : uint8_t
   ; streams_ver         : uint8_t
   ; load                : uint8_t
   ; ts_ver_com          : uint8_t
   ; bitrate             : uint32_t
   ; mode                : uint8_t
   ; rfu_1               : uint8_t
   ; t2mi_pid            : uint16_t
   ; t2mi_stream_id      : uint32_t
   ; rfu_2               : uint16_t
   ; flags               : uint8_t
   ; services_num        : uint8_t
   ; t2mi_sync           : uint8_t
   ; rfu_3               : uint8_t [@len 7]
   ; flags_2             : uint16_t
   ; rfu_4               : uint16_t
   ; t2mi_ver_lst        : uint32_t
   ; rfu_5               : uint32_t [@len 7]
   ; ts_ver_lst          : uint8_t [@len 50]
   ; rfu_6               : uint8_t [@len 50]
   ; jitter_stream_id    : uint32_t
   ; jitter_pid          : uint16_t
   ; rfu_7               : uint8_t [@len 6]
   ; ts_absent_lst       : uint16_t [@len 8]
   ; ts_not_verified_lst : uint16_t [@len 8]
   ; rfu_8               : uint16_t [@len 146]
   } [@@little_endian]]

(* TS errors *)

[%%cstruct
 type ts_error =
   { rfu      : uint16_t
   ; count    : uint16_t
   ; err_code : uint8_t
   ; err_ext  : uint8_t
   ; pid      : uint16_t
   ; packet   : uint32_t
   ; param_1  : uint32_t
   ; param_2  : uint32_t
   } [@@little_endian]]

[%%cstruct
 type ts_errors =
   { length    : uint16_t
   ; rfu_1     : uint16_t
   ; stream_id : uint32_t
   ; rfu_2     : uint16_t
   ; count     : uint16_t
   } [@@little_endian]]

(* T2-MI errors *)

[%%cstruct
 type t2mi_error =
   { index : uint16_t
   ; data  : uint16_t
   } [@@little_endian]]

[%%cstruct
 type t2mi_errors =
   { length    : uint16_t
   ; rfu_1     : uint16_t
   ; stream_id : uint32_t
   ; pid       : uint16_t
   ; sync      : uint8_t
   ; rfu_2     : uint8_t
   ; err_flags : uint16_t
   ; rfu_3     : uint8_t [@len 5]
   ; count     : uint8_t
   } [@@little_endian]]

(* Board errors *)

[%%cstruct
 type board_errors =
   { count  : uint32_t
   ; errors : uint32_t [@len 17]
   } [@@little_endian]]

(* T2-MI frame sequence*)

[%%cstruct
 type t2mi_frame_seq_item =
   { typ        : uint8_t
   ; sframe     : uint8_t
   ; frame      : uint8_t
   ; plp        : uint8_t
   ; dyn1_frame : uint8_t
   ; dyn2_frame : uint8_t
   ; count      : uint32_t
   ; time       : uint32_t
   } [@@little_endian]]

(* Jitter*)

[%%cstruct
 type jitter_item =
   { status : uint16_t
   ; d_packet : uint16_t
   ; d_pcr    : uint32_t
   ; drift    : uint32_t
   ; fo       : uint32_t
   ; jitter   : uint16_t
   ; rfu      : uint32_t [@len 10]
   } [@@little_endian]]

[%%cstruct
 type jitter =
   { count       : uint16_t
   ; pid         : uint16_t
   ; time        : uint32_t
   ; req_ptr     : uint32_t
   ; req_next    : uint32_t
   ; bitrate     : uint32_t
   ; t_pcr       : uint32_t
   ; packet_time : uint32_t
   ; rfu         : uint32_t
   ; k_drift     : uint32_t
   ; k_fo        : uint32_t
   ; k_jitter    : uint32_t
   ; rfu_1       : uint64_t
   } [@@little_endian]]

[%%cstruct
 type req_get_jitter =
   { ptr : uint32_t
   } [@@little_endian]]

(* Ts structures *)

[%%cstruct
 type struct_block_header =
   { code   : uint16_t
   ; length : uint16_t
   } [@@little_endian]]

[%%cstruct
 type general_struct_block =
   { services_num : uint8_t
   ; version      : uint8_t
   ; bitrate      : uint32_t
   ; packet_count : uint32_t
   ; network_pid  : uint16_t
   ; ts_id        : uint16_t
   ; nw_id        : uint16_t
   ; orig_nw_id   : uint16_t
   ; bouquet_id   : uint16_t
   ; nw_info      : uint8_t [@len 12]
   ; string_len   : uint16_t
   } [@@little_endian]]

[%%cstruct
 type services_struct_block =
   { id      : uint16_t
   ; pmt_pid : uint16_t
   ; pcr_pid : uint16_t
   ; flags   : uint16_t
   ; bitrate : uint32_t
   } [@@little_endian]]

[%%cstruct
 type es_struct_block =
   { pid          : uint16_t
   ; es_type      : uint8_t
   ; es_stream_id : uint8_t
   } [@@little_endian]]

[%%cstruct
 type ecm_struct_block =
   { pid          : uint16_t
   ; ca_system_id : uint8_t
   ; rfu          : uint8_t
   } [@@little_endian]]

[%%cstruct
 type table_struct_block =
   { version    : uint8_t
   ; id         : uint8_t
   ; id_ext     : uint16_t
   ; lsn        : uint8_t
   ; rfu        : uint8_t
   ; adv_info_1 : uint16_t
   ; adv_info_2 : uint16_t
   ; adv_info_3 : uint8_t
   ; adv_info_4 : uint8_t
   ; pid        : uint16_t
   } [@@little_endian]]

[%%cstruct
 type ts_struct =
   { length    : uint32_t
   ; stream_id : uint32_t
   } [@@little_endian]]

[%%cstruct
 type ts_structs =
   { count   : uint8_t
   ; version : uint8_t
   ; rfu     : uint8_t [@len 6]
   } [@@little_endian]]

[%%cstruct
 type req_get_ts_struct =
   { stream_id : uint32_t
   } [@@little_endian]]

(* Bitrates *)

[%%cstruct
 type pid_bitrate =
   { pid     : uint16_t
   ; packets : uint32_t
   } [@@little_endian]]

[%%cstruct
 type table_bitrate =
   { table_id     : uint8_t
   ; flags        : uint8_t
   ; table_id_ext : uint16_t
   ; adv_info_1   : uint16_t
   ; adv_info_2   : uint16_t
   ; packets      : uint32_t
   } [@@little_endian]]

[%%cstruct
 type stream_bitrate =
   { length        : uint32_t
   ; stream_id     : uint32_t
   ; ts_bitrate    : uint32_t
   ; total_packets : uint32_t
   ; total_pids    : uint16_t
   ; total_tables  : uint16_t
   } [@@little_endian]]

[%%cstruct
 type bitrates =
   { count   : uint8_t
   ; version : uint8_t
   ; rfu     : uint8_t [@len 6]
   } [@@little_endian]]

(* T2-MI info *)

[%%cstruct
 type t2mi_info =
   { version   : uint16_t
   ; rfu       : uint16_t
   ; stream_id : uint16_t
   ; packets   : uint16_t [@len 16]
   ; length    : uint16_t
   ; t2mi_pid  : uint16_t
   ; conf_len  : uint16_t
   ; l1_pre    : uint8_t [@len 21]
   } [@@little_endian]]

(* Streams list *)

[%%cstruct
 type streams_list =
   { count   : uint8_t
   ; version : uint8_t
   ; rfu     : uint16_t
   } [@@little_endian]]

(* Set jitter mode *)

[%%cstruct
 type req_set_jitter_mode =
   { pid       : uint16_t
   ; stream_id : uint32_t
   } [@@little_endian]]

[@@@ocaml.warning "+32"]

open Board_types

type part =
  { first      : bool
  ; param      : int32
  ; data       : Cbuffer.t
  }

type event_response = Board_errors of board_errors
                    | Bitrate      of (streams * bitrate list)
                    | Struct       of (streams * ts_struct list)
                    | T2mi_info    of t2mi_info list
                    | Jitter       of jitter
                    | Streams      of streams

type api = { devinfo         : unit        -> info Lwt.t
           ; set_mode        : mode        -> unit Lwt.t
           ; set_jitter_mode : jitter_mode -> unit Lwt.t
           ; get_t2mi_seq    : int         -> t2mi_packet list Lwt.t
           ; reset           : unit        -> unit Lwt.t
           ; config          : unit        -> config Lwt.t
           }

type _ instant_request = Set_board_mode  : mode -> unit instant_request
                       | Set_jitter_mode : jitter_mode -> unit instant_request
                       | Reset           : unit instant_request

type jitter_req =
  { request_id : int
  ; pointer    : int32
  }

type ts_req =
  { request_id : int
  ; version    : int
  }

type t2mi_req =
  { request_id     : int
  ; t2mi_stream_id : int
  ; version        : int
  }

type _ event_request = Get_board_errors : int        -> event_response event_request
                     | Get_jitter       : jitter_req -> event_response event_request
                     | Get_ts_structs   : ts_req     -> event_response event_request
                     | Get_bitrates     : ts_req     -> event_response event_request
                     | Get_t2mi_info    : t2mi_req   -> event_response event_request
                     | Get_streams      : ts_req     -> event_response event_request

type _ request = Get_board_info     : info request
               | Get_board_mode     : mode request
               | Get_t2mi_frame_seq : (int * int) -> t2mi_packet list request

(* ------------------- Misc ------------------- *)

let io = fun x -> Lwt_io.printf "%s\n" x |> ignore

let input_to_int = function
  | SPI -> 0 | ASI -> 1
let input_of_int = function
  | 0 -> Some SPI | 1 -> Some ASI | _ -> None

let int_to_bool_list x = CCList.map (fun i -> (x land CCInt.pow 2 i) > 0) (CCList.range 0 7)

let int_to_t2mi_sync_list x = int_to_bool_list x
                              |> CCList.foldi (fun acc i x -> if x then i :: acc else acc) []

(* -------------------- Message constructors ------------------*)

(* Common *)

let to_common_header ~msg_code () =
  let hdr = Cbuffer.create sizeof_common_header in
  let () = set_common_header_prefix hdr prefix in
  let () = set_common_header_msg_code hdr msg_code in
  hdr

let to_complex_req_header ?(client_id=0) ?(request_id=0) ~msg_code ~length () =
  let hdr = to_common_header ~msg_code () in
  let complex_hdr = Cbuffer.create sizeof_complex_req_header in
  let () = set_complex_req_header_client_id complex_hdr client_id in
  let () = set_complex_req_header_length complex_hdr length in
  let () = set_complex_req_header_request_id complex_hdr request_id in
  Cbuffer.append hdr complex_hdr

let to_simple_req ~msg_code ~body () =
  let hdr = to_common_header ~msg_code () in
  Cbuffer.append hdr body

let to_complex_req ?client_id ?request_id ~msg_code ~body () =
  let length = (Cbuffer.len body / 2) + 1 in
  let hdr = to_complex_req_header ?client_id ?request_id ~msg_code ~length () in
  Cbuffer.append hdr body

let to_mode_exn mode t2mi_pid t2mi_stream_id =
  { input = CCOpt.get_exn @@ input_of_int (mode land 1)
  ; t2mi = Some { enabled   = if (mode land 4) > 0 then true else false
                ; pid       = t2mi_pid
                ; stream_id = Common.Stream.of_int32 t2mi_stream_id
                }
  }

(* -------------------- Response parsers ------------------*)

(* Get board info *)

let of_rsp_get_board_info msg =
  { typ = get_board_info_board_type msg
  ; ver = get_board_info_board_version msg
  }

(* Get board mode *)

let of_rsp_get_board_mode msg =
  to_mode_exn (get_board_mode_mode msg)
              (get_board_mode_t2mi_pid msg)
              (get_board_mode_t2mi_stream_id msg)

(* Get board errors *)

let of_rsp_get_board_errors msg =
  let iter = Cbuffer.iter (fun _ -> Some sizeof_t2mi_frame_seq_item)
                          (fun buf -> Cbuffer.LE.get_uint32 buf 0)
                          (get_board_errors_errors msg) in
  let errors = List.rev @@ Cbuffer.fold (fun acc el -> el :: acc) iter []
               |> CCList.foldi (fun acc i x -> if x = 0l then acc
                                               else (match i with
                                                     | 0  -> Unknown_request x
                                                     | 1  -> Too_many_args x
                                                     | 2  -> Msg_queue_overflow x
                                                     | 3  -> Not_enough_memory x
                                                     | 4  -> Total_packets_overflow x
                                                     | 5  -> Tables_overflow x
                                                     | 6  -> Sections_overflow x
                                                     | 7  -> Table_list_overflow x
                                                     | 8  -> Services_overflow x
                                                     | 9  -> Es_overflow x
                                                     | 10 -> Ecm_overflow x
                                                     | 11 -> Emm_overflow x
                                                     | 12 -> Section_array_not_found x
                                                     | 13 -> Dma_error x
                                                     | 14 -> Pcr_freq_error x
                                                     | 15 -> Packets_overflow x
                                                     | 16 -> Streams_overflow x
                                                     | _  -> assert false) :: acc) [] in
  { count = get_board_errors_count msg; errors}

(* Get t2mi frames sequence *)

let of_rsp_get_t2mi_frame_seq msg =
  let iter = Cbuffer.iter (fun _ -> Some sizeof_t2mi_frame_seq_item) (fun buf -> buf) msg in
  Cbuffer.fold (fun acc el -> let typ    = get_t2mi_frame_seq_item_typ el in
                              let frame  = get_t2mi_frame_seq_item_frame el in
                              let common = { id          = typ
                                           ; super_frame = get_t2mi_frame_seq_item_sframe el
                                           ; count       = Int32.to_int @@ get_t2mi_frame_seq_item_count el
                                           } in
                              (match typ with
                               | 0x00 -> BB { common
                                            ; frame
                                            ; plp = get_t2mi_frame_seq_item_plp el
                                            }
                               | 0x01 -> Aux_stream_iq_data common
                               | 0x02 -> Arbitrary_cell_insertion { common; frame }
                               | 0x10 -> L1_current { common
                                                    ; frame
                                                    ; dyn_cur_frame = get_t2mi_frame_seq_item_dyn1_frame el
                                                    }
                               | 0x11 -> L1_future { common
                                                   ; frame
                                                   ; dyn_next_frame = get_t2mi_frame_seq_item_dyn1_frame el
                                                   ; dyn_next2_frame = get_t2mi_frame_seq_item_dyn2_frame el
                                                   }
                               | 0x12 -> P2_bias_balancing_cells { common; frame }
                               | 0x20 -> Timestamp common
                               | 0x21 -> Individual_addressing common
                               | 0x30 -> FEF_null common
                               | 0x31 -> FEF_iq common
                               | 0x32 -> FEF_composite common
                               | 0x33 -> FEF_sub_part common
                               | _    -> Unknown common) :: acc)
               iter []
  |> List.rev

(* Get jitter *)

let of_rsp_get_jitter (req_ptr,msg) =
  let hdr,bdy' = Cbuffer.split msg sizeof_jitter in
  let count    = get_jitter_count hdr in
  let bdy,_    = Cbuffer.split bdy' @@ sizeof_jitter_item * count in
  let iter     = Cbuffer.iter (fun _ -> Some sizeof_jitter_item) (fun buf -> buf) bdy in
  let values   = List.rev @@ Cbuffer.fold (fun acc el -> ({ status   = get_jitter_item_status el
                                                          ; d_packet = get_jitter_item_d_packet el
                                                          ; d_pcr    = get_jitter_item_d_pcr el
                                                          ; drift    = get_jitter_item_drift el
                                                          ; fo       = get_jitter_item_fo el
                                                          ; jitter   = get_jitter_item_jitter el }) :: acc)
                                          iter [] in
  { pid         = get_jitter_pid hdr
  ; time        = get_jitter_time hdr
  ; req_ptr
  ; next_ptr    = get_jitter_req_next hdr
  ; bitrate     = get_jitter_bitrate hdr
  ; t_pcr       = get_jitter_t_pcr hdr
  ; packet_time = get_jitter_packet_time hdr
  ; k_drift     = get_jitter_k_drift hdr
  ; k_fo        = get_jitter_k_fo hdr
  ; k_jitter    = get_jitter_k_jitter hdr
  ; values
  }

(* Get struct *)

let of_streams_list version buf =
  let iter = Cbuffer.iter (fun _ -> Some 4) (fun buf -> Cbuffer.LE.get_uint32 buf 0) buf in
  { version; streams = List.rev @@ Cbuffer.fold (fun acc el -> (Common.Stream.of_int32 el) :: acc) iter [] }

let of_general_struct_block msg =
  let bdy,rest   = Cbuffer.split msg sizeof_general_struct_block in
  let string_len = get_general_struct_block_string_len bdy in
  let nw_pid'    = get_general_struct_block_network_pid bdy in
  let strings,_  = Cbuffer.split rest (string_len * 2) in
  let nw_name,_  = Cbuffer.split strings string_len in
  { services_num = get_general_struct_block_services_num bdy
  ; nw_pid       = nw_pid' land 0x1FFF
  ; complete     = nw_pid' land 0x4000 > 0
  ; ts_id        = get_general_struct_block_ts_id bdy
  ; nw_id        = get_general_struct_block_nw_id bdy
  ; orig_nw_id   = get_general_struct_block_orig_nw_id bdy
  ; nw_name      = Cbuffer.to_string nw_name
  }

let of_pids_struct_block msg =
  let iter = Cbuffer.iter (fun _ -> Some 2) (fun buf -> Cbuffer.LE.get_uint16 buf 0) msg in
  Cbuffer.fold (fun acc el -> { pid       = el land 0x1FFF
                              ; has_pts   = el land 0x8000 > 0
                              ; scrambled = el land 0x4000 > 0
                              ; present   = el land 0x2000 > 0 } :: acc)
               iter []
  |> List.rev

let of_services_struct_block string_len msg =
  let bdy,rest   = Cbuffer.split msg sizeof_services_struct_block in
  let flags      = get_services_struct_block_flags bdy in
  let strings,_  = Cbuffer.split rest (string_len * 2) in
  let sn,pn      = Cbuffer.split strings string_len in
  { id            = get_services_struct_block_id bdy
  ; name          = Cbuffer.to_string sn
  ; provider_name = Cbuffer.to_string pn
  ; pmt_pid       = get_services_struct_block_pmt_pid bdy
  ; pcr_pid       = get_services_struct_block_pcr_pid bdy
  ; has_pmt       = flags land 0x8000 > 0
  ; has_sdt       = flags land 0x4000 > 0
  ; dscr          = flags land 0x2000 > 0
  ; list_dscr     = flags land 0x1000 > 0
  ; eit_schedule  = flags land 0x0080 > 0
  ; eit_pf        = flags land 0x0040 > 0
  ; free_ca_mode  = flags land 0x0020 > 0
  ; es            = []
  ; ecm           = []
  }

let of_es_struct_block msg =
  let iter     = Cbuffer.iter (fun _ -> Some 4) (fun buf -> buf) msg in
  Cbuffer.fold (fun acc x -> let pid' = get_es_struct_block_pid x in
                             { pid          = pid' land 0x1FFF
                             ; has_pts      = pid' land 0x8000 > 0
                             ; es_type      = get_es_struct_block_es_type x
                             ; es_stream_id = get_es_struct_block_es_stream_id x
                             } :: acc)
               iter []
  |> List.rev

let of_ecm_struct_block msg =
  let iter     = Cbuffer.iter (fun _ -> Some 4) (fun buf -> buf) msg in
  Cbuffer.fold (fun acc x -> { pid       = get_ecm_struct_block_pid x land 0x1FFF
                             ; ca_sys_id = get_ecm_struct_block_ca_system_id x} :: acc)
               iter []
  |> List.rev

let of_table_struct_block msg =
  let bdy,rest = Cbuffer.split msg sizeof_table_struct_block in
  let iter     = Cbuffer.iter (fun _ -> Some 2)
                              (fun buf -> Cbuffer.LE.get_uint16 buf 0) rest in
  let sections = Cbuffer.fold (fun acc x -> { id = List.length acc
                                            ; analyzed = x land 0x8000 > 0
                                            ; length   = x land 0x0FFF } :: acc) iter []
                 |> List.filter (fun x -> x.length > 0)
                 |> List.rev in
  let pid'     = get_table_struct_block_pid msg in
  let id       = get_table_struct_block_id msg in
  let id_ext   = get_table_struct_block_id_ext bdy in
  let common   =  { version        = get_table_struct_block_version bdy
                  ; id             = get_table_struct_block_id bdy
                  ; pid            = pid' land 0x1FFF
                  ; lsn            = get_table_struct_block_lsn bdy
                  ; section_syntax = pid' land 0x8000 > 0
                  ; sections
                  } in
  let eit_info = { ts_id         = get_table_struct_block_adv_info_1 bdy
                 ; orig_nw_id    = get_table_struct_block_adv_info_2 bdy
                 ; segment_lsn   = get_table_struct_block_adv_info_3 bdy
                 ; last_table_id = get_table_struct_block_adv_info_4 bdy
                 }  in
  (match id with
   | 0x00 -> PAT    { common; ts_id = id_ext }
   | 0x01 -> CAT    common
   | 0x02 -> PMT    { common; program_number = id_ext }
   | 0x03 -> TSDT   common
   | 0x40 -> NIT_a  { common; nw_id = id_ext }
   | 0x41 -> NIT_o  { common; nw_id = id_ext }
   | 0x42 -> SDT_a  { common; ts_id = id_ext }
   | 0x46 -> SDT_o  { common; ts_id = id_ext }
   | 0x4A -> BAT    { common; bouquet_id = id_ext }
   | 0x4E -> EIT_ap { common; service_id = id_ext; eit_info }
   | 0x4F -> EIT_op { common; service_id = id_ext; eit_info }
   | x when x >= 0x50 && x <= 0x5F -> EIT_as { common; service_id = id_ext; eit_info }
   | x when x >= 0x60 && x <= 0x6F -> EIT_os { common; service_id = id_ext; eit_info }
   | 0x70 -> TDT common
   | 0x71 -> RST common
   | 0x72 -> ST  common
   | 0x73 -> TOT common
   | 0x7E -> DIT common
   | 0x7F -> SIT common
   | _    -> Unknown common)

let rec of_ts_struct_blocks msg acc =
  match Cbuffer.len msg with
  | 0 -> acc
  | _ -> let hdr,data   = Cbuffer.split msg sizeof_struct_block_header in
         let typ        = get_struct_block_header_code hdr in
         let len        = get_struct_block_header_length hdr in
         let block,rest = Cbuffer.split data len in
         (match typ with
          | 0x2000 -> `General (of_general_struct_block block)
          | 0x2100 -> `Pids (of_pids_struct_block block)
          | 0x2200 -> `Services (of_services_struct_block 32 block) (* FIXME string length *)
          | 0x2201 -> `Es (of_es_struct_block block)
          | 0x2202 -> `Ecm (of_ecm_struct_block block)
          | 0x2300 -> `Emm (of_ecm_struct_block block)
          | 0x2400 -> `Tables (of_table_struct_block block)
          | _      -> `Unknown)
         |> (function
             | `Es es -> (match acc with
                          | [] -> failwith "of_ts_struct_blocks: no blocks before es block"
                          | hd::tl ->
                             (match hd with
                              | `Services s -> `Services ({ s with es = es }) :: tl
                                               |> of_ts_struct_blocks rest
                              | _ -> failwith "of_ts_struct_blocks: no services block before es block"))
             | `Ecm ecm -> (match acc with
                            | [] -> failwith "of_ts_struct_blocks: no blocks before ecm block"
                            | hd::tl ->
                               (match hd with
                                | `Services s -> `Services ({ s with ecm = ecm }) :: tl
                                                 |> of_ts_struct_blocks rest
                                | _ -> failwith "of_ts_struct_blocks: no services block before ecm block"))
             | x -> of_ts_struct_blocks rest (x :: acc))

let of_ts_struct msg =
  let open CCOpt in
  let hdr,rest = Cbuffer.split msg sizeof_ts_struct in
  let len      = (Int32.to_int @@ get_ts_struct_length hdr) in
  let bdy,rest = Cbuffer.split rest len in
  let blocks   = of_ts_struct_blocks bdy [] in
  { stream_id = Common.Stream.of_int32 @@ get_ts_struct_stream_id hdr
  ; general   = get_exn @@ CCList.find_map (function `General x -> Some x | _ -> None) blocks
  ; pids      = get_exn @@ CCList.find_map (function `Pids x -> Some x | _ -> None) blocks
  ; services  = CCList.filter_map (function `Services x -> Some x | _ -> None) blocks
  ; emm       = get_exn @@ CCList.find_map (function `Emm x -> Some x | _ -> None) blocks
  ; tables    = CCList.filter_map (function `Tables x -> Some x | _ -> None) blocks
  }, if Cbuffer.len rest > 0 then Some rest else None

let of_rsp_get_ts_structs (version,msg) =
  let hdr,bdy'     = Cbuffer.split msg sizeof_ts_structs in
  let count        = get_ts_structs_count hdr in
  let streams,bdy  = Cbuffer.split bdy' (count * 4) in
  let streams_list = of_streams_list version streams in
  let rec parse    = (fun acc buf -> let x,rest = of_ts_struct buf in
                                     match rest with
                                     | Some b -> parse (x :: acc) b
                                     | None   -> List.rev (x :: acc)) in
  let structs      = parse [] bdy in
  streams_list, structs

(* Get bitrate *)

let of_pids_bitrate total_pids br_per_pkt buf =
  let msg,rest = Cbuffer.split buf (sizeof_pid_bitrate * total_pids) in
  let iter     = Cbuffer.iter (fun _ -> Some sizeof_pid_bitrate) (fun buf -> buf) msg in
  let pids     = (Cbuffer.fold (fun acc el ->
                      let packets = get_pid_bitrate_packets el in
                      { pid     = get_pid_bitrate_pid el land 0x1FFF
                      ; bitrate = int_of_float @@ br_per_pkt *. (Int32.to_float packets) } :: acc)
                               iter []) in
  List.rev pids, rest

let of_tbls_bitrate total_tbls br_per_pkt buf =
  let msg,_ = Cbuffer.split buf (sizeof_table_bitrate * total_tbls) in
  let iter  = Cbuffer.iter (fun _ -> Some sizeof_table_bitrate) (fun buf -> buf) msg in
  Cbuffer.fold (fun acc el -> let packets    = get_table_bitrate_packets el in
                              let flags      = get_table_bitrate_flags el in
                              let adv_info_1 = get_table_bitrate_adv_info_1 el in
                              let adv_info_2 = get_table_bitrate_adv_info_2 el in
                              { id             = get_table_bitrate_table_id el
                              ; id_ext         = get_table_bitrate_table_id_ext el
                              ; fully_analyzed = flags land 2 > 0
                              ; section_syntax = flags land 1 > 0
                              ; eit_info       = Some (adv_info_1, adv_info_2)
                              ; bitrate        = int_of_float @@ br_per_pkt *. (Int32.to_float packets) } :: acc)
               iter []
  |> List.rev

let of_stream_bitrate buf =
  let length     = (Int32.to_int @@ get_stream_bitrate_length buf) in
  let msg,rest   = Cbuffer.split buf (length + 8) in
  let hdr,bdy    = Cbuffer.split msg sizeof_stream_bitrate in
  let ts_bitrate = Int32.to_int @@ get_stream_bitrate_ts_bitrate hdr in
  let total_pkts = get_stream_bitrate_total_packets hdr in
  let br_per_pkt = (float_of_int ts_bitrate) /. (Int32.to_float total_pkts)  in
  let total_pids = get_stream_bitrate_total_pids hdr in
  let total_tbls = get_stream_bitrate_total_tables hdr in
  let pids,tbls  = of_pids_bitrate total_pids br_per_pkt bdy in
  let tables     = of_tbls_bitrate total_tbls br_per_pkt tbls in
  { stream_id  = Common.Stream.of_int32 @@ get_stream_bitrate_stream_id hdr
  ; ts_bitrate
  ; pids
  ; tables
  }, if Cbuffer.len rest > 0 then Some rest else None

let of_rsp_get_bitrates (version,msg) =
  let hdr,bdy   = Cbuffer.split msg sizeof_bitrates in
  let count     = get_bitrates_count hdr in
  let rec parse = (fun acc buf -> let x,rest = of_stream_bitrate buf in
                                  match rest with
                                  | Some b -> parse (x :: acc) b
                                  | None   -> List.rev (x :: acc)) in
  let bitrates  = if count > 0 then parse [] bdy else [] in
  let streams   = List.map (fun (x:bitrate) -> x.stream_id) bitrates in
  if List.length streams <> count
  then failwith (Printf.sprintf "of_rsp_get_bitrates: expected %d streams, got %d" count (List.length streams))
  else { version; streams}, bitrates

(* Get t2mi info *)

(* Get streams list *)

let of_rsp_get_streams (version,msg) =
  let hdr,bdy' = Cbuffer.split msg sizeof_streams_list in
  let count    = get_streams_list_count hdr in
  let bdy,_    = Cbuffer.split bdy' (count * 4) in
  of_streams_list version bdy

(* Status *)

let of_status msg =
  let iter     = fun x -> Cbuffer.iter (fun _ -> Some 1) (fun buf -> Cbuffer.get_uint8 buf 0) x in
  let flags    = get_status_flags msg in
  let has_sync = not (flags land 0x04 > 0) in
  let ts_num   = get_status_ts_num msg in
  let flags2   = get_status_flags_2 msg in
  { user_status = { load             = (float_of_int ((get_status_load msg) * 100)) /. 255.
                  ; mode             = to_mode_exn (get_status_mode msg)
                                                   (get_status_t2mi_pid msg)
                                                   (get_status_t2mi_stream_id msg)
                  ; jitter_mode      = { stream_id = Common.Stream.of_int32 (get_status_jitter_stream_id msg)
                                       ; pid       = get_status_jitter_pid msg
                                       }
                  ; ts_num           = if has_sync then ts_num else 0
                  ; services_num     = if has_sync then get_status_services_num msg else 0
                  ; bitrate          = Int32.to_int @@ get_status_bitrate msg
                  ; packet_sz        = if      (flags land 0x08) <> 0 then Ts192
                                       else if (flags land 0x10) <> 0 then Ts204
                                       else Ts188
                  ; has_stream       = flags  land 0x80 = 0
                  }
  ; has_board_errs   = flags  land 0x20 <> 0
  ; reset_flag       = flags2 land 0x02 <> 0
  ; ts_sync_lst      = Cbuffer.fold (fun acc el -> (int_to_bool_list el) @ acc)
                                    (iter @@ get_status_ts_absent_lst msg) []
                       |> CCList.rev
                       |> CCList.take ts_num
                       |> List.map (fun x -> if has_sync then not x else false)
  ; ts_verified_lst  = Cbuffer.fold (fun acc el -> (int_to_bool_list el) @ acc)
                                    (iter @@ get_status_ts_not_verified_lst msg) []
                       |> CCList.rev
                       |> CCList.take ts_num
                       |> List.map not
  ; streams_ver      = get_status_streams_ver msg
  ; ts_ver_com       = get_status_ts_ver_com msg
  ; ts_ver_lst       = Cbuffer.fold (fun acc el -> el :: acc)
                                    (iter @@ get_status_ts_ver_lst msg) []
                       |> CCList.rev
                       |> CCList.take ts_num
  ; t2mi_sync_lst    = int_to_bool_list (get_status_t2mi_sync msg)
                       |> (fun l -> CCList.foldi (fun acc i x -> if x then i :: acc else acc) [] l)
  ; t2mi_ver_lst     = get_status_t2mi_ver_lst msg
                       |> (fun v -> CCList.map (fun x -> Int32.shift_right v (4 * x)
                                                         |> Int32.logand 0xfl
                                                         |> Int32.to_int)
                                               (CCList.range 0 7))
                       |> List.rev
  ; streams          = []
  }

(* Ts errors *)

let of_ts_errors msg =
  let common,rest = Cbuffer.split msg sizeof_ts_errors in
  let number      = get_ts_errors_count common in
  let errors,_    = Cbuffer.split rest (number * sizeof_ts_error) in
  let iter        = Cbuffer.iter (fun _ -> Some sizeof_ts_error) (fun buf -> buf) errors in
  let errors      = Cbuffer.fold (fun acc el -> let pid'      = get_ts_error_pid el in
                                                let pid       = pid' land 0x1FFF in
                                                let multi_pid = if (pid' land 0x8000) > 0 then true else false in
                                                { count     = get_ts_error_count el
                                                ; err_code  = get_ts_error_err_code el
                                                ; err_ext   = get_ts_error_err_ext el
                                                ; multi_pid
                                                ; pid
                                                ; packet    = get_ts_error_packet el
                                                ; param_1   = get_ts_error_param_1 el
                                                ; param_2   = get_ts_error_param_2 el
                                                } :: acc)
                                 iter [] in
  { stream_id = Common.Stream.of_int32 (get_ts_errors_stream_id common); errors }

(* T2-MI errors *)

let of_t2mi_errors msg =
  let common,rest = Cbuffer.split msg sizeof_t2mi_errors in
  let number      = get_t2mi_errors_count common in
  let errors,_    = Cbuffer.split rest (number * sizeof_t2mi_error) in
  let iter        = Cbuffer.iter (fun _ -> Some sizeof_t2mi_error) (fun buf -> buf) errors in
  let parser_errs = get_t2mi_errors_err_flags common in
  let errors      = Cbuffer.fold (fun acc el -> let _index   = get_t2mi_error_index el in
                                                let _data    = get_t2mi_error_data el in
                                                let cnt_flag = _index land 8 = 0 in
                                                let index    = _index lsr 4 in
                                                if (not cnt_flag) && index = 0 then acc
                                                else { count          = if cnt_flag then Some _data else None
                                                     ; err_code       = _index lsr 4
                                                     ; t2mi_stream_id = _index land 7
                                                     ; param          = if cnt_flag then None else Some _data
                                                     } :: acc)
                                 iter [] in
  { stream_id        = Common.Stream.of_int32 (get_t2mi_errors_stream_id common)
  ; t2mi_pid         = get_t2mi_errors_pid common
  ; sync             = int_to_t2mi_sync_list (get_t2mi_errors_sync common)
  ; ts_parser_errors = CCList.filter_map (fun x -> if CCOpt.is_some x then x else None)
                                         [ if parser_errs land 1 > 0 then Some Af_too_long_for_new_packet else None
                                         ; if parser_errs land 2 > 0 then Some Af_too_long else None
                                         ; if parser_errs land 4 > 0 then Some Pf_out_of_bounds else None
                                         ; if parser_errs land 8 > 0 then Some Packet_intersection else None
                                         ]
  ; errors
  }

(* ----------------- Message deserialization ---------------- *)

type err = Bad_prefix           of int
         | Bad_length           of int
         | Bad_msg_code         of int
         | Bad_crc              of int * int * int
         | No_prefix_after_msg  of int
         | Insufficient_payload of Cbuffer.t
         | Unknown_err          of string

let string_of_err = function
  | Bad_prefix x           -> "incorrect prefix: " ^ (string_of_int x)
  | Bad_length x           -> "incorrect length: " ^ (string_of_int x)
  | Bad_msg_code x         -> "incorrect code: "   ^ (string_of_int x)
  | Bad_crc (code,x,y)     -> (Printf.sprintf "incorrect crc in msg with code = 0x%x" code)
                              ^ ", expected " ^ (string_of_int x) ^ ", got " ^ (string_of_int y)
  | No_prefix_after_msg x  -> (Printf.sprintf "no prefix found after message payload, code = 0x%x" x)
  | Insufficient_payload _ -> "insufficient payload"
  | Unknown_err s          -> s

let check_prefix buf =
  let prefix' = get_common_header_prefix buf in
  if prefix <> prefix' then Error (Bad_prefix prefix') else Ok buf

let check_msg_code buf =
  let hdr,rest = Cbuffer.split buf sizeof_common_header in
  let code     = get_common_header_msg_code hdr in
  let has_crc  = (code land 2) > 0 in
  let length   = (match code lsr 8 with
                  | 0x01 -> Some sizeof_board_info                               (* board info*)
                  | 0x02 -> Some sizeof_board_mode                               (* board mode *)
                  | 0x03 -> Some sizeof_status                                   (* status *)
                  | 0x04 -> Some ((get_ts_errors_length rest * 2) + 2)           (* ts errors *)
                  | 0x05 -> Some ((get_t2mi_errors_length rest * 2) + 2)         (* t2mi errors *)
                  | 0x09 -> Some ((get_complex_rsp_header_length rest * 2) + 2)  (* complex response *)
                  | 0xFD -> Some 4                                               (* end of errors *)
                  | 0xFF -> Some 0                                               (* end of transmission *)
                  | _    -> None) in
  match length with
  | Some x -> Ok (x + (if has_crc then 2 else 0), has_crc, code, rest)
  | None   -> Error (Bad_msg_code code)

let check_length (len,has_crc,code,rest') =
  if len > 512 - sizeof_common_header
  then Error (Bad_length len)
  else let body,rest = Cbuffer.split rest' len in
       Ok (has_crc,code,body,rest)

let check_next_prefix ((code,_,rest) as x) =
  if Cbuffer.len rest < sizeof_common_header
  then Ok x
  else (match check_prefix rest with
        | Ok _    -> Ok x
        | Error _ -> Error (No_prefix_after_msg code))

let check_crc (code,body,rest) =
  let b         = Cbuffer.create 2 |> (fun b -> Cbuffer.LE.set_uint16 b 0 code; b) in
  let body,crc' = Cbuffer.split body ((Cbuffer.len body) - 2) in
  let iter      = Cbuffer.iter (fun _ -> Some 2) (fun buf -> Cbuffer.LE.get_uint16 buf 0)
                               (Cbuffer.append b body) in
  let crc       = (Cbuffer.fold (fun acc el -> el + acc) iter 0) land 0xFFFF in
  let crc'      = Cbuffer.LE.get_uint16 crc' 0 in
  if crc <> crc' then Error (Bad_crc (code,crc,crc')) else Ok (code,body,rest)

let get_msg buf =
  try
    CCResult.(check_prefix buf
              >>= check_msg_code
              >>= check_length
              >>= (fun (has_crc,code,body,rest) -> if has_crc
                                             then check_crc (code,body,rest)
                                             else check_next_prefix (code,body,rest)))
  with
  | Invalid_argument _ -> Error (Insufficient_payload buf)
  | e                  -> Error (Unknown_err (Printexc.to_string e))

let parse_simple_msg = fun (code,body,parts) ->
  try
    (match code lsr 8 with
     | 0x01 -> `R (`Board_info body)
     | 0x02 -> `R (`Board_mode body)
     | 0x03 -> `E (`Status (of_status body))
     | 0x04 -> `E (`Ts_errors body)
     | 0x05 -> `E (`T2mi_errors body)
     | 0x09 -> let code_ext   = get_complex_rsp_header_code_ext body in
               let long       = code_ext land 0x2000 > 0 in
               let parity     = if code_ext land 0x1000 > 0 then 1 else 0 in
               let _,data'    = if long then Cbuffer.split body sizeof_complex_rsp_header_ext
                                else Cbuffer.split body sizeof_complex_rsp_header in
               let data,_     = Cbuffer.split data' (Cbuffer.len data' - parity) in
               let part       = { first = code_ext land 0x8000 > 0
                                ; param = Int32.mul 2l (if long then get_complex_rsp_header_ext_param body
                                                        else Int32.of_int @@ get_complex_rsp_header_param body)
                                ; data
                                } in
               let code       = code_ext land 0x0FFF in
               let request_id = get_complex_rsp_header_request_id body in
               `P (CCList.Assoc.update (code,request_id)
                                       ~f:(function
                                           | Some x -> Some (part :: x)
                                           | None   -> Some ([part]))
                                       parts)
     | _ -> `N)
  with e -> io @@ Printexc.to_string e; `N

let parse_complex_msg = fun ((code,r_id),msg) ->
  (* no parsing here, only tag wrapping *)
  try
    let x = (r_id,msg) in
    (match code with
     | 0x0110 -> `ER (`Board_errors x)
     | 0x0306 -> `R  (`T2mi_frame_seq x)
     | 0x0307 -> `ER (`Jitter (r_id,(get_jitter_req_ptr msg),msg))
     | 0x0309 -> `ER (`Struct (r_id,(get_ts_structs_version msg),msg))
     | 0x030A -> `ER (`Bitrates (r_id,(get_bitrates_version msg),msg))
     | 0x030C -> `ER (`Streams (r_id,(get_streams_list_version msg),msg))
     | _      -> `N)
  with _ -> `N

let try_compose_parts ((id,gp) as x) =
  let gp = CCList.sort (fun x y -> if x.first then (-1)
                                   else if y.first then 1
                                   else Int32.compare x.param y.param) gp in
  let first,rest = CCList.hd_tl gp in
  try (let acc = List.fold_left (fun acc x -> if x.param = Int32.of_int (Cbuffer.len acc)
                                              then Cbuffer.append acc x.data
                                              else failwith "Incorrect part offset")
                                first.data rest in
       if first.param = Int32.of_int (Cbuffer.len acc) then `F (id,acc) else `P x)
  with _ -> `N

let deserialize parts buf =
  let rec f events event_rsps rsps parts b =
    if Cbuffer.len b >= sizeof_common_header
    then (match get_msg b with
          | Ok (code,bdy,rest) -> (match parse_simple_msg (code,bdy,parts) with
                                   | `E x  -> f (x::events) event_rsps rsps parts rest
                                   | `ER x -> f events (x::event_rsps) rsps parts rest
                                   | `R x  -> f events event_rsps (x::rsps) parts rest
                                   | `P x  -> f events event_rsps rsps x rest
                                   | `N    -> f events event_rsps rsps parts rest)
          | Error e ->
             (match e with
              | Insufficient_payload x -> (List.rev events, List.rev event_rsps, List.rev rsps, List.rev parts, x)
              | _ -> (match e with
                      | Bad_prefix _ -> ()
                      | _            -> io (string_of_err e); io (Cbuffer.pp buf));
                     Cbuffer.split b 1 |> fun (_,x) -> f events event_rsps rsps parts x))
    else (List.rev events, List.rev event_rsps, List.rev rsps, List.rev parts, b) in
  let ev,ev_rsps,rsps,parts,res = f [] [] [] parts buf in
  let parts = List.filter (fun (_,x) -> let first_msgs = CCList.find_all (fun x -> x.first) x in
                                        match first_msgs with
                                        | [_] -> true
                                        | _   -> false) parts in
  let ev_rsps,rsps,parts = List.fold_left (fun acc x -> let e,r,p = acc in
                                                        (match try_compose_parts x with
                                                         | `F x -> (match (parse_complex_msg x) with
                                                                    | `ER x -> x::e,r,p
                                                                    | `R x -> e,x::r,p
                                                                    | `N   -> acc)
                                                         | `P x -> e,r,x::p
                                                         | `N   -> e,r,p))
                                          (ev_rsps,rsps,[]) parts in
  ev, ev_rsps, rsps, parts, if Cbuffer.len res > 0 then Some res else None

let try_parse f x = try Some (f x) with _ -> None

let parse_get_board_info = function
  | `Board_info buf -> try_parse of_rsp_get_board_info buf
  |  _ -> None

let parse_get_board_mode = function
  | `Board_mode buf -> try_parse of_rsp_get_board_mode buf
  | _ -> None

let parse_get_board_errors req_id = function
  | `Board_errors (r_id,buf) -> if req_id <> r_id then None
                                else (match try_parse of_rsp_get_board_errors buf with
                                      | Some x -> Some (Board_errors x)
                                      | None   -> None)
  | _ -> None

let parse_get_t2mi_frame_seq req_id = function
  | `T2mi_frame_seq (r_id,buf) -> if req_id <> r_id then None
                                  else try_parse of_rsp_get_t2mi_frame_seq buf
  | _ -> None

let parse_get_jitter (req : jitter_req) = function
  | `Jitter (r_id,pointer,buf) -> if req.request_id <> r_id then None
                                  else if req.pointer <> pointer then None
                                  else (match try_parse of_rsp_get_jitter (pointer,buf) with
                                        | Some x -> Some (Jitter x)
                                        | None   -> None)
  | _ -> None

let parse_get_ts_structs (req : ts_req) = function
  | `Struct (r_id,version,buf) -> if req.request_id <> r_id then None
                                  else if req.version <> version then None
                                  else (match try_parse of_rsp_get_ts_structs (version,buf) with
                                        | Some x -> Some (Struct x)
                                        | None   -> None)
  | _ -> None

let parse_get_bitrates (req : ts_req) = function
  | `Bitrates (r_id,version,buf) -> if req.request_id <> r_id then None
                                    else if req.version <> version then None
                                    else (match try_parse of_rsp_get_bitrates (version,buf) with
                                          | Some x -> Some (Bitrate x)
                                          | None   -> None)
  | _ -> None

let parse_get_streams (req : ts_req) = function
  | `Streams (r_id,version,buf) -> if req.request_id <> r_id then None
                                   else if req.version <> version then None
                                   else (match try_parse of_rsp_get_streams (version,buf) with
                                         | Some x -> Some (Streams x)
                                         | None   -> None)
  | _ -> None

let is_response (type a) (req : a request) msg : a option =
  match req with
  | Get_board_info            -> parse_get_board_info msg
  | Get_board_mode            -> parse_get_board_mode msg
  | Get_t2mi_frame_seq (id,_) -> parse_get_t2mi_frame_seq id msg

let is_event (type a) (req : a event_request) msg : a option =
  match req with
  | Get_board_errors id -> parse_get_board_errors id msg
  | Get_jitter x        -> parse_get_jitter x msg
  | Get_ts_structs x    -> parse_get_ts_structs x msg
  | Get_bitrates x      -> parse_get_bitrates x msg
  | Get_t2mi_info _     -> None
  | Get_streams x       -> parse_get_streams x msg
