open Common
open Common.Dvb_t2_types
open Common.Topology
open Containers

(** Board info **)

type devinfo =
  { typ : int
  ; ver : int
  } [@@deriving yojson]

(** Modes **)

type input = SPI | ASI [@@deriving yojson,show,eq]

let input_to_string = function SPI -> "SPI" | ASI -> "ASI"

type t2mi_mode =
  { enabled        : bool
  ; pid            : int
  ; t2mi_stream_id : int
  ; stream         : Stream.id
  } [@@deriving yojson,eq,show]

type jitter_mode =
  { stream  : Stream.id
  ; pid     : int
  } [@@deriving yojson,eq,show]

(** Config **)

type config =
  { input       : input
  ; t2mi_mode   : t2mi_mode option
  ; jitter_mode : jitter_mode option
  } [@@deriving yojson,eq]

let t2mi_mode_default   = { enabled = false; pid = 0; t2mi_stream_id = 0; stream = Single }
let jitter_mode_default = { pid = 0x1fff; stream = Single }

let config_to_string c = Yojson.Safe.to_string @@ config_to_yojson c
let config_of_string s = config_of_yojson @@ Yojson.Safe.from_string s

let config_default =
  { input       = ASI
  ; t2mi_mode   = None
  ; jitter_mode = None
  }

type packet_sz = Ts188 | Ts192 | Ts204 [@@deriving show,eq]
let packet_sz_to_string : packet_sz -> string = function
  | Ts188 -> "Ts188"
  | Ts192 -> "Ts192"
  | Ts204 -> "Ts204"
let packet_sz_of_string_option : string -> packet_sz option = function
  | "Ts188" -> Some Ts188
  | "Ts192" -> Some Ts192
  | "Ts204" -> Some Ts204
  | _       -> None
let packet_sz_to_yojson x : Yojson.Safe.json = `String (packet_sz_to_string x)
let packet_sz_of_yojson = function
  | `String s -> (match packet_sz_of_string_option s with
                  | Some x -> Ok x
                  | None   -> Error (Printf.sprintf "packet_sz_of_yojson: bad string (%s)" s))
  | x         -> Error (Printf.sprintf "packet_sz_of_yojson: not string value (%s)" @@ Yojson.Safe.to_string x)

type status =
  { timestamp    : Time.t
  ; load         : float
  ; ts_num       : int
  ; services_num : int
  ; bitrate      : int
  ; packet_sz    : packet_sz
  ; has_sync     : bool
  ; has_stream   : bool
  } [@@deriving yojson,show,eq]

type reset_ts =
  { timestamp : Time.t
  }

type statuses = status list [@@deriving yojson]

(** Board errors **)

type board_error =
  { timestamp : Time.t
  ; err_code  : int
  ; count     : int
  } [@@deriving yojson]

type board_errors = board_error list [@@deriving yojson]

module Jitter = struct

  type session =
    { timestamp : Time.t
    ; t_pcr     : float
    ; mode      : jitter_mode
    } [@@deriving yojson,eq]

  type measure =
    { discont_err : bool
    ; discont_ok  : bool
    ; t_pcr       : float
    (* for charts *)
    ; accuracy    : float
    ; jitter      : int
    ; drift       : float
    ; fo          : float
    ; period      : float
    } [@@deriving yojson]

  type measures = measure list [@@deriving yojson]

  type archive_item =
    { session  : session
    ; measures : measure list
    } [@@deriving yojson]

  type archive = archive_item list [@@deriving yojson]

end

module Streams = struct

  module TS = struct

    (** TS bitrate *)

    type bitrate =
      { timestamp : Time.t
      ; total     : int
      ; pids      : (int * int) list
      } [@@deriving yojson]

    (** TS structure *)

    type pid_type =
      | SEC of int list
      | PES of int
      | ECM of int
      | EMM of int
      | Private
      | Null [@@deriving yojson, eq]

    type pid_info =
      { pid       : int
      ; has_pts   : bool
      ; has_pcr   : bool
      ; scrambled : bool
      ; present   : bool
      ; service   : string option
      ; pid_type  : pid_type
      } [@@deriving yojson, eq]

    type es_info =
      { pid          : int
      ; has_pcr      : bool
      ; has_pts      : bool
      ; es_type      : int
      ; es_stream_id : int
      } [@@deriving yojson, eq]

    type ecm_info =
      { pid       : int
      ; ca_sys_id : int
      } [@@deriving yojson, eq]

    type service_info =
      { id             : int
      ; name           : string
      ; provider_name  : string
      ; pmt_pid        : int
      ; pcr_pid        : int
      ; has_pmt        : bool
      ; has_sdt        : bool
      ; dscr           : bool
      ; list_dscr      : bool
      ; eit_schedule   : bool
      ; eit_pf         : bool
      ; free_ca_mode   : bool
      ; running_status : int
      ; service_type_1 : int
      ; service_type_2 : int
      ; es             : es_info list
      ; ecm            : ecm_info list
      } [@@deriving yojson, eq]

    type emm_info = ecm_info [@@deriving yojson, eq]

    type eit_params =
      { ts_id         : int (* eit param 1*)
      ; orig_nw_id    : int (* eit param 2*)
      ; segment_lsn   : int
      ; last_table_id : int
      } [@@deriving yojson, eq]

    type table_info =
      { version        : int
      ; id             : int
      ; id_ext         : int
      ; eit_params     : eit_params
      ; pid            : int
      ; section_syntax : bool
      ; section        : int
      ; last_section   : int
      ; length         : int
      } [@@deriving yojson, eq]

    type general_info =
      { complete     : bool
      ; services_num : int
      ; nw_pid       : int
      ; ts_id        : int
      ; nw_id        : int
      ; orig_nw_id   : int
      ; nw_name      : string
      ; bouquet_name : string
      } [@@deriving yojson, eq]

    (* type structure =
     *   { timestamp : Time.t
     *   ; bitrate   : int option
     *   ; general   : general_info
     *   ; pids      : pid_info list
     *   ; services  : service_info list
     *   ; emm       : emm_info list
     *   ; tables    : table_info list
     *   } [@@deriving yojson, eq] *)

    type info =
      { timestamp : Time.t
      ; info      : general_info
      } [@@deriving yojson]

    let equal_info x y =
      equal_general_info x.info y.info

    type services =
      { timestamp : Time.t
      ; services  : service_info list
      } [@@deriving yojson]

    let equal_services x y =
      (Equal.list equal_service_info) x.services y.services

    type tables =
      { timestamp : Time.t
      ; tables    : table_info list
      } [@@deriving yojson]

    let equal_tables x y =
      (Equal.list equal_table_info) x.tables y.tables

    type pids =
      { timestamp : Time.t
      ; pids      : pid_info list
      } [@@deriving yojson]

    let equal_pids x y =
      (Equal.list equal_pid_info) x.pids y.pids

    type structure =
      { info     : info
      ; services : services
      ; tables   : tables
      ; pids     : pids
      } [@@deriving yojson]

    let equal_structure x y =
      equal_info x.info y.info
      && equal_services x.services y.services
      && equal_tables x.tables y.tables
      && equal_pids x.pids y.pids

    type table =
      [ `PAT
      | `CAT
      | `PMT
      | `TSDT
      | `NIT of ao
      | `SDT of ao
      | `BAT
      | `EIT of ao * ps
      | `TDT
      | `RST
      | `ST
      | `TOT
      | `DIT
      | `SIT
      | `Unknown of int
      ]
    and ao = [ `Actual | `Other ]
    and ps = [ `Present | `Schedule ]

    let table_of_int : int -> table = function
      | 0x00 -> `PAT
      | 0x01 -> `CAT
      | 0x02 -> `PMT
      | 0x03 -> `TSDT
      | 0x40 -> `NIT `Actual
      | 0x41 -> `NIT `Other
      | 0x42 -> `SDT `Actual
      | 0x46 -> `SDT `Other
      | 0x4A -> `BAT
      | 0x4E -> `EIT (`Actual, `Present)
      | 0x4F -> `EIT (`Other,  `Present)
      | x when x >= 0x50 && x <= 0x5F ->
         `EIT (`Actual, `Schedule)
      | x when x >= 0x60 && x <= 0x6F ->
         `EIT (`Other,  `Schedule)
      | 0x70 -> `TDT
      | 0x71 -> `RST
      | 0x72 -> `ST
      | 0x73 -> `TOT
      | 0x7E -> `DIT
      | 0x7F -> `SIT
      | x    -> `Unknown x

    let table_to_string : ?simple:bool -> table -> string =
      fun ?(simple=false) -> function
      | `PAT   -> "PAT"
      | `CAT   -> "CAT"
      | `PMT   -> "PMT"
      | `TSDT  -> "TSDT"
      | `NIT x ->
         if simple then "NIT"
         else (match x with
               | `Actual -> "NIT actual"
               | `Other  -> "NIT other")
      | `SDT x ->
         if simple then "SDT"
         else (match x with
               | `Actual -> "SDT actual"
               | `Other  -> "SDT other")
      | `BAT   -> "BAT"
      | `EIT x ->
         if simple then "EIT"
         else (match x with
               | `Actual, `Present  -> "EIT actual present"
               | `Other , `Present  -> "EIT other present"
               | `Actual, `Schedule -> "EIT actual schedule"
               | `Other , `Schedule -> "EIT other schedule")
      | `TDT   -> "TDT"
      | `RST   -> "RST"
      | `ST    -> "ST"
      | `TOT   -> "TOT"
      | `DIT   -> "DIT"
      | `SIT   -> "SIT"
      | `Unknown _ -> "Unknown"

    (** SI/PSI section **)

    type section_error = Zero_length
                       | Table_not_found
                       | Section_not_found
                       | Stream_not_found
                       | Unknown [@@deriving yojson]

    type section =
      { stream_id : Stream.id
      ; table_id  : int
      ; section   : string
      ; parsed    : Yojson.Safe.json option
      } [@@deriving yojson]

    type streams_states =
      (Stream.t * Time.t * Time.t) list [@@deriving yojson]
    type streams_unique =
      (Stream.t * [`Now | `Last of Time.t]) list [@@deriving yojson]

  end

  module T2MI = struct

    (** T2MI structure **)

    type t2mi_stream_info =
      { packets      : int list
      ; t2mi_pid     : int option
      ; l1_pre       : string option
      ; l1_post_conf : string option
      } [@@deriving yojson]

    type structure =
      { timestamp : Time.t
      ; streams   : (int * t2mi_stream_info) list
      } [@@deriving yojson]

    (** T2-MI packet sequence **)

    (* T2-MI packet types:
     * 0x00  - BB frame
     * 0x01  - Auxiliary stream I/Q data
     * 0x02  - Arbitrary cell insertion
     * 0x10  - L1 current
     * 0x11  - L1 future
     * 0x12  - P2 bias balancing cells
     * 0x20  - DVB-T2 timestamp
     * 0x21  - Individual addressing
     * 0x30  - FEF part : Null
     * 0x31  - FEF part : I/Q data
     * 0x32  - FEF part : composite
     * 0x33  - FEF sub-part
     * other - Reserved for future use
     *)

    (* NOTE suggest using fields like plp, frame, l1_param_1 and l1_param_2 as abstract parameters,
     * which have its own meaning for each packet type
     *)

    type sequence_item =
      { typ         : int (* T2-MI packet type according to TS 102 773 *)
      ; super_frame : int
      ; stream_id   : int
      ; frame       : int (* for packet types 0x00 .. 0x02, 0x10 .. 0x12 *)
      ; count       : int
      ; plp         : int (* only for BB frames *)
      ; l1_param_1  : int (* L1DYN_CURR.FRAME_IDX  for L1 current, L1DYN_NEXT.FRAME_IDX for L1 future *)
      ; l1_param_2  : int (* L1DYN_NEXT2.FRAME_IDX for L1 future *)
      ; ts_packet   : int
      } [@@deriving yojson]

    type sequence = sequence_item list [@@deriving yojson]

  end

end

module Errors = struct

  open Common.Time

  type segmentation =
    { errors     : float
    ; no_stream  : float
    ; no_measure : float
    } [@@deriving yojson]

  type t =
    { timestamp : Time.t
    ; count     : int
    ; err_code  : int
    ; err_ext   : int
    ; priority  : int
    ; multi_pid : bool
    ; pid       : int
    ; packet    : int32
    ; param_1   : int32
    ; param_2   : int32 (* t2mi stream id for t2mi error *)
    } [@@deriving yojson,eq]

  type raw =
    (Stream.id * t) list [@@deriving yojson]

  type compressed = percent list
  and percent =
    { errors    : float
    ; no_stream : float
    ; period    : Time.t * Time.t
    } [@@deriving yojson]

end
