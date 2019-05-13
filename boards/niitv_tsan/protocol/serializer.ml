open Board_niitv_tsan_types
open Application_types

let request_id = ref 0

let get_request_id () =
  let id = !request_id in
  incr request_id;
  id

let to_common_header (code : int) =
  let hdr = Cstruct.create Message.sizeof_common_header in
  Message.set_common_header_prefix hdr Message.prefix;
  Message.set_common_header_msg_code hdr code;
  hdr

let to_simple_req (msg : Request.req_tag Request.msg) =
  let common = to_common_header @@ Request.req_tag_to_enum msg.tag in
  Cstruct.append common msg.data

let to_complex_req (msg : Request.complex_msg) =
  let common = to_common_header @@ Request.complex_tag_to_enum msg.tag in
  let header = Cstruct.create Message.sizeof_complex_req_header in
  let length = (Cstruct.len msg.data / 2) + 1 in
  Message.set_complex_req_header_client_id header msg.client_id;
  Message.set_complex_req_header_length header length;
  Message.set_complex_req_header_request_id header msg.request_id;
  Cstruct.concat [common; header; msg.data]

let to_msg (type a) : a Request.t -> Request.req_msg = function
  | Get_devinfo -> `Simple { tag = `Get_devinfo; data = Cstruct.empty }
  | Get_deverr request_id -> `Complex (Request.make_complex_msg ~request_id `Deverr)
  | Get_mode -> `Simple { tag = `Get_mode; data = Cstruct.empty }
  | Set_mode (input, { pid; enabled; stream; t2mi_stream_id }) ->
    let data = Cstruct.create Message.sizeof_board_mode in
    let pid = (t2mi_stream_id lsl 13) lor (pid land 0x1FFF) in
    input_to_int input
    |> (lor) (if enabled then 4 else 0)
    |> (lor) 8 (* disable board storage by default *)
    |> Message.set_board_mode_mode data;
    Message.set_board_mode_t2mi_pid data pid;
    Message.set_board_mode_stream_id data @@ Stream.Multi_TS_ID.to_int32_pure stream;
    `Simple { tag = `Set_mode; data }
  | Set_jitter_mode mode ->
    let pid, stream = match mode with
      | None -> 0, 0l
      | Some m -> m.pid, Stream.Multi_TS_ID.to_int32_pure m.stream in
    let data = Cstruct.create Message.sizeof_req_set_jitter_mode in
    Message.set_req_set_jitter_mode_stream_id data stream;
    Message.set_req_set_jitter_mode_pid data pid;
    `Simple { tag = `Set_jitter_mode; data }
  | Reset -> `Complex (Request.make_complex_msg `Reset)
  | Set_src_id { input; t2mi } ->
    let data = Cstruct.create Message.sizeof_req_source_id in
    Message.set_req_source_id_input_src_id data input;
    Message.set_req_source_id_t2mi_src_id data t2mi;
    `Simple { tag = `Set_source_id; data }
  | Get_t2mi_seq { request_id; stream; seconds } ->
    let data = Cstruct.create Message.sizeof_req_t2mi_seq in
    Message.set_req_t2mi_seq_time data seconds;
    `Complex (Request.make_complex_msg ~request_id ~data `T2mi_seq)
  | Get_section { request_id
                ; stream_id
                ; table_id
                ; table_id_ext
                ; id_ext_1
                ; id_ext_2
                ; section
                } ->
    let iter f x = match x with None -> () | Some x -> f x in
    let data = Cstruct.create Message.sizeof_req_section in
    let stream = Stream.Multi_TS_ID.to_int32_pure stream_id in
    Message.set_req_section_stream_id data stream;
    Message.set_req_section_table_id data table_id;
    iter (Message.set_req_section_section data) section;
    iter (Message.set_req_section_table_id_ext data) table_id_ext;
    iter (Message.set_req_section_id_ext_1 data) id_ext_1;
    iter (Message.set_req_section_id_ext_2 data) id_ext_2;
    `Complex (Request.make_complex_msg ~request_id ~data `Section)
  | Get_jitter { request_id; pointer } ->
    let data = Cstruct.create Message.sizeof_req_jitter in
    Message.set_req_jitter_ptr data pointer;
    `Complex (Request.make_complex_msg ~request_id ~data `Jitter)
  | Get_bitrate request_id ->
    `Complex (Request.make_complex_msg ~request_id `Bitrate)
  | Get_structure { request_id; stream } ->
    let stream = match stream with
      | `All -> 0xFFFF_FFFFl
      | `Single x -> Stream.Multi_TS_ID.to_int32_pure x in
    let data = Cstruct.create Message.sizeof_req_structure in
    Message.set_req_structure_stream_id data stream;
    `Complex (Request.make_complex_msg ~request_id ~data `Structure)
  | Get_t2mi_info { request_id; t2mi_stream_id; stream } ->
    let data = Cstruct.create Message.sizeof_req_t2mi_info in
    Message.set_req_t2mi_info_t2mi_stream_id data t2mi_stream_id;
    `Complex (Request.make_complex_msg ~request_id ~data `T2mi_info)

let serialize (type a) (req : a Request.t) : Cstruct.t =
  match to_msg req with
  | `Simple msg -> to_simple_req msg
  | `Complex msg -> to_complex_req msg