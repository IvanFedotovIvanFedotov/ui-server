open Application_types
open Board_niitv_tsan_types
open Api_util

let invalid_stream = Request.Custom "Unsupported stream format"

let stream_not_found = Request.Custom "Stream not found"

let is_incoming_stream (api : Protocol.api) (s : Stream.t) =
  let { t2mi_source; _ } = React.S.value api.kv#s in
  match s.orig_id with
  | TS_multi x -> Stream.Multi_TS_ID.source_id x <> t2mi_source
  | _ -> true

module Event = struct
  open Util_react

  let get_streams (api : Protocol.api) incoming ids _user =
    let to_yojson = Util_json.List.to_yojson Stream.to_yojson in
    let filter = match incoming, ids with
      | (None | Some false), [] -> None
      | Some true, [] -> Some (List.filter (is_incoming_stream api))
      | Some true, ids ->
        Some (List.filter (is_incoming_stream api) % filter_streams ids)
      | _, ids -> Some (filter_streams ids) in
    let signal = match filter with
      | None -> api.notifs.streams
      | Some f ->
        let eq = Util_equal.List.equal Stream.equal in
        S.map ~eq f api.notifs.streams in
    Lwt.return (E.map to_yojson @@ S.changes signal)

end

let find_multi_id id streams =
  match Stream.find_by_id id @@ React.S.value streams with
  | None -> Error stream_not_found
  | Some s -> match s.orig_id with
    | TS_multi id -> Ok id
    | _ -> Error invalid_stream

let return_value_or_not_found = function
  | None -> return_error stream_not_found
  | Some x -> return_value x

let get_streams (api : Protocol.api) incoming ids _user _body _env _state =
  let filter = match incoming, ids with
    | (None | Some false), [] -> None
    | Some true, [] -> Some (List.filter (is_incoming_stream api))
    | Some true, ids ->
      Some (List.filter (is_incoming_stream api) % filter_streams ids)
    | _, ids -> Some (filter_streams ids) in
  let streams = match filter with
    | None -> React.S.value api.notifs.streams
    | Some f -> f @@ React.S.value api.notifs.streams in
  let to_json = Util_json.List.to_yojson Stream.to_yojson in
  Lwt.return (`Value (to_json streams))

let get_stream (api : Protocol.api) id _user _body _env _state =
  let streams = React.S.value api.notifs.streams in
  let to_json = Util_json.Option.to_yojson Stream.to_yojson in
  let stream = to_json @@ Stream.find_by_id id streams in
  Lwt.return (`Value stream)

let get_bitrate (api : Protocol.api) id _user _body _env _state =
  Lwt.pick
    [ (Boards.Board.await_no_response api.notifs.state >>= not_responding)
    ; (Util_react.E.next api.notifs.bitrate >>= Lwt.return_ok)
    ; Fsm.sleep Fsm.status_timeout ]
  >>=? fun bitrate ->
  return_value
  @@ Util_json.Option.to_yojson Bitrate.to_yojson
  @@ find_by_id id bitrate

let get_ts_info (api : Protocol.api) id force _user _body _env _state =
  match force with
  | None | Some false ->
    check_state api.notifs.state
    >>=? fun () ->
    return_value_or_not_found
    @@ find_map_by_id id (TS_info.to_yojson % Structure.info)
    @@ React.S.value api.notifs.structure
  | Some true ->
    match find_multi_id id api.notifs.streams with
    | Error e -> return_error e
    | Ok id ->
      let request_id = Request_id.next () in
      let req = Request.Get_structure { request_id; stream = `Single id } in
      api.channel req
      >>=? fun structures ->
      match List.assoc_opt id structures with
      | None -> return_error stream_not_found
      | Some x -> return_value (TS_info.to_yojson x.info)

let get_pids (api : Protocol.api) id force _user _body _env _state =
  match force with
  | None | Some false ->
    check_state api.notifs.state
    >>=? fun () ->
    return_value_or_not_found
    @@ find_map_by_id id (pids_to_yojson % Structure.pids)
    @@ React.S.value api.notifs.structure
  | Some true ->
    match find_multi_id id api.notifs.streams with
    | Error e -> return_error e
    | Ok id ->
      let request_id = Request_id.next () in
      let req = Request.Get_structure { request_id; stream = `Single id } in
      api.channel req
      >>=? fun structures ->
      match List.assoc_opt id structures with
      | None -> return_error stream_not_found
      | Some x -> return_value (pids_to_yojson x.pids)

let get_si_psi_tables (api : Protocol.api) id _user _body _env _state =
  check_state api.notifs.state
  >>=? fun () ->
  return_value_or_not_found
  @@ find_map_by_id id (si_psi_tables_to_yojson % Structure.tables)
  @@ React.S.value api.notifs.structure

let get_services (api : Protocol.api) id _user _body _env _state =
  check_state api.notifs.state
  >>=? fun () ->
  return_value_or_not_found
  @@ find_map_by_id id (services_to_yojson % Structure.services)
  @@ React.S.value api.notifs.structure

let get_t2mi_info (api : Protocol.api) id _user _body _env _state =
  check_state api.notifs.state
  >>=? fun () ->
  return_value_or_not_found
  @@ find_map_by_id id t2mi_info_to_yojson
  @@ React.S.value api.notifs.t2mi_info

let get_t2mi_sequence (api : Protocol.api) id duration t2mi_stream_ids
    _user _body _env _state =
  let duration = match duration with
    | None -> 5
    | Some x -> min 120 x in
  let streams = React.S.value api.notifs.streams in
  (match Stream.find_by_id id streams with
   | Some { orig_id = TS_multi _stream_id; _ } ->
     let request_id = Request_id.next () in
     let req = Request.Get_t2mi_seq { request_id; duration } in
     api.channel req
   | Some _ -> Lwt.return_error invalid_stream
   | None -> Lwt.return_error stream_not_found)
  >>=? fun x ->
  let value = match t2mi_stream_ids with
    | [] -> x
    | ids ->
      let data = List.filter (fun (x : T2mi_sequence.item) ->
          List.mem x.stream_id ids) x.data in
      { x with data } in
  let to_yojson = ts_to_yojson T2mi_sequence.to_yojson in
  return_value @@ to_yojson value

let get_section (api : Protocol.api) stream_id table_id
    section table_id_ext id_ext_1 id_ext_2
    _user _body _env _state =
  let streams = React.S.value api.notifs.streams in
  (match Stream.find_by_id stream_id streams with
   | Some { orig_id = TS_multi stream_id; _ } ->
     let request_id = Request_id.next () in
     let req = Request.Get_section { request_id
                                   ; stream_id
                                   ; table_id
                                   ; table_id_ext
                                   ; id_ext_1
                                   ; id_ext_2
                                   ; section
                                   } in
     api.channel req
   | Some _ -> Lwt.return_error invalid_stream
   | None -> Lwt.return_error stream_not_found)
  >>=? fun x -> return_value @@ ts_to_yojson SI_PSI_section.Dump.to_yojson x
