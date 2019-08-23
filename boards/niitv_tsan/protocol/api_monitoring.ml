open Application_types
open Board_niitv_tsan_types
open Api_util

let filter_errors f x =
  match List.filter_map (fun (id, errors) ->
      match List.filter f errors with
      | [] -> None
      | e -> Some (id, e)) x with
  | [] -> None
  | x -> Some x

let get_errors (api : Protocol.api) ids timeout _user _body _env _state =
  let timeout = match timeout with
    | None -> Fsm.status_timeout
    | Some x -> int_ms_to_float_s x in
  Lwt.pick
    [ (Boards.Board.await_no_response api.notifs.state >>= not_responding)
    ; (Util_react.E.next api.notifs.errors >>= Lwt.return_ok)
    ; (Lwt_unix.sleep timeout >>= fun () -> Lwt.return_ok []) ]
  >>=? fun errors ->
  return_value
  @@ stream_assoc_list_to_yojson (Util_json.List.to_yojson Error.to_yojson)
  @@ filter_ids ids errors

let get_bitrate (api : Protocol.api) ids timeout _user _body _env _state =
  let timeout = match timeout with
    | None -> Fsm.status_timeout
    | Some x -> int_ms_to_float_s x in
  Lwt.pick
    [ (Boards.Board.await_no_response api.notifs.state >>= not_responding)
    ; (Util_react.E.next api.notifs.bitrate >>= Lwt.return_ok)
    ; (Lwt_unix.sleep timeout >>= fun () -> Lwt.return_ok []) ]
  >>=? fun bitrate ->
  return_value
  @@ stream_assoc_list_to_yojson Bitrate.to_yojson
  @@ filter_ids ids bitrate

let ( >>= ) = Lwt_result.( >>= )

let get_ts_info (api : Protocol.api) force ids _user _body _env _state =
  (match force with
   | Some true ->
     let request_id = Request_id.next () in
     api.channel (Get_structure { request_id; stream = `All })
     >>= Lwt.return_ok % map_stream_id (React.S.value api.notifs.streams)
   | None | Some false ->
     check_state api.notifs.state
     >>= fun () -> Lwt.return_ok @@ React.S.value api.notifs.structure)
  >>=? return_value
       % stream_assoc_list_to_yojson TS_info.to_yojson
       % filter_ids ids
       % List.map (fun (id, (x : Structure.t)) -> id, x.info)

let get_pids (api : Protocol.api) force ids _user _body _env _state =
  (match force with
   | Some true ->
     let request_id = Request_id.next () in
     api.channel (Get_structure { request_id; stream = `All })
     >>= Lwt.return_ok % map_stream_id (React.S.value api.notifs.streams)
   | None | Some false ->
     check_state api.notifs.state
     >>= fun () -> Lwt.return_ok @@ React.S.value api.notifs.structure)
  >>=? return_value
       % stream_assoc_list_to_yojson pids_to_yojson
       % filter_ids ids
       % List.map (fun (id, (x : Structure.t)) -> id, x.pids)

let get_si_psi_tables (api : Protocol.api) force ids _user _body _env _state =
  (match force with
   | Some true ->
     let request_id = Request_id.next () in
     api.channel (Get_structure { request_id; stream = `All })
     >>= Lwt.return_ok % map_stream_id (React.S.value api.notifs.streams)
   | None | Some false ->
     check_state api.notifs.state
     >>= fun () -> Lwt.return_ok @@ React.S.value api.notifs.structure)
  >>=? return_value
       % stream_assoc_list_to_yojson si_psi_tables_to_yojson
       % filter_ids ids
       % List.map (fun (id, (s : Structure.t)) -> id, s.tables)

let get_services (api : Protocol.api) force ids _user _body _env _state =
  (match force with
   | Some true ->
     let request_id = Request_id.next () in
     api.channel (Get_structure { request_id; stream = `All })
     >>= Lwt.return_ok % map_stream_id (React.S.value api.notifs.streams)
   | None | Some false ->
     check_state api.notifs.state
     >>= fun () -> Lwt.return_ok @@ React.S.value api.notifs.structure)
  >>=? return_value
       % stream_assoc_list_to_yojson services_to_yojson
       % filter_ids ids
       % List.map (fun (id, (s : Structure.t)) -> id, s.services)

let filter_t2mi_stream_id ids l =
  match ids with
  | [] -> l
  | ids ->
    List.filter_map (fun (id, l) ->
        match List.filter (fun (id, _) -> List.mem id ids) l with
        | [] -> None
        | l -> Some (id, l)) l

let range i j =
  let rec up i j acc =
    if i = j then i :: acc else up i (pred j) (j :: acc)
  and down i j acc =
    if i = j then i :: acc else down i (succ j) (j :: acc)
  in
  if i <= j then up i j [] else down i j []

let get_t2mi_info (api : Protocol.api) force ids t2mi_stream_ids
    _user _body _env _state =
  (match force with
   | Some true ->
     let rec loop acc = function
       | [] -> Lwt.return_ok @@ acc
       | t2mi_stream_id :: tl ->
         let request_id = Request_id.next () in
         api.channel (Get_t2mi_info { request_id; t2mi_stream_id })
         >>= fun x ->
         let id = Boards.Util.List.find_map (fun (s : Stream.t) ->
             match s.typ with
             | T2MI -> Some s.id
             | _ -> None)
           @@ React.S.value api.notifs.streams in
         match id with
         | None -> loop acc tl
         | Some id ->
           let acc = Boards.Util.List.Assoc.update ~eq:Stream.ID.equal (function
               | None -> Some [x]
               | Some l -> Some (x :: l)) id acc in
           loop acc tl in
     (match t2mi_stream_ids with
      | [] -> loop [] (range 0 7)
      | l -> loop [] l)
   | None | Some false ->
     check_state api.notifs.state
     >>= fun () -> Lwt.return_ok @@ React.S.value api.notifs.t2mi_info)
  >>=? return_value
       % stream_assoc_list_to_yojson t2mi_info_to_yojson
       % filter_t2mi_stream_id t2mi_stream_ids
       % filter_ids ids
