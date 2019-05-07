open Board_niitv_dvb_types.Device
open Application_types

let ( >>= ) = Lwt.bind

let ( % ) f g x = f (g x)

module Event = struct
  open Util_react

  let get_state (api : Protocol.api) _user _body _env state =
    let event =
      S.changes api.notifs.state
      |> E.map Topology.state_to_yojson in
    Lwt.return (`Ev (state, event))

  let get_receivers (api : Protocol.api) _user _body _env state =
    let event =
      S.map ~eq:(Boards.Util.(Option.equal @@ List.equal (=))) (function
          | None -> None
          | Some x -> Some x.receivers) api.notifs.devinfo
      |> S.changes
      |> E.map Util_json.(Option.to_yojson @@ List.to_yojson Int.to_yojson) in
    Lwt.return (`Ev (state, event))

  let get_mode (api : Protocol.api) (ids : int list) _user _body _env state =
    let to_yojson = Util_json.(
        List.to_yojson (Pair.to_yojson Int.to_yojson mode_to_yojson)) in
    let event = match ids with
      | [] ->
        S.changes api.notifs.config
        |> E.map (fun x -> to_yojson x.mode)
      | ids ->
        S.changes api.notifs.config
        |> E.fmap (fun x ->
            match List.filter (fun (id, _) -> List.mem id ids) x.mode with
            | [] -> None
            | l -> Some (to_yojson l)) in
    Lwt.return (`Ev (state, event))
end

let reset (api : Protocol.api) _user _body _env _state =
  api.channel Reset
  >>= function
  | Ok x -> Lwt.return (`Value Util_json.(info_to_yojson x))
  | Error e -> Lwt.return @@ `Error (Request.error_to_string e)

let get_state (api : Protocol.api) _user _body _env _state =
  let value = Topology.state_to_yojson @@ React.S.value api.notifs.state in
  Lwt.return (`Value value)

let get_info (api : Protocol.api) _user _body _env _state =
  let value = React.S.value api.notifs.devinfo in
  Lwt.return (`Value Util_json.(Option.to_yojson info_to_yojson value))

let get_receivers (api : Protocol.api) _user _body _env _state =
  let value = match React.S.value api.notifs.devinfo with
    | None -> None
    | Some x -> Some x.receivers in
  let to_yojson = Util_json.(Option.to_yojson @@ List.to_yojson Int.to_yojson) in
  Lwt.return (`Value (to_yojson value))

let get_mode (api : Protocol.api) (ids : int list) _user _body _env _state =
  api.kv#get
  >>= fun (config : config) ->
  let value = match ids with
    | [] -> config.mode
    | ids -> List.filter (fun (id, _) -> List.mem id ids) @@ config.mode in
  let to_yojson = Util_json.(
      List.to_yojson (Pair.to_yojson Int.to_yojson mode_to_yojson)) in
  Lwt.return (`Value (to_yojson value))
