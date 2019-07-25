open Boards
open Application_types
open Board_dektec_dtm3200_types
open Board_dektec_dtm3200_protocol

module Config = Kv_v.RW(Board_settings)

let ( >>= ) = Lwt_result.bind

let get_address_from_env src (b : Topology.topo_board) =
  match Topology.Env.find_opt "address" b.env with
  | None -> None
  | Some s ->
    match int_of_string_opt s with
    | Some x -> Some x
    | None ->
      Logs.warn ~src (fun m ->
          m "Failed to parse address value from environment: %s" s);
      None

let create (b : Topology.topo_board)
    (_ : Stream.t list React.signal)
    (convert_streams : Topology.topo_board ->
     Stream.Raw.t list React.signal ->
     Stream.t list React.signal)
    (send : Cstruct.t -> unit Lwt.t)
    (db : Db.t)
    (kv : Kv.RW.t) : (Board.t, [> Board.error]) Lwt_result.t =
  Lwt.return @@ Boards.Board.create_log_src b
  >>= fun (src : Logs.src) ->
  let default = match get_address_from_env src b with
    | None -> Board_settings.default
    | Some address -> { Board_settings.default with address } in
  Config.create ~default kv ["board"; (string_of_int b.control)]
  >>= fun (cfg : config Kv_v.rw) ->
  Protocol.create src send (convert_streams b) cfg db
  >>= fun (api : Protocol.api) ->
  let state = object
    method finalize () = Lwt.return ()
  end in
  let (board : Board.t) =
    { http = Board_dektec_dtm3200_http.handlers b.control api
    ; ws = Board_dektec_dtm3200_http.ws b.control api
    ; templates = []
    ; control = b.control
    ; streams_signal = api.notifs.streams
    ; log_source = (fun _ -> React.E.never) (* TODO implement source *)
    ; loop = api.loop
    ; push_data = api.push_data
    ; connection = api.notifs.state
    ; ports_sync =
        List.fold_left (fun acc (p : Topology.topo_port) ->
            (match p.port with
             | 0 -> React.S.map ~eq:(=) (function [] -> false | _ -> true)
                 api.notifs.streams
             | x -> Boards.Board.invalid_port src x)
            |> fun x -> Board.Ports.add p.port x acc)
          Board.Ports.empty b.ports
    ; ports_active =
        List.fold_left (fun acc (p : Topology.topo_port) ->
            (match p.port with
             | 0 -> React.S.const true
             | x -> Boards.Board.invalid_port src x)
            |> fun x -> Board.Ports.add p.port x acc)
          Board.Ports.empty b.ports
    ; stream_handler = None
    ; state = (state :> < finalize : unit -> unit Lwt.t >)
    } in
  Lwt.return_ok board
