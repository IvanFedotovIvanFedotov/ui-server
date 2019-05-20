open Board_niitv_ts2ip_types
open Application_types

(* TODO
   1. If stream_id is set in udp_mode, disable this stream
      when stream_id is not found at device's input *)

(* TODO remove after 4.08 *)
module List = Boards.Util.List

type notifs =
  { state : Topology.state React.signal
  ; device_status : device_status React.event
  ; transmitter_status : transmitter_status React.event
  ; devinfo : devinfo option React.signal
  ; config : config React.signal
  ; incoming_streams : Stream.t list React.signal
  ; outgoing_streams : Stream.t list React.signal
  }

type api =
  { notifs : notifs
  ; kv : config Kv_v.rw
  ; ports : Topology.topo_port list
  ; channel : 'a. 'a Request.t -> ('a, Request.error) Lwt_result.t
  ; loop : unit -> unit Lwt.t
  ; push_data : Cstruct.t -> unit
  }

let msg_queue_size = 20

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.Infix.( >>= )

let send (type a)
    (src : Logs.src)
    (state : Topology.state React.signal)
    (push : _ Lwt_stream.bounded_push)
    (sender : Cstruct.t -> unit Lwt.t)
    (req : a Request.t) =
  match React.S.value state with
  | `Init | `No_response | `Detect -> Lwt.return_error Request.Not_responding
  | `Fine ->
    Lwt.catch (fun () ->
        let t, w = Lwt.task () in
        let send = fun stream ->
          Fsm.request src stream sender req
          >>= fun x -> Lwt.wakeup_later w x; Lwt.return_unit in
        Lwt.pick
          [ (Boards.Board.await_no_response state >>= Api_util.not_responding)
          ; (push#push send >>= fun () -> t)
          ])
      (function
        | Lwt.Canceled -> Lwt.return_error Request.Not_responding
        | Lwt_stream.Full -> Lwt.return_error Request.Queue_overflow
        | exn -> Lwt.fail exn)

let find_stream (ports : Topology.topo_port list)
    (stream : Stream.t)
    (socket : socket)
    (streams : Stream.t list) =
  List.find_opt (fun (t : Stream.t) ->
      let p = Stream.to_topo_port ports t in
      let port = socket_to_enum socket in
      match p with
      | Some p when Stream.equal t stream && p.port = port -> true
      | _ -> false) streams

let to_out_streams_s (ports : Topology.topo_port list)
    (status : transmitter_status React.event)
    (streams : Stream.t list React.signal) =
  React.S.sample (fun (status : transmitter_status) streams ->
      List.filter_map (fun { enabled; sync; stream; _ } ->
          if enabled && sync
          then None
          else None) status.udp)
    status streams

let port_to_stream (port : Topology.topo_port) =
  match port.child with
  | Board _ -> None (* This board should return list of streams *)
  | Input _ ->
    match socket_of_enum port.port with
    | None -> assert false
    | Some socket ->
      let info = match socket with
        | ASI_1 | ASI_2 -> Stream.Source.ASI
        | SPI_1 | SPI_2 | SPI_3 -> SPI in
      Some (socket, { Stream.Raw.
                      source = { info; node = Port port.port }
                    ; id = TS_raw
                    ; typ = TS
                    })

let update_incoming_streams
    (streams : Stream.t list React.signal)
    (conv : Stream.Raw.t list React.signal -> Stream.t list React.signal)
    (status : device_status React.event)
    (ports : Topology.topo_port list) =
  let inputs = List.filter_map port_to_stream ports in
  let input_streams =
    React.S.hold []
    @@ React.E.fmap (fun (sync : socket list) ->
        match List.filter_map (fun id -> List.assoc_opt id inputs) sync with
        | [] -> None
        | l -> Some l)
    @@ React.E.changes ~eq:(Util_equal.List.equal equal_socket)
    @@ React.E.map (fun (status : device_status) -> status.sync) status in
  React.S.merge ~eq:(Util_equal.List.equal Stream.equal) (@)
    [] [conv input_streams; streams]

let create (src : Logs.src)
    (sender : Cstruct.t -> unit Lwt.t)
    (incoming_streams : Stream.t list React.signal)
    (streams_conv : Stream.Raw.t list React.signal -> Stream.t list React.signal)
    (kv : config Kv_v.rw)
    (ports : Topology.topo_port list)
    (control : int) =
  let state, set_state =
    React.S.create ~eq:Topology.equal_state `No_response in
  let devinfo, set_devinfo =
    React.S.create ~eq:(Util_equal.Option.equal equal_devinfo) None in
  let device_status, set_device_status = React.E.create () in
  let transmitter_status, set_transmitter_status = React.E.create () in
  let incoming_streams =
    update_incoming_streams
      incoming_streams
      streams_conv
      device_status
      ports in
  let outgoing_streams =
    React.S.hold ~eq:(Util_equal.List.equal Stream.equal) []
    @@ to_out_streams_s ports transmitter_status incoming_streams in
  let (notifs : notifs) =
    { state
    ; devinfo
    ; incoming_streams
    ; outgoing_streams
    ; device_status
    ; transmitter_status
    ; config = kv#s
    } in
  let req_queue, push_req_queue = Lwt_stream.create_bounded msg_queue_size in
  let rsp_queue, push_rsp_queue = Lwt_stream.create () in
  let evt_queue, push_evt_queue = Lwt_stream.create () in
  let acc = ref None in
  let push_data (buf : Cstruct.t) =
    let buf = match !acc with
      | None -> buf
      | Some acc -> Cstruct.append acc buf in
    let parsed, new_acc = Parser.deserialize src buf in
    acc := new_acc;
    match React.S.value state with
    | `No_response -> ()
    | _ -> List.iter (function
        | { Request. tag = `Status; data } -> push_evt_queue @@ Some data
        | x -> push_rsp_queue @@ Some x) parsed in
  let channel = fun req -> send src state push_req_queue sender req in
  let loop = Fsm.start src sender req_queue rsp_queue evt_queue kv
      set_state
      (fun ?step x -> set_devinfo ?step @@ Some x)
      set_device_status
      set_transmitter_status in
  let api =
    { notifs
    ; loop
    ; ports
    ; push_data
    ; channel
    ; kv
    } in
  Lwt.return_ok api
