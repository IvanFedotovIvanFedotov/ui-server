open Board_types
open Lwt.Infix
open Storage.Options
open Api.Handler
open Meta_board
open Meta_board.Msg
   
include Board_parser
   
let ( % ) = CCFun.(%)
      
(* Board protocol implementation *)

let timeout_period step_duration = 2 * int_of_float (1. /. step_duration) (* 2 secs *)
          
let request_period step_duration = 5 * int_of_float (1. /. step_duration) (* 5 secs *)

let detect = Get_devinfo

let init =
  let get_s x = match x.mode with
    | T2 -> { mode = T2; channel = x.t2 }
    | T  -> { mode = T ; channel = x.t  }
    | C  -> { mode = C ; channel = x.c  }
  in
  CCList.map (fun (i,x) -> Set_settings (i,get_s x))

let detect_msgs (send_req : 'a request -> unit Lwt.t) timeout =
  [ { send = (fun () -> send_req detect)
    ; pred = (is_response detect)
    ; timeout
    ; exn = None
  } ]

let init_msgs (send_req : 'a request -> unit Lwt.t) timeout d =
  CCList.map (fun x -> { send = (fun () -> send_req x)
                       ; pred = (is_response x)
                       ; timeout
                       ; exn = None })
             (init d)

let measure_probes (send_ev : 'a event_request -> unit Lwt.t) timeout config =
  List.map (fun x ->
      { send = (fun () -> send_ev @@ Get_measure x)
      ; pred = (is_event (Get_measure x))
      ; timeout
      ; exn = None
    })
    config.modules

module SM = struct

  type event = [ `Measure of (int * Cbuffer.t) ]

  let wakeup_timeout t = t.pred `Timeout |> ignore

  type push_events = { measure : measure_response -> unit
                     ; devinfo : devinfo_response -> unit
                     }

  let event_push pe = function
    | Measure x -> pe.measure x

  let send_msg (type a) sender (msg : a request) : unit Lwt.t =
    (match msg with
     | Get_devinfo            -> to_req_devinfo false
     | Reset                  -> to_req_devinfo true
     | Set_settings (id, buf) -> to_req_settings id buf
     | Set_plp (id, buf)      -> to_req_plp_set id buf
     | Get_plps id            -> to_req_plp_list id)
    |> sender

  let send_event (type a) sender (msg : a event_request) : unit Lwt.t =
    (* no instant msgs *)
    match msg with
    | Get_measure id -> sender @@ to_req_measure id

  let send (type a) msgs sender (storage : config storage) timeout (msg : a request) : a Lwt.t =
    (* no instant msgs *)
    let t, w = Lwt.wait () in
    let pred = function
      | `Timeout -> Lwt.wakeup_exn w (Failure "msg timeout"); None
      | l -> let open CCOpt in
             is_response msg l >|= fun r ->
             (match msg with
              | Set_settings (i,ns) ->
                 let upd =
                   CCList.Assoc.update ~f:(function
                                           | Some os -> let os = { os with mode = ns.mode } in
                                                        Some (match ns.mode with
                                                              | T2 -> { os with t2 = ns.channel }
                                                              | T  -> { os with t  = ns.channel }
                                                              | C  -> { os with c  = ns.channel })
                                           | None   -> None)
                                       i storage#get
                 in storage#store upd;
              | _ -> ());
             Lwt.wakeup w r
    in
    let send = fun () -> send_msg sender msg in
    msgs := Queue.append !msgs { send; pred; timeout; exn = None };
    t

  let initial_timeout = -1

  let step msgs sender (storage : config storage) step_duration push_state push_events =
    let period         = timeout_period step_duration in
    let request_period = request_period step_duration in
    let detect_pool  = Pool.create (detect_msgs (send_msg sender) period) in
    let time         = ref 0.0 in

    let rec first_step () =
      Queue.iter !msgs wakeup_timeout;
      msgs := Queue.create [];
      push_state `No_response;
      Pool.send detect_pool () |> ignore;
      `Continue (step_detect detect_pool None)

    and step_detect detect_pool acc recvd =
      try
        (*Lwt_io.printf "Detect step\n" |> ignore;*)
        let recvd = Meta_board.concat_acc acc recvd in
        let _, responses, acc = deserialize recvd in
        match Pool.responsed detect_pool responses with
        | Some detect -> push_state `Init;
                         push_events.devinfo (Some detect);
                         step_start_init (measure_probes (send_event sender) period detect)
        | _           -> `Continue (step_detect (Pool.step detect_pool) acc)
      with Timeout -> first_step ()

    and step_start_init probes =
      try
        match init_msgs (send_msg sender) period storage#get with
        | [] -> let probes_pool = Pool.create probes in
                Pool.send probes_pool () |> ignore;
                `Continue (step_normal_probes_send probes_pool 0 None)
        | lst -> let init_pool = Pool.create lst in
                 Pool.send init_pool () |> ignore;
                 `Continue (step_init init_pool probes None)
      with Timeout -> first_step ()

    and step_init init_pool probes acc recvd =
      try
        (*Lwt_io.printf "Init step\n" |> ignore;*)
        let recvd = Meta_board.concat_acc acc recvd in
        let _, responses, acc = deserialize recvd in
        match Pool.responsed init_pool responses with
        | None    -> `Continue (step_init (Pool.step init_pool) probes acc)
        | Some _  ->
           (match Pool.last init_pool with
            | true   -> push_state `Fine;
                        let probes_pool = Pool.create probes in
                        `Continue (step_normal_probes_send probes_pool 0 acc)
            | false  -> let init_pool = Pool.next init_pool in
                        Pool.send init_pool () |> ignore;
                        `Continue (step_init init_pool probes acc))
      with Timeout -> first_step ()

    and step_normal_probes_send probes_pool period_timer acc _ =
      (*Lwt_io.printf "Normal step probes send \n" |> ignore;*)
      if (period_timer >= request_period) then raise (Failure "board_dvb: sm invariant is broken");

      if Pool.empty probes_pool
      then `Continue (step_normal_requests_send probes_pool (succ period_timer) acc)
      else (Pool.send probes_pool () |> ignore;
            (* Lwt_io.printlf "sending probe" |> ignore; *)
            time := Unix.gettimeofday ();
            `Continue (step_normal_probes_wait probes_pool (succ period_timer) acc))

    and step_normal_probes_wait probes_pool period_timer acc recvd =
      (*Lwt_io.printf "Normal step probes recv\n" |> ignore;*)
      let recvd_buf = Meta_board.concat_acc acc recvd in
      let events, _, acc = deserialize recvd_buf in

      try
        (match Pool.responsed probes_pool events with
         | None    -> let probes_pool = Pool.step probes_pool in
                      `Continue (step_normal_probes_wait probes_pool (succ period_timer) acc)
         | Some ev -> (* Lwt_io.printlf "received probe, %f" (Unix.gettimeofday () -. !time) |> ignore; *)
                      let new_probes_pool = Pool.next probes_pool in
                      event_push push_events ev;
                      if Pool.last probes_pool
                      then `Continue (step_normal_requests_send new_probes_pool period_timer acc)
                      else step_normal_probes_send new_probes_pool period_timer acc recvd)
      with Timeout -> (* Lwt_io.printlf "probe not received, %f" (Unix.gettimeofday () -. !time)|> ignore; *)
        first_step ()

    and step_normal_requests_send probes_pool period_timer acc _ =
      (*Lwt_io.printf "Normal step requests send\n" |> ignore;*)
      if (period_timer >= request_period)
      then `Continue (step_normal_probes_send probes_pool ((succ period_timer) mod request_period) acc)
      else 
        if Queue.empty !msgs
        then `Continue (step_normal_requests_send probes_pool (succ period_timer) acc)
        else (Queue.send !msgs () |> ignore;
              `Continue (step_normal_requests_wait probes_pool (succ period_timer) acc))

    and step_normal_requests_wait probes_pool period_timer acc recvd =
      (*Lwt_io.printf "Normal step requests recv\n" |> ignore;*)
      let recvd = Meta_board.concat_acc acc recvd in
      let _, responses, acc = deserialize recvd in
      try
        match Queue.responsed !msgs responses with
        | None    -> msgs := Queue.step !msgs;
                     `Continue (step_normal_requests_wait probes_pool (succ period_timer) acc)
        | Some () -> msgs := Queue.next !msgs;
                     `Continue (step_normal_requests_send probes_pool (succ period_timer) acc)
      with Timeout -> first_step ()
    in
    first_step ()

  let create sender (storage : config storage) push_state step_duration =
    let period = timeout_period step_duration in
    let e_config, e_config_push   = React.E.create () in
    let s_devinfo, s_devinfo_push = React.S.create None in
    let e_measure, e_measure_push = React.E.create () in
    let (events : events)   = { measure = e_measure
                              ; config  = React.E.changes ~eq:equal_config e_config
                              }
    in
    let (push_events : push_events) = { measure = e_measure_push
                                      ; devinfo = s_devinfo_push
                                      }
    in
    let msgs = ref (Queue.create []) in
    let send x = send msgs sender storage period x in
    let api = { devinfo     = (fun ()    -> Lwt.return @@ React.S.value s_devinfo)
              ; reset       = (fun ()    -> send Reset)
              ; settings    = (fun s     -> send (Set_settings s)
                                            >>= (fun x -> e_config_push storage#get; Lwt.return x))
              ; plp_setting = (fun (n,s) -> send (Set_plp (n,s)))
              ; plps        = (fun n     -> send (Get_plps n))
              ; config      = (fun ()    -> Lwt.return storage#get)
              }
    in
    events,
    api,
    (step msgs sender storage step_duration push_state push_events)

end