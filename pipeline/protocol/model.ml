open Lwt.Infix

type t =
  { db : Database.Conn.t
  ; tick  : unit React.event
  ; _loop : unit Lwt.t
  }

let tick () =
  let e,push = React.E.create () in
  let rec loop () =
    Lwt_unix.sleep 5. >>= fun () -> push (); loop ()
  in
  e, loop
       
let create db _s_struct _s_status _e_video _e_audio =
  let (>>=) = Lwt_result.bind in
  
  Database.Conn.create db ()
  >>= fun db ->
  
  let tick, loop = tick () in
  (* Pids *)
  (*
  let strip = let open Structure in List.map (fun s -> s.structure) in
  let pids =
    let open Structure in
    React.S.sample (fun () sl -> `Active (active_pids @@ strip sl)) tick s_struct
  in
  let pids_diff =
    let open Structure in
    React.S.diff (fun pres past ->
        let pads = appeared_pids ~past:(strip past) ~pres:(strip pres) in
        if List.is_empty pads then `None else `New pads) s_struct
  in
  React.E.keep
  @@ React.E.map_s (function
         | `Active pids -> Db.Pid_state.bump db pids
         | `New pids -> Db.Pid_state.init db pids
         | `None -> Lwt.return_unit)
  @@ React.E.select [pids; pids_diff];
  (* Structures *)
  React.S.keep
  @@ React.S.map ~eq:(fun _ _ -> false) (fun x ->
         Lwt.catch
           (fun () -> Db.Structure.insert_structures db x)
           (function Failure e -> Lwt_io.printf "str error: %s\n" e)
         |> Lwt.ignore_result) s_struct;
  (* Stream status *)
  let stream_status =
    let open Qoe_status in
    React.S.sample (fun () sl -> `Bump (List.filter (fun s -> not s.playing) sl))
      tick s_status
  in
  let stream_status_diff =
    let open Qoe_status in
    let merge pres past =
      let find s =
        List.find (fun sold ->
            Common.Stream.ID.equal s.stream sold.stream
            && s.channel = sold.channel
            && s.pid = sold.pid)
          past in
      List.fold_left (fun acc s ->
          if s.playing then acc
          else try if (find s).playing
                   then s::acc
                   else acc
               with _ -> s::acc)
        [] pres
    in
    React.S.diff (fun pres past -> `Lost (merge pres past)) s_status
  in
  React.E.keep
  @@ React.E.map (function
         | `Bump pids -> Db.Stream_status.bump db pids
         | `Lost pids -> Db.Stream_status.init db pids)
  @@ React.E.select [stream_status; stream_status_diff];
   *)
  (* Errors (TODO) *)
  (*e_video
  |> Util_react.E.map_p (fun x -> Lwt.catch
                               (fun () -> Database.Errors.insert_video db x)
                               (function Failure e -> Lwt_io.printf "vdata error: %s\n" e
                                       | _ -> Lwt_io.printf "vdata error: UNKNOWN\n"))
  |> Util_react.E.keep;

  e_audio
  |> Util_react.E.map_p (fun x -> Lwt.catch (fun () -> Database.Errors.insert_audio db x)
                               (function Failure e -> Lwt_io.printf "adata error: %s\n" e
                                       | _ -> Lwt_io.printf "adata error: UNKNOWN\n"))
  |> Util_react.E.keep;
   *)
       
  Lwt.return_ok { db; tick; _loop = loop () }
  
let set_streams model streams =
  (* Streams *)
  Lwt.ignore_result 
    (Database.Streams.init model.db streams >|= fun () ->
     Util_react.E.keep
     @@ Util_react.E.map_s (fun () -> Database.Streams.bump model.db) model.tick);
