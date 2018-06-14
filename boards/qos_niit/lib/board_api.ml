open Containers
open Api.Interaction
open Board_protocol
open Lwt.Infix
open Board_types
open Containers
open Websocket_cohttp_lwt
open Frame

module Api_handler = Api.Handler.Make(Common.User)

let ( >|= ) = Lwt.Infix.(>|=)
let ( >>= ) = Json.( >>= )
let ( %> )  = Fun.( %> )
let ( % )   = Fun.( % )

(* TODO reason about random key *)
let () = Random.init (int_of_float @@ Unix.time ())
let rand_int = fun () -> Random.run (Random.int 10000000)

let socket_table = Hashtbl.create 1000

let reset (api : api) () =
  api.reset () >|= Result.return
  >>= Json.respond_result_unit

let set_mode (api : api) body () =
  Json.of_body body >>= fun mode ->
  (match mode_request_of_yojson mode with
   | Error e -> Lwt_result.fail @@ Json.of_error_string e
   | Ok mode -> api.set_mode mode >|= Result.return)
  >>= Json.respond_result_unit

let set_input (api : api) body () =
  Json.of_body body >>= fun inp ->
  (match input_of_yojson inp with
   | Error e -> Lwt_result.fail @@ Json.of_error_string e
   | Ok inp  -> api.set_input inp >|= Result.return)
  >>= Json.respond_result_unit

let set_t2mi_mode (api : api) body () =
  Json.of_body body >>= fun mode ->
  (match t2mi_mode_request_of_yojson mode with
   | Error e -> Lwt_result.fail @@ Json.of_error_string e
   | Ok mode -> api.set_t2mi_mode mode >|= Result.return)
  >>= Json.respond_result_unit

let set_jitter_mode api body () =
  Json.of_body body >>= fun mode ->
  (match jitter_mode_request_of_yojson mode with
   | Error e -> Lwt_result.fail @@ Json.of_error_string e
   | Ok mode -> api.set_jitter_mode mode >|= Result.return)
  >>= Json.respond_result_unit

let devinfo api () =
  api.get_devinfo () >|= (devinfo_response_to_yojson %> Result.return)
  >>= Json.respond_result

let config api () =
  api.config () >|= (config_to_yojson %> Result.return)
  >>= Json.respond_result

let get_t2mi_seq api seconds () =
  (match Int.of_string seconds with
   | None   -> Lwt_result.fail @@ Json.of_error_string @@ Printf.sprintf "bad argument: %s" seconds
   | Some x -> api.get_t2mi_seq x >|= (t2mi_seq_response_to_yojson %> Result.return))
  >>= Json.respond_result

let get_structs api () =
  api.get_structs () >|= (ts_structs_to_yojson %> Result.return)
  >>= Json.respond_result

let get_bitrates api () =
  api.get_bitrates () >|= (ts_structs_to_yojson %> Result.return)
  >>= Json.respond_result

let get_incoming_streams streams () =
  React.S.value streams
  |> Common.Stream.t_list_to_yojson
  |> Result.return
  |> Json.respond_result

let get_state s_state () =
  React.S.value s_state
  |> Common.Topology.state_to_yojson
  |> Result.return
  |> Json.respond_result

let sock_handler sock_data (event:'a React.event) (to_yojson:'a -> Yojson.Safe.json) body =
  let id = rand_int () in
  Cohttp_lwt.Body.drain_body body
  >>= fun () ->
  Websocket_cohttp_lwt.upgrade_connection
    (fst sock_data)
    (snd sock_data)
    (fun f -> match f.opcode with
              | Opcode.Close -> Hashtbl.remove socket_table id
              | _ -> ())
  >>= fun (resp, body, frames_out_fn) ->
  let send x =
    let msg = Yojson.Safe.to_string x in
    frames_out_fn @@ Some (Frame.create ~content:msg ())
  in
  let sock_events = Lwt_react.E.map (send % to_yojson) event in
  Hashtbl.add socket_table id sock_events;
  Lwt.return (resp, (body :> Cohttp_lwt.Body.t))

let state_ws sock_data s_state body =
  sock_handler sock_data (Lwt_react.S.changes s_state) Common.Topology.state_to_yojson body

let config_ws sock_data (events : events) body =
  sock_handler sock_data events.config config_to_yojson body

let status_ws sock_data (events : events) body =
  sock_handler sock_data events.status user_status_to_yojson body

let ts_errors_ws sock_data (events : events) body =
  sock_handler sock_data events.ts_errors ts_errors_to_yojson body

let t2mi_errors_ws sock_data (events : events) body =
  sock_handler sock_data events.t2mi_errors t2mi_errors_to_yojson body

let board_errors_ws sock_data (events : events) body =
  sock_handler sock_data events.board_errors board_errors_to_yojson body

let bitrate_ws sock_data (events : events) body =
  sock_handler sock_data events.bitrates ts_structs_to_yojson body

let structs_ws sock_data (events : events) body =
  sock_handler sock_data events.structs ts_structs_to_yojson body

let t2mi_info_ws sock_data (events : events) body =
  sock_handler sock_data events.t2mi_info t2mi_info_to_yojson body

let jitter_ws sock_data (events : events) body =
  sock_handler sock_data events.jitter jitter_to_yojson body

let incoming_streams_ws sock_data streams body =
  sock_handler sock_data (React.S.changes streams) Common.Stream.t_list_to_yojson body

let handle api events s_state s_input streams _ meth uri_sep sock_data _ body =
  let open Api.Redirect in
  (* TODO match string + query *)
  let path_list = Common.Uri.(split @@  Path.to_string uri_sep.path) in
  match meth, path_list with
  | `POST, ["reset"]              -> reset api ()
  | `POST, ["mode"]               -> set_mode api body ()
  | `POST, ["input"]              -> set_input api body ()
  | `POST, ["port";id;set]        ->
     (match (Option.flat_map Board_parser.input_of_int @@ Int.of_string id), set with
      | Some i, "set"     -> api.set_input i   >|= Result.return >>= Json.respond_result_unit
      | Some ASI, "unset" -> api.set_input SPI >|= Result.return >>= Json.respond_result_unit
      | Some SPI, "unset" -> api.set_input ASI >|= Result.return >>= Json.respond_result_unit
      | _                 -> not_found ())
  | `POST, ["t2mi_mode"]          -> set_t2mi_mode api body ()
  | `POST, ["jitter_mode"]        -> set_jitter_mode api body ()

  | `GET, ["config"]              -> config api ()
  | `GET, ["devinfo"]             -> devinfo api ()
  | `GET, "t2mi_seq"::[sec]       -> get_t2mi_seq api sec ()
  | `GET, ["structs"]             -> get_structs api ()
  | `GET, ["bitrates"]            -> get_bitrates api ()
  | `GET, ["state"]               -> get_state s_state ()
  | `GET, ["incoming_streams"]    -> get_incoming_streams streams ()

  | `GET, ["state_ws"]            -> state_ws sock_data s_state body
  | `GET, ["config_ws"]           -> config_ws sock_data events body
  | `GET, ["status_ws"]           -> status_ws sock_data events body
  | `GET, ["ts_errors_ws"]        -> ts_errors_ws sock_data events body
  | `GET, ["t2mi_errors_ws"]      -> t2mi_errors_ws sock_data events body
  | `GET, ["board_errors_ws"]     -> board_errors_ws sock_data events body
  | `GET, ["bitrate_ws"]          -> bitrate_ws sock_data events body
  | `GET, ["structs_ws"]          -> structs_ws sock_data events body
  | `GET, ["t2mi_info_ws"]        -> t2mi_info_ws sock_data events body
  | `GET, ["jitter_ws"]           -> jitter_ws sock_data events body
  | `GET, ["incoming_streams_ws"] -> incoming_streams_ws sock_data streams body
  | _ -> not_found ()

let handlers id api events s_state s_input streams =
  [ (module struct
       let domain = Common.Topology.get_api_path id
       let handle = handle api events s_state s_input streams
     end : Api_handler.HANDLER) ]
