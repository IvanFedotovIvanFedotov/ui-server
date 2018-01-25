open Api.Interaction
open Common.Topology
open Meta_board
open Containers

open Websocket_cohttp_lwt
open Frame
open Lwt.Infix
open Api.Interaction
   
open Hardware

(* TODO reason about random key *)
let () = Random.init (int_of_float @@ Unix.time ())
let rand_int = fun () -> Random.run (Random.int 10000000)

let socket_table = Hashtbl.create 1000

let topology topo () =
  respond_js (topology_to_yojson (React.S.value topo)) ()

let get_page () =
  respond_html_elt
    Tyxml.Html.(div
                  [ h2 [ pcdata "Hardware page" ];
                    p  [ pcdata "Some text" ];
                    div ~a:[ a_id "hardware_container" ] [  ] ] )
    ()

let topology_socket sock_data body topo () =
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
    let msg = Yojson.Safe.to_string @@ topology_to_yojson x in
    frames_out_fn @@ Some (Frame.create ~content:msg ())
  in
  let sock_events = Lwt_react.S.map send topo in
  Hashtbl.add socket_table id sock_events;
  Lwt.return (resp, (body :> Cohttp_lwt.Body.t))

let handle hw _ meth args sock_data _ body =
  match meth, args with
  | `GET, []                  -> get_page ()
  | `GET, ["topology_sock"]   -> topology_socket sock_data body hw.topo ()
  | `GET, ["topology"]        -> topology hw.topo ()
  | _        -> Api.Redirect.not_found ()

let handlers hw =
  let hls = Hardware.Map.fold (fun _ x acc -> x.handlers @ acc) hw.boards [] in
  [ Api_handler.add_layer "board" hls ;
    (module struct
       let domain = "hardware"
       let handle = handle hw
     end : Api_handler.HANDLER) ]