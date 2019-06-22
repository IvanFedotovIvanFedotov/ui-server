open Js_of_ocaml
open Netlib
open Components
open Pipeline_types
open Pipeline_http_js
(*
let ( >>= ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  (match Utils.Option.bind (fun x -> x#leading) scaffold#top_app_bar with
   | None -> ()
   | Some x ->
     let icon = Icon.SVG.(make_simple Path.close) in
     Dom.appendChild x icon#root);
  let thread =
    Http_wm.get_layout ()
    >>= fun wm ->
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>= fun socket -> Http_wm.Event.get socket
    >>= fun (_, event) ->
    let editor = Editor.make wm scaffold in
    let e = React.E.map (fun x -> editor#notify (`Layout x)) event in
    editor#set_on_destroy (fun () ->
        React.E.stop ~strong:true e;
        React.E.stop ~strong:true event;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok editor in
  let body = Ui_templates.Loader.create_widget_loader thread in
  body#add_class "wm";
  scaffold#set_body body
*)



let ( >>= ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let wm2 () = 
  let a=({
  resolution = (1280,720);
  widgets = [
  "ID", { Pipeline_types.Wm.  
    type_ = Video
  ; domain = Nihil
  ; pid = None
  ; position = None
  ; layer = 0
  ; aspect = None
  ; description = "Description"
  }]
  ; layout = []
}:Pipeline_types.Wm.t) in 
  Lwt.return_ok a

  
  
let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  (match Utils.Option.bind (fun x -> x#leading) scaffold#top_app_bar with
   | None -> ()
   | Some x ->
     let icon = Icon.SVG.(make_simple Path.close) in
     Dom.appendChild x icon#root);
  let thread = wm2 ()
    (*Http_wm.get_layout ()*)
    >>= fun wm ->
    (*Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>= fun socket -> Http_wm.Event.get socket
    >>= fun (_, event) -> *)
    let editor = Editor.make wm scaffold in
    (*let e = React.E.map (fun x -> editor#notify (`Layout x)) event in
    editor#set_on_destroy (fun () ->
        React.E.stop ~strong:true e;
        React.E.stop ~strong:true event;
        Api_js.Websocket.close_socket socket);*)
    Lwt.return_ok editor in
  let body = Ui_templates.Loader.create_widget_loader thread in
  body#add_class "wm";
  scaffold#set_body body
  
  
  (*
let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  (match Option.bind (fun x -> x#leading) scaffold#top_app_bar with
   | None -> ()
   | Some x ->
     let icon = Icon.SVG.(make_simple Path.close) in
     Dom.appendChild x icon#root);
  let thread = wm2 ()
    (*Http_wm.get_layout () *)
    >>= fun wm ->
    (*Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") () 
    >>= fun socket -> Http_wm.Event.get socket
    >>= fun (_, event) ->*)
    let main = Widget.create_div () in
    on_data (*socket*) scaffold main wm;
    main#add_class "wm";
    (*let e = React.E.map (on_data socket scaffold main) event in
    main#set_on_destroy (fun () ->
        React.E.stop ~strong:true e;
        React.E.stop ~strong:true event;
        Api_js.Websocket.close_socket socket);*)
    Lwt.return_ok main in
  let body = Ui_templates.Loader.create_widget_loader
      (*~parent:scaffold#app_content_inner*)
      thread in
  scaffold#set_body body
  *)
