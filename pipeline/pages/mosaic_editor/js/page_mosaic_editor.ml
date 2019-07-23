open Js_of_ocaml
open Netlib
open Components
open Pipeline_types
open Pipeline_http_js

let ( >>= ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

module Test = struct
  let make_widget 
      (index:int)
      ?(type_ = Wm.Video)
      ?(domain = Wm.Nihil)
      ?aspect
      ~x ~y ~w ~h () : string * Wm.widget =
    let (position : Wm.position) = { x; y; w; h } in
    string_of_int @@ Random.bits (),
    { position = Some position
    ; description = String.concat "" ("Widget_" :: string_of_int(index) :: []) 
    ; pid = Some 4096
    ; type_
    ; aspect
    ; domain
    ; layer = 0
    }

  let annotate_widgets ?(state = `Active) w =
    List.map (fun (id, w) -> id, state, w) w

  let make_container
      ?(title = "Sample container")
      ?(widgets = [])
      ~position () : string * Wm.Annotated.state * Wm.Annotated.container =
    title, `Active, { position; widgets }

  let widgets =
    [ make_widget 1 ~type_:Audio ~x:0. ~y:0. ~w:(50. /. 580.) ~h:(50. /. 220.) ()
    ; make_widget 2 ~type_:Audio ~x:(10. /. 580.) ~y:(10. /. 580.) ~w:(50. /. 580.) ~h:(50. /. 220.) ()
    ; make_widget 3 ~type_:Audio ~x:(20. /. 580.) ~y:(20. /. 580.) ~w:(50. /. 580.) ~h:(50. /. 220.) ()
    ; make_widget 4 ~type_:Audio ~x:(30. /. 580.) ~y:(30. /. 580.) ~w:(50. /. 580.) ~h:(50. /. 220.) ()
    ; make_widget 5 ~type_:Audio ~x:(40. /. 580.) ~y:(40. /. 580.) ~w:(50. /. 580.) ~h:(50. /. 220.) ()
    ; make_widget 6 ~aspect:(16, 9) ~x:(50. /. 580.) ~y:0. ~w:(50. /. 580.) ~h:(50. /. 220.) ()
    ; make_widget 7
        ~domain:(Chan { stream = Application_types.Stream.ID.make "id"
                      ; channel = 2
                      })
        ~x:0. ~y:(50. /. 220.) ~w:(50. /. 580.) ~h:(50. /. 220.) ()
    ]

  let containers =
    [ make_container
        ~title:"Россия 1"
        ~position:{ x = 0.; y = 0.; w = 240. /. 1280.; h = 160. /. 720. }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"ТНТ"
        ~position:{ x = 240. /. 1280.; y = 0.; w = 520. /. 1280.; h = 160. /. 720. }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"Канал"
        ~position:{ x = 760. /. 1280.; y = 0.; w = 520. /. 1280.; h = 360. /. 720. }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"Первый канал"
        ~position:{ x = 0.; y = 160. /. 720.; w = 760. /. 1280.; h = 560. /. 720. }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"СТС"
        ~position:{ x = 760. /. 1280.; y = 360. /. 720.; w = 520. /. 1280.; h = 360. /. 720. }
        ~widgets:(annotate_widgets widgets)
        ()
    ]

  let (wm : Wm.Annotated.t) =
    { layout = containers
    ; widgets = widgets
    ; resolution = 1280, 720
    }

end
(*
let () =
  let open React in
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let thread =
    (* Lwt.return_ok Test.wm *)
    Http_wm.get_layout ()
    >>= fun wm ->
    (* let wm = { wm with widgets = Test.widgets } in *)
    Http_structure.get_annotated ()
    >>= fun streams ->
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>= fun socket -> Http_wm.Event.get socket
    >>= fun (_, wm_event) -> Http_structure.Event.get_annotated socket
    >>= fun (_, streams_event) ->
    let editor = Container_editor.make ~scaffold streams wm in
    let notif =
      E.merge (fun _ -> editor#notify) ()
        [ E.map (fun x -> `Layout x) wm_event
        ; E.map (fun x -> `Streams x) streams_event
        ] in
    editor#set_on_destroy (fun () ->
        React.E.stop ~strong:true notif;
        React.E.stop ~strong:true wm_event;
        React.E.stop ~strong:true streams_event;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok editor in
  let body = Ui_templates.Loader.create_widget_loader thread in
  body#add_class "wm";
  scaffold#set_body body
*)

let () =
  let open React in
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let thread =
    Lwt.return_ok Test.wm
    (* Http_wm.get_layout () *)
    >>= fun wm ->
    (*Http_structure.get_streams_applied_with_source ()
    >>= fun streams ->
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>= fun socket -> Http_wm.Event.get socket
    >>= fun (_, wm_event) -> Http_structure.Event.get_streams_applied_with_source socket
    >>= fun (_, streams_event) ->*)
    let editor = Container_editor.make ~scaffold [] wm in
    (*let notif =
      E.merge (fun _ -> editor#notify) ()
        [ E.map (fun x -> `Layout x) wm_event
        ; E.map (fun x -> `Streams x) streams_event
        ] in
    editor#set_on_destroy (fun () ->
        React.E.stop ~strong:true notif;
        React.E.stop ~strong:true wm_event;
        React.E.stop ~strong:true streams_event;
        Api_js.Websocket.close_socket socket);*)
    Lwt.return_ok editor in
  let body = Ui_templates.Loader.create_widget_loader thread in
  body#add_class "wm";
  scaffold#set_body body

