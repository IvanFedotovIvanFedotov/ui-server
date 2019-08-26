open Js_of_ocaml
(*open Netlib*)
open Components
open Pipeline_types
(*open Pipeline_http_js*)

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

(*

  let containers =
    [ make_container
        ~title:"Россия 1"
        ~position:{ x = 0.; y = 0.; w = 0.2; h = 0.2 }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"ТНТ"
        ~position:{ x = 0.2; y = 0.; w = 0.5; h = 0.2 }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"Канал"
        ~position:{ x = 0.7; y = 0.; w = 0.4; h = 0.5 }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"Первый канал"
        ~position:{ x = 0.; y = 0.2; w = 0.7; h = 0.8 }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"СТС"
        ~position:{ x = 0.7; y = 0.5; w = 0.4; h = 0.5 }
        ~widgets:(annotate_widgets widgets)
        ()
    ]

*)



  let containers =
    [ make_container
        ~title:"Россия 1"
        ~position:{ x = 0.1; y = 0.; w = 0.2; h = 0.2 }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"ТНТ"
        ~position:{ x = 0.3; y = 0.; w = 0.4; h = 0.2 }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"Канал"
        ~position:{ x = 0.7; y = 0.; w = 0.4; h = 0.5 }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"Первый канал"
        ~position:{ x = 0.1; y = 0.2; w = 0.6; h = 0.6 }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"СТС"
        ~position:{ x = 0.7; y = 0.5; w = 0.4; h = 0.3 }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"1"
        ~position:{ x = 0.0; y = 0.0; w = 0.1; h = 0.3 }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"2"
        ~position:{ x = 0.0; y = 0.3; w = 0.1; h = 0.5 }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"3"
        ~position:{ x = 0.0; y = 0.8; w = 0.1; h = 0.2 }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"4"
        ~position:{ x = 0.1; y = 0.8; w = 0.3; h = 0.2 }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"5"
        ~position:{ x = 0.4; y = 0.8; w = 0.3; h = 0.2 }
        ~widgets:(annotate_widgets widgets)
        ()
    ; make_container
        ~title:"6"
        ~position:{ x = 0.7; y = 0.8; w = 0.4; h = 0.2 }
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
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let thread =
    Http_wm.get_layout ()
    >>= fun wm -> Http_structure.get_annotated ()
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
        E.stop ~strong:true notif;
        E.stop ~strong:true wm_event;
        E.stop ~strong:true streams_event;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok editor in
  let loader = Ui_templates.Loader.make_widget_loader thread in
  Element.add_class loader "wm";
  scaffold#set_body loader
*)
(*
let () =
let () =
  let open React in
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let thread =
    Http_wm.get_layout ()
    >>= fun wm -> Http_structure.get_annotated ()
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
        E.stop ~strong:true notif;
        E.stop ~strong:true wm_event;
        E.stop ~strong:true streams_event;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok editor in
  let loader = Components_lab.Loader.make_widget_loader thread in
  Element.add_class loader "wm";
  scaffold#set_body loader
*)
  
  
let () =
  (*let open React in*)
  let (scaffold : Scaffold.t) = Js.Unsafe.global##.scaffold in
  let thread =
    Lwt.return_ok Test.wm
    (*Http_wm.get_layout ()*)
    >>= fun wm -> (*Http_structure.get_annotated () *)
    (*>>= fun streams ->
    Api_js.Websocket.JSON.open_socket ~path:(Uri.Path.Format.of_string "ws") ()
    >>= fun socket -> Http_wm.Event.get socket
    >>= fun (_, wm_event) -> Http_structure.Event.get_annotated socket
    >>= fun (_, streams_event) -> *)
    let editor = Container_editor.make ~scaffold [] wm in
    (*let notif =
      E.merge (fun _ -> editor#notify) ()
        [ E.map (fun x -> `Layout x) wm_event
        ; E.map (fun x -> `Streams x) streams_event
        ] in
    editor#set_on_destroy (fun () ->
        E.stop ~strong:true notif;
        E.stop ~strong:true wm_event;
        E.stop ~strong:true streams_event;
        Api_js.Websocket.close_socket socket); *)
    Lwt.return_ok editor in
  let loader = Components_lab.Loader.make_widget_loader thread in
  Element.add_class loader "wm";
  scaffold#set_body loader  
  
  

