open Js_of_ocaml
open Netlib
open Components
open Wm_types
open Basic_widgets
open Container
open Pipeline_types
open Pipeline_http_js

let ( >>= ) = Lwt.( >>= )

module Selector = struct
  let video = "." ^ Page_mosaic_editor_tyxml.CSS.video
end

type container_grids =
  { rect : Wm.position
  ; grids : (int * int) list
  }

let get_bounding_rect (positions : Wm.position list) : Wm.position =
  let open Wm in
  match positions with
  | [] -> { left = 0; right = 0; top = 0; bottom = 0 }
  | hd :: tl ->
    List.fold_left (fun acc (x : Wm.position) ->
        let { left; top; bottom; right } = x in
        let left = min acc.left left in
        let top = min acc.top top in
        let bottom = max acc.bottom bottom in
        let right = max acc.right right in
        { left; top; right; bottom}) hd tl

let get_bounding_rect_and_grids (positions : Wm.position list) =
  let rect = get_bounding_rect positions in
  let resolution = rect.right - rect.left, rect.bottom - rect.top in
  let positions  =
    List.map (fun x -> pos_absolute_to_relative x rect) positions in
  { rect
  ; grids = Utils.get_grids ~resolution ~positions ()
  }

let resize ~(resolution : int * int)
    ~(to_position : 'a -> Wm.position)
    ~(f : Wm.position -> 'a -> 'a) = function
  | [] -> []
  | l ->
    let grids = get_bounding_rect_and_grids @@ List.map to_position l in
    let rect = grids.rect |> Utils.to_grid_position in
    let nw, nh =
      Utils.resolution_to_aspect (rect.w, rect.h)
      |> Dynamic_grid.Position.correct_aspect
        { x = 0
        ; y = 0
        ; w = fst resolution
        ; h = snd resolution }
      |> (fun p -> p.w, p.h) in
    let w, h =
      if nw > rect.w
      then List.hd grids.grids
      else List.fold_left (fun acc (w, h) ->
          if w > (fst acc) && w <= nw
          then (w,h) else acc) (0,0) grids.grids in
    let cw, rh = nw / w, nh / h in
    let dx = (fst resolution - (w * cw)) / 2 in
    let dy = (snd resolution - (h * rh)) / 2 in
    let apply (item : 'a) : 'a =
      Utils.of_grid_position rect
      |> pos_absolute_to_relative (to_position item)
      |> Layer.grid_pos_of_layout_pos
        ~resolution:(rect.w, rect.h) ~cols:w ~rows:h
      |> (fun pos -> Dynamic_grid.Position.(
          { x = (pos.x * cw) + dx
          ; y = (pos.y * rh) + dy
          ; w = pos.w * cw
          ; h = pos.h * rh }))
      |> Utils.of_grid_position
      |> (fun x -> f x item)
    in
    List.map apply l

let resize_container (p : Wm.position) (t : Wm.container wm_item) =
  let resolution = p.right - p.left, p.bottom - p.top in
  let widgets =
    resize ~resolution
      ~to_position:(fun (_, (x : Wm.widget)) -> Utils.Option.get x.position)
      ~f:(fun pos (s, (x : Wm.widget)) -> s, { x with position = Some pos })
      (List.filter (fun (_, (w : Wm.widget)) -> Option.is_some w.position) t.item.widgets)
      (* TODO cleanup the the mess induced by relative widget position *)
  in
  { t with item = Wm.{ position = p; widgets }}

let resize_layout ~(resolution : int * int) (l : Wm.container wm_item list) =
  let containers =
    resize ~resolution
      ~to_position:(fun (t : Wm.container wm_item) -> t.item.position)
      ~f:resize_container
      l
  in
  containers

module Widget_item : Item with type item = Wm.widget = struct

  type item = Wm.widget [@@deriving yojson, eq]

  type layout_item = string * item

  type t = item wm_item  [@@deriving yojson, eq]

  let max_layers = 10

  let update_min_size (t : t) = t

  let t_to_layout_item (t : t) = t.name, t.item

  let t_of_layout_item (k, (v : item)) =
    let path =
      let open Icon.SVG.Path in
      match v.type_ with
      | Video -> video
      | Audio -> music in
    let t =
      { icon = Icon.SVG.(make_simple path)#widget
      ; name = k
      ; unique = true
      ; min_size = None
      ; item = v } in
    update_min_size t

  let to_grid_item (t : t) (pos : Dynamic_grid.Position.t) =
    let widget = Item_info.make_widget_info t in
    let keep_ar   =
      match t.item.aspect with
      | Some _ -> true
      | None   -> false in
    Dynamic_grid.Item.to_item ~keep_ar ~widget ~value:t ~pos ()

  let layer_of_t (t : t) = t.item.layer

  let size_of_t (t : t) = match t.item.aspect with
    | Some (x, y) -> Some x, Some y
    | None -> None, None

  let layers_of_t_list l =
    List.fold_left (fun acc x ->
        if List.mem (layer_of_t x) acc
        then acc else layer_of_t x :: acc) [] l
    |> List.sort compare
    |> function [] -> [0] | l -> l

  (* TODO check if this invarian always holds *)
  let position_of_t (t : t) = Utils.Option.get t.item.position

  let update_layer (t : t) (layer : int) =
    { t with item = { t.item with layer } }

  let update_position (t : t) (p : Wm.position) =
    { t with item = { t.item with position = Some p }}

  let make_item_name (t : t) _ =
    let typ =
      match t.item.type_ with
      | Video -> "Видео "
      | Audio -> "Аудио " in
    match t.item.pid with
    | Some pid -> typ ^ "PID:" ^ string_of_int pid
    | None     -> t.name

  let make_item_properties (t : t React.signal) _ _ =
    Item_properties.make_widget_props t

end

module Cont = Editor.Make(Container.Container_item)
module Widg = Editor.Make(Widget_item)

let get_free_widgets containers widgets =
  let used = List.fold_left (fun acc (_, (x : Wm.container)) ->
      x.widgets @ acc) [] containers in
  List.filter (fun (k, (v : Wm.widget)) ->
      let eq (k1, _) (k2, _) = String.equal k1 k2 in
      not @@ List.exists (eq (k,v)) used)
    widgets

(* Switches top app bar between contextual action mode and normal mode *)
let transform_top_app_bar
    ?(actions = [])
    ~(title : string)
    ~(class_ : string)
    (scaffold : Scaffold.t)
    (cont : Cont.t)
    (x : Top_app_bar.t) =
  let prev_title = x#title in
  let prev_actions = x#actions in
  x#set_title title;
  x#add_class class_;
  x#set_actions @@ List.map Widget.root actions;
  (* scaffold#set_on_navigation_icon_click (fun _ _ ->
   *     (React.S.value cont.ig#s_active)#clear_selection ();
   *     Lwt.return_unit); *)
  (fun () ->
     scaffold#set_on_navigation_icon_click_default ();
     List.iter Widget.destroy actions;
     x#set_title prev_title;
     x#set_actions prev_actions;
     x#remove_class class_)

let handle_item_selected
    scaffold
    set_edit
    (cont : Cont.t) =
  React.S.fold ~eq:(==) (fun acc -> function
      | None ->
        (match acc with
         | Some f -> f (); None
         | None -> None)
      | Some item ->
        match scaffold#top_app_bar with
        | None -> acc
        | Some x ->
          let (value : Container_item.t) = item#value in
          let remove =
            Actions.make_action
              ~on_click:(fun _ _ -> item#remove (); Lwt.return_unit)
              { icon = Icon.SVG.(make_simple Path.delete)#widget
              ; name = "Удалить"
              } in
          let edit =
            Actions.make_action
              ~on_click:(fun _ _ -> set_edit item; Lwt.return_unit)
              { icon = Icon.SVG.(make_simple Path.pencil)#widget
              ; name = "Редактировать"
              } in
          let restore =
            transform_top_app_bar
              ~class_:Page_mosaic_editor_tyxml.CSS.top_app_bar_contextual
              ~actions:[edit; remove]
              ~title:value.name
              scaffold
              cont
              x in
          match acc with
          | None -> Some restore
          | Some _ as acc -> acc)
    None React.E.never
    (* (React.S.changes cont.ig#s_selected) *)

let create
    ~(init : Wm.t)
    ~(post : Wm.t -> unit Lwt.t)
    (scaffold : Scaffold.t)
    (main : Widget.t)
    (*socket*) =
  (* Convert widgets positions to relative *)
  let open React in
  let wc =
    List.map Widget_item.t_of_layout_item
    @@ get_free_widgets init.layout init.widgets in
  let s_wc, s_wc_push = React.S.create wc in
  (* FIXME icon shoud be common *)
  let new_cont =
    ({ icon = Icon.SVG.(make_simple Path.contain)#widget
     ; name = "Новый контейнер"
     ; unique = false
     ; min_size = None
     ; item =
         { position = { left = 0; right = 0; top = 0; bottom = 0 }
         ; widgets = []
         }
     } : Container_item.t) in
  let containers = [new_cont] in
  let s_cc, s_cc_push = React.S.create containers in
  let wz_e, wz_push = React.E.create () in
  (*let wz_dlg, wz_show = Wizard.to_dialog socket init wz_push in*)
  let resolution = init.resolution in
  let s_state, s_state_push = React.S.create `Container in
  let title = "Контейнеры" in
  let on_remove = fun (t : Wm.container wm_item) ->
    print_endline "remove";
    let eq = Widget_item.equal in
    let ws = List.map Widget_item.t_of_layout_item t.item.widgets in
    List.iter (fun x -> Editor.remove ~eq s_wc s_wc_push x) ws in
  let wizard =
    Actions.make_action
      { icon = Icon.SVG.(make_simple Path.auto_fix)#widget
      ; name = "Мастер"
      } in
  (*let wz = Events.clicks wizard#root (fun _ _ -> wz_show ()) in
  wizard#set_on_destroy (fun () -> Lwt.cancel wz);*)
  let cont =
    Cont.make ~title
      ~init:(List.map Container_item.t_of_layout_item init.layout)
      ~candidates:s_cc
      ~set_candidates:s_cc_push
      ~resolution
      ~on_remove
      () in
  let add =
    Actions.make_action
      ~on_click:(fun _ _ ->
        match scaffold#side_sheet with
          | None -> Lwt.return_unit
          | Some x ->
            x#remove_children ();
            x#append_child cont.rt;
            cont.rt#layout ();
            x#toggle ())
      { icon = Icon.SVG.(make_simple Path.plus)#widget
      ; name = "Добавить"
      } in
  let layers =
    Actions.make_action
      ~on_click:(fun _ _ ->
          match scaffold#side_sheet with
          | None -> Lwt.return_unit
          | Some x ->
            x#remove_children ();
            let layers = List_of_layers.make
                ~init:[]
                ~max:10 in
            x#append_child layers;
            layers#layout ();
            x#toggle ())
      { icon = Icon.SVG.(make_simple Path.layers)#widget
      ; name = "Слои"
      } in
  let size =
    Actions.make_action
      { icon = Icon.SVG.(make_simple Path.aspect_ratio)#widget
      ; name = "Разрешение"
      } in
  (match scaffold#top_app_bar with
   | None -> ()
   | Some app_bar ->
     let actions =
       (List.map Widget.root [wizard; size; add; layers])
       @ app_bar#actions in
     app_bar#set_actions actions);
  let e_edit, set_edit = React.E.create () in
  let _s_selected = handle_item_selected scaffold set_edit cont in
  let add_to_view ig rt =
    main#remove_children (); main#append_child ig;
    main#set_on_layout ig#layout;
    main#layout ();
    match scaffold#side_sheet with
    | None -> ()
    | Some x ->
      x#remove_children ();
      x#append_child rt
  in
  ignore
  @@ React.S.map (function
      | `Widget (w : Widg.t) -> add_to_view w.ig w.rt
      | `Container -> add_to_view cont.ig cont.rt)
    s_state

let post = fun w ->
  Http_wm.set_layout w
  >>= function
  | Ok () -> Lwt.return ()
  | Error e ->
    print_endline @@ Api_js.Http.error_to_string e;
    Lwt.return ()

(*
(* TODO seems that it can fail, handle exception *)
let on_data socket
    (scaffold : Scaffold.t)
    (main : Widget.t)
    wm =
  main#remove_children ();
  create ~init:wm ~post scaffold main socket

let ( >>= ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f)

let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  (match Option.bind (fun x -> x#leading) scaffold#top_app_bar with
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
    let main = Widget.create_div () in
    on_data socket scaffold main wm;
    main#add_class "wm";
    let e = React.E.map (on_data socket scaffold main) event in
    main#set_on_destroy (fun () ->
        React.E.stop ~strong:true e;
        React.E.stop ~strong:true event;
        Api_js.Websocket.close_socket socket);
    Lwt.return_ok main in
  let body = Ui_templates.Loader.create_widget_loader
      (* ~parent:scaffold#app_content_inner *)
      thread in
  scaffold#set_body body
*)

let on_data (*socket*)
    (scaffold : Scaffold.t)
    (main : Widget.t)
    wm =
  main#remove_children ();
  create ~init:wm ~post scaffold main (* socket*)

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
