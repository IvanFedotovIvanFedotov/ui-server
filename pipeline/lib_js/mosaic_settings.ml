open Containers
open Components
open Requests
open Lwt_result.Infix

module Wm = struct

  let make_grid (wm: Wm.t) =
    let ((grid,_,_,_) as res) = Layout.initialize wm in
    let () = grid#add_class "wm-grid" in
    res

  let make_layers () =
    let (grid:Dynamic_grid.grid) = { rows             = None
                                   ; cols             = 3
                                   ; min_col_width    = 20
                                   ; max_col_width    = None
                                   ; row_height       = Some 50
                                   ; vertical_compact = true
                                   ; items_margin     = None
                                   ; multi_select     = false
                                   ; restrict_move    = true
                                   }
    in
    let grid    = new Dynamic_grid.t ~grid ~items:[] () in
    let layers  = Dom_html.createDiv Dom_html.document |> Widget.create in
    let ()      = layers#add_class "wm-layers" in
    let ()      = Dom.appendChild layers#root grid#root in
    let add     = new Icon.Button.Font.t ~icon:"add_box" () in
    let rm      = new Icon.Button.Font.t ~icon:"delete" () in
    let up      = new Icon.Button.Font.t ~icon:"arrow_upward" () in
    let down    = new Icon.Button.Font.t ~icon:"arrow_downward" () in
    let icons   = new Card.Actions.Icons.t ~widgets:[down#widget;up#widget;add#widget;rm#widget] () in
    let actions = new Card.Actions.t ~widgets:[icons#widget] () in
    let card    = new Card.t ~widgets:[layers#widget;actions#widget] () in
    let ()      = grid#set_on_load @@ Some (fun () -> grid#layout) in
    let ()      = card#add_class "wm-layers-card" in
    let _       = React.E.map (fun _ ->
                      let y    = match List.rev @@ grid#items with
                        | []    -> 0
                        | hd::_ -> hd#pos.y + 1
                      in
                      let drag   = new Icon.Font.t ~icon:"drag_handle" () in
                      let text   = new Typography.Text.t ~text:(Printf.sprintf "Слой %d" (y + 1)) () in
                      let box    = new Box.t ~vertical:false ~widgets:[text#widget; drag#widget] () in
                      let ()     = box#set_justify_content `Space_between in
                      let ()     = box#add_class "wm-layer" in
                      let (pos:Dynamic_grid.Position.t) = { x = 0; y; w = 3; h = 1 } in
                      let item = Dynamic_grid.Item.to_item ~pos
                                                           ~move_widget:drag#widget
                                                           ~widget:box#widget
                                                           ~resizable:false
                                                           ~selectable:false
                                                           ~value:()
                                                           ()
                      in
                      grid#add item)
                              add#e_click in
    card

  let make_left_toolbar widgets =
    let _class = "wm-left-toolbar" in
    let box = new Box.t ~widgets () in
    let () = List.iter (fun x -> x#add_class @@ Markup.CSS.add_element _class "action") widgets in
    let () = box#add_class _class in
    box

  let make_right_toolbar widgets =
    let _class = "wm-right-toolbar" in
    let box = new Box.t ~widgets () in
    let () = box#add_class _class in
    box

  let make_placeholder ~text ~icon () =
    let ph = Dom_html.createDiv Dom_html.document |> Widget.create in
    ph#add_class "wm-grid__placeholder";
    let txt = new Typography.Text.t ~text () in
    let ico = new Icon.Font.t ~icon () in
    let box = new Box.t ~widgets:[txt#widget;ico#widget] () in
    let _   = box#set_align_items `Center in
    let _   = box#set_justify_content `Center in
    Dom.appendChild ph#root box#root;
    ph

  let create ~(wm:   Wm.t)
             ~(post: Wm.t -> unit) =
    let open Layout in
    (* grid *)
    let ph = make_placeholder ~text:"Добавьте элементы в раскладку" ~icon:"add_box" () in
    let grid,layout,f_add,_= make_grid wm in
    let grid_wp  = Dom_html.createDiv Dom_html.document |> Widget.create in
    let ()       = Dom.appendChild grid_wp#root grid#root in
    let grid_box = new Box.t ~vertical:false ~widgets:[grid_wp] () in
    let () = grid_wp#add_class "wm-grid-wrapper" in
    let () = grid_box#add_class "wm-grid-container" in
    let () = grid_box#set_align_items `Center in
    let () = grid#set_on_load @@ Some (fun () -> grid#layout) in
    let _  = React.S.map (function
                          | [] -> Dom.appendChild grid#root ph#root
                          | _  -> try Dom.removeChild grid#root ph#root with _ -> ())
                         grid#s_items
    in
    (* left toolbar *)
    let add   = new Fab.t ~mini:true ~icon:"add" () in
    let rm    = new Fab.t ~mini:true ~icon:"delete" () in
    let edit  = new Fab.t ~mini:true ~icon:"edit" () in
    let apply = new Button.t ~label:"применить" () in
    let sel   = React.S.map (function
                             | [x] -> rm#set_disabled false;
                                      edit#set_disabled false;
                                      Some x
                             | _   -> rm#set_disabled true;
                                      edit#set_disabled true;
                                      None)
                            grid#s_selected
    in
    let ()    = add#set_attribute  "title" "Добавить" in
    let ()    = rm#set_attribute   "title" "Удалить" in
    let ()    = edit#set_attribute "title" "Редактировать" in
    let _     = React.E.map (fun _ -> post { wm with layout = React.S.value layout }) apply#e_click in
    let _     = f_add add#e_click in
    let _     = React.E.map (fun _ -> Option.iter (fun x -> x#remove) @@ React.S.value sel) rm#e_click in
    let left_toolbar = make_left_toolbar [add#widget; rm#widget; edit#widget] in
    (* right toolbar *)
    let layers        = make_layers () in
    let right_toolbar = make_right_toolbar [layers#widget] in

    (* main *)
    let lc = new Layout_grid.Cell.t ~widgets:[left_toolbar] () in
    let mc = new Layout_grid.Cell.t ~widgets:[grid_box] () in
    let rc = new Layout_grid.Cell.t ~widgets:[right_toolbar] () in
    let () = lc#set_span 1 in
    let () = mc#set_span 8 in
    let () = rc#set_span 3 in
    let w  = new Layout_grid.t ~cells:[lc; mc; rc] () in
    w

end

class t () =

  let elt = Dom_html.createDiv Dom_html.document in

  object(self)

    val mutable sock : WebSockets.webSocket Js.t option = None

    inherit Widget.widget elt () as super

    method private on_load =
      Requests.get_wm ()
      >>= (fun wm ->
        let e_wm,wm_sock = Requests.get_wm_socket () in
        let s_wm = React.S.hold wm e_wm in
        let open Lwt.Infix in
        let post  = (fun w -> Requests.post_wm w
                              >|= (function
                                   | Ok () -> ()
                                   | Error e -> print_endline @@ "error post wm" ^ e)
                              |> Lwt.ignore_result)
        in
        let id    = "wm-editor" in
        let _     = React.S.map (fun s -> (try Dom.removeChild self#root (Dom_html.getElementById id)
                                           with _ -> print_endline "No el");
                                          let wm_el = Wm.create ~wm:s ~post in
                                          let ()    = wm_el#set_id id in
                                          Dom.appendChild self#root wm_el#root)
                                s_wm
        in
        sock <- Some wm_sock;
        Lwt_result.return ())
      |> ignore

    initializer
      self#add_class "wm";
      super#set_on_unload @@ Some (fun () -> Option.iter (fun x -> x##close; sock <- None) sock);
      super#set_on_load   @@ Some (fun () -> self#on_load);

  end

let page () = new t ()
