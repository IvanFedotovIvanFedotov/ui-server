open Js_of_ocaml
open Containers
open Dynamic_grid_types
open Dom_events

include Dynamic_grid_cell

type action =
  | Mouse of Dom_html.mouseEvent Js.t
  | Touch of Dom_html.touchEvent Js.t

let remove_event = "MDCDynamicGridItem:remove"

let ( % ) = Fun.( % )

let to_item ?min_w ?min_h ?max_w ?max_h
      ?(keep_ar = false)
      ?(resizable = true)
      ?(draggable = true)
      ?(selectable = true)
      ?on_resize ?on_resizing ?on_drag ?on_dragging
      ?close_widget ?move_widget ?widget ~pos ~value () =
  { pos; min_w; min_h; max_w; max_h; keep_ar; resizable; draggable; selectable;
    on_resize; on_resizing; on_drag; on_dragging;
    close_widget; move_widget; widget; value}

let listen ?save target typ f =
  let listener = Dom_events.listen target typ f in
  Option.iter (fun f -> f @@ Some listener) save

let stop_listen = Option.iter Dom_events.stop_listen

(* given a touchList Js.t, finds a touch with needed identifier among *)
let rec find_touch id num source =
  if num < 0 then None else
    if Js.Optdef.test (source##item num)
    then let touch =
           Js.Optdef.get (source##item num)
             (fun () -> failwith "No touch with such id") in
         if touch##.identifier = id
         then Some touch
         else find_touch id (num - 1) source
    else find_touch id (num - 1) source

let filter ~(exclude : #Widget.t list) (l : #Widget.t list) =
  List.filter (fun x -> not (List.mem ~eq:Widget.equal x exclude)) l

class ['a] t ~s_grid (* grid props *)
        ~(item : 'a item) (* item props *)
        ~s_selected (* selected items *)
        ~s_selected_push (* selected items signal modifier *)
        ~s_col_w (* column width signal -- px *)
        ~s_row_h (* row height signal -- px *)
        ~(s_items : 'a t list React.signal) (* items signal *)
        () =
  let s_value, s_value_push = React.S.create item.value in
  object(self)

    inherit ['a] cell ~typ:`Item ~s_col_w ~s_row_h ~s_grid ~pos:item.pos () as super

    (* FIXME make normal signal *)
    val s_change = React.S.create ~eq:(fun _ _ -> false) item.pos

    val resize_button =
      let icon = Icon.SVG.(make_simple Path.resize_bottom_right) in
      icon#add_class Markup.Item.resize_class;
      icon

    val mutable _item = item

    val mutable _main_move_mouse_listener = None
    val mutable _main_move_touch_listener = None
    val mutable _main_resize_mouse_listener = None
    val mutable _main_resize_touch_listener = None
    val mutable _main_select_listener = None

    val mutable mov_listener = None
    val mutable end_listener = None
    val mutable cancel_listener = None

    val mutable selected = false
    val mutable drag_timer = None

    (** API *)

    method! init () : unit =
      super#init ();
      (self#_keep_s
       @@ React.S.map (function
              | Some x -> self#_set_draggable x
              | None -> self#_set_draggable self#draggable)
       @@ React.S.map ~eq:(Equal.option Equal.bool) (fun x -> x.draggable) s_grid);
      (self#_keep_s
       @@ React.S.map (function
              | Some x -> self#_set_resizable x
              | None -> self#_set_resizable self#resizable)
       @@ React.S.map ~eq:(Equal.option Equal.bool) (fun x -> x.resizable) s_grid);
      (self#_keep_s
       @@ React.S.map (function
              | Some x -> self#_set_selectable x
              | None -> self#_set_selectable self#selectable)
       @@ React.S.map ~eq:(Equal.option Equal.bool) (fun x -> x.selectable) s_grid);
      (* add close listener to close widget if provided *)
      Option.iter (fun x ->
          Dom_events.listen x#root Dom_events.Typ.click (fun _ _ ->
              self#remove (); true) |> ignore) item.close_widget;
      (* append widget to cell if provided *)
      Option.iter self#append_child item.widget;

    method! layout () : unit =
      super#layout ();
      Option.iter (fun x -> x#layout ()) _item.widget;
      Option.iter (fun x -> x#layout ()) item.move_widget;
      Option.iter (fun x -> x#layout ()) item.close_widget

    method! destroy () : unit =
      super#destroy ();
      Option.iter (fun x -> x#destroy ()) item.widget;
      Option.iter (fun x -> x#destroy ()) item.move_widget;
      Option.iter (fun x -> x#destroy ()) item.close_widget;

    method! set_pos (p : Position.t) =
      super#set_pos p

    method set_min_w (x : int option) =
      _item <- { _item with min_w = x }

    method set_min_h (x : int option) =
      _item <- { _item with min_h = x }

    method set_max_w (x : int option) =
      _item <- { _item with max_w = x }

    method set_max_h (x : int option) =
      _item <- { _item with max_h = x }

    method set_keep_ar (x : bool) =
      _item <- { _item with keep_ar = x }

    method s_changing : Position.t React.signal =
      (* FIXME formely ghost post was here *)
      self#s_pos

    (* FIXME this is ok, but will raise change event even before dragging is not over *)
    method s_change : Position.t React.signal =
      self#s_pos

    method s_value : 'a React.signal =
      s_value

    method value : 'a =
      React.S.value self#s_value

    method set_value (x : 'a) : unit =
      s_value_push x

    method draggable : bool =
      _item.draggable

    method set_draggable (x : bool) : unit =
      self#_set_draggable x;
      _item <- { _item with draggable = x }

    method resizable : bool =
      _item.resizable

    method set_resizable (x : bool) : unit =
      self#_set_resizable x;
      _item <- { _item with resizable = x }

    method selectable : bool =
      _item.selectable

    method set_selectable (x : bool) : unit =
      self#_set_selectable x;
      _item <- { _item with selectable = x }

    method selected : bool =
      selected

    method set_selected (x : bool) : unit =
      let o = React.S.value s_selected in
      match x with
      | true ->
         if self#grid.multi_select
         then (if not self#selected
               then s_selected_push ((self :> 'a t) :: o))
         else (List.iter (fun x -> if not (Widget.equal x (self :> 'a t))
                                   then x#set_selected false) o;
               s_selected_push [(self :> 'a t)]);
         self#add_class Markup.Item.selected_class;
         selected <- true
      | false ->
         if self#selected
         then (super#remove_class Markup.Item.selected_class;
               selected <- false;
               s_selected_push
               @@ List.filter (not % Widget.equal (self :> 'a t)) o)

    method remove () : unit =
      self#set_selected false;
      super#emit ~should_bubble:true
        remove_event (Js.Unsafe.inject super#root)

    (** Private methods *)

    method private _set_draggable x =
      (match x with
       | true  -> self#add_move_listener ()
       | false -> self#stop_move_listener ());
      self#get_drag_target#toggle_class ~force:x Markup.Item.drag_handle_class

    method private _set_resizable x =
      (match x with
       | true  -> self#add_resize_listener ()
       | false -> self#stop_resize_listener ());
      if x then super#append_child resize_button
      else super#remove_child resize_button

    method private _set_selectable x =
      (match x with
       | true  ->
          self#add_select_listener ();
          super#set_attribute "tabindex" "0"
       | false ->
          self#stop_select_listener ();
          super#set_attribute "tabindex" "-1";
          self#set_selected false);
      super#toggle_class ~force:x Markup.Item.select_handle_class

    method private grid : grid =
      React.S.value s_grid

    method private get_drag_target : Widget.t =
      match item.move_widget with
      | Some w -> w
      | None -> (self :> Widget.t)

    method private items =
      React.S.value s_items

    method private has_collision (pos : Position.t) : bool =
      Position.has_collision ~f:(fun x -> x#pos) pos
        (List.filter (not % Widget.equal (self :> 'a t)) self#items)

    method private mouse_action meth ghost ev =
      let init_pos = px_pos in
      let init_x, init_y = ev##.clientX, ev##.clientY in
      listen ~save:(fun x -> mov_listener <- x)
        Dom_html.window Typ.mousemove (fun _ ev ->
          let x,y = ev##.clientX, ev##.clientY in
          meth ~x ~y ~init_x ~init_y ~init_pos `Move ghost;
          false);
      listen ~save:(fun x -> end_listener <- x)
        Dom_html.window Typ.mouseup (fun _ ev ->
          if ev##.button = 0
          then (let x, y = ev##.clientX, ev##.clientY in
                meth ~x ~y ~init_x ~init_y ~init_pos `End ghost);
          false);

    method private touch_action meth ghost ev =
      let init_pos = px_pos in
      Js.Optdef.iter
        (ev##.touches##item (ev##.touches##.length - 1))
        (fun touch ->
          let id = touch##.identifier in
          let init_x, init_y = touch##.clientX, touch##.clientY in
          listen ~save:(fun x -> mov_listener <- x)
            Dom_html.window Typ.touchmove (fun _ ev ->
              (match find_touch id
                       (ev##.changedTouches##.length - 1)
                       ev##.changedTouches with
               | Some touch -> meth
                                 ~x:touch##.clientX
                                 ~y:touch##.clientY
                                 ~init_x ~init_y ~init_pos
                                 `Move
                                 ghost
               | None       -> ());
              false);
          listen ~save:(fun x -> end_listener <- x)
            Dom_html.window Typ.touchend (fun _ ev ->
              (match find_touch id
                       (ev##.changedTouches##.length - 1)
                       ev##.changedTouches with
               | Some touch -> meth
                                 ~x:touch##.clientX
                                 ~y:touch##.clientY
                                 ~init_x ~init_y ~init_pos
                                 `End
                                 ghost
               | None       -> ());
              false);
          listen ~save:(fun x -> cancel_listener <- x)
            Dom_html.window Typ.touchcancel (fun _ ev ->
              (match find_touch id
                       (ev##.changedTouches##.length - 1)
                       ev##.changedTouches with
               | Some touch -> meth
                                 ~x:touch##.clientX
                                 ~y:touch##.clientY
                                 ~init_x ~init_y ~init_pos
                                 `End
                                 ghost
               | None -> ());
              false))

    method private resolve_pos_conflicts ~action ghost (pos : Position.t) =
      let other = filter ~exclude:[(self :> 'a t)] self#items in
      let f = fun x -> x#pos in
      let cols, rows = self#grid.cols, self#grid.rows in
      let new_pos =
        match List.filter (fun x -> Position.collides pos x#pos) other with
        | [] -> pos
        | l ->
           begin match self#grid.vertical_compact with
           | false -> ghost#pos
           | true  ->
              let bind f = function [] -> f () | l -> l in
              let ( >>= ) x f = bind f x in
              let check_top () = match action with
                | `Size -> []
                | `Drag -> Position.move_top ~f ~eq:Widget.equal
                             ~collisions:l pos other in
              let check_bot () =
                Position.move_down ?rows ~f ~eq:Widget.equal
                  ~collisions:l
                  pos other in
              let check_swap () =
                Position.swap ~cols ~f ~eq:Widget.equal
                  ~collisions:l
                  ~ghost_pos:ghost#pos
                  pos other in
              let res =
                check_top ()
                >>= check_bot
                >>= check_swap
                >>= fun () -> [] in
              match res with
              | [] -> ghost#pos
              | l -> List.iter (fun (pos, item) -> item#set_pos pos) l;
                     pos
           end in
      begin match action with
      | `Drag ->
         if self#grid.vertical_compact
         then ghost#set_pos @@ Position.compact ~f new_pos other
         else ghost#set_pos new_pos;
      | `Size -> ghost#set_pos new_pos
      end;
      if self#grid.vertical_compact;
      then
        List.iter (fun x ->
            let lst = filter ~exclude:[(x:>'a t)] other
                      |> List.map f
                      |> List.cons ghost#pos in
            let pos = Position.compact ~f:(fun x -> x) x#pos lst in
            x#set_pos pos)
          (Position.sort_by_y ~f other)

    method private make_ghost () : 'a cell =
      new Dynamic_grid_cell.cell
        ~typ:`Ghost
        ~s_col_w
        ~s_row_h
        ~s_grid
        ~pos:self#pos
        ()

    method private start_dragging (ev : action) =
      stop_listen mov_listener;
      stop_listen end_listener;
      stop_listen cancel_listener;
      if self#draggable
      then
        (self#get_drag_target#add_class Markup.Item.dragging_class;
         let ghost = self#make_ghost () in
         ghost#style##.zIndex := Js.string "1";
         self#style##.zIndex := Js.string "3";
         (* add ghost item to dom to show possible element position *)
         Option.iter (fun x -> Dom.appendChild x ghost#root)
           super#parent_element;
         match ev with
         | Mouse ev -> self#mouse_action self#apply_position ghost ev
         | Touch ev -> self#touch_action self#apply_position ghost ev)

    method private start_resizing (ev : action) =
      if self#resizable
      then (let ghost = self#make_ghost () in
            ghost#style##.zIndex := Js.string "4";
            self#style##.zIndex := Js.string "3";
            Option.iter (fun x -> Dom.appendChild x ghost#root)
              super#parent_element;
            (* add resize/stop resize event listeners *)
            match ev with
            | Mouse ev ->
               Dom_html.stopPropagation ev;
               self#mouse_action self#apply_size ghost ev
            | Touch ev ->
               Dom_html.stopPropagation ev;
               self#touch_action self#apply_size ghost ev)

    method private apply_position ~x ~y ~init_x ~init_y ~init_pos typ ghost =
      match x, y with
      | 0, 0 -> ()
      | _ ->
         let col_px, row_px =
           React.S.value s_col_w,
           React.S.value s_row_h in
         match typ with
         | `Move ->
            let open Utils in
            let cols, rows = self#grid.cols,self#grid.rows in
            let x, y = init_pos.x + x - init_x, init_pos.y + y - init_y in
            let pos = Position.correct_xy { self#pos with x = x // col_px
                                                        ; y = y // row_px }
                        cols rows in
            self#resolve_pos_conflicts ~action:`Drag ghost pos;
            let x, y =
              if self#grid.restrict_move
              then (let pos = Position.correct_xy { self#px_pos with x;y }
                                (cols * col_px)
                                (Option.map (fun x -> x * row_px) rows)
                    in
                    pos.x, pos.y)
              else x,y in
            self#set_x x;
            self#set_y y;
            self#layout ();
            Option.iter (fun f -> f self#pos ghost#pos col_px row_px)
              item.on_dragging
         | `End ->
            self#get_drag_target#remove_class Markup.Item.dragging_class;
            Option.iter Dom_events.stop_listen mov_listener;
            Option.iter Dom_events.stop_listen end_listener;
            Option.iter Dom_events.stop_listen cancel_listener;
            self#style##.zIndex := Js.string "";
            (* update element position from ghost *)
            self#set_x @@ React.S.value s_col_w * ghost#pos.x;
            self#set_y @@ React.S.value s_row_h * ghost#pos.y;
            self#set_pos ghost#pos;
            Option.iter (fun x -> try Dom.removeChild x ghost#root with _ -> ())
              super#parent_element;
            ghost#destroy ();
            if self#grid.vertical_compact
            then List.iter (fun x ->
                     let lst = filter ~exclude:[(x:>'a t)] self#items in
                     let pos = Position.compact ~f:(fun x -> x#pos) x#pos lst in
                     x#set_pos pos)
                   (Position.sort_by_y ~f:(fun x -> x#pos) self#items);
            (snd s_change) self#pos;
            self#layout ();
            Option.iter (fun f -> f self#pos ghost#pos col_px row_px) item.on_drag
         | _ -> ()

    method private apply_size ~x ~y ~init_x ~init_y ~init_pos typ ghost =
      let col_px, row_px =
        React.S.value s_col_w,
        React.S.value s_row_h in
      match typ with
      | `Move ->
         let open Utils in
         let cols, rows = self#grid.cols, self#grid.rows in
         let w, h = init_pos.w + x - init_x, init_pos.h + y - init_y in
         let pos =
           let { max_w; min_w; max_h; min_h; _ } = _item in
           Position.correct_wh ?max_w ?min_w ?max_h ?min_h
             { self#pos with w = w // col_px
                           ; h = h // row_px }
             cols rows in
         let pos =
           if not _item.keep_ar then pos else
             let resolution = self#pos.w,self#pos.h in
             let aspect = Utils.resolution_to_aspect resolution in
             Position.correct_aspect pos aspect in
         self#resolve_pos_conflicts ~action:`Size ghost pos;
         let w, h =
           if self#grid.restrict_move
           then
             (let pos =
                Position.correct_wh { self#px_pos with w; h }
                  (cols * col_px)
                  (Option.map (( * ) row_px) rows) in
              pos.w, pos.h)
           else w, h in
         self#set_w w;
         self#set_h h;
         self#layout ();
         Option.iter (fun f -> f self#pos ghost#pos col_px row_px)
           item.on_resizing
      | `End ->
         stop_listen mov_listener;
         stop_listen end_listener;
         stop_listen cancel_listener;
         self#style##.zIndex := Js.string "";
         (* update element position from ghost *)
         self#set_w @@ React.S.value s_col_w * ghost#pos.w;
         self#set_h @@ React.S.value s_row_h * ghost#pos.h;
         self#set_pos ghost#pos;
         Option.iter (fun x -> try Dom.removeChild x ghost#root with _ -> ())
           super#parent_element;
         ghost#destroy ();
         if self#grid.vertical_compact
         then List.iter (fun x ->
                  let lst = filter ~exclude:[(x:>'a t)] self#items in
                  let pos = Position.compact ~f:(fun x -> x#pos) x#pos lst in
                  x#set_pos pos)
                (Position.sort_by_y ~f:(fun x -> x#pos) self#items);
         (snd s_change) self#pos;
         self#layout ();
         Option.iter (fun f -> f self#pos ghost#pos col_px row_px)
           item.on_resize
      | _ -> ()

    method private stop_move_listener () =
      stop_listen _main_move_mouse_listener;
      stop_listen _main_move_touch_listener;
      _main_move_mouse_listener <- None;
      _main_move_touch_listener <- None

    method private add_move_listener () =
      self#stop_move_listener ();
      listen ~save:(fun l -> _main_move_mouse_listener <- l)
        self#get_drag_target#root Typ.mousedown
        (fun _ e ->
          Dom_html.stopPropagation e;
          listen ~save:(fun x -> mov_listener <- x) Dom_html.window Typ.mousemove
            (fun _ _ ->
              if e##.button = 0 && self#draggable
              then self#start_dragging (Mouse e);
              false);
          listen ~save:(fun x -> end_listener <- x) Dom_html.window Typ.mouseup
            (fun _ _ -> stop_listen mov_listener; false);
          true);
      listen ~save:(fun l -> _main_move_touch_listener <- l)
        self#get_drag_target#root Typ.touchstart
        (fun _ e ->
          Dom_html.stopPropagation e;
          if self#selectable && not self#grid.multi_select then self#set_selected true;
          if e##.touches##.length <= 1
          then
            ( let timer = 400. in                 (**  TIMER is here **)
              let touch = Js.Optdef.get (e##.touches##item 0)
                            (fun () -> failwith "No touch with such id") in
              let id = touch##.identifier in
              let wrapped =
                Js.wrap_callback
                  (fun _ ->
                    if self#draggable
                    then self#start_dragging (Touch e)) in
              let timeout = Dom_html.window##setTimeout wrapped timer in
              let stop_timeout e =
                let touch = Js.Optdef.get (e##.changedTouches##item 0)
                              (fun () -> failwith "No touch with such id") in
                if touch##.identifier = id
                then (Dom_html.window##clearTimeout timeout;
                      stop_listen mov_listener;
                      stop_listen end_listener;
                      stop_listen cancel_listener)
              in
              listen ~save:(fun x -> mov_listener <- x)
                Dom_html.window Typ.touchmove
                (fun _ ev ->
                  (match find_touch id
                           (ev##.changedTouches##.length-1)
                           ev##.changedTouches with
                   | Some touch1 ->
                      let dx = abs @@ touch1##.clientX - touch##.clientX in
                      let dy = abs @@ touch1##.clientY - touch##.clientY in
                      if dx > 8 || dy > 8
                      then (Dom_html.window##clearTimeout timeout;
                            stop_listen mov_listener;
                            stop_listen end_listener;
                            stop_listen cancel_listener);
                   | None       -> ());
                  false);
              listen ~save:(fun x -> cancel_listener <- x)
                Dom_html.window Typ.touchcancel
                (fun _ e -> stop_timeout e; false);
              listen ~save:(fun x -> end_listener <- x)
                Dom_html.window Typ.touchend
                (fun _ e -> stop_timeout e; false));
          false);

    method private stop_resize_listener () =
      stop_listen _main_resize_mouse_listener;
      stop_listen _main_resize_touch_listener;
      _main_resize_mouse_listener <- None;
      _main_resize_touch_listener <- None

    method private add_resize_listener () =
      self#stop_resize_listener ();
      listen ~save:(fun l -> _main_resize_mouse_listener <- l)
        resize_button#root Typ.mousedown (fun _ e ->
          Dom_html.stopPropagation e;
          Dom.preventDefault e;
          if e##.button = 0 && self#resizable
          then self#start_resizing (Mouse e);
          false);
      listen ~save:(fun l -> _main_resize_touch_listener <- l)
        resize_button#root Typ.touchstart (fun _ e ->
          Dom_html.stopPropagation e;
          if e##.touches##.length <= 1
          then (if self#resizable then self#start_resizing (Touch e));
          false)

    method private stop_select_listener () =
      stop_listen _main_select_listener;
      _main_select_listener <- None

    method private add_select_listener () =
      self#stop_select_listener ();
      listen ~save:(fun l -> _main_select_listener <- l)
        super#root Typ.focus (fun _ _ ->
          if self#selectable && not self#grid.multi_select
          then self#set_selected true; true);

  end
