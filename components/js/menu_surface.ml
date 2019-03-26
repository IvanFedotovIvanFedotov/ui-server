open Js_of_ocaml
open Tyxml_js
open Containers

module Markup = Components_markup.Menu_surface.Make(Xml)(Svg)(Html)

let transform_prop_name = ref None

let get_transform_property_name ?(force = false) () : string =
  match !transform_prop_name, force with
  | None, _ | _, true ->
     let elt = Dom_html.(createDiv document) in
     let _ = elt##.style in
     if Js.Optdef.test (Js.Unsafe.coerce elt##.style)##.transform
     then "transform" else "webkitTransform"
  | Some v, _ -> v

let transition_open_duration = 120.
let transition_close_duration = 75.
let margin_to_edge = 32.
let anchor_to_menu_surface_width_ratio = 0.67

let opened_event_name = "MDCMenuSurface:opened"
let closed_event_name = "MDCMenuSurface:closed"

let focusables =
  "button:not(:disabled), \
   [href]:not([aria-disabled=\"true\"]), \
   input:not(:disabled), \
   select:not(:disabled), \
   textarea:not(:disabled), \
   [tabindex]:not([tabindex=\"-1\"]):not([aria-disabled=\"true\"])"

module Event = struct
  let opened : Dom_html.event Js.t Widget.Event.typ =
    Widget.Event.make opened_event_name
  let closed : Dom_html.event Js.t Widget.Event.typ =
    Widget.Event.make closed_event_name
end

type position =
  { top : float
  ; right : float
  ; left : float
  ; bottom : float
  }

let get_position_value_by_name (pos : position) = function
  | "top" -> pos.top
  | "right" -> pos.right
  | "left" -> pos.left
  | "bottom" -> pos.bottom
  | _ -> invalid_arg "bad position key"

let make_position ?(top = 0.) ? (right = 0.) ?(left = 0.) ?(bottom = 0.) ()
    : position =
  { top; right; left; bottom }

type point =
  { x : float
  ; y : float
  }

type layout =
  { viewport : int * int
  ; viewport_distance : position
  ; anchor_height : float
  ; anchor_width : float
  ; surface_height : float
  ; surface_width : float
  ; body_dimensions : int * int
  ; window_scroll : int * int
  }

module Corner = struct

  type t =
    | Top_left
    | Top_right
    | Bottom_left
    | Bottom_right
    | Top_start
    | Top_end
    | Bottom_start
    | Bottom_end

  let to_string = function
    | Top_left -> "top left"
    | Top_right -> "top right"
    | Bottom_left -> "bottom left"
    | Bottom_right -> "bottom right"
    | Top_start -> "top start"
    | Top_end -> "top end"
    | Bottom_start -> "bottom start"
    | Bottom_end -> "bottom end"

  let is_bottom = function
    | Bottom_left | Bottom_right | Bottom_start | Bottom_end -> true
    | _ -> false

  let is_right = function
    | Top_right | Bottom_right | Top_end | Bottom_end -> true
    | _ -> false

  let is_flip_rtl = function
    | Top_start | Top_end | Bottom_start | Bottom_end -> true
    | _ -> false

end

let check_finite (x : float) : bool =
  Js.to_bool (Js.Unsafe.global##isFinite x)

let is_focused (x : #Dom_html.element Js.t) : bool =
  Js.Opt.to_option Dom_html.document##.activeElement
  |> function None -> false | Some a -> Equal.physical a x

class t ?(anchor_corner = Corner.Top_start)
        ?(quick_open = false) ~widgets () =
  let elt = To_dom.of_element
            @@ Markup.create
            @@ List.map Widget.to_markup widgets in
  object(self)
    inherit Widget.t elt () as super

    val mutable anchor_element = None

    val mutable anchor_corner : Corner.t = anchor_corner
    val mutable anchor_margin : position = make_position ()
    val mutable quick_open = quick_open
    val mutable is_open = false
    val mutable previous_focus = None
    val mutable hoisted_element = false
    val mutable is_fixed_position = false

    val mutable first_focusable = None
    val mutable last_focusable = None

    val mutable open_animation_timer_id = None
    val mutable close_animation_timer_id = None
    val mutable animation_request_id = None

    val mutable dimensions = 0, 0
    val mutable position = { x = 0.; y = 0. }

    val mutable keydown_listener = None
    val mutable body_click_listener = None

    method! init () : unit =
      super#init ();
      let parent = match Js.Opt.to_option super#root##.parentNode with
        | None -> None
        | Some p ->
           let (p : Dom_html.element Js.t) = Js.Unsafe.coerce p in
           if Js.to_bool (p##.classList##contains (Js.string Markup.CSS.anchor))
           then Some p else None in
      anchor_element <- parent;
      if super#has_class Markup.CSS.open_ then is_open <- true;
      if super#has_class Markup.CSS.fixed then self#set_fixed_position true;
      let keydown =
        super#listen_lwt Widget.Event.keydown (fun e _ ->
            self#handle_keydown e;
            Lwt.return_unit) in
      keydown_listener <- Some keydown;

    method! destroy () : unit =
      super#destroy ();
      Option.iter Utils.clear_timeout open_animation_timer_id;
      open_animation_timer_id <- None;
      Option.iter Utils.clear_timeout close_animation_timer_id;
      close_animation_timer_id <- None;
      Option.iter Utils.Animation.cancel_animation_frame animation_request_id;
      animation_request_id <- None;
      Option.iter Lwt.cancel keydown_listener;
      keydown_listener <- None

    method close () : unit =
      if is_open then self#close_ ()

    method reveal () : unit =
      if not is_open then self#open_ ()

    method opened : bool =
      is_open

    method set_quick_open (x : bool) : unit =
      quick_open <- x

    method set_is_hoisted (x : bool) : unit =
      hoisted_element <- x

    method hoist_menu_to_body () : unit =
      Option.iter (fun parent -> Dom.removeChild parent super#root)
        (Js.Opt.to_option super#root##.parentNode);
      Widget.append_to_body self;
      self#set_is_hoisted true

    method set_anchor_element (elt : Widget.element) : unit =
      anchor_element <- Some elt

    method set_anchor_widget (w : Widget.t) : unit =
      anchor_element <- Some w#root

    method set_fixed_position (x : bool) : unit =
      super#add_or_remove_class x Markup.CSS.fixed;
      is_fixed_position <- x

    method set_absolute_position ((x : float), (y : float)) : unit =
      let x = if check_finite x then x else 0. in
      let y = if check_finite y then y else 0. in
      position <- { x; y };
      self#set_is_hoisted true

    method set_anchor_margin (x : position) : unit =
      anchor_margin <- x

    method set_anchor_corner (c : Corner.t) : unit =
      anchor_corner <- c

    (* Private methods *)

    method private open_ () : unit =
      let focusables = super#root##querySelectorAll (Js.string focusables) in
      let first_focusable' = match focusables##.length with
        | 0 -> None
        | _ -> Js.Opt.to_option (focusables##item 0) in
      let last_focusable' = match focusables##.length with
        | 0 -> None
        | l -> Js.Opt.to_option (focusables##item (l - 1)) in
      first_focusable <- first_focusable';
      last_focusable <- last_focusable';
      let active = Js.Opt.to_option Dom_html.document##.activeElement in
      previous_focus <- active;
      if not quick_open then super#add_class Markup.CSS.animating_open;
      let id =
        Utils.Animation.request_animation_frame (fun _ ->
            super#add_class Markup.CSS.open_;
            dimensions <- super#offset_width, super#offset_height;
            self#auto_position ();
            if quick_open
            then self#notify_open ()
            else
              let timer =
                Utils.set_timeout (fun () ->
                    open_animation_timer_id <- None;
                    super#remove_class Markup.CSS.animating_open;
                    self#notify_open ())
                  transition_open_duration in
              open_animation_timer_id <- Some timer) in
      animation_request_id <- Some id;
      is_open <- true

    method private close_ () : unit =
      if not quick_open then super#add_class Markup.CSS.animating_closed;
      Utils.Animation.request_animation_frame (fun _ ->
          super#remove_class Markup.CSS.open_;
          if quick_open
          then self#notify_close ()
          else
            let timer =
              Utils.set_timeout (fun () ->
                  close_animation_timer_id <- None;
                  super#remove_class Markup.CSS.animating_closed;
                  self#notify_close ())
                transition_close_duration in
            close_animation_timer_id <- Some timer)
      |> ignore;
      is_open <- false;
      self#maybe_restore_focus ()

    method private handle_body_click (e : #Dom_html.event Js.t) : unit =
      match Js.Opt.to_option e##.target with
      | None -> self#close_ ()
      | Some target ->
         if not (self#is_element_in_container target)
         then self#close_ ()

    method private handle_keydown (e : Dom_html.keyboardEvent Js.t) : unit =
      let shift = Js.to_bool e##.shiftKey in
      match Utils.Keyboard_event.event_to_key e with
      | `Escape -> self#close ()
      | `Tab ->
         let is_focused = Option.map_or ~default:false is_focused in
         if is_focused last_focusable && not shift
         then (Option.iter (fun x -> x##focus) first_focusable;
               Dom.preventDefault e)
         else if is_focused first_focusable && shift
         then (Option.iter (fun x -> x##focus) last_focusable;
               Dom.preventDefault e)
      | _ -> ()

    method private get_origin_corner ({ viewport_distance = dist
                                      ; anchor_height
                                      ; anchor_width
                                      ; surface_height
                                      ; surface_width
                                      ; _
                                      } : layout) : Corner.t =
      let is_bottom_aligned = Corner.is_bottom anchor_corner in
      let available_top =
        if is_bottom_aligned
        then dist.top +. anchor_height +. anchor_margin.bottom
        else dist.top +. anchor_margin.top in
      let available_bot =
        if is_bottom_aligned
        then dist.bottom -. anchor_margin.bottom
        else dist.bottom +. anchor_height -. anchor_margin.top in
      let top_overflow = surface_height -. available_top in
      let bot_overflow = surface_height -. available_bot in
      let is_rtl = super#is_rtl () in
      let is_flip_rtl = Corner.is_flip_rtl anchor_corner in
      let avoid_hor_overlap = Corner.is_right anchor_corner in
      let is_aligned_right =
        (avoid_hor_overlap && not is_rtl)
        || (not avoid_hor_overlap && is_flip_rtl && is_rtl) in
      let available_left =
        if is_aligned_right
        then dist.left +. anchor_width +. anchor_margin.right
        else dist.left +. anchor_margin.left in
      let available_right =
        if is_aligned_right
        then dist.right -. anchor_margin.right
        else dist.right +. anchor_width -. anchor_margin.left in
      let left_overflow = surface_width -. available_left in
      let right_overflow = surface_width -. available_right in
      let is_bottom = bot_overflow >. 0. && top_overflow <. bot_overflow in
      let is_right =
        (left_overflow <. 0. && is_aligned_right && is_rtl)
        || (avoid_hor_overlap && not is_aligned_right && left_overflow <. 0.)
        || (right_overflow >. 0. && left_overflow <. right_overflow) in
      match is_bottom, is_right with
      | false, false -> Top_left
      | false, true -> Top_right
      | true, false -> Bottom_left
      | true, true -> Bottom_right

    method private is_element_in_container (el : Widget.element) : bool =
      Equal.physical super#root el
      || Js.to_bool @@ (Js.Unsafe.coerce super#root)##contains el

    method private maybe_restore_focus () : unit =
      match Js.Opt.to_option Dom_html.document##.activeElement with
      | None -> ()
      | Some el ->
         match previous_focus with
         | None -> ()
         | Some prev ->
            if self#is_element_in_container el
            then prev##focus

    method private notify_open () : unit =
      super#emit opened_event_name (Js.Unsafe.inject ());
      match body_click_listener with
      | Some _ -> ()
      | None ->
         Dom_events.(
          let listener =
            listen Dom_html.document##.body Typ.click (fun _ e ->
                self#handle_body_click e;
                true) in
          body_click_listener <- Some listener)

    method private notify_close () : unit =
      super#emit closed_event_name (Js.Unsafe.inject ());
      Option.iter Dom_events.stop_listen body_click_listener;
      body_click_listener <- None

    method private get_horizontal_origin_offset
                     ({ anchor_width; viewport; body_dimensions; _ } : layout)
                     (corner : Corner.t) : float =
      let avoid_horizontal_overlap = Corner.is_right anchor_corner in
      if Corner.is_right corner
      then (
        let right_offset =
          if avoid_horizontal_overlap
          then anchor_width -. anchor_margin.left
          else anchor_margin.right in
        (* For hoisted or fixed elements, adjust the offset by the difference
           between viewport width and body width so when we calculate the right
           value (`adjustPositionForHoistedElement_`) based on the element
           position, the right property is correct.
         *)
        if hoisted_element || is_fixed_position
        then let diff = fst viewport - fst body_dimensions in
             right_offset -. float_of_int diff
        else right_offset)
      else if avoid_horizontal_overlap
      then anchor_width -. anchor_margin.right
      else anchor_margin.left

    method private get_vertical_origin_offset
                     ({ anchor_height; _ } : layout)
                     (corner : Corner.t) : float =
      let avoid_vertical_overlap = Corner.is_bottom anchor_corner in
      if Corner.is_bottom corner
      then
        if avoid_vertical_overlap
        then anchor_height -. anchor_margin.top
        else anchor_margin.bottom
      else if avoid_vertical_overlap
      then anchor_height +. anchor_margin.bottom
      else anchor_margin.top

    method private get_menu_surface_max_height
                     ({ viewport_distance = dist; anchor_height; _ } : layout)
                     (corner : Corner.t) : float =
      if Corner.is_bottom corner
      then
        let h = dist.top +. anchor_margin.top -. margin_to_edge in
        if Corner.is_bottom anchor_corner
        then h else h +. anchor_height
      else
        let h = dist.bottom -. anchor_margin.bottom -. margin_to_edge in
        if Corner.is_bottom anchor_corner
        then h else h +. anchor_height

    method private auto_position () : unit =
      let meas = self#get_auto_layout_measurements () in
      let corner = self#get_origin_corner meas in
      let valign = if Corner.is_bottom corner then "bottom" else "top" in
      let halign = if Corner.is_right corner then "right" else "left" in
      let voffset = self#get_vertical_origin_offset meas corner in
      let hoffset = self#get_horizontal_origin_offset meas corner in
      let position = [(valign, voffset); (halign, hoffset)] in
      let { anchor_width; surface_width; _ } = meas in
      (* Center align when anchor width is comparable or greater than
         menu surface, otherwise keep corner. *)
      let halign =
        if anchor_width /. surface_width >. anchor_to_menu_surface_width_ratio
        then "center" else halign in
      let position =
        if hoisted_element || is_fixed_position
        then self#adjust_position_for_hoisted_element meas position
        else position in
      Js.Unsafe.set super#style
        (Js.string (get_transform_property_name () ^ "-origin"))
        (Js.string (Printf.sprintf "%s %s" halign valign));
      self#set_position position;
      begin match self#get_menu_surface_max_height meas corner with
      | 0. -> super#style##.maxHeight := Js.string ""
      | x -> super#style##.maxHeight := Utils.px_js (int_of_float x)
      end

    method private adjust_position_for_hoisted_element
                     ({ window_scroll = x, y
                      ; viewport_distance = dist
                      ; _ } : layout)
                     (position : (string * float) list)
                   : (string * float) list =
      List.map (fun (k, v) ->
          (* Hoisted surfaces need to have the anchor elements location
             on the page added to the position properties for proper alignment
             on the body. *)
          let v = v +. get_position_value_by_name dist k in
          let v =
            (* Surfaces that are absolutely positioned need to have
               additional calculations for scroll and bottom positioning. *)
            if is_fixed_position then v else
              match k with
              | "top" | "bottom" -> v +. float_of_int y
              | "left" | "right" -> v +. float_of_int x
              | _ -> v in
          k, v) position

    method private get_auto_layout_measurements () : layout =
      let anchor_rect =
        Option.map (fun e -> Widget.to_rect e##getBoundingClientRect)
          anchor_element in
      let viewport = self#get_window_dimensions () in
      let body = self#get_body_dimensions () in
      let scroll = self#get_window_scroll () in
      let anchor_rect = match anchor_rect with
        | Some x -> x
        | None ->
           Widget.{ top = position.y
                  ; bottom = position.y
                  ; left = position.x
                  ; right = position.x
                  ; height = Some 0.
                  ; width = Some 0.
           } in
      { viewport
      ; viewport_distance =
          { top = anchor_rect.top
          ; right = (float_of_int @@ fst viewport) -. anchor_rect.right
          ; left = anchor_rect.left
          ; bottom = (float_of_int @@ snd viewport) -. anchor_rect.bottom
          }
      ; body_dimensions = body
      ; window_scroll = scroll
      ; anchor_height = Option.get_or ~default:0. anchor_rect.height
      ; anchor_width = Option.get_or ~default:0. anchor_rect.width
      ; surface_height = float_of_int @@ snd dimensions
      ; surface_width = float_of_int @@ fst dimensions
      }

    method private set_position (pos : (string * float) list) : unit =
      let get = List.Assoc.get ~eq:String.equal in
      let conv x =
        Js.string
        @@ Option.map_or ~default:"" (Printf.sprintf "%gpx") x in
      super#style##.top := conv (get "top" pos);
      super#style##.bottom := conv (get "bottom" pos);
      super#style##.right := conv (get "right" pos);
      super#style##.left := conv (get "left" pos)

    method private get_window_dimensions () : int * int =
      Js.Optdef.get Dom_html.window##.innerWidth (fun () -> 0),
      Js.Optdef.get Dom_html.window##.innerHeight (fun () -> 0)

    method private get_window_scroll () : int * int =
      (Js.Unsafe.coerce Dom_html.window)##.pageXOffset,
      (Js.Unsafe.coerce Dom_html.window)##.pageYOffset

    method private get_body_dimensions () : int * int =
      Dom_html.document##.body##.clientWidth,
      Dom_html.document##.body##.clientHeight

    method private get_inner_dimensions () : int * int =
      super#offset_width, super#offset_height
  end
