open Js_of_ocaml
open Containers
open Tyxml_js

type event =
  | Mouse of Dom_html.mouseEvent Js.t
  | Touch of Dom_html.touchEvent Js.t

let page_factor = 10.

let clamp ?(min = 0.) ?(max = 100.) (v : float) : float =
  CCFloat.min (CCFloat.max v min) max

module Markup = Components_tyxml.Slider.Make(Xml)(Svg)(Html)

class t (elt : #Dom_html.element Js.t) () =
object(self)

  val mutable _touch_id = None
  val mutable _prevent_focus_state = false

  val mutable _vertical = false
  val mutable _min = 0.
  val mutable _max = 100.
  val mutable _step = None
  val mutable _value = 0.
  val mutable _disabled = false

  inherit Widget.t elt () as super

  method! initial_sync_with_dom () : unit =
    super#initial_sync_with_dom ();
    _vertical <- super#has_class "vertical FIXME";

  method vertical : bool =
    _vertical

  method min : float =
    _min

  method max : float =
    _max

  method step : float option =
    _step

  method value : float =
    _value

  method set_value (v : float) : unit =
    self#set_value_ ~fire_input:false v

  (* Private methods *)

  method private set_value_ ~fire_input (v : float) : unit =
    ignore fire_input;
    _value <- v

  method private notify_change () : unit =
    (* FIXME implement DOM event *)
    ()

  method private notify_input () : unit =
    (* FIXME implement DOM event *)
    ()

  method private percent_to_value (percent : float) : float =
    let min, max = self#min, self#max in
    ((max -. min) *. percent) /. 100. +. min

  method private round_to_step ~(step : float) (value : float) : float =
    Float.round (value /. step) *. step

  method private get_offset (rect : Widget.rect) : float * float =
    let Widget.{ left; bottom; _ } = rect in
    let window = Js.Unsafe.coerce Dom_html.window in
    let (page_y : float), (page_x : float) =
      window##.pageYOffset, window##.pageXOffset in
    bottom +. page_y,
    left +. page_x

  method private get_mouse_position : event -> float * float = function
    | Mouse e ->
       begin match Js.Optdef.(to_option e##.pageX,
                              to_option e##.pageY) with
       | Some page_x, Some page_y ->
          float_of_int page_x, float_of_int page_y
       | _ -> failwith "no page coordinates in mouse event"
       end
    | Touch e ->
       let touches = e##.changedTouches in
       let unwrap x = Js.Optdef.get x (fun () -> assert false) in
       let rec aux acc i =
         if i >= touches##.length then acc else
           let touch = unwrap (touches##item i) in
           match _touch_id with
           | None -> Some touch
           | Some id ->
              if touch##.identifier = id then Some touch else
                aux acc (succ i) in
       match aux None 0 with
       | None -> failwith "no touch event found"
       | Some t -> float_of_int t##.pageX, float_of_int t##.pageY

  method private calculate_percent (e : event) =
    let ({ width; height; _ } as rect : Widget.rect) =
      super#bounding_client_rect in
    match width, height with
    | Some width, Some height ->
       let bottom, left = self#get_offset rect in
       let x, y = self#get_mouse_position e in
       let value =
         if self#vertical
         then bottom -. y
         else x -. left in
       let one_percent =
         if self#vertical
         then height /. 100.
         else width /. 100. in
       if super#is_rtl () && not self#vertical
       then 100. -. (clamp (value /. one_percent))
       else clamp (value /. one_percent)
    | _ -> 0.

  method private emit_change ?(jump_animation = false)
                   (raw_value : float) : unit =
    ignore raw_value;
    ignore jump_animation

  method private handle_focus _ _ : unit Lwt.t =
    if not _prevent_focus_state
    then super#add_class Markup.CSS.focus;
    Lwt.return_unit

  method private handle_blur _ _ : unit Lwt.t =
    _prevent_focus_state <- false;
    super#remove_class Markup.CSS.focus;
    Lwt.return_unit

  method private handle_click (e : Dom_html.mouseEvent Js.t) _
                 : unit Lwt.t =
    let e = Mouse e in
    let percent = self#calculate_percent e in
    let value = self#percent_to_value percent in
    self#emit_change value;
    Lwt.return_unit

  method private handle_mouse_move (e : Dom_html.mouseEvent Js.t) _
                 : unit Lwt.t =
    let e = Mouse e in
    let percent = self#calculate_percent e in
    let value = self#percent_to_value percent in
    self#emit_change value;
    Lwt.return_unit

  method private handle_keydown (e : Dom_html.keyboardEvent Js.t) _
                 : unit Lwt.t =
    let key = Utils.Keyboard_event.event_to_key e in
    let min, max, value = self#min, self#max, self#value in
    let one_percent = Float.abs ((max -. min) /. 100.) in
    let step = Option.get_or ~default:one_percent self#step in
    let value = match key with
      | `Arrow_left | `Arrow_down -> Some (value -. step)
      | `Arrow_right | `Arrow_up -> Some (value +. step)
      | `Home -> Some min
      | `End -> Some max
      | `Page_up -> Some (value +. one_percent *. page_factor)
      | `Page_down -> Some (value -. one_percent *. page_factor)
      | _ -> None in
    match value with
    | None -> Lwt.return_unit
    | Some value ->
       Dom.preventDefault e;
       let value = clamp ~min ~max value in
       super#add_class Markup.CSS.focus;
       self#emit_change value;
       Lwt.return_unit


end
