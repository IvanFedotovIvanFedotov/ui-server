open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_tyxml.Snackbar.Make(Xml)(Svg)(Html)

module Const = struct
  let def_auto_dismiss_timeout_ms = 5000.
  let max_auto_dismiss_timeout_ms = 10000.
  let min_auto_dismiss_timeout_ms = 4000.

  (* These constants need to be kept in sync with the values in _variables.scss *)
  let animation_close_time_ms = 75.
  let animation_open_time_ms = 150.

  (** Number of milliseconds to wait between temporarily clearing the label
      text in the DOM and subsequently restoring it. This is necessary to force
      IE 11 to pick up the `aria-live` content change and announce it to the
      user *)
  let aria_live_delay_ms = 1000.
end

module Selectors = struct
  let action = "." ^ Markup.CSS.action
  let dismiss = "." ^ Markup.CSS.dismiss
  let label = "." ^ Markup.CSS.label
  let surface = "." ^ Markup.CSS.surface
end

type dismiss_reason =
  | Action
  | Dismiss
  | Custom of string

class t ?(auto_dismiss_timeout = Const.def_auto_dismiss_timeout_ms)
        ?(close_on_escape = true)
        (elt : #Dom_html.element Js.t)
        () =
object(self)

  val mutable _animation_frame = None
  val mutable _animation_timer : (unit Lwt.t * Dom_html.timeout_id_safe) option = None
  val mutable _auto_dismiss_timer = None
  val mutable _auto_dismiss_timeout = auto_dismiss_timeout
  val mutable _is_open = false
  val mutable _close_on_escape = close_on_escape

  val mutable _keydown_handler = None
  val mutable _surface_click_handler = None

  val action_button : Element.t option =
    Element.query_selector elt Selectors.action
  val label_element : Element.t =
    Option.get_exn @@ Element.query_selector elt Selectors.label

  inherit Widget.t elt () as super

  method! init () : unit =
    super#init ()

  method! initial_sync_with_dom () : unit =
    super#initial_sync_with_dom ();
    let (keydown_handler : unit Lwt.t) =
      super#listen_lwt Events.Typ.keydown self#handle_keydown in
    _keydown_handler <- Some keydown_handler;
    let (surface : Element.t) =
      Option.get_exn
      @@ Element.query_selector elt Selectors.surface in
    let (surface_click_handler : unit Lwt.t) =
      Events.listen_lwt surface Events.Typ.click self#handle_surface_click in
    _surface_click_handler <- Some surface_click_handler

  method! destroy () : unit =
    super#destroy ();
    Option.iter Lwt.cancel _keydown_handler;
    _keydown_handler <- None;
    Option.iter Lwt.cancel _surface_click_handler;
    _surface_click_handler <- None

  method timeout : float = _auto_dismiss_timeout

  method set_timeout (x : float) : unit =
    if x <=. Const.max_auto_dismiss_timeout_ms
       && x >=. Const.min_auto_dismiss_timeout_ms
    then _auto_dismiss_timeout <- x
    else (
      let s =
        Printf.sprintf
          "timeout must be in the range (%g - %g), but got %g"
          Const.max_auto_dismiss_timeout_ms
          Const.min_auto_dismiss_timeout_ms
          x in
      failwith s)

  method close_on_escape : bool = _close_on_escape

  method set_close_on_escape (x : bool) : unit =
    _close_on_escape <- x

  method label_text : string =
    Js.Opt.map label_element##.textContent Js.to_string
    |> fun x -> Js.Opt.get x (fun () -> "")

  method set_label_text (s : string) : unit =
    label_element##.textContent := Js.some @@ Js.string s

  method action_button_text : string option =
    Option.flat_map (fun e ->
        Js.Opt.map e##.textContent Js.to_string
        |> Js.Opt.to_option) action_button

  method set_action_button_text (s : string) : unit =
    match action_button with
    | None -> ()
    | Some button -> button##.textContent := Js.some @@ Js.string s

  method is_open : bool = _is_open

  method open_ () : unit Lwt.t =
    let t, w = Lwt.task () in
    _is_open <- true;
    super#remove_class Markup.CSS.closing;
    super#add_class Markup.CSS.opening;
    (* Wait a frame once display is no longe "none",
       to establish basis for animation *)
    self#run_next_animation_frame t (fun () ->
        super#add_class Markup.CSS.open_;
        let timer =
          Utils.set_timeout (fun () ->
              self#handle_animation_timer_end w;
              let dismiss_timer =
                Utils.set_timeout (fun () ->
                    Lwt.ignore_result @@ self#close ~reason:Dismiss ())
                  self#timeout in
              _auto_dismiss_timer <- Some dismiss_timer)
            Const.animation_open_time_ms in
        _animation_timer <- Some (t, timer));
    t

  method close ?(reason : dismiss_reason option) () : unit Lwt.t =
    match self#is_open with
    | false -> Lwt.return_unit
    | true ->
       let t, w = Lwt.task () in
       (match _animation_frame with
        | None -> ()
        | Some frame -> Utils.Animation.cancel_animation_frame frame);
       self#clear_auto_dismiss_timer ();
       _is_open <- false;
       super#add_class Markup.CSS.closing;
       super#remove_class Markup.CSS.open_;
       super#remove_class Markup.CSS.opening;
       (match _animation_timer with
        | None -> ()
        | Some (t, timer) ->
           Lwt.cancel t;
           Utils.clear_timeout timer);
       let timer =
         Utils.set_timeout (fun () -> self#handle_animation_timer_end w)
           Const.animation_close_time_ms in
       _animation_timer <- Some (t, timer);
       ignore reason;
       t

  (* Private methods *)

  method private handle_surface_click (e : #Dom_html.event Js.t)
                   (_ : unit Lwt.t) : unit Lwt.t =
    Js.Opt.map e##.target (fun (elt : Dom_html.element Js.t) ->
        if Js.Opt.test @@ Element.closest elt Selectors.action
        then self#handle_action_button_click ()
        else if Js.Opt.test @@ Element.closest elt Selectors.dismiss
        then self#handle_action_icon_click ()
        else Lwt.return_unit)
    |> fun x -> Js.Opt.get x Lwt.return

  method private handle_keydown (e : Dom_html.keyboardEvent Js.t)
                   (_ : unit Lwt.t) : unit Lwt.t =
    match Events.Key.of_event e, self#close_on_escape with
    | `Escape, true -> self#close ~reason:Dismiss ()
    | _ -> Lwt.return_unit

  method private handle_action_button_click () : unit Lwt.t =
    self#close ~reason:Action ()

  method private handle_action_icon_click () : unit Lwt.t =
    self#close ~reason:Dismiss ()

  method private clear_auto_dismiss_timer () =
    match _auto_dismiss_timer with
    | None -> ()
    | Some timer ->
       Utils.clear_timeout timer;
       _auto_dismiss_timer <- None

  method private handle_animation_timer_end (w : unit Lwt.u) =
    Lwt.wakeup w ();
    _animation_timer <- None;
    super#remove_class Markup.CSS.opening;
    super#remove_class Markup.CSS.closing

  (** Runs the given logic on the next animation frame, using setTimeout
      to factor in Firefox reflow behaviour *)
  method private run_next_animation_frame t f =
    (match _animation_frame with
    | None -> ()
    | Some frame -> Utils.Animation.cancel_animation_frame frame);
    let frame =
      Utils.Animation.request_animation_frame (fun _ ->
          _animation_frame <- None;
          (match _animation_timer with
           | None -> ()
           | Some (t, timer) ->
              Lwt.cancel t;
              Utils.clear_timeout timer);
          _animation_timer <- Some (t, (Utils.set_timeout f 0.))) in
    _animation_frame <- Some frame

end
