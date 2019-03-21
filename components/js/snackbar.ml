open Js_of_ocaml
open Utils

include Components_tyxml.Snackbar
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

type dismiss_reason =
  | Action
  | Dismiss
  | Timeout
  | Custom of string

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
  let action = "." ^ CSS.action
  let dismiss = "." ^ CSS.dismiss
end

module Event = struct
  class type close = object inherit [dismiss_reason] Widget.custom_event end
  class type open_ = object inherit [unit] Widget.custom_event end

  let closing : close Js.t Events.Typ.t =
    Events.Typ.make "snackbar:closing"
  let closed : close Js.t Events.Typ.t =
    Events.Typ.make "snackbar:closed"
  let opening : open_ Js.t Events.Typ.t =
    Events.Typ.make "snackbar:opening"
  let opened : open_ Js.t Events.Typ.t =
    Events.Typ.make "snackbar:opened"
end

let announce ?(label_elt : Element.t option) (aria_elt : Element.t) =
  let label_elt = match label_elt with
    | Some x -> x
    | None -> aria_elt in
  let live_attr = Js.string "aria-live" in
  let priority = aria_elt##getAttribute live_attr in
  let label_text =
    Js.Opt.map label_elt##.textContent (fun (s : Js.js_string Js.t) ->
        s##trim) in
  match Js.Opt.to_option label_text, Js.Opt.to_option priority with
  | None, _ | _, None -> ()
  | Some label_text, Some priority ->
     (* Temporarily disable `aria-live` to prevent JAWS+Firefox from
        announcing the message twice. *)
     Element.set_attribute aria_elt "aria-live" "off";
     (* Temporarily clear `textContent` to force a DOM mutation event that will
        be detected by screen readers.
        `aria-live` elements are only announced when the element's `textContent`
        *changes*, so snackbars sent to the browser in the initial HTML response
        won't be read unless we clear the element's `textContent` first.
        Similarly, displaying the same snackbar message twice in a row doesn't
        trigger a DOM mutation event, so screen readers won't announce the second
        message unless we first clear `textContent`.
        We have to clear the label text two different ways to make it work in all
        browsers and screen readers:
        1. `textContent = ''` is required for IE11 + JAWS
        2. `innerHTML = '&nbsp;'` is required for Chrome + JAWS and NVDA
        All other browser/screen reader combinations support both methods.
        The wrapper `<span>` visually hides the space character so that it doesn't
        cause jank when added/removed.
        N.B.: Setting `position: absolute`, `opacity: 0`, or `height: 0` prevents
        Chrome from detecting the DOM change.
        This technique has been tested in:
        * JAWS 2019:
        - Chrome 70
        - Firefox 60 (ESR)
        - IE 11
        * NVDA 2018:
        - Chrome 70
        - Firefox 60 (ESR)
        - IE 11
        * ChromeVox 53 *)
     label_elt##.textContent := Js.some (Js.string "");
     label_elt##.innerHTML := Js.string "<span style=\"display: inline-block; \
                                         width: 0; height: 1px;\">&nbsp;</span>";
     (* Prevent visual jank by temporarily displaying the label text in the
        ::before pseudo-element. CSS generated content is normally announced
        by screen readers (except in IE 11;
        see https://tink.uk/accessibility-support-for-css-generated-content/);
        however, `aria-live` is turned off, so this DOM update will be ignored
        by screen readers. *)
     let attr = Js.string "data-mdc-snackbar-label-text" in
     label_elt##setAttribute attr label_text;
     set_timeout (fun () ->
         (* Allow screen readers to announce changes to the DOM again. *)
         aria_elt##setAttribute live_attr priority;
         (* Remove the message from the ::before pseudo-element *)
         label_elt##removeAttribute attr;
         (* Restore the original label text,
            which will be announced by screen readers. *)
         label_elt##.textContent := Js.some label_text)
       Const.aria_live_delay_ms
     |> ignore

class t ?(auto_dismiss_timeout = Const.def_auto_dismiss_timeout_ms)
        ?(close_on_escape = true)
        (elt : #Dom_html.element Js.t)
        () =
object(self)
  val mutable _animation_frame = None
  val mutable _animation_timer : (unit Lwt.t * Dom_html.timeout_id_safe) option = None
  val mutable _auto_dismiss_timer = None
  val mutable _auto_dismiss_timeout = auto_dismiss_timeout
  val mutable _close_on_escape = close_on_escape

  val mutable _keydown_handler = None
  val mutable _surface_click_handler = None

  val _action_button : Element.t option =
    Element.query_selector elt Selectors.action
  val _label_element : Element.t =
    find_element_by_class_exn elt CSS.label
  val _surface_element : Element.t =
    find_element_by_class_exn elt CSS.surface

  inherit Widget.t elt () as super

  method! initial_sync_with_dom () : unit =
    super#initial_sync_with_dom ();
    (* Attach event listeners *)
    let (keydown_handler : unit Lwt.t) =
      Events.keydowns super#root self#handle_keydown in
    _keydown_handler <- Some keydown_handler;
    let (surface_click_handler : unit Lwt.t) =
      Events.keydowns _surface_element self#handle_surface_click in
    _surface_click_handler <- Some surface_click_handler

  method! destroy () : unit =
    super#destroy ();
    (* Detach event listeners *)
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
    Js.Opt.map _label_element##.textContent Js.to_string
    |> fun x -> Js.Opt.get x (fun () -> "")

  method set_label_text (s : string) : unit =
    _label_element##.textContent := Js.some @@ Js.string s

  method action_button_text : string option =
    match _action_button with
    | None -> None
    | Some button ->
       Js.Opt.to_option
       @@ Js.Opt.map button##.textContent Js.to_string

  method set_action_button_text (s : string) : unit =
    match _action_button with
    | None -> ()
    | Some button -> button##.textContent := Js.some @@ Js.string s

  method is_open : bool =
    super#has_class CSS.opening || super#has_class CSS.open_

  method open_ () : unit Lwt.t =
    let t, w = Lwt.task () in
    self#notify_opening ();
    super#remove_class CSS.closing;
    super#add_class CSS.opening;
    announce _label_element;
    (* Wait a frame once display is no longe "none",
       to establish basis for animation *)
    self#run_next_animation_frame t (fun () ->
        super#add_class CSS.open_;
        let timer =
          set_timeout (fun () ->
              self#handle_animation_timer_end w;
              self#notify_opened ();
              let dismiss_timer =
                set_timeout (fun () ->
                    Lwt.ignore_result @@ self#close ~reason:Timeout ())
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
        | Some frame -> Animation.cancel_animation_frame frame);
       self#clear_auto_dismiss_timer ();
       self#notify_closing reason;
       super#add_class CSS.closing;
       super#remove_class CSS.open_;
       super#remove_class CSS.opening;
       (match _animation_timer with
        | None -> ()
        | Some (t, timer) ->
           Lwt.cancel t;
           clear_timeout timer);
       let timer =
         set_timeout (fun () ->
             self#handle_animation_timer_end w;
             self#notify_closed reason)
           Const.animation_close_time_ms in
       _animation_timer <- Some (t, timer);
       ignore reason;
       t

  (* Private methods *)

  method private notify_opening () : unit =
    super#emit Event.opening

  method private notify_opened () : unit =
    super#emit Event.opened

  method private notify_closing (reason : dismiss_reason option) : unit =
    super#emit ?detail:reason Event.closing

  method private notify_closed (reason : dismiss_reason option) : unit =
    super#emit ?detail:reason Event.closed

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
       clear_timeout timer;
       _auto_dismiss_timer <- None

  method private handle_animation_timer_end (w : unit Lwt.u) =
    Lwt.wakeup w ();
    _animation_timer <- None;
    super#remove_class CSS.opening;
    super#remove_class CSS.closing

  (** Runs the given logic on the next animation frame, using setTimeout
      to factor in Firefox reflow behaviour *)
  method private run_next_animation_frame t f =
    (match _animation_frame with
    | None -> ()
    | Some frame -> Animation.cancel_animation_frame frame);
    let frame =
      Animation.request_animation_frame (fun _ ->
          _animation_frame <- None;
          (match _animation_timer with
           | None -> ()
           | Some (t, timer) ->
              Lwt.cancel t;
              clear_timeout timer);
          _animation_timer <- Some (t, (set_timeout f 0.))) in
    _animation_frame <- Some frame

end

type 'a action =
  | Label of string
  | Widget of (#Widget.t as 'a)

type 'a dismiss =
  | True
  | Widget of (#Widget.t as 'a)

let make ?leading ?stacked
      ?(action : 'a action option)
      ?(dismiss : 'a dismiss option)
      ~(label : string)
      () : t =
  let label = Markup.create_label label () in
  let action = match action with
    | None -> None
    | Some Label s -> Some (Markup.create_action s ())
    | Some Widget w -> Some (Widget.to_markup w) in
  let dismiss = match dismiss with
    | None -> None
    | Some True ->
       let d = Components_tyxml.Svg_icons.close in
       let path = Icon.SVG.Markup.(create_path d ()) in
       let icon = Icon.SVG.Markup.create [path] () in
       Some (Icon_button.Markup.create
               ~classes:[CSS.dismiss]
               ~ripple:false
               ~icon
               ())
    | Some Widget w -> Some (Widget.to_markup w) in
  let actions = match action, dismiss with
    | None, None -> None
    | _ -> Some (Markup.create_actions ?dismiss ?action ()) in
  let surface = Markup.create_surface ?actions ~label () in
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?leading ?stacked ~surface () in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
