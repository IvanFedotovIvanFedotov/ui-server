open Js_of_ocaml
open Utils

include Components_tyxml.Checkbox
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

type transition_state =
  | Init
  | Checked
  | Unchecked
  | Indeterminate

let equal_transition_state (a : transition_state as 'a) (b : 'a) : bool =
  match a, b with
  | Init, Init -> true
  | Checked, Checked -> true
  | Unchecked, Unchecked -> true
  | Indeterminate, Indeterminate -> true
  | _, _ -> false

module Const = struct
  let anim_end_latch_ms = 250.
end

module Attr = struct
  let aria_checked = "aria-checked"
end

class t ?on_change (elt : Dom_html.element Js.t) () =
object(self)
  val input_elt : Dom_html.inputElement Js.t =
    find_element_by_class_exn elt CSS.native_control
  val mutable _ripple : Ripple.t option = None
  val mutable _change_listener = None
  val mutable _animationend_listener = None
  val mutable _cur_check_state : transition_state = Init
  val mutable _cur_animation_class : string option = None
  val mutable _enable_animationend_handler = false
  val mutable _anim_end_latch_timer = None

  inherit Widget.t elt ()as super

  method! init () : unit =
    super#init ();
    _ripple <- Some (self#create_ripple ())

  method! initial_sync_with_dom () : unit =
    super#initial_sync_with_dom ();
    let change_listener =
      Events.listen_lwt input_elt Events.Typ.change (fun _ _ ->
          self#transition_check_state ();
          self#notify_change ();
          Lwt.return_unit) in
    _change_listener <- Some change_listener;
    let animationend_listener =
      Events.listen_lwt super#root Events.Typ.animationend (fun _ _ ->
          self#handle_animation_end ();
          Lwt.return_unit) in
    _animationend_listener <- Some animationend_listener

  method! layout () : unit =
    super#layout ();
    Option.iter Ripple.layout _ripple

  method! destroy () : unit =
    super#destroy ();
    (* Detach event listeners *)
    Option.iter Lwt.cancel _change_listener;
    _change_listener <- None;
    Option.iter Lwt.cancel _animationend_listener;
    _animationend_listener <- None;
    (* Destroy internal components *)
    Option.iter Ripple.destroy _ripple;
    _ripple <- None;
    (* Clear internal timers *)
    Option.iter clear_timeout _anim_end_latch_timer;
    _anim_end_latch_timer <- None

  method value : string =
    Js.to_string input_elt##.value

  method set_value (s : string) : unit =
    input_elt##.value := Js.string s

  method set_indeterminate (x : bool) : unit =
    (Js.Unsafe.coerce input_elt)##.indeterminate := Js.bool x

  method indeterminate : bool =
    Js.to_bool (Js.Unsafe.coerce input_elt)##.indeterminate

  method disabled : bool =
    Js.to_bool input_elt##.disabled

  method set_disabled (x : bool) : unit =
    input_elt##.disabled := Js.bool x;
    super#toggle_class ~force:x CSS.disabled

  method checked : bool =
    Js.to_bool input_elt##.checked

  method toggle ?(notify = false) ?(force : bool option) () : unit =
    let v = match force with None -> not self#checked | Some x -> x in
    input_elt##.checked := Js.bool v;
    if notify then self#notify_change ()

  method input_element : Dom_html.inputElement Js.t =
    input_elt

  method ripple : Ripple.t option =
    _ripple

  (* Private methods *)

  method private notify_change () : unit =
    Option.iter (fun f -> f self#checked) on_change

  method private create_ripple () : Ripple.t =
    let adapter = Ripple.make_default_adapter super#root in
    let is_unbounded = fun () -> true in
    let is_surface_active = fun () ->
      Ripple.Util.get_matches_property input_elt ":active" in
    let register_handler = fun typ f ->
      Events.listen input_elt typ (fun _ e -> f (e :> Dom_html.event Js.t); true) in
    let adapter =
      { adapter with is_unbounded
                   ; is_surface_active
                   ; register_handler } in
    new Ripple.t adapter ()

  method private force_layout () : unit =
    ignore super#root##.offsetWidth

  (** Handles the `animationend` event for the checkbox *)
  method private handle_animation_end () : unit =
    if _enable_animationend_handler
    then (
      Option.iter clear_timeout _anim_end_latch_timer;
      let timer =
        set_timeout (fun () ->
            Option.iter super#remove_class _cur_animation_class;
            _enable_animationend_handler <- false)
          Const.anim_end_latch_ms in
      _anim_end_latch_timer <- Some timer)

  method private transition_check_state () : unit =
    let prev = Init in
    let cur = self#determine_check_state () in
    if not (equal_transition_state prev cur)
    then (
      self#update_aria_checked ();
      (* Check to ensure that there isn't a previously existing animation class,
         in case for example the user interacted with the checkbox before
         then animation has finished *)
      (match _cur_animation_class with
       | None -> ()
       | Some c ->
          Option.iter clear_timeout _anim_end_latch_timer;
          _anim_end_latch_timer <- None;
          self#force_layout ();
          super#remove_class c);
      _cur_animation_class <- self#get_transition_animation_class ~prev cur;
      _cur_check_state <- cur;
      (* Check for parentNode so that animations are only run when
         then element is attached to the DOM *)
      match Js.Opt.to_option self#root##.parentNode, _cur_animation_class with
      | None, _ | _, None -> ()
      | Some _, Some c ->
         super#add_class c;
         _enable_animationend_handler <- true)

  method private determine_check_state () : transition_state =
    if self#indeterminate
    then Indeterminate
    else if self#checked then Checked else Unchecked

  method private get_transition_animation_class ~(prev : transition_state)
                   (cur : transition_state) : string option =
    match prev, cur with
    | Init, Unchecked -> None
    | Init, Checked -> Some CSS.anim_indeterminate_checked
    | Init, _ -> Some CSS.anim_indeterminate_unchecked
    | Unchecked, Checked -> Some CSS.anim_unchecked_checked
    | Unchecked, _ -> Some CSS.anim_unchecked_indeterminate
    | Checked, Unchecked -> Some CSS.anim_checked_unchecked
    | Checked, _ -> Some CSS.anim_checked_indeterminate
    | Indeterminate, Checked -> Some CSS.anim_indeterminate_checked
    | Indeterminate, _ -> Some CSS.anim_indeterminate_unchecked

  method private update_aria_checked () : unit =
    (* Ensure aria-checked is set to mixed if checkbox is in indeterminate state *)
    if self#indeterminate
    then Element.set_attribute input_elt Attr.aria_checked "mixed"
    else
      (* The on/off state does not need to keep track of aria-checked, since
         the screenreader uses the checked property on the checkbox element *)
      Element.remove_attribute input_elt Attr.aria_checked
end

let make ?input_id ?checked ?disabled ?on_change () =
  let elt =
    Tyxml_js.To_dom.of_div
    @@ Markup.create ?input_id ?checked ?disabled () in
  new t ?on_change elt ()

let attach ?on_change (elt : #Dom_html.element Js.t) : t =
  new t ?on_change (elt :> Dom_html.element Js.t) ()
