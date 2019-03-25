open Js_of_ocaml
open Utils

include Components_tyxml.Textfield
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

module Event = struct
  class type icon =
    object
      inherit [unit] Widget.custom_event
    end

  let icon : icon Js.t Events.Typ.t =
    Events.Typ.make "textfield:icon"
end

module Icon = struct
  module Attr = struct
    let icon_role = "button"
    let aria_label = "aria-label"
  end

  class t (elt : Dom_html.element Js.t) () =
  object(self)
    val mutable _saved_tab_index = None
    val mutable _click_listener = None
    val mutable _keydown_listener = None
    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      _saved_tab_index <- Element.get_attribute super#root "tabindex";
      (* Attach event listeners *)
      let click =
        Events.clicks super#root (fun e _ -> self#handle_click e;) in
      let keydown =
        Events.keydowns super#root (fun e _ -> self#handle_keydown e) in
      _click_listener <- Some click;
      _keydown_listener <- Some keydown

    method! destroy () : unit =
      super#destroy ();
      (* Detach event listeners *)
      Option.iter Lwt.cancel _click_listener;
      Option.iter Lwt.cancel _keydown_listener;
      _click_listener <- None;
      _keydown_listener <- None

    method set_aria_label (label : string) : unit =
      Element.set_attribute super#root Attr.aria_label label

    method set_disabled (x : bool) : unit =
      match _saved_tab_index with
      | None -> ()
      | Some tabindex ->
         if x then (
           Element.set_attribute super#root "tabindex" "-1";
           Element.remove_attribute super#root "role")
         else (
           Element.set_attribute super#root "tabindex" tabindex;
           Element.set_attribute super#root "role" Attr.icon_role)

    (** Private methods *)

    method private notify_action () : unit =
      super#emit ~should_bubble:true Event.icon

    method private handle_keydown (e : Dom_html.keyboardEvent Js.t) : unit Lwt.t =
      (match Events.Key.of_event e with
       | `Enter -> self#notify_action ()
       | _ -> ());
      Lwt.return_unit

    method private handle_click (_ : Dom_html.mouseEvent Js.t) : unit Lwt.t =
      self#notify_action ();
      Lwt.return_unit
  end

  let attach (elt : #Dom_html.element Js.t) : t =
    new t (Element.coerce elt) ()

end

module Helper_text = struct

  module Attr = struct
    let aria_hidden = "aria-hidden"
  end

  class t (elt : Dom_html.element Js.t) () =
  object(self)
    inherit Widget.t elt () as super

    method set_content (s : string) : unit =
      super#root##.textContent := Js.some @@ Js.string s

    method persistent : bool =
      super#has_class CSS.Helper_text.persistent

    method set_persistent (x : bool) : unit =
      super#toggle_class ~force:x CSS.Helper_text.persistent

    method validation : bool =
      super#has_class CSS.Helper_text.validation_msg

    method set_validation (x : bool) : unit =
      super#toggle_class ~force:x CSS.Helper_text.validation_msg

    method show_to_screen_reader () : unit =
      super#remove_attribute Attr.aria_hidden

    method set_validity (is_valid : bool) : unit =
      let needs_display = self#validation && not is_valid in
      if needs_display
      then super#set_attribute "role" "alert"
      else super#remove_attribute "role";
      if not self#persistent && not needs_display
      then self#hide ()

    (* Private methods *)

    method private hide () : unit =
      super#set_attribute Attr.aria_hidden "true"
  end

  let make ?persistent ?validation text : t =
    let (elt : Dom_html.element Js.t) =
      Tyxml_js.To_dom.of_div
      @@ Markup.Helper_text.create ?persistent ?validation ~text () in
    new t elt ()

  let attach (elt : #Dom_html.element Js.t) : t =
    new t (Element.coerce elt) ()

end

module Id = struct
  let id_ref = ref (Unix.time () |> int_of_float)
  let get = fun () ->
    incr id_ref;
    Printf.sprintf "text-field-%d" !id_ref
end

module Const = struct

  let label_scale = 0.75

  let always_float_types =
    [ "color"
    ; "date"
    ; "datetime-local"
    ; "month"
    ; "range"
    ; "time"
    ; "week"
    ]

  let validation_attr_whitelist =
    [ "pattern"
    ; "min"
    ; "max"
    ; "required"
    ; "step"
    ; "minlength"
    ; "maxlength"
    ]

end

class type validity_state =
  object
    method badInput : bool Js.t Js.readonly_prop
    method customError : bool Js.t Js.readonly_prop
    method patternMismatch : bool Js.t Js.readonly_prop
    method rangeOverflow : bool Js.t Js.readonly_prop
    method rangeUnderflow : bool Js.t Js.readonly_prop
    method stepMismatch : bool Js.t Js.readonly_prop
    method tooLong : bool Js.t Js.readonly_prop
    method tooShort : bool Js.t Js.readonly_prop
    method typeMismatch : bool Js.t Js.readonly_prop
    method valid : bool Js.t Js.readonly_prop
    method valueMissing : bool Js.t Js.readonly_prop
  end

type 'a validation =
  | Email : string validation
  | Integer : (int option * int option) -> int validation
  | Float : (float option * float option) -> float validation
  | Text : string validation
  | Password : (string -> (unit, string) result) -> string validation
  | Custom : ((string -> ('a, string) result) * ('a -> string)) -> 'a validation

let input_type_of_validation : type a. a validation -> Html_types.input_type =
  function
  | Text -> `Text
  | Email -> `Email
  | Float _ -> `Number
  | Integer _ -> `Number
  | Password _ -> `Password
  | Custom _ -> `Text

let parse_valid (type a) (v : a validation)
      (on_fail : string -> unit)
      (s : string) : a option =
  match v with
  | Text -> Some s
  | Email -> Some s
  | Integer (None, None) -> int_of_string_opt s
  | Integer (Some min, None) ->
     (match int_of_string_opt s with
      | None -> None
      | Some (v : int) -> if v >= min then Some v else None)
  | Integer (None, Some max) ->
     (match int_of_string_opt s with
      | None -> None
      | Some (v : int) -> if v <= max then Some v else None)
  | Integer (Some min, Some max) ->
     (match int_of_string_opt s with
      | None -> None
      | Some (v : int) -> if v <= max && v >= min then Some v else None)
  | Float (None, None) -> float_of_string_opt s
  | Float (Some min, None) ->
     (match float_of_string_opt s with
      | None -> None
      | Some (v : float) -> if v >=. min then Some v else None)
  | Float (None, Some max) ->
     (match float_of_string_opt s with
      | None -> None
      | Some (v : float) -> if v <=. max then Some v else None)
  | Float (Some min, Some max) ->
     (match float_of_string_opt s with
      | None -> None
      | Some (v : float) -> if v <=. max && v >=. min then Some v else None)
  | Password vf  ->
     (match vf s with
      | Ok () -> Some s
      | Error e -> on_fail e; None)
  | Custom (f,_) ->
     (match f s with
      | Ok v -> Some v
      | Error s -> on_fail s; None)

let valid_to_string (type a) (v : a validation) (e : a) : string =
  match v with
  | Custom (_, conv) -> conv e
  | Float _ ->
     let s = string_of_float e in
     if String.suffix ~suf:"." s then s ^ "0" else s
  | Integer _ -> string_of_int e
  | Email -> e
  | Password _ -> e
  | Text -> e

class ['a] t (elt : Dom_html.element Js.t) () =
object(self)
  val mutable _ripple : Ripple.t option = None
  val mutable _use_native_validation = true
  val mutable _received_user_input = false
  val mutable _is_valid = true
  val mutable _is_focused = false
  val mutable _listeners = []
  val mutable _validation_observer = None

  inherit Widget.t elt () as super

  method set_leading_icon_aria_label (s : string) =
    Option.iter (fun (x : Icon.t) -> Icon.set_aria_label x s)
      leading_icon

  method set_trailing_icon_aria_label (s : string) =
    Option.iter (fun (x : Icon.t) -> Icon.set_aria_label x s)
      trailing_icon

  method set_helper_text_content (s : string) =
    Option.iter (fun (x : Helper_text.t) ->
        x#set_content s) helper_text

  method set_required (x : bool) : unit =
    input_elt##.required := Js.bool x

  method set_disabled (x : bool) : unit =
    self#style_disabled x

  method set_use_native_validation (x : bool) : unit =
    _use_native_validation <- x

  method valid : bool =
    if _use_native_validation
    then self#is_native_input_valid ()
    else _is_valid

  method set_valid (x : bool) : unit =
    _is_valid <- x;
    self#style_validity x;
    let shold_shake = not x && not _is_focused in
    Option.iter (fun x -> x#shake shold_shake) floating_label

  method empty : bool =
    String.is_empty self#raw_value

  method raw_value : string =
    Js.to_string input_elt##.value

  method value : 'a option =
    React.S.value s_input

  method clear () : unit =
    self#_set_value ""

  method set_value (v : 'a) =
    super#_set_value (valid_to_string input_type v);
    self#style_validity self#valid;
    Option.iter (fun l ->
        self#notch_outline self#should_float;
        l#float self#should_float;
        l#shake self#should_shake) floating_label

  method validation_message : string =
    Js.to_string (Js.Unsafe.coerce input_elt)##.validationMessage

  method update () : unit =
    self#deactivate_focus ()

  method focus () : unit =
    self#activate_focus ();
    input_elt##focus

  (* Private methods *)

  method private focused () : bool =
    let active = Dom_html.document##.activeElement in
    match Js.Opt.to_option active with
    | None -> false
    | Some elt -> Element.equal input_widget#root elt

  method private handle_text_field_interaction () : unit =
    if not (Js.to_bool input_elt##.disabled)
    then _received_user_input <- true

  method private handle_validation_attribute_change
                   (attrs : string list) : unit =
    List.fold_while (fun _ attr ->
        if List.mem ~eq:String.equal attr Const.validation_attr_whitelist
        then self#style_validity true, `Stop
        else (), `Continue) () attrs

  method private notch_outline (open_notch : bool) : unit =
    match notched_outline with
    | None -> ()
    | Some outline ->
       if open_notch
       then
         let label_scale =
           if self#dense then dense_label_scale
           else label_scale in
         let label_width =
           Option.map_or ~default:0 (fun l -> l#width) floating_label in
         outline#notch (float_of_int label_width *. label_scale)
       else outline#close_notch ()

  method private activate_focus () : unit =
    _is_focused <- true;
    self#style_focused _is_focused;
    Option.iter (fun r -> r#activate ()) line_ripple;
    Option.iter (fun l ->
        self#notch_outline self#should_float;
        l#float self#should_float;
        l#shake self#should_shake) floating_label;
    Option.iter (fun x -> x#show_to_screen_reader ()) helper_text

  method private auto_complete_focus () : unit =
    if not _received_user_input
    then self#activate_focus ()

  method private deactivate_focus () : unit =
    (match parse_valid input_type self#set_custom_validity self#_value with
     | Some v ->
        set_s_input (Some v);
        self#remove_custom_validity ()
     | None -> set_s_input None);
    _is_focused <- false;
    Option.iter (fun r -> r#deactivate ()) line_ripple;
    self#style_validity self#valid;
    self#style_focused _is_focused;
    Option.iter (fun l ->
        self#notch_outline self#should_float;
        l#float self#should_float;
        l#shake self#should_shake) floating_label;
    if not self#should_float
    then _received_user_input <- false

  (* Sets the line ripple's transform origin,
   * so that the line ripple activate
   * animation will animate out from the user's click location.*)
  method private set_transform_origin (event : Dom_html.mouseEvent Js.t) : unit =
    let target = event##.target in
    let left = match Js.Opt.to_option target with
      | None -> 0
      | Some x -> int_of_float x##getBoundingClientRect##.left in
    let (x : int) = event##.clientX in
    let normalized_x = x - left in
    Option.iter (fun r -> r#set_ripple_center normalized_x) line_ripple

  method private is_bad_input () : bool =
    let (validity : validity_state Js.t) =
      (Js.Unsafe.coerce input_elt)##.validity in
    Js.to_bool validity##.badInput

  method private is_native_input_valid () : bool =
    let (validity : validity_state Js.t) =
      (Js.Unsafe.coerce input_elt)##.validity in
    Js.to_bool validity##.valid

  method private style_validity (is_valid : bool) : unit =
    super#toggle_class ~force:(not is_valid) Markup.invalid_class;
    Option.iter (fun x ->
        x#set_validity is_valid;
        if x#auto_validation_message
        then x#set_content self#validation_message) helper_text

  method private style_focused (is_focused : bool) : unit =
    super#toggle_class ~force:is_focused Markup.focused_class

  method private style_disabled (is_disabled : bool) : unit =
    if is_disabled
    then (super#add_class Markup.disabled_class;
          super#remove_class Markup.invalid_class)
    else super#remove_class Markup.disabled_class;
    Option.iter (fun x -> Icon.set_disabled x is_disabled) leading_icon;
    Option.iter (fun x -> Icon.set_disabled x is_disabled) trailing_icon;

  method private should_always_float : bool =
    let typ = Js.to_string input_elt##._type in
    List.mem ~eq:String.equal typ always_float_types

  method private should_float : bool =
    self#should_always_float
    || _is_focused
    || not self#empty
    || self#is_bad_input ()

  method private should_shake : bool =
    not self#valid && not _is_focused && not self#empty

  method private register_validation_handler handler =
    MutationObserver.observe
      ~node:input_elt
      ~attributes:true
      ~f:(fun arr _ ->
        let a = Js.to_array arr in
        let a =
          Array.filter_map (fun x ->
              let opt = Js.Opt.to_option x##.attributeName in
              Option.map Js.to_string opt) a in
        let l = Array.to_list a in
        handler l)
      ()

  method private set_max (x : float) : unit =
    (Js.Unsafe.coerce input_elt)##.max := x

  method private set_min (x : float) : unit =
    (Js.Unsafe.coerce input_elt)##.min := x

  method private set_max_length (x : int) : unit =
    input_elt##.maxLength := x

  method private set_min_length (x : int) : unit =
    (Js.Unsafe.coerce input_elt)##.minLength := x

  method private apply_border (type a) (v : a validation) : unit =
    match v with
    | Float (min, max) ->
       Option.iter self#set_min min;
       Option.iter self#set_max max
    | Integer (min, max) ->
       Option.iter (fun min -> self#set_min @@ float_of_int min) min;
       Option.iter (fun max -> self#set_max @@ float_of_int max) max
    | _ -> ()

  method private set_custom_validity (s : string) : unit =
    (Js.Unsafe.coerce input_elt)##setCustomValidity (Js.string s)

  method private remove_custom_validity () : unit =
    self#set_custom_validity ""

  method! init () : unit =
    super#init ();
    Option.iter self#set_required required;
    Option.iter (fun (x : Icon.t) ->
        x.widget#add_class Markup.Icon._class) leading_icon;
    Option.iter (fun (x : Icon.t) ->
        x.widget#add_class Markup.Icon._class) trailing_icon;
    (* Validitation *)
    self#apply_border input_type;
    (* Other initialization *)
    if self#focused ()
    then self#activate_focus ()
    else if Option.is_some floating_label && self#should_float
    then (self#notch_outline true;
          Option.iter (fun x -> x#float true) floating_label);
    let focus =
      input_widget#listen_lwt Events.Typ.focus (fun _ _ ->
          Lwt.return @@ self#activate_focus ()) in
    let blur =
      input_widget#listen_lwt Events.Typ.blur (fun _ _ ->
          Lwt.return @@ self#deactivate_focus ()) in
    let input =
      input_widget#listen_lwt Events.Typ.input (fun _ _ ->
          Lwt.return @@ self#auto_complete_focus ()) in
    let ptr =
      List.map (fun x ->
          input_widget#listen_lwt (Events.Typ.make x) (fun e _ ->
              Lwt.return @@ self#set_transform_origin e))
        ["mousedown"; "touchstart"] in
    let click_keydown =
      List.map (fun x ->
          super#listen_lwt (Events.Typ.make x) (fun _ _ ->
              Lwt.return @@ self#handle_text_field_interaction ()))
        ["click"; "keydown"] in
    let observer =
      let handler = self#handle_validation_attribute_change in
      self#register_validation_handler handler in
    _listeners <- _listeners @ [focus; blur; input] @ ptr @ click_keydown;
    _validation_observer <- Some observer;
    if not (super#has_class Markup.textarea_class)
       && not (super#has_class Markup.outlined_class)
    then
      let adapter = Ripple.make_default_adapter self#root in
      let is_surface_disabled = fun () -> self#disabled in
      let is_surface_active = fun () ->
        Ripple.Util.get_matches_property input_widget#root ":active" in
      let register_handler = fun typ f ->
        Dom_events.listen input_elt (Events.Typ.make typ) (fun _ e ->
            f e; true) in
      let adapter =
        { adapter with is_surface_active
                     ; register_handler
                     ; is_surface_disabled } in
      let ripple = new Ripple.t adapter () in
      _ripple <- Some ripple

  method! destroy () : unit =
    super#destroy ();
    List.iter Lwt.cancel _listeners;
    _listeners <- [];
    Option.iter (fun x -> x##disconnect) _validation_observer;
    _validation_observer <- None;
    Option.iter (fun x -> x#destroy ()) line_ripple;
    Option.iter (fun x -> x#destroy ()) floating_label;
    Option.iter (fun x -> x#destroy ()) notched_outline;

end

let make_textarea ?input_id
      ?label
      ?(native_validation = true)
      ?required
      ?(line_ripple = true)
      ?(outlined = false)
      ?(full_width = false)
      ?(textarea = false)
      ?rows ?cols
      ?(helper_text : Helper_text.t option)
      ?(leading_icon : #Widget.t option)
      ?(trailing_icon : #Widget.t option)
      ~(input_type : 'a validation)
      () =
  let input_id = match input_id with
    | None -> Id.get ()
    | Some s -> s in
  let outline = match outlined with
    | false -> None
    | true -> Some (new Notched_outline.t (),
                    (Notched_outline.Markup.create_idle ())) in
  let notched_outline = Option.map fst outline in
  (* Label should be floating when text field is not full-width *)
  let floating_label = match full_width, label with
    | _, None | true, _ -> None
    | false, Some x -> Some (new Floating_label.t ~for_:input_id x ()) in
  (* Label should be used as placeholder when text field is full-width *)
  let placeholder = match full_width, label with
    | _, None | false, _ -> None
    | true, Some x -> Some x in
  let line_ripple = match line_ripple with
    | false -> None
    | true -> Some (new Line_ripple.t ()) in
  let leading_icon = Option.map Icon.make leading_icon in
  let trailing_icon = Option.map Icon.make trailing_icon in
  let leading_event = match leading_icon with
    | None -> React.E.never
    | Some x -> x.event in
  let trailing_event = match trailing_icon with
    | None -> React.E.never
    | Some x -> x.event in
  let input_elt = match textarea with
    | false ->
       Markup.create_input
         ?placeholder
         ~input_id
         ~input_type:(input_type_of_validation input_type)
         ()
       |> To_dom.of_input
    | true ->
       Markup.create_textarea
         ?placeholder
         ?rows ?cols
         ~input_id
         ()
       |> To_dom.of_textarea
       |> Js.Unsafe.coerce in
  let input_widget =
    Widget.create input_elt in
  let elt =
    Markup.create
      ?label:(Option.map Widget.to_markup floating_label)
      ?outline:(Option.map (fun (x, y) -> Widget.to_markup x, y) outline)
      ?line_ripple:(Option.map Widget.to_markup line_ripple)
      ?leading_icon:(Option.map (fun (x : Icon.t) ->
                         Widget.to_markup x.widget) leading_icon)
      ?trailing_icon:(Option.map (fun (x : Icon.t) ->
                          Widget.to_markup x.widget) trailing_icon)
      ~textarea
      ~input:(Widget.to_markup input_widget) ()
    |> To_dom.of_element in
  new t elt ()

let wrap ~(textfield : 'a t)
      ~(helper_text : Helper_text.t) =
  let _class = Markup.container_class in
  let w =
    Widget.create_div
      ~widgets:[ textfield#widget
               ; helper_text#widget ]
      () in
  w#add_class _class;
  w
