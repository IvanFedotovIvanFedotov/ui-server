open Js_of_ocaml
open Utils

include Components_tyxml.Tab
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

type dimensions =
  { root_left : int
  ; root_right : int
  ; content_left : int
  ; content_right : int
  }

module Event = struct
  class type interacted =
    object
      inherit [Element.t] Widget.custom_event
    end

  let interacted : interacted Js.t Events.Typ.t =
    Events.Typ.make "tab:interacted"
end

class t (elt : Dom_html.buttonElement Js.t) () =
object(_ : 'self)
  val ripple_elt : Dom_html.element Js.t option =
    Element.query_selector elt CSS.ripple
  val content_elt : Dom_html.element Js.t =
    find_element_by_class_exn elt CSS.content
  val indicator : Tab_indicator.t =
    Tab_indicator.attach
    @@ find_element_by_class_exn elt Tab_indicator.CSS.root
  inherit Widget.t elt () as super

  val mutable _ripple : Ripple.t option = None
  val mutable _click_listener = None

  method! init () : unit =
    super#init ();

  method! initial_sync_with_dom () : unit =
    super#initial_sync_with_dom ();
    (* Attach event handlers *)
    let click_listener =
      Events.clicks super#root (fun _ _ ->
          super#emit ~should_bubble:true ~detail:super#root Event.interacted;
          Lwt.return_unit) in
    _click_listener <- Some click_listener

  method! layout () : unit =
    super#layout ();
    Option.iter Ripple.layout _ripple

  method! destroy () : unit =
    super#destroy ();
    (* Destroy internal components *)
    Option.iter Ripple.destroy _ripple;
    _ripple <- None;
    (* Detach event listeners *)
    Option.iter Lwt.cancel _click_listener;
    _click_listener <- None

  method indicator : Tab_indicator.t =
    indicator

  method disabled : bool =
    Js.to_bool elt##.disabled

  method set_disabled (x : bool) : unit =
    elt##.disabled := Js.bool x

  method active : bool =
    super#has_class CSS.active

  method set_active ?(previous : 'self option) (x : bool) : unit =
    super#toggle_class ~force:x CSS.active;
    super#set_attribute "aria-selected" @@ string_of_bool x;
    super#set_attribute "tabindex" (if x then "0" else "-1");
    let previous = Option.map (fun x -> x#indicator) previous in
    indicator#set_active ?previous x;
    if x then super#root##focus

  method compute_dimensions () : dimensions =
    let root_width = super#root##.offsetWidth in
    let root_left = super#root##.offsetWidth in
    let content_width = content_elt##.offsetWidth in
    let content_left = content_elt##.offsetLeft in
    { root_left
    ; root_right = root_left + root_width
    ; content_left = root_left + content_left
    ; content_right = root_left + content_left + content_width
    }

  method index : int =
    let rec aux i node =
      match Js.Opt.to_option node##.previousSibling with
      | None -> i
      | Some x -> aux (succ i) x in
    aux 0 super#node

  method width : int =
    super#root##.offsetWidth

  method left : int =
    super#root##.offsetLeft

  method ripple : Ripple.t option =
    _ripple

  (* Private methods *)

  method private create_ripple (surface : Dom_html.element Js.t) : Ripple.t =
    let adapter = Ripple.make_default_adapter super#root in
    let add_class = Element.add_class surface in
    let remove_class = Element.remove_class surface in
    let update_css_variable = Ripple.update_css_variable surface in
    let is_surface_disabled = fun () -> Js.to_bool elt##.disabled in
    let adapter =
      { adapter with add_class
                   ; remove_class
                   ; update_css_variable
                   ; is_surface_disabled } in
    new Ripple.t adapter ()
end

let make ?min_width ?disabled ?active ?stacked
      ?(icon : #Widget.t option)
      ?(label : string option)
      ?(indicator_span_content = false)
      ~(indicator : Tab_indicator.t)
      () : t =
  let text_label = match label with
    | None -> None
    | Some l -> Some (Markup.create_text_label l ()) in
  let content =
    Markup.create_content
      ?indicator:(if not indicator_span_content then None else
                    Some (Widget.to_markup indicator))
      ?icon:(Option.map Widget.to_markup icon)
      ?text_label
      () in
  let (elt : Dom_html.buttonElement Js.t) =
    Tyxml_js.To_dom.of_button
    @@ Markup.create
         ?min_width ?disabled ?active ?stacked
         ?indicator:(if indicator_span_content then None else
                       Some (Widget.to_markup indicator))
         content
         () in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  match Js.to_string elt##.tagName with
  | "BUTTON" -> new t (Js.Unsafe.coerce elt) ()
  | _ -> failwith "tab: host element must have a `button` tag"
