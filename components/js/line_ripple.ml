open Js_of_ocaml
open Utils

include Components_tyxml.Line_ripple
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

class t (elt : Dom_html.element Js.t) () =
object(self)

  val mutable _transitionend_listener = None

  inherit Widget.t elt () as super

  (** Activates the line ripple *)
  method activate () : unit =
    self#remove_class CSS.deactivating;
    self#add_class CSS.active

  (** Deactivates the line ripple *)
  method deactivate () : unit =
    self#add_class CSS.deactivating

  (** Sets the center of the ripple animation to the given X coordinate. *)
  method set_ripple_center (x_coordinate : int) : unit =
    let value = Js.string @@ Printf.sprintf "%dpx center" x_coordinate in
    (Js.Unsafe.coerce self#style)##.transformOrigin := value

  method! init () : unit =
    super#init ();
    self#listen_lwt (Events.Typ.make "transitionend") (fun e _ ->
        Lwt.return @@ self#handle_transition_end e)
    |> fun x -> _transitionend_listener <- Some x

  method! destroy () : unit =
    super#destroy ();
    Option.iter Lwt.cancel _transitionend_listener;
    _transitionend_listener <- None

  (* Private methods *)

  (** Handles a transition end event *)
  method private handle_transition_end (e : Dom_html.event Js.t) : unit =
    let prop = Js.to_string (Js.Unsafe.coerce e)##.propertyName in
    if String.equal prop "opacity"
    then (
      (* Wait for the line ripple to be either transparent or opaque
         before emitting the animation end event *)
      let is_deactivating = self#has_class CSS.deactivating in
      if is_deactivating then
        (self#remove_class CSS.active;
         self#remove_class CSS.deactivating))
end

let make () : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create () in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
