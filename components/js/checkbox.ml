open Js_of_ocaml
open Utils

(* TODO
   - add 'on_change' callback arg *)

include Components_tyxml.Checkbox
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

class t ?(ripple = true) (elt : Dom_html.element Js.t) () =
object
  val input_elt : Dom_html.inputElement Js.t =
    elt##querySelector (Js.string ("." ^ CSS.native_control))
    |> (fun x -> Js.Opt.get x (fun () -> failwith "No <input> element found"))
    |> Js.Unsafe.coerce
  val mutable _ripple : Ripple.t option = None
  val mutable _change_listener = None

  inherit Widget.t elt ()as super

  method! init () : unit =
    super#init ();
    if ripple then
      let adapter = Ripple.make_default_adapter super#root in
      let is_unbounded = fun () -> true in
      let is_surface_active = fun () ->
        Ripple.Util.get_matches_property input_elt ":active" in
      let register_handler =
        fun (typ : #Dom_html.event Js.t Events.Typ.t) f ->
        Dom_events.listen input_elt typ (fun _ (e : #Dom_html.event Js.t) ->
            f (e :> Dom_html.event Js.t); true) in
      let adapter =
        { adapter with is_unbounded
                     ; is_surface_active
                     ; register_handler } in
      let ripple = new Ripple.t adapter () in
      _ripple <- Some ripple

  method! layout () : unit =
    super#layout ();
    Option.iter Ripple.layout _ripple

  method! destroy () : unit =
    super#destroy ();
    Option.iter Ripple.destroy _ripple;
    _ripple <- None

  method value : string =
    Js.to_string input_elt##.value

  method set_value (s : string) : unit =
    input_elt##.value := Js.string s

  method set_indeterminate (x : bool) : unit =
    (Js.Unsafe.coerce input_elt)##.indeterminate := Js.bool x

  method indeterminate : bool =
    Js.to_bool (Js.Unsafe.coerce input_elt)##.indeterminate

  method checked : bool =
    Js.to_bool input_elt##.checked

  method set_checked (x : bool) : unit =
    input_elt##.checked := Js.bool x

  method disabled : bool =
    Js.to_bool input_elt##.disabled

  method set_disabled (x : bool) : unit =
    input_elt##.disabled := Js.bool x;
    super#toggle_class ~force:x CSS.disabled
end

let make ?input_id ?ripple ?checked ?disabled () =
  let elt =
    Tyxml_js.To_dom.of_div
    @@ Markup.create ?input_id ?checked ?disabled () in
  new t ?ripple elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t (elt :> Dom_html.element Js.t) ()
