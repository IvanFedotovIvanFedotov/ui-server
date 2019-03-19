open Js_of_ocaml
open Utils

include Components_tyxml.Radio
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

class t ?on_change (elt : Dom_html.element Js.t) () =
object(self)
  val input_elt : Dom_html.inputElement Js.t =
    find_element_by_class_exn elt CSS.native_control
  val mutable _ripple : Ripple.t option = None
  val mutable _change_listener = None

  inherit Widget.t elt () as super

  method! init () : unit =
    super#init ();
    _ripple <- Some (self#create_ripple ())

  method! initial_sync_with_dom () : unit =
    super#initial_sync_with_dom ();
    let change_listener =
      if Option.is_none on_change then None else
        Some (Events.listen_lwt input_elt Events.Typ.change (fun _ _ ->
                  self#notify_change ();
                  Lwt.return_unit)) in
    _change_listener <- change_listener

  method! layout () : unit =
    super#layout ();
    Option.iter (fun r -> r#layout ()) _ripple

  method! destroy () : unit =
    super#destroy ();
    (* Destroy internal components *)
    Option.iter (fun r -> r#destroy ()) _ripple;
    _ripple <- None

  method value : string =
    Js.to_string input_elt##.value

  method set_value (s : string) : unit =
    input_elt##.value := Js.string s

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

  (* Private methods *)

  method private notify_change () : unit =
    Option.iter (fun f -> f self#checked) on_change

  method private create_ripple () : Ripple.t =
    let adapter = Ripple.make_default_adapter super#root in
    let is_unbounded = fun () -> true in
    let is_surface_active = fun () -> false in
    let register_handler = fun typ f ->
      Events.listen input_elt typ (fun _ e -> f (e :> Dom_html.event Js.t); true) in
    let adapter =
      { adapter with is_unbounded
                   ; is_surface_active
                   ; register_handler } in
    new Ripple.t adapter ()

end

let make ?input_id ?name ?checked ?disabled ?on_change () : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?input_id ?name ?checked ?disabled () in
  new t ?on_change elt ()

let attach ?on_change (elt : #Dom_html.element Js.t) : t =
  new t ?on_change (Element.coerce elt) ()
