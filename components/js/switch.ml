open Js_of_ocaml

include Components_tyxml.Switch
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

class t (elt : #Dom_html.element Js.t) () =
object(self)
  val input_elt : Dom_html.inputElement Js.t =
    elt##querySelector (Js.string ("." ^ CSS.native_control))
    |> (fun x -> Js.Opt.get x (fun () -> failwith "No <input> element found"))
    |> Js.Unsafe.coerce
  val mutable _ripple : Ripple.t option = None
  val mutable _change_listener = None

  inherit Widget.t elt () as super

  method! init () : unit =
    super#init ();
    _ripple <- Some (self#create_ripple ())

  method! initial_sync_with_dom () : unit =
    super#initial_sync_with_dom ();
    let change_listener =
      Events.listen_lwt input_elt Events.Typ.change
        (fun (e : Dom_html.event Js.t) _ ->
          Js.Opt.iter e##.target (fun elt ->
              let (elt : Dom_html.inputElement Js.t) = Js.Unsafe.coerce elt in
              super#toggle_class ~force:(Js.to_bool elt##.checked)
                CSS.checked);
          Lwt.return_unit) in
    _change_listener <- Some change_listener;
    input_elt##.checked := input_elt##.checked

  method! destroy () : unit =
    super#destroy ();
    Utils.Option.iter Ripple.destroy _ripple;
    _ripple <- None;
    Utils.Option.iter Lwt.cancel _change_listener;
    _change_listener <- None

  method checked : bool =
    Js.to_bool input_elt##.checked

  method set_checked (x : bool) : unit =
    input_elt##.checked := Js.bool x;
    super#toggle_class ~force:x CSS.checked

  method disabled : bool =
    Js.to_bool input_elt##.disabled

  method set_disabled (x : bool) : unit =
    input_elt##.disabled := Js.bool x;
    super#toggle_class ~force:x CSS.disabled

  (* Private methods *)

  method private create_ripple () : Ripple.t =
    let selector = "." ^ CSS.thumb_underlay in
    let (surface : Dom_html.element Js.t) =
      Js.Opt.get (elt##querySelector (Js.string selector))
        (fun () -> failwith "no ripple surface element found")in
    Ripple.attach ~unbounded:true surface
end

let make ?input_id ?checked ?disabled () : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?input_id ?checked ?disabled () in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t elt ()
