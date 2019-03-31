open Js_of_ocaml
open Utils

class type ['a] custom_event =
  object
    inherit Events.event
    method detail : 'a Js.opt Js.readonly_prop
  end

class t ?(widgets : #t list option)
        (elt : #Dom_html.element Js.t)
        () = object(self)

  val mutable _on_destroy = None
  val mutable _widgets : t list = []

  method init () : unit =
    begin match widgets with
    | None -> ()
    | Some w -> _widgets <- List.map (fun (x : #t) -> x#widget) w
    end;
    List.iter self#append_child _widgets

  (** Initial synchronization with host DOM element,
      e.g. reading element's properties or attributes *)
  method initial_sync_with_dom () : unit =
    ()

  (** Destroys a widget and its children *)
  method destroy () : unit =
    List.iter (fun x -> x#destroy ()) _widgets;
    _widgets <- [];
    Option.iter (fun f -> f ()) _on_destroy

  (** Layout widget in DOM *)
  method layout () : unit =
    List.iter (fun x -> x#layout ()) _widgets

  (** Returns [true] if a widget is in DOM, [false] otherwise *)
  method in_dom : bool =
    Js.to_bool
    @@ (Js.Unsafe.coerce Dom_html.document##.body)##contains self#root

  method root : Dom_html.element Js.t =
    (elt :> Dom_html.element Js.t)

  method node : Dom.node Js.t =
    (elt :> Dom.node Js.t)

  method markup : Tyxml_js.Xml.elt =
    Tyxml_js.Of_dom.of_element self#root
    |> Tyxml_js.Html.toelt

  method widget : t = (self :> t)

  method widgets : t list =
    List.map (fun x -> x#widget) _widgets

  method append_child : 'a. (< node : Dom.node Js.t;
                             widget : t;
                             layout : unit -> unit;
                             .. > as 'a) -> unit =
    fun x ->
    Dom.appendChild self#root x#node;
    _widgets <- x#widget :: _widgets;
    if self#in_dom then self#layout ()

  method insert_child_at_idx : 'a. int -> (< root : Dom_html.element Js.t;
                                           widget : t;
                                           layout : unit -> unit;
                                           .. > as 'a) -> unit =
    fun index x ->
    Element.insert_child_at_index self#root index x#root;
    _widgets <- x#widget :: _widgets;
    if self#in_dom then self#layout ()

  method remove_child : 'a. (< node : Dom.node Js.t;
                             widget : t;
                             .. > as 'a) -> unit =
    fun x ->
    try
      Dom.removeChild self#root x#node;
      let equal (a : < root : Dom_html.element Js.t; ..> as 'a) (b : 'a) : bool =
        a#root == b#root in
      let wdgs = List.remove ~eq:equal x#widget _widgets in
      _widgets <- wdgs;
      if self#in_dom then self#layout ()
    with _ -> ()

  (** Removes all children from a widget.
      If [hard] = [true], then all child widgets are destroyed. *)
  method set_empty ?(hard = false) () : unit =
    Element.remove_children self#root;
    if hard then List.iter (fun x -> x#destroy ()) _widgets;
    _widgets <- []

  method set_on_destroy (f : unit -> unit) : unit =
    _on_destroy <- Some f

  method get_child_element_by_class x =
    self#root##querySelector (Js.string ("." ^ x))
    |> Js.Opt.to_option

  method get_child_element_by_id x =
    self#root##querySelector (Js.string ("#" ^ x))
    |> Js.Opt.to_option

  method get_attribute (a : string) : string option =
    Element.get_attribute self#root a

  method set_attribute (a : string) (v : string) : unit =
    Element.set_attribute self#root a v

  method remove_attribute (a : string) : unit =
    Element.remove_attribute self#root a

  method has_attribute (a : string) : bool =
    Js.to_bool @@ self#root##hasAttribute (Js.string a)

  method classes : string list =
    String.split_on_char ' ' @@ Js.to_string @@ self#root##.className

  method add_class (_class : string) : unit =
    Element.add_class self#root _class

  method remove_class (_class : string) : unit =
    Element.remove_class self#root _class

  method toggle_class' ?(force : bool option) (_class : string) : bool =
    Element.toggle_class ?force self#root _class

  method toggle_class ?(force : bool option) (_class : string) : unit =
    ignore @@ self#toggle_class' ?force _class

  method has_class (_class : string) : bool =
    Element.has_class self#root _class

  method is_rtl () : bool =
    let style = (Dom_html.window##getComputedStyle self#root) in
    let dir = Js.to_string style##.direction in
    String.equal dir "rtl"

  method emit : 'a 'e. ?should_bubble:bool ->
                ?detail:'a ->
                ('a #custom_event as 'e) Js.t Events.Typ.t ->
                unit =
    fun ?(should_bubble = false)
        ?(detail : _ option)
        (evt_type : _ Events.Typ.t) ->
    let (evt : 'a custom_event Js.t) =
      match Js.(to_string @@ typeof (Unsafe.global##.CustomEvent)) with
      | "function" ->
         let custom : (_ Events.Typ.t -> _ Js.t -> _ custom_event Js.t) Js.constr =
           Js.Unsafe.global##.CustomEvent in
         let obj =
           object%js
             val detail = Js.Opt.option detail
             val bubbles = should_bubble
           end in
         new%js custom evt_type obj
      | _ ->
         let doc = Js.Unsafe.coerce Dom_html.document in
         let evt = doc##createEvent (Js.string "CustomEvent") in
         evt##initCustomEvent evt_type
           (Js.bool should_bubble)
           Js._false
           (Js.Opt.option detail) in
    (Js.Unsafe.coerce self#root)##dispatchEvent evt

  initializer
    self#init ();
    self#initial_sync_with_dom ()
end

let equal (x : (#t as 'a)) (y : 'a) =
  x#root == y#root

let coerce (x : #t) = (x :> t)

let root (x : #t) : Dom_html.element Js.t = x#root

let layout (x : #t) : unit = x#layout ()

let destroy (x : #t) = x#destroy ()

let to_markup (x : #t) = Tyxml_js.Of_dom.of_element x#root

let create ?widgets x = new t ?widgets x ()

let create_div ?(widgets = []) () =
  let div = create @@ Dom_html.(createDiv document) in
  List.iter div#append_child widgets;
  div

let create_span ?(widgets = []) () =
  let span = create @@ Dom_html.(createSpan document) in
  List.iter span#append_child widgets;
  span
