open Js_of_ocaml
open Utils

class type ['a] custom_event =
  object
    inherit Events.event
    method detail : 'a Js.opt Js.readonly_prop
  end

type rect =
  { top : float
  ; right : float
  ; bottom : float
  ; left : float
  ; width : float option
  ; height : float option
  }

let to_rect (x : Dom_html.clientRect Js.t) =
  { top = x##.top
  ; right = x##.right
  ; bottom = x##.bottom
  ; left = x##.left
  ; width = Js.Optdef.to_option x##.width
  ; height = Js.Optdef.to_option x##.height
  }

class t ?(widgets : #t list option)
        (elt : #Dom_html.element Js.t)
        () = object(self)

  val mutable _on_destroy = None
  val mutable _listeners_lwt = []
  val mutable _widgets : t list = []

  val mutable _e_storage : unit React.event list = []
  val mutable _s_storage : unit React.signal list = []

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
    List.iter (React.S.stop ~strong:true) _s_storage;
    List.iter (React.E.stop ~strong:true) _e_storage;
    _s_storage <- [];
    _e_storage <- [];
    List.iter (fun x -> x#destroy ()) _widgets;
    _widgets <- [];
    List.iter (fun x -> try Lwt.cancel x with _ -> ()) _listeners_lwt;
    _listeners_lwt <- [];
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

  method parent_element : Element.t option =
    match Js.Opt.to_option self#root##.parentNode with
    | None -> None
    | Some p -> match p##.nodeType with
                | ELEMENT -> Some (Js.Unsafe.coerce p)
                | _ -> None

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

  method insert_child_at_idx : 'a. int -> (< node : Dom.node Js.t;
                                           widget : t;
                                           layout : unit -> unit;
                                           .. > as 'a) -> unit =
    fun index x ->
    Element.insert_child_at_index self#root index x#node;
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

  method has_attribute a =
    self#root##hasAttribute (Js.string a)
    |> Js.to_bool

  method inner_html =
    Js.to_string self#root##.innerHTML

  method outer_html =
    Js.to_string self#root##.outerHTML

  method set_inner_html s =
    self#root##.innerHTML := Js.string s

  method text_content : string option =
    self#root##.textContent
    |> Js.Opt.to_option
    |> Option.map Js.to_string

  method set_text_content s =
    self#root##.textContent := Js.some @@ Js.string s

  method id : string =
    Js.to_string self#root##.id

  method set_id (id : string) : unit =
    self#root##.id := Js.string id

  method style = self#root##.style

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

  method client_left : int =
    self#root##.clientLeft

  method client_top : int =
    self#root##.clientTop

  method client_width : int =
    self#root##.clientWidth

  method client_height : int =
    self#root##.clientHeight

  method offset_left : int =
    self#root##.offsetLeft

  method offset_top : int =
    self#root##.offsetTop

  method offset_width : int =
    self#root##.offsetWidth

  method offset_height : int =
    self#root##.offsetHeight

  method scroll_left : int =
    self#root##.scrollLeft

  method scroll_top : int =
    self#root##.scrollTop

  method scroll_width : int =
    self#root##.scrollWidth

  method scroll_height : int =
    self#root##.scrollHeight

  method set_scroll_left (x : int) : unit =
    self#root##.scrollLeft := x

  method set_scroll_top (x : int) : unit =
    self#root##.scrollTop := x

  method set_scroll_width (x : int) : unit =
    self#root##.scrollWidth := x

  method set_scroll_height (x : int) : unit =
    self#root##.scrollHeight := x

  method is_rtl () : bool =
    let style = (Dom_html.window##getComputedStyle self#root) in
    let dir = Js.to_string style##.direction in
    String.equal dir "rtl"

  method tab_index : int =
    (Js.Unsafe.coerce self#root)##.tabIndex

  method set_tab_index (v : int) : unit =
    (Js.Unsafe.coerce self#root)##.tabIndex := v

  method listen :
           'a. (#Events.event as 'a) Js.t Events.Typ.t ->
           (#Dom_html.element Js.t -> 'a Js.t -> bool) ->
           Dom_events.listener =
    Dom_events.listen self#root

  method listen_once_lwt :
           'a. ?use_capture:bool ->
           (#Events.event as 'a) Js.t Events.Typ.t ->
           'a Js.t Lwt.t =
    fun ?use_capture x ->
    Lwt_js_events.make_event x ?use_capture self#root

  method listen_lwt :
           'a. ?store:bool ->
           ?cancel_handler:bool ->
           ?use_capture:bool ->
           (#Events.event as 'a) Js.t Events.Typ.t ->
           ('a Js.t -> unit Lwt.t -> unit Lwt.t) ->
           unit Lwt.t =
    fun ?(store = false) ?cancel_handler ?use_capture x f ->
    let (t : unit Lwt.t) =
      Events.listen_lwt ?cancel_handler ?use_capture self#root x f in
    if store then _listeners_lwt <- t :: _listeners_lwt;
    t

  method listen_lwt' :
           'a. ?cancel_handler:bool ->
           ?use_capture:bool ->
           (#Events.event as 'a) Js.t Events.Typ.t ->
           ('a Js.t -> unit Lwt.t -> unit Lwt.t) ->
           unit =
    fun ?cancel_handler ?use_capture x f ->
    let (t : unit Lwt.t) = self#listen_lwt ?cancel_handler ?use_capture x f in
    _listeners_lwt <- t :: _listeners_lwt

  method listen_click_lwt :
           ?store:bool ->
           ?cancel_handler:bool ->
           ?use_capture:bool ->
           (Dom_html.mouseEvent Js.t -> unit Lwt.t -> unit Lwt.t) ->
           unit Lwt.t =
    fun ?(store = false) ?cancel_handler ?use_capture f ->
    Events.(
      let t = self#listen_lwt ?cancel_handler ?use_capture Typ.click f in
      if store then _listeners_lwt <- t :: _listeners_lwt;
      t)

  method listen_click_lwt' ?cancel_handler ?use_capture f : unit =
    let (t : unit Lwt.t) =
      self#listen_click_lwt ?cancel_handler ?use_capture f in
    _listeners_lwt <- t :: _listeners_lwt

  method bounding_client_rect =
    (self#root##getBoundingClientRect)
    |> (fun x ->
      { top = x##.top
      ; right = x##.right
      ; bottom = x##.bottom
      ; left = x##.left
      ; width = Js.Optdef.to_option x##.width
      ; height = Js.Optdef.to_option x##.height })

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

  (* Private methods *)

  method private _keep_s : 'a. 'a React.signal -> unit = fun s ->
    _s_storage <- React.S.map ignore s :: _s_storage

  method private _keep_e : 'a. 'a React.event -> unit  = fun e ->
    _e_storage <- React.E.map ignore e :: _e_storage

  initializer
    self#init ();
    self#initial_sync_with_dom ()

end

let equal (x : (#t as 'a)) (y : 'a) =
  x#root == y#root

let coerce (x : #t) = (x :> t)

let layout (widget : #t) : unit = widget#layout ()

let destroy (x : #t) = x#destroy ()

let to_markup (x : #t) = Tyxml_js.Of_dom.of_element x#root

let append_to_body (x : #t) =
  Dom.appendChild Dom_html.document##.body x#root

let remove_from_body (x : #t) =
  try Dom.removeChild Dom_html.document##.body x#root
  with _ -> ()

let create ?widgets x = new t ?widgets x ()

let create_div ?(widgets = []) () =
  let div = create @@ Dom_html.(createDiv document) in
  List.iter div#append_child widgets;
  div

let create_span ?(widgets = []) () =
  let span = create @@ Dom_html.(createSpan document) in
  List.iter span#append_child widgets;
  span
