open Js_of_ocaml
open Utils

(* TODO
   - add RTL support
 *)

include Components_tyxml.Tab_scroller
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let ( >>= ) = Lwt.bind

type animation =
  { final_scroll_position : int
  ; scroll_delta : int
  }

let compute_horizontal_scroll_height () : int option =
  let el = Dom_html.(createDiv document) in
  el##.classList##add (Js.string CSS.scroll_test);
  Dom.appendChild Dom_html.document##.body el;
  let height = el##.offsetHeight - el##.clientHeight in
  Dom.removeChild Dom_html.document##.body el;
  Some height

class t ?tabs (elt : Dom_html.element Js.t) () =
object(self)
  val _scroll_content = find_element_by_class_exn elt CSS.scroll_content
  val _scroll_area = find_element_by_class_exn elt CSS.scroll_area
  val mutable _hscroll_height = 0
  val mutable _animating : bool = false
  val mutable _listeners = []
  val mutable _tabs = match tabs with
    | Some x -> x
    | None ->
       (* If we're attaching to an element, instantiate tabs *)
       List.filter_map (fun e ->
           if Element.has_class e Tab.CSS.root
           then Some (Tab.attach e) else None)
       @@ Element.children elt

  inherit Widget.t elt () as super

  method! init () : unit =
    super#init ();
    (* Compute horizontal scrollbar height on scroller with overflow initially hidden,
       then update overflow to scroll and immediately adjust bottom margin to avoid
       the scrollbar initially appearing before JS runs *)
    _scroll_area##.style##.marginBottom :=
      (match compute_horizontal_scroll_height () with
       | Some x -> Js.string (Printf.sprintf "-%dpx" x)
       | None -> Js.string "");
    Element.add_class _scroll_area CSS.scroll_area_scroll

  method! initial_sync_with_dom () : unit =
    super#initial_sync_with_dom ();
    (* Attach event listeners *)
    let handle_interaction = fun _ _ ->
      self#handle_interaction ();
      Lwt.return_unit in
    let handle_transitionend = fun e _ ->
      self#handle_transition_end e;
      Lwt.return_unit in
    let wheel = Events.wheels super#root handle_interaction in
    let touchstart = Events.touchstarts super#root handle_interaction in
    let pointerdown =
      Events.listen_lwt super#root (Events.Typ.make "pointerdown")
        handle_interaction in
    let mousedown = Events.mousedowns super#root handle_interaction in
    let keydown = Events.keydowns super#root handle_interaction in
    let transitionend =
      Events.listen_lwt super#root (Events.Typ.make "transitionend")
        handle_transitionend in
    let listeners =
      [ wheel
      ; touchstart
      ; pointerdown
      ; mousedown
      ; keydown
      ; transitionend
      ] in
    _listeners <- listeners;

  method! destroy () : unit =
    super#destroy ();
    (* Detach event listeners *)
    List.iter Lwt.cancel _listeners;
    _listeners <- []

  method! layout () : unit Lwt.t =
    super#layout ()
    >>= fun () -> Lwt_list.iter_p Widget.layout _tabs

  method tabs : Tab.t list =
    _tabs

  method remove_tab (tab : Tab.t) : unit =
    _tabs <- List.remove ~eq:Widget.equal tab _tabs;
    Element.remove_child_safe super#root tab#root;
    Lwt.ignore_result @@ self#layout ()

  method append_tab (tab : Tab.t) : unit =
    _tabs <- tab :: _tabs;
    Element.append_child super#root tab#root;
    Lwt.ignore_result @@ self#layout ()

  method insert_tab_at_index (i : int) (tab : Tab.t) : unit =
    _tabs <- tab :: _tabs;
    Element.insert_child_at_index super#root i tab#root;
    Lwt.ignore_result @@ self#layout ()

  method get_tab_at_index (i : int) : Tab.t option =
    List.find_opt (fun (tab : Tab.t) -> tab#index = i) _tabs

  method active_tab : Tab.t option =
    List.find_opt (fun (tab : Tab.t) -> tab#active) _tabs

  method set_active_tab (tab : Tab.t) : unit =
    match self#active_tab with
    | None -> tab#set_active true
    | Some previous ->
       tab#set_active ~previous true;
       previous#set_active false;

  method align : align option =
    if super#has_class CSS.align_start
    then Some Start
    else if super#has_class CSS.align_end
    then Some End
    else if super#has_class CSS.align_center
    then Some Center
    else None

  method set_align (x : align option) : unit =
    super#remove_class CSS.align_start;
    super#remove_class CSS.align_end;
    super#remove_class CSS.align_center;
    match x with
    | None -> ()
    | Some Start -> super#add_class CSS.align_start
    | Some End -> super#add_class CSS.align_end
    | Some Center -> super#add_class CSS.align_center

  method content_width : int =
    _scroll_content##.offsetWidth

  (* Computes the current visual scroll position *)
  method get_scroll_position () : int =
    let current_translate_x = self#get_current_translate_x () in
    let scroll_left = _scroll_area##.scrollLeft in
    scroll_left - current_translate_x

  (* Scrolls to the given scrollX value *)
  method scroll_to (scroll_x : int) : unit =
    let current_scroll_x = self#get_scroll_position () in
    let safe_scroll_x = self#clamp_scroll_value scroll_x in
    let scroll_delta = safe_scroll_x - current_scroll_x in
    self#animate { scroll_delta; final_scroll_position = safe_scroll_x }

  (* Increment scroll value by the given value *)
  method increment_scroll (scroll_x : int) : unit =
    match scroll_x with
    | 0 -> ()
    | x ->
       let current_scroll_x = self#get_scroll_position () in
       let target_scroll_x = x + current_scroll_x in
       let safe_scroll_x = self#clamp_scroll_value target_scroll_x in
       let scroll_delta = safe_scroll_x - current_scroll_x in
       self#animate { scroll_delta; final_scroll_position = safe_scroll_x }

  (* Private methods *)

  (* Handles interaction events that occur during transition *)
  method private handle_interaction () : unit =
    if _animating then self#stop_scroll_animation ()

  (* Handles transitionend event *)
  method private handle_transition_end e : unit =
    Js.Opt.iter e##.target (fun (target : Dom_html.element Js.t) ->
        if _animating && Element.matches target ("." ^ CSS.scroll_content)
        then (_animating <- false; super#remove_class CSS.animating))

  method private calculate_scroll_edges () : int * int =
    let content_width = _scroll_content##.offsetWidth in
    let root_width = _scroll_area##.offsetWidth in
    (* left, right *)
    (0, content_width - root_width)

  (* Calculates a safe scroll value that is > 0 and < the max scroll value
   * v - the distance to scroll
   *)
  method private clamp_scroll_value (v : int) :  int =
    let left, right = self#calculate_scroll_edges () in
    min (max left v) right

  method private get_current_translate_x () =
    let style = Dom_html.window##getComputedStyle _scroll_content in
    let value = Js.to_string style##.transform in
    match value with
    | "none" -> 0
    | value ->
       try
         int_of_float
         @@ float_of_string
         @@ String.trim
         @@ List.nth (String.split_on_char ',' value) 4
       with _ -> 0

  (* Gets the current scroll position during animation *)
  method private get_animating_scroll_position () : int =
    let current_translate_x = self#get_current_translate_x () in
    let scroll_left = _scroll_area##.scrollLeft in
    scroll_left - current_translate_x

  (* Stops scroll animation *)
  method private stop_scroll_animation () : unit =
    _animating <- false;
    let current_scroll_position = self#get_animating_scroll_position () in
    super#remove_class CSS.animating;
    super#root##.style##.transform := Js.string "translateX(0px)";
    _scroll_area##.scrollLeft := current_scroll_position

  (* Animates the tab scrolling *)
  method private animate (a : animation) : unit =
    (* Early exit if translateX is 0, which means
     * there is no animation to perform *)
    if a.scroll_delta <> 0 then (
      self#stop_scroll_animation ();
      _scroll_area##.scrollLeft := a.final_scroll_position;
      let translate_x = Printf.sprintf "translateX(%dpx)" a.scroll_delta in
      _scroll_content##.style##.transform := Js.string translate_x;
      (* Force repaint *)
      ignore @@ _scroll_area##getBoundingClientRect;
      Animation.request_animation_frame (fun _ ->
          super#add_class CSS.animating;
          _scroll_content##.style##.transform := Js.string "none")
      |> ignore;
      _animating <- true)

end

let make ?align (tabs : Tab.t list) : t =
  let content = Markup.create_scroll_content (List.map Widget.to_markup tabs) () in
  let scroll_area = Markup.create_scroll_area ~content () in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.create ?align ~scroll_area () in
  new t ~tabs elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
