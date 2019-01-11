open Js_of_ocaml
open Containers
open Tyxml_js

type slide = [`Leading | `Trailing] [@@deriving eq]

module type M = sig
  include Components_markup.Side_sheet.Common_css
  val slide : slide
end

module Make_parent(M : M) = struct

  let get_target (e : #Dom_html.event Js.t) : Dom_html.element Js.t =
    Js.Opt.get (e##.target) (fun () -> raise Not_found)

  let get_touch (e : Dom_html.touchEvent Js.t) : Dom_html.touch Js.t =
    Js.Optdef.get (e##.changedTouches##item 0)
      (fun () -> raise Not_found)

  class t (elt : #Dom_html.element Js.t) () =
    let state, set_state = React.S.create false in
    object(self)
      val mutable previous_focus = None

      (* Animation *)
      val mutable animation_frame = None
      val mutable animation_timer = None

      (* Initial touch point *)
      val mutable start_x = 0

      (* Touch event listeners *)
      val mutable touchstart_listener = None
      val mutable touchmove_listener = None
      val mutable touchcancel_listener = None
      val mutable touchend_listener = None

      (* Other event listeners *)
      val mutable transitionend_listener = None
      val mutable keydown_listener = None

      inherit Widget.t elt () as super

      method! init () : unit =
        super#init ();
        (* Connect event listeners *)
        self#listen_lwt Widget.Event.touchstart (fun e _ -> self#on_touchstart e)
        |> (fun x -> touchstart_listener <- Some x);
        Dom_events.listen Dom_html.window Widget.Event.keydown
          (fun _ e -> self#on_keydown e)
        |> (fun x -> keydown_listener <- Some x);
        self#listen_lwt (Dom_events.Typ.make "transitionend") (fun e _ ->
            self#handle_transition_end e; Lwt.return_unit)
        |> (fun x -> transitionend_listener <- Some x)

      method! destroy () : unit =
        super#destroy ();
        (* Disconnect event listeners *)
        Option.iter Lwt.cancel touchstart_listener;
        touchstart_listener <- None;
        Option.iter Lwt.cancel touchmove_listener;
        touchmove_listener <- None;
        Option.iter Dom_events.stop_listen touchend_listener;
        touchend_listener <- None;
        Option.iter Dom_events.stop_listen touchcancel_listener;
        touchcancel_listener <- None;
        Option.iter Dom_events.stop_listen keydown_listener;
        keydown_listener <- None;
        Option.iter Lwt.cancel transitionend_listener;
        transitionend_listener <- None;
        (* Clear animation *)
        Option.iter Utils.Animation.cancel_animation_frame animation_frame;
        animation_frame <- None;
        Option.iter Utils.clear_timeout animation_timer;
        animation_timer <- None

      method show () : unit =
        if not self#is_open && not self#is_opening && not self#is_closing
        then begin
            super#add_class M.open_;
            super#add_class M.animate;
            self#run_next_animation_frame (fun () ->
                super#add_class M.opening);
            self#save_focus ()
          end

      method show_await () : unit Lwt.t =
        match self#is_open with
        | true -> Lwt.return_unit
        | false ->
           let open Lwt.Infix in
           self#show ();
           Lwt_react.E.next (React.S.changes self#s_open)
           >|= ignore

      method hide () : unit =
        if self#is_open && not self#is_opening && not self#is_closing
        then super#add_class M.closing

      method toggle () : unit =
        if self#is_open then self#hide () else self#show ()

      method is_open : bool =
        super#has_class M.open_

      method s_open : bool React.signal = state

      (* Private methods *)

      method private save_focus () : unit =
        previous_focus <- Js.Opt.to_option Dom_html.document##.activeElement

      method private restore_focus () : unit =
        match previous_focus with
        | None -> ()
        | Some elt ->
           if Js.to_bool @@ (Js.Unsafe.coerce self#root)##contains elt
           then elt##focus

      method private focus_active_navigation_item () : unit =
        (* TODO improve query *)
        let query = Js.string "a, button, input" in
        Js.Opt.iter (self#root##querySelector query) (fun e -> e##focus)

      method private is_opening : bool =
        super#has_class M.opening

      method private is_closing : bool =
        super#has_class M.closing

      method private run_next_animation_frame (cb : unit -> unit) : unit =
        Option.iter Utils.Animation.cancel_animation_frame animation_frame;
        let af =
          Utils.Animation.request_animation_frame (fun _ ->
              animation_frame <- None;
              Option.iter Utils.clear_timeout animation_timer;
              let timer = Utils.set_timeout cb 0. in
              animation_timer <- Some timer) in
        animation_frame <- Some af

      method private handle_transition_end (e : #Dom_html.event Js.t) : unit =
        try
          let target = get_target e in
          let class' = Js.string M.root in
          if Js.to_bool @@ target##.classList##contains class'
          then begin
              if self#is_closing
              then (super#remove_class M.open_;
                    self#restore_focus ();
                    set_state false)
              else (self#focus_active_navigation_item ();
                    set_state true)
            end;
          super#remove_class M.animate;
          super#remove_class M.opening;
          super#remove_class M.closing;
        with Not_found -> ()

      method private get_delta ~x ~touch =
        match M.slide with
        | `Leading -> x - touch##.clientX
        | `Trailing -> touch##.clientX - x

      method private on_keydown (e : Dom_html.keyboardEvent Js.t) : bool =
        match Utils.Keyboard_event.event_to_key e with
        | `Escape -> self#hide (); true
        | _ -> true

      method private on_event_end (e : Dom_html.touchEvent Js.t) : unit =
        Option.iter Lwt.cancel touchmove_listener;
        touchmove_listener <- None;
        Option.iter Dom_events.stop_listen touchend_listener;
        touchend_listener <- None;
        Option.iter Dom_events.stop_listen touchcancel_listener;
        touchcancel_listener <- None;
        let touch =
          Js.Optdef.get
            (e##.changedTouches##item 0)
            (fun () -> failwith "touch fail") in
        let delta = self#get_delta ~x:start_x ~touch in
        if delta > self#offset_width / 2 then
          begin
            self#hide ();
            self#style##.transform := Js.string ""
          end
        else self#style##.transform := Js.string ""

      method private on_touchstart (e : Dom_html.touchEvent Js.t) : unit Lwt.t =
        let touch = get_touch e in
        if self#is_open
           && not (self#scroll_width > self#offset_width) then
          begin
            start_x <- touch##.clientX;
            self#listen_lwt Widget.Event.touchmove
              (fun e _ -> self#on_touchmove e)
            |> (fun x -> touchmove_listener <- Some x);
            Dom_events.listen Dom_html.window Widget.Event.touchend
              (fun _ e -> self#on_event_end e; true)
            |> (fun x -> touchend_listener <- Some x);
            Dom_events.listen Dom_html.window Widget.Event.touchcancel
              (fun _ e -> self#on_event_end e; true)
            |> (fun x -> touchcancel_listener <- Some x)
          end;
        Lwt.return_unit

      method private on_touchmove (e : Dom_html.touchEvent Js.t) : unit Lwt.t =
        let touch =
          Js.Optdef.get
            (e##.changedTouches##item 0)
            (fun () -> failwith "touch fail") in
        let delta, transform =
          match M.slide with
          | `Leading ->
             let dx = start_x - touch##.clientX in
             dx, Printf.sprintf "translateX(-%dpx)" dx
          | `Trailing ->
             let dx = touch##.clientX - start_x in
             dx, "translateX(" ^(string_of_int dx) ^ "px)" in
        if delta > 0 then
          begin
            Dom_html.stopPropagation e;
            self#style##.transform := Js.string transform;
          end;
        Lwt.return_unit

    end

end

module Markup = Components_markup.Side_sheet.Make(Xml)(Svg)(Html)

module Parent =
  Make_parent(struct
      include Markup.CSS
      let slide = `Trailing
    end)

module Scrim = struct

  class t ?elt () =
    let elt = match elt with
      | Some elt -> elt
      | None -> To_dom.of_element @@ Markup.create_scrim () in
    object
      inherit Widget.t elt ()
    end

  (** Creates new widget from scratch *)
  let make () : t =
    new t ()

  (** Attach widget to existing element *)
  let attach (elt : #Dom_html.element Js.t) : t =
    new t ~elt ()

end

type elt =
  [ `Elt of Dom_html.element Js.t
  | `Content of Widget.t list
  ]

class t (elt : elt) () =
  let elt = match elt with
    | `Elt elt -> elt
    | `Content cnt ->
       let content_wrapper =
         Markup.create_content (List.map Widget.to_markup cnt) () in
       To_dom.of_element @@ Markup.create content_wrapper () in
  object
    inherit Parent.t elt ()
  end

(** Creates new widget from scratch *)
let make ~(content : #Widget.t list) () : t =
  new t (`Content (List.map Widget.coerce content)) ()

(** Attach widget to existing element *)
let attach (elt : #Dom_html.element Js.t) : t =
  new t (`Elt (Widget.Element.coerce elt)) ()