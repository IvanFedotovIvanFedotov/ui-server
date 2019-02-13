open Js_of_ocaml
open Containers
open Components
open Tyxml_js

(* TODO
 * add orientation change handling - open video in fullscreen;
 *)

let fullscreen_enabled = Fullscreen.is_enabled ()

let get_boolean_attr ?(default = false)
      (elt : #Dom_html.element Js.t)
      (attr : string) : bool =

  match Js.Opt.to_option (elt##getAttribute (Js.string attr)) with
  | None -> default
  | Some x -> bool_of_string @@ Js.to_string x

let set_boolean_attr (elt : #Dom_html.element Js.t)
      (attr : string) (v : bool) : unit =
  elt##setAttribute (Js.string attr) (Js.string @@ string_of_bool v)

module Markup = Page_mosaic_video_tyxml.Player.Make(Xml)(Svg)(Html)

module Selectors = struct
  let video = "." ^ Markup.CSS.video
  let action_icon =
    Printf.sprintf ".%s:not(.%s)"
      Icon_button.Markup.CSS.icon
      Icon_button.Markup.CSS.icon_on
  let play = "." ^ Markup.CSS.Controls.action_play
  let fullscreen = "." ^ Markup.CSS.Controls.action_fullscreen
  let mute = "." ^ Markup.CSS.Controls.action_mute
  let volume = "." ^ Markup.CSS.Controls.volume
end

module Controls = struct

  let icon_of_volume ?(muted = false) (vol : float) : string =
    if vol <=. 0. || muted
    then Icon.SVG.Path.volume_off
    else if vol <. 0.33
    then Icon.SVG.Path.volume_low
    else if vol <. 0.67
    then Icon.SVG.Path.volume_medium
    else Icon.SVG.Path.volume_high

  module Action = struct

    class t (elt : #Dom_html.buttonElement Js.t) () =
    object
      inherit Icon_button.t elt ()

      method! initial_sync_with_dom () : unit =
        ()

    end

    let attach (elt : #Dom_html.element Js.t) : t =
      new t (Js.Unsafe.coerce elt) ()

  end

  class t (elt : #Dom_html.element Js.t) () =
    let (parent : Dom_html.element Js.t) =
      Js.Unsafe.coerce
      @@ Option.get_exn
      @@ Js.Opt.to_option elt##.parentNode in
    object(self)

      (* React events *)
      val mutable e_volume : unit React.event option = None

      (* DOM nodes *)
      val video : Dom_html.videoElement Js.t =
        Js.Unsafe.coerce
        @@ Option.get_exn
        @@ Element.query_selector parent Selectors.video
      val play =
        Option.map Action.attach
        @@ Element.query_selector elt Selectors.play
      val fullscreen =
        Option.map Action.attach
        @@ Element.query_selector elt Selectors.fullscreen
      val mute =
        Option.map Action.attach
        @@ Element.query_selector elt Selectors.mute
      val volume =
        Option.map Slider.attach
        @@ Element.query_selector elt Selectors.volume

      (* Event listeners *)
      val mutable fullscreen_handler = None

      inherit Widget.t elt () as super

      method! init () : unit =
        super#init ();
        (* Add event listeners *)
        Option.iter (fun (btn : Icon_button.t) ->
            btn#listen_click_lwt' (fun _ _ ->
                if Js.to_bool video##.paused || Js.to_bool video##.ended
                then video##play else video##pause;
                Lwt.return_unit))
          self#play_button;
        Option.iter (fun (btn : Icon_button.t) ->
            btn#listen_click_lwt' (fun _ _ ->
                let volume = video##.volume in
                if volume =. 0.
                then (
                  let to_set = 0.1 in
                  video##.volume := to_set;
                  video##.muted := Js._false)
                else
                  video##.muted := Js.bool @@ not (Js.to_bool video##.muted);
                Lwt.return_unit))
          self#mute_button;
        if fullscreen_enabled
        then (
          Option.iter (fun (btn : Icon_button.t) ->
              btn#style##.display := Js.string "block";
              btn#listen_click_lwt' (fun _ _ ->
                  if Fullscreen.is_fullscreen ()
                  then Fullscreen.cancel ()
                  else Fullscreen.enter parent;
                  Lwt.return_unit))
            self#fullscreen_button;
          super#listen_lwt' Events.Typ.dblclick (fun e _ ->
              Dom_html.stopPropagation e;
              Lwt.return_unit));
        (* Add react event listeners *)
        let e_volume' =
          Option.map (fun (slider : Slider.t) ->
              React.E.map (fun (v : float) ->
                  if Js.to_bool video##.muted
                  then video##.muted := Js._false;
                  video##.volume := v /. 100.) slider#e_input)
            volume in
        e_volume <- e_volume';

      method! destroy () : unit =
        super#destroy ();
        Option.(
          iter Widget.destroy play;
          iter Widget.destroy fullscreen;
          iter Widget.destroy mute;
          iter Widget.destroy volume;
          iter (React.E.stop ~strong:true) e_volume);
        e_volume <- None

      method play_button : Icon_button.t option =
        play

      method fullscreen_button : Icon_button.t option =
        fullscreen

      method mute_button : Icon_button.t option =
        mute

      method volume_slider : Slider.t option =
        volume

    end

  let attach (elt : #Dom_html.element Js.t) : t =
    new t elt ()

end

class t (elt : #Dom_html.element Js.t) () =
  let (video_elt : Dom_html.videoElement Js.t) =
    match Element.query_selector elt ("." ^ Markup.CSS.video) with
    | Some x -> Js.Unsafe.coerce x
    | None -> failwith "no video element found" in
  let (controls : Controls.t option) =
    match Element.query_selector elt ("." ^ Markup.CSS.Controls.root) with
    | None -> None
    | Some x -> Some (Controls.attach x) in
  let video = Widget.create video_elt in
  object(self)

    (* Dom event handlers *)
    val mutable fullscreen_handlers = []

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      if fullscreen_enabled
      then (
        super#listen_lwt' Events.Typ.dblclick (fun _ _ ->
            if Fullscreen.is_fullscreen ()
            then Fullscreen.cancel ()
            else Fullscreen.enter super#root;
            Lwt.return_unit));
      video#listen_lwt' Events.Typ.canplay (fun _ _ ->
          Option.iter (fun x -> x#set_disabled false) self#play_button;
          Lwt.return_unit);
      video#listen_lwt' Events.Typ.play (fun _ _ ->
          Option.iter (fun (x : Icon_button.t) ->
              x#set_disabled false;
              x#set_on true) self#play_button;
          Lwt.return_unit);
      video#listen_lwt' Events.Typ.pause (fun _ _ ->
          Option.iter (fun (x : Icon_button.t) -> x#set_on false)
            self#play_button;
          Lwt.return_unit);
      video#listen_lwt' Events.Typ.volumechange (fun _ _ ->
          begin match self#volume_slider with
          | None -> ()
          | Some (s : Slider.t) ->
             if self#muted
             then s#set_value 0.
             else s#set_value (100. *. self#volume)
          end;
          match self#mute_button with
          | None -> Lwt.return_unit
          | Some (x : Icon_button.t) ->
             if self#muted
             then x#set_on true
             else (
               x#set_on false;
               let path = Controls.icon_of_volume self#volume in
               Element.query_selector x#root Selectors.action_icon
               |> Option.flat_map (fun x -> Element.query_selector x "path")
               |> Option.iter (fun e -> Element.set_attribute e "d" path));
             Lwt.return_unit);
      let fs_handlers =
        List.map (fun typ ->
            Events.(listen Dom_html.document (Typ.make typ)
                      self#handle_fullscreenchange))
          [ "fullscreenchange"
          ; "webkitfullscreenchange"
          ; "mozfullscreenchange"
          ; "msfullscreenchange"
          ] in
      fullscreen_handlers <- fs_handlers;

    method! destroy () : unit =
      super#destroy ();
      (* Detach DOM event listeners *)
      List.iter Dom_events.stop_listen fullscreen_handlers;
      fullscreen_handlers <- [];

    method video_element : Dom_html.videoElement Js.t =
      video_elt

    method theater_mode : bool =
      super#has_class Markup.CSS.theater_mode
    method set_theater_mode (x : bool) : unit =
      super#toggle_class ~force:x Markup.CSS.theater_mode

    method play () : unit =
      self#video_element##play

    method pause () : unit =
      self#video_element##pause

    method autoplay : bool =
      Js.to_bool (self#video_element##.autoplay)
    method set_autoplay (x : bool) : unit =
      self#video_element##.autoplay := Js.bool x

    method playsinline : bool =
      get_boolean_attr self#video_element "playsinline"
    method set_playsinline (x : bool) : unit =
      set_boolean_attr self#video_element "playsinline" x

    method muted : bool =
      Js.to_bool (self#video_element##.muted)
      || self#volume =. 0.
    method set_muted (x : bool) =
      self#video_element##.muted := Js.bool x

    method paused : bool =
      Js.to_bool (self#video_element##.paused)

    method ended : bool =
      Js.to_bool (self#video_element##.ended)

    method duration : float =
      self#video_element##.duration

    method current_time : float =
      self#video_element##.currentTime

    method volume : float =
      self#video_element##.volume
    method set_volume (v : float) : unit =
      self#video_element##.volume := v

    (* Private methods *)

    method private play_button : Icon_button.t option =
      Option.flat_map (fun x -> x#play_button) controls

    method private fullscreen_button : Icon_button.t option =
      Option.flat_map (fun x -> x#fullscreen_button) controls

    method private mute_button : Icon_button.t option =
      Option.flat_map (fun x -> x#mute_button) controls

    method private volume_slider : Slider.t option =
      Option.flat_map (fun x -> x#volume_slider) controls

    method private set_controls (x : bool) : unit =
      self#video_element##.controls := Js.bool x

    method private handle_fullscreenchange _ _ : bool =
      begin match Fullscreen.is_fullscreen () with
      | true ->
         super#add_class Markup.CSS.big_mode;
         Option.iter (fun (x : Icon_button.t) -> x#set_on true)
           self#fullscreen_button
      | false ->
         super#remove_class Markup.CSS.big_mode;
         Option.iter (fun (x : Icon_button.t) -> x#set_on false)
           self#fullscreen_button
      end;
      true

  end

let attach (elt : #Dom_html.element Js.t) : t =
  new t elt ()
