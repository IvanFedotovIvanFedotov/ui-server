open Js_of_ocaml
open Containers
open Components
open Tyxml_js

(* TODO
 * add orientation change handling - open video in fullscreen;
 * implement `make` function;
 *)

let get_boolean_attr ?(default = false)
      (elt : #Dom_html.element Js.t)
      (attr : string) : bool =

  match Js.Opt.to_option (elt##getAttribute (Js.string attr)) with
  | None -> default
  | Some x -> bool_of_string @@ Js.to_string x

let set_boolean_attr (elt : #Dom_html.element Js.t)
      (attr : string) (v : bool) : unit =
  elt##setAttribute (Js.string attr) (Js.string @@ string_of_bool v)

module Fullscreen = struct

  let fullscreen_element () : Element.t option =
    Js.Opt.to_option (Js.Unsafe.coerce Dom_html.document)##.fullscreenElement

  let enter (elt : #Dom.node Js.t) : unit =
    let test = Js.Optdef.test in
    let elt = Js.Unsafe.coerce elt in
    if test elt##.requestFullscreen
    then elt##requestFullscreen
    else if test elt##.mozRequestFullScreen
    then elt##mozRequestFullScreen
    else if test elt##.webkitRequestFullScreen
    then elt##webkitRequestFullScreen
    else if test elt##.msRequestFullscreen
    then elt##msRequestFullscreen

  let cancel (elt : #Dom.node Js.t) : unit =
    let test = Js.Optdef.test in
    let elt = Js.Unsafe.coerce elt in
    if test elt##.exitFullscreen
    then elt##exitFullscreen
    else if test elt##.mozCancelFullScreen
    then elt##mozCancelFullScreen
    else if test elt##.webkitExitFullScreen
    then elt##webkitExitFullScreen
    else if test elt##.msExitFullscreen
    then elt##msExitFullscreen

end

module Markup = Page_mosaic_video_tyxml.Player.Make(Xml)(Svg)(Html)

module Selectors = struct
  let video = "." ^ Markup.CSS.video
  let action_icon = "." ^ Icon_button.Markup.CSS.icon
  let play = "." ^ Markup.CSS.Controls.action_play
  let fullscreen = "." ^ Markup.CSS.Controls.action_fullscreen
  let volume = "." ^ Markup.CSS.Controls.action_volume
  let volume_slider = "." ^ Markup.CSS.Controls.volume_slider
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

  class t (elt : #Dom_html.element Js.t) () =
    let (parent : Dom_html.element Js.t) =
      Js.Unsafe.coerce
      @@ Option.get_exn
      @@ Js.Opt.to_option elt##.parentNode in
    object(self)

      val video : Dom_html.videoElement Js.t =
        Js.Unsafe.coerce
        @@ Option.get_exn
        @@ Element.query_selector parent Selectors.video

      (* DOM nodes *)
      val play =
        Option.map Icon_button.attach
        @@ Element.query_selector elt Selectors.play
      val fullscreen =
        Option.map Icon_button.attach
        @@ Element.query_selector elt Selectors.fullscreen
      val volume =
        Option.map Icon_button.attach
        @@ Element.query_selector elt Selectors.volume
      val volume_slider =
        Option.map Slider.attach
        @@ Element.query_selector elt Selectors.volume_slider

      (* Event listeners *)
      val mutable fullscreen_handler = None

      inherit Widget.t elt () as super

      method! init () : unit =
        super#init ();
        (* Add event listeners *)
        Option.iter (fun btn ->
            btn#set_on_change (function
                | true -> video##play
                | false -> video##pause))
          self#play_button;
        Option.iter (fun btn ->
            btn#set_on_change (function
                | true -> Fullscreen.enter parent
                | false -> Fullscreen.cancel Dom_html.document))
          self#fullscreen_button;
        super#listen_lwt' Events.Typ.dblclick (fun e _ ->
            Dom_html.stopPropagation e;
            Lwt.return_unit);

      method play_button : Icon_button.t option =
        play

      method fullscreen_button : Icon_button.t option =
        fullscreen

      method volume_button : Icon_button.t option =
        volume

      method volume_slider : Slider.t option =
        volume_slider

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

    val mutable fullscreen_handler = None

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      super#listen_lwt' Events.Typ.dblclick (fun _ _ ->
          if self#fullscreen
          then Fullscreen.cancel Dom_html.document
          else Fullscreen.enter super#root;
          Lwt.return_unit);
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
          let muted = self#muted in
          let path = Controls.icon_of_volume ~muted self#volume in
          begin match self#volume_button with
          | None -> ()
          | Some i ->
             Element.query_selector i#root Selectors.action_icon
             |> Option.flat_map (fun x -> Element.query_selector x "path")
             |> Option.iter (fun e -> Element.set_attribute e "d" path)
          end;
          Lwt.return_unit);
      let fs_handler =
        Events.(listen Dom_html.document Typ.fullscreenchange
                  self#handle_fullscreenchange) in
      fullscreen_handler <- Some fs_handler;

    method! destroy () : unit =
      super#destroy ();
      (* Detach DOM event listeners *)
      Option.iter Dom_events.stop_listen fullscreen_handler;
      fullscreen_handler <- None;

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

    method fullscreen : bool =
      let elt = Fullscreen.fullscreen_element () in
      match elt with
      | None -> false
      | Some (elt : Dom_html.element Js.t) -> Element.equal super#root elt

    (* Private methods *)

    method private play_button : Icon_button.t option =
      Option.flat_map (fun x -> x#play_button) controls

    method private fullscreen_button : Icon_button.t option =
      Option.flat_map (fun x -> x#fullscreen_button) controls

    method private volume_button : Icon_button.t option =
      Option.flat_map (fun x -> x#volume_button) controls

    method private set_controls (x : bool) : unit =
      self#video_element##.controls := Js.bool x

    method private handle_fullscreenchange _ _ : bool =
      begin match self#fullscreen with
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
