open Components_tyxml
open Tyxml

module CSS = Page_mosaic_video_tyxml.CSS
module Markup = Page_mosaic_video_tyxml.Make(Xml)(Svg)(Html)
module Icon = struct
  include Icon
  include Icon.Make(Xml)(Svg)(Html)
end
module Icon_button = struct
  include Icon_button
  include Icon_button.Make(Xml)(Svg)(Html)
end
module Slider = struct
  include Slider
  include Slider.Make(Xml)(Svg)(Html)
end
module Player = struct
  include Page_mosaic_video_tyxml.Player
  include Page_mosaic_video_tyxml.Player.Make(Xml)(Svg)(Html)
end

let make_icon ?classes path =
  let path = Icon.SVG.create_path path () in
  Icon.SVG.create ?classes [path] ()

let make_icon_button ?classes path =
  let path = Icon.SVG.create_path path () in
  let icon = Icon.SVG.create [path] () in
  Icon_button.create ?classes ~icon ()

let make_slider () =
  let classes = [Player.CSS.Controls.volume] in
  Slider.create ~classes ~step:5. ()

let make_player_action ?classes ?disabled ?on_path path =
  let make_icon ?(on = false) path =
    let classes =
      [Icon_button.CSS.icon]
      |> Utils.cons_if on Icon_button.CSS.icon_on in
    make_icon ~classes path in
  Player.Controls.(
    create_action
      ?classes
      ?disabled
      ~ripple:false
      ~icon:(make_icon path)
      ?on_icon:(match on_path with
          | None -> None
          | Some x -> Some (make_icon ~on:true x))
      ())

let make_player_controls () =
  Player.Controls.(
    let play =
      make_player_action
        (* ~disabled:true *)
        ~classes:[Player.CSS.Controls.action_play]
        ~on_path:Player.Path.pause
        Player.Path.play in
    let volume =
      make_player_action
        ~classes:[Player.CSS.Controls.action_mute]
        ~on_path:Player.Path.volume_off
        Player.Path.volume_high in
    let slider = make_slider () in
    let fullscreen =
      make_player_action
        ~classes:[Player.CSS.Controls.action_fullscreen]
        ~on_path:Player.Path.fullscreen_exit
        Player.Path.fullscreen in
    let volume_panel =
      Player.Controls.create_volume_panel [volume; slider] () in
    let section_left =
      create_section ~align:`Start
        [play; volume_panel]
        () in
    let section_right =
      create_section ~align:`End
        [fullscreen]
        () in
    create [section_left; section_right] ())

let make_player () : 'a Html.elt =
  let audio =
    Player.create_audio
      ~autoplay:false
      ~playsinline:true
      ~controls:true
      () in
  let video =
    Player.create_video
      ~autoplay:true
      ~controls:false
      ~playsinline:true
      () in
  let state_overlay = Player.create_state_overlay Player.Path.play () in
  let gradient = Player.create_gradient () in
  let controls = make_player_controls () in
  Player.create ~audio ~video ~state_overlay ~controls ~gradient ()
