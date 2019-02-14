open Containers
open Api.Template
open Tyxml

module Markup = Components_tyxml.Make(Xml)(Svg)(Html)
module Player = Page_mosaic_video_tyxml.Player.Make(Xml)(Svg)(Html)

let make_icon ?classes path =
  let open Markup.Icon.SVG in
  let path = create_path path () in
  create ?classes [path] ()

let make_icon_button path =
  let path = Markup.Icon.SVG.create_path path () in
  let icon = Markup.Icon.SVG.create [path] () in
  Markup.Icon_button.create ~icon ()

let make_slider () =
  let classes = [Player.CSS.Controls.volume] in
  Markup.Slider.create ~classes ~step:5. ()

let make_player_action ?classes ?disabled ?on_path path =
  let make_icon ?(on = false) path =
    let classes =
      [Markup.Icon_button.CSS.icon]
      |> Markup.Utils.cons_if on Markup.Icon_button.CSS.icon_on in
    make_icon ~classes path in
  Player.Controls.(
    create_action
      ?classes
      ?disabled
      ~ripple:false
      ~icon:(make_icon path)
      ?on_icon:(Option.map (make_icon ~on:true) on_path)
      ())

let make_player_controls () =
  Player.Controls.(
    let play =
      make_player_action
        ~disabled:true
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
      ~autoplay:true
      ~playsinline:true
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
  Player.create ~theater_mode:true
    ~audio
    ~video
    ~state_overlay
    ~controls
    ~gradient
    ()

let create () : 'a item =
  let sprintf = Printf.sprintf in
  let id = "mosaic_video" in
  let app_bar = make_app_bar_props ~title:"Мозаика" () in
  let template =
    make_tmpl_props ~id ~app_bar
      ~pre_scripts:[ Src "/js/janus.nojquery.js"
                   ; Src "/js/adapter.min.js" ]
      ~post_scripts:[Src (sprintf "/js/%s.js" id)]
      ~stylesheets:[(sprintf "/css/%s.min.css" id)]
      ~content:[Html.toelt @@ make_player ()]
      () in
  Simple { id
         ; title = "Видео"
         ; icon = Some (Html.toelt @@ make_icon Player.Path.video)
         ; href = Common.Uri.Path.of_string "video"
         ; template }
