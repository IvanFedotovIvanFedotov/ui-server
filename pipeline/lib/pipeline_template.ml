open Common.User
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
  let classes = [Player.CSS.Controls.volume_slider] in
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
  let open Icon.SVG in
  Player.Controls.(
    let play =
      make_player_action
        ~disabled:true
        ~classes:[Player.CSS.Controls.action_play]
        ~on_path:Path.pause
        Path.play in
    let volume =
      make_player_action
        ~classes:[Player.CSS.Controls.action_volume]
        Path.volume_high in
    let slider = make_slider () in
    let fullscreen =
      make_player_action
        ~classes:[Player.CSS.Controls.action_fullscreen]
        ~on_path:Path.fullscreen_exit
        Path.fullscreen in
    let section_left = create_section ~align:`Start [play; volume; slider] () in
    let section_right = create_section ~align:`End [fullscreen] () in
    create [section_left; section_right] ())

let make_player () : 'a Html.elt =
  let video =
    Player.create_video
      ~autoplay:true
      ~controls:false
      ~playsinline:true
      () in
  let gradient = Player.create_gradient () in
  let controls = make_player_controls () in
  Player.create ~theater_mode:true ~video ~controls ~gradient ()

let create_video () : 'a item =
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
         ; icon = Some (Html.toelt @@ make_icon Icon.SVG.Path.video)
         ; href = Common.Uri.Path.of_string "video"
         ; template }

let create_editor () : 'a item =
  let id = "mosaic-editor" in
  let app_bar = make_app_bar_props ~title:"Редактор мозаики" () in
  let template =
    make_tmpl_props ~id ~app_bar
      ~post_scripts:[Src "/js/mosaic_editor.js"]
      ~stylesheets:["/css/pipeline.min.css"]
      () in
  Simple { id
         ; title = "Редактор"
         ; icon = Some (Html.toelt @@ make_icon Icon.SVG.Path.view_dashboard)
         ; href = Common.Uri.Path.of_string "editor"
         ; template }

let create_subtree () =
  Subtree { title = "Мозаика"
          ; icon = None
          ; href = Common.Uri.Path.of_string "mosaic"
          ; templates =
              [ (`Index 1, create_video ())
              ; (`Index 2, create_editor ()) ]}

let create () : upper ordered_item list user_table =
  let rval = [`Index 3, create_subtree ()] in
  { root = rval; operator = rval; guest = rval }
