open Api.Template
open Tyxml

module Icon = Components_tyxml.Icon.Make(Xml)(Svg)(Html)

let make_icon ?classes path =
  let path = Icon.SVG.create_path path () in
  Icon.SVG.create ?classes [path] ()

let create () : 'a item =
  let id = "mosaic-editor" in
  let app_bar = make_app_bar_props ~title:"Редактор мозаики" () in
  let template =
    make_tmpl_props ~id ~app_bar
      ~post_scripts:[Src "/js/mosaic_editor.js"]
      ~stylesheets:["/css/pipeline.min.css"]
      () in
  Simple { id
         ; title = "Редактор"
         ; icon = Some (Html.toelt
                        @@ make_icon Components_tyxml.Svg_icons.view_dashboard)
         ; href = Common.Uri.Path.of_string "editor"
         ; template }
