open Common.User
open Api.Template
open Common.Uri

module Icon = Components_tyxml.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let create () : upper ordered_item list user_table =
  let id = "user-settings" in
  let props =
    make_tmpl_props ~id
      ~app_bar:(make_app_bar_props ~title:"Пользователи" ())
      ~post_scripts:[Src "/js/settings_user.js"]
      ~stylesheets:["/css/user.min.css"]
      () in
  let icon x =
    let open Icon.SVG in
    let path = create_path x () in
    let icon = create [path] () in
    Tyxml.Html.toelt icon in
  let user_pages =
    [`Index 10,
     Simple { id
            ; title = "Пользователи"
            ; icon = Some (icon Components_tyxml.Svg_icons.account_settings_variant)

            ; href = Path.of_string "user"
            ; template = props }] in
  { root = [ `Index 5,
             Subtree { title = "Настройки"
                     ; icon = Some (icon Components_tyxml.Svg_icons.settings)
                     ; href = Path.of_string "settings"
                     ; templates = user_pages } ]
  ; operator = []
  ; guest    = []
  }
