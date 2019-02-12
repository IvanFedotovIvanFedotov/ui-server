open Common.User
open Api.Template

let create_subtree () =
  Subtree { title = "Мозаика"
          ; icon = None
          ; href = Common.Uri.Path.of_string "mosaic"
          ; templates =
              [ (`Index 1, Page_mosaic_video_template.create ())
              ; (`Index 2, Page_mosaic_editor_template.create ()) ]}

let create () : upper ordered_item list user_table =
  let rval = [`Index 3, create_subtree ()] in
  { root = rval; operator = rval; guest = rval }
