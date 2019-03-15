open Js_of_ocaml

let log (x : 'a) : unit =
  Js.Unsafe.global##.console##log x

let onload _ =
  let root = Dom_html.getElementById "root" in
  let page = Components.Scaffold.attach root in
  let snackbar = Snackbar.section () in
  let slider = Slider.section () in
  let div =
    Components.Widget.create_div
      ~widgets:[snackbar#widget; slider#widget] () in
  page#set_body div;
  Js._false

let () =
  Dom_html.addEventListener Dom_html.document
    Dom_events.Typ.domContentLoaded
    (Dom_html.handler onload)
    Js._false
  |> ignore
