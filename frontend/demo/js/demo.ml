open Js_of_ocaml
open Components

let log (x : 'a) : unit =
  Js.Unsafe.global##.console##log x

let onload _ =
  let root = Dom_html.getElementById "root" in
  let slider = Slider.make ~step:5. ~discrete:true ~markers:true () in
  let div = Widget.create_div ~widgets:[slider] () in
  div#add_class "slider-wrapper";
  let page = Scaffold.attach root in
  page#set_body div;
  Js._false

let () =
  Dom_html.addEventListener Dom_html.document
    Dom_events.Typ.domContentLoaded
    (Dom_html.handler onload)
    Js._false
  |> ignore
