open Js_of_ocaml
open Containers
open Components

let log (x : 'a) : unit =
  Js.Unsafe.global##.console##log x

let time name : unit =
  Js.Unsafe.global##.console##time (Js.string name)

let time_end name : unit =
  Js.Unsafe.global##.console##timeEnd (Js.string name)

let table_demo timestamp count =
  print_endline "started building table...";
  let fmt =
    let open Table in
    let open Format in
    (to_column ~sortable:true "Date",     Time None)
    :: (to_column ~sortable:true "Input",    String None)
    :: (to_column ~sortable:true "Service",  String None)
    :: (to_column ~sortable:true "PID",      Int None)
    :: (to_column ~sortable:true "Severity", Option (String None,""))
    :: (to_column ~sortable:true "Check",    String None)
    :: (to_column "Message",                 String None)
    :: [] in
  let table =
    new Table.t
      ~scroll_target:Clusterize.Scroll_target.window
      ~sticky_header:true
      ~clusterize:true
      ~dense:true
      ~fmt
      () in
  let channels = [|"BBC"; "CNN"; "MTV"; "AnimalPlanet"|] in
  let err = [|"1.3.1 PAT error"; "1.4. Continuity count error"|] in
  let make_data index =
    let ch  = channels.(index mod 4) in
    let inp = Ipaddr.V4.make 224 1 2 (index mod 255) in
    let err = err.(index mod 2) in
    Table.Data.(
      timestamp
      :: (Ipaddr.V4.to_string inp)
      :: ch
      :: index
      :: (Some "Warning")
      :: err
      :: "Error description here"
      :: []) in
  let data = List.map make_data @@ List.range' 0 count in
  table#append data;
  table

let onload _ =
  let timestamp = Ptime_clock.now () in
  let page = new Ui_templates.Page.t (`Static []) () in
  let text = new Textfield.t ~input_type:(Integer (None, None)) () in
  let btn = new Button.t ~label:"Refresh" () in
  let div = Widget.create_div ~widgets:[text#widget; btn#widget] () in
  let box = Widget.create_div () in
  page#arbitrary#append_child div;
  page#arbitrary#append_child box;
  btn#listen_click_lwt (fun _ _ ->
      box#set_empty ();
      let num = Option.get_or ~default:500 @@ React.S.value text#s_input in
      let table = table_demo timestamp num in
      box#append_child table;
      Lwt.return_unit)
  |> Lwt.ignore_result;

  Js._false

let () =
  Dom_html.addEventListener Dom_html.document
    Dom_events.Typ.domContentLoaded
    (Dom_html.handler onload)
    Js._false
  |> ignore
