open Containers
open Api.Template
open Common

module Icon = Components_tyxml.Icon.Make(Tyxml.Xml)(Tyxml.Svg)(Tyxml.Html)

let make_icon ?rotate path =
  let open Icon.SVG in
  let attrs = match rotate with
    | None -> None
    | Some x ->
       Printf.sprintf "transform: rotate(%ddeg)" x
       |> Tyxml.Svg.a_style
       |> List.pure
       |> Option.return in
  let path = create_path path () in
  let icon = create ?attrs [path] () in
  Tyxml.Html.toelt icon

let get_input_href (x : Topology.topo_input) =
  let name = Topology.input_to_string x.input in
  let id = string_of_int x.id in
  Filename.concat name id

let input topo (input : Topology.topo_input) =
  let path =
    List.find_map (fun (i, p, c) ->
        if Topology.equal_topo_input i input
        then Some (p, c) else None)
      (Topology.get_paths topo) in
  match path with
  | None -> failwith "input not found"
  | Some (boards, cpu) ->
     let title = Topology.get_input_name input in
     let boards =
       List.map (fun (x : Topology.topo_board) -> x.control, x.typ) boards
       |> Topology.boards_to_yojson
       |> Yojson.Safe.to_string in
     let cpu =
       Option.map (fun (x : Topology.topo_cpu) -> x.process) cpu
       |> Json.Option.to_yojson Topology.process_type_to_yojson
       |> Yojson.Safe.to_string in
     let input_string = Topology.Show_topo_input.to_string input in
     let input_template =
       make_tmpl_props ~id:input_string
         ~app_bar:(make_app_bar_props ~title ())
         ~pre_scripts:[Raw (Printf.sprintf "var input = \"%s\";\
                                            var boards = %s;\
                                            var cpu = %s;"
                              input_string boards cpu)]
         ~post_scripts:[Src "/js/input.js"]
         () in
     let input_page =
       `Index input.id,
       Simple { id = input_string
              ; title
              ; icon = Some (make_icon Icon.SVG.Path.arrow_right_box)
              ; href = Uri.Path.of_string @@ get_input_href input
              ; template = input_template } in
     let pre = "input/" ^ get_input_href input in
     let stream_template =
       make_tmpl_props ~id:input_string
         ~app_bar:(make_app_bar_props ~title:("Входы / " ^ title) ())
         ~pre_scripts:[ Raw (Printf.sprintf "var input = \"%s\";\
                                             var boards = %s;\
                                             var cpu = %s;"
                               input_string boards cpu)
                      ; Src "/js/moment.min.js"
                      ; Src "/js/Chart.min.js"
                      ; Src "/js/chartjs-plugin-streaming.min.js"
                      ; Src "/js/chartjs-plugin-datalabels.min.js" ]
         ~post_scripts:[Src "/js/stream.js"]
         () in
     let stream_page =
       `Index input.id,
       Pure { path = Uri.Path.Format.(pre @/ Stream.ID.fmt ^/ empty)
            ; template = stream_template } in
     input_page, stream_page

let create_topology () =
  let id = "topology" in
  let icon = make_icon ~rotate:90 Icon.SVG.Path.sitemap in
  let template =
    make_tmpl_props ~id
      ~app_bar:(make_app_bar_props ~title:"Конфигурация" ())
      ~post_scripts:[ Src "/js/ResizeObserver.js"
                    ; Src "js/topo.js" ]
      ~stylesheets:["/css/topology.min.css"]
      () in
  Simple { id
         ; title = "Конфигурация"
         ; icon = Some icon
         ; href = Uri.Path.of_string "application"
         ; template }

let create_demo () =
  let path = Uri.Path.Format.("demo" @/ empty) in
  let template =
    make_tmpl_props
      ~app_bar:(make_app_bar_props ~title:"UI Демо" ())
      ~pre_scripts:[ Src "/js/moment.min.js"
                   ; Src "/js/Chart.min.js" ]
      ~post_scripts:[Src "/js/demo.js"]
      ~stylesheets:["/css/demo.min.css"]
      () in
  Pure { path; template }

let create (app : Application.t)
    : upper ordered_item list User.user_table =
  let topo = React.S.value app.topo in
  let hw_templates =
    Hardware.Map.fold (fun _ (x : Boards.Board.t) acc ->
        List.cons_maybe x.templates acc) app.hw.boards [] in
  let inputs = Topology.get_inputs topo in
  let input_templates, stream_templates =
    List.map (input topo) inputs
    |> List.split in
  let app_template =
    [ `Index 2, create_topology ()
    ; `Index 3, create_demo ()
    ; `Index 4,
      Subtree { title = "Входы"
              ; icon = Some (make_icon Icon.SVG.Path.arrow_right_box)
              ; href = Uri.Path.of_string "input"
              ; templates = input_templates }
    ] in
  let proc = match app.proc with
    | None -> Common.User.empty_table
    | Some p -> p#template () in
  Common.User.concat_table
    ([ Responses.home_template ()
     ; User_template.create ()
     ; Pc_control.Network_template.create ()
     ; proc
     ; { root = app_template
       ; operator = app_template
       ; guest = app_template }
     ; { root = stream_templates
       ; operator = stream_templates
       ; guest = stream_templates }
     ]
     @ hw_templates)
