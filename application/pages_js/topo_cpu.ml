open Components
open Application_types
open Topo_types

let base_class = "topology__cpu"

let get_cpu_name (cpu : Topology.topo_cpu) = match cpu.process with
  | "pipeline" -> "Анализатор QoE"
  | s -> Printf.sprintf "Неизвестный модуль: %s" s

let make_cpu_page socket (cpu : Topology.topo_cpu) =
  match cpu.process with
  | "pipeline" ->
     let getter () =
       Topo_pipeline.make cpu socket
       |> Ui_templates.Loader.create_widget_loader
       |> Widget.coerce in
     Some getter
  | _ -> None

module Header = struct

  class t (has_settings_button : bool)
          (cpu : Topology.topo_cpu) () =
    let _class = BEM.add_element base_class "header" in
    let title = get_cpu_name cpu in
    let settings = match has_settings_button with
      | false -> None
      | true ->
         let icon = Icon.SVG.(make_simple Path.settings) in
         let button = Icon_button.make ~icon () in
         Some button in
    object(self)
      inherit Topo_block.Header.t ?action:settings ~title () as super

      method settings_icon = settings

      method! init () : unit =
        super#init ();
        self#add_class _class

      method! layout () : unit =
        super#layout ();
        Utils.Option.iter Widget.layout self#settings_icon

    end

  let create (has_settings_button : bool)
        (cpu : Topology.topo_cpu) =
    new t has_settings_button cpu ()

end

module Body = struct

  class t (cpu : Topology.topo_cpu) () =
  object
    inherit Topo_block.Body.t (List.length cpu.ifaces) ()
  end

  let create (cpu : Topology.topo_cpu) =
    new t cpu ()

end

class t ~(connections : (#Topo_node.t * connection_point) list)
    (socket : Api_js.Websocket.JSON.t)
    (cpu : Topology.topo_cpu)=
  let e_settings, push_settings = React.E.create () in
  let make_settings = make_cpu_page socket cpu in
  let header = Header.create (Utils.Option.is_some make_settings) cpu in
  let body = Body.create cpu in
  let port_setter = fun _ _ ->
    Lwt_result.fail "CPU ports are not switchable" in
  object(self)

    val mutable _click_listener = None

    inherit Topo_block.t
              ~port_setter
              ~node:(`CPU cpu)
              ~connections
              ~header
              ~body
              () as super

    method! init () : unit =
      super#init ();
      List.iter (fun p -> p#set_state `Active) self#paths;
      self#add_class base_class;
      self#set_attribute "data-cpu" cpu.process;
      Utils.Option.iter (fun (w : #Widget.t) ->
          let listener =
            Events.clicks w#root (fun _ _ ->
                let name = get_cpu_name self#cpu in
                let widget = self#make_settings_widget () in
                push_settings (widget, name);
                Lwt.return_unit) in
          _click_listener <- Some listener)
        header#settings_icon

    method! destroy () : unit =
      super#destroy ();
      Utils.Option.iter Lwt.cancel _click_listener;
      _click_listener <- None

    method! layout () : unit =
      super#layout ();
      header#layout ()

    method settings_event : (Widget.t * string) React.event =
      e_settings

    method cpu = cpu

    (* Private methods *)

    method private make_settings_widget () : Widget.t =
      match make_settings with
      | None ->
         let icon = Icon.SVG.(make_simple Path.stop) in
         let ph =
           Ui_templates.Placeholder.With_icon.make
             ~icon
             ~text:"Нет доступных настроек для модуля"
             () in
         ph#widget
      | Some make -> make ()

  end

let create ~connections socket (cpu : Topology.topo_cpu) =
  new t ~connections socket cpu
