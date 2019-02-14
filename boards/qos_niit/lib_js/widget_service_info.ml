open Containers
open Components
open Common
open Board_types
open Widget_common

let base_class = "qos-niit-service-info"
let no_sync_class = CSS.add_modifier base_class "no-sync"
let no_response_class = CSS.add_modifier base_class "no-response"
let not_available_class = CSS.add_modifier base_class "not-available"

module Settings = struct
  type t = { hex : bool } [@@deriving eq]

  let (default : t) = { hex = false }

  let make_hex_switch state =
    let input = new Switch.t ~state () in
    new Form_field.t ~input ~align_end:true ~label:"HEX IDs" ()

  class view ?(settings = default) () =
    let hex_switch = make_hex_switch settings.hex in
    object
      val mutable value = settings
      inherit Vbox.t ~widgets:[hex_switch] ()
      method value = value
      method apply () : unit =
        let hex = hex_switch#input_widget#checked in
        value <- { hex }
      method reset () : unit =
        let { hex } = value in
        hex_switch#input_widget#set_checked hex
    end

  let make ?settings () = new view ?settings ()
end

let sum_bitrate : (int * int) list -> int =
  List.fold_left (fun acc (_, x) -> acc + x) 0

let make_list_title title =
  let text = new Typography.Text.t ~text:title () in
  text#add_class "mdc-list-title";
  text

let make_general_info () =
  let make_item title =
    let open Item_list in
    let meta = new Typography.Text.t ~text:"" () in
    let item = new Item.t ~text:title ~value:() ~meta () in
    item, fun ?(hex = false) x ->
          let s = match hex with
            | false -> Printf.sprintf "%04d" x
            | true -> Printf.sprintf "0x%04X" x in
          meta#set_text s in
  let make_br_item title =
    let open Item_list in
    let meta = new Typography.Text.t ~text:"" () in
    let item = new Item.t ~text:title ~value:() ~meta () in
    item, function
    | Some x -> let x = Float.(of_int x /. 1_000_000.) in
                let s = Printf.sprintf "%.2f Мбит/с" x in
                meta#set_text s
    | None   -> meta#set_text "-" in
  let id, set_id = make_item "Service ID" in
  let pmt, set_pmt = make_item "PMT PID" in
  let pcr, set_pcr = make_item "PCR PID" in
  let rate, set_rate = make_br_item "Битрейт" in
  let min, set_min = make_br_item "Min" in
  let max, set_max = make_br_item "Max" in
  let list =
    new Item_list.t
      ~dense:true
      ~non_interactive:true
      ~items:[ `Item id
             ; `Item pmt
             ; `Item pcr
             ; `Item rate
             ; `Item min
             ; `Item max ] () in
  let set_info = fun ?hex ((id, info) : Service.t) ->
    set_id ?hex id;
    set_pmt ?hex info.pmt_pid;
    set_pcr ?hex info.pcr_pid in
  list, set_info, set_rate, set_min, set_max

let make_sdt_info () =
  let open Item_list in
  let ok_class = CSS.add_modifier "mdc-icon" "ok" in
  let err_class = CSS.add_modifier "mdc-icon" "error" in
  let name, set_name =
    let meta = new Typography.Text.t ~text:"" () in
    let item = new Item.t ~value:() ~text:"Имя" ~meta () in
    item, fun x -> meta#set_text x in
  let prov, set_prov =
    let meta = new Typography.Text.t ~text:"" () in
    let item = new Item.t ~value:() ~text:"Провайдер" ~meta () in
    item, fun x -> meta#set_text x in
  let typ, set_typ =
    let meta = new Typography.Text.t ~text:"" () in
    let item = new Item.t ~value:() ~text:"Тип" ~meta () in
    item, fun x -> meta#set_text @@ Mpeg_ts.service_type_to_string x in
  let eit_s, set_eit_s =
    let meta = Icon.SVG.(make_simple Path.check_circle) in
    let item = new Item.t ~value:() ~text:"EIT schedule" ~meta () in
    item, function
    | true ->
       meta#add_class ok_class;
       meta#path#set Icon.SVG.Path.check_circle
    | false ->
       meta#add_class err_class;
       meta#path#set Icon.SVG.Path.close_circle in
  let scr, set_scr =
    let meta = Icon.SVG.(make_simple Path.check_circle) in
    let item = new Item.t ~value:() ~text:"Скремблирование" ~meta () in
    item, function
    | true ->
       meta#add_class ok_class;
       meta#path#set Icon.SVG.Path.check_circle
    | false ->
       meta#add_class err_class;
       meta#path#set Icon.SVG.Path.close_circle in
  let eit_pf, set_eit_pf =
    let meta = Icon.SVG.(make_simple Path.check_circle) in
    let item = new Item.t ~value:() ~text:"EIT P/F" ~meta () in
    item, function
    | true ->
       meta#add_class ok_class;
       meta#path#set Icon.SVG.Path.check_circle
    | false ->
       meta#add_class err_class;
       meta#path#set Icon.SVG.Path.close_circle in
  let running_status, set_running_status =
    let meta = new Typography.Text.t ~text:"" () in
    let item = new Item.t ~value:() ~text:"Running status" ~meta () in
    item, fun x -> meta#set_text @@ Mpeg_ts.running_status_to_string x in
  let list =
    new Item_list.t
      ~dense:true
      ~non_interactive:true
      ~items:[ `Item name
             ; `Item prov
             ; `Item typ
             ; `Item eit_s
             ; `Item scr
             ; `Item eit_pf
             ; `Item running_status ]
      () in
  list, fun ((_, info) : Service.t) ->
        set_name info.name;
        set_prov info.provider_name;
        set_typ info.service_type;
        set_eit_s info.eit_schedule;
        set_scr info.free_ca_mode;
        set_eit_pf info.eit_pf;
        set_running_status info.running_status

let make_description () =
  let _class = CSS.add_element base_class "description" in
  let main, set_main, set_rate, set_min, set_max = make_general_info () in
  let sdt, set_sdt = make_sdt_info () in
  let main_title = new Card.Primary.title "Общая информация" () in
  let main_primary = new Card.Primary.t ~widgets:[ main_title ] () in
  let main_media = new Card.Media.t ~widgets:[ main ] () in
  let sdt_title = new Card.Primary.title "Информация из SDT" () in
  let sdt_primary = new Card.Primary.t ~widgets:[ sdt_title ] () in
  let sdt_media = new Card.Media.t ~widgets:[ sdt ] () in
  let main_box =
    new Card.t
      ~outlined:true
      ~widgets:[ main_primary#widget
               ; main_media#widget] () in
  let sdt_box =
    new Card.t
      ~outlined:true
      ~widgets:[ sdt_primary#widget
               ; sdt_media#widget ] () in
  let box = new Hbox.t ~wrap:`Wrap ~widgets:[main_box; sdt_box] () in
  box#add_class _class;
  let set = fun ?hex (x : Service.t) ->
    set_main ?hex x;
    set_sdt x in
  box#widget, set, set_rate, set_min, set_max

module Pids = struct

  let _class = CSS.add_element base_class "pids"

  let get_service_pids ((_, info) : Service.t) =
    let pmt = if info.has_pmt then Some info.pmt_pid else None in
    List.cons_maybe pmt info.elements
    |> List.sort_uniq ~cmp:compare

  let filter_pids service (pids : Pid.t list) =
  let service_pids = get_service_pids service in
      List.filter (fun ((pid, _) : Pid.t) ->
          List.mem ~eq:(=) pid service_pids)
        pids

  class t ?(settings : Settings.t option)
          (service : Service.t)
          (init : Pid.t list Time.timestamped option)
          () =
    let init = match init with
      | None -> None
      | Some x -> Some { x with data = filter_pids service x.data } in
    let (settings : Widget_pids_overview.Settings.t option) =
      match settings with
      | None -> None
      | Some { hex } -> Some { hex } in
    object(self)
      val mutable _service : Service.t = service

      inherit Widget_pids_overview.t ?settings init () as super

      method! init () : unit =
        super#init ();
        super#add_class _class

      method! update (pids : Pid.t list Time.timestamped) =
        super#update { pids with data = filter_pids _service pids.data }

      method update_service (service : Service.t) =
        _service <- service;
        let data = filter_pids service self#pids in
        self#update { data; timestamp = Ptime_clock.now () }
    end

end

class t ?(settings : Settings.t option)
        ?(rate : Bitrate.t option)
        ?min ?max
        (init : Service.t)
        (pids : Pid.t list Time.timestamped option)
        () =
  let info, set_info, set_rate, set_min, set_max = make_description () in
  let pids = new Pids.t ?settings init pids () in
  let table = pids in
  let tabs =
    let info_icon =
      Icon.SVG.(make_simple Path.file_document_box_outline) in
    let list_icon =
      Icon.SVG.(make_simple Path.view_list) in
    [ new Tab.t ~content:(Both ("Описание", info_icon)) ~value:info ()
    ; new Tab.t ~content:(Both ("PIDs", list_icon)) ~value:table#widget () ] in
  let div = Widget.create_div () in
  let bar, _ = Ui_templates.Tabs.create_simple ~body:div tabs in
  object(self)
    inherit Vbox.t ~widgets:[ bar#widget
                            ; (new Divider.t ())#widget
                            ; div ] () as super
    val mutable _settings = Option.get_or ~default:Settings.default settings
    val mutable _info = init
    val mutable _min = Option.map sum_bitrate min
    val mutable _max = Option.map sum_bitrate max

    method! init () : unit =
      super#init ();
      self#update _info;
      pids#set_rate rate;
      set_rate @@ Option.map (fun (x : Bitrate.t) -> sum_bitrate x.pids) rate;
      set_min _min;
      set_max _max;

    method info : Service.t =
      _info

    method service_id : int =
      fst _info

    method not_available : bool =
      super#has_class not_available_class

    method set_not_available (x : bool) : unit =
      super#toggle_class ~force:x not_available_class

    (** Updates widget state *)
    method set_state = function
      | Fine ->
         super#remove_class no_response_class;
         super#remove_class no_sync_class
      | No_sync ->
         super#remove_class no_response_class;
         super#add_class no_sync_class
      | No_response ->
         super#remove_class no_sync_class;
         super#add_class no_response_class

    method settings : Settings.t =
      _settings

    method set_settings ({ hex } : Settings.t) : unit =
      let pids_settings = Widget_pids_overview.Settings.{ hex } in
      set_info ~hex _info;
      pids#set_settings pids_settings

    (** Updates the description *)
    method update (x : Service.t) =
      _info <- x;
      pids#update_service x;
      set_info ~hex:_settings.hex x

    method update_pids (x : Pid.t list Time.timestamped) : unit =
      pids#update x

    (** Updates bitrate values *)
    method set_rate : Bitrate.t option -> unit = function
      | None ->
         self#_set_max None;
         self#_set_min None;
         set_rate None;
         pids#set_rate None;
      | Some rate ->
         let sum = sum_bitrate rate.pids in
         let rate = { rate with total = sum } in
         pids#set_rate @@ Some rate;
         self#_set_max @@ Some sum;
         self#_set_min @@ Some sum;
         set_rate @@ Some sum

    (* Private methods *)

    method private set_hex (x : bool) : unit =
      set_info ~hex:x _info

    method private _set_min x =
      let eq = match _min, x with
        | None, None     -> true
        | None, Some _   -> false
        | Some p, Some c -> p <= c
        | Some _, None   -> false in
      if not eq then (_min <- x; set_min x)

    method private _set_max x =
      let eq = match _max, x with
        | None, None     -> true
        | None, Some _   -> false
        | Some p, Some c -> p >= c
        | Some _, None   -> false in
      if not eq then (_max <- x; set_max x)
  end

let make ?rate ?min ?max ?settings
      (init : Service.t)
      (pids : Pid.t list Time.timestamped option) =
  new t ?rate ?min ?max ?settings init pids ()
