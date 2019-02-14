open Containers
open Components
open Common
open Widget_common
open Board_types

type config =
  { stream : Stream.t
  } [@@deriving yojson]

module Settings = struct
  type t = { hex : bool } [@@deriving eq]

  let (default : t) = { hex = false }

  let make_hex_swith state =
    let input = new Switch.t ~state () in
    new Form_field.t ~input ~align_end:true ~label:"HEX IDs" ()

  class view ?(settings = default) () =
    let hex_switch = make_hex_swith settings.hex in
    object
      val mutable value = settings
      inherit Vbox.t ~widgets:[hex_switch] ()
      method value : t = value
      method apply () : unit =
        let hex = hex_switch#input_widget#checked in
        value <- { hex }
      method reset () : unit =
        let { hex } = value in
        hex_switch#input_widget#set_checked hex
    end

  let make ?settings () = new view ?settings ()

end

module Pid_info = struct

  include Pid

  let compare (a : t) (b : t) : int =
    Int.compare (fst a) (fst b)
end

module Set = Set.Make(Pid_info)

let base_class = "qos-niit-pids-summary"
let no_sync_class = CSS.add_modifier base_class "no-sync"
let no_response_class = CSS.add_modifier base_class "no-response"

(* TODO improve tooltip. add pid type, bitrate units *)
module Pie = struct

  let other = "Другие"

  let colors =
    let open Material_color_palette in
    [| Red C500
     ; Orange C500
     ; Green C500
     ; Blue C500
     ; Purple C500
     ; Grey C500
     ; Brown C500
     ; Pink C500
     ; Blue_grey C500
     ; Deep_purple C500
     ; Deep_orange C500
     ; Indigo C500
     ; Amber C500
     ; Light_blue C500
    |]

  let make_pie_datalabels () : Chartjs_datalabels.t =
    let open Chartjs in
    let open Chartjs_datalabels in
    let color = fun context ->
      let open Option_types.Option_context in
      let index = data_index context in
      let color = colors.(index) in
      Color.to_css_rgba
      @@ Color.text_color
      @@ Material_color_palette.make color in
    let display = fun context ->
      let open Option_types.Option_context in
      let index = data_index context in
      let value = Js_array.Float.(
          let data = Pie.Dataset.Float.data (dataset context) in
          let sum = reduce' data (fun acc x _ _ -> acc +. x) in
          let v = data.%[index] in
          (v *. 100.) /. sum) in
      value >. 5. in
    let font = Font.make ~weight:"bold" () in
    make
      ~formatter:(fun _ context ->
        let open Chartjs.Option_types.Option_context in
        let index = data_index context in
        let data = data (chart context) in
        Js_array.String.((Data.labels data).%[index]))
      ~color:(`Fun color)
      ~display:(`Fun display)
      ~align:(`Single `Start)
      ~offset:(`Single (-50))
      ~clip:(`Single true)
      ~font:(`Single font)
      ()

  let make_pie_options () : Chartjs.Options.t =
    let open Chartjs in
    let hover = Options.Hover.make ~animation_duration:0 () in
    let callbacks =
      Options.Tooltips.Callbacks.make
        ~label:(fun item data ->
          let ds_index = item.dataset_index in
          Js_array.(
            let dataset = Data.Datasets.((Data.datasets data).%[ds_index]) in
            let values = Pie.Dataset.Float.data dataset in
            let value = Float.(values.%[item.index]) in
            let label = match String.((Data.labels data).%[item.index]) with
              | s when Equal.string s other -> s
              | s -> Printf.sprintf "PID %s" s in
            Printf.sprintf "%s: %.3g Мбит/с" label value))
        () in
    let tooltips =
      Options.Tooltips.make
        ~callbacks
        () in
    let legend =
      Options.Legend.make
        ~position:`Left
        ~display:false
        () in
    let animation = Options.Animation.make () in
    Pie.Options.Animation.set_animate_rotate animation false;
    let datalabels = make_pie_datalabels () in
    let plugins = Options.Plugins.make () in
    Chartjs_datalabels.Per_chart.set plugins (Some datalabels);
    Options.make
      ~responsive:true
      ~maintain_aspect_ratio:true
      ~aspect_ratio:1.0
      ~animation
      ~tooltips
      ~legend
      ~hover
      ~plugins
      ()

  let make_pie_dataset () : Chartjs.Pie.Dataset.Float.t =
    let open Chartjs.Pie in
    let background_color =
      Array.map Fun.(Color.to_css_rgba % Material_color_palette.make) colors
      |> Array.to_list in
    Dataset.Float.make
      ~background_color
      ~border_color:background_color
      ~data:[]
      ()

  let make_pie () =
    let open Chartjs in
    let dataset = make_pie_dataset () in
    let options = make_pie_options () in
    let data = Data.make ~datasets:[] ~labels:[] () in
    let node = `Canvas Js_of_ocaml.Dom_html.(createCanvas document) in
    let chart = make ~options ~data `Pie node in
    chart, dataset

  class t ?(hex = false) () =
    let _class = CSS.add_element base_class "pie" in
    let box_class = CSS.add_element _class "wrapper" in
    let title_class = CSS.add_element _class "title" in
    let text = "Битрейт" in
    let title = new Typography.Text.t ~font:Caption ~text () in
    let box = Widget.create_div () in
    let pie, dataset = make_pie () in
    object(self)

      val mutable _hex = hex
      val mutable _rate = None

      inherit Widget.t Js_of_ocaml.Dom_html.(createDiv document) () as super

      method! init () : unit =
        super#init ();
        box#add_class box_class;
        box#append_child @@ Widget.create @@ Obj.magic @@ Chartjs.canvas pie;
        title#add_class title_class;
        self#set_rate None;
        self#add_class _class;
        self#append_child title;
        self#append_child box;

      method! destroy () : unit =
        super#destroy ();
        title#destroy ();
        box#destroy ();
        Chartjs.destroy pie

      method set_hex (x : bool) : unit =
        _hex <- x;
        match _rate with
        | None -> ()
        | Some (pids, oth) ->
           let open Chartjs in
           let data = data pie in
           Data.set_labels data (self#make_labels pids oth);
           update pie None

      method set_rate : Bitrate.t option -> unit = function
        | None ->
           let open Chartjs in
           let data = data pie in
           Data.set_datasets data [];
           _rate <- None;
           update pie None
        | Some { total; pids; _ } ->
           let open Chartjs in
           let pids = List.sort (fun a b -> compare (fst a) (fst b)) pids in
           let data = data pie in
           let br =
             List.fold_left (fun acc (pid, br) ->
                 let open Float in
                 let pct = 100. * (of_int br) / (of_int total) in
                 let br = (of_int br) / 1_000_000. in
                 (pid, (br, pct)) :: acc) [] pids in
           let pids, oth =
             List.fold_left (fun (pids, oth) (pid, (br, pct)) ->
                 if pct >. 1. then (pid, br) :: pids, oth
                 else pids, br :: oth) ([], []) br in
           if Option.is_none _rate
           then Data.set_datasets data [dataset];
           _rate <- Some (pids, oth);
           let data' =
             let pids = List.map snd pids in
             match oth with
             | [] -> pids
             | l  -> pids @ [List.fold_left (+.) 0. l] in
           ignore data';
           Data.set_labels data (self#make_labels pids oth);
           Pie.Dataset.Float.set_data dataset data';
           update pie None

      (* Private methods *)

      method private make_labels pids oth =
        let to_string =
          if _hex then PID.to_hex_string
          else PID.to_dec_string in
        let pids = List.map Fun.(to_string % fst) pids in
        match oth with
        | [] -> pids
        | _  -> pids @ [other]

    end

end

(* TODO show only N pids, and allow user to expand if necessary
   TODO sort pids by value *)
module Info = struct

  module Rate = struct

    let base_class = "qos-niit-pids-summary-rate"

    let make () =
      let na = "n/a" in
      let to_string = Printf.sprintf "%f Мбит/с" in
      let set meta = function
        | None -> meta#set_text na
        | Some x -> meta#set_text @@ to_string x in
      let total, set_total =
        let meta = new Typography.Text.t ~text:na () in
        let item = new Item_list.Item.t
                     ~text:"Общий битрейт: "
                     ~value:None
                     ~meta
                     () in
        item, set meta in
      let effective, set_effective =
        let meta = new Typography.Text.t ~text:na () in
        let item = new Item_list.Item.t
                     ~text:"Полезный битрейт: "
                     ~value:None
                     ~meta
                     () in
        item, set meta in
      let list =
        new Item_list.t
          ~dense:true
          ~non_interactive:true
          ~items:[`Item total; `Item effective]
          () in
      list#add_class base_class;
      list, set_total, set_effective

  end

  module Pids = struct

    let base_class = "qos-niit-pids-summary-box"
    let content_class = CSS.add_element base_class "content"
    let wrapper_class = CSS.add_element base_class "wrapper"
    let pid_class = CSS.add_element base_class "pid"
    let title_class = CSS.add_element base_class "title"
    let lost_class = CSS.add_modifier pid_class "lost"

    let make_title num =
      Printf.sprintf "PIDs (%d)" num

    let make_nav_button dir =
      let path = match dir with
        | `Left -> Icon.SVG.Path.chevron_left
        | `Right -> Icon.SVG.Path.chevron_right in
      let icon = Icon.SVG.make_simple path in
      let button = new Button.t ~label:"" ~icon () in
      button

    let make_pid ?(hex = false) ((pid, info) : Pid.t) =
      object(self)
        inherit Widget.t Js_of_ocaml.Dom_html.(createSpan document) () as super

        method! init () : unit  =
          super#init ();
          super#add_class pid_class;
          self#update info;
          self#set_hex hex

        method update (info : Pid.info) : unit =
          super#toggle_class ~force:(not info.present) lost_class

        method set_hex (x : bool) : unit =
          let s = match x with
            | true -> PID.to_hex_string self#pid
            | false -> PID.to_dec_string self#pid in
          super#set_text_content s

        method pid : int = pid

      end

    class t ?hex (init : Pid.t list) () =
      let text = make_title @@ List.length init in
      let title = new Typography.Text.t ~font:Caption ~text () in
      let content = Widget.create_div () in
      let wrapper = new Hbox.t ~widgets:[content] () in
      object(self)

        val mutable _ext = false
        val mutable _pids = []

        inherit Vbox.t ~widgets:[title#widget; wrapper#widget] () as super

        method! init () : unit =
          super#init ();
          _pids <- List.map (make_pid ?hex) init;
          let obs =
            Ui_templates.Resize_observer.observe
              ~node:content#root
              ~f:self#observe
              () in
          ignore obs;
          List.iter content#append_child _pids;
          self#add_class base_class;
          title#add_class title_class;
          content#add_class content_class;
          wrapper#add_class wrapper_class

        method! destroy () : unit =
          super#destroy ();
          title#destroy ();
          content#destroy ();
          wrapper#destroy ()

        method update ~(lost : Set.t)
                 ~(found : Set.t)
                 ~(changed : Set.t) : unit =
          Set.iter self#remove_pid lost;
          Set.iter self#update_pid changed;
          Set.iter self#add_pid found;
          title#set_text @@ make_title @@ List.length _pids;

        method set_hex (x : bool) : unit =
          List.iter (fun cell -> cell#set_hex x) _pids

        (* Private methods *)

        method private observe =
          let open Js_of_ocaml in
          let open Ui_templates.Resize_observer in
          fun (entries : resizeEntry Js.t Js.js_array Js.t) ->
          let l = Array.to_list @@ Js.to_array entries in
          List.find_map (fun (x : resizeEntry Js.t) ->
              if Equal.physical x##.target content#node
              then Some x##.contentRect else None) l
          |> function
            | None -> ()
            | Some x ->
               let height = Js.Optdef.get x##.height (fun () -> 0.) in
               if not _ext && height >. 160.
               then (
                 _ext <- true;
                 print_endline "in setting ext";
                 let left = make_nav_button `Left in
                 let right = make_nav_button `Right in
                 wrapper#insert_child_at_idx 0 left;
                 wrapper#append_child right);
               Printf.printf "height: %f\n" height

        method private update_pid ((pid, info) : Pid.t) : unit =
          match List.find_opt (fun cell ->
                    cell#pid = pid) _pids with
          | None -> ()
          | Some cell -> cell#update info

        method private add_pid (x : Pid.t) : unit =
          let pid = make_pid x in
          _pids <- pid :: _pids;
          (* FIXME sort? *)
          content#append_child pid

        method private remove_pid ((pid, _) : Pid.t) : unit =
          match List.find_opt (fun cell ->
                    cell#pid = pid) _pids with
          | None -> ()
          | Some cell ->
             content#remove_child cell;
             cell#destroy ();
             _pids <- List.remove ~eq:Widget.equal cell _pids

      end

  end

  let base_class = "qos-niit-pids-summary-info"

  class t ?hex (init : Pid.t list) () =
    let rate, set_total, set_effective = Rate.make () in
    let pids = new Pids.t ?hex init () in
    object(self)

      val mutable _pids = []

      inherit Vbox.t
                ~widgets:[ rate#widget
                         ; (new Divider.t ())#widget
                         ; pids#widget ] () as super

      method! init () : unit =
        super#init ();
        self#add_class base_class

      method! destroy () : unit =
        super#destroy ();
        pids#destroy ();
        rate#destroy ()

      method set_rate (rate : Bitrate.t option) =
        match rate with
        | None -> set_total None; set_effective None
        | Some x ->
           let null = List.Assoc.get ~eq:(=) 0x1FFF x.pids
                      |> Option.get_or ~default:0 in
           let e = x.total - null in
           let e = Float.(of_int e / 1_000_000.) in
           let v = Float.(of_int x.total / 1_000_000.) in
           set_total (Some v);
           set_effective (Some e);

      method update ~(lost : Set.t)
               ~(found : Set.t)
               ~(changed : Set.t) : unit =
        pids#update ~lost ~found ~changed;

      method set_hex (x : bool) : unit =
        pids#set_hex x

  end

end

class t ?(settings : Settings.t option)
        (init : Pid.t list Time.timestamped option) () =
  let init, timestamp = match init with
    | None -> [], None
    | Some { data; timestamp } -> data, Some timestamp in
  let s_time, set_time =
    React.S.create ~eq:(Equal.option Time.equal) timestamp in
  let pie = new Pie.t () in
  let info = new Info.t init () in
  object(self)

    val mutable _data : Set.t = Set.of_list init

    inherit Widget.t Js_of_ocaml.Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      Option.iter self#set_settings settings;
      self#add_class base_class;
      self#append_child pie;
      self#append_child info

    method! destroy () : unit =
      super#destroy ();
      pie#destroy ();
      info#destroy ();
      React.S.stop ~strong:true s_time;

    method s_timestamp : Time.t option React.signal =
      s_time

    method update ({ timestamp; data } : Pid.t list Time.timestamped) =
      (* Update timestamp *)
      set_time @@ Some timestamp;
      (* Manage found, lost and updated items *)
      let prev = _data in
      _data <- Set.of_list data;
      let lost = Set.diff prev _data in
      let found = Set.diff _data prev in
      let inter = Set.inter prev _data in
      let upd =
        Set.filter (fun ((_, info) : Pid.t) ->
            List.mem ~eq:Pid.equal_info info @@ List.map snd data) inter in
      info#update ~lost ~found ~changed:upd

    (** Updates widget state *)
    method set_state = function
      | Fine ->
         self#remove_class no_response_class;
         self#remove_class no_sync_class
      | No_sync ->
         self#remove_class no_response_class;
         self#add_class no_sync_class
      | No_response ->
         self#remove_class no_sync_class;
         self#add_class no_response_class

    method set_hex (x : bool) : unit =
      pie#set_hex x;
      info#set_hex x

    method set_rate (x : Bitrate.t option) =
      info#set_rate x;
      pie#set_rate x

    method set_settings (x : Settings.t) : unit =
      self#set_hex x.hex

  end

let make ?(settings : Settings.t option)
      (init : Pid.t list Time.timestamped option) =
  new t ?settings init ()

let make_dashboard_item ?settings init : 'a Dashboard.Item.item =
  let w = make ?settings init in
  let settings = Settings.make ?settings () in
  let (settings : Dashboard.Item.settings) =
    Dashboard.Item.make_settings
      ~widget:settings
      ~set:(fun () -> Lwt_result.return @@ w#set_settings settings#value)
      () in
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let timestamp =
    Dashboard.Item.make_timestamp
      ~time:w#s_timestamp
      ~to_string:(Time.to_human_string ?tz_offset_s)
      () in
  Dashboard.Item.make_item
    ~name:"Сводка PID"
    ~subtitle:(Timestamp timestamp)
    ~settings
    w
