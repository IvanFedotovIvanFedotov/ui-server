open Components
open Widget_types
open Board_niitv_dvb_types
open Application_types

(* TODO
   1. Make data units configurable, if possible (e.g. for power chart)
 *)

module Point = struct
  open Chartjs.Types
  include Chartjs.Line.Dataset.Make_point(Time)(Float)
end

module Dataset = Chartjs.Line.Dataset.Make(Point)

type data = (Stream.ID.t * Point.t list) list

type init = (int * Measure.t ts list) list

type event = (int * Measure.t ts) list React.event

type widget_config =
  { sources : data_source list
  ; typ : measure_type
  ; duration : Time.Period.t
  ; settings : widget_settings option
  }
and widget_settings =
  { range : (float * float) option
  }
and data_source = Stream.ID.t [@@deriving yojson, eq]

let base_class = "dvb-niit-measures-line-chart"

let colors =
  Random.init 255;
  Array.init 100 (fun _ ->
      Random.int 255,
      Random.int 255,
      Random.int 255)

let get_suggested_range = function
  | `Power -> (-70.0, 0.0)
  | `Mer -> (0.0, 45.0)
  | `Ber -> (0.0, 0.00001)
  | `Freq -> (-10.0, 10.0)
  | `Bitrate -> (0.0, 1.0)

let make_x_axis ?(id = "x-axis") (config : widget_config)
    : Chartjs.Scales.t =
  let open Chartjs in
  let open Chartjs_streaming in
  let duration =
    int_of_float
    @@ Ptime.Span.to_float_s config.duration *. 1000. in
  let scale_label =
    Scales.Scale_label.make
      ~display:true
      ~label_string:"Время"
      () in
  let display_formats =
    Scales.Cartesian.Time.Time.Display_formats.make
      ~minute:"HH:mm:ss"
      ~second:"HH:mm:ss"
      ~hour:"HH:mm:ss"
      () in
  let time =
    Scales.Cartesian.Time.Time.make
      ~iso_weekday:true
      ~display_formats
      ~tooltip_format:"ll HH:mm:ss"
      () in
  let ticks =
    Scales.Cartesian.Time.Ticks.make
      ~auto_skip_padding:2
      () in
  let axis =
    Scales.Cartesian.Time.make
      ~id
      ~scale_label
      ~ticks
      ~time
      ~position:`Bottom
      ~type_:axis_type
      () in
  let streaming = make ~duration () in
  Per_axis.set axis streaming;
  axis

let format_value (v : float) (config : widget_config) : string =
  let unit = measure_type_to_unit config.typ in
  let v = match config.typ with
    | `Power -> Printf.sprintf "%g" v
    | `Ber -> Printf.sprintf "%0.3e" v
    | `Mer -> Printf.sprintf "%g" v
    | `Freq -> Printf.sprintf "%g" v
    | `Bitrate -> Printf.sprintf "%g" v in
  Printf.sprintf "%s %s" v unit

let make_options ~x_axes ~y_axes
      (config : widget_config) : Chartjs.Options.t =
  let open Chartjs in
  let scales = Scales.make ~x_axes ~y_axes () in
  let legend = Options.Legend.make ~display:false () in
  let plugins = Options.Plugins.make () in
  let callbacks =
    Options.Tooltips.Callbacks.make
      ~label:(fun item data ->
        let ds_index = item.dataset_index in
        let datasets = Data.datasets data in
        let dataset = Data.Datasets.(datasets.%[ds_index]) in
        let label = Dataset.label dataset in
        let data = Dataset.data dataset in
        let value = Dataset.Values.(data.%[item.index]) in
        Printf.sprintf "%s: %s" label (format_value value.y config))
      () in
  let tooltips =
    Options.Tooltips.make
      ~callbacks
      ~mode:`Nearest
      ~intersect:false
      () in
  Chartjs_datalabels.Per_chart.set plugins None;
  Options.make
    ~scales
    ~plugins
    ~legend
    ~tooltips
    ~maintain_aspect_ratio:false
    ~responsive:true
    ~responsive_animation_duration:0
    ()

let make_dataset id src (data : Point.t list) =
  let data = List.sort (fun (a : Point.t) b -> Ptime.compare a.x b.x) data in
  let label = Printf.sprintf "%s" module_name in
  let (r, g, b) = colors.(id) in
  let color = Color.to_hexstring @@ Color.of_rgb r g b in
  let ds =
    Dataset.make
      ~data
      ~label
      ~fill:`Off
      ~point_radius:(`Single 2)
      ~line_tension:0.
      ~background_color:color
      ~border_color:color
      ~cubic_interpolation_mode:`Monotone
      () in
  src, ds

let make_datasets (init : data)
      (sources : Stream.ID.t list) =
  let map id (src : Stream.ID.t) =
    let data =
      List.find_map (fun (src', data) ->
          if Stream.ID.equal src src'
          then Some data else None) init
      |> Option.get_or ~default:[] in
    make_dataset id src data in
  List.mapi map sources

let to_init (get : Measure.t -> float option)
      (init : init) : data =
  List.map (fun ((id : id), meas) ->
      let data =
        List.map (fun Time.{ data; timestamp } ->
            let y = Option.get_or ~default:nan (get data) in
            ({ x = timestamp; y } : Point.t)) meas in
      id.stream, data) init

let to_event (get : Measure.t -> float option)
      (event : event) : data React.event =
  React.E.map (
      List.map (fun ((id : id), Time.{ data; timestamp }) ->
          let y = Option.get_or ~default:nan (get data) in
          id.stream, List.return ({ x = timestamp; y } : Point.t)))
    event

let get_power (m : Measure.t) = m.power

let get_mer (m : Measure.t) = m.mer

let get_ber (m : Measure.t) = m.ber

let get_freq (m : Measure.t) = match m.freq with
  | None -> None
  | Some x -> Some (float_of_int x)

let get_bitrate (m : Measure.t) = match m.bitrate with
  | None -> None
  | Some x -> Some (float_of_int x /. 1_000_000.)

class t ~(init : init)
        ~(event : event)
        (widget_config : widget_config)
        () =
  let getter = match widget_config.typ with
    | `Power -> get_power
    | `Mer -> get_mer
    | `Ber -> get_ber
    | `Freq -> get_freq
    | `Bitrate -> get_bitrate in
  let init = to_init getter init in
  let x_axis = make_x_axis widget_config in
  let y_axis, _ = make_y_axis widget_config in
  let (event : data React.event) = to_event getter event in
  let (options : Chartjs.Options.t) =
    make_options ~x_axes:[x_axis] ~y_axes:[y_axis] widget_config in
  let datasets = make_datasets init widget_config.sources in
  let data = Chartjs.Data.make ~datasets:(List.map snd datasets) () in
  let canvas = Js_of_ocaml.Dom_html.(createCanvas document) in
  let line = Chartjs.make ~options ~data `Line (`Canvas canvas) in
  let box = Js_of_ocaml.Dom_html.(createDiv document) in
  object(self)

    val mutable _datasets = datasets

    inherit Widget.t box () as super

    method! init () : unit =
      super#init ();
      super#append_child @@ Widget.create canvas;
      super#add_class base_class;
      React.E.map (fun d ->
          List.iter (fun ((s : Stream.ID.t), data) ->
              match List.Assoc.get ~eq:equal_data_source s datasets with
              | None -> ()
              | Some ds ->
                 let data' = Dataset.data ds in
                 let push v = Dataset.(Values.push data' [v]) in
                 List.iter Fun.(ignore % push) data;
                 Chartjs.update line None) d)
        event
      |> self#_keep_e

  end

let make ~(init : init)
      ~(measures : event)
      (widget_config : widget_config) =
  let event = match widget_config.sources with
    | [] -> measures
    | ids ->
       React.E.fmap (fun l ->
           List.filter (fun ((id : id), _) ->
               List.mem ~eq:Stream.ID.equal id.stream ids) l
           |> function
             | [] -> None
             | l -> Some l) measures in
  new t ~init ~event widget_config ()
