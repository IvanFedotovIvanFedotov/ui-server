open Containers
open Components
open Common
open Chartjs
open Qoe_errors

type widget_config =
  { duration : Time.Period.t
  ; typ : labels
  ; sources : data_source list
  ; settings : widget_settings option
  }
and widget_settings =
  { range : (float * float) option
  }
and data_source =
  { stream : Stream.ID.t
  ; service : int
  ; pid : int
  } [@@deriving eq]

type 'a point = (Time.t, 'a) Chartjs.Line.point

type 'a data = (data_source * ('a point list)) list

type dataset = (Time.t, float) Line.Dataset.t

let convert_video_data (config : widget_config)
      (d : Video_data.t) : 'a data =
  let (src : data_source) =
    { stream = d.stream
    ; service = d.channel
    ; pid = d.pid
    } in
  let (error : error) = match config.typ with
    | `Black -> d.errors.black
    | `Luma -> d.errors.luma
    | `Freeze -> d.errors.freeze
    | `Diff -> d.errors.diff
    | `Blocky -> d.errors.blocky
    | `Silence_shortt | `Silence_moment | `Loudness_shortt | `Loudness_moment ->
       failwith "not a video chart" in
  let (point : float point) =
    { x = error.timestamp
    ; y = error.params.avg
    } in
  [src, [point]]

let data_source_to_string (src : data_source) : string =
  "Label"

let typ_to_string : labels -> string = function
  | `Black -> "Чёрный кадр"
  | `Luma -> "Средняя яркость"
  | `Freeze -> "Заморозка видео"
  | `Diff -> "Средняя разность"
  | `Blocky -> "Блочность"
  | `Silence_shortt -> "\"Тишина\" (short term)"
  | `Silence_moment -> "\"Тишина\" (momentary)"
  | `Loudness_shortt -> "Перегрузка звука (short term)"
  | `Loudness_moment -> "Перегрузка звука (momentary)"

let typ_to_unit_string : labels -> string = function
  | `Black | `Freeze | `Blocky -> "%"
  | `Luma | `Diff -> ""
  | (`Silence_shortt | `Silence_moment
    | `Loudness_shortt | `Loudness_moment) -> "LUFS"

let make_x_axis ?(id = "x-axis") (config : widget_config)
    : (Time.t, Time.span) Line.Axes.Time.t =
  let delta = config.duration in
  let axis =
    new Line.Axes.Time.t
      ~id
      ~delta
      ~position:`Bottom
      ~typ:Ptime
      () in
  axis#scale_label#set_display true;
  axis#scale_label#set_label_string "Время";
  axis#time#set_tooltip_format "ll HH:mm:ss";
  axis#ticks#set_auto_skip_padding 2;
  axis

let make_y_axis ?(id = "y-axis") (config : widget_config)
    : float Line.Axes.Linear.t =
  let axis =
    new Line.Axes.Linear.t
      ~id
      ~position:`Left
      ~typ:Float
      () in
  axis#scale_label#set_display true;
  axis#scale_label#set_label_string @@ typ_to_unit_string config.typ;
  axis

let make_options ~x_axes ~y_axes
      (config : widget_config) : Line.Options.t =
  let options = new Line.Options.t ~x_axes ~y_axes () in
  options#set_maintain_aspect_ratio false;
  options

let make_dataset ~x_axis ~y_axis src data =
  let label = data_source_to_string src in
  let ds =
    new Line.Dataset.t ~label
      ~data
      ~x_axis
      ~y_axis
      () in
  src, ds

let make_datasets ~x_axis ~y_axis
      (init : float data)
      (sources : data_source list)
    : (data_source * dataset) list =
  let map (src : data_source) =
    let data =
      List.find_map (fun (src', data) ->
          if equal_data_source src src'
          then Some data else None) init
      |> Option.get_or ~default:[] in
    make_dataset ~x_axis ~y_axis src data in
  List.map map sources

class t ~(init : float data)
        ~(config : widget_config)
        () =
  let x_axis = make_x_axis config in
  let y_axis = make_y_axis config in
  let (options : Line.Options.t) =
    make_options ~x_axes:[x_axis] ~y_axes:[y_axis] config in
  let (datasets : (data_source * dataset) list) =
    make_datasets ~x_axis ~y_axis init config.sources in
  object

    val mutable _datasets = datasets

    inherit Line.t
              ~options
              ~datasets:(List.map snd datasets) () as super

    method append_data (data : float data) : unit =
      List.iter (fun (src, data) ->
          match List.Assoc.get ~eq:equal_data_source src datasets with
          | None ->
             begin match config.sources with
             | [] ->
                let ds = make_dataset ~x_axis ~y_axis src data in
                _datasets <- ds :: _datasets;
                super#set_datasets @@ List.map snd _datasets;
                super#update None
             | _ -> ()
             end
          | Some ds ->
             List.iter (fun point -> ds#push point) data;
             super#update None) data

  end

let make_dashboard_item ~init ~config () =
  let widget = new t ~init ~config () in
  Dashboard.Item.make_item
    ~name:(typ_to_string config.typ)
    widget#widget
