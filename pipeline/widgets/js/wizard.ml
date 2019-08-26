open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Application_types
open Pipeline_types

include Pipeline_widgets_tyxml.Wizard
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let ( >>= ) = Lwt.bind

type event =
  [ `Streams of Structure.Annotated.t
  | `Layout of Wm.t
  ]

let compare_domain a b = match a, b with
  | Wm.Nihil, Wm.Nihil -> 0
  | Nihil, Chan _ -> -1
  | Chan _, Nihil -> 1
  | Chan a, Chan b ->
    match Stream.ID.compare a.stream b.stream with
    | 0 -> compare a.channel b.channel
    | x -> x

module type S = sig
  type t

  val container_title : t -> string

  val set_position : Wm.position -> t -> t

  val to_widget : t -> string * Wm.widget
end

module Pair = struct
  type t = int * int

  let compare a b = Int.neg @@ compare a b
end

module Widget_type = struct
  type t = Wm.widget_type

  let compare = compare
end

module Domain = struct
  type t = Wm.domain

  let compare = compare_domain
end

module Make(S : S) = struct

  module Aspects = Map.Make(Pair)

  module Types = Map.Make(Widget_type)

  module Domains = Map.Make(Domain)

  let widget x = snd @@ S.to_widget x

  let aspect_to_float (a, b) = (float_of_int a) /. (float_of_int b)

  let default_aspect = function
    | Wm.Video -> 16, 9
    | Audio -> 1, 10

  let container_position ~cols ~rows i =
    { Wm.
      x = float_of_int (i mod rows) /. float_of_int rows
    ; y = float_of_int (i / rows) /. float_of_int cols
    ; w = 1. /. float_of_int rows
    ; h = 1. /. float_of_int cols
    }

  let make_container ~cols ~rows ~video_asp ~audio_asp index (_domain, (v, a)) =
    print_endline @@ Printf.sprintf "cols: %d, rows: %d" cols rows;
    let position = container_position ~cols ~rows index in
    let vwidth = video_asp /. (video_asp +. audio_asp) in
    let awidth = 1. -. vwidth in
    let v = Option.map (S.set_position { x = 0.; y = 0.; w = vwidth; h = 1. }) v in
    let a = Option.map (S.set_position { x = vwidth; y = 0.; w = awidth; h = 1. }) a in
    match v, a with
    | None, None ->
      "", { Wm. position; widgets = [] }
    | Some x, None | None, Some x->
      S.container_title x, { position; widgets = [S.to_widget x] }
    | Some x, Some y ->
      S.container_title x, { position; widgets = [S.to_widget x; S.to_widget y] }

  let get_primary_aspect typ (widgets : S.t list Types.t Domains.t) =
    let aspects =
      Aspects.bindings
      @@ Domains.fold (fun _ widgets acc ->
          match Types.find_opt typ widgets with
          | None -> acc
          | Some widgets ->
            List.fold_left (fun acc (x : S.t) ->
                match (widget x).aspect with
                | None -> acc
                | Some aspect ->
                  Aspects.update aspect (function
                      | None -> Some 1
                      | Some x -> Some (succ x)) acc)
              acc widgets) widgets Aspects.empty in
    match aspects with
    | [] -> default_aspect typ
    | (aspect, _) :: _ -> aspect

  let get_pairs widgets =
    Domains.map (fun widgets ->
        (* rev to take first selected widget *)
        let video = Option.map List.rev @@ Types.find_opt Video widgets in
        let audio = Option.map List.rev @@ Types.find_opt Audio widgets in
        match video, audio with
        | Some (v :: _), Some (a :: _) -> Some v, Some a
        | None, Some (a :: _) | Some [], Some (a :: _) -> None, Some a
        | Some (v :: _), None | Some (v :: _), Some [] -> Some v, None
        | _ -> None, None) widgets

  let widgets data =
    List.fold_left (fun acc x ->
        let (widget : Wm.widget) = widget x in
        Domains.update widget.domain (fun acc ->
            let widgets = match acc with None -> Types.empty | Some x -> x in
            Some (Types.update widget.type_ (fun acc ->
                let widgets = match acc with None -> [] | Some x -> x in
                Some (x :: widgets)) widgets))
          acc)
      Domains.empty data

  let layout_of_widgets ~resolution = function
    | [] -> []
    | data ->
      let widgets = widgets data in
      let asp_res = aspect_to_float resolution in
      let video_asp = aspect_to_float @@ get_primary_aspect Video widgets in
      let audio_asp = aspect_to_float @@ get_primary_aspect Audio widgets in
      let total_asp = video_asp +. audio_asp in
      let av_pairs = get_pairs widgets in
      let n = float_of_int @@ Domains.cardinal av_pairs in
      let rows = int_of_float @@ Float.round @@ sqrt n /. asp_res *. total_asp in
      let cols = int_of_float @@ Float.round @@ n /. float_of_int rows in
      List.mapi (make_container ~cols ~rows ~video_asp ~audio_asp)
      @@ Domains.bindings av_pairs
end

module Layout = Make(struct
    type t = data

    let container_title (x : t) = x.service_name

    let to_widget (x : t) = x.widget

    let set_position (p : Wm.position) (x : t) =
      let widget = { (snd @@ to_widget x) with position = Some p } in
      { x with widget = (fst x.widget, widget) }
  end)

class t ~resolution ~treeview (elt : Dom_html.element Js.t) () = object(self)
  inherit Dialog.t elt () as super

  val mutable resolution = resolution
  val mutable _treeview : Treeview.t = treeview
  val mutable listeners = []

  method! initial_sync_with_dom () : unit =
    listeners <- Js_of_ocaml_lwt.Lwt_js_events.(
        [ seq_loop (make_event Treeview.Event.action)
            super#root self#handle_treeview_action
        ]
      );
    super#initial_sync_with_dom ()

  method value : Wm.t =
    let data =
      List.filter_map (fun x ->
          match _treeview#node_value x with
          | None -> None
          | Some json ->
            try
              match data_of_yojson @@ Yojson.Safe.from_string json with
              | Error _ -> None
              | Ok x -> Some x
            with _ -> None)
      @@ _treeview#selected_leafs
    in
    { Wm.
      resolution
    ; widgets = []
    ; layout = Layout.layout_of_widgets ~resolution data
    }

  (* TODO implement *)
  method notify : event -> unit = function
    | `Streams _streams -> ()
    | `Layout layout -> resolution <- layout.resolution

  method private handle_treeview_action _ _ : unit Lwt.t =
    super#layout ();
    Lwt.return_unit
end

let make
    (structure : Structure.Annotated.t)
    (wm : Wm.Annotated.t) =
  let treeview =
    Treeview.attach
    @@ Tyxml_js.To_dom.of_element
    @@ Markup.make_treeview structure wm in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.make ~treeview:treeview#markup () in
  new t ~resolution:wm.resolution ~treeview elt ()
