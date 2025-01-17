open Js_of_ocaml
open Components
open Basic_widgets
open Application_types
open Pipeline_types

let ( >>= ) = Lwt.( >>= )

type channel =
  { stream : Stream.ID.t
  ; channel : int
  }

let split_three l =
  List.fold_left (fun (first, second, third) (a, b, c) ->
      a :: first, b :: second, c :: third) ([], [], []) l

module Parse_struct = struct

  let stream id (signal : Structure.packed list React.signal) =
    let packed =
      List.find (fun (x : Structure.packed) ->
          Stream.ID.equal x.structure.id id) (React.S.value signal) in
    Stream.Source.to_string packed.source.source.info,
    Uri.to_string packed.structure.uri, packed

  let channel (channel_ : int) (packed : Structure.packed) =
    let channel =
      List.find (fun (ch : Structure.channel) -> ch.number = channel_)
        packed.structure.channels in
    channel.service_name, channel.provider_name, channel

  let widget pid_ (channel : Structure.channel) =
    let pid = List.find (fun (pid : Structure.pid) -> pid.pid = pid_) channel.pids in
    "PID " ^ (string_of_int pid.pid) ^ (Printf.sprintf " (0x%04X)" pid.pid)

end

module Find = struct

  let channels (widgets : ((string * Wm.widget) * channel) list) =
    List.rev
    @@ List.fold_left (fun acc (widget : ((string * Wm.widget) * channel)) ->
        if List.exists (fun ch -> (snd widget).channel = ch.channel) acc
        then acc
        else (snd widget) :: acc)
      [] widgets

  let widget ~(widgets : ((string * Wm.widget) * channel) list) ~(typ : Wm.widget_type) ch =
    List.find_opt (fun ((_, (widget : Wm.widget)), wdg_channel) ->
        Stream.ID.equal ch.stream wdg_channel.stream
        && ch.channel = wdg_channel.channel
        && Wm.widget_type_equal widget.type_ typ) widgets

end

module Positioning = struct

  let soundbar_ch_w = 10
  let channels = 2

  let video_position ~(cont_pos : Wm.position) ~audio : Wm.position =
    match audio with
    | `With_audio ->
      { left = 0
      ; top = 0
      ; right = cont_pos.right - cont_pos.left - soundbar_ch_w * channels
      ; bottom = cont_pos.bottom - cont_pos.top
      }
    | `Without_audio ->
      { left = 0
      ; top = 0
      ; right = cont_pos.right - cont_pos.left
      ; bottom = cont_pos.bottom - cont_pos.top
      }

  let audio_position ~(cont_pos : Wm.position) ~video : Wm.position =
    match video with
    | `With_video ->
      { left = cont_pos.right - cont_pos.left - soundbar_ch_w * channels
      ; top = 0
      ; right = cont_pos.right - cont_pos.left
      ; bottom = cont_pos.bottom - cont_pos.top
      }
    | `Without_video ->
      { left = 0
      ; top = 0
      ; right = cont_pos.right - cont_pos.left
      ; bottom = cont_pos.bottom - cont_pos.top
      }

  let get_items_in_row
      ~(resolution : int * int)
      ~(item_ar : int * int)
      num =
    let calculate_cols_rows () =
      let resolution_ar =
        Utils.resolution_to_aspect resolution
        |> (fun (x, y) -> (float_of_int x) /. (float_of_int y)) in
      let rec get_squares acc = function
        | 0 -> acc
        | rows ->
          let cols = ceil (float_of_int num /. float_of_int rows) in
          let w = float_of_int (fst resolution) /. cols in
          let h = w /. resolution_ar in
          let acc =
            if h *. float_of_int rows > float_of_int @@ snd resolution
            then (0, 0, 0.) :: acc
            else (
              let squares  = w *. h *. float_of_int num in
              (int_of_float cols, rows, squares) :: acc) in
          get_squares acc (pred rows) in
      let squares = get_squares [] num in
      let (cols : int), (rows : int), _ =
        List.fold_left (fun acc x ->
            let _, _, sq = x in
            let _, _, gr = acc in
            if Float.(gr > sq) then acc else x)
          (1, 1, 1.) squares in
      cols, rows in
    if ( Float.equal (float_of_int (fst resolution) /. float_of_int (snd resolution))
           (float_of_int (fst item_ar) /. float_of_int (snd item_ar))) then
      match num with
      | 1 -> 1, 1
      | 2 -> 2, 1
      | 3 | 4 -> 2, 2
      | 5 | 6 -> 3, 2
      | x when x >= 7  && x <= 9  -> 3, 3
      | x when x >= 10 && x <= 12 -> 4, 3
      | x when x >= 13 && x <= 16 -> 4, 4
      | x when x >= 17 && x <= 20 -> 5, 4
      | x when x >= 21 && x <= 25 -> 5, 5
      | x when x >= 26 && x <= 30 -> 6, 5
      | x when x >= 31 && x <= 36 -> 6, 6
      | x when x >= 37 && x <= 42 -> 7, 6
      | x when x >= 43 && x <= 49 -> 7, 7
      | x when x >= 50 && x <= 56 -> 8, 7
      | x when x >= 57 && x <= 64 -> 8, 8
      | x when x >= 65 && x <= 72 -> 9, 8
      | x when x >= 73 && x <= 81 -> 9, 9
      | x when x >= 82 && x <= 90 -> 10, 9
      | x when x >= 90 && x <= 100 -> 10, 10
      | _ -> calculate_cols_rows ()
    else calculate_cols_rows ()

  (* position_widget_1 works better but it still adds slight distortion to an image *)
  let position_widget_1 ~(pos : Wm.position) (widget : Wm.widget) : Wm.widget =
    match widget.aspect with
    | Some aspect ->
      let w = pos.right - pos.left in
      let h = pos.bottom - pos.top in
      let new_w, new_h =
        if w / (fst aspect) * (snd aspect) > h then
          h / (snd aspect) * (fst aspect), h
        else
          w, w / (fst aspect) * (snd aspect) in
      if new_w > 0 && new_w <= w
         && new_h > 0 && new_h <= h then
        let left = pos.left + (w - new_w) / 2 in
        let top  = pos.top + (h - new_h) / 2 in
        let position  =
          Wm.{ left
             ; top
             ; right = left + new_w
             ; bottom = top + new_h
             } in
        { widget with position = Some position }
      else
        widget
    | None -> widget

  (* NOTE this function is not being used currently, but may be useful *)
  (* position_widget works properly, but leaves more blank space *)
  let position_widget ~(pos : Wm.position) (widget : Wm.widget) : Wm.widget =
    let cpos = Utils.to_grid_position pos in
    let wpos = match widget.aspect with
      | None -> cpos
      | Some x -> Dynamic_grid.Position.correct_aspect cpos x in
    let x = cpos.x + ((cpos.w - wpos.w) / 2) in
    let y = cpos.y + ((cpos.h - wpos.h) / 2) in
    let pos = Utils.of_grid_position { wpos with x ; y } in
    { widget with position = Some pos }

end

module Branches = struct

  (* makes a checkbox with id of domains and typ, and a tree item named by channel*)
  let make_widget (widget : (string * Wm.widget) * channel) channel_struct =
    let widget, channel = widget in
    let typ = (snd widget).type_ in
    let text =
      match typ with
      | Video -> "Виджет видео"
      | Audio -> "Виджет аудио" in
    let secondary_text =
      match (snd widget).pid with
      | Some pid -> Parse_struct.widget pid channel_struct
      | None -> "" in
    let checkbox = Checkbox.make () in
    let widget_typ =
      match typ with
      | Video -> "Video"
      | Audio -> "Audio" in
    Treeview.make_node
      ~secondary_text
      ~graphic:checkbox#root
      ~value:(string_of_int channel.channel ^ "|" ^ widget_typ)
      text

  (* makes all the widgets checkboxes with IDs, checkboxes of channels Tree items,
   * and a Tree.t containing all given channels *)
  let make_channels
      (widgets : ((string * Wm.widget) * channel) list)
      (packed : Structure.packed) =
    let channels = Find.channels widgets in
    List.map (fun channel ->
        let channel, stream = channel.channel, channel.stream in
        let channel_struct =
          List.find (fun (ch : Structure.channel) ->
              channel = ch.number) packed.structure.channels in
        let text, secondary_text =
          channel_struct.service_name, string_of_int channel in
        let widgets =
          List.filter (fun (_, (ch : channel)) ->
              Stream.ID.equal ch.stream stream
              && channel = ch.channel) widgets in
        let children = List.map (fun widget ->
            make_widget widget channel_struct) widgets in
        let checkbox = Checkbox.make () in
        Treeview.make_node
          ~value:text
          ~graphic:checkbox#root
          ~secondary_text
          ~children
          text) channels

  (* makes all the widget checkboxes with IDs, and a Tree.t containing all streams *)
  let make_streams (widgets : ((string * Wm.widget) * channel) list) signal =
    let streams =
      List.fold_left (fun acc (x : (string * Wm.widget) * channel) ->
          let channel = snd x in
          if List.exists (fun stream ->
              Stream.ID.equal channel.stream stream) acc then
            acc
          else
            channel.stream :: acc) [] widgets in
    let streams_of_widgets =
      List.map (fun stream ->
          let wds =
            List.filter (fun (x : (string * Wm.widget) * channel) ->
                let wdg_stream = (snd x).stream in
                Stream.ID.equal wdg_stream stream) widgets in
          stream, wds) streams in
    let nodes =
      List.fold_left (fun acc (stream, wds) ->
          let text, secondary_text, packed =
            Parse_struct.stream stream signal in
          let channels = make_channels wds packed in
          let checkbox = Checkbox.make () in
          let stream_node =
            Treeview.make_node
              ~secondary_text
              ~graphic:checkbox#root
              ~children:channels
              ~value:(Stream.ID.to_string stream)
              text in
          stream_node :: acc)
        [] streams_of_widgets in
    Treeview.make ~dense:true ~two_line:true nodes

end

module Create = struct

  let video_widget
      ~(video : ((string * Wm.widget) * channel) option)
      ~(audio : ((string * Wm.widget) * channel) option)
      cont_pos =
    match video with
    | Some (video, _) ->
      let video_pos =
        match audio with
        | Some _ -> Positioning.video_position ~audio:`With_audio ~cont_pos
        | None   -> Positioning.video_position ~audio:`Without_audio ~cont_pos in
      (* actually we should use position_widget here,
       * but it leaves more blank space *)
      (* fst video, {(snd video) with position = video_pos} in *)
      Some (fst video, Positioning.position_widget_1 ~pos:video_pos (snd video))
    | None -> None

  let audio_widget
      ~(video : ((string * Wm.widget) * channel) option)
      ~(audio : ((string * Wm.widget) * channel) option)
      cont_pos =
    match audio with
    | Some (audio, _) ->
      let audio_pos =
        match video with
        | Some _ -> Positioning.audio_position ~video:`With_video ~cont_pos
        | None   -> Positioning.audio_position ~video:`Without_video ~cont_pos in
      Some (fst audio, {(snd audio) with position = Some audio_pos})
    | None -> None

end

(* makes a list of containers with given widgets, calculates its positions *)
let to_layout ~resolution ~widgets signal =
  let ar_x, ar_y = 16, 9 in
  let channels   = Find.channels widgets in
  let num        = List.length channels in
  if num <> 0 then
    let cols, rows =
      Positioning.get_items_in_row ~resolution ~item_ar:(ar_x, ar_y) num in
    let cont_std_w = fst resolution / cols in
    let cont_std_h = (ar_y * cont_std_w) / ar_x in
    let remain = num - (cols * (rows - 1)) in
    (* 'greatest' is the number of containers we should increase in size,
     * 'multiplier' is the number to multiply width and height on *)
    let greatest, multiplier =
      if rows < cols
      && remain <> cols then
        if float_of_int remain /. float_of_int cols <= 0.5 then
          remain, 2
        else
          (* if the number of remaining containers
           * is greater than the half of the row *)
          cols - remain, 2
      else
        remain, 1 in
    let start_h =
      if multiplier = 1
      && not (remain = cols)
      && not (cols = rows) then
        (snd resolution - cont_std_h * rows) / 2
      else
        0 in
    List.fold_left (fun (i, containers) channel ->
        let _, _, stream  = Parse_struct.stream channel.stream signal in
        let s1, _, _ = Parse_struct.channel channel.channel stream in
        let name = s1 in
        let row_num = i / cols in
        let cont_w  =
          if i + 1 > num - remain then
            fst resolution / cols * multiplier
          else
            fst resolution / cols in
        let cont_h = (ar_y * cont_w) / ar_x in
        let greater_num = i - (num - greatest) in
        (* the number of greater elements behind this *)
        let left =
          if greater_num > 0 then               (* magical *)
            (i - cols * row_num - greater_num)  (* do not touch *)
            * cont_std_w + greater_num * cont_w
          else
            (i - cols * row_num) * cont_std_w in
        let top = row_num * cont_std_h + start_h in
        let cont_pos : Wm.position =
          { left ; top ; right  = left + cont_w ; bottom = top + cont_h } in
        let audio = Find.widget ~typ:Audio ~widgets channel in
        let video = Find.widget ~typ:Video ~widgets channel in
        let video_wdg = Create.video_widget ~video ~audio cont_pos in
        let audio_wdg = Create.audio_widget ~video ~audio cont_pos in
        let container =
          if cont_pos.left >= 0 && cont_pos.right <= fst resolution
             && cont_pos.top >= 0 && cont_pos.bottom <= snd resolution then
            let widgets =
              match video_wdg, audio_wdg with
              | Some video_wdg, Some audio_wdg -> [video_wdg; audio_wdg]
              | None,           Some audio_wdg -> [audio_wdg]
              | Some video_wdg, None           -> [video_wdg]
              | _ -> [] in
            match widgets with
            | []      -> None
            | widgets -> Some ({ position = cont_pos
                               ; widgets } : Wm.container)
          else
            (Printf.printf "Error building container %s!\n" name;
             None) in
        match container with
        | Some cont ->
          succ i, (name, cont) :: containers
        | None      -> i, containers)
      (0, []) channels
    |> snd
  else
    []

(* makes a dialog which shows a tree of available
 * streams
 *    |_ channels
 *        |_ widgets,
 * all with checkboxes.
 * it returns dialog, react event and a fun showing dialog *)

let to_content socket (wm : Wm.t) =
  let open Wm in
  let open Pipeline_http_js in
  let ( >>= ) x f = Lwt_result.(map_err Api_js.Http.error_to_string @@ x >>= f) in
  Http_structure.get_streams_applied_with_source ()
  >>= fun init -> Http_structure.Event.get_streams_applied_with_source socket
  >>= fun (id, event) ->
  let s, set_s = React.S.create init in
  let _e = React.E.map set_s event in
  let widgets = Utils.List.filter_map (fun (name, (widget : widget)) ->
      match (widget.domain : domain) with
      | ((Chan {stream; channel}) : domain) ->
        Some ((name, widget),
              ({ stream; channel } : channel))
      | (Nihil : domain) -> None) wm.widgets in
  let tree = Branches.make_streams widgets s in
  let box = Box.make ~dir:`Column [tree#widget] in
  box#set_on_destroy (fun () ->
      React.E.stop ~strong:true _e;
      React.S.stop ~strong:true s;
      Lwt.async (fun () -> Api_js.Websocket.JSON.unsubscribe socket id));
  let set = fun () ->
    let wds =
      Utils.List.filter_map tree#node_value
      @@ tree#selected_leafs in
    let widgets =
      List.fold_left (fun acc channel ->
          let typ =
            match String.sub channel (String.length channel - 5) 5 with
            | "Video" -> Video
            | "Audio" -> Audio
            | _ -> failwith "Wrong widget type!" in
          let channel =
            String.sub channel 0 (String.length channel - 6)
            |> int_of_string in
          match List.filter (fun ((wdg : (string * Wm.widget)), (ch : channel)) ->
              channel = ch.channel
              && widget_type_equal (snd wdg).type_ typ) widgets with
          | [] -> acc
          | l -> l @ acc) [] wds in
    to_layout ~resolution:wm.resolution ~widgets s in
  Lwt.return_ok (box, set)

let to_dialog socket (wm : Wm.t) push =
  let thread = to_content socket wm in
  let content =
    Ui_templates.Loader.create_widget_loader
    @@ Lwt_result.map fst thread in
  let dialog = Dialog.make
      ~title:(Dialog.Markup.create_title_simple ~title:"Выберите виджеты" ())
      ~content:(Dialog.Markup.create_content ~content:[content#markup] ())
      ~actions:[ Dialog.Markup.create_action ~action:Close ~label:"Отмена" ()
               ; Dialog.Markup.create_action ~action:Accept ~label:"Применить" () ]
      () in
  let show () =
    let open Lwt.Infix in
    dialog#open_await ()
    >>= function
    | Accept ->
      (thread
       >>= function
       | Ok (_, set) -> Lwt.return @@ push @@ set ()
       | Error e -> Lwt.return @@ print_endline @@ "Wm_wizard error!: " ^ e)
    | _ -> Lwt.return () in
  dialog, show
