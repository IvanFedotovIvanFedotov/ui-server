open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Application_types
open Pipeline_types

let ( >>= ) = Lwt.( >>= )

type event =
  [ `Streams of Structure.Annotated.t
  | `Layout of Wm.t
  ]

type channel =
  { stream : Stream.ID.t
  ; channel : int
  }

let split_three l =
  List.fold_left (fun (first, second, third) (a, b, c) ->
      a :: first, b :: second, c :: third) ([], [], []) l

(* module Parse_struct = struct
 * 
 *   let stream id (signal : Structure.Annotated.t) =
 *     let packed =
 *       List.find_opt (fun (state, (structure : Structure.Annotated.structure)) ->
 *           Stream.ID.equal structure.id id) signal in
 *     match packed with
 *     | None -> None
 *     | Some (_, structure) ->
 *       Some (Stream.Source.to_string structure.source.info,
 *             Uri.to_string packed.structure.uri, packed)
 * 
 *   let channel (channel_ : int) (structure : Structure.Annotated.structure) =
 *     let channel =
 *       List.find_opt (fun (ch : Structure.channel) -> ch.number = channel_)
 *         structure.channels in
 *     match channel with
 *     | None -> None
 *     | Some channel -> Some (channel.service_name, channel.provider_name, channel)
 * 
 *   let widget pid_ (channel : Structure.channel) =
 *     let pid = List.find_opt (fun (pid : Structure.pid) ->
 *         pid.pid = pid_) channel.pids in
 *     match pid with
 *     | None -> None
 *     | Some pid ->
 *       let stream_type =
 *         Application_types.MPEG_TS.stream_type_to_string
 *           pid.stream_type in
 *       Some (stream_type,
 *             "PID "
 *             ^ (string_of_int pid.pid)
 *             ^ (Printf.sprintf " (0x%04X)" pid.pid))
 * 
 * end *)

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

module Branches = struct

  type data =
    { widget : string * Wm.widget
    ; service_name : string
    ; provider_name : string
    } [@@deriving yojson]

  (* makes a checkbox with id of domains and typ, and a tree item named by channel*)
  let make_widget (widget : (string * Wm.widget) * channel)
      (channel_struct : Structure.Annotated.channel) =
    let widget, _channel = widget in
    let typ = (snd widget).type_ in
    let default_text () =
      match typ with
      | Video -> "Видео"
      | Audio -> "Аудио" in
    let text, secondary_text =
      match (snd widget).pid with
      | Some _pid -> "", ""
        (* (match Parse_struct.widget pid channel_struct with
         *  | None -> default_text (), Printf.sprintf "PID: %d (0x%04X)" pid pid
         *  | Some s -> s) *)
      | None -> default_text (), "" in
    let checkbox = Checkbox.make () in
    let data =
      { widget
      ; service_name = channel_struct.service_name
      ; provider_name = channel_struct.provider_name
      } in
    let node =
      Treeview.make_node
        ~secondary_text
        ~graphic:checkbox#root
        ~value:(Yojson.Safe.to_string @@ data_to_yojson data)
        text in
    node

  (* makes all the widgets checkboxes with IDs, checkboxes of channels Tree items,
   * and a Tree.t containing all given channels *)
  let make_channels
      (widgets : ((string * Wm.widget) * channel) list)
      (_state, (structure : Structure.Annotated.structure)) =
    let channels = Find.channels widgets in
    List.rev
    @@ List.fold_left (fun acc channel ->
        let channel, stream = channel.channel, channel.stream in
        let channel_struct =
          List.find_opt (fun (_, (ch : Structure.Annotated.channel)) ->
              channel = ch.number) structure.channels in
        match channel_struct with
        | None -> acc
        | Some (_, channel_struct) ->
          let text, secondary_text =
            channel_struct.service_name, string_of_int channel in
          let widgets =
            List.filter (fun (_, (ch : channel)) ->
                Stream.ID.equal ch.stream stream
                && channel = ch.channel) widgets in
          let children = List.map (fun widget ->
              make_widget widget channel_struct) widgets in
          let checkbox = Checkbox.make () in
          let node =
            Treeview.make_node
              ~value:text
              ~graphic:checkbox#root
              ~secondary_text
              ~children
              text in
          node :: acc)
      [] channels

  (* makes all the widget checkboxes with IDs, and a Tree.t containing all streams *)
  let make_streams (widgets : ((string * Wm.widget) * channel) list)
      (_structure : Structure.Annotated.t) =
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
      List.fold_left (fun acc (_stream, _wds) ->
          acc
          (* match Parse_struct.stream stream structure with
           * | None -> acc
           * | Some (text, secondary_text, packed) ->
           *   let channels = make_channels wds packed in
           *   let checkbox = Checkbox.make () in
           *   let stream_node =
           *     Treeview.make_node
           *       ~secondary_text
           *       ~graphic:checkbox#root
           *       ~children:channels
           *       ~value:(Stream.ID.to_string stream)
           *       text in
           *   stream_node :: acc *))
        [] streams_of_widgets in
    Treeview.make ~dense:true ~two_line:true nodes

end


module LayoutOfWidget = struct

  let make_widget 
  (index:int)
  ?(type_ = Wm.Video)
  ?(domain = Wm.Nihil)
  ?aspect
  ~x ~y ~w ~h () : string * Wm.widget =
  let (position : Wm.position) = { x; y; w; h } in
    string_of_int @@ Random.bits (),
    { position = Some position
    ; description = String.concat "" ("" :: string_of_int(index) :: []) 
    ; pid = Some 4096
    ; type_
    ; aspect
    ; domain
    ; layer = 0
  }

  let widgets =
    let stream1 = Application_types.Stream.ID.make "id" in
    let stream2 = Application_types.Stream.ID.make "id" in
    let stream3 = Application_types.Stream.ID.make "id" in
    let stream4 = Application_types.Stream.ID.make "id" in
    let counter = ref 0 in
    let acc = ref [] in
    (*1 Duplication stream channel *)
    [ (counter:=!counter+1; make_widget !counter ~type_:Video
      ~aspect:(16, 9) ~domain:(Chan { stream = stream1 ; channel = 1 })
      ~x:((float_of_int !counter) *. 0.03) ~y:((float_of_int !counter) *. 0.03) ~w:(0.3) ~h:(0.3) () )
    ; (counter:=!counter+1; make_widget !counter ~type_:Video
      ~aspect:(4, 3) ~domain:(Chan { stream = stream1 ; channel = 1  })
      ~x:((float_of_int !counter) *. 0.03) ~y:((float_of_int !counter) *. 0.03) ~w:(0.3) ~h:(0.3) () )
    ; (counter:=!counter+1; make_widget !counter ~type_:Audio
      ~domain:(Chan { stream = stream1 ; channel = 1 })
      ~x:((float_of_int !counter) *. 0.03) ~y:((float_of_int !counter) *. 0.03) ~w:(0.1) ~h:(0.3) () )
    ; (counter:=!counter+1; make_widget !counter ~type_:Audio
      ~domain:(Chan { stream = stream1 ; channel = 1 })
      ~x:((float_of_int !counter) *. 0.03) ~y:((float_of_int !counter) *. 0.03) ~w:(0.1) ~h:(0.3) () )    
    
    (*2 Normal *)
    ; (counter:=!counter+1; make_widget !counter ~type_:Video
      ~aspect:(16, 9) ~domain:(Chan { stream = stream2 ; channel = 2 })
      ~x:((float_of_int !counter) *. 0.03) ~y:((float_of_int !counter) *. 0.03) ~w:(0.3) ~h:(0.3) () )
    ; (counter:=!counter+1; make_widget !counter ~type_:Audio
      ~domain:(Chan { stream = stream2 ; channel = 2 })
      ~x:((float_of_int !counter) *. 0.03) ~y:((float_of_int !counter) *. 0.03) ~w:(0.1) ~h:(0.3) () )

    (*3 Nihil *)
    ; (counter:=!counter+1; make_widget !counter ~type_:Video
      ~aspect:(16, 9) ~domain:Wm.Nihil
      ~x:((float_of_int !counter) *. 0.03) ~y:((float_of_int !counter) *. 0.03) ~w:(0.3) ~h:(0.3) () )
    ; (counter:=!counter+1; make_widget !counter ~type_:Audio
      ~domain:(Chan { stream = stream3 ; channel = 3 })
      ~x:((float_of_int !counter) *. 0.03) ~y:((float_of_int !counter) *. 0.03) ~w:(0.1) ~h:(0.3) () )
    
    (*4 *)
    ; (counter:=!counter+1; make_widget !counter ~type_:Video
      ~aspect:(16, 9) ~domain:(Chan { stream = stream4 ; channel = 4 })
      ~x:((float_of_int !counter) *. 0.03) ~y:((float_of_int !counter) *. 0.03) ~w:(0.3) ~h:(0.3) () )
    (*Nihil *)
    ; (counter:=!counter+1; make_widget !counter ~type_:Audio
      ~domain:Wm.Nihil
      ~x:((float_of_int !counter) *. 0.03) ~y:((float_of_int !counter) *. 0.03) ~w:(0.1) ~h:(0.3) () )

    (*5 Nihil *)
    ; (counter:=!counter+1; make_widget !counter ~type_:Video
      ~aspect:(16, 9) ~domain:Wm.Nihil
      ~x:((float_of_int !counter) *. 0.03) ~y:((float_of_int !counter) *. 0.03) ~w:(0.3) ~h:(0.3) () )
    (* Nihil *)
    ; (counter:=!counter+1; make_widget !counter ~type_:Audio
      ~domain:Wm.Nihil
      ~x:((float_of_int !counter) *. 0.03) ~y:((float_of_int !counter) *. 0.03) ~w:(0.1) ~h:(0.3) () )

    ] @
    (for v = 1 to 8 do
      let stream_n = Application_types.Stream.ID.make "id" in
      acc := [
        (counter:=!counter+1; make_widget !counter ~type_:Video
        ~aspect:(16, 9) ~domain:(Chan { stream = stream_n ; channel = v + 100 })
        ~x:((float_of_int !counter) *. 0.03) ~y:((float_of_int !counter) *. 0.03) ~w:(0.1) ~h:(0.3) () )
      ; (counter:=!counter+1; make_widget !counter ~type_:Audio
        ~domain:(Chan { stream = stream_n ; channel = v + 100 })
        ~x:((float_of_int !counter) *. 0.03 +. 0.015) ~y:((float_of_int !counter) *. 0.03 +. 0.015) ~w:(0.1) ~h:(0.3) () )      
        ] @ !acc
    done;
    !acc
    )

  let create_test s : Branches.data list =
    List.map (fun v ->
      ({ widget = v
       ; service_name = s
       ; provider_name = "provider1"
       }:Branches.data)
    ) widgets


  let get_float_aspect (aspect : int * int)  =
      let asp =
        if fst aspect = 0
        then 1.0
        else (float_of_int (snd aspect)) /. (float_of_int (fst aspect)) in
      if asp <= 0.0 then 1.0 else asp    

end


let to_content (streams : Structure.Annotated.t)
    (wm : Wm.Annotated.t) =
  let widgets = Utils.List.filter_map (fun (name, (widget : Wm.widget)) ->
      match (widget.domain : Wm.domain) with
      | (Chan {stream; channel} : Wm.domain) ->
        Some ((name, widget), ({ stream; channel } : channel))
      | (Nihil : Wm.domain) -> None) wm.widgets in
  Branches.make_streams widgets streams

let layout_of_widgets ~(resolution:(int * int)) (data : Branches.data list)
  : ((string * Wm.container) list) =
  let default_video_aspect = (16, 9) in
  let default_audio_aspect = (1, 10) in
  let compare_aspects a b = let (ax, ay) = a in
    let (bx, by) = b in ax = bx && ay = by
  in
  let filter_widgets 
      (widgets:Pipeline_types.Wm.widget list)
      (widget_type:Pipeline_types.Wm.widget_type) =
    (List.filter (fun (w:Pipeline_types.Wm.widget) -> 
      w.type_ = widget_type && w.domain <> Nihil ) widgets)
  in  
  let get_all_aspects (widgets:Pipeline_types.Wm.widget list) =
    List.map (fun (w:Pipeline_types.Wm.widget) -> 
      let aspect = w.aspect in 
      match aspect with
        | None -> (0, 0) (* none aspect *)
        | Some x -> x
    ) widgets
  in
  let get_uniq_aspects (aspects:(int * int) list) =
    List.sort_uniq (fun a b -> 
     let a_asp = LayoutOfWidget.get_float_aspect a in 
     let b_asp = LayoutOfWidget.get_float_aspect b in 
     if a_asp = b_asp then 0
     else if a_asp > b_asp then 1 else -1) aspects in
  let get_uniq_aspects_sorted_by_weight (uniq_aspects:(int * int) list)= 
    List.sort_uniq (fun a b -> 
     let (_, aweight) = a in 
     let (_, bweight) = b in
     if aweight = bweight then 0
     else if aweight > bweight then 1 else -1)
    (List.map (fun u -> 
      (u, List.fold_left (fun acc v ->
        if compare_aspects v u 
        then acc + 1 
        else acc
        ) 0 uniq_aspects
      )
    ) uniq_aspects)
    in
  let get_main_aspect (aspects_weighted_sorted:((int * int) * int) list) 
     (default_aspect:(int * int))=
    LayoutOfWidget.get_float_aspect
    (if (List.length aspects_weighted_sorted) > 0
    then let (asp, _) = List.hd aspects_weighted_sorted in 
      if compare_aspects asp (0,0) 
      then default_aspect
      else asp
    else default_aspect)
    in
  let compare_widgets_by_strm_chnl
      (a:Pipeline_types.Wm.widget)
      (b:Pipeline_types.Wm.widget) =
    let ((a_stream: Application_types.Stream.ID.t option), a_channel) = 
      match (a.domain:Pipeline_types.Wm.domain) with
        | Nihil -> (None, (-1))
        | Chan x -> (Some x.stream, x.channel)
    in      
    let ((b_stream: Application_types.Stream.ID.t option), b_channel) = 
      match (b.domain:Pipeline_types.Wm.domain) with
        | Nihil -> (None, (-1))
        | Chan x -> (Some x.stream, x.channel)
    in      
    let stream_compared =
      match a_stream, b_stream with
      | None, None -> 0
      | Some x, None -> 1
      | None, Some y -> 1
      | Some x, Some y -> compare x y
      in
    let channel_compared =
      if a_channel = b_channel then 0
      else if a_channel > b_channel then 1
      else -1 
      in
    if stream_compared = 0 && channel_compared = 0 then 0
    else if stream_compared > 0 && channel_compared > 0 then 1
    else if stream_compared < 0 && channel_compared > 0 then 1
    else -1
  in
  let get_widgets_uniq_chnl_strm (widgets:Pipeline_types.Wm.widget list) =
    List.sort_uniq (fun (a:Pipeline_types.Wm.widget) (b:Pipeline_types.Wm.widget) -> 
      compare_widgets_by_strm_chnl a b
    ) widgets
      in  
      let get_av_widget_pairs 
      (video_widgets_uniq_chnl_strm:Pipeline_types.Wm.widget list)
      (audio_widgets_uniq_chnl_strm:Pipeline_types.Wm.widget list) =
    let rec aux acc audio_widgets_uniq_chnl_strm = function
        | [] -> acc
        | hd :: tl -> let acc =
          match List.find_opt (fun w -> 
            if (compare_widgets_by_strm_chnl hd w) = 0 
            then true
            else false
          ) audio_widgets_uniq_chnl_strm 
          with
            | None -> acc
            | Some a -> (Some hd, Some a) :: acc
            in
        aux acc audio_widgets_uniq_chnl_strm tl
      in
    aux [] audio_widgets_uniq_chnl_strm video_widgets_uniq_chnl_strm
  in
  let get_non_paired_av_widgets
      (widgets_uniq_chnl_strm:Pipeline_types.Wm.widget list)
      (selector: Pipeline_types.Wm.widget_type)
      (av_pairs: (Pipeline_types.Wm.widget option * Pipeline_types.Wm.widget option) list) =
  let rec aux acc widgets_uniq_chnl_strm (selector: Pipeline_types.Wm.widget_type) = function
      | [] -> acc
      | hd :: tl -> let (v,a) = hd in
         let acc = 
           match v with
            | None -> acc
            | Some x ->
        match
          List.find_opt (fun w -> 
            if (compare_widgets_by_strm_chnl x w) = 0 
            then true
            else false
        ) widgets_uniq_chnl_strm 
        with
          | None -> acc
          | Some a -> match selector with
             | Video -> v :: acc
             | Audio -> Some a :: acc
        in
      aux acc widgets_uniq_chnl_strm selector tl
    in
  aux [] widgets_uniq_chnl_strm selector av_pairs
  in      
  let rec generate_containers 
      (acc: (string * Wm.container) list)
      (x:float) (* initial 0.0 *)
      (y:float) (* initial 0.0 *)
      (nx:float) 
      (ny:float) 
      (container_aspect:float) 
      (video_asp:float) 
      (audio_asp:float)
      (av_pairs:(Pipeline_types.Wm.widget option * Pipeline_types.Wm.widget option) list) =
   (* container: 
    position : Pipeline_types.Wm.position;
    widgets : (string * Pipeline_types.Wm.widget) list;  
    
    type widget =
      Qoe_backend_types__Wm.Make(Application_types.Stream.ID).widget = {
      type_ : Pipeline_types.Wm.widget_type;
      domain : Pipeline_types.Wm.domain;
      pid : int option;
      position : Pipeline_types.Wm.position option;
      layer : int;
      aspect : (int * int) option;
      description : string;
    }
    *)      
    match av_pairs with
      | [] -> acc
      | hd :: tl -> 
        let (container_pos:Pipeline_types.Wm.position) = 
          { x = x /. nx
          ; y = y /. ny
          ; w = 1.0 /. ny
          ; h = 1.0 /. ny }
        in
        let (v,a) = hd in
        let wv = match v with
          | None -> None
          | Some w -> Some {w with position = (Some
              { x = 0.0
              ; y = 0.0
              ; w = video_asp /. (video_asp +. audio_asp)
              ; h = 1.0 }) }
              in
        let wa = match a with
          | None -> None
          | Some w -> Some {w with position = (Some
              { x = video_asp/. (video_asp +. audio_asp)
              ; y = 0.0
              ; w = 1.0 -. video_asp/. (video_asp +. audio_asp)
              ; h = 1.0 }) }
              in
        let acc = ("", { position = container_pos; widgets =
              (match wv, wa with
                | None, None -> []
                | Some x, None -> [("", x)]
                | None, Some y -> [("", y)]
                | Some x, Some y -> [("", x); ("", y)])
              }:(string * Wm.container)) :: acc
            in
    generate_containers acc x y nx ny container_aspect video_asp audio_asp tl
  in
  let widgets = List.map (fun (v:Branches.data) -> 
    let (_, (w:Pipeline_types.Wm.widget)) = v.widget in w
    ) data in
  let video_widgets = filter_widgets widgets Video in
  let audio_widgets = filter_widgets widgets Audio in
  let video_all_aspects = get_all_aspects video_widgets in
  let video_uniq_aspects = get_uniq_aspects video_all_aspects in
  let video_uniq_aspects_weight_sorted = 
    get_uniq_aspects_sorted_by_weight video_uniq_aspects in
  let video_main_aspect = get_main_aspect 
    video_uniq_aspects_weight_sorted default_video_aspect in
  let audio_all_aspects = get_all_aspects audio_widgets in
  let audio_uniq_aspects = get_uniq_aspects audio_all_aspects in
  let audio_uniq_aspects_weight_sorted = 
    get_uniq_aspects_sorted_by_weight audio_uniq_aspects in
  let audio_main_aspect = get_main_aspect 
    audio_uniq_aspects_weight_sorted default_audio_aspect in
  let main_aspect = video_main_aspect +. audio_main_aspect in
  let video_widgets_uniq_chnl_strm = get_widgets_uniq_chnl_strm video_widgets in
  let audio_widgets_uniq_chnl_strm = get_widgets_uniq_chnl_strm audio_widgets in
  (* widgets for install to containers:*)
  let av_pairs = get_av_widget_pairs video_widgets_uniq_chnl_strm audio_widgets_uniq_chnl_strm in
  let v_non_paired = get_non_paired_av_widgets video_widgets_uniq_chnl_strm Video av_pairs in
  let a_non_paired = get_non_paired_av_widgets audio_widgets_uniq_chnl_strm Audio av_pairs in
  let common_av_pairs = av_pairs @
    (List.map (fun x -> (x, None)) v_non_paired) @
    (List.map (fun x -> (None, x)) a_non_paired) in
  let n = List.length common_av_pairs in
  let asp_res = LayoutOfWidget.get_float_aspect resolution in
  let frnd f = floor (f +. 0.5) in
  let nx = int_of_float (frnd (sqrt (float_of_int n) *. asp_res /. main_aspect)) in
  let ny = int_of_float (frnd (float_of_int (n) /. (float_of_int (if nx > 0 then nx else 1)))) in
  if nx <= 0 || ny <= 0 
  then []
  else
  let container_aspect = LayoutOfWidget.get_float_aspect 
    ((fst resolution) / nx, (snd resolution) / ny) in
  generate_containers [] 0.0 0.0 (float_of_int nx) (float_of_int ny) 
    container_aspect video_main_aspect audio_main_aspect common_av_pairs

  (* TODO implement *)
 (* ignore resolution;
  ignore data;
  [] *)

  let test_layout_of_widgets _ =
    let s = "stream1" in
    let _ = layout_of_widgets (LayoutOfWidget.create_test s) in
    ()

class t ~resolution ~treeview (elt : Dom_html.element Js.t) () =
  object
    inherit Dialog.t elt ()

    val mutable resolution = resolution
    val mutable _treeview : Treeview.t = treeview

    method value =
      let data =
        Utils.List.filter_map (fun x ->
            match _treeview#node_value x with
            | None -> None
            | Some json ->
              try
                match Branches.data_of_yojson @@ Yojson.Safe.from_string json with
                | Error _ -> None
                | Ok x -> Some x
              with _ -> None)
        @@ _treeview#selected_leafs in
      layout_of_widgets ~resolution data

    (* TODO implement *)
    method notify : event -> unit = function
      | `Streams _streams -> ()
      | `Layout layout -> resolution <- layout.resolution
  end

let make
    (streams : Structure.Annotated.t)
    (wm : Wm.Annotated.t) =
  let content = to_content streams wm in
  let actions =
    List.map Tyxml_js.Of_dom.of_button
      [ Dialog.make_action ~action:Close ~label:"Отмена" ()
      ; Dialog.make_action ~action:Accept ~label:"Применить" () ] in
  let surface = Dialog.Markup.(
      create_surface
        ~title:(create_title_simple ~title:"Выберите виджеты" ())
        ~content:(create_content ~content:[content#markup] ())
        ~actions:(create_actions ~actions ())
        ()) in
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.(
        create
          ~scrim:(create_scrim ())
          ~container:(create_container ~surface ())
          ()) in
  new t
    ~resolution:wm.resolution
    ~treeview:content
    elt ()
