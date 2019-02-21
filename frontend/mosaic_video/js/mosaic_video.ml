open Js_of_ocaml
open Containers
open Components
open Lwt.Infix
open Tyxml_js

(* TODO
   - add hotkeys legend
   - add stats inside the side sheet
   - add settings inside the side sheet
   - add switch to the editor mode
 *)

module Markup = Page_mosaic_video_tyxml.Make(Xml)(Svg)(Html)
module Hotkeys = Page_mosaic_video_tyxml.Hotkeys.Make(Xml)(Svg)(Html)

module Selectors = struct
  let side_sheet_icon = "." ^ Markup.CSS.side_sheet_icon
end

module Janus = struct
  open Janus

  type t =
    { session : Session.t
    ; video : Plugin.t
    ; audio : Plugin.t
    ; event : string React.event
    }

  type track =
    { id : int
    ; description : string
    ; video : Janus_streaming.Mp_rtp.video option
    ; audio : Janus_streaming.Mp_rtp.audio option
    }

  type media =
    { main : track
    ; opt : track list
    }

  let server =
    let location = Dom_html.window##.location in
    let protocol = Js.to_string location##.protocol in
    let hostname = Js.to_string location##.hostname in
    protocol ^ "//" ^ hostname ^ ":8088/janus"

  let main =
    { id = 1
    ; description = "Video plus alarm audio"
    ; video =
        Some { videomcast = None
             ; videoport = 5004
             ; videopt = 100
             ; videortpmap = "H264/90000" (* FIXME should be configurable *)
             ; videofmtp = None
             ; videoiface = None
             ; videobufferkf = None }
    ; audio =
        Some { audiomcast = None
             ; audioport = 5005
             ; audiopt = 111
             ; audiortpmap = "opus/48000/2" (* FIXME should be configurable *)
             ; audiofmtp = None
             ; audioiface = None }
    }

  let opt =
    { id = 2
    ; description = "Program 1 audio"
    ; video = None
    ; audio =
        Some { audiomcast = None
             ; audioport = 5006
             ; audiopt = 111
             ; audiortpmap = "opus/48000/2"
             ; audiofmtp = None
             ; audioiface = None }
    }

  module MP = struct
    open Janus_streaming

    let track_to_create_req (track : track) : Mp_create.t =
      let (base : Janus_streaming.Mp_base.t) =
        { id = Some track.id
        ; name = None
        ; description = Some track.description
        ; is_private = false
        ; audio = Option.is_some track.audio
        ; video = Option.is_some track.video
        ; data = false
        } in
      let (rtp : Janus_streaming.Mp_rtp.t) =
        { base
        ; audio = track.audio
        ; video = track.video
        ; data = None
        } in
      { type_ = Rtp rtp
      ; admin_key = None
      ; secret = None
      ; pin = None
      ; permanent = true
      }

    let send ?jsep (plugin : Plugin.t) (req : 'a) : ('b, string) Lwt_result.t =
      plugin#send ?jsep req request_to_obj parse_response

    let create (plugin : Plugin.t) (req : Mp_create.t)
        : (Mp_create.r, string) Lwt_result.t =
      send plugin (Create req)

    let watch ?secret (plugin : Plugin.t) (id : int)
        : (unit, string) Lwt_result.t =
      send plugin (Watch { id; secret })

    let switch (plugin : Plugin.t) (id : int)
        : (unit, string) Lwt_result.t =
      send plugin (Switch id)
  end

  let create_session ?log_level ()
      : (Session.t * Session.e React.event, string) Lwt_result.t =
    Lwt.catch (fun () ->
        init ?log_level ()
        >>= (Session.create ~server:(`One server))
        >|= Result.return)
      (fun exn ->
        let err = match exn with
          | Janus.Not_created s ->
             Printf.sprintf "WebRTC session is not created:\n %s" s
          | e -> Ui_templates.Loader.exn_to_string e in
        Lwt.return_error err)

  let handle_jsep (plugin : Plugin.t) = function
    | Session.Unknown _ -> Lwt.return_error "Unknown jsep received"
    | Answer x -> plugin#handle_remote_jsep x
    | Offer x ->
       plugin#create_answer Janus_streaming.default_media_props x
       >>= (function
            | Ok jsep -> MP.send ~jsep plugin Start
            | Error e ->
               Printf.printf "Error creating answer: %s\n" e;
               Lwt.return_ok ())

  let handle_plugin ~tracks plugin : unit Lwt.t =
    List.iter (fun (x : track) ->
        MP.create plugin (MP.track_to_create_req x)
        |> Lwt_result.map_err (Printf.printf "failure creating mp: %s\n")
        |> Lwt.ignore_result) tracks;
    match List.head_opt tracks with
    | None -> Lwt.return_unit
    | Some (x : track) ->
       MP.watch plugin x.id
       |> Lwt_result.map_err failwith
       |> Lwt_result.get_exn

  let create_plugin ?on_remote_stream
        ~(tracks : track list)
        ~(target : #Dom_html.mediaElement Js.t)
        (session : Session.t)
      : (Plugin.t * Plugin.e React.event, string) Lwt_result.t =
    let on_jsep p = Fun.(Lwt.ignore_result % handle_jsep p) in
    Lwt.catch (fun () ->
        session#attach
          ~typ:Plugin.Streaming
          ~on_remote_stream:(fun stream ->
            Janus.attach_media_stream target stream;
            Option.iter (fun f -> f stream) on_remote_stream)
          ~on_jsep
          ()
        >>= fun (plugin, e) -> handle_plugin ~tracks plugin
        >>= fun () -> Lwt.return_ok (plugin, e))
      (fun exn ->
        let err = match exn with
          | Janus.Not_created s ->
             Printf.sprintf "WebRTC plugin is not created:\n %s" s
          | e -> Ui_templates.Loader.exn_to_string e in
        Lwt.return_error err)

  let start_webrtc (player : Player.t) =
    Lwt_result.Infix.(
      Lwt.catch (fun () ->
          create_session ~log_level:Debug ()
          >>= fun (session, se) ->
          create_plugin ~tracks:[main]
            ~target:player#video_element
            session
          >>= fun (video, ve) ->
          create_plugin ~tracks:[opt]
            ~target:player#audio_element
            session
          >>= fun (audio, ae) ->
          let se' =
            React.E.map (function
                | Session.Err e -> e
                | Destroyed -> "WebRTC session is destroyed") se in
          let event = React.E.select [ se'; ve; ae ] in
          Lwt.return_ok { session; video; audio; event })
        Fun.(Lwt.return_error % Ui_templates.Loader.exn_to_string))

end

let tie_side_sheet_with_trigger (scaffold : Scaffold.t) : unit Lwt.t option =
  let hotkeys =
    Widget.create
    @@ Tyxml_js.To_dom.of_element
    @@ Markup.create_hotkeys () in
  let cancel = new Button.t ~label:"Закрыть" () in
  let (dialog : Dialog.t) =
    new Dialog.t
      ~scrollable:true
      ~title:"Быстрые клавиши"
      ~actions:[Dialog.Action.make ~typ:`Cancel cancel]
      ~content:(`Widgets [hotkeys])
      () in
  Dom.appendChild Dom_html.document##.body dialog#root;
  let show = new Button.t ~label:"Hotkeys" () in
  show#listen_click_lwt' (fun _ _ -> dialog#show_await () >|= fun _ -> ());
  match Element.query_selector scaffold#root Selectors.side_sheet_icon,
        scaffold#side_sheet with
  | Some i, Some side_sheet ->
     side_sheet#append_child show;
     Some (Events.listen_lwt i Events.Typ.click (fun _ _ ->
               side_sheet#toggle_await ()))
  | _ -> None

let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let player = match scaffold#body with
    | None -> failwith "no video player element found"
    | Some x -> Player.attach x#root in
  Option.iter Lwt.ignore_result
  @@ tie_side_sheet_with_trigger scaffold;
  Janus.start_webrtc player
  >|= (function
       | Ok (j : Janus.t) ->
          (* Show error overlay in case of failure during playback *)
          Lwt_react.E.next j.event >|= (fun s ->
           let ph = Ui_templates.Placeholder.Err.make ~text:s () in
           player#set_overlay ph)
          |> Lwt.ignore_result;
          player#root##focus
       | Error e ->
          (* Show error overlay in case of failure while starting webrtc session *)
          let ph = Ui_templates.Placeholder.Err.make ~text:e () in
          ph#add_class Player.Markup.CSS.overlay;
          player#append_child ph)
  |> Lwt.ignore_result
