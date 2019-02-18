open Js_of_ocaml
open Containers
open Components
open Common
open Lwt.Infix
open Janus_static
open Tyxml_js

(* TODO
   - add hotkeys legend
   - add stats inside the side sheet
   - add settings inside the side sheet
   - add switch to the editor mode
 *)

module Markup = Page_mosaic_video_tyxml.Make(Xml)(Svg)(Html)

module Selectors = struct
  let side_sheet_icon = "." ^ Markup.CSS.side_sheet_icon
end

module Janus = struct

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

  let mp_create_of_track (track : track) : Janus_streaming.Mp_create.t =
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

  let create_session ?(debug = `All false) ()
      : (Session.t * Session.e React.event, string) Lwt_result.t =
    Lwt.catch (fun () ->
        init debug
        >>= (create ~server:(`One server))
        >|= Result.return)
      (fun exn ->
        let err = match exn with
          | Janus_static.Not_created s ->
             Printf.sprintf "WebRTC session is not created:\n %s" s
          | e -> Ui_templates.Loader.exn_to_string e in
        Lwt.return_error err)

  let handle_jsep (plugin : Plugin.t) = function
    | Session.Unknown _ -> Lwt.return_error "Unknown jsep received"
    | Answer x -> Plugin.handle_remote_jsep plugin x
    | Offer x ->
       Plugin.create_answer plugin Janus_streaming.default_media_props None x
       >>= (function
            | Ok jsep -> Janus_streaming.send ~jsep plugin Start
            | Error e ->
               Printf.printf "Error creating answer: %s\n" e;
               Lwt.return_ok ())

  let handle_plugin ~selected ~tracks plugin : unit Lwt.t =
    List.iter (fun (x : track) ->
        let req = mp_create_of_track x in
        Janus_streaming.send plugin (Create req)
        |> Lwt_result.map_err (Printf.printf "failure creating mp: %s\n")
        |> Lwt.ignore_result) tracks;
    React.S.changes selected
    |> React.E.map_s (fun x -> Janus_streaming.send plugin (Switch x.id))
    |> React.E.keep;
    let init = React.S.value selected in
    Janus_streaming.send plugin (Watch { id = init.id; secret = None })
    |> Lwt_result.map_err (fun x -> failwith x)
    |> Lwt_result.get_exn

  let create_plugin ~(tracks : track list)
        ~(selected : track React.signal)
        ~(target : #Dom_html.element Js.t)
        (session : Session.t)
      : (Plugin.t * Plugin.e React.event, string) Lwt_result.t =
    let on_jsep p = Fun.(Lwt.ignore_result % handle_jsep p) in
    Lwt.catch (fun () ->
        Session.attach ~session
          ~typ:Plugin.Streaming
          ~on_remote_stream:(Janus.attachMediaStream target)
          ~on_jsep
          ()
        >>= fun (plugin, e) -> handle_plugin ~tracks ~selected plugin
        >>= fun () -> Lwt.return_ok (plugin, e))
      (fun exn ->
        let err = match exn with
          | Janus_static.Not_created s ->
             Printf.sprintf "WebRTC plugin is not created:\n %s" s
          | e -> Ui_templates.Loader.exn_to_string e in
        Lwt.return_error err)

  let main =
    { id = 1
    ; description = "Video plus alarm audio"
    ; video =
        Some { videomcast = None
             ; videoport = 5004
             ; videopt = 100
             ; videortpmap = "VP9/90000" (* FIXME should be configurable *)
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

end

type janus =
  { session : Session.t
  ; video : Plugin.t
  ; audio : Plugin.t option
  ; event : string React.event
  }

let start_webrtc (player : Player.t) =
  Lwt_result.Infix.(
    Lwt.catch (fun () ->
        Janus.create_session ()
        >>= fun (session, se) ->
        Janus.create_plugin ~tracks:[Janus.main]
          ~selected:(React.S.const Janus.main)
          ~target:player#video_element
          session
        >>= fun (video, ve) ->
        (match player#audio_element with
         | None -> Lwt_result.return (None, React.E.never)
         | Some target ->
            Janus.create_plugin ~tracks:[Janus.opt] ~target
              ~selected:(React.S.const Janus.opt) session
            >|= fun (t, e) -> Some t, e)
        >>= fun (audio, ae) ->
        let se' =
          React.E.map (function
              | Session.Err e -> e
              | Destroyed -> "WebRTC session is destroyed") se in
        let event = React.E.select [ se'; ve; ae ] in
        Lwt.return_ok { session; video; audio; event })
      Fun.(Lwt.return_error % Ui_templates.Loader.exn_to_string))

let tie_side_sheet_with_trigger (scaffold : Scaffold.t) : unit Lwt.t option =
  match Element.query_selector scaffold#root Selectors.side_sheet_icon,
        scaffold#side_sheet with
  | Some i, Some side_sheet ->
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
  start_webrtc player
  >|= (function
       | Ok (j : janus) ->
          player#root##focus;
          (* Show error overlay in case of failure during playback *)
          Lwt_react.E.next j.event >|= (fun s ->
            let ph = Ui_templates.Placeholder.Err.make ~text:s () in
            player#set_overlay ph)
          |> Lwt.ignore_result
       | Error e ->
          (* Show error overlay in case of failure while starting webrtc session *)
          let ph = Ui_templates.Placeholder.Err.make ~text:e () in
          ph#add_class Player.Markup.CSS.overlay;
          player#append_child ph)
  |> Lwt.ignore_result
