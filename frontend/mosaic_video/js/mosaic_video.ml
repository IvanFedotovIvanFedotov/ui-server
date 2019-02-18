open Js_of_ocaml
open Containers
open Components
open Lwt.Infix

module Markup = Page_mosaic_video_tyxml

module Selectors = struct
  let side_sheet_icon = "." ^ Markup.CSS.side_sheet_icon
end

module Janus = struct

  open Janus_static

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

  let create_session ?(debug = `All false) ()
      : (Session.t, string) Lwt_result.t =
    Lwt.catch (fun () ->
        init debug
        >>= Fun.(fst % create ~server:(`One server))
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

  let handle_plugin ~tracks plugin : unit Lwt.t =
    List.iter (fun (x : track) ->
        MP.create plugin (MP.track_to_create_req x)
        |> Lwt_result.map_err (Printf.printf "failure creating mp: %s\n")
        |> Lwt.ignore_result) tracks;
    Lwt.return_unit

  let create_plugin ~(tracks : track list)
        ~(target : #Dom_html.mediaElement Js.t)
        (session : Session.t)
      : (Plugin.t, string) Lwt_result.t =
    let on_jsep p = Fun.(Lwt.ignore_result % handle_jsep p) in
    Lwt.catch (fun () ->
        Session.attach ~session
          ~typ:Plugin.Streaming
          ~on_remote_stream:(Janus.attachMediaStream target)
          ~on_jsep
          ()
        >>= fun plugin -> handle_plugin ~tracks plugin
        >>= fun () -> Lwt.return_ok plugin)
      (fun exn ->
        let err = match exn with
          | Janus_static.Not_created s ->
             Printf.sprintf "WebRTC plugin is not created:\n %s" s
          | e -> Ui_templates.Loader.exn_to_string e in
        Lwt.return_error err)

end

type janus =
  { session : Janus_static.Session.t
  ; video : Janus_static.Plugin.t
  ; audio : Janus_static.Plugin.t option
  }

let start_webrtc (player : Player.t) =
  Lwt_result.Infix.(
    Lwt.catch (fun () ->
        Janus.create_session ()
        >>= fun session ->
        Janus.create_plugin ~tracks:[Janus.main]
          ~target:player#video_element
          session
        >>= fun video ->
        (match player#audio_element with
         | None -> Lwt_result.return None
         | Some target ->
            Janus.create_plugin ~tracks:[Janus.opt]
              ~target
              session
            >|= Option.return)
        >>= fun audio -> Lwt.return_ok { session; video; audio })
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
       | Ok (_ : janus) -> player#root##focus
       | Error e ->
          let ph = Ui_templates.Placeholder.Err.make ~text:e () in
          ph#add_class Player.Markup.CSS.overlay;
          player#append_child ph)
  |> Lwt.ignore_result
