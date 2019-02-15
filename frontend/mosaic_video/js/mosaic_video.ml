open Js_of_ocaml
open Containers
open Components
open Common
open Lwt.Infix

module CSS = struct
  let root = "mosaic"
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

  let session ?(debug = `All false) () =
    init debug
    >>= (fun () -> fst @@ create ~server:(`One server) ())

  let handle_jsep (plugin : Plugin.t) = function
    | Session.Offer x ->
       Plugin.create_answer plugin Janus_streaming.default_media_props None x
       >>= (function
            | Ok jsep -> Janus_streaming.send ~jsep plugin Start
            | Error e ->
               Printf.printf "Error creating answer: %s\n" e;
               Lwt.return_ok ())
    | Answer x -> Plugin.handle_remote_jsep plugin x
    | Unknown _ -> Lwt.return_error "Unknown jsep received"

  let handle_plugin ~selected ~tracks plugin : unit Lwt.t =
    List.iter (fun x ->
        let (base : Janus_streaming.Mp_base.t) =
          { id = Some x.id
          ; name = None
          ; description = Some x.description
          ; is_private = false
          ; audio = Option.is_some x.audio
          ; video = Option.is_some x.video
          ; data = false
          } in
        let (rtp : Janus_streaming.Mp_rtp.t) =
          { base
          ; audio = x.audio
          ; video = x.video
          ; data = None
          } in
        let (req : Janus_streaming.Mp_create.t) =
          { type_ = Rtp rtp
          ; admin_key = None
          ; secret = None
          ; pin = None
          ; permanent = true
          } in
        Janus_streaming.send plugin (Create req)
        >|= (function
             | Ok _ -> ()
             | Error e -> Printf.printf "failure creating mp: %s\n" e)
        |> Lwt.ignore_result) tracks;
    React.S.changes selected
    |> React.E.map_s (fun x -> Janus_streaming.send plugin (Switch x.id))
    |> React.E.keep;
    let init = React.S.value selected in
    Janus_streaming.send plugin (Watch { id = init.id; secret = None })
    |> Lwt_result.map_err (fun x -> failwith x)
    |> Lwt_result.get_exn

  let plugin ~(tracks : track list)
        ~(selected : track React.signal)
        ~(target : #Dom_html.element Js.t)
        (session : Session.t)
      : Plugin.t Lwt.t =
    let on_jsep p = Fun.(Lwt.ignore_result % handle_jsep p) in
    Session.attach ~session
      ~typ:Plugin.Streaming
      ~on_remote_stream:(Janus.attachMediaStream target)
      ~on_jsep
      ()
    >>= fun plugin -> handle_plugin ~tracks ~selected plugin
    >|= (fun () -> plugin)

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

let load (player : Player.t) =
  Lwt.catch
    (fun () ->
      Janus.session ()
      >>= fun session ->
       let (video : Janus_static.Plugin.t) =
         Janus.plugin ~tracks:[Janus.main]
           ~selected:(React.S.const Janus.main)
           ~target:player#video_element
           s in
       let (audio : Janus_static.Plugin.t option) =
         match player#audio_element with
         | None -> None
         | Some audio ->
            Some (Janus.plugin ~tracks:[Janus.opt]
                    ~selected:(React.S.const Janus.opt)
                    ~target:audio
                    s) in
       Lwt.join
         [ 
         ; 
         ]
       >>= Lwt.return_ok (video, audio)))
    (fun exn ->
      let err = match exn with
        | Janus_static.Not_created s ->
           Printf.sprintf "WebRTC session is not created:\n %s" s
        | e -> Printexc.to_string e in
      Lwt.return_error err)

let () =
  let scaffold = Scaffold.attach (Dom_html.getElementById "root") in
  let player = match scaffold#body with
    | None -> failwith "no video player element found"
    | Some x -> Player.attach x#root in
  load player
  >|= (function
       | Ok () -> player#root##focus
       | Error e ->
          let ph = Ui_templates.Placeholder.Err.make ~text:e () in
          ph#add_class Player.Markup.CSS.overlay;
          player#append_child ph)
  |> Lwt.ignore_result;
