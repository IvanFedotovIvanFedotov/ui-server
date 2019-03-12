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
    }

  type track =
    { id : int
    ; description : string
    ; video : Streaming.Mp_rtp.video option
    ; audio : Streaming.Mp_rtp.audio option
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
    open Streaming

    let track_to_create_req (track : track) : Mp_create.t =
      let (base : Mp_base.t) =
        { id = Some track.id
        ; name = None
        ; description = Some track.description
        ; is_private = false
        ; audio = Option.is_some track.audio
        ; video = Option.is_some track.video
        ; data = false
        } in
      let (rtp : Mp_rtp.t) =
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
      Plugin.send_message
        ~message:(Js.Unsafe.obj @@ request_to_obj (Create req))
        plugin
      >|= function
      | Ok None -> Error "empty response"
      | Ok Some d -> Mp_create.of_js_obj d
      | Error e -> Error e

    let watch ?secret (plugin : Plugin.t) (id : int)
        : (unit, string) Lwt_result.t =
      let req = Mp_watch.{ id; secret } in
      Plugin.send_message
        ~message:(Js.Unsafe.obj @@ request_to_obj (Watch req))
        plugin
      >|= function
      | Ok _ -> Ok ()
      | Error e -> Error e

    let start ?jsep (plugin : Plugin.t) =
      Plugin.send_message
        ?jsep
        ~message:(Js.Unsafe.obj @@ request_to_obj Start)
        plugin
      >|= function
      | Ok _ -> Ok ()
      | Error e -> Error e

  end

  let handle_jsep (jsep : Webrtc._RTCSessionDescriptionInit Js.t)
        (plugin : Plugin.t) =
    match Js.to_string jsep##._type with
    | "answer" -> Plugin.handle_remote_jsep jsep plugin
    | "offer" ->
       (let video = Media.make_video ~recv:true ~send:(`Bool false) () in
        let audio = Media.make_audio ~recv:true ~send:(`Bool false) () in
        let media = Media.make ~audio ~video () in
        Plugin.create_answer ~jsep (`Create media) plugin
        >>= function
        | Ok jsep -> MP.start ~jsep plugin
        | Error e -> Lwt.return_error e)
    | s -> Lwt.return_error @@ Printf.sprintf "Unknown jsep received: %s" s

  let start_webrtc (player : Player.t) : (t, string) Lwt_result.t =
    let open Janus in
    Lwt.Infix.(
      Janus.create_session
        ~server
        ~on_error:(fun s ->
          let ph = Ui_templates.Placeholder.Err.make ~text:s () in
          player#set_overlay ph)
        (create ~log_level:Debug ())
      >>= function
      | Error e -> Lwt_result.fail e
      | Ok session ->
         Session.attach_plugin
           ~typ:Streaming
           ~on_message:(fun ?jsep _ (plugin : Plugin.t) ->
             match jsep with
             | None -> ()
             | Some jsep ->
                (handle_jsep jsep plugin
                 >|= function
                 | Error e ->
                    let ph = Ui_templates.Placeholder.Err.make ~text:e () in
                    player#set_overlay ph
                 | Ok _ -> ())
                |> Lwt.ignore_result)
           ~on_remote_stream:(fun stream ->
             Janus.attach_media_stream player#video_element stream)
           session
         >>= (function
              | Ok (plugin : Plugin.t) ->
                 List.map (fun (x : track) ->
                     MP.create plugin (MP.track_to_create_req x)
                     |> Lwt_result.map_err (Printf.printf "failure creating mp: %s\n")
                     |> Lwt.map (fun _ -> ()))
                   [main]
                 |> Lwt.join
                 >>= fun () -> MP.watch plugin main.id
                 >>= (fun _ ->
                   Lwt.return_ok { session
                                 ; video = plugin
                                 ; audio = plugin })
              | Error e -> Lwt_result.fail e))

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
       | Ok (_ : Janus.t) -> player#root##focus
       | Error e ->
          (* Show error overlay in case of failure while starting webrtc session *)
          let ph = Ui_templates.Placeholder.Err.make ~text:e () in
          ph#add_class Player.Markup.CSS.overlay;
          player#append_child ph)
  |> Lwt.ignore_result
