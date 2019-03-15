open Components_tyxml
open Utils

module CSS = struct

  let root = "player"

  (* Element playing audio from the selected service *)
  let audio = CSS.add_element root "audio"

  (* Element playing video and alarm audio *)
  let video = CSS.add_element root "video"

  let gradient = root ^ "-controls-gradient"
  let overlay = CSS.add_element root "overlay"
  let state_overlay = CSS.add_element root "state-overlay"
  let state_overlay_icon = CSS.add_element root "state-overlay-icon"
  let state_overlay_wrapper = CSS.add_element root "state-overlay-wrapper"
  let big_button = CSS.add_element root "big-button"

  let autohide = CSS.add_modifier root "autohide"
  let paused = CSS.add_modifier root "paused"
  let playing = CSS.add_modifier root "playing"
  let big_mode = CSS.add_modifier root "big-mode"

  module Controls = struct
    let root = root ^ "-controls"
    let action = CSS.add_element root "action"
    let section = CSS.add_element root "section"

    let section_start = CSS.add_modifier section "align-start"
    let section_end = CSS.add_modifier section "align-end"
    let action_play = CSS.add_modifier action "play"
    let action_mute = CSS.add_modifier action "mute"
    let action_fullscreen = CSS.add_modifier action "fullscreen"

    let volume_panel = CSS.add_element root "volume-panel"
    let volume = CSS.add_element root "volume"
  end

end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  module Icon = Icon.Make(Xml)(Svg)(Html)
  module Icon_button = Icon_button.Make(Xml)(Svg)(Html)

  module Path = struct
    open Icon.SVG.Path
    let video = video
    let play = play
    let pause = pause
    let fullscreen = fullscreen
    let fullscreen_exit = fullscreen_exit
    let volume_off = volume_off
    let volume_low = volume_low
    let volume_medium = volume_medium
    let volume_high = volume_high
  end

  module Controls = struct

    type align = [`Start | `End]

    let create_action ?(classes = []) ?attrs ?ripple ?disabled
          ?on_icon ~icon () : 'a elt =
      let classes = CSS.Controls.action :: classes in
      Icon_button.create ?attrs ?ripple ?disabled
        ?on_icon ~classes ~icon ()

    let create_volume_panel ?(classes = []) ?attrs content () : 'a elt =
      let classes = CSS.Controls.volume_panel :: classes in
      div ~a:([a_class classes] <@> attrs) content

    let create_section ?(classes = []) ?attrs ?(align : align option)
          content () : 'a elt =
      let classes =
        classes
        |> map_cons_option (function
               | `End -> CSS.Controls.section_end
               | `Start -> CSS.Controls.section_start) align
        |> List.cons CSS.Controls.section in
      div ~a:([a_class classes] <@> attrs) content

    let create ?(classes = []) ?attrs sections () : 'a elt =
      let classes = CSS.Controls.root :: classes in
      div ~a:([a_class classes] <@> attrs) sections

  end

  let create_audio ?(classes = []) ?attrs
        ?(controls = true)
        ?(autoplay = false)
        ?(playsinline = false)
        () : 'a elt =
    let classes = CSS.audio :: classes in
    audio ~a:(
        [ a_class classes
        ; Unsafe.string_attrib "preload" "auto" ]
        |> cons_if_lazy controls a_controls
        |> cons_if_lazy autoplay a_autoplay
        |> cons_if_lazy playsinline (fun () ->
               Unsafe.string_attrib "playsinline" "true")
        <@> attrs) []

  let create_video ?(classes = []) ?attrs
        ?(autoplay = false)
        ?(playsinline = false)
        ?(controls = true)
        () : 'a elt =
    let classes = CSS.video :: classes in
    video ~a:(
        [ a_class classes
        ; Unsafe.string_attrib "preload" "auto" ]
        |> cons_if_lazy controls a_controls
        |> cons_if_lazy autoplay a_autoplay
        |> cons_if_lazy playsinline (fun () ->
               Unsafe.string_attrib "playsinline" "true")
        <@> attrs) []

  let create_state_overlay ?(classes = []) ?attrs path () : 'a elt =
    let classes = CSS.state_overlay_wrapper :: classes in
    div ~a:([ a_class classes
            ; a_style "display: none;" ]
            <@> attrs)
      [div ~a:([a_class [CSS.state_overlay]])
         [Icon.SVG.create ~classes:[CSS.state_overlay_icon]
            [Icon.SVG.create_path path ()] ()]]

  let create_gradient ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.gradient :: classes in
    div ~a:([a_class classes] <@> attrs) []

  let create ?(classes = []) ?attrs ?controls ?gradient
        ?state_overlay ?audio ~video ()
      : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([ a_class classes
            ; a_tabindex (-1) ]
            <@> attrs)
      (video :: (audio ^:: state_overlay ^:: gradient ^:: controls ^:: []))

end
