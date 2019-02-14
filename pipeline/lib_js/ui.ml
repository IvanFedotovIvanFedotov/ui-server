open Containers
open Components
open Common

module Structure = struct

  let make_pid ?(applied : Structure.pid option) (pid : Structure.pid) =
    let text, stext =
      match pid.content with
      | Empty ->
         "Empty " ^ pid.stream_type_name, ""
      | Audio a ->
         "Аудио " ^ pid.stream_type_name,
         Printf.sprintf "Кодек: %s; Битрейт: %s;" a.codec a.bitrate
      | Video v ->
         "Видео " ^ pid.stream_type_name,
         Printf.sprintf "Кодек: %s; Разрешение: %dx%d;"
           v.codec (fst v.resolution) (snd v.resolution)
    in
    let checkbox = new Checkbox.t ~ripple:false () in
    checkbox#set_checked (Option.is_some applied);
    let signal, push = React.S.create ~eq:Equal.bool (Option.is_some applied) in
    let pid_s = React.S.map ~eq:(Option.equal Structure.equal_pid)
                  (fun b -> if b then Some pid else None)
                  signal
    in
    React.S.map ~eq:Equal.unit push checkbox#s_state
    |> React.S.keep;
    let item =
      new Tree.Item.t
        ~text
        ~secondary_text:stext
        ~graphic:checkbox
        ~value:()
        () in
    item, pid_s

  let make_channel ?(applied : Structure.channel option) (ch : Structure.channel) =
    let make (pid : Structure.pid) =
      let applied =
        let open Option in
        applied >>= fun v ->
        List.find_opt (fun (x : Structure.pid) -> x.pid = pid.pid) v.pids
      in make_pid ?applied pid
    in
    let text, stext =
      Printf.sprintf "%s" ch.service_name,
      Printf.sprintf "Провайдер: %s"  ch.provider_name in
    let wl, sl = List.split @@ List.map make ch.pids in
    let ch_s =
      sl
      |> React.S.merge ~eq:(Equal.list Structure.equal_pid)
           (fun a p -> match p with None -> a | Some p -> p :: a) []
      (* TODO clear this abominable mess *)
      |> React.S.map ~eq:Structure.equal_channel
           (fun pl -> { ch with pids = pl }) in
    let nested = new Tree.t ~items:wl () in
    let e = new Tree.Item.t ~text ~secondary_text:stext ~nested ~value:() ()
    in e, ch_s

  let make_structure ?(applied : Structure.t option) (s : Structure.packed) =
    let make (chan : Structure.channel) =
      let applied =
        let open Option in
        applied >>= fun v ->
        List.find_opt (fun (c : Structure.channel) -> c.number = chan.number) v.channels 
      in make_channel ?applied chan
    in
    let text, stext =
      let h = Printf.sprintf "Поток: %s" (Stream.Source.to_string s.source.source.info)
      in h, (Printf.sprintf "ip: %s" @@ Url.to_string s.structure.uri) in
    let wl, cl = List.split @@ List.map make s.structure.channels in
    let st_s =
      cl
      |> React.S.merge ~eq:(Equal.list Structure.equal_channel)
           (fun a p -> p :: a) []
      |> React.S.map ~eq:Structure.equal
           (fun chl -> { s.structure with channels = chl }) in
    let nested = new Tree.t ~items:wl () in
    let e = new Tree.Item.t ~text ~secondary_text:stext ~nested ~value:() ()
    in e, st_s

  let make_structure_list (applied : Structure.t list) (sl : Structure.packed list) =
    match sl with
    | [] ->
       let ph =
         Ui_templates.Placeholder.create_with_icon
           ~text:"Потоки не обнаружены"
           ~icon:Icon.SVG.(make_simple Path.information) () in
       ph#widget, React.S.const None
    | sl ->
       let make (s : Structure.packed) =
         let open Structure in
         make_structure s
           ?applied:(List.find_opt (fun x -> Url.equal x.uri s.structure.uri) applied)
       in
       let eq = Equal.list Structure.equal in
       let wl, sl = List.split @@ List.map make sl in
       let sl_s = React.S.merge ~eq (fun a p -> p :: a) [] sl in
       let lst = new Tree.t ~items:wl () in
       lst#widget, React.S.map ~eq:(Equal.option eq) Option.return sl_s

  let make
        ~(init_applied : Structure.t list)
        ~(init : Structure.packed list)
        ~(event_applied : Structure.t list React.event)
        ~(event : Structure.packed list React.event)
        () : (Structure.t list, unit) Ui_templates.Types.settings_block =
    let div = Widget.create_div () in
    let make (applied : Structure.t list) (str : Structure.packed list) =
      let dis, s = make_structure_list applied str in
      let place = dis in
      place, s in
    let s_applied = React.S.hold ~eq:(Equal.list Structure.equal) init_applied event_applied in
    let s_in      = React.S.hold ~eq:(Equal.list Structure.equal_packed) init event in
    let eq_s =
      let eq = Equal.option @@ Equal.list Structure.equal in
      React.S.equal ~eq in
    let s_div =
      React.S.l2 ~eq:(fun (w1, s1) (w2, s2) ->
          Widget.equal w1 w2 && eq_s s1 s2)
        (fun applied s -> make applied s) s_applied s_in in
    let s =
      React.S.switch ~eq:(Equal.option @@ Equal.list Structure.equal)
        (React.S.map ~eq:eq_s (fun n ->
             div#set_empty ();
             let tree, n_s = n in
             div#append_child tree;
             n_s) s_div)
    in
    let post = Requests_structure.HTTP.apply_streams in
    div, s, post

end

module Settings = struct

  let make_setting h (s : Settings.setting) =
    let header = Js_of_ocaml.Dom_html.(createH5 document) in
    header##.textContent := Js_of_ocaml.Js.(some @@ string h);
    let peak_en_chck       = new Checkbox.t () in
    let peak_en_field      = new Form_field.t ~label:"Пиковая ошибка" ~input:peak_en_chck () in
    let peak_field         = new Textfield.t
                               ~input_id:"peak_field"
                               ~label:"Значение"
                               ~input_type:(Float ((Some (-100.)),(Some 100.))) () in
    let cont_en_chck       = new Checkbox.t () in
    let cont_en_field      = new Form_field.t ~label:"Длительная ошибка" ~input:cont_en_chck () in
    let cont_field         = new Textfield.t
                               ~input_id:"cont_field"
                               ~label:"Значение"
                               ~input_type:(Float ((Some (-100.)),(Some 100.))) () in
    let dur_field          = new Textfield.t
                               ~input_id:"dur_field"
                               ~label:"Длительность"
                               ~input_type:(Float ((Some (-100.)),(Some 100.))) () in
    peak_en_chck#set_checked s.peak_en;
    cont_en_chck#set_checked s.cont_en;
    peak_field#set_value s.peak;
    cont_field#set_value s.cont;
    dur_field#set_value s.duration;
    let peak_box = new Vbox.t ~widgets:[peak_en_field#widget; peak_field#widget;] () in
    let cont_box = new Vbox.t ~widgets:[cont_en_field#widget; cont_field#widget;] () in
    let box = new Vbox.t ~widgets:[(Widget.create header);
                                   peak_box#widget;
                                   cont_box#widget;
                                   dur_field#widget] () in
    let signal =
      React.S.l5 ~eq:Settings.equal_setting
        (fun peak_en peak cont_en cont dur ->
          Settings.{ peak_en
                   ; peak = Option.get_or ~default:(s.peak) peak
                   ; cont_en
                   ; cont = Option.get_or ~default:(s.cont) cont
                   ; duration = Option.get_or ~default:(s.duration) dur } )
        peak_en_chck#s_state peak_field#s_input
        cont_en_chck#s_state cont_field#s_input
        dur_field#s_input in
    box, signal

  let make_black (b : Settings.black) =
    let header = Js_of_ocaml.Dom_html.(createH5 document) in
    header##.textContent := Js_of_ocaml.Js.(some @@ string "Чёрный кадр");
    let black_w, black_s = make_setting "Доля чёрных пикселей" b.black in
    let luma_w, luma_s = make_setting "Средняя яркость" b.luma in
    let bpixel_field =
      new Textfield.t
        ~label:"Чёрный пиксель"
        ~input_type:(Integer ((Some 1), (Some 256))) () in
    bpixel_field#set_value b.black_pixel;
    let box =
      new Vbox.t ~widgets:[(Widget.create header);
                           black_w#widget;
                           luma_w#widget;
                           bpixel_field#widget] () in
    let signal =
      React.S.l3 ~eq:Settings.equal_black (fun black luma bpixel ->
          match bpixel with
          | None -> Settings.{ black; luma; black_pixel = b.black_pixel }
          | Some black_pixel -> Settings.{ black; luma; black_pixel } )
        black_s luma_s bpixel_field#s_input in
    box, signal

  let make_freeze (f : Settings.freeze) =
    let header = Js_of_ocaml.Dom_html.(createH5 document) in
    header##.textContent := Js_of_ocaml.Js.(some @@ string "Заморозка видео");
    let freeze_w, freeze_s = make_setting "Доля идентичных пикселей" f.freeze in
    let diff_w, diff_s = make_setting "Средняя разность" f.diff in
    let pixeld_field =
      new Textfield.t
        ~label:"Идентичный пиксель"
        ~input_type:(Integer ((Some 1), (Some 256))) () in
    pixeld_field#set_value f.pixel_diff;
    let box = new Vbox.t ~widgets:[(Widget.create header);
                                   freeze_w#widget;
                                   diff_w#widget;
                                   pixeld_field#widget] () in
    let signal =
      React.S.l3 ~eq:Settings.equal_freeze
        (fun freeze diff pixeld ->
          match pixeld with
          | None -> Settings.{ freeze; diff; pixel_diff = f.pixel_diff }
          | Some pixel_diff -> Settings.{ freeze; diff; pixel_diff } )
        freeze_s diff_s pixeld_field#s_input in
    box, signal

  let make_blocky (b : Settings.blocky) =
    let header = Js_of_ocaml.Dom_html.(createH5 document) in
    header##.textContent := Js_of_ocaml.Js.(some @@ string "Блочность");
    let blocky_w, blocky_s = make_setting "Блочность" b.blocky in
    let box = new Vbox.t ~widgets:[(Widget.create header);
                                   blocky_w#widget] () in
    let signal =
      React.S.map ~eq:Settings.equal_blocky
        (fun blocky -> Settings.{ blocky })
        blocky_s in
    box, signal

  let make_video (v : Settings.video) =
    let header = Js_of_ocaml.Dom_html.(createH5 document) in
    header##.textContent := Js_of_ocaml.Js.(some @@ string "Видео");
    let loss_field =
      new Textfield.t
        ~label:"Пропадание видео"
        ~input_type:(Float ((Some 0.), (Some 1.))) () in
    loss_field#set_value v.loss;
    let black_w, black_s   = make_black v.black in
    let freeze_w, freeze_s = make_freeze v.freeze in
    let blocky_w, blocky_s = make_blocky v.blocky in
    let box = new Vbox.t ~widgets:[(Widget.create header);
                                   loss_field#widget;
                                   black_w#widget;
                                   freeze_w#widget;
                                   blocky_w#widget] () in
    let signal =
      React.S.l4 ~eq:Settings.equal_video
        (fun loss black freeze blocky ->
          match loss with
          | None -> Settings.{ loss = v.loss; black; freeze; blocky }
          | Some loss -> Settings.{ loss; black; freeze; blocky } )
        loss_field#s_input black_s freeze_s blocky_s in
    box, signal

  let make_silence (s : Settings.silence) =
    let header = Js_of_ocaml.Dom_html.(createH5 document) in
    header##.textContent := Js_of_ocaml.Js.(some @@ string "Тишина");
    let sil_w, sil_s = make_setting "Громкость" s.silence in
    let box = new Vbox.t ~widgets:[(Widget.create header); sil_w#widget] () in
    let signal =
      React.S.map ~eq:Settings.equal_silence
        (fun silence -> Settings.{ silence } ) sil_s in
    box, signal

  let make_loudness (s : Settings.loudness) =
    let header = Js_of_ocaml.Dom_html.(createH5 document) in
    header##.textContent := Js_of_ocaml.Js.(some @@ string "Перегрузка звука");
    let sil_w, sil_s = make_setting "Громкость" s.loudness in
    let box = new Vbox.t ~widgets:[(Widget.create header); sil_w#widget] () in
    let signal =
      React.S.map ~eq:Settings.equal_loudness
        (fun loudness -> Settings.{ loudness } ) sil_s in
    box, signal

  let make_adv (s : Settings.adv) =
    let header = Js_of_ocaml.Dom_html.(createH5 document) in
    header##.textContent := Js_of_ocaml.Js.(some @@ string "Рекламные вставки");
    let diff =
      new Textfield.t
        ~label:"diff"
        ~input_type:(Float ((Some 0.), (Some 100.))) () in
    let buf =
      new Textfield.t
        ~label:"buf"
        ~input_type:(Integer ((Some 0), (Some 100))) () in
    diff#set_value s.adv_diff;
    buf#set_value s.adv_buf;
    let box =
      new Vbox.t
        ~widgets:[ (Widget.create header)
                 ; diff#widget
                 ; buf#widget]
        () in
    let signal =
      React.S.l2 ~eq:Settings.equal_adv (fun diff buf ->
          Settings.{ adv_diff = Option.get_or ~default:(s.adv_diff) diff
                   ; adv_buf  = Option.get_or ~default:(s.adv_buf) buf })
        diff#s_input buf#s_input in
    box, signal

  let make_audio (a : Settings.audio) =
    let header = Js_of_ocaml.Dom_html.(createH5 document) in
    header##.textContent := Js_of_ocaml.Js.(some @@ string "Аудио");
    let loss_field =
      new Textfield.t
        ~label:"Пропадание аудио"
        ~input_type:(Float ((Some 0.),(Some 1.))) () in
    loss_field#set_value a.loss;
    let silence_w, sil_s = make_silence  a.silence in
    let loudness_w, loud_s = make_loudness a.loudness in
    let _, adv_s = make_adv a.adv in
    let box =
      new Vbox.t ~widgets:[(Widget.create header);
                           loss_field#widget;
                           silence_w#widget;
                           loudness_w#widget; (* adv_w#widget *)] () in
    let signal =
      React.S.l4 ~eq:Settings.equal_audio
        (fun loss silence loudness adv ->
          match loss with
          | None -> Settings.{ loss = a.loss; silence; loudness; adv }
          | Some loss -> Settings.{ loss; silence; loudness; adv } )
        loss_field#s_input sil_s loud_s adv_s in
    box, signal

  let make_layout (s : Settings.t) =
    let header = Js_of_ocaml.Dom_html.(createH5 document) in
    header##.textContent := Js_of_ocaml.Js.(some @@ string "Настройки");
    let v, v_s = make_video s.video in
    let a, a_s = make_audio s.audio in
    let s =
      React.S.l2 ~eq:(Equal.option Settings.equal)
        (fun v a -> Some Settings.{ video = v; audio = a; }) v_s a_s in
    let box =
      new Vbox.t
        ~widgets:[ (Widget.create header)
                 ; v#widget
                 ; a#widget]
        () in
    box, s

  let make ~(init : Settings.t)
        ~(event : Settings.t React.event)
        () : (Settings.t, unit) Ui_templates.Types.settings_block =
    let div = Widget.create_div () in
    let make (set : Settings.t) =
      let dis, s = make_layout set in
      let place = dis in
      place, s
    in
    let eq = Equal.option Settings.equal in
    let eq_s =
      Equal.pair Widget.equal (React.S.equal ~eq) in
    let s_in = React.S.hold ~eq:Settings.equal init event in
    let s_div = React.S.map ~eq:eq_s (fun s -> make s) s_in in
    let s  =
      React.S.switch ~eq
        (React.S.map ~eq:(React.S.equal ~eq)
           (fun n ->
             div#set_empty ();
             let w, n_s = n in
             div#append_child w;
             n_s) s_div)
    in
    let post = Requests_settings.HTTP.set in
    div, s, post
end
