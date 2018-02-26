open Containers

type 'a tab =
  { href     : string option
  ; content  : content
  ; disabled : bool
  ; value    : 'a
  }
and content = [ `Text of string | `Icon of string * (string option) | `Text_and_icon of string * string ]

module Tab = struct

  class ['a] t push (props:'a tab) () =

    let elt = Markup.Tabs.Tab.create ~content:props.content () |> Tyxml_js.To_dom.of_a in

    object(self)

      inherit Widget.widget elt () as super

      val mutable tab    = props
      val mutable active = false
      val mutable prevent_default_on_click = false
      val mutable value : 'a = props.value

      method anchor_element = elt

      method set_value (x:'a) = value <- x
      method get_value : 'a   = value

      method get_disabled   = tab.disabled
      method set_disabled x = tab <- { tab with disabled = x };
                              let a = "disabled" in
                              if x then self#set_attribute a "true" else self#remove_attribute a

      method get_href   = Js.to_string self#anchor_element##.href
      method set_href s = self#anchor_element##.href := Js.string s

      method get_content : content = tab.content
      method set_content (x : content) =
        match self#get_content,x with
        | `Text _, `Text x ->
           super#set_text_content x;
           Result.return ()
        | `Icon _, `Icon (i,fallback) ->
           let icon = super#get_child_element_by_class Markup.Tabs.Tab.icon_class in
           (match icon with
            | Some icon -> icon##.textContent := Js.some @@ Js.string i;
                           Option.iter (fun x -> icon##setAttribute (Js.string "aria-label") (Js.string x)) fallback;
                           Result.return ()
            | None      -> Result.fail "icon element not found")
        | `Text_and_icon _, `Text_and_icon (t,i) ->
           let icon = super#get_child_element_by_class Markup.Tabs.Tab.icon_class in
           let text = super#get_child_element_by_class Markup.Tabs.Tab.icon_text_class in
           (match icon,text with
            | Some icon, Some text -> icon##.textContent := Js.some @@ Js.string i;
                                      text##.textContent := Js.some @@ Js.string t;
                                      Result.return ()
            | None, _ -> Result.fail "icon element not found"
            | _, None -> Result.fail "text element not found")
        | _  -> Result.fail "tab content type mismatch"

      method get_active   = active
      method set_active x = active <- x;
                            if x
                            then (self#add_class Markup.Tabs.Tab.active_class;
                                  push (Some self))
                            else self#remove_class Markup.Tabs.Tab.active_class

      method get_width = self#get_offset_width
      method get_left  = self#get_offset_left

      method get_prevent_default_on_click   = prevent_default_on_click
      method set_prevent_default_on_click x = prevent_default_on_click <- x

      initializer
        Ripple.attach self |> ignore;
        self#set_attribute "tabindex" "0";
        self#set_disabled tab.disabled;
        Option.iter (fun x -> self#set_href x) props.href;
        Dom_events.listen self#root Dom_events.Typ.click (fun _ _ ->
                            self#set_active true;
                            not prevent_default_on_click)
        |> ignore;
        Dom_events.listen self#root Dom_events.Typ.keydown (fun _ (ev:Dom_html.keyboardEvent Js.t) ->
                            let key  = Option.map Js.to_string @@ Js.Optdef.to_option ev##.key in
                            (match key,ev##.keyCode with
                             | Some "Enter", _ | _, 13 -> self#set_active true
                             | _ -> ());
                            true)
        |> ignore

    end

end

module Tab_bar = struct

  class ['a] t ~(tabs:'a tab list) () =

    let s_active,s_active_push = React.S.create None in
    let tabs      = List.map (fun x -> new Tab.t s_active_push x ()) tabs in
    let indicator = new Widget.widget (Markup.Tabs.Tab_bar.Indicator.create () |> Tyxml_js.To_dom.of_span) () in
    let typ_of_tab_content = function
      | `Text _          -> `Text
      | `Icon _          -> `Icon
      | `Text_and_icon _ -> `Text_and_icon
    in
    let typ = List.map (fun x -> typ_of_tab_content x#get_content) tabs
              |> List.sort_uniq ~cmp:Pervasives.compare
              |> (function
                  | [x] -> x
                  | _   -> failwith "All tabs must be of the same type: text, icon or text with icon") in
    let elt = Markup.Tabs.Tab_bar.create ~typ
                                         ~indicator:(Widget.widget_to_markup indicator)
                                         ~tabs:(Widget.widgets_to_markup tabs) ()
              |> Tyxml_js.To_dom.of_nav in
    let ()  = Option.iter (fun x -> x#set_active true) (List.head_opt tabs) in

    object(self)

      inherit Widget.widget elt () as super

      val mutable init         = false
      val mutable tabs         = tabs
      val mutable layout_frame = None
      val mutable width        = 0

      method typ : [ `Text | `Icon | `Text_and_icon ] = typ
      method indicator_widget = indicator
      method tabs             = tabs
      method s_active         = s_active


      method set_indicator_default = super#remove_class Markup.Tabs.Tab_bar.indicator_accent_class;
                                     super#remove_class Markup.Tabs.Tab_bar.indicator_primary_class
      method set_indicator_primary = super#remove_class Markup.Tabs.Tab_bar.indicator_accent_class;
                                     super#add_class Markup.Tabs.Tab_bar.indicator_primary_class
      method set_indicator_accent  = super#remove_class Markup.Tabs.Tab_bar.indicator_primary_class;
                                     super#add_class Markup.Tabs.Tab_bar.indicator_accent_class

      (* Active getters *)

      method get_active_tab_index = match React.S.value s_active with
        | Some tab -> List.find_idx (fun x -> Equal.physical x tab) self#tabs
                      |> (function Some (idx,_) -> Some idx | None -> None)
        | None     -> None
      method get_active_tab = React.S.value s_active
      method get_active_value = Option.map (fun x -> x#get_value) self#get_active_tab

      (* Active setters *)

      method set_active_tab_index x = match List.get_at_idx x self#tabs with
        | Some tab -> Ok (tab#set_active true)
        | None     -> Error (Printf.sprintf "set_active_tab_index: tab with index %d not found" x)
      method set_active_tab tab = match List.find_idx (fun x -> Equal.physical x tab) self#tabs with
        | Some (_,tab) -> Ok (tab#set_active true)
        | None         -> Error "set_active_tab: tab not found"

      method get_tab_at_index i = List.get_at_idx i self#tabs

      method append_tab (tab : 'a tab) =
        if Equal.poly (typ_of_tab_content tab.content) self#typ
        then (let t = new Tab.t s_active_push tab () in
              tabs <- tabs @ [t];
              Dom.appendChild self#root t#root;
              self#layout;
              Ok ())
        else (Error "append_tab: tab content mismatch")

      method insert_tab_at_index index (tab : 'a tab) =
        if Equal.poly (typ_of_tab_content tab.content) self#typ
        then (let t = new Tab.t s_active_push tab () in
              tabs <- List.insert_at_idx index t tabs;
              let tab_elts = self#root##.childNodes in
              let item     = tab_elts##item index in
              let _        = match Js.Opt.to_option item with
                | Some _ -> self#root##insertBefore (t#root :> Dom.node Js.t) item
                | None   -> self#root##appendChild  (t#root :> Dom.node Js.t)
              in
              self#layout;
              Ok ())
        else (Error "insert_tab_at_index: tab content mismatch")

      method remove_tab_at_index i = match self#get_tab_at_index i with
        | Some tab -> tabs <- List.remove_at_idx i tabs;
                      Dom.removeChild self#root tab#root;
                      if tab#get_active then s_active_push None;
                      self#layout;
                      Ok ()
        | None     -> Error (Printf.sprintf "remove_tab_at_index: tab with index %d not found" i)

      method layout =
        Option.iter (fun x -> Dom_html.window##cancelAnimationFrame x) layout_frame;
        let f = fun _ -> self#layout_internal; layout_frame <- None in
        layout_frame <- Some (Dom_html.window##requestAnimationFrame (Js.wrap_callback f))

      method private layout_internal = match self#get_active_tab with
        | Some tab ->
           let f () =
             width <- self#get_offset_width;
             let l = tab#get_left in
             let w = match width with
               | 0 -> 0.
               | x -> (float_of_int tab#get_width) /. (float_of_int x)
             in
             let t = Printf.sprintf "translateX(%dpx) scale(%f,1)" l w in
             if not init then (Js.Unsafe.coerce indicator#style)##.transition := Js.string "none";
             indicator#style##.transform := Js.string t;
             if not init then (indicator#get_offset_width |> ignore;
                               (Js.Unsafe.coerce indicator#style)##.transition := Js.string "";
                               indicator#style##.visibility := Js.string "visible";
                               init <- true)
           in
           (match Js.Opt.to_option @@ self#root##.parentNode with
            | None   -> let p_vis = self#style##.visibility in
                        let p_pos = self#style##.position in
                        self#style##.visibility := Js.string "hidden";
                        self#style##.position   := Js.string "absolute";
                        Dom.appendChild Dom_html.document##.body self#root;
                        f ();
                        Dom.removeChild Dom_html.document##.body self#root;
                        self#style##.visibility := p_vis;
                        self#style##.position   := p_pos
            | Some _ -> f ())
        | None -> (Js.Unsafe.coerce indicator#style)##.transition := Js.string "none";
                  indicator#style##.visibility := Js.string "hidden";
                  init <- false

      initializer
        self#add_class (Markup.Tabs.Tab_bar._class ^ "-upgraded");
        Dom_events.listen Dom_html.window Dom_events.Typ.resize (fun _ _ -> self#layout; false) |> ignore;
        React.S.map (fun _   -> self#layout) s_active |> ignore;
        React.E.map (fun tab -> Option.iter (fun x -> x#set_active false) tab) (React.S.diff (fun _ x -> x) s_active)
        |> ignore


    end

end

module Scroller = struct

  class ['a] t ~(tabs:'a tab list) () =

    let tab_bar = new Tab_bar.t ~tabs () in
    let elt     = Markup.Tabs.Scroller.create ~tabs:(Widget.widget_to_markup tab_bar) ()
                  |> Tyxml_js.To_dom.of_div in
    let wrapper = elt##querySelector (Js.string ("." ^ Markup.Tabs.Scroller.scroll_frame_tabs_class))
                  |> Js.Opt.to_option |> Option.get_exn |> Widget.create
    in
    let back    = elt##querySelector (Js.string ("." ^ Markup.Tabs.Scroller.indicator_back_class))
                  |> Js.Opt.to_option |> Option.get_exn |> Widget.create
    in
    let forward = elt##querySelector (Js.string ("." ^ Markup.Tabs.Scroller.indicator_forward_class))
                  |> Js.Opt.to_option |> Option.get_exn |> Widget.create
    in
    let on_scroll_change = (fun _ h -> wrapper#style##.marginBottom := Js.string (Printf.sprintf "-%dpx" h)) in
    let scroll_listener  = new Utils.Scroll_size_listener.t ~on_change:on_scroll_change () in

    object(self)

      inherit Widget.widget elt ()

      (* User methods *)

      method tab_bar        = tab_bar
      method scroll_back    = self#move_tabs_scroll (- wrapper#get_client_width)
      method scroll_forward = self#move_tabs_scroll wrapper#get_client_width
      method layout         = self#update_scroll_buttons;
                              self#tab_bar#layout;
                              scroll_listener#measure

      (* Private methods *)

      method private scroll next =
        let old = wrapper#root##.scrollLeft in
        Utils.Animation.animate ~timing:Utils.Animation.Timing.in_out_sine
                                ~draw:(fun x -> let n = float_of_int next in
                                                let o = float_of_int old in
                                                let v = int_of_float @@ (x *. (n -. o)) +. o in
                                                wrapper#root##.scrollLeft := v)
                                ~duration:0.35

      method private move_tabs_scroll (delta:int) =
        let multiplier = 1 in
        let old        = wrapper#root##.scrollLeft in
        let next       = old + delta * multiplier in
        self#scroll next

      method private scroll_tab_into_view tab =
        let left  = wrapper#root##.scrollLeft in
        let right = left + wrapper#get_client_width in
        if tab#get_left < left
        then self#scroll tab#get_left
        else if tab#get_left + tab#get_width > right
        then self#scroll @@ left + (tab#get_width + tab#get_left - right)

      method private update_scroll_buttons =
        let scroll_width = wrapper#root##.scrollWidth in
        let scroll_left  = wrapper#root##.scrollLeft in
        let client_width = wrapper#root##.clientWidth in
        let show_left    = scroll_left > 0 in
        let show_rigth   = scroll_width > client_width + scroll_left in
        let f x w        = w#add_or_remove_class x Markup.Tabs.Scroller.indicator_enabled_class in
        f show_left back;
        f show_rigth forward

      initializer
        self#layout;
        Dom.appendChild self#root scroll_listener#root;
        Dom_events.listen back#root    Dom_events.Typ.click  (fun _ _ -> self#scroll_back;    true)        |> ignore;
        Dom_events.listen forward#root Dom_events.Typ.click  (fun _ _ -> self#scroll_forward; true)        |> ignore;
        Dom_events.listen wrapper#root Dom_events.Typ.scroll (fun _ _ -> self#update_scroll_buttons; true) |> ignore;
        React.S.map (fun x -> Option.iter self#scroll_tab_into_view x) self#tab_bar#s_active                |> ignore;

    end

end
