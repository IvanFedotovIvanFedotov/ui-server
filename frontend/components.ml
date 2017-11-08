module Widgets = Common.Components.Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

                                       [@@@ocaml.warning "-60"]

let of_dom el = Tyxml_js.Of_dom.of_element (el :> Dom_html.element Js.t)

module Button = struct

  include Widgets.Button

  class type t = Dom_html.buttonElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_button elt

end

module Card = struct

  include Widgets.Card

  class type t = Dom_html.divElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_div elt

end

module Checkbox = struct

  include Widgets.Checkbox

  class type t =
    object
      method root__         : Dom_html.divElement Js.t Js.readonly_prop
      method checked_       : bool Js.t Js.prop
      method indeterminate_ : bool Js.t Js.prop
      method disabled_      : bool Js.t Js.prop
      method value_         : Js.js_string Js.prop
    end

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.checkbox##.MDCCheckbox##attachTo elt

end

module Dialog = struct

  include Widgets.Dialog

  class type foundation =
    object
      method open_    : unit -> unit Js.meth
      method close_   : unit -> unit Js.meth
      method is_open_ : unit -> bool Js.t Js.meth
      method accept_  : bool Js.t -> unit Js.meth
      method cancel_  : bool Js.t -> unit Js.meth
    end

  class type t =
    object
      method root__      : Dom_html.element Js.t Js.readonly_prop
      method foundation_ : foundation Js.t Js.readonly_prop
      method open_       : bool Js.t Js.prop
      method show_       : unit -> unit Js.meth
      method close_      : unit -> unit Js.meth
    end

  type events =
    { accept : Dom_html.event Js.t Dom_events.Typ.typ
    ; cancel : Dom_html.event Js.t Dom_events.Typ.typ
    }

  let events = { accept = Dom_events.Typ.make "MDCDialog:accept"
               ; cancel = Dom_events.Typ.make "MDCDialog:cancel"
               }

  let attach (elt : Html_types.aside Tyxml_js.Html.elt) : t Js.t =
    Js.Unsafe.global##.mdc##.dialog##.MDCDialog##attachTo elt

end

module Drawer = struct

  include Widgets.Drawer

end

module Elevation = struct

  include Widgets.Elevation

end

module Fab = struct

  include Widgets.Fab

  class type t = Dom_html.buttonElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_button elt

end

module Form_field = struct

  include Widgets.Form_field

  class type t = Dom_html.divElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_div elt

end

module Grid_list = struct

  include Widgets.Grid_list

  class type t = Dom_html.divElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_div elt

end

module Icon_toggle = struct

  include Widgets.Icon_toggle

  class type component =
    object
      method on_       : bool Js.t Js.prop
      method disabled_ : bool Js.t Js.prop
    end

  class type t =
    object
      method root__     : Dom_html.element Js.t Js.readonly_prop
      method component_ : component Js.t Js.readonly_prop
    end

  class type change_event =
    object
      inherit Dom_html.event
      method detail_ : < isOn : bool Js.t Js.readonly_prop > Js.t Js.readonly_prop
    end

  type events =
    { change : change_event Js.t Dom_events.Typ.typ
    }

  let events =
    { change = Dom_events.Typ.make "MDCIconToggle:change"
    }

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.iconToggle##.MDCIconToggle##attachTo elt

end

module Layout_grid = struct

  include Widgets.Layout_grid

  class type t = Dom_html.divElement

  let attach elt : t Js.t = Tyxml_js.To_dom.of_div elt

end

module Linear_progress = struct

  include Widgets.Linear_progress

  class type t =
    object
      method root__       : Dom_html.divElement Js.t Js.readonly_prop
      method determinate_ : bool Js.t Js.writeonly_prop
      method progress_    : Js.number Js.t Js.writeonly_prop
      method buffer_      : Js.number Js.t Js.writeonly_prop
      method reverse_     : bool Js.t Js.writeonly_prop
      method open_        : unit -> unit Js.meth
      method close_       : unit -> unit Js.meth
    end

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.linearProgress##.MDCLinearProgress##attachTo elt

end

module List_ = struct

  include Widgets.List_

  class type t = Dom_html.element

  let attach elt : t Js.t = Tyxml_js.To_dom.of_element elt

end

module Menu = struct

  include Widgets.Menu

  let focus_index_to_js_obj x : < focusIndex : Js.number Js.t Js.prop > Js.t =
    Js.Unsafe.(obj [| "focusIndex", inject @@ Js.number_of_float (float_of_int x) |])

  class type t =
    object
      method root__       : Dom_html.divElement Js.t Js.readonly_prop
      method open_        : bool Js.t Js.prop
      method hide_        : unit -> unit Js.meth
      method show_        : unit -> unit Js.meth
      method show_focused : < focusIndex : Js.number Js.t Js.prop > Js.t -> unit Js.meth
    end

  class type event =
    object
      inherit Dom_html.event
      method detail_ : < item_  : Dom_html.element Js.t Js.readonly_prop;
                         index_ : Js.number Js.t Js.readonly_prop > Js.t Js.readonly_prop
    end

  type events =
    { selected : event Js.t Dom_events.Typ.typ
    ; cancel   : Dom_html.event Js.t Dom_events.Typ.typ
    }

  let events =
    { selected = Dom_events.Typ.make "MDCSimpleMenu:selected"
    ; cancel   = Dom_events.Typ.make "MDCSimpleMenu:cancel"
    }

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.menu##.MDCSimpleMenu##attachTo elt


end

module Radio = struct

  include Widgets.Radio

  class type t =
    object
      method root__    : Dom_html.divElement Js.t Js.readonly_prop
      method checked_  : bool Js.t Js.prop
      method disabled_ : bool Js.t Js.prop
      method value_    : Js.js_string Js.prop
    end

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.radio##.MDCRadio##attachTo elt

end

module Ripple = struct

  include Widgets.Ripple

  class type t =
    object
      method root__      : Dom_html.element Js.t Js.readonly_prop
      method activate_   : unit -> unit Js.meth
      method deactivate_ : unit -> unit Js.meth
      method layout_     : unit -> unit Js.meth
      method unbounded_  : bool Js.t Js.prop
    end

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.ripple##.MDCRipple##attachTo elt

end

module Rtl = struct

  include Widgets.Rtl

end

module Select = struct

  include Widgets.Select

  class type t = Dom_html.element

end

module Slider = struct

  include Widgets.Slider

  class type t =
    object
      method root__         : Dom_html.divElement Js.t Js.readonly_prop
      method value_         : Js.number Js.t Js.prop
      method min_           : Js.number Js.t Js.prop
      method max_           : Js.number Js.t Js.prop
      method step_          : Js.number Js.t Js.prop
      method disabled_      : bool Js.t Js.prop
      method layout_        : unit -> unit Js.meth
      method stepUp_        : unit -> unit Js.meth
      method stepDown_      : unit -> unit Js.meth
      method stepUp_value   : Js.number Js.t -> unit Js.meth
      method stepDown_value : Js.number Js.t -> unit Js.meth
    end

  class type event =
    object
      inherit Dom_html.event
      method detail_ : t Js.t Js.readonly_prop
    end

  type events =
    { input  : event Js.t Dom_events.Typ.typ
    ; change : event Js.t Dom_events.Typ.typ
    }

  let events =
    { input  = Dom_events.Typ.make "MDCSlider:input"
    ; change = Dom_events.Typ.make "MDCSlider:change"
    }

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.slider##.MDCSlider##attachTo elt

end

module Snackbar = struct

  include Widgets.Snackbar

  type data =
    { message          : string
    ; timeout          : int option
    ; action           : action option
    ; multiline        : multiline option
    }
   and action    = { handler : unit -> unit
                   ; text    : string
                   }
   and multiline = { enable           : bool
                   ; action_on_bottom : bool
                   }

  class type data_obj =
    object
      method actionHandler  : (unit -> unit) Js.optdef Js.readonly_prop
      method actionOnBottom : bool Js.t Js.optdef Js.readonly_prop
      method actionText     : Js.js_string Js.t Js.optdef Js.readonly_prop
      method message        : Js.js_string Js.t Js.readonly_prop
      method multiline      : bool Js.t Js.optdef Js.readonly_prop
      method timeout        : Js.number Js.t Js.optdef Js.readonly_prop
    end

  let data_to_js_obj x : data_obj Js.t =
    object%js
      val message        = Js.string x.message
      val timeout        = CCOpt.map (fun x -> Js.number_of_float @@ float_of_int x) x.timeout |> Js.Optdef.option
      val actionHandler  = CCOpt.map (fun x -> x.handler) x.action |> Js.Optdef.option
      val actionText     = CCOpt.map (fun x -> Js.string x.text) x.action |> Js.Optdef.option
      val multiline      = CCOpt.map (fun x -> Js.bool x.enable) x.multiline |> Js.Optdef.option
      val actionOnBottom = CCOpt.map (fun x -> Js.bool x.action_on_bottom) x.multiline |> Js.Optdef.option
    end

  class type t =
    object
      method root__             : Dom_html.divElement Js.t Js.readonly_prop
      method show_              : data_obj Js.t -> unit Js.meth
      method dismissesOnAction_ : bool Js.t Js.prop
    end

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.snackbar##.MDCSnackbar##attachTo elt

end

module Switch = struct

  include Widgets.Switch

  class type t =
    object
      inherit Dom_html.element
      method is_checked_   : unit -> bool Js.t Js.meth
      method set_checked_  : bool Js.t -> unit Js.meth
      method is_disabled_  : unit -> bool Js.t Js.meth
      method set_disabled_ : bool Js.t -> unit Js.meth
      method get_value_    : unit -> Js.js_string Js.meth
      method set_value_    : Js.js_string -> unit Js.meth
    end

  let get_input (switch : t Js.t) =
    switch##querySelector (Js.string ("." ^ native_control_class))

  let attach elt : t Js.t =
    let (elt : t Js.t) = elt |> Tyxml_js.To_dom.of_element |> Js.Unsafe.coerce in
    let set    = fun (x : t Js.t) (name : string) f -> Js.Unsafe.set x name f in
    set elt "is_checked"   @@ Js.wrap_callback (fun () -> match Js.Opt.to_option @@ get_input elt with
                                                          | Some nc -> (Js.Unsafe.coerce nc)##.checked
                                                          | None    -> Js._false);
    set elt "set_checked"  @@ Js.wrap_callback (fun x -> match Js.Opt.to_option @@ get_input elt with
                                                         | Some nc -> (Js.Unsafe.coerce nc)##.checked := x
                                                         | None    -> ());
    set elt "is_disabled"  @@ Js.wrap_callback (fun () -> match Js.Opt.to_option @@ get_input elt with
                                                          | Some nc -> (Js.Unsafe.coerce nc)##.disabled
                                                          | None    -> Js._false);
    set elt "set_disabled" @@ Js.wrap_callback (fun x -> match Js.Opt.to_option @@ get_input elt with
                                                         | Some nc -> (Js.Unsafe.coerce nc)##.disabled := x
                                                         | None    -> ());
    set elt "get_value"    @@ Js.wrap_callback (fun () -> match Js.Opt.to_option @@ get_input elt with
                                                          | Some nc -> (Js.Unsafe.coerce nc)##.value
                                                          | None    -> Js.string "");
    set elt "set_value"    @@ Js.wrap_callback (fun x -> match Js.Opt.to_option @@ get_input elt with
                                                         | Some nc -> (Js.Unsafe.coerce nc)##.value := x
                                                         | None    -> ());
    elt


end

module Tabs = struct

  include Widgets.Tabs

  class type component =
    object
    end

  class type t =
    object
      inherit Dom_html.element
      method component_ : component Js.t Js.readonly_prop
    end

  module Scroller_ = struct

    include Widgets.Tabs.Scroller

    class type component =
      object
      end

    class type scroller =
      object
        inherit Dom_html.element
        method component_ : component Js.t Js.readonly_prop
      end

    let create ?id ?style ?classes ?attrs ~tabs () : scroller Js.t =
      let (elt : scroller Js.t) = create ?id ?style ?classes ?attrs ~tabs ()
                                  |> Tyxml_js.To_dom.of_element
                                  |> Js.Unsafe.coerce in
      let set = fun (x : scroller Js.t) (name : string) f -> Js.Unsafe.set x name f in
      set elt "component" @@ Js.Unsafe.(fun_call (js_expr "mdc.tabs.MDCTabBarScroller.attachTo") [| inject elt |]);
      elt


  end

  let create ?id ?style ?classes ?attrs ?with_indicator ?color_scheme ~_type ~content () : t Js.t =
    let (elt : t Js.t) = create ?id ?style ?classes ?attrs ?with_indicator ?color_scheme ~_type ~content ()
                         |> Tyxml_js.To_dom.of_element
                         |> Js.Unsafe.coerce in
      let set = fun (x : t Js.t) (name : string) f -> Js.Unsafe.set x name f in
      set elt "component" @@ Js.Unsafe.(fun_call (js_expr "mdc.tabs.MDCTabBar.attachTo") [| inject elt |]);
      elt

end

module Textfield = struct

  include Widgets.Textfield

  class type t =
    object
      method root__ : Dom_html.divElement Js.t Js.readonly_prop
    end

  let attach elt : t Js.t =
    Js.Unsafe.global##.mdc##.textfield##.MDCTextfield##attachTo elt

end

module Toolbar = struct

  include Widgets.Toolbar

  class type component =
    object
      method fixedAdjustElement : Dom_html.element Js.t Js.prop
    end

  class type t =
    object
      inherit Dom_html.element
      method component_            : component Js.t Js.readonly_prop
      method set_fixed_adjust_elt_ : Dom_html.element Js.t -> unit Js.meth
      method get_fixed_adjust_elt_ : unit -> Dom_html.element Js.t Js.meth
    end

  class type detail =
    object
      method flexibleExpansionRatio : Js.number Js.t Js.readonly_prop
    end

  class type event =
    object
      inherit Dom_html.event
      method detail_ : detail Js.t Js.readonly_prop
    end

  type events =
    { change : event Js.t Dom_events.Typ.typ
    }

  let events =
    { change = Dom_events.Typ.make "MDCToolbar:change"
    }

  let create ?id ?style ?classes ?attrs ?fixed ?fixed_last_row ?waterfall
             ?flexible ?flexible_height ~content () =
    let (elt : t Js.t) = create ?id ?style ?classes ?attrs ?fixed ?fixed_last_row ?waterfall
                                ?flexible ?flexible_height ~content ()
                         |> Tyxml_js.To_dom.of_element
                         |> Js.Unsafe.coerce in
    let set = fun (x : t Js.t) (name : string) f -> Js.Unsafe.set x name f in
    set elt "component" @@ Js.Unsafe.(fun_call (js_expr "mdc.toolbar.MDCToolbar.attachTo") [| inject elt |]);
    set elt "set_fixed_adjust_elt" @@ Js.wrap_callback (fun x  -> elt##.component_##.fixedAdjustElement := x);
    set elt "get_fixed_adjust_elt" @@ Js.wrap_callback (fun () -> elt##.component_##.fixedAdjustElement);
    elt

end

module Typography = struct

  include Widgets.Typography

end
