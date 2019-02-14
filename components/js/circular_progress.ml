open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_tyxml.Circular_progress.Make(Xml)(Svg)(Html)

module Attr = struct
  let now = "aria-valuenow"
  let max = "aria-valuemax"
  let min = "aria-valuemin"
end

module Selectors = struct
  let circle = "." ^ Markup.CSS.circle
end

let pi = 4.0 *. atan 1.0

class t (elt : #Dom_html.element Js.t) () =
  object(self)

    inherit Widget.t elt () as super

    (* DOM nodes *)
    val circle =
      Option.get_exn
      @@ Option.map Js.Unsafe.coerce
      @@ Element.query_selector elt Selectors.circle

    (* Reactive events *)
    val _e_value = React.E.create ()
    val mutable _s_value = None

    val mutable _min : float = 0.
    val mutable _max : float = 1.
    val mutable _value : float = 0.

    method! init () : unit =
      super#init ()

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      let min' =
        Option.get_or ~default:_min
        @@ Option.flat_map float_of_string_opt
        @@ super#get_attribute Attr.min in
      let max' =
        Option.get_or ~default:_max
        @@ Option.flat_map float_of_string_opt
        @@ Element.get_attribute elt Attr.max in
      if min' >=. self#max
      then (self#set_max max'; self#set_min min')
      else (self#set_min min'; self#set_max max');
      _value <- (Option.get_or ~default:_value
                 @@ Option.flat_map float_of_string_opt
                 @@ super#get_attribute Attr.now);

    method min : float =
      _min

    method set_min (x : float) : unit =
      if x >. self#max
      then raise (Invalid_argument "Min cannot be greater than max")
      else (
        _min <- x;
        self#set_value_ ~force:true self#value;
        super#set_attribute Attr.min (Markup.string_of_float x))

    method max : float =
      _max

    method set_max (x : float) : unit =
      if x <. self#min
      then raise (Invalid_argument "Max cannot be less than min")
      else (
        _max <- x;
        self#set_value_ ~force:true self#value;
        super#set_attribute Attr.max (Markup.string_of_float x))

    method value : float =
      _value

    method set_value (v : float) : unit =
      self#set_value_ v

    method set_indeterminate (x : bool) : unit =
      super#toggle_class ~force:x Markup.CSS.indeterminate;
      if x
      then (circle##.style##.strokeDashoffset := Js.string "";
            circle##.style##.strokeDasharray := Js.string "")
      else self#set_value_ ~force:true self#value

    method indeterminate : bool =
      super#has_class Markup.CSS.indeterminate

    method e_value : float React.event =
      fst _e_value

    method s_value : float React.signal =
      match _s_value with
      | Some s -> s
      | None ->
         let s = React.S.hold ~eq:Float.equal self#value self#e_value in
         _s_value <- Some s;
         s

    method show () : unit =
      super#style##.display := Js.string ""

    method hide () : unit =
      super#style##.display := Js.string "none"

    (* Private methods *)

    method private set_value_ ?(force = false) (v : float) : unit =
      let min, max, prev = self#min, self#max, self#value in
      let v = Utils.clamp ~min ~max v in
      if force || v <>. prev
      then (
        _value <- v;
        super#set_attribute Attr.now (Markup.string_of_float v);
        self#update_ui_for_value ();
        (snd _e_value) v)

    method private update_ui_for_value () : unit =
      let min, max, value = self#min, self#max, self#value in
      let rel_val = (value -. min) /. (max -. min) *. 100. in
      let circumference = 2. *. pi *. (Markup.sz /. 2. -. 5.) in
      let dash_offset = Float.(
          (round ((100. - rel_val) / 100. * circumference * 1000.)) / 1000.) in
      let dash_array = Float.(
          (round (circumference * 1000.)) / 1000.) in
      let dash_offset' = Js.string (Printf.sprintf "%gpx" dash_offset) in
      let dash_array' = Js.string (Printf.sprintf "%g" dash_array) in
      circle##.style##.strokeDashoffset := dash_offset';
      circle##.style##.strokeDasharray := dash_array';

  end

let make ?min ?max ?value
      ?indeterminate ?thickness ?size () : t =
  let (elt : Element.t) =
    To_dom.of_element
    @@ Markup.create ?min ?max ?value
         ?indeterminate ?thickness ?size () in
  new t elt ()

let attach (elt : #Dom_html.element Js.t) : t =
  new t elt ()
