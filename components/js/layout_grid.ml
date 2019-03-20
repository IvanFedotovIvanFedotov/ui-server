open Js_of_ocaml
open Utils

include Components_tyxml.Layout_grid
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

module Cell = struct

  let parse_span (c : string) =
    let pre = Components_tyxml.BEM.add_modifier CSS.cell "span" in
    match String.split_on_char '-' c with
    | [prefix; span] when String.equal prefix pre ->
       begin match int_of_string_opt span with
       | Some x when x > 0 && x <= max_columns -> Some (x, `All)
       | _ -> None
       end
    | [prefix; span; device] when String.equal prefix pre ->
       begin match int_of_string_opt span with
       | Some x when x > 0 && x <= max_columns ->
          begin match device with
          | "phone" -> Some (x, `Phone)
          | "tablet" -> Some (x, `Tablet)
          | "desktop" -> Some (x, `Desktop)
          | _ -> None
          end
       | _ -> None
       end
    | _ -> None

  class t (elt : Dom_html.element Js.t) () =
  object(self)

    inherit Widget.t elt () as super

    val mutable _span : int option = None
    val mutable _span_phone : int option = None
    val mutable _span_tablet : int option = None
    val mutable _span_desktop : int option = None

    val mutable align : align option = None
    val mutable order : int option = None

    method! initial_sync_with_dom () : unit =
      super#initial_sync_with_dom ();
      let classes = Element.classes super#root in
      let span, span_phone, span_tablet, span_desktop =
        List.fold_left (fun ((all, phone, tablet, desktop) as acc) (c : string) ->
            match parse_span c with
            | Some (x, `All) -> (Some x, phone, tablet, desktop)
            | Some (x, `Phone) -> (all, Some x, tablet, desktop)
            | Some (x, `Tablet) -> (all, phone, Some x, desktop)
            | Some (x, `Desktop) -> (all, phone, tablet, Some x)
            | None -> acc)
          (None, None, None, None) classes in
      _span <- span;
      _span_phone <- span_phone;
      _span_tablet <- span_tablet;
      _span_desktop <- span_desktop

    method! layout () : unit =
      super#layout ();

    method span : int option =
      _span

    method set_span : int option -> unit = function
      | Some x ->
         Option.iter self#rm_span _span;
         super#add_class @@ CSS.cell_span x;
         _span <- Some x
      | None   ->
         Option.iter self#rm_span _span;
         _span <- None

    method span_phone : int option =
      _span_phone

    method set_span_phone : int option -> unit = function
      | Some x ->
         Option.iter (self#rm_span ~dt:Phone) _span_phone;
         super#add_class @@ CSS.cell_span ~device:Phone x;
         _span_phone <- Some x
      | None   ->
         Option.iter (self#rm_span ~dt:Phone) _span_phone;
         _span_phone <- None

    method span_tablet : int option =
      _span_tablet

    method set_span_tablet : int option -> unit = function
      | Some x ->
         Option.iter (self#rm_span ~dt:Tablet) _span_tablet;
         super#add_class @@ CSS.cell_span ~device:Tablet x;
         _span_tablet <- Some x
      | None   ->
         Option.iter (self#rm_span ~dt:Tablet) _span_tablet;
         _span_tablet <- None

    method span_desktop : int option =
      _span_desktop

    method set_span_desktop : int option -> unit = function
      | Some x ->
         Option.iter (self#rm_span ~dt:Desktop) _span_desktop;
         super#add_class @@ CSS.cell_span ~device:Desktop x;
         _span_tablet <- Some x
      | None   ->
         Option.iter (self#rm_span ~dt:Desktop) _span_desktop;
         _span_desktop <- None

    method order : int option =
      order

    method set_order : int option -> unit = function
      | Some x ->
         Option.iter (super#remove_class % CSS.cell_order) order;
         super#add_class @@ CSS.cell_order x;
         order <- Some x
      | None   ->
         Option.iter (super#remove_class % CSS.cell_order) order;
         order <- None

    method align : align option =
      align

    method set_align : align option -> unit = function
      | Some x ->
         Option.iter (super#remove_class % CSS.cell_align) align;
         super#add_class @@ CSS.cell_align x;
         align <- Some x
      | None   ->
         Option.iter (super#remove_class % CSS.cell_align) align;
         align <- None

    method private rm_span ?dt x =
      super#remove_class @@ CSS.cell_span ?device:dt x
  end

  let make ?span ?span_phone ?span_tablet ?span_desktop ?align ?order
        (widgets : #Widget.t list) : t =
    let (elt : Dom_html.element Js.t) =
      Tyxml_js.To_dom.of_element
      @@ Markup.create_cell ?span ?span_phone ?span_tablet ?span_desktop
           ?align ?order (List.map Widget.to_markup widgets) () in
    new t elt ()

  let attach (elt : #Dom_html.element Js.t) : t =
    new t (Element.coerce elt) ()
end

class t ?align ~(cells : Cell.t list) () =
  let inner =
    Markup.create_inner ~cells:(List.map Widget.to_markup cells) ()
    |> To_dom.of_div
    |> Widget.create in
  let (elt : Dom_html.element Js.t) =
    Markup.create ~content:[Widget.to_markup inner] ()
    |> To_dom.of_div in

  object(self)
    inherit Widget.t elt () as super

    val mutable _cells = cells

    val mutable _align : [ `Left | `Right ] option = align

    method! layout () : unit =
      super#layout ();
      inner#layout ();
      List.iter (fun x -> x#layout ()) _cells

    method inner = inner
    method cells = _cells

    method insert_cell_at_idx (i : int) (x : Cell.t) =
      _cells <- List.add_nodup ~eq:Widget.equal x _cells;
      self#inner#insert_child_at_idx i x

    method append_cell (x : Cell.t) =
      _cells <- List.add_nodup ~eq:Widget.equal x _cells;
      self#inner#append_child x

    method remove_cell (x : Cell.t) =
      _cells <- List.remove ~eq:Widget.equal x _cells;
      self#inner#remove_child x

    method align = _align
    method set_align : [ `Left | `Right ] option -> unit = function
      | Some x ->
         Option.iter (super#remove_class % Markup.get_grid_align) _align;
         super#add_class @@ Markup.get_grid_align x;
         _align <- Some x
      | None   ->
         Option.iter (super#remove_class % Markup.get_grid_align) _align;
         _align <- None

    method set_fixed_column_width x =
      super#toggle_class ~force:x Markup.fixed_column_width_class

  end
