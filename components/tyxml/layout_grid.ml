type align =
  | Top
  | Middle
  | Bottom

type device =
  | Desktop
  | Phone
  | Tablet

let max_columns = 12

let check_columns_number_exn n =
  if n > max_columns || n < 0
  then failwith "Layout grid: bad columns number"

module CSS = struct
  (** Mandatory, for the layout grid element. *)
  let root = "mdc-layout-grid"

  (** Mandatory, for wrapping grid cell. *)
  let inner = BEM.add_element root "inner"

  (** Optional, specifies the alignment of the whole grid. *)
  let align = function
    | `Left -> BEM.add_modifier root "align-left"
    | `Right -> BEM.add_modifier root "align-right"

  (** Mandatory, for the layout grid cell. *)
  let cell = BEM.add_element root "cell"

  (** Optional, specifies the number of columns the cell spans on a type of device
      (desktop, tablet, phone). *)
  let cell_span ?device (n : int) : string =
    check_columns_number_exn n;
    BEM.add_modifier cell ("span-" ^ string_of_int n)
    |> (fun s ->
      match device with
      | None -> s
      | Some dt ->
         match dt with
         | Desktop -> s ^ "-desktop"
         | Tablet -> s ^ "-tablet"
         | Phone -> s ^ "-phone")

  (** Optional, specifies the order of the cell. *)
  let cell_order (n : int) : string =
    check_columns_number_exn n;
    BEM.add_modifier cell ("order-" ^ string_of_int n)

  (** Optional, specifies the alignment of cell. *)
  let cell_align = function
    | Top -> BEM.add_modifier cell "align-top"
    | Middle -> BEM.add_modifier cell "align-middle"
    | Bottom -> BEM.add_modifier cell "align-bottom"

  (** Optional, specifies the grid should have fixed column width. *)
  let fixed_column_width = BEM.add_modifier root "fixed-column-width"
end

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html
  open Utils

  let create_cell ?(classes = []) ?attrs ?align ?order
        ?span ?span_phone ?span_tablet ?span_desktop
        content () : 'a elt =
    let (classes : string list) =
      CSS.cell :: classes
      |> map_cons_option CSS.cell_span span
      |> map_cons_option (CSS.cell_span ~device:Phone) span_phone
      |> map_cons_option (CSS.cell_span ~device:Tablet) span_tablet
      |> map_cons_option (CSS.cell_span ~device:Desktop) span_desktop
      |> map_cons_option CSS.cell_align align
      |> map_cons_option CSS.cell_order order in
    div ~a:([a_class classes] <@> attrs) content

  let create_inner ?(classes = []) ?attrs ~cells () : 'a elt =
    let classes = CSS.inner :: classes in
    div ~a:([a_class classes] <@> attrs) cells

  let create ?(classes = []) ?attrs ?align
        ?(fixed_column_width = false) ~inner () : 'a elt =
    let (classes : string list) =
      classes
      |> map_cons_option CSS.align align
      |> cons_if fixed_column_width CSS.fixed_column_width
      |> List.cons CSS.root in
    div ~a:([a_class classes] <@> attrs) inner
end
