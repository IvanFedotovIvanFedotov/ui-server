open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Components
open Pc_control_types

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.bind

let name = "Routes"

let make_dialog () =
  let accept =
    Button.attach
    @@ Dialog.make_action ~action:Accept ~label:"Добавить" () in
  let check_input address mask =
    (match address#value, mask#value with
     | Some _, Some _ -> accept#set_disabled false
     | _ -> accept#set_disabled true);
    Lwt.return_unit in
  let address =
    Textfield.make_textfield
      ~label:"IP адрес"
      Util.ipv4_validation in
  let mask =
    Textfield.make_textfield
      ~label:"Маска подсети"
      Util.mask_validation in
  let listeners = Lwt_js_events.(
      [ inputs address#input_element (fun _ _ -> check_input address mask)
      ; inputs mask#input_element (fun _ _ -> check_input address mask)
      ]) in
  let title =
    Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.create_title_simple
      ~title:"Добавление статического маршрута"
      () in
  let content =
    Tyxml_js.To_dom.of_element
    @@ Dialog.Markup.create_content
      ~content:[ address#markup
               ; mask#markup ]
      () in
  let actions =
    Dialog.[ Element.coerce (make_action ~action:Close ~label:"Отмена" ())
           ; accept#root
           ] in
  let dialog = Dialog.make ~classes:[Markup.CSS.dialog]
      ~title ~content ~actions () in
  dialog#set_on_destroy (fun () -> List.iter Lwt.cancel listeners);
  dialog,
  (fun () ->
     check_input address mask
     >>= fun () -> dialog#open_await ()
     >>= function
     | Close | Destroy | Custom _ -> Lwt.return_none
     | Accept ->
       match address#value, mask#value with
       | Some addr, Some mask -> Lwt.return_some (addr, mask)
       | _ -> Lwt.return_none)

let parse_address (s : string) =
  match String.split_on_char '/' s with
  | [addr; mask] ->
    begin match Ipaddr.V4.of_string addr, int_of_string_opt mask with
      | Ok addr, Some mask ->
        if mask >= 0 && mask <= 32
        then Some (addr, Int32.of_int mask)
        else None
      | _ -> None
    end
  | _ -> None

module Selector = struct
  let routes_list = Printf.sprintf ".%s" Item_list.CSS.root
  let button = Printf.sprintf ".%s" Card.CSS.action
end

class t (elt : Dom_html.element Js.t) = object(self)

  val routes_list : Item_list.t =
    match Element.query_selector elt Selector.routes_list with
    | None -> failwith @@ name ^ ": routes list element not found"
    | Some x -> Item_list.attach x
  val add : Button.t =
    match Element.query_selector elt Selector.button with
    | None -> failwith @@ name ^ ": add button element not found"
    | Some x -> Button.attach x
  val dialog : Dialog.t * (unit -> Network_config.address option Lwt.t) =
    make_dialog ()

  val mutable _ripples = []
  val mutable _listeners = []

  inherit Widget.t elt () as super

  method! init () : unit =
    _ripples <- List.map (fun x -> x, Ripple.attach x) routes_list#items;
    Dom.appendChild Dom_html.document##.body (fst dialog)#root;
    super#init ()

  method! initial_sync_with_dom () : unit =
    _listeners <- Lwt_js_events.(
        [ seq_loop (make_event Item_list.Event.action) super#root
            self#handle_item_action
        ; clicks add#root (fun _ _ ->
              (snd dialog) ()
              >>= function
              | None -> Lwt.return_unit
              | Some route -> self#append_route route; Lwt.return_unit)
        ]);
    super#initial_sync_with_dom ()

  method set_value (x : Network_config.address list) : unit =
    let rec aux = function
      | [], routes -> List.iter self#append_route routes
      | items, [] -> List.iter self#remove_item items
      | (item : Dom_html.element Js.t) :: x, (addr, mask) :: y ->
        let s = Format.asprintf "%a/%ld" Ipaddr.V4.pp addr mask in
        let children = Dom.list_of_nodeList item##.childNodes in
        let text = List.find (fun x -> match x##.nodeType with
            | Dom.TEXT -> true
            | _ -> false) children in
        (Js.Unsafe.coerce text)##.textContent := Js.some @@ Js.string s;
        aux (x, y) in
    aux (routes_list#items, x)

  method value : Network_config.address list =
    List.filter_map (fun (x : Dom_html.element Js.t) ->
        Js.Opt.case x##.textContent
          (fun () -> None)
          (parse_address % Js.to_string))
      routes_list#items

  method private append_route (x : Network_config.address) =
    let item =
      Tyxml_js.To_dom.of_element
      @@ Markup.Routes.make_item x in
    Element.append_child routes_list#root item;
    _ripples <- (item, Ripple.attach item) :: _ripples;
    routes_list#layout ()

  method private remove_item (item : Dom_html.element Js.t) : unit =
    (match List.find_opt (Element.equal item % fst) _ripples with
     | None -> ()
     | Some (_, r) ->
       _ripples <- List.filter (Element.equal item % fst) _ripples;
       Ripple.destroy r);
    Element.remove_child_safe routes_list#root item

  method private handle_item_action e _ =
    let detail = Widget.event_detail e in
    let original_target = Dom.eventTarget detail##.originalEvent in
    if Element.has_class original_target Item_list.CSS.item_meta
    then self#remove_item detail##.item;
    Lwt.return_unit

end

let make (init : Network_config.ipv4_conf) : t =
  let (elt : Dom_html.element Js.t) =
    Tyxml_js.To_dom.of_element
    @@ Markup.Routes.make init in
  new t elt
