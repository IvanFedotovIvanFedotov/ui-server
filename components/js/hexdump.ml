open Js_of_ocaml
open Utils

include Components_tyxml.Hexdump
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

let elt_to_string (elt : 'a Tyxml.Html.elt) : string =
  Format.asprintf "%a" (Tyxml.Html.pp_elt ()) elt

let make_data id ?(width = 16) ?(grouping = 1) (data : char list) =
  let space cnt =
    if grouping = 0 then false else (cnt mod grouping) = 0 in
  let conv_hex ?value () = to_hex_span ?value config in
  let conv_chr ?value () = to_char_span ?value config in
  let rec aux_empty cnt hex_acc chr_acc = function
    | x when x > 0 ->
       let cnt, hex_acc =
         let hex = hex_acc ^ (conv_hex ()) in
         let hex = if space cnt then hex ^ " " else hex in
         (succ cnt),hex
       in aux_empty cnt hex_acc (chr_acc ^ conv_chr ()) (pred x)
    | _ -> (match (String.chop_suffix ~suf:" " hex_acc) with
            | Some x -> x
            | None -> hex_acc),
           chr_acc in
  let rec aux id cnt hex_acc chr_acc = function
    | [] ->
       let hex_acc, chr_acc =
         aux_empty cnt hex_acc chr_acc (width - cnt + 1)
       in id, hex_acc, chr_acc
    | hd :: tl ->
       let cnt, hex_acc =
         let code = Char.code hd in
         let hex = hex_acc ^ (conv_hex ~value:(id, code) ()) in
         let hex = if space cnt then hex ^ " " else hex in
         (succ cnt), hex
       in
       let chr_acc = chr_acc ^ (conv_chr ~value:(id, hd) ()) in
       aux (succ id) cnt hex_acc chr_acc tl in
  let id, hex, chars = aux id 1 "" "" data in
  id, hex ^ "\n", chars ^ "\n"

class t ?(interactive = true)
        ~(config : config)
        (data : string) () =
  let num_elt = Markup.create_block ()
                |> To_dom.of_element |> Widget.create in
  let hex_elt = Markup.create_block ()
                |> To_dom.of_element |> Widget.create in
  let chr_elt = Markup.create_block ()
                |> To_dom.of_element |> Widget.create in
  object(self)
    val mutable _listener = None
    val mutable _selected : #Dom_html.element Js.t list = []
    val mutable _config : config = config
    val mutable _bytes : string = data

    inherit Hbox.t ~widgets:[num_elt; hex_elt; chr_elt] () as super

    method! init () : unit =
      super#init ();
      self#set_bytes data;
      self#set_interactive interactive;
      num_elt#add_class Markup.line_numbers_block_class;
      hex_elt#add_class Markup.hex_block_class;
      chr_elt#add_class Markup.chars_block_class;
      self#add_class Markup.base_class

    method hex_widget = hex_elt
    method char_widget = chr_elt
    method num_widget = num_elt

    method set_base (x : base) : unit =
      if not (equal_base x _config.base)
      then (_config <- { _config with base = x };
            self#set_bytes _bytes)

    method set_width (x : int) : unit =
      if not (x = _config.width)
      then (_config <- { _config with width = x };
            self#set_bytes _bytes)

    method set_grouping (x : int) : unit =
      if not (x = _config.grouping)
      then (_config <- { _config with grouping = x };
            self#set_bytes _bytes)

    method set_line_numbers (x : bool) : unit =
      if not (Equal.bool x _config.line_numbers)
      then (_config <- { _config with line_numbers = x };
            if x then num_elt#style##.display := Js.string ""
            else num_elt#style##.display := Js.string "none")

    method set_bytes (bytes : string) : unit =
      _bytes <- bytes;
      let rec aux acc bytes = match List.take_drop _config.width bytes with
        | l, [] -> List.rev (l :: acc)
        | l, r -> aux (l :: acc) r in
      let bytes = aux [] (String.to_list bytes) in
      let _, num, hex, chr =
        List.fold_left (fun (id, num, hex, chr) (x : char list) ->
            let num' = to_line_number (id / _config.width) _config in
            let id, hex', chr' = make_data id _config x in
            id, num ^ num' ^ "\n", hex ^ hex',chr ^ chr') (0,"","","") bytes in
      hex_elt#set_inner_html hex;
      chr_elt#set_inner_html chr;
      num_elt#set_inner_html num;

    method select ?(flush = true) (id : int) : unit =
      let eq = fun x -> id = self#_get_hex_id x in
      match List.find_opt eq self#_hex_items with
      | None -> ()
      | Some elt ->
         (match List.find_opt (Equal.physical elt) _selected with
          | Some _ -> () (* already selected *)
          | None  ->
             if flush then List.iter self#_unselect _selected;
             self#_select elt)

    method select_range ?(flush = true) (from : int) (till : int) : unit =
      if till < from || from < 0 || till < 0
      then raise_notrace (Invalid_argument "bad range")
      else
        if flush then List.iter self#_unselect _selected;
      List.iter (self#select ~flush:false) (List.range from till)

    method interactive : bool =
      super#has_class Markup.interactive_class

    method set_interactive (x : bool) : unit =
      super#toggle_class ~force:x Markup.interactive_class;
      match x, _listener with
      | true, None   ->
         _listener <- Some (self#listen_click_lwt self#_on_click)
      | false, Some l -> Lwt.cancel l; _listener <- None
      | _ -> ()


    (* Private methods *)

    method private _hex_items : Dom_html.element Js.t list =
      Dom.list_of_nodeList @@ self#hex_widget#root##.childNodes
      |> List.filter_map (fun x -> match Dom.nodeType x with
                                   | Element e -> Some e
                                   | _         -> None)
      |> List.map Js.Unsafe.coerce
      |> List.filter (fun x ->
             let class' = Js.string Markup.Hex._class in
             Js.to_bool @@ x##.classList##contains class')
      |> List.sort (fun e1 e2 -> compare
                                   (self#_get_hex_id e1)
                                   (self#_get_hex_id e2))

    method private _char_items : Dom_html.element Js.t list =
      Dom.list_of_nodeList @@ self#char_widget#root##.childNodes
      |> List.filter_map (fun x -> match Dom.nodeType x with
                                   | Element e -> Some e
                                   | _         -> None)
      |> List.map Js.Unsafe.coerce
      |> List.filter (fun x ->
             let class' = Js.string Markup.Char._class in
             Js.to_bool @@ x##.classList##contains class')
      |> List.sort (fun e1 e2 -> compare
                                   (self#_get_char_id e1)
                                   (self#_get_char_id e2))

    method private _get_char_id (x : Dom_html.element Js.t) =
      x##getAttribute (Js.string "data-char-id") |> Js.Opt.to_option
      |> Option.map Fun.(int_of_string % Js.to_string)
      |> Option.get_exn

    method private _get_hex_id (x : Dom_html.element Js.t) =
      x##getAttribute (Js.string "data-hex-id") |> Js.Opt.to_option
      |> Option.map Fun.(int_of_string % Js.to_string)
      |> Option.get_exn

    method private _get_hex_char (x : Dom_html.element Js.t) =
      let f_conv s = match config.base with
        | `Hex -> "0x" ^ s
        | `Bin -> "0b" ^ s
        | `Dec -> s in
      x##.textContent |> Js.Opt.to_option
      |> Option.map Fun.(Char.chr % int_of_string % f_conv % Js.to_string)
      |> Option.get_exn

    method private _add_hex_selected x =
      x##.classList##add (Js.string Markup.Hex.selected_class)

    method private _rm_hex_selected x =
      x##.classList##remove (Js.string Markup.Hex.selected_class)

    method private _add_char_selected x =
      x##.classList##add (Js.string Markup.Char.selected_class)

    method private _rm_char_selected x =
      x##.classList##remove (Js.string Markup.Char.selected_class)

    method private _unselect x =
      List.find_opt Fun.(Int.equal (self#_get_hex_id x) % self#_get_char_id)
        self#_char_items
      |> Option.iter self#_rm_char_selected;
      _selected <- List.remove ~eq:Equal.physical x _selected;
      self#_rm_hex_selected x

    method private _select x =
      List.find_opt (fun c -> Int.equal (self#_get_char_id c)
                                (self#_get_hex_id x))
        self#_char_items
      |> Option.iter self#_add_char_selected;
      self#_add_hex_selected x;
      _selected <- x :: _selected

    method private _on_click = fun e _ ->
      let eq = Equal.physical in
      let ctrl = Js.to_bool e##.ctrlKey in
      let target = Js.Opt.to_option e##.target in
      let class' = Js.string Markup.Hex._class in
      let is_span =
        Option.map (fun e -> e##.classList##contains class' |> Js.to_bool)
          target
        |> Option.get_or ~default:false in
      begin match target, is_span with
      | Some e, true ->
         begin match List.find_opt (eq e) _selected with
         | Some x ->
            if not ctrl
            then List.iter self#_unselect (List.remove ~eq x _selected)
            else self#_unselect x
         | None ->
            if not ctrl then List.iter self#_unselect _selected;
            self#_select e
         end
      | _ -> ();
      end;
      Lwt.return_unit

  end
