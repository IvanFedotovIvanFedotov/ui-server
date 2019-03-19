open Utils

module Const = struct
  let line_number_len = 4
end

module CSS = struct
  let root = "mdc-hexdump"
  let block = BEM.add_element root "block"
  let line_number = BEM.add_element root "line-number"
  let hex = BEM.add_element root "hex"
  let hex_empty = BEM.add_modifier hex "empty"
  let char = BEM.add_element root "char"
  let char_empty = BEM.add_modifier char "empty"

  let interactive = BEM.add_modifier root "interactive"
  let block_hex = BEM.add_modifier block "hex"
  let block_chars = BEM.add_modifier block "chars"
  let block_line_numbers = BEM.add_modifier block "line-numbers"
  let char_selected = BEM.add_modifier char "selected"
  let hex_selected = BEM.add_modifier hex "selected"
end

type block_type = Num | Hex | Chr

type base = Hex | Dec | Bin

let equal_base (a : base) (b : base) : bool =
  match a, b with
  | Hex, Hex | Dec, Dec | Bin, Bin -> true
  | _, _ -> false

let elt_to_string (elt : 'a Tyxml.Html.elt) : string =
  Format.asprintf "%a" (Tyxml.Html.pp_elt ()) elt

let int_to_string_binary ?(prefix = false) (n : int) : string =
  let buf = Buffer.create 16 in
  let out = Buffer.add_char buf in
  let n = if n < 0 then (out '-'; -n) else n in
  if prefix then (out '0'; out 'b');
  let rec loop started bit n =
    if bit = 0 then (
      if not started then out '0')
    else (
      let b = n land bit in
      if b = 0 then (
        if started then out '0';
        loop started (bit lsr 1) n)
      else (
        out '1';
        loop true (bit lsr 1) n)) in
  let most_significant_bit = (-1) lxor ((-1) lsr 1) in
  loop false most_significant_bit n;
  Buffer.contents buf

let get_padding = function
  | Hex -> 2
  | Dec -> 3
  | Bin -> 8

let to_base_string = function
  | Hex -> Printf.sprintf "%x"
  | Dec -> Printf.sprintf "%d"
  | Bin -> int_to_string_binary ~prefix:false

let map_char_printable ?(_or = '.') (c : char) : char =
  match Char.code c with
  | x when x > 31 && x < 128 -> c
  | _ -> _or

let pad (need : int) (c : char) (s : string) : string =
  let len = String.length s in
  match need - len with
  | x when x > 0 -> (String.make x c) ^ s
  | _ -> s

let take n l =
  let rec direct i n l = match l with
    | [] -> []
    | _ when i = 0 -> safe n [] l
    | x :: l' ->
       if n > 0
       then x :: direct (i - 1) (n - 1) l'
       else []
  and safe n acc l = match l with
    | [] -> List.rev acc
    | _ when n = 0 -> List.rev acc
    | x :: l' -> safe (n - 1) (x :: acc) l'
  in
  direct 1000 n l

let rec drop n l = match l with
  | [] -> []
  | _ when n = 0 -> l
  | _ :: l' -> drop (n - 1) l'

let take_drop n l = take n l, drop n l

let string_to_list s =
  let rec aux s acc i len =
    if len = 0 then List.rev acc
    else aux s (s.[i] :: acc) (i + 1) (len - 1) in
  aux s [] 0 (String.length s)

let line_number_to_string ~(width : int) (i : int) : string =
  let cur_line_count = i * width in
  let s = string_of_int cur_line_count in
  pad Const.line_number_len '0' s

let should_insert_space ~(grouping : int) (cnt : int) =
  if grouping = 0 then false else ((succ cnt) mod grouping) = 0

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let create_line_number ?(classes = []) ?attrs n () : 'a elt =
    let classes = CSS.line_number :: classes in
    span ~a:([a_class classes] <@> attrs) [txt @@ string_of_int n]

  let create_char_empty ?(classes = []) ?attrs
        (empty_char : char) () : 'a elt =
    let classes = CSS.char :: CSS.char_empty :: classes in
    span ~a:([ a_class classes
             ; a_user_data "char-empty" "true"
             ] <@> attrs)
      [txt (String.make 2 empty_char)]

  let create_char ?(classes = []) ?attrs ?_or
        ~(id : int) (chr : char) () : 'a elt =
    let classes = CSS.char :: classes in
    let chr = map_char_printable ?_or chr in
    span ~a:([ a_class classes
             ; a_user_data "char-id" (string_of_int id)
             ] <@> attrs)
      [txt (String.make 1 chr)]

  let create_hex_empty ?(classes = []) ?attrs ?(grouping = 1)
        ~(base : base) ~id (empty_hex : char) () : 'a elt =
    let classes = CSS.hex :: CSS.hex_empty :: classes in
    let text = String.make (get_padding base) empty_hex in
    span ~a:([ a_class classes
             ; a_user_data "hex-empty" "true"
             ] <@> attrs)
      [txt (if should_insert_space ~grouping id
            then text ^ " " else text)]

  let create_hex ?(classes = []) ?attrs ?(grouping = 1)
        ~(base : base) ~(id : int) (hex : int) () : 'a elt =
    let classes = CSS.hex :: classes in
    let text = pad (get_padding base) '0' @@ to_base_string base hex in
    span ~a:([ a_class classes
             ; a_user_data "hex-id" (string_of_int id)
             ] <@> attrs)
      [txt (if should_insert_space ~grouping id
            then text ^ " " else text)]

  let append_empty ~empty_hex ~empty_chr base ~width ~grouping i acc =
    let rec aux i acc = function
      | 0 -> acc
      | rest ->
         let hex, chr = acc in
         (* Insert space if this item is not last in a row *)
         let hex = create_hex_empty ~base ~grouping ~id:i empty_hex () :: hex in
         let chr = create_char_empty empty_chr () :: chr in
         aux (succ i) (hex, chr) (pred rest) in
    aux i acc (width - i)

  let create_row ~empty_hex ~empty_chr ~width ~grouping
        (base : base) (data : char list) =
    let rec aux i acc = function
      | [] -> append_empty ~empty_hex ~empty_chr base ~grouping ~width i acc
      | (hd : char) :: tl ->
         let hex, chr = acc in
         let code = Char.code hd in
         let hex = (create_hex ~base ~grouping ~id:i code ()) :: hex in
         let chr = (create_char ~_or:empty_chr ~id:i hd ()) :: chr in
         aux (succ i) (hex, chr) tl in
    aux 0 ([], []) data

  let create_rows ?(empty_hex = '.') ?(empty_chr = '.')
        ?(width = 16) ?(grouping = 1) base (data : string) =
    let rec aux acc bytes = match take_drop width bytes with
      | l, [] -> List.rev (l :: acc)
      | l, r -> aux (l :: acc) r in
    let bytes = aux [] (string_to_list data) in
    let _, num, hex, chr =
      List.fold_left (fun (id, num, hex, chr) (x : char list) ->
          let num' = create_line_number (id / width) () in
          let hex', chr' = create_row ~empty_hex ~empty_chr ~width ~grouping base x in
          id + List.length x,
          br () :: num' :: num,
          br () :: hex' @ hex,
          br () :: chr' @ chr)
        (0, [], [], []) bytes in
    List.rev num,
    List.rev hex,
    List.rev chr

  let create_block ?(classes = []) ?attrs ~typ cells () : 'a elt =
    let typ_class = match typ with
      | Num -> CSS.block_line_numbers
      | Hex -> CSS.block_hex
      | Chr -> CSS.block_chars in
    let classes = CSS.block :: typ_class :: classes in
    pre ~a:([a_class classes] <@> attrs) cells

  let create ?(classes = []) ?attrs ~blocks () : 'a elt =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] <@> attrs) blocks

  let of_bytes ?classes ?attrs ?empty_hex ?empty_chr ?width ?grouping
        base (bytes : string) : 'a elt =
    let num, hex, chr =
      create_rows ?empty_hex ?empty_chr ?width ?grouping base bytes in
    create ?classes ?attrs
      ~blocks:[ create_block ~typ:Num num ()
              ; create_block ~typ:Hex hex ()
              ; create_block ~typ:Chr chr () ]
      ()
end
