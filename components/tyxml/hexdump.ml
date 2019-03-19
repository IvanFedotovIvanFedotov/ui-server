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

type base = Hex | Dec | Bin

let equal_base (a : base) (b : base) : bool =
  match a, b with
  | Hex, Hex | Dec, Dec | Bin, Bin -> true
  | _, _ -> false

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

let get_converter = function
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

let line_number_to_string ~(width : int) (i : int) : string =
  let cur_line_count = i * width in
  let s = string_of_int cur_line_count in
  pad Const.line_number_len '0' s

module Make(Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
           and module Svg := Svg) = struct
  open Html

  let create_line_number ?(classes = []) ?attrs s () : 'a elt =
    let classes = CSS.line_number :: classes in
    span ~a:([a_class classes] <@> attrs) [txt s]

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

  let create_hex_empty ?(classes = []) ?attrs
        ~(base : base) (empty_hex : char) () : 'a elt =
    let classes = CSS.hex :: CSS.hex_empty :: classes in
    span ~a:([ a_class classes
             ; a_user_data "hex-empty" "true"
             ] <@> attrs)
      [txt (String.make (get_padding base) empty_hex)]

  let create_hex ?(classes = []) ?attrs ?_or
        ~(base : base) ~(id : int) (hex : int) () : 'a elt =
    let classes = CSS.hex :: classes in
    let hex = (get_converter base) hex in
    span ~a:([ a_class classes
             ; a_user_data "hex-id" (string_of_int id)
             ] <@> attrs)
      [txt (pad (get_padding base) '0' hex)]

  let create_block ?(classes = []) ?attrs () : 'a elt =
    let classes = CSS.block :: classes in
    pre ~a:([a_class classes] <@> attrs) []
end
