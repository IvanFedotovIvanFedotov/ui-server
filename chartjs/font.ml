module Obj = Base.Obj

type style = [`Normal | `Italic | `Oblique | `Bold ]

type font =
    { family : string (* FIXME declare a type *)
    ; size   : int
    ; color  : CSS.Color.t
    ; style  : style
    }

let style_to_string = function
  | `Normal  -> "normal"
  | `Italic  -> "italic"
  | `Oblique -> "oblique"
  | `Bold    -> "bold"

let style_of_string_exn = function
  | "normal"  -> `Normal
  | "italic"  -> `Italic
  | "oblique" -> `Oblique
  | "bold"    -> `Bold
  | _         -> failwith "Bad style string"

class type t_js =
  object
    method fontSize   : int Js.prop
    method fontStyle  : Js.js_string Js.t Js.prop
    method fontColor  : CSS.Color.js_t Js.prop
    method fontFamily : Js.js_string Js.t Js.prop
  end

class virtual ['a] t font () =
        object(self)
          val mutable virtual obj : (#t_js as 'a) Js.t

          method set_font_size x = obj##.fontSize := x
          method get_font_size   = obj##.fontSize

          method set_font_style (x:style) = obj##.fontStyle := Js.string @@ style_to_string x
          method get_font_style : style   = style_of_string_exn @@ Js.to_string obj##.fontStyle

          method set_font_color x = obj##.fontColor := CSS.Color.js x
          method get_font_color   = CSS.Color.ml obj##.fontColor

          method set_font_family x = obj##.fontFamily := Js.string x
          method get_font_family   = Js.to_string obj##.fontFamily

          initializer
            self#set_font_size font.size;
            self#set_font_style font.style;
            self#set_font_color font.color;
            self#set_font_family font.family
        end