open Js_of_ocaml
open Utils

(* TODO add attach *)

include Components_tyxml.Typography

type font =
  | Headline_1
  | Headline_2
  | Headline_3
  | Headline_4
  | Headline_5
  | Headline_6
  | Subtitle_1
  | Subtitle_2
  | Body_1
  | Body_2
  | Button
  | Caption
  | Overline

let font_to_class = function
  | Headline_1 -> CSS.headline1
  | Headline_2 -> CSS.headline2
  | Headline_3 -> CSS.headline3
  | Headline_4 -> CSS.headline4
  | Headline_5 -> CSS.headline5
  | Headline_6 -> CSS.headline6
  | Subtitle_1 -> CSS.subtitle1
  | Subtitle_2 -> CSS.subtitle2
  | Body_1 -> CSS.body1
  | Body_2 -> CSS.body2
  | Button -> CSS.button
  | Caption -> CSS.caption
  | Overline -> CSS.overline

let remove (w : #Widget.t) =
  List.iter (fun x ->
      if String.prefix ~pre:CSS.root x
      then w#remove_class x)
    w#classes

let set ~font (w : #Widget.t) =
  remove w;
  w#add_class CSS.root;
  w#add_class @@ font_to_class font

module Text = struct
  class t ?(split = false) ?font ~text () =
  object(self)

    inherit Widget.t Dom_html.(createSpan document) () as super

    val mutable _text : string = text
    val mutable _font : font option = font

    method! init () : unit =
      super#init ();
      self#set_text text;
      self#add_class CSS.root;
      Option.iter (self#add_class % font_to_class) font

    method font : font option =
      _font

    method set_font (x : font) : unit =
      Option.iter (self#remove_class % font_to_class) font;
      self#add_class @@ font_to_class x;
      _font <- Some x

    method text : string =
      _text

    method set_text (s : string) : unit =
      _text <- s;
      self#set_inner_html (self#to_inner_html s)

    (* Private methods *)

    method private to_inner_html (text : string) : string =
      let open Tyxml.Html in
      let inner =
        if split
        then (
          let rec aux acc = function
            | [] -> List.rev acc
            | [x] -> List.rev ((txt x) :: acc)
            | x :: tl -> aux (br () :: txt x :: acc) tl in
          aux [] (String.split_on_char '\n' text))
        else [txt text] in
      String.concat "" @@ List.map (Format.asprintf "%a" (pp_elt ())) inner

  end

  let make ?split ?font (text : string) : t =
    new t ?split ?font ~text ()

end
