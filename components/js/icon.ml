open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_tyxml.Icon.Make(Xml)(Svg)(Html)

module Font = struct

  module Markup = Markup.Font

  class t ~icon () =
    let elt = Markup.create ~icon () |> Tyxml_js.To_dom.of_i in
    object
      inherit Widget.t elt () as super

      method icon : string =
        super#text_content |> Option.get_or ~default:""

      method set_icon (i : string) : unit =
        super#set_text_content i
    end

end

module SVG = struct

  module Markup = Markup.SVG

  module To_dom = Tyxml_cast.MakeTo(struct
                      type 'a elt = 'a Tyxml_js.Svg.elt
                      let elt = Tyxml_js.Svg.toelt
                    end)

  module Of_dom = Tyxml_cast.MakeOf(struct
                      type 'a elt = 'a Tyxml_js.Svg.elt
                      let elt = Tyxml_js.Svg.tot
                    end)

  module Path = struct

    include Markup.Path

    class t (elt : #Dom_html.element Js.t) () =
    object(self)
      inherit Widget.t elt ()

      method get : string =
        Option.get_or ~default:"" @@ self#get_attribute "d"

      method set (s : string) : unit =
        self#set_attribute "d" s

    end

    let make ?(fill : Color.t option) (path : string) () : t =
      let fill = Option.map Color.to_css_rgba fill in
      let elt =
        To_dom.of_element
        @@ Markup.create_path ?fill path () in
      new t elt ()

    let attach (elt : #Dom_html.element Js.t) : t =
      new t elt ()

  end

  (* paths variable is passed to avoid double allocation of paths objects *)
  class t ?(paths : Path.t list option)
          (elt : #Dom_html.element Js.t) () =
  object(self)
    inherit Widget.t elt ()

    val mutable _paths = paths

    method paths : Path.t list =
      match _paths with
      | Some x -> x
      | None ->
         let paths = List.map Path.attach @@ Element.children elt in
         _paths <- Some paths;
         paths

    method path : Path.t =
      List.hd self#paths
  end

  let make ?size (paths : Path.t list) () : t =
    let paths' = List.map (fun x -> Of_dom.of_element x#root) paths in
    let elt =
      Tyxml_js.To_dom.of_element
      @@ Markup.create ?size paths' () in
    new t ~paths elt ()

  let make_simple ?size (path : string) : t =
    make ?size [Path.make path ()] ()

  let attach (elt : #Dom_html.element Js.t) : t =
    new t elt ()

end
