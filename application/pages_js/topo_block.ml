open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Topo_types

let port_section_height = 50
let base_class = "topology__block"
let fine_class = BEM.add_modifier base_class "fine"
let init_class = BEM.add_modifier base_class "init"
let fail_class = BEM.add_modifier base_class "fail"

let ( % ) f g x = f (g x)

module Header = struct

  let _class = BEM.add_element base_class "header"
  let action_class = BEM.add_element _class "action"

  class t ?action ?subtitle ~title () =
    let title_w = Card.Primary.make_title title in
    let subtitle_w = Utils.Option.map
        (Widget.coerce % Card.Primary.make_subtitle)
        subtitle in
    let box =
      Box.make ~dir:`Column
        ([]
         |> Utils.List.cons_maybe subtitle_w
         |> List.cons title_w#widget) in
    let widgets =
      []
      |> Utils.List.cons_maybe @@ Utils.Option.map Widget.coerce action
      |> List.cons box#widget in
    let elt =
      Tyxml_js.To_dom.of_element
      @@ Card.Markup.create_primary (List.map Widget.to_markup widgets) () in
    object(self)

      inherit Widget.t elt () as super

      method! init () : unit =
        super#init ();
        Utils.Option.iter (fun a -> a#add_class action_class) action;
        self#add_class _class

    end

end

module Body = struct

  let _class = BEM.add_element base_class "body"

  class t n () =
    object(self)
      inherit Widget.t Dom_html.(createDiv document) () as super

      method! init () : unit =
        super#init ();
        self#set_n n;
        self#add_class _class;

      method set_n n =
        super#root##.style##.height := Utils.px_js (n * port_section_height)

    end

end

class virtual t ~port_setter
        ~(connections : (#Topo_node.t * connection_point) list)
        ~(node : Topo_node.node_entry)
        ~(header : #Header.t)
        ~(body : #Body.t)
        () =
  let card = Card.make [header#widget; body#widget] in
  object(self)
    inherit Topo_node.parent
              ~port_setter
              ~node
              ~connections
              ~body:body#root
              card#root () as super

    method! init () : unit =
      super#init ();
      body#set_n @@ List.length connections;
      self#add_class base_class

    method virtual settings_event : (Widget.t * string) React.event

    method private set_state : Application_types.Topology.state -> unit = function
      | `Fine ->
         self#add_class fine_class;
         self#remove_class init_class;
         self#remove_class fail_class
      | `Init ->
         self#add_class init_class;
         self#remove_class fine_class;
         self#remove_class fail_class
      | `No_response | `Detect ->
         self#add_class fail_class;
         self#remove_class init_class;
         self#remove_class fine_class
  end
