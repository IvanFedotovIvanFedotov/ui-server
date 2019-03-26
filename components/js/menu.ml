open Js_of_ocaml
open Tyxml_js
open Containers

module Markup = Components_tyxml.Menu.Make(Xml)(Svg)(Html)

let elements_key_allowed_in = ["input"; "button"; "textarea"; "select"; "a"]

module Item = struct

  class ['a] t ?secondary_text ?graphic ?meta ~value ~text () = object
    inherit ['a] Item_list.Item.t ?secondary_text
              ?graphic
              ?meta
              ~value
              ~text
              () as super

    method! init () =
      super#init ();
      super#set_attribute "role" "menuitem";
      super#set_attribute "tabindex" "0"

    method disabled = match super#get_attribute "aria-disabled" with
      | Some "true" -> true
      | _ -> false

    method set_disabled x =
      if x then (super#set_attribute "aria-disabled" "true";
                 super#set_attribute "tabindex" "-1")
      else super#remove_attribute "aria-disabled"
  end

end

class type event =
  object
    inherit Dom_html.event
    method detail : < item  : Dom_html.element Js.t Js.readonly_prop;
           index : int Js.readonly_prop > Js.t Js.readonly_prop
  end

class ['a] t ?anchor_corner (list : 'a Item_list.t) () =
object(self)

  inherit Menu_surface.t ?anchor_corner ~widgets:[list] () as super

  val mutable close_animation_end_timer : Utils.timer_id option = None

  method! init () : unit =
    super#init ();
    list#set_attribute "role" "menu";
    list#set_attribute "aria-hidden" "true"

  method! destroy () : unit =
    super#destroy ();
    list#destroy ();
    Option.iter Utils.clear_timeout close_animation_end_timer;
    close_animation_end_timer <- None

  method items : 'a Item_list.Item.t list =
    list#items

  method get_option_by_index (i : int) : 'a Item_list.Item.t option =
    List.get_at_idx i self#items

  method! private notify_open () : unit =
    super#notify_open ();
    match self#items with
    | [] -> ()
    | x :: _ -> x#focus ()

end
