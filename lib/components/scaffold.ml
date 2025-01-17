open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Utils

(* TODO
   - breakpoints could be read from DOM as data attributes
 *)

include Components_tyxml.Scaffold
module Markup = Make(Tyxml_js.Xml)(Tyxml_js.Svg)(Tyxml_js.Html)

type drawer_elevation =
  | Full_height
  | Clipped

let equal_drawer_elevation (a : drawer_elevation as 'a) (b : 'a) =
  match a, b with
  | Full_height, Full_height | Clipped, Clipped -> true
  | _ -> false

module Breakpoint = struct
  include Components_tyxml.Breakpoint

  let default_side_sheet : Side_sheet.typ t =
    make ~points:[1160, (Modal : Side_sheet.typ)] Dismissible

  let default_drawer : Side_sheet.typ t =
    make ~points:[1160, (Modal : Side_sheet.typ)] Dismissible

  let get_screen_width () : int =
    Dom_html.document##.body##.offsetWidth

end

let drawer_type_to_enum : Side_sheet.typ -> int = function
  | Modal -> 0
  | Dismissible -> 1
  | Permanent -> 2

let equal_drawer_type (a : Side_sheet.typ as 'a) (b : 'a) : bool =
  (drawer_type_to_enum a) = (drawer_type_to_enum b)

module Selector = struct

  let not_found (name : string) : string =
    Printf.sprintf "%s: %s not found" CSS.root name

  let by_class_opt (elt : Dom_html.element Js.t) (c : string)
      : Dom_html.element Js.t option =
    elt##querySelector (Js.string ("." ^ c))
    |> Js.Opt.to_option

  let by_class (elt : Dom_html.element Js.t) (c : string)
      : Dom_html.element Js.t =
    match by_class_opt elt c with
    | Some x -> x
    | None -> failwith (not_found c)

end

let attach_top_app_bar ?scroll_target (elt : Dom_html.element Js.t) () =
  Selector.by_class_opt elt Top_app_bar.CSS.root
  |> Option.map (Top_app_bar.attach ?scroll_target)

let attach_drawer (elt : Dom_html.element Js.t) () =
  Selector.by_class_opt elt Drawer.CSS.root
  |> Option.map Drawer.attach

let attach_side_sheet (elt : Dom_html.element Js.t) () =
  Selector.by_class_opt elt Side_sheet.CSS.root
  |> Option.map Side_sheet.attach

let attach_body (app_content_inner : Dom_html.element Js.t) () =
  let elt = (Js.Unsafe.coerce app_content_inner)##.firstElementChild in
  match Js.Opt.to_option elt with
  | None -> None
  | Some (node : Dom_html.element Js.t) -> Some (Widget.create node)

class t ?(drawer : #Drawer.t option)
        ?(drawer_elevation : drawer_elevation option)
        ?(drawer_breakpoints = Breakpoint.default_drawer)
        ?(side_sheet : #Side_sheet.t option)
        ?(side_sheet_elevation : drawer_elevation option)
        ?(side_sheet_breakpoints = Breakpoint.default_side_sheet)
        ?(top_app_bar : #Top_app_bar.t option)
        ?(body : #Widget.t option)
        (elt : Dom_html.element Js.t)
        () =
  let drawer_frame_full_height =
    Selector.by_class elt CSS.drawer_frame_full_height in
  let drawer_frame_clipped =
    Selector.by_class elt CSS.drawer_frame_clipped in
  let app_content_outer =
    Selector.by_class elt CSS.app_content_outer in
  let app_content_inner =
    Selector.by_class elt CSS.app_content_inner in
  object(self)

    (* Nodes *)
    val mutable top_app_bar = match top_app_bar with
      | None -> attach_top_app_bar ~scroll_target:app_content_inner elt ()
      | Some x -> Some x
    val mutable drawer = match drawer with
      | None -> attach_drawer elt ()
      | Some x -> Some x
    val mutable side_sheet = match side_sheet with
      | None -> attach_side_sheet elt ()
      | Some x -> Some x
    val mutable body = match body with
      | None -> attach_body app_content_inner ()
      | Some x -> Some x

    (* Event listeners *)
    val mutable menu_click_listener = None
    val mutable resize_listener = None

    val mutable drawer_type =
      Breakpoint.(current (get_screen_width ()) drawer_breakpoints)
    val mutable side_sheet_type =
      Breakpoint.(current (get_screen_width ()) side_sheet_breakpoints)

    val mutable side_sheet_breakpoints = side_sheet_breakpoints
    val mutable drawer_breakpoints = drawer_breakpoints

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      (* Setup top app bar *)
      Option.iter self#setup_app_bar top_app_bar;
      (* Setup drawer *)
      Option.iter (fun drawer ->
          let typ = drawer_type in
          let elv = match drawer_elevation with
            | None -> (match self#drawer_elevation with
                       | None -> Clipped
                       | Some x -> x)
            | Some x -> x in
          self#set_drawer_properties_ ~is_leading:true typ elv drawer)
        self#drawer;
      (* Setup side sheet *)
      Option.iter (fun side_sheet ->
          let typ = side_sheet_type in
          let elv = match side_sheet_elevation with
            | None -> (match self#side_sheet_elevation with
                       | None -> Clipped
                       | Some x -> x)
            | Some x -> x in
          self#set_drawer_properties_ ~is_leading:false typ elv side_sheet)
        self#side_sheet;
      (* Setup body *)
      Option.iter self#set_body body;
      (* Handle window resize *)
      Lwt_js_events.limited_onresizes ~elapsed_time:0.05
        (fun _ _ -> self#handle_resize ())
      |> (fun x -> resize_listener <- Some x);

    method! destroy () : unit =
      super#destroy ();
      (* Stop window resize listener *)
      Option.iter Lwt.cancel resize_listener;
      resize_listener <- None;

    (* Widgets *)

    method top_app_bar : Top_app_bar.t option =
      top_app_bar

    method drawer : Drawer.t option =
      drawer

    method set_drawer : 'a. ?elevation:drawer_elevation ->
                        ?breakpoints:Side_sheet.typ Breakpoint.t ->
                        (#Drawer.t as 'a) ->
                        unit =
      fun ?(elevation = Clipped) ?breakpoints (w : #Drawer.t) ->
      Option.iter (fun bp -> drawer_breakpoints <- bp) breakpoints;
      let w = (w :> Side_sheet.t) in
      let bp = drawer_breakpoints in
      let typ = Breakpoint.(current (get_screen_width ()) bp) in
      self#set_drawer_properties_ ~is_leading:true typ elevation w;
      drawer <- Some w

    method side_sheet : Side_sheet.t option =
      side_sheet

    method set_side_sheet : 'a. ?elevation:drawer_elevation ->
                            ?breakpoints:Side_sheet.typ Breakpoint.t ->
                            (#Side_sheet.t as 'a) ->
                            unit =
      fun ?(elevation = Clipped) ?breakpoints (w : #Side_sheet.t) ->
      Option.iter (fun bp -> side_sheet_breakpoints <- bp) breakpoints;
      let w = (w :> Side_sheet.t) in
      let bp = side_sheet_breakpoints in
      let typ = Breakpoint.(current (get_screen_width ()) bp) in
      self#set_drawer_properties_ ~is_leading:false typ elevation w;
      side_sheet <- Some w

    method set_side_sheet_breakpoints (bp : Side_sheet.typ Breakpoint.t)
           : unit =
      side_sheet_breakpoints <- bp

    method set_drawer_breakpoints (bp : Side_sheet.typ Breakpoint.t) : unit =
      drawer_breakpoints <- bp

    method app_content_inner = app_content_inner

    method app_content_outer = app_content_outer

    method body : Widget.t option =
      body

    method set_body : 'a. (#Widget.t as 'a) -> unit =
      fun (body : #Widget.t) ->
      Element.remove_children app_content_inner;
      Dom.appendChild app_content_inner body#root

    method drawer_elevation : drawer_elevation option =
      self#drawer_elevation_ self#drawer

    method drawer_type : Side_sheet.typ =
      drawer_type

    method side_sheet_elevation : drawer_elevation option =
      self#drawer_elevation_ self#side_sheet

    method side_sheet_type : Side_sheet.typ =
      side_sheet_type

    method show_snackbar (snackbar : Snackbar.t) : unit =
      ignore snackbar;
      ()

    (* Private methods *)

    method private setup_app_bar (app_bar : #Top_app_bar.t) : unit =
      (* FIXME rework, not very readable *)
      let leading = match app_bar#leading, drawer with
        | None, Some _ ->
           let d = Components_tyxml.Svg_icons.menu in
           let icon = Icon.SVG.(make_simple d) in
           let w = Icon_button.make ~icon () in
           w#add_class Top_app_bar.CSS.navigation_icon;
           Some w
        | _ -> None in
      Option.iter app_bar#set_leading leading;
      (match app_bar#leading, drawer with
       | Some l, Some d ->
          let listener =
            Events.clicks l#root (fun _ _ -> d#toggle ()) in
          menu_click_listener <- Some listener;
       | _ -> ());
      Element.insert_child_at_index app_content_outer 0 app_bar#root

    (** Determines drawer or side sheet elevation *)
    method private drawer_elevation_ (drawer : #Widget.t option)
           : drawer_elevation option =
      match drawer with
      | None -> None
      | Some (d : #Side_sheet.t) ->
         match Js.Opt.to_option @@ Element.get_parent d#root with
         | None -> None
         | Some parent ->
            if Element.has_class parent CSS.drawer_frame_full_height
            then Some Full_height
            else if Element.has_class parent CSS.drawer_frame_clipped
            then Some Clipped
            else None

    method private render_drawer_
                     ~(is_leading : bool)
                     (typ : Side_sheet.typ)
                     (elevation : drawer_elevation)
                     (drawer : #Side_sheet.Parent.t) : unit =
      let create_scrim, scrim_class =
        if is_leading
        then Drawer.Markup.create_scrim, Drawer.CSS.scrim
        else Side_sheet.Markup.create_scrim, Side_sheet.CSS.scrim in
      (* Where to place drawer *)
      let parent = match elevation with
        | Clipped -> drawer_frame_clipped
        | Full_height -> drawer_frame_full_height in
      (* How to render drawer *)
      begin match typ with
      | Permanent | Dismissible ->
         (* Remove scrim *)
         let scrim = Selector.by_class_opt parent scrim_class in
         Option.iter (Dom.removeChild parent) scrim;
      | Modal ->
         (* Create drawer scrim or use existing one, if any *)
         let scrim = match Selector.by_class_opt parent scrim_class with
           | Some x -> x
           | None -> Tyxml_js.To_dom.of_element @@ create_scrim () in
         Dom.insertBefore parent scrim drawer#root##.nextSibling
      end;
      let need_insert = match Js.Opt.to_option @@ Element.get_parent drawer#root with
        | None -> true
        | Some p when not (Element.equal p parent) -> true
        | _ -> false in
      if need_insert then
        Dom.insertBefore parent drawer#root parent##.firstChild

    method private set_drawer_properties_
                     ~(is_leading : bool)
                     (typ : Side_sheet.typ)
                     (elevation : drawer_elevation)
                     (drawer : #Side_sheet.Parent.t) : unit =
      begin match elevation with
      | Clipped -> drawer#add_class Top_app_bar.CSS.fixed_adjust
      | _ -> ()
      end;
      match typ with
      | Permanent ->
         self#render_drawer_ ~is_leading typ elevation drawer;
         drawer#set_permanent ();
         if is_leading then
           Option.iter (fun x -> x#hide_leading ()) top_app_bar;
      | Dismissible ->
         self#render_drawer_ ~is_leading typ elevation drawer;
         drawer#set_dismissible ();
         if is_leading then
           Option.iter (fun x -> x#show_leading ()) top_app_bar;
      | Modal ->
         self#render_drawer_ ~is_leading typ elevation drawer;
         drawer#set_modal ();
         if is_leading then
           Option.iter (fun x -> x#show_leading ()) top_app_bar;

    method private handle_drawer_resize_
                     ~(is_leading : bool)
                     (screen : int)
                     (elevation : drawer_elevation)
                     (drawer : #Side_sheet.Parent.t)
                   : unit Lwt.t =
      let typ, breakpoints =
        if is_leading
        then drawer_type, drawer_breakpoints
        else side_sheet_type, side_sheet_breakpoints in
      let cur = Breakpoint.current screen breakpoints in
      if equal_drawer_type typ cur then Lwt.return_unit else
        Lwt.Infix.(
        drawer#toggle ~force:false ()
        >|= fun () ->
        self#set_drawer_properties_ ~is_leading cur elevation drawer;
        if is_leading
        then drawer_type <- cur
        else side_sheet_type <- cur)

    method private handle_resize () : unit Lwt.t =
      let screen = Breakpoint.get_screen_width () in
      let drawer_lwt =
        match self#drawer, self#drawer_elevation_ self#drawer with
        | Some drawer, Some elevation ->
           self#handle_drawer_resize_ ~is_leading:true
             screen elevation drawer
        | _ -> Lwt.return_unit in
      let side_sheet_lwt =
        match self#side_sheet, self#drawer_elevation_ self#side_sheet with
        | Some side_sheet, Some elevation ->
           self#handle_drawer_resize_ ~is_leading:false
             screen elevation side_sheet
        | _ -> Lwt.return_unit in
      Lwt.join [drawer_lwt; side_sheet_lwt]

  end

(** Create new scaffold widget from scratch *)
let make ?drawer ?drawer_elevation ?drawer_breakpoints
      ?side_sheet ?side_sheet_elevation ?side_sheet_breakpoints
      ?(top_app_bar : #Top_app_bar.t option)
      ?(body : #Widget.t option)
      () =
  let elt =
    Tyxml_js.To_dom.of_element
    @@ Markup.(
      create
        [create_drawer_frame ~full_height:true
           [create_app_content ~outer:true
              [create_drawer_frame ~clipped:true
                 [create_app_content ~inner:true [] ()] ()]
              ()]
           ()]
        ()) in
  new t ?drawer ?drawer_elevation ?drawer_breakpoints
    ?side_sheet ?side_sheet_elevation ?side_sheet_breakpoints
    ?top_app_bar ?body elt ()

(** Attach scaffold widget to existing element *)
let attach (elt : #Dom_html.element Js.t) : t =
  new t (Element.coerce elt) ()
