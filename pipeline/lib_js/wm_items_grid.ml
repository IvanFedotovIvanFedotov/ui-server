open Containers
open Components
open Wm_types
open Wm_components

let base_class = "wm-grid"

module Make(I : Item) = struct

  module G = Wm_items_layer.Make(I)

  let grid_to_string (x,y) = Printf.sprintf "%dx%d" x y

  class t ~title ~resolution ~(init: I.t list) ~e_layers () =
    let positions         = List.map I.position_of_t init in
    let s_grids,set_grids = React.S.create @@ Utils.get_grids ~resolution ~positions () in
    let s_grid,set_grid   = React.S.create @@ Utils.get_best_grid ~cols:90 ~resolution (React.S.value s_grids) in
    let grouped =
      List.fold_left (fun acc (x:I.t) ->
          let layer = I.layer_of_t x in
          List.Assoc.update ~eq:(=) ~f:(function Some l -> Some (x :: l) | None -> Some [x]) layer acc)
                     [] init
    in
    let layers  = I.layers_of_t_list init in
    let wrapper = Dom_html.createDiv Dom_html.document |> Widget.create in
    let s_layers,set_layers =
      List.map (fun x -> let items = List.Assoc.get ~eq:(=) x grouped in
                         let init  = Option.get_or ~default:[] items in
                         let grid  = new G.t ~layer:x ~init ~s_grid ~resolution () in
                         Dom.appendChild wrapper#root grid#root;
                         grid) layers
      |> React.S.create
    in
    let active     = List.hd @@ React.S.value s_layers in
    let ()         = active#set_active true in
    let s_active,set_active = React.S.create active in
    let title      = new Typography.Text.t ~adjust_margin:false ~text:title () in
    let on_data    = ({ icon = "grid_off"; label = None; css_class = None }:Markup.Icon_toggle.data) in
    let off_data   = ({ icon = "grid_on"; label = None; css_class = None }:Markup.Icon_toggle.data) in
    let grid_icon  = new Icon_toggle.t ~on_data ~off_data () in
    let menu_items = List.map (fun (w,h) -> `Item (new Menu.Item.t ~text:(Printf.sprintf "%dx%d" w h) ()))
                     @@ React.S.value s_grids in
    let menu       = new Menu.t ~items:menu_items () in
    let menu_text  = new Typography.Text.t ~text:"Сетка:" () in
    let menu_sel   = new Typography.Text.t ~text:(grid_to_string @@ React.S.value s_grid) () in
    let menu_ico   = new Icon.Font.t ~icon:"arrow_drop_down" () in
    let anchor     = new Box.t ~vertical:false ~widgets:[ menu_sel#widget; menu_ico#widget ] () in
    let menu_wrap  = new Menu.Wrapper.t ~anchor ~menu () in
    let menu_block = new Box.t ~vertical:false ~widgets:[ menu_text#widget; menu_wrap#widget ] () in
    let icons      = new Box.t ~vertical:false ~widgets:[ grid_icon#widget; menu_block#widget ] () in
    let header     = new Box.t ~vertical:false ~widgets:[ title#widget;icons#widget ] () in
    object(self)

      inherit Box.t ~vertical:true ~widgets:[header#widget;wrapper#widget] ()

      val s_sel =
        let eq = fun _ _ -> false in
        React.S.map ~eq:Equal.physical
                    (fun x -> let s = React.S.hold ~eq [] x#e_selected in
                              React.S.map ~eq (function [x] -> Some x | _ -> None) s)
                    s_active
        |> React.S.switch ~eq

      method s_active     = s_active
      method s_layers     = s_layers
      method s_selected   = s_sel
      method layout_items = List.map I.t_to_layout_item self#items
      method items        = List.fold_left (fun acc x -> x#items @ acc) [] @@ React.S.value s_layers
                            |> List.map (fun x -> x#get_value)

      initializer
        grid_icon#set_on true;
        (* update available grids *)
        let eq = (fun _ _ -> false) in
        let s = React.S.switch ~eq (React.S.map ~eq (fun x -> x#s_change) s_active) in
        React.S.map ~eq (fun _ -> let positions = List.map (fun x -> I.position_of_t x) self#items in
                                  let grids = Utils.get_grids ~resolution ~positions () in
                                  set_grids grids) s |> ignore;
        (* update selected grid *)
        React.E.map (fun (_,i) -> let s = i##.textContent
                                          |> Js.Opt.to_option
                                          |> Option.get_exn
                                          |> Js.to_string
                                  in
                                  match String.split_on_char 'x' s with
                                  | w::h::[] -> (match Int.of_string w, Int.of_string h with
                                                 | Some w,Some h -> set_grid (w,h)
                                                 | _             -> ())
                                  | _ -> ()) menu#e_selected |> ignore;
        React.S.map (fun x -> menu_sel#set_text_content @@ grid_to_string x) s_grid |> ignore;
        (* show grid menu *)
        Dom_events.listen anchor#root Dom_events.Typ.click (fun _ _ ->
                            let item text = new Menu.Item.t ~text () in
                            let items = List.map (fun (w,h) -> let text = Printf.sprintf "%dx%d" w h in
                                                               item text) @@ React.S.value s_grids in
                            Utils.rm_children menu#get_list#root;
                            List.iter (fun x -> Dom.appendChild menu#get_list#root x#root) items;
                            menu#show;
                            true) |> ignore;
        anchor#set_align_items `Center;
        title#add_class      @@ Markup.CSS.add_element base_class "title";
        menu_block#add_class @@ Markup.CSS.add_element base_class "grid-select";
        icons#add_class      @@ Markup.CSS.add_element base_class "right-menu";
        grid_icon#add_class  @@ Markup.CSS.add_element base_class "menu";
        header#add_class     @@ Markup.CSS.add_element base_class "header";
        React.S.l2 (fun conf grid -> if conf then grid#overlay_grid#show else grid#overlay_grid#hide)
                   grid_icon#s_state s_active |> ignore;
        React.S.diff (fun n o -> n#set_active true;
                                 o#set_active false;
                                 List.iter (fun x -> x#set_selected false) o#items) s_active |> ignore;
        React.E.map (fun e -> let grids = React.S.value s_layers in
                              match e with
                              | `Selected x ->
                                 let grid = List.find_pred (fun g -> g#layer = x) grids in
                                 Option.iter (fun g -> set_active g) grid
                              | `Added x    ->
                                 let grid = new G.t ~layer:x ~init:[] ~s_grid ~resolution () in
                                 Dom.appendChild wrapper#root grid#root;
                                 set_layers (grid :: grids)
                              | `Removed x  ->
                                 let grid = List.find_pred (fun g -> g#layer = x) grids in
                                 Option.iter (fun x -> (try Dom.removeChild wrapper#root x#root with _ -> ()))
                                             grid;
                                 set_layers @@ List.filter (fun g -> g#layer <> x) grids
                              | `Changed l  ->
                                 List.iter (fun g -> match List.Assoc.get ~eq:(=) g#layer l with
                                                     | Some n -> g#set_layer n
                                                     | None   -> ()) grids)
                    e_layers |> ignore;
        self#add_class base_class;
        self#set_justify_content `Center;
        wrapper#add_class @@ Markup.CSS.add_element base_class "wrapper"

    end

  let make ~title ~resolution ~(init: I.t list) ~selected_push ~e_layers () =
    let ig = new t ~title ~resolution ~init ~e_layers () in
    let _  = React.S.map (fun x -> selected_push x) ig#s_selected in
    ig

end
