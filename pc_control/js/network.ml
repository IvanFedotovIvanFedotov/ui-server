open Application_types
open Components
open Util_react
open Lwt.Infix

module Api_http = Api_js.Http.Make(Body)

module Requests = struct
  open Netlib.Uri

  (* TODO fix relative addr *)
  let get_config () =
    Api_http.perform
      ~meth:`GET
      ~path:Path.Format.("api/network/config" @/ empty)
      ~query:Query.empty
      (fun _env -> function
         | Error e -> Lwt.return_error e
         | Ok x ->
           match Network_config.of_yojson x with
           | Error e -> Lwt.return_error (`Conv_error e)
           | Ok x -> Lwt.return_ok x)

  let post_config conf =
    Api_http.perform_unit
      ~meth:`POST
      ~body:(Network_config.to_yojson conf)
      ~path:Path.Format.("api/network/config" @/ empty)
      ~query:Query.empty
      (fun _env res -> Lwt.return res)

end

(* FIXME remove 4.08 *)
let rec equal_list (f : 'a -> 'a -> bool) l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _ | _, [] -> false
  | x1 :: l1', x2 :: l2' -> f x1 x2 && equal_list f l1' l2'

let equal_option (f : 'a -> 'a -> bool) o1 o2 = match o1, o2 with
  | None, None -> true
  | None, Some _ | Some _, None -> false
  | Some a, Some b -> f a b

let make_eth (eth : Network_config.ethernet_conf) =
  let eth_head  = new Card.Primary.t ~widgets:[new Card.Primary.title "Настройки устройства" ()] () in

  let of_string x = match Macaddr.of_string x with
    | Ok _ as v -> v
    | Error (`Msg m) -> Error ("Неверный мак адрес " ^ m)
  in
  let to_string x = Macaddr.to_string x in
  let address =
    new Textfield.t
      ~label:"MAC адрес"
      ~input_type:(Custom (of_string, to_string))
      () in
  let signal, push =
    S.create ~eq:Network_config.equal_ethernet_conf eth in

  let set (eth : Network_config.ethernet_conf) = address#set_value eth.mac_address in

  let media      = new Card.Media.t ~widgets:[new Vbox.t ~widgets:[ address#widget ] ()] () in
  media#style##.margin := Utils.px_js 15;
  let eth_sets  = new Card.t ~widgets:[ eth_head#widget; media#widget ] () in
  
  let signal =
    S.l2 ~eq:Network_config.equal_ethernet_conf
      (fun (config : Network_config.ethernet_conf) mac_address ->
        match mac_address with
        | None -> config
        | Some mac_address -> { config with mac_address })
      signal address#s_input
  in
  
  eth_sets, signal, set

let make_dns (dns : Network_config.v4 list) =
  let make_dns_entry del_dns addr =
    let text = Ipaddr.V4.to_string addr in
    let del_button = new Button.t ~label:"delete" () in
    let item = new Item_list.Item.t ~text ~meta:del_button ~value:() () in
    del_button#listen_click_lwt (fun _ _ -> del_dns item addr; Lwt.return_unit)
    |> Lwt.ignore_result;
    item
  in

  let header = new Typography.Text.t ~font:Subtitle_1 ~text:"Список DNS" () in
  let list = new Item_list.t ~items:[] () in
  let address = new Textfield.t ~label:"Адрес" ~input_type:IPV4 () in
  let add_but = new Button.t ~label:"Добавить" () in
  let add_box = new Hbox.t ~widgets:[address#widget; add_but#widget] () in
  let full_box = new Vbox.t ~widgets:[header#widget; list#widget; add_box#widget] () in

  let signal, push =
    S.create ~eq:(equal_list Network_config.equal_v4) [] in

  let del_dns item addr =
    list#remove_item item;
    push
    @@ List.filter (fun dns -> not (Network_config.equal_v4 addr dns))
    @@ S.value signal
  in
  let add_dns addr =
    let rlst = S.value signal in
    if List.exists (Network_config.equal_v4 addr) rlst
    then failwith "dns exists"; (* TODO fix *)
    let entry = make_dns_entry del_dns addr in
    list#append_item entry;
    push (addr::rlst)
  in
  let set dns =
    list#set_empty ();
    push [];
    List.iter add_dns dns
  in
  let set_disabled flag =
    address#set_disabled flag;
    add_but#set_disabled flag;
  in
  add_but#listen_click_lwt (fun _ _ ->
      begin match S.value address#s_input with
      | Some addr -> add_dns addr
      | _ -> ()
      end;
      Lwt.return_unit) |> Lwt.ignore_result;

  full_box, signal, set, set_disabled

let make_routes (routes : Network_config.address list) =
  let make_route_entry del_route route =
    let (addr,mask) = route in
    let text        = (Ipaddr.V4.to_string addr) ^ "/" ^ (Int32.to_string mask) in
    let del_button  = new Button.t ~label:"delete" () in
    let item        = new Item_list.Item.t ~text ~meta:del_button ~value:() () in
    del_button#listen_click_lwt (fun _ _ ->
        del_route item route |> Lwt.return) |> Lwt.ignore_result;
    item
  in

  let header = new Typography.Text.t ~font:Subtitle_1 ~text:"Список статических маршрутов" () in
  let list   = new Item_list.t ~items:[] () in
  
  let address =
    new Textfield.t
      ~label:"Адрес"
      ~input_type:IPV4
      () in
  let mask =
    new Textfield.t
      ~label:"Маска подсети"
      ~input_type:(Integer (Some 0, Some 32))
      () in
  let add_but = new Button.t ~label:"Добавить" () in
  let add_box = new Hbox.t ~widgets:[address#widget; mask#widget; add_but#widget] () in

  let full_box = new Vbox.t ~widgets:[header#widget; list#widget; add_box#widget] () in

  let signal, push =
    S.create ~eq:(equal_list Network_config.equal_address) [] in

  let del_route item (addr, mask) =
    list#remove_item item;
    push
    @@ List.filter (fun route -> not (Network_config.equal_address (addr,mask) route))
    @@ S.value signal
  in
  let add_route (addr, mask) =
    let rlst = S.value signal in
    if List.exists (Network_config.equal_address (addr,mask)) rlst
    then failwith "route exists"; (* TODO fix *)
    let entry = make_route_entry del_route (addr, mask) in
    list#append_item entry;
    push ((addr, mask)::rlst)
  in
  let set routes =
    list#set_empty ();
    push [];
    List.iter add_route routes
  in
  let set_disabled flag =
    address#set_disabled flag;
    mask#set_disabled flag;
    add_but#set_disabled flag;
  in
  add_but#listen_click_lwt (fun _ _ ->
      begin match S.value address#s_input, S.value mask#s_input with
      | Some addr, Some mask -> add_route (addr, Int32.of_int mask)
      | _ -> ()
      end;
      Lwt.return_unit) |> Lwt.ignore_result;
  full_box, signal, set, set_disabled

let make_ipv4 (ipv4 : Network_config.ipv4_conf) =
  let ipv4_head =
    new Card.Primary.t
      ~widgets:[new Card.Primary.title "Настройки IP" ()]
      () in
  let meth =
    new Form_field.t
      ~input:(new Switch.t ())
      ~label:"Автоматическая настройка"
      () in
  let address =
    new Textfield.t
      ~label:"Адрес"
      ~input_type:IPV4
      () in
  let mask =
    new Textfield.t
      ~label:"Маска подсети"
      ~input_type:(Integer (Some 0, Some 32))
      () in
  let gateway =
    new Textfield.t
      ~label:"Шлюз"
      ~input_type:IPV4
      () in
  let dns, dns_s, dns_set, dns_disable = make_dns ipv4.dns in
  let routes, routes_s, routes_set, routes_disable = make_routes ipv4.routes.static in

  let signal, push =
    S.create ~eq:Network_config.equal_ipv4_conf ipv4 in

  let set (ipv4 : Network_config.ipv4_conf) =
    meth#input_widget#set_checked (Network_config.equal_meth ipv4.meth Auto);
    address#set_value @@ fst ipv4.address;
    mask#set_value (Int32.to_int @@ snd ipv4.address);
    (match ipv4.routes.gateway with
     | None -> ()
     | Some gw -> gateway#set_value gw);
    dns_set ipv4.dns;
    routes_set ipv4.routes.static;
    push ipv4
  in

  (* disable settings on Auto config *)
  S.map ~eq:(=) (fun disabled ->
      address#set_disabled disabled;
      mask#set_disabled disabled;
      gateway#set_disabled disabled;
      dns_disable disabled;
      routes_disable disabled) meth#input_widget#s_state
  |> S.keep;

  (* disable routes on gateway config *)
  S.map ~eq:(=) (function
      | None -> routes_disable false
      | _ -> routes_disable true) gateway#s_input
  |> S.keep;

  let media =
    new Card.Media.t
      ~widgets:[new Vbox.t ~widgets:[ meth#widget
                                    ; address#widget
                                    ; mask#widget
                                    ; gateway#widget
                                    ; dns#widget
                                    ; routes#widget ] ()]
      () in
  media#style##.margin := Utils.px_js 15;
  let ipv4_sets =
    new Card.t
      ~widgets:[ ipv4_head#widget
               ; media#widget ]
      () in
  let signal =
    S.l6 ~eq:Network_config.equal_ipv4_conf
      (fun (config : Network_config.ipv4_conf) meth address mask gateway routes ->
         let addr = match address with
           | None -> fst config.address
           | Some x -> x in
         let mask = match mask with
           | None -> snd config.address
           | Some x -> x in
         { config with
           meth = if meth then Auto else Manual
         ; address = (addr, mask)
         ; routes = { gateway = gateway; static = routes } }) (* TODO fix *)
      signal
      meth#input_widget#s_state
      address#s_input
      (S.map ~eq:(equal_option Int32.equal)
         (CCOpt.map Int32.of_int) mask#s_input)
      gateway#s_input
      routes_s
  in
  let signal =
    S.l2 ~eq:Network_config.equal_ipv4_conf
      (fun (config : Network_config.ipv4_conf) dns ->
        { config with dns } ) signal dns_s in
  ipv4_sets, signal, set

let make_card is_root post (config : Network_config.t) =
  let cancel = new Button.t ~label:"Отмена" () in
  let accept = new Button.t ~label:"Применить" () in
  let warning =
    new Dialog.t
      ~title:"Внимание!"
      ~actions:[ Dialog.Action.make ~typ:`Cancel cancel
               ; Dialog.Action.make ~typ:`Accept accept ]
      ~content:(`String "Применение настроек может привести к разрыву соединения. \
                         Вы уверены, что хотите применить данные настройки?") ()
  in
  Widget.append_to_body warning;

  let eth_sets, eth_s, eth_set = make_eth config.ethernet in
  let ipv4_sets, ipv4_s, ipv4_set  = make_ipv4 config.ipv4 in

  let apply = new Button.t ~label:"Применить" () in
  apply#set_disabled (not is_root);

  let signal, push =
    S.create ~eq:Network_config.equal config in

  let set (config : Network_config.t) =
    eth_set config.ethernet;
    ipv4_set config.ipv4;
    push config
  in

  (* init *)
  set config;
  let signal =
    S.l3 ~eq:Network_config.equal
      (fun (config : Network_config.t) ipv4 ethernet ->
        { config with ipv4; ethernet })
      signal ipv4_s eth_s
  in
  apply#listen_click_lwt (fun _ _ ->
      warning#show_await ()
      >>= function
      | `Accept -> post @@ S.value signal
      | `Cancel -> Lwt.return_unit) |> Lwt.ignore_result;

  let box = new Vbox.t ~widgets:[eth_sets#widget; ipv4_sets#widget; apply#widget] () in
  List.iter (fun card -> card#style##.marginBottom := Utils.px_js 15) box#widgets;
  box, set

let page user =
  let is_root = User.equal user `Root in
  Requests.get_config () >>= function
  | Error e -> Lwt.fail_with (Api_js.Http.error_to_string e)
  | Ok config ->
    let event, push = E.create () in
    let post new_config =
      Requests.post_config new_config
      >>= function
      | Ok _ -> (push new_config; Lwt.return_unit)
      | Error _ ->
        Requests.get_config () >>= function
        | Error _ -> (push config; Lwt.return_unit)
        | Ok config -> (push config; Lwt.return_unit)
    in
    let card, set = make_card is_root post config in
    E.keep @@ E.map (fun config ->
        print_endline (Yojson.Safe.pretty_to_string @@ Network_config.to_yojson config);
        set config) event;
    Lwt.return card
