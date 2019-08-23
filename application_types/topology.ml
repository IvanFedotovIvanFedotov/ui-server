type state =
  [ `Fine
  | `Detect
  | `Init
  | `No_response
  ] [@@deriving yojson, show, eq, ord]

let state_to_string = function
  | `Fine -> "fine"
  | `Init -> "init"
  | `No_response -> "no-response"

let state_of_string = function
  | "fine" -> Some `Fine
  | "init" -> Some `Init
  | "no-response" -> Some `No_response
  | _ -> None

type input =
  | RF
  | TSOIP
  | ASI
  | SPI [@@deriving show, eq, enum, ord]

type process_type = string [@@deriving yojson, show, eq, ord]

let input_to_string = function
  | RF -> "RF"
  | TSOIP -> "TSOIP"
  | ASI -> "ASI"
  | SPI -> "SPI"

let input_of_string = function
  | "RF" -> Ok RF
  | "ASI" -> Ok ASI
  | "SPI" -> Ok SPI
  | "TSOIP" -> Ok TSOIP
  | s -> Error ("input_of_string: bad input string: " ^ s)

let input_to_yojson x = `String (input_to_string x)

let input_of_yojson = function
  | `String s -> input_of_string s
  | _ as e -> Error ("input_of_yojson: unknown value: " ^ (Yojson.Safe.to_string e))

type version = int [@@deriving yojson, show, eq, ord]

type id = int [@@deriving yojson, show, eq, ord]

module Env = Map.Make(String)

type env = string Env.t [@@deriving ord]

let env_to_yojson (e : env) : Yojson.Safe.t =
  `Assoc (Env.fold (fun k v a -> (k, `String v) :: a) e [])

let env_of_yojson : Yojson.Safe.t -> (env, string) result = function
  | `Assoc ls -> begin
      try ls
          |> List.map (function
                 | (k, `String v) -> (k, v)
                 | _ -> raise_notrace (Failure "env_of_yojson: value should be string"))
          |> List.fold_left (fun env (k, v) -> Env.add k v env) Env.empty
          |> fun x -> Ok x
      with Failure e -> Error e
    end
  | j ->
     let e = Printf.sprintf "env_of_yojson: got bad value - %s" (Yojson.Safe.to_string j) in
     Error e

(* TODO proper pp *)
let pp_env _ppf _ = () (* Format. Env. String.pp String.pp*)
let equal_env = Env.equal String.equal

type t =
  [ `CPU of topo_cpu
  | `Boards of topo_board list
  ] [@@deriving yojson, show, eq, ord]

and topo_entry =
  | Input : topo_input -> topo_entry
  | Board : topo_board -> topo_entry

and topo_input =
  { input : input
  ; id : int
  }

and topo_board =
  { model : string
  ; manufacturer : string
  ; version : version
  ; control : int
  ; connection : (state [@default `No_response])
  ; env : (env [@default Env.empty])
  ; ports : topo_port list
  ; logs : (string option [@default None])
  }

and topo_port =
  { port : int
  ; listening : (bool [@default false])
  ; has_sync : (bool [@default false])
  ; switchable : (bool [@default false])
  ; child : topo_entry
  }

and topo_cpu  =
  { process : process_type
  ; ifaces : topo_interface list
  }

and topo_interface =
  { iface : string
  ; conn : topo_entry
  }

let of_string s =
  Yojson.Safe.from_string s
  |> of_yojson
  |> function Ok v -> v | Error e -> failwith e

let to_string x =
  to_yojson x
  |> Yojson.Safe.to_string

module Show_topo_input = struct
  type t = topo_input
  let typ = "topo input"
  let to_string (x : t) =
    input_to_string x.input
    ^ "-"
    ^ string_of_int x.id
  let of_string s : t =
    String.split_on_char '-' s
    |> (function
        | [ input; id ] ->
           { input = (match input_of_string input with Ok v -> v | _ -> failwith "Topo_input")
           ; id    = int_of_string id
           }
        | _ -> failwith "bad input string")
end

let iter_boards f =
  let rec iter_entry f = function
    | Input _ -> ()
    | Board b -> iter_board f b
  and iter_board f b =
    f b;
    List.iter (fun port -> iter_entry f port.child) b.ports
  and iter_cpu f c =
    List.iter (fun iface -> iter_entry f iface.conn) c.ifaces
  in function
  | `CPU c -> iter_cpu f c
  | `Boards bs -> List.iter (iter_board f) bs
                       
let cpu_subbranches = function
  | `Boards _ -> `No_cpu
  | `CPU c -> `Branches (List.map (fun i -> i.conn) c.ifaces)

let get_entries = function
  | `Boards l -> List.fold_left (fun acc b -> (List.map (fun p -> p.child) b.ports) @ acc) [] l
  | `CPU c -> List.map (fun i -> i.conn) c.ifaces

let get_input_name (i : topo_input) =
  let to_string s = Printf.sprintf "%s-%d" s i.id in
  match i.input with
  | RF -> to_string "RF"
  | TSOIP -> to_string "TSoIP"
  | ASI -> to_string "ASI"
  | SPI -> to_string "SPI"

let get_board_name (b : topo_board) =
  Printf.sprintf "%s %s" b.manufacturer b.model

let get_inputs t =
  let rec get acc = function
    | Input x -> x :: acc
    | Board x -> List.fold_left (fun acc x -> get acc x.child) acc x.ports in
  let topo_inputs_cpu   c = List.fold_left (fun acc i -> get acc i.conn) [] c.ifaces in
  let topo_inputs_board b = List.fold_left (fun acc p -> get acc p.child) [] b.ports in
  match t with
  | `CPU c -> topo_inputs_cpu c
  | `Boards bs -> List.fold_left (fun acc b -> (topo_inputs_board b) @ acc) [] bs

let get_boards t =
  let rec get acc = function
    | Input _ -> acc
    | Board x -> List.fold_left (fun acc x -> get acc x.child) (x :: acc) x.ports
  in
  let topo_boards_cpu   c = List.fold_left (fun acc i -> get acc i.conn) [] c.ifaces in
  let topo_boards_board b = List.fold_left (fun acc p -> get acc p.child) [b] b.ports in
  match t with
  | `CPU c -> topo_boards_cpu c
  | `Boards bs -> List.fold_left (fun acc b -> (topo_boards_board b) @ acc) [] bs

let get_paths t =
  let topo_paths acc =
    let rec add_node acc paths = function
      | Input i -> (i,acc) :: paths
      | Board b -> (let ports = List.map (fun x -> x.child) b.ports in
                    match ports with
                    | [] -> paths
                    | l  -> List.fold_left (fun a x -> add_node (b :: acc) a x) paths l)
    in
    List.fold_left (fun a x -> add_node acc a x) []
  in
  let add_cpu cpu (i,bl) = (i,bl,cpu) in
  match t with
  | `Boards bs ->
     List.map (fun b -> List.map (add_cpu None) @@ topo_paths [b] @@ List.map (fun p -> p.child) b.ports) bs
     |> List.concat
  | `CPU c     ->
     List.map (add_cpu (Some c)) @@ topo_paths [] @@ List.map (fun p -> p.conn) c.ifaces

let board_list_for_input input : t -> 'a =
  let rec fmap_first f = function
    | [] -> None
    | h::tl ->
       match f h with
       | Some _ as res -> res
       | None -> fmap_first f tl
  in
  let rec traverse acc = function
    | Input i when equal_topo_input i input -> Some (List.rev acc)
    | Input _ -> None
    | Board b ->
       b.ports
       |> fmap_first (fun port -> (traverse (b::acc)) port.child)
  in function
  | `CPU c ->
     fmap_first (fun iface -> traverse [] iface.conn) c.ifaces
  | `Boards bs ->
     fmap_first (fun board ->
         fmap_first (fun port -> traverse [] port.child) board.ports)
       bs

let topo_inputs_of_topo_board (board : topo_board) =
  let rec aux acc = function
    | [] -> acc
    | port :: tl ->
      match port.child with
      | Board b -> aux ((aux acc b.ports) @ acc) tl
      | Input i -> aux (i :: acc) tl in
  List.sort_uniq compare_topo_input
  @@ aux [] board.ports

let topo_inputs_of_topo_cpu (cpu : topo_cpu) =
  List.sort_uniq compare_topo_input
  @@ List.fold_left (fun acc ({ conn; _ } : topo_interface)->
      match conn with
      | Input i -> i :: acc
      | Board b -> topo_inputs_of_topo_board b @ acc)
    [] cpu.ifaces

type board_id =
  { manufacturer : string
  ; model : string
  ; version : int
  } [@@deriving eq, ord, yojson]

let make_board_path (b : board_id) (control : int) =
  Printf.sprintf "board/%s/%s/%d"
    (String.lowercase_ascii b.manufacturer)
    (String.lowercase_ascii b.model)
    control

let board_id_of_topo_board ({ model; manufacturer; version; _ } : topo_board) =
  { manufacturer; model; version }

type boards = (board_id * int list) list [@@deriving yojson]
