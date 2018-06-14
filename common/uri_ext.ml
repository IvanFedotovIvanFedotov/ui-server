include Uri

let (%) f g x = f (g x)
      
module Path : sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val s : string -> t
  val next : t -> string option * t
  val append : t -> t -> t
  val (/) : t -> t -> t
end = struct
  type t = Str of string
         | Lst of string list
                                         (*      
  let root x =
    let s = CCString.drop_while ((=) '/') x in
    let ind = CCString.find ~sub:"/" s in
    if ind < 0 then (None, x)
    else let (root, rest) = CCString.take_drop ind s in
         (Some root, rest)
                                          *)
              
  let split s =
    String.split_on_char '/' s
    |> List.filter (not % String.equal "")

  let merge = String.concat "/"

  let of_string s = Str s

  let to_string = function
    | Str s -> s
    | Lst l -> merge l

  let s = of_string

  let rec next = function
    | Lst [] as l -> (None, l)
    | Lst (h::tl) -> (Some h, Lst tl)
    | Str s -> next (Lst (split s))

  let append l r =
    match l, r with
    | Lst l, Lst r -> Lst (l @ r)
    | Lst l, Str r -> Lst (l @ (split r))
    | Str l, Lst r -> Lst ((split l) @ r)
    | Str l, Str r -> Str (l ^ "/" ^ r)

  let (/) = append
             
end
            
module Query = struct

  type t = (string * string list) list

  let grep_arg (name : string) lst =
    let rec grep' acc = function
      | [] -> [], lst
      | (title, arg)::tl ->
         if String.equal title name
         then (arg, (List.rev acc) @ tl)
         else grep' ((title, arg)::acc) tl
    in grep' [] lst
     
  module type Show = sig
    type t
    val to_string : t -> string
    val of_string : string -> t 
  end

  module type Convert = sig
    type t
    val to_query : t -> string list
    val of_query : string list -> t 
  end

  module String = struct
    type t = string
    let of_string x = x
    let to_string x = x
  end

  module Int = struct
    type t = int
    let of_string = int_of_string
    let to_string = string_of_int
  end

  module List (E : Show) = struct
    type t = E.t list
    let of_query = List.map E.of_string
    let to_query = List.map E.to_string
  end

  module Single (E : Show) = struct
    type t = E.t
    let of_query = function [v] -> E.of_string v
    let to_query v = [ E.to_string v ]
  end

  module Option (E : Show) = struct
    type t = E.t option
    let of_query = function [] -> None | [v] -> Some (E.of_string v)
    let to_query = function Some v -> [ E.to_string v ] | None -> []
  end                      

  type (_,_) compose =
    | (::) : (string * (module Convert with type t = 'a)) * ('b, 'c) compose -> ('a -> 'b, 'c) compose
    | []   : ('c, 'c) compose

  let rec make_q : type ty v. (t -> v) -> (ty, v) compose -> ty =
    fun k ->
    function
    | [] -> k []
    | (q, (module C)) :: rest ->
       let f x = make_q (fun lst  -> k ((q, (C.to_query x))::lst)) rest
       in f

  let make_query q = make_q (fun x -> x) q
                   
  let rec parse_q : type ty v. ty -> (ty, v) compose -> t -> v =
    fun k ->
    function
    | [] ->
       fun _ -> k
    | (q, (module C)) :: rest ->
       fun sl ->
       let (arg, args) = grep_arg q sl in
       parse_q (k (C.of_query arg)) rest args

  let parse_query lst f queries =
    parse_q f lst queries
    
end

type sep = { scheme : string option
           ; path   : Path.t
           ; query  : Query.t
           }
      
let sep u : sep =
  { scheme = scheme u
  ; path = Path.of_string @@ path u
  ; query = query u }

let upgrade_path s path : sep = { s with path }
  
let sep_path (s : sep) = s.path

(** TO DO remove *)
let split s =
  String.split_on_char '/' s
  |> List.filter (not % String.equal "")
