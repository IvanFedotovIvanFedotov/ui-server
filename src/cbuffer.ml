type t = { buf : (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
         ; sz  : int
         }

let create sz =
  let buf = Bigarray.(Array1.create char c_layout sz) in
  { buf; sz }

let to_bytes b =
  let module BA = Bigarray.Array1 in
  let r = Bytes.create b.sz in
  Bytes.iteri (fun i _ ->
      let c = BA.unsafe_get b.buf i in Bytes.set r i c) r;
  r
  
let of_string s =
  let sz  = CCString.length s in
  let buf = Bigarray.(Array1.create char c_layout sz) in
  CCString.iteri (fun i c -> Bigarray.Array1.unsafe_set buf i c) s;
  { buf; sz }

let to_string b =
  let module BA = Bigarray.Array1 in
  let r = Bytes.create b.sz in
  Bytes.iteri (fun i _ ->
      let c = BA.unsafe_get b.buf i in Bytes.set r i c) r;
  Bytes.unsafe_to_string r
  
(**
   Concatenates a list of bufs [an .. a2 a1] 
   into a single buf a1a2..an
 *) 
let rev_concat arrs =
  let rec cat sz = function
    | []    -> Bigarray.(Array1.create char c_layout sz), 0
    | x::xs ->
       let arr, pos = cat (sz + x.sz) xs in
       let end' = x.sz in
       for i = 0 to (end' - 1) do
         Bigarray.Array1.unsafe_set
           arr
           (pos + i)
           (Bigarray.Array1.unsafe_get x.buf i)
       done;
       arr, (end' + pos)
  in
  let buf, sz = cat 0 arrs in
  { buf; sz }

let concat arrs = rev_concat (List.rev arrs)

exception Not_equal
  
let split on buf =
  let module BA = Bigarray.Array1 in
  
  let onsz = CCString.length on in
  if onsz >= buf.sz
  then []
  else

    let rec lst_to_pairs = function
      | [] -> []
      | [_] -> failwith "odd number of elements"
      | x::y::xs -> (x,y)::(lst_to_pairs xs)
    in

    let rec find_subs i acc =
      if i < 0 then acc
      else try CCString.iteri
                 (fun j c ->
                   if c <> BA.unsafe_get buf.buf (i + j)
                   then raise_notrace Not_equal)
                 on;
               find_subs (i - onsz) (i::(i+onsz)::acc)
           with Not_equal -> find_subs (i - 1) acc
    in

    let points =
      ([0] @ (find_subs (buf.sz - onsz) []) @ [buf.sz])
      |> lst_to_pairs |> List.filter (fun (x,y) -> x <> y)
    in
    match points with
    | [] | [(_,_)] -> [buf]
    | _ ->
       List.map
         (fun (s,e) -> { buf = BA.sub buf.buf s (e - s)
                       ; sz  = e - s })
         points

let split_size size buf =
  let module BA = Bigarray.Array1 in

  let rec split point acc =
    if buf.sz - point < size
    then
      let sz = (buf.sz - point) in
      let r  = { buf = BA.sub buf.buf point sz; sz } in
      r::acc
    else
      let sz = size in
      let r  = { buf = BA.sub buf.buf point sz; sz } in
      split (point+size) (r::acc)

  in List.rev @@ split 0 []

let fold f acc buf =
  let module BA = Bigarray.Array1 in
  let rec fold' point acc =
    if point = buf.sz
    then acc
    else fold' (succ point) (f acc (BA.unsafe_get buf.buf point))
  in fold' 0 acc

let map f buf =
  let module BA = Bigarray.Array1 in
  let nb = create buf.sz in
  let rec map' point =
    if point = buf.sz
    then nb
    else begin
      BA.unsafe_get buf.buf point
      |> f
      |> BA.unsafe_set nb.buf point;
      map' (succ point)
      end
  in map' 0

let iter (f : char -> unit) buf =
  let module BA = Bigarray.Array1 in
  for i = 0 to buf.sz do
    f (BA.unsafe_get buf.buf i)
  done

let iteri (f : int -> char -> unit) buf =
  let module BA = Bigarray.Array1 in
  for i = 0 to buf.sz do
    f i (BA.unsafe_get buf.buf i)
  done

let modify f buf =
  let module BA = Bigarray.Array1 in
  for i = 0 to buf.sz do
    BA.unsafe_set buf.buf i @@ f @@ BA.unsafe_get buf.buf i
  done

let modifyi f buf =
  let module BA = Bigarray.Array1 in
  for i = 0 to buf.sz do
    BA.unsafe_set buf.buf i @@ f i @@ BA.unsafe_get buf.buf i
  done

exception Bad_boundaries
   
let sub buf ?(start = 0) len =
  if len + start > buf.sz || start < 0 || len <= 0
  then raise Bad_boundaries;
  { buf = Bigarray.Array1.sub buf.buf start len
  ; sz  = len
  }