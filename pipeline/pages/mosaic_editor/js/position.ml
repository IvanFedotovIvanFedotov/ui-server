open Js_of_ocaml
open Components

let ( % ) f g x = f (g x)

module Attr = struct
  let keep_aspect_ratio = "data-keep-aspect-ratio"
  let aspect_ratio = "data-aspect-ratio"
  let width = "data-width"
  let height = "data-height"
  let left = "data-left"
  let top = "data-top"
end

type line =
  { is_vertical : bool (* Is line vertical *)
  ; is_multiple : bool (* Multiple intersection detected *)
  ; is_center : bool
  ; origin : int
  }

type line_f =
  { is_vertical : bool (* Is line vertical *)
  ; is_multiple : bool (* Multiple intersection detected *)
  ; is_center : bool
  ; origin_f : float
  }


type resize_direction =
  | Top_left
  | Top_right
  | Bottom_left
  | Bottom_right
  | Top
  | Bottom
  | Left
  | Right

type line_align_direction =
  | Htop
  | Hcenter
  | Hbottom
  | Vleft
  | Vcenter
  | Vright
  | Nill


type t =
  { x : int
  ; y : int
  ; w : int
  ; h : int
  }

let empty =
  { x = 0
  ; y = 0
  ; w = 0
  ; h = 0
  }

type t_f =
  { x_f : float
  ; y_f : float
  ; w_f : float
  ; h_f : float
  }

let fmod v1 v2 =
  if v2 = 0.0 
  then 0.0
  else let val1 = floor (v1 /. v2) in
    let ost = v1 -. val1 in 
    ost
  
let fabs (v1 : float) = if v1 >= 0.0 
  then v1
  else (-. v1)

let show { x; y; w; h } =
  Printf.sprintf "x=%d, y=%d, w=%d, h=%d"
    x y w h


let compare (a : t) (b : t) =
  let c = compare a.x b.x in
  if c <> 0 then c
  else (let c = compare a.y b.y in
        if c <> 0 then c
        else (let c = compare a.w b.w in
              if c <> 0 then c
              else compare a.h b.h))

let equal (a : t) (b : t) =
  a.x = b.x && a.y = b.y && a.w = b.w && a.h = b.h

(** Checks if two elements collide, returns [true] if so and [false] otherwise *)
let collides (pos1 : t) (pos2 : t) =
  if (pos1.x + pos1.w <= pos2.x) then false
  else if (pos1.x >= pos2.x + pos2.w) then false
  else if (pos1.y + pos1.h <= pos2.y) then false
  else if (pos1.y >= pos2.y + pos2.h) then false
  else true

(** Returns first collision *)
let collision_map
    ~(f : 'a -> t)
    (pos : 'a)
    (l : 'a list) =
  let rec aux = function
    | [] -> None
    | hd :: tl ->
      if collides (f pos) (f hd)
      then Some hd
      else aux tl
  in
  aux l

(** Returns all collisions *)
let collisions_map
    ~(f : 'a -> t)
    (item : 'a)
    (l : 'a list) =
  List.fold_left (fun acc (x : 'a) ->
      if collides (f item) (f x)
      then x :: acc
      else acc) [] l

(** Checks if element collides with other elements *)
let has_collision_map ~f x (l : 'a list) =
  match collision_map ~f x l with
  | None -> false | Some _ -> true

let collision x l = collision_map ~f:(fun x -> x) x l

let collisions x l = collisions_map ~f:(fun x -> x) x l

let has_collision x l = has_collision_map ~f:(fun x -> x) x l

(** Changes top and left coordinates to correspond parent dimentions *)
(* original
   let fix_xy par_w par_h (p : t) =
   let x = if p.x < 0 then 0 else if p.x + p.w > par_w then par_w - p.w else p.x in
   let y =
    match par_h with
    | None -> if p.y < 0 then 0 else p.y
    | Some ph -> if p.y < 0 then 0 else if p.y + p.h > ph then ph - p.h else p.y
   in
   { p with x; y }
*)

(** Changes top and left coordinates to correspond parent dimentions *)
let fix_xy ?min_x ?min_y ?max_x ?max_y (*par_w par_h*) (p : t_f) =
  let x = if p.x_f < 0.0 then 0.0 else if p.x_f +. p.w_f > 1.0 then 1.0 -. p.w_f else p.x_f in
  let y = if p.y_f < 0.0 then 0.0 else if p.y_f +. p.h_f > 1.0 then 1.0 -. p.h_f else p.y_f
  in
  let x = match max_x with
    | Some max -> if x > max then max else x
    | None -> x
  in
  let x = match min_x with
    | Some min -> if x > min then min else x
    | None -> x
  in
  let y = match max_y with
    | Some max -> if y > max then max else y
    | None -> y
  in
  let y = match min_y with
    | Some min -> if y > min then min else y
    | None -> y
  in
  { p with x_f = x; y_f = y }

(** Changes width to correspond provided constraints *)
let fix_w ?max_w ?(min_w = 0.0) (p : t_f) =
  let w = match max_w with
    | Some max ->
      if p.w_f > max then max
      else if p.w_f < min_w then min_w
      else p.w_f
    | None -> if p.w_f < min_w then min_w else p.w_f
  in
  let w =
    if p.x_f +. w > 1.0
    then 1.0 -. p.x_f
    else if p.x_f < 0.0
    then w +. p.x_f
    else w in
  { p with w_f = w }

(** Changes height to correspond provided constraints *)
let fix_h ?max_h ?(min_h = 0.0) (p : t_f) =
  let h = match max_h with
    | Some max -> if p.h_f > max then max else if p.h_f < min_h then min_h else p.h_f
    | None -> if p.h_f < min_h then min_h else p.h_f
  in
  let h = if p.y_f +. h > 1.0
      then 1.0 -. p.y_f
      else if p.y_f < 0.0
      then h +. p.y_f
      else h
  in
  { p with h_f = h }

(** Changes width and height to correspond provided constraints *)
let fix_wh ?max_w ?min_w ?max_h ?min_h =
  (fix_h ?max_h ?min_h) % (fix_w ?max_w ?min_w)

(** Changes width and height to correspond provided aspect *)
let fix_aspect (p : t) (aspect : int * int) =
  let w =
    if p.w mod (fst aspect) <> 0 then
      let w = (p.w / (fst aspect)) * (fst aspect) in
      if w = 0 then (fst aspect)  else w
    else p.w
  in
  let h =
    if p.h mod (snd aspect) <> 0 then
      let h = (p.h / (snd aspect)) * (snd aspect) in
      if h = 0 then (snd aspect) else h
    else p.h
  in
  let sw = w / (fst aspect) in
  let sh = h / (snd aspect) in
  let w, h =
    if sw > sh
    then (fst aspect) * sh, h
    else w, (snd aspect) * sw
  in
  { p with w; h }

let apply_to_element (pos : t) (elt : #Dom_html.element Js.t) =
  elt##.style##.width := Utils.px_js pos.w;
  elt##.style##.left := Utils.px_js pos.x;
  elt##.style##.height := Utils.px_js pos.h;
  elt##.style##.top := Utils.px_js pos.y

let of_element (elt : #Dom_html.element Js.t) =
  { x = elt##.offsetLeft
  ; y = elt##.offsetTop
  ; w = elt##.offsetWidth
  ; h = elt##.offsetHeight
  }

let to_client_rect (t : t) : Dom_html.clientRect Js.t =
  object%js
    val top = float_of_int t.y
    val left = float_of_int t.x
    val right = float_of_int @@ t.x + t.w
    val bottom = float_of_int @@ t.y + t.h
    val width = Js.def @@ float_of_int t.w
    val height = Js.def @@ float_of_int t.h
  end

let of_client_rect (rect : Dom_html.clientRect Js.t) : t =
  { x = int_of_float rect##.left
  ; y = int_of_float rect##.top
  ; w = Js.Optdef.case rect##.width (fun () -> 0) int_of_float
  ; h = Js.Optdef.case rect##.height (fun () -> 0) int_of_float
  }

let default_aspect_ratio = 1.

let string_of_float = Printf.sprintf "%g"

let get_int_attribute (elt : #Dom_html.element Js.t) attr : int =
  match Element.get_attribute elt attr with
  | None -> 0
  | Some x ->
    match int_of_string_opt x with
    | None -> 0
    | Some x -> x

let get_original_width (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.width

let get_original_height (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.height

let get_original_left (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.left

let get_original_top (elt : #Dom_html.element Js.t) : int =
  get_int_attribute elt Attr.top

let get_original_aspect_ratio (elt : #Dom_html.element Js.t) : float option =
  match Element.get_attribute elt Attr.aspect_ratio with
  | Some x -> Some (float_of_string x)
  | None ->
    let w, h = get_original_width elt, get_original_height elt in
    if w = 0 || h = 0
    then None
    else
      let ar = (float_of_int w) /. (float_of_int h) in
      Element.set_attribute elt Attr.aspect_ratio (string_of_float ar);
      Some ar

(* min_distance - pixels
   return: (other element align as line_align_direction *
            minimum distance of several lines of one align as int) 
*)
let line_find_closest_align
    (item : Dom_html.element Js.t)
    (pos : t_f)
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float)
    min_distance
    line_align_val =
  let rec count_aligns line_align_val (distance: (line_align_direction * float)) = function
    | [] -> distance
    | hd :: tl ->
      let icompare = 
        { x_f = (float_of_int (of_element hd).x) /. parent_w_f
        ; y_f = (float_of_int (of_element hd).y) /. parent_h_f
        ; w_f = (float_of_int (of_element hd).w) /. parent_w_f
        ; h_f = (float_of_int (of_element hd).h) /. parent_h_f
        }
      in
      let (distance : line_align_direction * float) =
        if Element.equal item hd
        then count_aligns line_align_val distance tl (*distance*) else
          match line_align_val with
          | Htop ->
            let dist1 = pos.y_f -. icompare.y_f in
            let dist2 = pos.y_f -. icompare.y_f -. icompare.h_f /. 2.0 in
            let dist3 = pos.y_f -. icompare.y_f -. icompare.h_f in
            if (fabs dist1 < min_distance)
            && (fabs (snd distance) > fabs dist1)
            && (fabs dist1 <= fabs dist2)
            && (fabs dist1 <= fabs dist3)
            then Htop, dist1
            else if (fabs dist2 < min_distance)
                 && (fabs (snd distance) > fabs dist2)
                 && (fabs dist2 <= fabs dist1)
                 && (fabs dist2 <= fabs dist3)
            then Hcenter, dist2
            else if (fabs dist3 < min_distance)
                 && (fabs (snd distance) > fabs dist3)
                 && (fabs dist3 <= fabs dist1)
                 && (fabs dist3 <= fabs dist2)
            then Hbottom, dist3
            else distance
          | Hcenter ->
            let dist1 = pos.y_f +. pos.h_f /. 2.0 -. icompare.y_f in
            let dist2 = pos.y_f +. pos.h_f /. 2.0 -. icompare.y_f -. icompare.h_f /. 2.0 in
            let dist3 = pos.y_f +. pos.h_f /. 2.0 -. icompare.y_f -. icompare.h_f in
            if (fabs dist1 < min_distance)
            && (fabs (snd distance) > fabs dist1)
            && (fabs dist1 <= fabs dist2)
            && (fabs dist1 <= fabs dist3)
            then Htop, dist1
            else if (fabs dist2 < min_distance)
                 && (fabs (snd distance) > fabs dist2)
                 && (fabs dist2 <= fabs dist1)
                 && (fabs dist2 <= fabs dist3)
            then Hcenter, dist2
            else if (fabs dist3 < min_distance)
                 && (fabs (snd distance) > fabs dist3)
                 && (fabs dist3 <= fabs dist1)
                 && (fabs dist3 <= fabs dist2)
            then Hbottom, dist3
            else distance
          | Hbottom ->
            let dist1 = pos.y_f +. pos.h_f -. icompare.y_f in
            let dist2 = pos.y_f +. pos.h_f -. icompare.y_f -. icompare.h_f /. 2.0 in
            let dist3 = pos.y_f +. pos.h_f -. icompare.y_f -. icompare.h_f in
            if (fabs dist1 < min_distance)
            && (fabs (snd distance) > fabs dist1)
            && (fabs dist1 <= fabs dist2)
            && (fabs dist1 <= fabs dist3)
            then Htop, dist1
            else if (fabs dist2 < min_distance)
                 && (fabs (snd distance) > fabs dist2)
                 && (fabs dist2 <= fabs dist1)
                 && (fabs dist2 <= fabs dist3)
            then Hcenter, dist2
            else if (fabs dist3 < min_distance)
                 && (fabs (snd distance) > fabs dist3)
                 && (fabs dist3 <= fabs dist1)
                 && (fabs dist3 <= fabs dist2)
            then Hbottom, dist3
            else distance
          | Vleft ->
            let dist1 = pos.x_f -. icompare.x_f in
            let dist2 = pos.x_f -. icompare.x_f -. icompare.w_f /. 2.0 in
            let dist3 = pos.x_f -. icompare.x_f -. icompare.w_f in
            if (fabs dist1 < min_distance)
            && (fabs (snd distance) > fabs dist1)
            && (fabs dist1 <= fabs dist2)
            && (fabs dist1 <= fabs dist3)
            then Vleft, dist1
            else if (fabs dist2 < min_distance)
                 && (fabs (snd distance) > fabs dist2)
                 && (fabs dist2 <= fabs dist1)
                 && (fabs dist2 <= fabs dist3)
            then Vcenter, dist2
            else if (fabs dist3 < min_distance)
                 && (fabs (snd distance) > fabs dist3)
                 && (fabs dist3 <= fabs dist1)
                 && (fabs dist3 <= fabs dist2)
            then Vright, dist3
            else distance
          | Vcenter ->
            let dist1 = pos.x_f +. pos.w_f /. 2.0 -. icompare.x_f in
            let dist2 = pos.x_f +. pos.w_f /. 2.0 -. icompare.x_f -. icompare.w_f /. 2.0 in
            let dist3 = pos.x_f +. pos.w_f /. 2.0 -. icompare.x_f -. icompare.w_f in
            if (fabs dist1 < min_distance)
            && (fabs (snd distance) > fabs dist1)
            && (fabs dist1 <= fabs dist2)
            && (fabs dist1 <= fabs dist3)
            then Vleft, dist1
            else if (fabs dist2 < min_distance)
                 && (fabs (snd distance) > fabs dist2)
                 && (fabs dist2 <= fabs dist1)
                 && (fabs dist2 <= fabs dist3)
            then Vcenter, dist2
            else if (fabs dist3 < min_distance)
                 && (fabs (snd distance) > fabs dist3)
                 && (fabs dist3 <= fabs dist1)
                 && (fabs dist3 <= fabs dist2)
            then Vright, dist3
            else distance
          | Vright ->
            let dist1 = pos.x_f +. pos.w_f -. icompare.x_f in
            let dist2 = pos.x_f +. pos.w_f -. icompare.x_f -. icompare.w_f /. 2.0 in
            let dist3 = pos.x_f +. pos.w_f -. icompare.x_f -. icompare.w_f in
            if (fabs dist1 < min_distance)
            && (fabs (snd distance) > fabs dist1)
            && (fabs dist1 <= fabs dist2)
            && (fabs dist1 <= fabs dist3)
            then Vleft, dist1
            else if (fabs dist2 < min_distance)
                 && (fabs (snd distance) > fabs dist2)
                 && (fabs dist2 <= fabs dist1)
                 && (fabs dist2 <= fabs dist3)
            then Vcenter, dist2
            else if (fabs dist3 < min_distance)
                 && (fabs (snd distance) > fabs dist3)
                 && (fabs dist3 <= fabs dist1)
                 && (fabs dist3 <= fabs dist2)
            then Vright, dist3
            else distance
          | Nill -> distance
      in
      count_aligns line_align_val distance tl
  in
  count_aligns line_align_val (Nill, (min_distance +. 1.0)) items  (* FIX + 1.0*)

(* min_distance - pixels
   return: counts of align of selected type in min_distance interval *)
let line_align_count
    (item : Dom_html.element Js.t)
    (pos : t_f)
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float)
    (min_distance : float)
    line_align_val =
  let rec aux line_align_val counts = function
    | [] -> counts
    | hd :: tl ->
      let icompare = 
         { x_f = (float_of_int (of_element hd).x) /. parent_w_f
         ; y_f = (float_of_int (of_element hd).y) /. parent_h_f
         ; w_f = (float_of_int (of_element hd).w) /. parent_w_f
         ; h_f = (float_of_int (of_element hd).h) /. parent_h_f
         }
      in
      if Element.equal item hd
      then aux line_align_val counts tl (*counts*)
      else
        let counts =
          if (line_align_val = Htop
              && (fabs (pos.y_f -. icompare.y_f) < min_distance
                  || fabs (pos.y_f -. icompare.y_f -. icompare.h_f /. 2.0) < min_distance
                  || fabs (pos.y_f -. icompare.y_f -. icompare.h_f) < min_distance))
          || (line_align_val = Hcenter
              && (fabs (pos.y_f +. pos.h_f /. 2.0 -. icompare.y_f) < min_distance
                  || fabs (pos.y_f +. pos.h_f /. 2.0 -. icompare.y_f -. icompare.h_f /. 2.0) < min_distance
                  || fabs (pos.y_f +. pos.h_f /. 2.0 -. icompare.y_f -. icompare.h_f) < min_distance))
          || (line_align_val = Hbottom
              && (fabs (pos.y_f +. pos.h_f -. icompare.y_f) < min_distance
                  || fabs (pos.y_f +. pos.h_f -. icompare.y_f -. icompare.h_f /. 2.0) < min_distance
                  || fabs (pos.y_f +. pos.h_f -. icompare.y_f -. icompare.h_f) < min_distance))
          || (line_align_val = Vleft
              && (fabs (pos.x_f -. icompare.x_f) < min_distance
                  || fabs (pos.x_f -. icompare.x_f -. icompare.w_f /. 2.0) < min_distance
                  || fabs (pos.x_f -. icompare.x_f -. icompare.w_f) < min_distance))
          || (line_align_val = Vcenter
              && (fabs (pos.x_f +. pos.w_f /. 2.0 -. icompare.x_f) < min_distance
                  || fabs (pos.x_f +. pos.w_f /. 2.0 -. icompare.x_f -. icompare.w_f /. 2.0) < min_distance
                  || fabs (pos.x_f +. pos.w_f /. 2.0 -. icompare.x_f -. icompare.w_f) < min_distance))
          || (line_align_val = Vright
              && (fabs (pos.x_f +. pos.w_f -. icompare.x_f) < min_distance
                  || fabs (pos.x_f +. pos.w_f -. icompare.x_f -. icompare.w_f /. 2.0) < min_distance
                  || fabs (pos.x_f +. pos.w_f -. icompare.x_f -. icompare.w_f) < min_distance))
          then succ counts
          else counts
        in
        aux line_align_val counts tl
  in
  aux line_align_val 0 items

let make_line_properties align item pos min_distance items (parent_w_f, parent_h_f : float * float) =
  align,
  line_align_count item pos items (parent_w_f, parent_h_f) min_distance align,
  line_find_closest_align item pos items (parent_w_f, parent_h_f) min_distance align


let find_closest_one_snap_line
  (min_distance: float) 
  (items : (line_align_direction * int * (line_align_direction * float)) list) =
  let rec aux 
    (acc: (line_align_direction * int * (line_align_direction * float)) list)
    (snap_min_delta: float) 
    (items: (line_align_direction * int * (line_align_direction * float)) list)
    = match items with
    | [] -> acc
    | hd :: tl ->
      let (_, aligns_count, distance__align_other) = hd in
      let (_, distance) = distance__align_other in
(*      let snap_min_delta =
        if aligns_count > 0 && fabs distance < fabs snap_min_delta
        then distance
        else snap_min_delta 
        in
        *)
      let acc =
        if aligns_count > 0 && fabs distance < fabs snap_min_delta  &&
          fabs distance <= min_distance
        then hd :: []
        else acc in
      aux acc 
        (if aligns_count > 0 && fabs distance < fabs snap_min_delta
        then distance
        else snap_min_delta)
      tl in
      
  aux [] (min_distance +. 1.0) items  (* FIX + 1.0; (1.0 != 1 pixel) *)



(* return: direction, count aligns (0 = none align lines),
   closest line distance (if distance > min_distance = no find lines) *)
let hlines_for_move_action (item : Dom_html.element Js.t)
    pos 
    min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float) =
  find_closest_one_snap_line min_distance
    [ make_line_properties Htop item pos min_distance items (parent_w_f, parent_h_f)
    ; make_line_properties Hcenter item pos min_distance items (parent_w_f, parent_h_f)
    ; make_line_properties Hbottom item pos min_distance items (parent_w_f, parent_h_f)
    ]

let hlines_for_resize_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float)
    (direction : resize_direction) =
  let align_direction = match direction with
    | Top_left | Top_right | Top -> Htop
    | Bottom_left | Bottom_right | Bottom -> Hbottom
    | Left | Right -> Hcenter in
  [make_line_properties align_direction item pos min_distance items (parent_w_f, parent_h_f)]

let vlines_for_move_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float) =
  find_closest_one_snap_line min_distance
    [ make_line_properties Vleft item pos min_distance items (parent_w_f, parent_h_f)
    ; make_line_properties Vcenter item pos min_distance items (parent_w_f, parent_h_f)
    ; make_line_properties Vright item pos min_distance items (parent_w_f, parent_h_f)
    ]

let vlines_for_resize_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float)
    (direction : resize_direction) =
  let align_direction = match direction with
    | Top_left | Bottom_left | Left -> Vleft
    | Top_right | Bottom_right | Right -> Vright
    | Top | Bottom -> Vcenter in
  [make_line_properties align_direction item pos min_distance items (parent_w_f, parent_h_f)]

let get_snap 
  (item : Dom_html.element Js.t) 
  (coord: float)
  (min_distance:float) 
  (items : (line_align_direction * int * (line_align_direction * float)) list) =
  let rec aux (snap:float) (snap_min_delta:float) 
    (items: (line_align_direction * int * (line_align_direction * float)) list)
    :float
    = match items with
    | [] -> snap
    | (_, aligns_count, distance__align_other) :: tl ->
      let (_ , distance) = distance__align_other in
      let snap_min_delta =
        if aligns_count > 0 && fabs distance < fabs snap_min_delta
        then distance else snap_min_delta in
      let snap =
        if fabs snap_min_delta <= min_distance
        then coord -. snap_min_delta else snap in
      aux snap snap_min_delta tl in
  aux coord (min_distance +. 1.0) items  (* FIX + 1.0; (1.0 != 1 pixel) *)


let get_item_snap_y (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float) =
  get_snap item pos.y_f min_distance
  @@ hlines_for_move_action item pos min_distance items (parent_w_f, parent_h_f)

let get_item_snap_x (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float) =
  get_snap item pos.x_f min_distance
  @@ vlines_for_move_action item pos min_distance items (parent_w_f, parent_h_f)

let get_item_snap_position_for_move_action
    (item : Dom_html.element Js.t)
    pos
    min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float) =
  { x_f = get_item_snap_x item pos min_distance items (parent_w_f, parent_h_f)
  ; y_f = get_item_snap_y item pos min_distance items (parent_w_f, parent_h_f)
  ; w_f = pos.w_f
  ; h_f = pos.h_f
  }

let get_item_snap_position_for_resize_action
    (item : Dom_html.element Js.t)
    pos
    min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float)
    (direction : resize_direction) =
  let make_line align = make_line_properties align item pos min_distance items (parent_w_f, parent_h_f) in
  match direction with
  | Top_left ->
    let snap_list_x = [make_line Vleft] in
    let snap_list_y = [make_line Htop] in
    { x_f = get_snap item pos.x_f min_distance snap_list_x
    ; y_f = get_snap item pos.y_f min_distance snap_list_y
    ; w_f = pos.x_f -. (get_snap item pos.x_f min_distance snap_list_x) +. pos.w_f
    ; h_f = pos.y_f -. (get_snap item pos.y_f min_distance snap_list_y) +. pos.h_f
    }
  | Top_right ->
    let snap_list_x = [make_line Vright] in
    let snap_list_y = [make_line Htop] in
    { x_f = pos.x_f (*get_snap item pos.x min_distance snap_list_x *)
    ; y_f = get_snap item pos.y_f min_distance snap_list_y
    ; w_f = (get_snap item pos.x_f min_distance snap_list_x) -. pos.x_f +. pos.w_f
    ; h_f = pos.y_f -. (get_snap item pos.y_f min_distance snap_list_y) +. pos.h_f
    }
  | Bottom_left ->
    let snap_list_x = [make_line Vleft] in
    let snap_list_y = [make_line Hbottom] in
    { x_f = get_snap item pos.x_f min_distance snap_list_x
    ; y_f = pos.y_f (*get_snap item pos.y min_distance snap_list_y *)
    ; w_f = pos.x_f -. (get_snap item pos.x_f min_distance snap_list_x) +. pos.w_f
    ; h_f = (get_snap item pos.y_f min_distance snap_list_y) -. pos.y_f +. pos.h_f
    }
  | Bottom_right ->
    let snap_list_x = [make_line Vright] in
    let snap_list_y = [make_line Hbottom] in
    { x_f = pos.x_f (*get_snap item pos.x min_distance snap_list_x *)
    ; y_f = pos.y_f (*get_snap item pos.y min_distance snap_list_y *)
    ; w_f = (get_snap item pos.x_f min_distance snap_list_x) -. pos.x_f +. pos.w_f
    ; h_f = (get_snap item pos.y_f min_distance snap_list_y) -. pos.y_f +. pos.h_f
    }
  | Top ->
    (* not tested *)
    let snap_list_x = [make_line Vcenter] in
    let snap_list_y = [make_line Htop] in
    { x_f = get_snap item pos.x_f min_distance snap_list_x
    ; y_f = get_snap item pos.y_f min_distance snap_list_y
    ; w_f = pos.x_f -. (get_snap item pos.x_f min_distance snap_list_x) +. pos.w_f
    ; h_f = pos.y_f -. (get_snap item pos.y_f min_distance snap_list_y) +. pos.h_f
    }
  | Bottom ->
    (* not tested *)
    let snap_list_x = [make_line Vcenter] in
    let snap_list_y = [make_line Hbottom] in
    { x_f = get_snap item pos.x_f min_distance snap_list_x
    ; y_f = get_snap item pos.y_f min_distance snap_list_y
    ; w_f = pos.x_f -. (get_snap item pos.x_f min_distance snap_list_x) +. pos.w_f
    ; h_f = pos.y_f -. (get_snap item pos.y_f min_distance snap_list_y) +. pos.h_f
    }
  | Left ->
    (* not tested *)
    let snap_list_x = [make_line Vleft] in
    let snap_list_y = [make_line Hcenter] in
    { x_f = get_snap item pos.x_f min_distance snap_list_x
    ; y_f = get_snap item pos.y_f min_distance snap_list_y
    ; w_f = pos.x_f -. (get_snap item pos.x_f min_distance snap_list_x) +. pos.w_f
    ; h_f = pos.y_f -. (get_snap item pos.y_f min_distance snap_list_y) +. pos.h_f
    }
  | Right ->
    (* not tested *)
    let snap_list_x = [make_line Vright] in
    let snap_list_y = [make_line Hcenter] in
    { x_f = get_snap item pos.x_f min_distance snap_list_x
    ; y_f = get_snap item pos.y_f min_distance snap_list_y
    ; w_f = pos.x_f -. (get_snap item pos.x_f min_distance snap_list_x) +. pos.w_f
    ; h_f = pos.y_f -. (get_snap item pos.y_f min_distance snap_list_y) +. pos.h_f
    }



(* glue lines to its item *)
let get_snap_lines
    (item : Dom_html.element Js.t)
    (pos : t_f)
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float)
    min_distance
    (action : [`Resize of resize_direction | `Move]) =
  let snap_list =
    match action with
    | `Move ->
      vlines_for_move_action item pos min_distance items (parent_w_f, parent_h_f)
      @ hlines_for_move_action item pos min_distance items (parent_w_f, parent_h_f)
    | `Resize dir ->
      vlines_for_resize_action item pos min_distance items (parent_w_f, parent_h_f) dir
      @ hlines_for_resize_action item pos min_distance items (parent_w_f, parent_h_f) dir
  in
  let rec create_lines action acc = function
    (* (inplist: (line_align_direction * int * (line_align_direction * int)) list )  (*= function *)
       = match inplist with*)
    | [] -> acc
    | (direction, aligns_count, (align_other, _)) :: tl ->
      let acc =
        if aligns_count > 0
        then
          let is_vertical = match direction with
            | Vleft | Vright | Vcenter -> true
            | Nill | Htop | Hbottom | Hcenter -> false in
          (*let is_center = match direction with
            | Vcenter | Hcenter -> true
            | _ -> false in*)
          let is_center = match align_other with
            | Vcenter | Hcenter -> true
            | _ -> false in
          let origin = match direction with
            | Vleft -> pos.x_f
            | Vcenter -> pos.x_f +. pos.w_f /. 2.0
            | Vright -> pos.x_f +. pos.w_f
            | Htop -> pos.y_f
            | Hcenter -> pos.y_f +. pos.h_f /. 2.0
            | Hbottom -> pos.y_f +. pos.h_f
            | Nill -> 0.0 in
          let line_ret =
            ({ is_vertical
            ; is_multiple = aligns_count > 1
            ; is_center
            ; origin_f = origin
            } : line_f) in
          line_ret :: acc
        else acc in
      create_lines action acc tl in
  create_lines action [] snap_list

let position_clipped_parent
    ({ w_f; h_f; x_f; y_f } as pos : t_f)
    (parent_w_f, parent_h_f : float * float)
    (min_w_f : float)
    (min_h_f : float) = function
  | `Move -> fix_xy pos
  | `Resize direction ->
    let (max_x, max_y, min_x, min_y) =
      match direction with
      | Top_left -> Some (x_f +. w_f -. min_w_f), Some (y_f +. h_f -. min_h_f), None, None
      | Top_right -> None, Some (y_f +. h_f -. min_h_f), Some x_f, None
      | Bottom_left -> Some (x_f +. w_f -. min_w_f), None, None, Some y_f
      | Bottom_right -> None, None, Some x_f, Some y_f
      (* not tested *)
      | Top -> Some (x_f +. w_f -. min_w_f), None, None, None
      (* not tested *)
      | Bottom -> None, None, None, Some y_f
      (* not tested *)
      | Left -> Some (x_f +. w_f -. min_h_f), None, None, None
      (* not tested *)
      | Right -> None, None, Some x_f, None
    in
    fix_xy ?min_x ?max_x ?min_y ?max_y
      (fix_wh ~min_w:min_w_f ~min_h:min_h_f pos)

(* alternative position_not_collide_others_glide *)
let fix_collisions
    (original_position : t)
    (position : t)
    (item : Dom_html.element Js.t)
    (items : Dom_html.element Js.t list) =
  let rec positions_at_items acc = function
    | [] -> acc
    | hd :: tl ->
      let acc =
        if Element.equal item hd
        then acc
        else (of_element hd) :: acc in
      positions_at_items acc tl in
  if has_collision position (positions_at_items [] items)
  then original_position
  else position

(* return position of item, closest and not to other
        (checks all others) at move direction (top bottom left right)
         or resize direction
       position init value = original_position
       recursion ends, if max_counter <= 0
       set max_counter as max width height parent
       _
       resize_direction used in this function NOT as resize_direction *)
let closest_non_intersecting
    (vector : resize_direction)
    (action : [`Resize of resize_direction | `Move])
    (item : Dom_html.element Js.t)
    (items : Dom_html.element Js.t list)
    (max_counter : int)
    (position : t) =
  let rec aux acc = function
    | n when n <= 0 -> position
    | n ->
      let pos = match action with
        | `Move -> (* move *)
          let x = match vector with
            | Left -> acc.x - 1
            | Right -> acc.x + 1
            | Top | Bottom | Top_left | Top_right
            | Bottom_left | Bottom_right -> acc.x in
          let y = match vector with
            | Top -> acc.y - 1
            | Bottom -> acc.y + 1
            | Left | Right | Top_left | Top_right
            | Bottom_left | Bottom_right -> acc.y in
          { acc with x; y }
        | `Resize resz ->
          (* direction to find closest position item witch other *)
          let x = match vector with
            | Left ->
              begin match resz with
                | Top_left -> acc.x - 1
                | Top_right -> acc.x
                | Bottom_left -> acc.x - 1
                | Bottom_right -> acc.x
                | Top | Bottom | Left | Right -> acc.x (* not tested *)
              end
            | _ -> acc.x in
          let y = match vector with
            | Top ->
              begin match resz with
                | Top_left -> acc.y - 1
                | Top_right -> acc.y - 1
                | Bottom_left -> acc.y
                | Bottom_right -> acc.y
                | Top | Bottom | Left | Right -> acc.y
              end (* not tested *)
            | _ -> acc.y in
          let w = match vector with
            | Right ->
              begin match resz with
                | Top_left -> acc.w
                | Top_right -> acc.w + 1
                | Bottom_left -> acc.w
                | Bottom_right -> acc.w + 1
                | Top | Bottom | Left | Right -> acc.w
              end (* not tested *)
            | _ -> acc.w in
          let h = match vector with
            | Bottom ->
              begin match resz with
                | Top_left -> acc.h
                | Top_right -> acc.h
                | Bottom_left -> acc.h + 1
                | Bottom_right -> acc.h + 1
                | Top | Bottom | Left | Right -> acc.h
              end (* not tested *)
            | _ -> acc.h in
          { x; y; w; h } in
      if equal (fix_collisions position pos item items) position
      then acc else aux pos (if equal pos position then (-1) else (pred n))
      in
  aux position max_counter

  (*
let fix_collisions_glide
    ~(origin : t)
    ({ x; y; w; h } as pos : t)
    (item : Dom_html.element Js.t)
    (items : Dom_html.element Js.t list)
    (action : [`Resize of resize_direction | `Move])
    (parent_size : int * int) =
  if equal pos origin then origin
  else
    let position = pos in
    let pos = fix_collisions origin pos item items in
    if equal pos origin
    then
       (* for four directions (to top, to bottom, to right, to left)
       find closest not clip position at item to other.
       It used for remove wrong addition distance at item to other
       if mouse big jump in other object presents -
       remove big space between item and other *)

       let closest =
        match action with
        | `Move ->
          if abs (x - origin.x) > abs (y - origin.y)
          then
            if x - origin.x < 0
            then closest_non_intersecting Left
                action item items
                (max (fst parent_size) (snd parent_size))
                origin
            else closest_non_intersecting Right
                action item items
                (max (fst parent_size) (snd parent_size))
                origin
          else
          if y - origin.y < 0
          then closest_non_intersecting Top
              action item items
              (max (fst parent_size) (snd parent_size))
              origin
          else closest_non_intersecting Bottom
              action item items
              (max (fst parent_size) (snd parent_size))
              origin
        | `Resize resz ->
          if match resz with
            | Top_left -> abs (x - origin.x) > abs (y - origin.y)
            | Top_right -> abs (w - origin.w) > abs (h - origin.h)
            | Bottom_left -> abs (x - origin.x) > abs (y - origin.y)
            | Bottom_right -> abs (w - origin.w) > abs (h - origin.h)
            | Top | Bottom | Left | Right -> true (* not tested *)
          then if match resz with
            | Top_left -> x - origin.x < 0
            | Top_right -> w - origin.w < 0
            | Bottom_left  -> x - origin.x < 0
            | Bottom_right -> w - origin.w < 0
            | Top | Bottom | Left | Right -> true (* not tested *)
            then
              closest_non_intersecting Left
                action item items
                (max (fst parent_size) (snd parent_size))
                origin
            else
              closest_non_intersecting Right
                action item items
                (max (fst parent_size) (snd parent_size))
                origin
          else if match resz with
            | Top_left -> y - origin.y < 0
            | Top_right -> y - origin.y < 0
            | Bottom_left  -> h - origin.h < 0
            | Bottom_right -> h - origin.h < 0
            | Top | Bottom | Left | Right -> true (* not tested *)
          then
            closest_non_intersecting Top
              action item items
              (max (fst parent_size) (snd parent_size))
              origin
          else
            closest_non_intersecting Bottom
              action item items
              (max (fst parent_size) (snd parent_size))
              origin
      in
      let pos_y = match action with
        | `Move -> { position with x = closest.x; w = closest.w }
        | `Resize sz -> match sz with
          | Top_left | Bottom_left -> { position with x = closest.x; w = w + x - closest.x }
          | Top_right | Bottom_right -> { position with x = closest.x; w = closest.w }
          | Top | Bottom | Left | Right -> origin (* not tested *)
      in
      let pos_x = match action with
        | `Move -> { position with y = closest.y; h = closest.h }
        | `Resize sz -> match sz with
          | Top_left | Top_right -> { position with y = closest.y; h = h + y - closest.y }
          | Bottom_left | Bottom_right -> { position with y = closest.y; h = closest.h }
          | Top | Bottom | Left | Right -> origin (* not tested *)
      in
      let pos = fix_collisions origin pos_y item items in
      if equal pos origin
      then fix_collisions origin pos_x item items
      else pos
    else pos

*)

let snap_to_grid position (grid_step : float) =
  { x_f = position.x_f -. ( fmod position.x_f grid_step )
  ; y_f = position.y_f -. ( fmod position.y_f grid_step )
  ; w_f = position.w_f -. ( fmod position.w_f grid_step )
  ; h_f = position.h_f -. ( fmod position.h_f grid_step )
  }

(*
(* FIXME remove*)
let global_saved_original_position = ref {x_f=0.0;y_f=0.0;h_f=0.0;w_f=0.0}
let global_saved_position_previous = ref {x_f=0.0;y_f=0.0;h_f=0.0;w_f=0.0}
*)

let adjust ?aspect_ratio
    ?(snap_lines = true)
    ?(collisions = false) (* need if we used collides *)
    ?(min_width = 20) (* XXX Do we need the default value at all? *) (* to float [0;1.0] *)
    ?(min_height = 20) (* to float [0;1.0] *)
    (*?(grid_step = 15)*) (* is needed if we used grid *)
    ?max_width (* not need *)
    ?max_height (* not need *)
    ~(action : [`Resize of resize_direction | `Move])
    ~(original_position : t) (* need if we use collides *) (* int coordinatrs to float [0;1.0] *)
    ~(position : t) (* int coordinatrs to float [0;1.0] *)
    ~(siblings : Dom_html.element Js.t list) (* widget positions int coordinatrs to float [0;1.0] *)
    ~(parent_size : int * int) (* need if input positions is int pixel coordinates *)
    (item : Dom_html.element Js.t) : t * line list 
    (*
    add:
    (input_container_aspect : float)    = width/height in float
    (input_table_cell_aspect : float)   = width/height in float
    (align : align)  - container align, if we use align container in cell
    *)
    =
  let min_distance = 12 in
  (* FIXME values to function declaration? *)
  let grid_step = 15 in
  let par_max_w_f = if (float_of_int (fst parent_size)) > 0.0 
    then  (float_of_int (fst parent_size))
    else 100.0
  in
  let par_max_h_f = if (float_of_int (snd parent_size)) > 0.0 
    then  (float_of_int (snd parent_size))
    else 100.0
  in
  let max_of_parent_size =  max par_max_w_f par_max_h_f in
  let min_distance_f = (float_of_int min_distance) /. max_of_parent_size in
  let grid_step_f = (float_of_int grid_step) /. max_of_parent_size in
  let min_width_f = (float_of_int min_width) /. max_of_parent_size in
  let min_height_f = (float_of_int min_height) /. max_of_parent_size in
  let position_f = (
    { x_f = (float_of_int position.x) /. par_max_w_f
    ; y_f = (float_of_int position.y) /. par_max_h_f
    ; w_f = (float_of_int position.w) /. par_max_w_f
    ; h_f = (float_of_int position.h) /. par_max_h_f
    } : t_f)
  in
  (* original_position need if used collides: *)
  let original_position_f = (
    { x_f = (float_of_int original_position.x) /. par_max_w_f
    ; y_f = (float_of_int original_position.y) /. par_max_h_f
    ; w_f = (float_of_int original_position.w) /. par_max_w_f
    ; h_f = (float_of_int original_position.h) /. par_max_h_f
    } : t_f)
  in
  (*let _ = Printf.printf "%f %f %f %f\n" position_f.x_f position_f.y_f position_f.w_f position_f.h_f in*)
  let grid_enable = false in
  let position_to_grid =
    if grid_enable
    then snap_to_grid position_f grid_step_f
    else position_f
  in
  let position_snaped = match snap_lines, action with
    | false, _ -> position_to_grid
    | true, `Move ->
      get_item_snap_position_for_move_action item position_to_grid
        min_distance_f siblings (par_max_w_f, par_max_h_f)
    | true, `Resize resz ->
      get_item_snap_position_for_resize_action item position_to_grid
        min_distance_f siblings (par_max_w_f, par_max_h_f) resz
  in
  let position_clipped_parent =
    position_clipped_parent position_snaped (par_max_w_f, par_max_h_f) min_width_f min_height_f action
  in
  (* FIXME remove *)
  (*
  let last_pos =
    if !global_saved_original_position <> original_position_f
    then begin
      global_saved_original_position:=original_position_f;
      global_saved_position_previous:=original_position_f;
      original_position_f
    end
    else !global_saved_position_previous in
    *)
  let position_not_collide_others = (* no collisions *)
    position_clipped_parent in (* pos snapped*)
  let snap_lines_f =
    if snap_lines
    then get_snap_lines item position_not_collide_others siblings (par_max_w_f, par_max_h_f) min_distance_f action
    else [] in
  (* FIXME remove *)
  (*global_saved_position_previous := position_not_collide_others;*)
  (* 
  transform to pixel, for out: 
  *)
  let out = (
    { x = (int_of_float (floor (position_not_collide_others.x_f *. par_max_w_f)))
    ; y = (int_of_float (floor (position_not_collide_others.y_f *. par_max_h_f)))
    ; w = (int_of_float (floor (position_not_collide_others.w_f *. par_max_w_f)))
    ; h = (int_of_float (floor (position_not_collide_others.h_f *. par_max_h_f)))
    } : t)
  in
  (*
  let _ = Printf.printf "in %d %d %d %d\n" position.x position.y position.w position.h in
  let _ = Printf.printf "in %f %f %f %f\n" position_f.x_f position_f.y_f position_f.w_f position_f.h_f in
  let _ = Printf.printf "out %d %d %d %d\n" out.x out.y out.w out.h in
  *)
  let rec snap_lines_t_f_to_t (acc : line list) (in_list_t: line_f list) = match in_list_t with
    | [] -> acc
    | hd :: tl ->
      let acc =  
        { is_vertical = hd.is_vertical
        ; is_multiple = hd.is_multiple
        ; is_center = hd.is_center
        ; origin =if hd.is_vertical
          then (int_of_float (floor (hd.origin_f *. par_max_w_f)))
          else (int_of_float (floor (hd.origin_f *. par_max_h_f)))
        } :: acc
        in
      snap_lines_t_f_to_t acc tl
      in
  let snap_lines = snap_lines_t_f_to_t [] snap_lines_f in      
  out, snap_lines

(* scale not need*)
let scale
    ~(original_parent_size : int * int)
    ~(parent_size : int * int)
    (position : t) : t =
(*  let pos = if (fst parent_size) <> 0 then
      { x = position.x * (fst original_parent_size) / (fst parent_size)
      ; y = position.y * (fst original_parent_size) / (fst parent_size)
      ; w = position.w * (fst original_parent_size) / (fst parent_size)
      ; h = position.h * (fst original_parent_size) / (fst parent_size)
      }
    else position
  in
  pos
*)
  position







(* 

not modified fix_functions

for function find_spare: 

*)


(** Changes top and left coordinates to correspond parent dimentions *)
let fix_xy2 ?min_x ?min_y ?max_x ?max_y par_w par_h (p : t) =
  let x = if p.x < 0 then 0 else if p.x + p.w > par_w then par_w - p.w else p.x in
  let y =
    match par_h with
    | None -> if p.y < 0 then 0 else p.y
    | Some ph -> if p.y < 0 then 0 else if p.y + p.h > ph then ph - p.h else p.y
  in
  let x = match max_x with
    | Some max -> if x > max then max else x
    | None -> x
  in
  let x = match min_x with
    | Some min -> if x > min then min else x
    | None -> x
  in
  let y = match max_y with
    | Some max -> if y > max then max else y
    | None -> y
  in
  let y = match min_y with
    | Some min -> if y > min then min else y
    | None -> y
  in
  { p with x; y }

(** Changes width to correspond provided constraints *)
let fix_w2 ?max_w ?(min_w = 1) par_w (p : t) =
  let w = match max_w with
    | Some max ->
      if p.w > max then max
      else if p.w < min_w then min_w
      else p.w
    | None -> if p.w < min_w then min_w else p.w
  in
  let w =
    if p.x + w > par_w
    then par_w - p.x
    else if p.x < 0
    then w + p.x
    else w in
  { p with w }

(** Changes height to correspond provided constraints *)
let fix_h2 ?max_h ?(min_h = 1) par_h (p : t) =
  let h = match max_h with
    | Some max -> if p.h > max then max else if p.h < min_h then min_h else p.h
    | None -> if p.h < min_h then min_h else p.h
  in
  let h = match par_h with
    | Some ph ->
      if p.y + h > ph
      then ph - p.y
      else if p.y < 0
      then h + p.y
      else h
    | None    -> h
  in
  { p with h }

(** Changes width and height to correspond provided constraints *)
let fix_wh2 ?max_w ?min_w ?max_h ?min_h par_w par_h =
  (fix_h2 ?max_h ?min_h par_h) % (fix_w2 ?max_w ?min_w par_w)

let find_spare ?(compare : (t -> t -> int) option)
    ?(aspect : (int * int) option)
    ?min_w ?min_h ?max_w ?max_h
    ~(siblings : t list)
    ~parent_size
    (x, y) =
  let (w, h) = parent_size in
  let pos = { x; y; w = 1; h = 1 } in
  if has_collision pos siblings
  then None
  else
    let area pos = pos.w * pos.h in
    let cmp =
      match compare with
      | Some f -> f
      | None ->
        (fun new_pos old_pos ->
           match aspect with
           | Some a ->
             let nasp = fix_aspect new_pos a in
             let oasp = fix_aspect old_pos a in
             let narea = area nasp in
             let oarea = area oasp in
             Stdlib.compare narea oarea
           | None ->
             let new_area = area new_pos in
             let old_area = area old_pos in
             Stdlib.compare new_area old_area) in
    (* FIXME obviously not optimized algorithm *)
    (* get only elements that are on the way to cursor proection to the left/right side *)
    let x_filtered = List.filter (fun i -> pos.y > i.y && pos.y < i.y + i.h) siblings in
    (* get cursor proection to the left side *)
    let l =
      List.filter (fun i -> i.x < pos.x) x_filtered
      |> List.fold_left (fun acc i ->
          if i.x + i.w > acc.x + acc.w
          then i else acc) empty
      |> (fun x -> x.x + x.w) in
    (* get cursor proection to the right side *)
    let r =
      List.filter (fun i -> i.x > pos.x) x_filtered
      |> List.fold_left (fun acc i ->
          if i.x < acc.x then i else acc)
        { x = w; y = 0; w = 0; h = 0 }
      |> (fun x -> x.x) in
    (* get only elements that are on the way to cursor proection to the top/bottom side *)
    let y_filtered = List.filter
        (fun i ->
           pos.x > i.x
           && pos.x < i.x + i.w
           && i.x + i.w > l
           && i.x < r)
        siblings in
    (* get cursor proection to the top side *)
    let t =
      List.filter (fun i -> i.y < pos.y) y_filtered
      |> List.fold_left (fun acc i ->
          if i.y + i.h > acc.y + acc.h
          then i else acc) empty
      |> (fun x -> x.y + x.h) in
    (* get cursor proection to the bottom side *)
    let b =
      List.filter (fun i -> i.y > pos.y) y_filtered
      |> List.fold_left (fun acc i -> if i.y < acc.y then i else acc)
        { x = 0; y = h; w = 0; h = 0}
      |> (fun x -> x.y) in
    (* get available x points *)
    (* FIXME obviously we don't need to iterate over all items *)
    let (xs : int list) =
      List.fold_left (fun acc i ->
          let join = fun x lst -> if x >= l && x <= r then x :: lst else lst in
          let acc  = join i.x acc |> join (i.x + i.w) in
          acc) [] siblings
      |> (fun x -> l :: x @ [r])
      |> List.sort_uniq (Pervasives.compare) in
    (* get available y points *)
    (* FIXME obviously we don't need to iterate over all items *)
    let (ys : int list) =
      List.fold_left (fun acc i ->
          let join = fun y lst -> if y >= t && y <= b then y :: lst else lst in
          let acc  = join i.y acc |> join (i.y + i.h) in
          acc) [] siblings
      |> (fun x -> t :: x @ [b])
      |> List.sort_uniq (Pervasives.compare) in
    (* get biggest non-overlapping rectangle under the cursor *)
    (* FIXME obviously not optimized at all *)
    let a =
      List.fold_left (fun acc x0 ->
          let xs = List.filter (fun i -> i > x0) xs in
          List.fold_left (fun acc x1 ->
              List.fold_left (fun acc y0 ->
                  let ys = List.filter (fun i -> i > y0) ys in
                  List.fold_left (fun acc y1 ->
                      let (new_pos : t) =
                        { x = x0
                        ; y = y0
                        ; w = x1 - x0
                        ; h = y1 - y0
                        } in
                      (* new rect must be the biggest one available,
                       * it must not overlap with other rects,
                       * it must be under the mouse cursor *)
                      if (not @@ has_collision new_pos siblings) && collides new_pos pos
                      then
                        let p = fix_wh2 ?max_w ?max_h ?min_h ?min_w w (Some h) new_pos in
                        let p =
                          match aspect with
                          | Some asp -> fix_aspect p asp
                          | None -> new_pos in
                        let cx = pos.x - new_pos.x in
                        let cy = pos.y - new_pos.y in
                        let cp = fix_xy2 new_pos.w (Some new_pos.h)
                            { pos with x = cx; y = cy } in
                        let p = fix_xy2 new_pos.w (Some new_pos.h)
                            { p with x = cp.x - (p.w / 2)
                                   ; y = cp.y - (p.h / 2)
                            } in
                        let p = { p with x = p.x + new_pos.x
                                       ; y = p.y + new_pos.y } in
                        match (cmp p acc),
                              p.x + p.w > w,
                              p.y + p.h > h,
                              p.w > new_pos.w,
                              p.h > new_pos.h with
                        | 1, false, false, false, false -> p
                        | _ -> acc
                      else acc) acc ys) acc ys) acc xs)
        empty xs
    in
    if equal a empty then None else Some a
