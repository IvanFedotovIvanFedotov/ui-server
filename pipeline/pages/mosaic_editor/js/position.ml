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
  ; origin : float
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
  { x : float
  ; y : float
  ; w : float
  ; h : float
  }

let empty =
  { x = 0.0
  ; y = 0.0
  ; w = 0.0
  ; h = 0.0
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
  Printf.sprintf "x=%f, y=%f, w=%f, h=%f"
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
  fabs( a.x -. b.x ) < 0.00001 &&
  fabs( a.y -. b.y ) < 0.00001 &&
  fabs( a.w -. b.w ) < 0.00001 &&
  fabs( a.h -. b.h ) < 0.00001

(** Checks if two elements collide, returns [true] if so and [false] otherwise *)
let collides (pos1 : t) (pos2 : t) =
  if (pos1.x +. pos1.w <= pos2.x) then false
  else if (pos1.x >= pos2.x +. pos2.w) then false
  else if (pos1.y +. pos1.h <= pos2.y) then false
  else if (pos1.y >= pos2.y +. pos2.h) then false
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
let fix_xy ?min_x ?min_y ?max_x ?max_y par_w par_h (p : t) =
  let x = if p.x < 0.0 then 0.0 else if p.x +. p.w > par_w then par_w -. p.w else p.x in
  let y = if p.y < 0.0 then 0.0 else if p.y +. p.h > par_h then par_h -. p.h else p.y
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
  { p with x = x; y = y }

(** Changes width to correspond provided constraints *)
let fix_w ?max_w ?(min_w = 0.0) (p : t) =
  let w = match max_w with
    | Some max ->
      if p.w > max then max
      else if p.w < min_w then min_w
      else p.w
    | None -> if p.w < min_w then min_w else p.w
  in
  let w = match max_w with
    | Some max ->
      if p.x +. w > max
      then max -. p.x
      else if p.x < 0.0
      then w +. p.x
      else w 
    | None -> w
  in
  { p with w = w }

(** Changes height to correspond provided constraints *)
let fix_h ?max_h ?(min_h = 0.0) (p : t) =
  let h = match max_h with
    | Some max -> if p.h > max then max else if p.h < min_h then min_h else p.h
    | None -> if p.h < min_h then min_h else p.h
  in
  let h = match max_h with
    | Some max -> 
      if p.y +. h > max
      then max -. p.y
      else if p.y < 0.0
      then h +. p.y
      else h
    | None -> h
  in  
  { p with h = h }

(** Changes width and height to correspond provided constraints *)
let fix_wh ?max_w ?min_w ?max_h ?min_h =
  (fix_h ?max_h ?min_h) % (fix_w ?max_w ?min_w)

(** Changes width and height to correspond provided aspect **)
let fix_aspect (p : t) (aspect : int * int) =
  let w =
    let w = int_of_float p.w in
    if w mod (fst aspect) <> 0 then
      let w = w / (fst aspect) * (fst aspect) in
      if w = 0 then (fst aspect) else w
    else w
  in
  let h =
    let h = int_of_float p.h in
    if h mod (snd aspect) <> 0 then
      let h = (h / (snd aspect)) * (snd aspect) in
      if h = 0 then (snd aspect) else h
    else h
  in
  let sw = w / (fst aspect) in
  let sh = h / (snd aspect) in
  let w, h =
    if sw > sh
    then (fst aspect) * sh, h
    else w, (snd aspect) * sw
  in
  { p with w = float_of_int w; h = float_of_int h }

let apply_to_element (pos : t) (elt : #Dom_html.element Js.t) =
  elt##.style##.width := Utils.px_js (int_of_float pos.w);
  elt##.style##.left := Utils.px_js (int_of_float pos.x);
  elt##.style##.height := Utils.px_js (int_of_float pos.h);
  elt##.style##.top := Utils.px_js (int_of_float pos.y)

let of_element (elt : #Dom_html.element Js.t) =
  { x = float_of_int( elt##.offsetLeft ) 
  ; y = float_of_int( elt##.offsetTop ) 
  ; w = float_of_int( elt##.offsetWidth ) 
  ; h = float_of_int( elt##.offsetHeight ) 
  }

let to_client_rect (t : t) : Dom_html.clientRect Js.t =
  object%js
    val top = t.y
    val left = t.x
    val right = t.x +. t.w
    val bottom = t.y +. t.h
    val width = Js.def @@ t.w
    val height = Js.def @@ t.h
  end

let of_client_rect (rect : Dom_html.clientRect Js.t) : t =
  { x = rect##.left
  ; y = rect##.top
  ; w = Js.Optdef.get rect##.width (fun () -> 0.)
  ; h = Js.Optdef.get rect##.height (fun () -> 0.)
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
    (pos : t)
    (items : Dom_html.element Js.t list)
    min_distance
    line_align_val =
  let rec count_aligns line_align_val (distance: (line_align_direction * float)) = function
    | [] -> distance
    | hd :: tl ->
      let icompare = 
        { x = (of_element hd ).x
        ; y = (of_element hd ).y
        ; w = (of_element hd ).w
        ; h = (of_element hd ).h
        }
      in
      let (distance : line_align_direction * float) =
        if Element.equal item hd
        then count_aligns line_align_val distance tl (*distance*) else
          match line_align_val with
          | Htop ->
            let dist1 = pos.y -. icompare.y in
            let dist2 = pos.y -. icompare.y -. icompare.h /. 2.0 in
            let dist3 = pos.y -. icompare.y -. icompare.h in
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
            let dist1 = pos.y +. pos.h /. 2.0 -. icompare.y in
            let dist2 = pos.y +. pos.h /. 2.0 -. icompare.y -. icompare.h /. 2.0 in
            let dist3 = pos.y +. pos.h /. 2.0 -. icompare.y -. icompare.h in
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
            let dist1 = pos.y +. pos.h -. icompare.y in
            let dist2 = pos.y +. pos.h -. icompare.y -. icompare.h /. 2.0 in
            let dist3 = pos.y +. pos.h -. icompare.y -. icompare.h in
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
            let dist1 = pos.x -. icompare.x in
            let dist2 = pos.x -. icompare.x -. icompare.w /. 2.0 in
            let dist3 = pos.x -. icompare.x -. icompare.w in
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
            let dist1 = pos.x +. pos.w /. 2.0 -. icompare.x in
            let dist2 = pos.x +. pos.w /. 2.0 -. icompare.x -. icompare.w /. 2.0 in
            let dist3 = pos.x +. pos.w /. 2.0 -. icompare.x -. icompare.w in
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
            let dist1 = pos.x +. pos.w -. icompare.x in
            let dist2 = pos.x +. pos.w -. icompare.x -. icompare.w /. 2.0 in
            let dist3 = pos.x +. pos.w -. icompare.x -. icompare.w in
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
    (pos : t)
    (items : Dom_html.element Js.t list)
    (min_distance : float)
    line_align_val =
  let rec aux line_align_val counts = function
    | [] -> counts
    | hd :: tl ->
      let icompare = 
        { x = (of_element hd).x 
        ; y = (of_element hd).y 
        ; w = (of_element hd).w 
        ; h = (of_element hd).h 
        }
      in
      if Element.equal item hd
      then aux line_align_val counts tl (*counts*)
      else
        let counts =
          if (line_align_val = Htop
              && (fabs (pos.y -. icompare.y) < min_distance
                  || fabs (pos.y -. icompare.y -. icompare.h /. 2.0) < min_distance
                  || fabs (pos.y -. icompare.y -. icompare.h) < min_distance))
          || (line_align_val = Hcenter
              && (fabs (pos.y +. pos.h /. 2.0 -. icompare.y) < min_distance
                  || fabs (pos.y +. pos.h /. 2.0 -. icompare.y -. icompare.h /. 2.0) < min_distance
                  || fabs (pos.y +. pos.h /. 2.0 -. icompare.y -. icompare.h) < min_distance))
          || (line_align_val = Hbottom
              && (fabs (pos.y +. pos.h -. icompare.y) < min_distance
                  || fabs (pos.y +. pos.h -. icompare.y -. icompare.h /. 2.0) < min_distance
                  || fabs (pos.y +. pos.h -. icompare.y -. icompare.h) < min_distance))
          || (line_align_val = Vleft
              && (fabs (pos.x -. icompare.x) < min_distance
                  || fabs (pos.x -. icompare.x -. icompare.w /. 2.0) < min_distance
                  || fabs (pos.x -. icompare.x -. icompare.w) < min_distance))
          || (line_align_val = Vcenter
              && (fabs (pos.x +. pos.w /. 2.0 -. icompare.x) < min_distance
                  || fabs (pos.x +. pos.w /. 2.0 -. icompare.x -. icompare.w /. 2.0) < min_distance
                  || fabs (pos.x +. pos.w /. 2.0 -. icompare.x -. icompare.w) < min_distance))
          || (line_align_val = Vright
              && (fabs (pos.x +. pos.w -. icompare.x) < min_distance
                  || fabs (pos.x +. pos.w -. icompare.x -. icompare.w /. 2.0) < min_distance
                  || fabs (pos.x +. pos.w -. icompare.x -. icompare.w) < min_distance))
          then succ counts
          else counts
        in
        aux line_align_val counts tl
  in
  aux line_align_val 0 items

let make_line_properties align item pos min_distance items =
  align,
  line_align_count item pos items min_distance align,
  line_find_closest_align item pos items min_distance align

(* return: direction, count aligns (0 = none align lines),
   closest line distance (if distance > min_distance = no find lines) *)
let hlines_for_move_action (item : Dom_html.element Js.t)
    pos 
    min_distance
    (items : Dom_html.element Js.t list) =
  [ make_line_properties Htop item pos min_distance items 
  ; make_line_properties Hcenter item pos min_distance items 
  ; make_line_properties Hbottom item pos min_distance items 
  ]

let hlines_for_resize_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (direction : resize_direction) =
  let align_direction = match direction with
    | Top_left | Top_right | Top -> Htop
    | Bottom_left | Bottom_right | Bottom -> Hbottom
    | Left | Right -> Hcenter in
  [make_line_properties align_direction item pos min_distance items]

let vlines_for_move_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list) =
  [ make_line_properties Vleft item pos min_distance items
  ; make_line_properties Vcenter item pos min_distance items
  ; make_line_properties Vright item pos min_distance items
  ]

let vlines_for_resize_action (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list)
    (direction : resize_direction) =
  let align_direction = match direction with
    | Top_left | Bottom_left | Left -> Vleft
    | Top_right | Bottom_right | Right -> Vright
    | Top | Bottom -> Vcenter in
  [make_line_properties align_direction item pos min_distance items]

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
    (items : Dom_html.element Js.t list) =
  get_snap item pos.y min_distance
  @@ hlines_for_move_action item pos min_distance items

let get_item_snap_x (item : Dom_html.element Js.t)
    pos min_distance
    (items : Dom_html.element Js.t list) =
  get_snap item pos.x min_distance
  @@ vlines_for_move_action item pos min_distance items

let get_item_snap_position_for_move_action
    (item : Dom_html.element Js.t)
    pos
    min_distance
    (items : Dom_html.element Js.t list)
    (parent_w_f, parent_h_f : float * float) =
  { x = get_item_snap_x item pos min_distance items
  ; y = get_item_snap_y item pos min_distance items
  ; w = pos.w
  ; h = pos.h
  }

let get_item_snap_position_for_resize_action
    (item : Dom_html.element Js.t)
    pos
    min_distance
    (items : Dom_html.element Js.t list)
    (direction : resize_direction) =
  let make_line align = make_line_properties align item pos min_distance items in
  match direction with
  | Top_left ->
    let snap_list_x = [make_line Vleft] in
    let snap_list_y = [make_line Htop] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x -. (get_snap item pos.x min_distance snap_list_x) +. pos.w
    ; h = pos.y -. (get_snap item pos.y min_distance snap_list_y) +. pos.h
    }
  | Top_right ->
    let snap_list_x = [make_line Vright] in
    let snap_list_y = [make_line Htop] in
    { x = pos.x (*get_snap item pos.x min_distance snap_list_x *)
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = (get_snap item pos.x min_distance snap_list_x) -. pos.x +. pos.w
    ; h = pos.y -. (get_snap item pos.y min_distance snap_list_y) +. pos.h
    }
  | Bottom_left ->
    let snap_list_x = [make_line Vleft] in
    let snap_list_y = [make_line Hbottom] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = pos.y (*get_snap item pos.y min_distance snap_list_y *)
    ; w = pos.x -. (get_snap item pos.x min_distance snap_list_x) +. pos.w
    ; h = (get_snap item pos.y min_distance snap_list_y) -. pos.y +. pos.h
    }
  | Bottom_right ->
    let snap_list_x = [make_line Vright] in
    let snap_list_y = [make_line Hbottom] in
    { x = pos.x (*get_snap item pos.x min_distance snap_list_x *)
    ; y = pos.y (*get_snap item pos.y min_distance snap_list_y *)
    ; w = (get_snap item pos.x min_distance snap_list_x) -. pos.x +. pos.w
    ; h = (get_snap item pos.y min_distance snap_list_y) -. pos.y +. pos.h
    }
  | Top ->
    (* not tested *)
    let snap_list_x = [make_line Vcenter] in
    let snap_list_y = [make_line Htop] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x -. (get_snap item pos.x min_distance snap_list_x) +. pos.w
    ; h = pos.y -. (get_snap item pos.y min_distance snap_list_y) +. pos.h
    }
  | Bottom ->
    (* not tested *)
    let snap_list_x = [make_line Vcenter] in
    let snap_list_y = [make_line Hbottom] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x -. (get_snap item pos.x min_distance snap_list_x) +. pos.w
    ; h = pos.y -. (get_snap item pos.y min_distance snap_list_y) +. pos.h
    }
  | Left ->
    (* not tested *)
    let snap_list_x = [make_line Vleft] in
    let snap_list_y = [make_line Hcenter] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x -. (get_snap item pos.x min_distance snap_list_x) +. pos.w
    ; h = pos.y -. (get_snap item pos.y min_distance snap_list_y) +. pos.h
    }
  | Right ->
    (* not tested *)
    let snap_list_x = [make_line Vright] in
    let snap_list_y = [make_line Hcenter] in
    { x = get_snap item pos.x min_distance snap_list_x
    ; y = get_snap item pos.y min_distance snap_list_y
    ; w = pos.x -. (get_snap item pos.x min_distance snap_list_x) +. pos.w
    ; h = pos.y -. (get_snap item pos.y min_distance snap_list_y) +. pos.h
    }



(* glue lines to its item *)
let get_snap_lines
    (item : Dom_html.element Js.t)
    (pos : t)
    (items : Dom_html.element Js.t list)
    min_distance
    (action : [`Resize of resize_direction | `Move]) =
  let snap_list =
    match action with
    | `Move ->
      vlines_for_move_action item pos min_distance items
      @ hlines_for_move_action item pos min_distance items
    | `Resize dir ->
      vlines_for_resize_action item pos min_distance items dir
      @ hlines_for_resize_action item pos min_distance items dir
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
            | Vleft -> pos.x
            | Vcenter -> pos.x +. pos.w /. 2.0
            | Vright -> pos.x +. pos.w
            | Htop -> pos.y
            | Hcenter -> pos.y +. pos.h /. 2.0
            | Hbottom -> pos.y +. pos.h
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
    ({ w; h; x; y } as pos : t)
    (parent_w_f, parent_h_f : float * float)
    (min_w_f : float)
    (min_h_f : float) = function
  | `Move -> fix_xy parent_w_f parent_h_f pos
  | `Resize direction ->
    let (max_x, max_y, min_x, min_y) =
      match direction with
      | Top_left -> Some (x +. w -. min_w_f), Some (y +. h -. min_h_f), None, None
      | Top_right -> None, Some (y +. h -. min_h_f), Some x, None
      | Bottom_left -> Some (x +. w -. min_w_f), None, None, Some y
      | Bottom_right -> None, None, Some x, Some y
      (* not tested *)
      | Top -> Some (x +. w -. min_w_f), None, None, None
      (* not tested *)
      | Bottom -> None, None, None, Some y
      (* not tested *)
      | Left -> Some (x +. w -. min_h_f), None, None, None
      (* not tested *)
      | Right -> None, None, Some x, None
    in
    fix_xy ?min_x ?max_x ?min_y ?max_y parent_w_f parent_h_f 
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

let snap_to_grid position (grid_step : float) =
  { x = position.x -. ( fmod position.x grid_step )
  ; y = position.y -. ( fmod position.y grid_step )
  ; w = position.w -. ( fmod position.w grid_step )
  ; h = position.h -. ( fmod position.h grid_step )
  }

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
    ~(parent_size : float * float) (* need if input positions is int pixel coordinates *)
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
  let par_max_w_f = if (fst parent_size) > 0.0 
    then  (fst parent_size)
    else 100.0
  in
  let par_max_h_f = if (snd parent_size) > 0.0 
    then  (snd parent_size)
    else 100.0
  in
  let max_of_parent_size =  max par_max_w_f par_max_h_f in
  let min_distance_f = (float_of_int min_distance) /. max_of_parent_size in
  let grid_step_f = (float_of_int grid_step) /. max_of_parent_size in
  let min_width_f = (float_of_int min_width) /. max_of_parent_size in
  let min_height_f = (float_of_int min_height) /. max_of_parent_size in
  let position_f = (
    { x = position.x
    ; y = position.y
    ; w = position.w
    ; h = position.h
    } : t)
  in
  (*let _ = Printf.printf "%f %f %f %f\n" position_f.x position_f.y position_f.w position_f.h in*)
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
        min_distance_f siblings resz
  in
  let position_clipped_parent =
    position_clipped_parent position_snaped (par_max_w_f, par_max_h_f) min_width_f min_height_f action
  in
  let position_not_collide_others = (* no collisions *)
    position_clipped_parent in (* pos snapped*)
  let snap_lines_f =
    if snap_lines
    then get_snap_lines item position_not_collide_others siblings min_distance_f action
    else [] in
  let out = (
    { x = position_not_collide_others.x
    ; y = position_not_collide_others.y
    ; w = position_not_collide_others.w
    ; h = position_not_collide_others.h
    } : t)
  in
  (*
  let _ = Printf.printf "in %d %d %d %d\n" position.x position.y position.w position.h in
  let _ = Printf.printf "in %f %f %f %f\n" position_f.x position_f.y position_f.w position_f.h in
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
            then (floor (hd.origin_f *. par_max_w_f))
            else (floor (hd.origin_f *. par_max_h_f))
        } :: acc
      in
      snap_lines_t_f_to_t acc tl
  in
  let snap_lines = snap_lines_t_f_to_t [] snap_lines_f in      
  out, snap_lines

  let to_wm_position (t : t) : Pipeline_types.Wm.position =
    { left = int_of_float t.x
    ; top = int_of_float t.y
    ; right = int_of_float @@ t.x +. t.w
    ; bottom = int_of_float @@ t.y +. t.h
    }
  
  let of_wm_position (t : Pipeline_types.Wm.position) : t =
    { x = float_of_int t.left
    ; y = float_of_int t.top
    ; w = float_of_int (t.right - t.left)
    ; h = float_of_int (t.bottom - t.top)
    }
  
  let to_relative ~(parent_size : float * float) (pos : t) =
    let parent_width, parent_height = parent_size in
    let w, h =
      if parent_width > pos.w
      then (pos.w /. parent_width,
            pos.h /. parent_height)
      else (parent_width /. pos.w,
            parent_height /. pos.h) in
    let x = (pos.x *. w) /. pos.w in
    let y = (pos.y *. h) /. pos.h in
    { x; y; w; h }
  
  let of_relative ~(parent_size : float * float) (pos : t) =
    let w = pos.w *. (fst parent_size) in
    let h = pos.h *. (snd parent_size) in
    let x = pos.x *. w /. pos.w in
    let y = pos.y *. h /. pos.h in
    { x; y; w; h }  
