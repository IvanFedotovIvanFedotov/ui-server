open Pipeline_types

type halign = Left | HCenter | Right
type valign = Top | VCenter | Bottom
type align = halign * valign

let widget_min_size 
    ?(min_width) 
    ?(min_height)    
    (widget : Pipeline_types.Wm.widget) =
  let (x, y, w, h) = match widget.position with 
    | None -> (0, 0, 0, 0)
    | Some v -> (v.left, v.top, v.right - v.left, v.bottom - v.top) 
  in
  let (min_w, min_h) = (
    (match min_width with | None -> 0 | Some w -> w),
    (match min_height with | None -> 0 | Some h -> h)) 
  in
  let (out_w, out_h) = match widget.aspect with
    | None -> (min_w, min_h)
    | Some asp -> asp
  in 
  let (out_w, out_h) = if out_w < min_w 
    then (min_w, out_h * out_w / min_w)
    else if out_h < min_h
      then (out_w * out_h / min_h, min_h)
      else (out_w, out_h)
  in
  if min_w <= 0 || min_h <= 0 || w <= 0 || h <= 0 
  then (0, 0, 0, 0)
  else (x * out_w / w, y * out_h / h, out_w, out_h)

(* return scale. 1.0 = no scale *)
let widget_min_scale 
    ?(min_width) 
    ?(min_height)    
    (widget : Pipeline_types.Wm.widget) =
  let (x_min, y_min, w_min, h_min) = widget_min_size ?min_width ?min_height widget in
  let (x, y, w, h) = match widget.position with 
    | None -> (0, 0, 0, 0)
    | Some v -> (v.left, v.top, v.right - v.left, v.bottom - v.top) 
  in 
  let w_scale = if w_min <= 0
    then 0.
    else (float_of_int w) /. (float_of_int w_min)
  in
  let h_scale = if h_min <= 0
    then 0.
    else (float_of_int h) /. (float_of_int h_min)
  in
  if w_scale > h_scale
  then w_scale
  else h_scale

let min_size 
    ?(min_width) 
    ?(min_height)
    (container : Wm.container) =
  let rec find_max_scale_in_min_scales scale = function
  | [] -> scale
  | hd :: tl ->
    let (_, widget) = hd in
    let scale_widget = widget_min_scale ?min_width ?min_height widget in
    if scale > scale_widget 
    then find_max_scale_in_min_scales scale tl
    else find_max_scale_in_min_scales scale_widget tl
  in
  let min_scale = find_max_scale_in_min_scales 0. container.widgets in
  let (x, y, w, h) =   
    (container.position.left, 
     container.position.top, 
     container.position.right - container.position.left,
     container.position.bottom - container.position.top) 
  in
  let (ox, oy, ow, oh) = 
    ((float_of_int x) *. min_scale,
     (float_of_int y) *. min_scale, 
     (float_of_int w) *. min_scale, 
     (float_of_int h) *. min_scale) 
  in
  (int_of_float ox, int_of_float oy, int_of_float ow, int_of_float oh)
  (*failwith "TODO"*)

let scale ?(min_width) 
    ?(min_height)
    (container : Wm.container)
    (width, height)
    (align : align) = (* work on align in process ... *) 
    let (_, _, w, h) = min_size container ?min_width ?min_height in
    if width = 0 || height = 0 
      then (w, h)
      else if width < w && height > h 
      then (w, height * w / width )
      else if width > w && height < h 
      then (width * h / height, h)
      else (width, height)
      (*failwith "TODO"*)
