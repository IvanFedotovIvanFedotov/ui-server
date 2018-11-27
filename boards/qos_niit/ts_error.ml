open Board_types
open Containers
open Common

type table_info =
  { section : int
  ; id : int
  ; id_ext : int
  }
let table_info_of_ts_error (e : 'a Error.error) =
  { section = Int32.to_int @@ Int32.((e.param_2 land 0xFF00_0000l) lsr 24)
  ; id = Int32.to_int @@ Int32.((e.param_2 land 0x00FF_0000l) lsr 16)
  ; id_ext = Int32.to_int @@ Int32.(e.param_2 land 0x0000_FFFFl)
  }

module Kind = struct

  type t = string * string

  let of_error (e : 'a Error.error) : t =
    match e.err_code with
    | 0x11 -> "1.1", "TS sync loss"
    | 0x12 -> "1.2", "Sync byte error"
    | 0x13 -> "1.3", "PAT error"
    | 0x14 -> "1.4", "Continuity count error"
    | 0x15 -> "1.5", "PMT error"
    | 0x16 -> "1.6", "PID error"
    | 0x21 -> "2.1", "Transport error"
    | 0x22 -> "2.2", "CRC error"
    | 0x23 -> "2.3", "PCR error"
    | 0x24 -> "2.4", "PCR accuracy error"
    | 0x25 -> "2.5", "PTS error"
    | 0x26 -> "2.6", "CAT error"
    | 0x31 -> "3.1", "NIT error"
    | 0x32 -> "3.2", "SI repetition error"
    | 0x34 -> "3.4", "Unreferenced PID"
    | 0x35 -> "3.5", "SDT error"
    | 0x36 -> "3.6", "EIT error"
    | 0x37 -> "3.7", "RST error"
    | 0x38 -> "3.8", "TDT error"
    | _ -> "", ""

end

module Info = struct

  type interval =
    [ `S of float
    | `Ms of float
    | `Us of float
    | `Ns of float
    | `Unk
    ] [@@deriving show]

  type possible_pids =
    [ `One of int
    | `List of int list
    | `Range of int * int
    | `Unknown
    ]

  type t = string

  let possible_si_pids : Mpeg_ts.table -> possible_pids = function
    | `PAT -> `One 0x00
    | `CAT -> `One 0x01
    | `PMT -> `One 0x02
    | `NIT _ -> `List [0x40;0x41;0x72]
    | `SDT _ -> `List [0x42;0x46;0x4A;0x72]
    | `EIT _ -> `Range (0x4E,0x6F)
    | `RST -> `List [0x71;0x72]
    | `TDT -> `List [0x70;0x72;0x73]
    | _ -> `Unknown

  let table_id_to_interval : int -> interval = function
    | x when x >= 0x00 && x <= 0x02 -> `S 0.5
    | 0x42 | 0x4E -> `S 2.0
    | 0x40 | 0x41 | 0x46 | 0x4A | 0x4F -> `S 10.0
    | x when x >= 0x50 && x <= 0x70 -> `S 30.0
    | 0x73 -> `S 30.0
    | _ -> `Unk

  (* FIXME extend basic table type and write its own converter *)
  let table_name = Mpeg_ts.table_to_string ~simple:true

  let period_to_unit_name : interval -> string = function
    | `S _ -> "с"
    | `Ms _ -> "мс"
    | `Us _ -> "мкс"
    | `Ns _ -> "нс"
    | `Unk -> ""

  let integrate_interval (x : float) : interval -> interval = function
    | `S _ -> `S x
    | `Ms _ ->
       let div = x /. 1e3 in
       if div >. 1. then `S div else `Ms x
    | `Us _ ->
       let s = x /. 1e6 in
       if s >. 1. then `S s
       else
         let ms = x /. 1e3 in
         if ms >. 1. then `Ms ms
         else `Us x
    | `Ns _ ->
       let s = x /. 1e9 in
       if s >. 1. then `S s
       else
         let ms = x /. 1e6 in
         if ms >. 1. then `Ms ms
         else
           let us = x /. 1e3 in
           if us >. 1. then `Us us
           else `Ns x
    | `Unk -> `Unk

  let period_to_float = function
    | `S x | `Ms x | `Us x  | `Ns x -> x
    | _ -> 0.0

  let crc_err ?(hex = false) ?table (e : 'a Error.error) =
    let base = "Ошибка CRC" in
    let prefix = match table with
      | None -> base
      | Some t -> Printf.sprintf "%s в %s" base @@ table_name t in
    let to_s = Printf.sprintf (if hex then "0x%08lX" else "%lu") in
    let computed = to_s e.param_1 in
    let actual = to_s e.param_2 in
    Printf.sprintf "%s, CRC = %s, должно быть %s" prefix computed actual

  let interval_err ?(coef = 0.1) ~prefix ~cmp_word ~period (e : 'a Error.error) =
    let got = match e.param_1 with
      | -1l -> None
      | x -> Int32.to_float x *. coef
             |> (fun x -> integrate_interval x period)
             |> Option.return in
    let must = match period with
      | `Unk -> None
      | x ->
         let s = Printf.sprintf "%s %g %s"
                   cmp_word (period_to_float x) (period_to_unit_name x) in
         Some s in
    match got, must with
    | None, None -> prefix
    | Some got, None ->
       Printf.sprintf "%s %g %s" prefix
         (period_to_float got) (period_to_unit_name got)
    | None, Some ext -> Printf.sprintf "%s %s" prefix ext
    | Some got, Some ext ->
       Printf.sprintf "%s %g %s (%s)" prefix
         (period_to_float got)
         (period_to_unit_name got)
         ext

  let table_short_interval ~period (e : 'a Error.error) table =
    let prefix = Printf.sprintf "Период следования таблицы %s -"
                 @@ table_name table in
    interval_err ~prefix ~cmp_word:"менее" ~period e

  let table_long_interval ~period (e : 'a Error.error) table =
    let prefix = Printf.sprintf "Таблица %s отсутствует в потоке"
                 @@ table_name table in
    interval_err ~prefix ~cmp_word:"более" ~period e

  let table_scrambled table =
    Printf.sprintf "Таблица %s скремблирована" @@ table_name table

  let table_crc ?hex e table = crc_err ?hex ~table e

  let table_id ?(short = false) ?(hex = false) (e : 'a Error.error) table =
    let to_s = if hex then Printf.sprintf "0x%02X"
               else Printf.sprintf "%u" in
    let got = to_s @@ Int32.to_int e.param_1 in
    let ppids = possible_si_pids table in
    let possible = match ppids with
      | `One x -> to_s x
      | `List l -> List.map (fun x -> to_s x) l |> String.concat ", "
      | `Range (f,t) -> Printf.sprintf "%s .. %s" (to_s f) (to_s t)
      | `Unknown -> ""
    in
    match ppids with
    | `Unknown ->
       Printf.sprintf "Поле table_id = %s" got
    | _ ->
       if short then Printf.sprintf "Поле table_id не равно %s" possible
       else Printf.sprintf "Поле table_id = %s, должно быть %s" got possible

  let table_ext_unknown _ = ""

  let of_error ?(hex = true) (e : 'a Error.error) =
    match e.err_code with
    (* First priority *)
    | 0x11 -> "Пропадание синхронизации"
    | 0x12 -> "Байт синхронизации не равен 0x47"
    | 0x13 ->
       let f = match e.err_ext with
         | 0x01 -> table_scrambled
         | 0x02 -> table_id ~hex e
         | 0x03 -> table_long_interval ~period:(`S 0.5) e
         | 0x05 -> table_crc ~hex e
         | _    -> table_ext_unknown
       in f `PAT
    | 0x14 ->
       begin match e.err_ext with
       | 0x01 -> "Повторение пакета более двух раз"
       | 0x02 -> "Потеря пакета (пакетов)"
       | 0x03 -> "Неправильный порядок пакетов"
       | 0x04 -> "Счётчик изменился при отсутствии полезной нагрузки"
       | _    -> ""
       end
    | 0x15 ->
       let f = match e.err_ext with
         | 0x01 -> table_scrambled
         | 0x02 -> table_id ~hex e
         | 0x03 -> table_long_interval ~period:(`S 0.5) e
         | 0x05 -> table_crc ~hex e
         | 0x06 -> fun _ -> "Разный номер программы в PAT и PMT"
         | _    -> table_ext_unknown
       in f `PMT
    | 0x16 -> "В потоке отсутствует PID"
    (* Second priority *)
    | 0x21 -> "В пакете установлен флаг transport_error_indicator"
    | 0x22 -> crc_err ~hex e
    | 0x23 ->
       begin match e.err_ext with
       | 0x01 -> let prefix = "Период повторения PCR" in
                 let period = `Ms 40. in
                 interval_err ~prefix ~cmp_word:"больше" ~period e
       | 0x02 -> interval_err ~prefix:"Разрыв PCR"
                   ~cmp_word:"больше"
                   ~period:(`Ms 100.) e
       | 0x03 -> Printf.sprintf "Отсутствует PCR для программы %04u"
                 @@ Int32.to_int e.param_1
       | _ -> ""
       end
    | 0x24 ->
       interval_err ~coef:1.0 ~prefix:"Неравномерность PCR"
         ~cmp_word:"больше"
         ~period:(`Ns 500.) e
    | 0x25 ->
       interval_err ~prefix:"Период повторения PTS"
         ~cmp_word:"больше"
         ~period:(`Ms 700.) e
    | 0x26 ->
       let f = match e.err_ext with
         | 0x01 -> table_scrambled
         | 0x02 -> table_id ~hex e
         | 0x03 -> table_crc ~hex e
         | 0x04 -> fun _ -> "Есть скремблирование, но нет CAT"
         | _ -> table_ext_unknown
       in f `CAT
    (* Third priority *)
    | 0x31 ->
       let f = match e.err_ext with
         | 0x01 -> table_id ~hex e
         | 0x02 -> table_long_interval ~period:(`S 10.) e
         | 0x04 -> table_scrambled
         | _    -> table_ext_unknown
       in f (`NIT `Actual)
    | 0x32 ->
       let { id; _ } = table_info_of_ts_error e in
       let f = match e.err_ext with
         | 0x01 -> table_short_interval ~period:(`Ms 25.) e
         | 0x02 -> table_long_interval ~period:(table_id_to_interval id) e
         | _ -> table_ext_unknown
       in f @@ Mpeg_ts.table_of_int id
    | 0x34 -> "Пакет с неизвестным PID"
    | 0x35 ->
       let f = match e.err_ext with
         | 0x01 -> table_id ~hex e
         | 0x02 -> table_long_interval ~period:(`S 2.) e
         | 0x04 -> table_scrambled
         | _    -> table_ext_unknown
       in f (`SDT `Actual)
    | 0x36 ->
       let f = match e.err_ext with
         | 0x01 -> table_id ~hex e
         | 0x02 -> table_long_interval ~period:(`S 2.) e
         | 0x03 -> table_scrambled
         | _    -> table_ext_unknown
       in f (`EIT (`Actual, `Present))
    | 0x37 ->
       let f = match e.err_ext with
         | 0x01 -> table_id ~hex e
         | 0x02 -> table_scrambled
         | _    -> table_ext_unknown
       in f `RST
    | 0x38 ->
       let f = match e.err_ext with
         | 0x01 -> table_id ~hex e
         | 0x02 -> table_long_interval ~period:(`S 30.) e
         | 0x03 -> table_scrambled
         | _    -> table_ext_unknown
       in f `TDT
    | _    -> assert false


end

let priority_name (x : int) =
  Printf.sprintf "%u приоритет" x
