open Board_niitv_tsan_types

let parse_l1_pre = function%bitstring
  | {| typ : 8
     ; bwt_ext : 1
     ; s1 : 3
     ; s2 : 4
     ; l1_repetition_flag : 1
     ; guard_interval : 3
     ; papr : 4
     ; l1_mod : 4
     ; l1_cod : 2
     ; l1_fec_type : 2
     ; l1_post_size : 18
     ; l1_post_info_size : 18
     ; pilot_pattern : 4
     ; tx_id_availability : 8
     ; cell_id : 16
     ; network_id : 16
     ; t2_system_id : 16
     ; num_t2_frames : 8
     ; num_data_symbols : 12
     ; regen_flag : 3
     ; l1_post_extension : 1
     ; num_rf : 3
     ; current_rf_idx : 3
     ; t2_version : 4
     ; l1_post_scrambled : 1
     ; t2_base_lite : 1
     ; reserved : 4
     |} -> { T2mi_info.
             typ
           ; preamble = s1
           ; fft = s2 lsr 1
           ; mixed_flag = s2 land 1 <> 0
           ; bwt_ext
           ; s1
           ; s2
           ; l1_repetition_flag
           ; guard_interval
           ; papr
           ; l1_mod
           ; l1_cod
           ; l1_fec_type
           ; l1_post_size
           ; l1_post_info_size
           ; pilot_pattern
           ; tx_id_availability
           ; cell_id
           ; network_id
           ; t2_system_id
           ; num_t2_frames
           ; num_data_symbols
           ; regen_flag
           ; l1_post_extension
           ; num_rf
           ; current_rf_idx
           ; t2_version
           ; l1_post_scrambled
           ; t2_base_lite
           ; reserved
           }

let parse_l1_post_conf_rf (bs : Bitstring.t) =
  let rec f = fun acc x ->
    if Bitstring.bitstring_length x = 0 then List.rev acc
    else
      let rf = match%bitstring x with
        | {| rf_idx : 3
           ; frequency : 32 : map (fun x -> Int32.to_int x)
           |} -> { T2mi_info. rf_idx; frequency } in
      f (rf :: acc) (Bitstring.dropbits 35 x) in
  f [] bs

let parse_l1_post_conf_fef = function%bitstring
  | {| fef_type : 4
     ; fef_length : 22
     ; fef_interval : 8 |} ->
    { T2mi_info. fef_type; fef_length; fef_interval }

let parse_l1_post_conf_plp (bs : Bitstring.t) =
  let rec f = fun acc x ->
    if Bitstring.bitstring_length x = 0 then List.rev acc
    else
      let plp = match%bitstring x with
        | {| plp_id : 8
           ; plp_type : 3
           ; plp_payload_type : 5
           ; ff_flag : 1
           ; first_rf_idx : 3
           ; first_frame_idx : 8
           ; plp_group_id : 8
           ; plp_cod : 3
           ; plp_mod : 3
           ; plp_rotation : 1
           ; plp_fec_type : 2
           ; plp_num_blocks_max : 10
           ; frame_interval : 8
           ; time_il_length : 8
           ; time_il_type : 1
           ; in_band_a_flag : 1
           ; in_band_b_flag : 1
           ; reserved_1 : 11
           ; plp_mode : 2
           ; static_flag : 1
           ; static_padding_flag : 1
           |} -> { T2mi_info.
                   plp_id
                 ; plp_type
                 ; plp_payload_type
                 ; ff_flag
                 ; first_rf_idx
                 ; first_frame_idx
                 ; plp_group_id
                 ; plp_cod
                 ; plp_mod
                 ; plp_rotation
                 ; plp_fec_type
                 ; plp_num_blocks_max
                 ; frame_interval
                 ; time_il_length
                 ; time_il_type
                 ; in_band_a_flag
                 ; in_band_b_flag
                 ; reserved_1
                 ; plp_mode
                 ; static_flag
                 ; static_padding_flag
                 } in
      f (plp :: acc) (Bitstring.dropbits 89 x) in
  f [] bs

let parse_l1_post_conf_aux (bs : Bitstring.t) =
  let rec f = fun acc x ->
    if Bitstring.bitstring_length x = 0 then List.rev acc
    else
      let aux = match%bitstring x with
        | {| aux_stream_type : 4
           ; aux_private_conf : 28
           |} -> { T2mi_info. aux_stream_type; aux_private_conf } in
      f (aux :: acc) (Bitstring.dropbits 32 x) in
  f [] bs

let parse_l1_post_conf ~num_rf ~s2 = function%bitstring
  | {| sub_slices_per_frame : 15
     ; num_plp : 8
     ; num_aux : 4
     ; aux_config_rfu : 8
     ; rf : (num_rf * 35) : bitstring
     ; fef : if s2 land 1 <> 0 then 34 else 0 : bitstring
     ; plp : num_plp * 89 : bitstring
     ; fef_length_msb : 2
     ; reserved_2 : 30
     ; aux : num_aux * 32 : bitstring
     |} ->
    let fef =
      if Bitstring.bitstring_length fef = 0
      then None else Some (parse_l1_post_conf_fef fef) in
    { T2mi_info.
      sub_slices_per_frame
    ; aux_config_rfu
    ; fef_length_msb
    ; reserved_2
    ; fef
    ; rf = parse_l1_post_conf_rf rf
    ; plp = parse_l1_post_conf_plp plp
    ; aux = parse_l1_post_conf_aux aux
    }
