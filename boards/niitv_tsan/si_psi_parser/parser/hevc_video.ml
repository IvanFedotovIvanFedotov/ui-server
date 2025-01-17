let name = "HEVC video descriptor"

let parse bs off =
  match%bitstring bs with
  | {| profile_space        : 2
     ; tier_flag            : 1  : save_offset_to (off_1)
     ; profile_idc          : 5  : save_offset_to (off_2)
     ; pr_com_ind           : 32 : save_offset_to (off_3)
     ; prog_src_flag        : 1  : save_offset_to (off_4)
     ; interlaced_src_flag  : 1  : save_offset_to (off_5)
     ; np_const_flag        : 1  : save_offset_to (off_6)
     ; fo_const_flag        : 1  : save_offset_to (off_7)
     ; res_zero_44bits      : 44 : save_offset_to (off_8)
     ; level_idc            : 8  : save_offset_to (off_9)
     ; true                 : 1  : save_offset_to (off_10)
     ; hevc_still_flag      : 1  : save_offset_to (off_11)
     ; hevc_24_flag         : 1  : save_offset_to (off_12)
     ; sph_flag             : 1  : save_offset_to (off_13)
     ; reserved_1           : 4  : save_offset_to (off_14)
     ; temp_id_min          : 3  : save_offset_to (off_15)
     ; reserved_2           : 5  : save_offset_to (off_16)
     ; temp_id_max          : 3  : save_offset_to (off_17)
     ; reserved_3           : 5  : save_offset_to (off_18)
     |} ->
    [ Node.make ~offset:off 2 "profile_space" (Hex (Int profile_space))
    ; Node.make ~offset:(off + off_1)  1  "tier_flag" (Bits (Bool tier_flag))
    ; Node.make ~offset:(off + off_2)  5  "profile_idc" (Hex (Int profile_idc))
    ; Node.make ~offset:(off + off_3)  32 "profile_compatibility_indication" (Hex (Int32 pr_com_ind))
    ; Node.make ~offset:(off + off_4)  1  "progressive_source_flag" (Bits (Bool prog_src_flag))
    ; Node.make ~offset:(off + off_5)  1  "interlaced_source_flag" (Bits (Bool interlaced_src_flag))
    ; Node.make ~offset:(off + off_6)  1  "non_packed_constraint_flag" (Bits (Bool np_const_flag))
    ; Node.make ~offset:(off + off_7)  1  "frame_only_constraint_flag" (Bits (Bool fo_const_flag))
    ; Node.make ~offset:(off + off_8)  44 "reserved_zero_44bits" (Bits (Int64 res_zero_44bits))
    ; Node.make ~offset:(off + off_9)  8  "level_idc" (Hex (Int level_idc))
    ; Node.make ~offset:(off + off_10) 1  "temporal_layer_subset_flag" (Bits (Bool true))
    ; Node.make ~offset:(off + off_11) 1  "HEVC_still_present_flag" (Bits (Bool hevc_still_flag))
    ; Node.make ~offset:(off + off_12) 1  "HEVC_24hr_picture_present_flag" (Bits (Bool hevc_24_flag))
    ; Node.make ~offset:(off + off_13) 1 "sub_pic_hrd_params_not_present_flag" (Bits (Bool sph_flag))
    ; Node.make ~offset:(off + off_14) 4  "reserved" (Bits (Int reserved_1))
    ; Node.make ~offset:(off + off_15) 3  "temporal_id_min" (Dec (Int temp_id_min))
    ; Node.make ~offset:(off + off_16) 5  "reserved" (Bits (Int reserved_2))
    ; Node.make ~offset:(off + off_17) 3  "temporal_id_max" (Dec (Int temp_id_max))
    ; Node.make ~offset:(off + off_18) 5  "reserved" (Bits (Int reserved_3)) ]
  | {| profile_space       : 2
     ; tier_flag           : 1  : save_offset_to (off_1)
     ; profile_idc         : 5  : save_offset_to (off_2)
     ; pr_com_ind          : 32 : save_offset_to (off_3)
     ; prog_src_flag       : 1  : save_offset_to (off_4)
     ; interlaced_src_flag : 1  : save_offset_to (off_5)
     ; np_const_flag       : 1  : save_offset_to (off_6)
     ; fo_const_flag       : 1  : save_offset_to (off_7)
     ; res_zero_44bits     : 44 : save_offset_to (off_8)
     ; level_idc           : 8  : save_offset_to (off_9)
     ; false               : 1  : save_offset_to (off_10)
     ; hevc_still_flag     : 1  : save_offset_to (off_11)
     ; hevc_24_flag        : 1  : save_offset_to (off_12)
     ; sph_flag            : 1  : save_offset_to (off_13)
     ; reserved_1          : 4  : save_offset_to (off_14)
     |} ->
    [ Node.make ~offset:off 2 "profile_space" (Hex (Int profile_space))
    ; Node.make ~offset:(off + off_1)  1  "tier_flag" (Bits (Bool tier_flag))
    ; Node.make ~offset:(off + off_2)  5  "profile_idc" (Hex (Int profile_idc))
    ; Node.make ~offset:(off + off_3)  32 "profile_compatibility_indication" (Hex (Int32 pr_com_ind))
    ; Node.make ~offset:(off + off_4)  1  "progressive_source_flag" (Bits (Bool prog_src_flag))
    ; Node.make ~offset:(off + off_5)  1  "interlaced_source_flag" (Bits (Bool interlaced_src_flag))
    ; Node.make ~offset:(off + off_6)  1  "non_packed_constraint_flag" (Bits (Bool np_const_flag))
    ; Node.make ~offset:(off + off_7)  1  "frame_only_constraint_flag" (Bits (Bool fo_const_flag))
    ; Node.make ~offset:(off + off_8)  44 "reserved_zero_44bits" (Bits (Int64 res_zero_44bits))
    ; Node.make ~offset:(off + off_9)  8  "level_idc" (Hex (Int level_idc))
    ; Node.make ~offset:(off + off_10) 1  "temporal_layer_subset_flag" (Bits (Bool false))
    ; Node.make ~offset:(off + off_11) 1  "HEVC_still_present_flag" (Bits (Bool hevc_still_flag))
    ; Node.make ~offset:(off + off_12) 1  "HEVC_24hr_picture_present_flag" (Bits (Bool hevc_24_flag))
    ; Node.make ~offset:(off + off_13) 1 "sub_pic_hrd_params_not_present_flag" (Bits (Bool sph_flag))
    ; Node.make ~offset:(off + off_14) 4  "reserved" (Bits (Int reserved_1))
    ]
