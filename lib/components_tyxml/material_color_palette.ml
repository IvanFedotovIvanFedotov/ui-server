open Color

type clr =
  | C50
  | C100
  | C200
  | C300
  | C400
  | C500
  | C600
  | C700
  | C800
  | C900

type a_clr =
  | C50
  | C100
  | C200
  | C300
  | C400
  | C500
  | C600
  | C700
  | C800
  | C900
  | A100
  | A200
  | A400
  | A700

type t =
  | Red of a_clr
  | Pink of a_clr
  | Purple of a_clr
  | Deep_purple of a_clr
  | Indigo of a_clr
  | Blue of a_clr
  | Light_blue  of a_clr
  | Cyan of a_clr
  | Teal of a_clr
  | Green of a_clr
  | Light_green of a_clr
  | Lime of a_clr
  | Yellow of a_clr
  | Amber of a_clr
  | Orange of a_clr
  | Deep_orange of a_clr
  | Brown of clr
  | Grey of clr
  | Blue_grey of clr

let make = function
  | Red x ->
    (match x with
     | C50  -> of_rgb 0xff 0xeb 0xee
     | C100 -> of_rgb 0xff 0xcd 0xd2
     | C200 -> of_rgb 0xef 0x9a 0x9a
     | C300 -> of_rgb 0xe5 0x73 0x73
     | C400 -> of_rgb 0xef 0x53 0x50
     | C500 -> of_rgb 0xf4 0x43 0x36
     | C600 -> of_rgb 0xe5 0x39 0x35
     | C700 -> of_rgb 0xd3 0x2f 0x2f
     | C800 -> of_rgb 0xc6 0x28 0x28
     | C900 -> of_rgb 0xb7 0x1c 0x1c
     | A100 -> of_rgb 0xff 0x8a 0x80
     | A200 -> of_rgb 0xff 0x52 0x52
     | A400 -> of_rgb 0xff 0x17 0x44
     | A700 -> of_rgb 0xd5 0x00 0x00)
  | Pink x ->
    (match x with
     | C50  -> of_rgb 0xfc 0xe4 0xec
     | C100 -> of_rgb 0xf8 0xbb 0xd0
     | C200 -> of_rgb 0xf4 0x8f 0xb1
     | C300 -> of_rgb 0xf0 0x62 0x92
     | C400 -> of_rgb 0xec 0x40 0x7a
     | C500 -> of_rgb 0xe9 0x1e 0x63
     | C600 -> of_rgb 0xd8 0x1b 0x60
     | C700 -> of_rgb 0xc3 0x18 0x5b
     | C800 -> of_rgb 0xad 0x14 0x57
     | C900 -> of_rgb 0x88 0x0e 0x4f
     | A100 -> of_rgb 0xff 0x80 0xab
     | A200 -> of_rgb 0xff 0x40 0x81
     | A400 -> of_rgb 0xf5 0x00 0x57
     | A700 -> of_rgb 0xc5 0x11 0x62)
  | Purple x ->
    (match x with
     | C50  -> of_rgb 0xf3 0xe5 0xf5
     | C100 -> of_rgb 0xe1 0xbe 0xe7
     | C200 -> of_rgb 0xce 0x93 0xd8
     | C300 -> of_rgb 0xba 0x68 0xc8
     | C400 -> of_rgb 0xab 0x47 0xbc
     | C500 -> of_rgb 0x9c 0x27 0xb0
     | C600 -> of_rgb 0x8e 0x24 0xaa
     | C700 -> of_rgb 0x7b 0x1f 0xa2
     | C800 -> of_rgb 0x6a 0x1b 0x9a
     | C900 -> of_rgb 0x4a 0x14 0x8c
     | A100 -> of_rgb 0xea 0x80 0xfc
     | A200 -> of_rgb 0xe0 0x40 0xfb
     | A400 -> of_rgb 0xd5 0x00 0xf9
     | A700 -> of_rgb 0xaa 0x00 0xff)
  | Deep_purple x ->
    (match x with
     | C50  -> of_rgb 0xed 0xe7 0xf6
     | C100 -> of_rgb 0xd1 0xc4 0xe9
     | C200 -> of_rgb 0xb3 0x9d 0xdb
     | C300 -> of_rgb 0x95 0x75 0xcd
     | C400 -> of_rgb 0x7e 0x57 0xc2
     | C500 -> of_rgb 0x67 0x3a 0xb7
     | C600 -> of_rgb 0x5e 0x35 0xb1
     | C700 -> of_rgb 0x51 0x2d 0xa8
     | C800 -> of_rgb 0x45 0x27 0xa0
     | C900 -> of_rgb 0x31 0x1b 0x92
     | A100 -> of_rgb 0xb3 0x88 0xff
     | A200 -> of_rgb 0x7c 0x4d 0xff
     | A400 -> of_rgb 0x65 0x1f 0xff
     | A700 -> of_rgb 0x62 0x00 0xea)
  | Indigo x ->
    (match x with
     | C50  -> of_rgb 0xe8 0xea 0xf6
     | C100 -> of_rgb 0xc5 0xca 0xe9
     | C200 -> of_rgb 0x9f 0xa8 0xda
     | C300 -> of_rgb 0x79 0x86 0xcb
     | C400 -> of_rgb 0x5c 0x6b 0xc0
     | C500 -> of_rgb 0x3f 0x51 0xb5
     | C600 -> of_rgb 0x39 0x49 0xab
     | C700 -> of_rgb 0x30 0x3f 0x9f
     | C800 -> of_rgb 0x28 0x35 0x93
     | C900 -> of_rgb 0x1a 0x23 0x7e
     | A100 -> of_rgb 0x8c 0x9e 0xff
     | A200 -> of_rgb 0x53 0x6d 0xfe
     | A400 -> of_rgb 0x3d 0x5a 0xfe
     | A700 -> of_rgb 0x30 0x4f 0xfe)
  | Blue x ->
    (match x with
     | C50  -> of_rgb 0xe3 0xf2 0xfd
     | C100 -> of_rgb 0xbb 0xde 0xfb
     | C200 -> of_rgb 0x90 0xca 0xf9
     | C300 -> of_rgb 0x64 0xb5 0xf6
     | C400 -> of_rgb 0x42 0xa5 0xf5
     | C500 -> of_rgb 0x21 0x96 0xf3
     | C600 -> of_rgb 0x1e 0x88 0xe5
     | C700 -> of_rgb 0x19 0x76 0xd2
     | C800 -> of_rgb 0x15 0x65 0xc0
     | C900 -> of_rgb 0x0d 0x47 0xa1
     | A100 -> of_rgb 0x82 0xb1 0xff
     | A200 -> of_rgb 0x44 0x8a 0xff
     | A400 -> of_rgb 0x29 0x79 0xff
     | A700 -> of_rgb 0x29 0x62 0xff)
  | Light_blue x ->
    (match x with
     | C50  -> of_rgb 0xe1 0xf5 0xfe
     | C100 -> of_rgb 0xb3 0xe5 0xfc
     | C200 -> of_rgb 0x81 0xd4 0xfa
     | C300 -> of_rgb 0x4f 0xc3 0xf7
     | C400 -> of_rgb 0x29 0xb6 0xf6
     | C500 -> of_rgb 0x03 0xa9 0xf4
     | C600 -> of_rgb 0x03 0x9b 0xe5
     | C700 -> of_rgb 0x02 0x88 0xd1
     | C800 -> of_rgb 0x02 0x77 0xbd
     | C900 -> of_rgb 0x01 0x57 0x9b
     | A100 -> of_rgb 0x80 0xd8 0xff
     | A200 -> of_rgb 0x40 0xc4 0xff
     | A400 -> of_rgb 0x00 0xb0 0xff
     | A700 -> of_rgb 0x00 0x91 0xea)
  | Cyan x ->
    (match x with
     | C50  -> of_rgb 0xe0 0xf7 0xfa
     | C100 -> of_rgb 0xb2 0xeb 0xf2
     | C200 -> of_rgb 0x80 0xde 0xea
     | C300 -> of_rgb 0x4d 0xd0 0xe1
     | C400 -> of_rgb 0x26 0xc6 0xda
     | C500 -> of_rgb 0x00 0xbc 0xd4
     | C600 -> of_rgb 0x00 0xac 0xc1
     | C700 -> of_rgb 0x00 0x97 0xa7
     | C800 -> of_rgb 0x00 0x83 0x8f
     | C900 -> of_rgb 0x00 0x60 0x64
     | A100 -> of_rgb 0x84 0xff 0xff
     | A200 -> of_rgb 0x18 0xff 0xff
     | A400 -> of_rgb 0x00 0xe5 0xff
     | A700 -> of_rgb 0x00 0xb8 0xd4)
  | Teal x ->
    (match x with
     | C50  -> of_rgb 0xe0 0xf2 0xf1
     | C100 -> of_rgb 0xb2 0xdf 0xdb
     | C200 -> of_rgb 0x80 0xcb 0xc4
     | C300 -> of_rgb 0x4d 0xb6 0xac
     | C400 -> of_rgb 0x26 0xa6 0x9a
     | C500 -> of_rgb 0x00 0x96 0x88
     | C600 -> of_rgb 0x00 0x89 0x7b
     | C700 -> of_rgb 0x00 0x79 0x6b
     | C800 -> of_rgb 0x00 0x69 0x5c
     | C900 -> of_rgb 0x00 0x4d 0x40
     | A100 -> of_rgb 0xa7 0xff 0xeb
     | A200 -> of_rgb 0x64 0xff 0xda
     | A400 -> of_rgb 0x1d 0xe9 0xb6
     | A700 -> of_rgb 0x00 0xbf 0xa5)
  | Green x ->
    (match x with
     | C50  -> of_rgb 0xe8 0xf5 0xe9
     | C100 -> of_rgb 0xc8 0xe6 0xc9
     | C200 -> of_rgb 0xa5 0xd6 0xa7
     | C300 -> of_rgb 0x81 0xc7 0x84
     | C400 -> of_rgb 0x66 0xbb 0x6a
     | C500 -> of_rgb 0x4c 0xaf 0x50
     | C600 -> of_rgb 0x43 0xa0 0x47
     | C700 -> of_rgb 0x38 0x8e 0x3c
     | C800 -> of_rgb 0x2e 0x7d 0x32
     | C900 -> of_rgb 0x1b 0x5e 0x20
     | A100 -> of_rgb 0xb9 0xf6 0xca
     | A200 -> of_rgb 0x69 0xf0 0xae
     | A400 -> of_rgb 0x00 0xe6 0x76
     | A700 -> of_rgb 0x00 0xc8 0x53)
  | Light_green x ->
    (match x with
     | C50  -> of_rgb 0xf1 0xf8 0xe9
     | C100 -> of_rgb 0xdc 0xed 0xc8
     | C200 -> of_rgb 0xc5 0xd1 0xa5
     | C300 -> of_rgb 0xae 0xd5 0x81
     | C400 -> of_rgb 0x9c 0xcc 0x65
     | C500 -> of_rgb 0x8b 0xc3 0x4a
     | C600 -> of_rgb 0x7c 0xb3 0x42
     | C700 -> of_rgb 0x68 0x9f 0x38
     | C800 -> of_rgb 0x55 0x8b 0x2f
     | C900 -> of_rgb 0x33 0x69 0x1e
     | A100 -> of_rgb 0xb9 0xf6 0xca
     | A200 -> of_rgb 0xb2 0xff 0x59
     | A400 -> of_rgb 0x76 0xff 0x03
     | A700 -> of_rgb 0x64 0xdd 0x17)
  | Lime x ->
    (match x with
     | C50  -> of_rgb 0xf9 0xfb 0xe7
     | C100 -> of_rgb 0xf0 0xf4 0xc3
     | C200 -> of_rgb 0xe6 0xee 0x9c
     | C300 -> of_rgb 0xdc 0xe7 0x75
     | C400 -> of_rgb 0xd4 0xe1 0x57
     | C500 -> of_rgb 0xcd 0xdc 0x39
     | C600 -> of_rgb 0xc0 0xca 0x33
     | C700 -> of_rgb 0xaf 0xb4 0x2b
     | C800 -> of_rgb 0x9e 0x9d 0x24
     | C900 -> of_rgb 0x82 0x77 0x17
     | A100 -> of_rgb 0xf4 0xff 0x81
     | A200 -> of_rgb 0xee 0xff 0x41
     | A400 -> of_rgb 0xc6 0xff 0x00
     | A700 -> of_rgb 0xae 0xea 0x00)
  | Yellow x ->
    (match x with
     | C50  -> of_rgb 0xff 0xfd 0xe7
     | C100 -> of_rgb 0xff 0xf9 0xc4
     | C200 -> of_rgb 0xff 0xf5 0x9d
     | C300 -> of_rgb 0xff 0xf1 0x76
     | C400 -> of_rgb 0xff 0xee 0x58
     | C500 -> of_rgb 0xff 0xeb 0x3b
     | C600 -> of_rgb 0xfd 0xd8 0x35
     | C700 -> of_rgb 0xfb 0xc0 0x2d
     | C800 -> of_rgb 0xf9 0xa8 0x25
     | C900 -> of_rgb 0xf5 0x7f 0x17
     | A100 -> of_rgb 0xff 0xff 0x8d
     | A200 -> of_rgb 0xff 0xff 0x00
     | A400 -> of_rgb 0xff 0xea 0x00
     | A700 -> of_rgb 0xff 0xd6 0x00)
  | Amber x ->
    (match x with
     | C50  -> of_rgb 0xff 0xf8 0xe1
     | C100 -> of_rgb 0xff 0xec 0xb3
     | C200 -> of_rgb 0xff 0xe0 0x82
     | C300 -> of_rgb 0xff 0xd5 0x4f
     | C400 -> of_rgb 0xff 0xca 0x28
     | C500 -> of_rgb 0xff 0xc1 0x07
     | C600 -> of_rgb 0xff 0xb3 0x00
     | C700 -> of_rgb 0xff 0xa0 0x00
     | C800 -> of_rgb 0xff 0x8f 0x00
     | C900 -> of_rgb 0xff 0x6f 0x00
     | A100 -> of_rgb 0xff 0xe5 0x7f
     | A200 -> of_rgb 0xff 0xd7 0x40
     | A400 -> of_rgb 0xff 0xc4 0x00
     | A700 -> of_rgb 0xff 0xab 0x00)
  | Orange x ->
    (match x with
     | C50  -> of_rgb 0xff 0xf3 0xe0
     | C100 -> of_rgb 0xff 0xe0 0xb2
     | C200 -> of_rgb 0xff 0xcc 0x80
     | C300 -> of_rgb 0xff 0xb7 0x4d
     | C400 -> of_rgb 0xff 0xa7 0x26
     | C500 -> of_rgb 0xff 0x98 0x00
     | C600 -> of_rgb 0xfb 0x8c 0x00
     | C700 -> of_rgb 0xf5 0x7c 0x00
     | C800 -> of_rgb 0xef 0x6c 0x00
     | C900 -> of_rgb 0xe6 0x51 0x00
     | A100 -> of_rgb 0xff 0xd1 0x80
     | A200 -> of_rgb 0xff 0xab 0x40
     | A400 -> of_rgb 0xff 0x91 0x00
     | A700 -> of_rgb 0xff 0x6d 0x00)
  | Deep_orange x ->
    (match x with
     | C50  -> of_rgb 0xfb 0xe9 0xe7
     | C100 -> of_rgb 0xff 0xcc 0xbc
     | C200 -> of_rgb 0xff 0xab 0x91
     | C300 -> of_rgb 0xff 0x8a 0x65
     | C400 -> of_rgb 0xff 0x70 0x43
     | C500 -> of_rgb 0xff 0x57 0x22
     | C600 -> of_rgb 0xf4 0x51 0x1e
     | C700 -> of_rgb 0xe6 0x4a 0x19
     | C800 -> of_rgb 0xd8 0x34 0x15
     | C900 -> of_rgb 0xbf 0x36 0x0c
     | A100 -> of_rgb 0xff 0x9e 0x80
     | A200 -> of_rgb 0xff 0x6e 0x40
     | A400 -> of_rgb 0xff 0x3d 0x00
     | A700 -> of_rgb 0xdd 0x2c 0x00)
  | Grey x ->
    (match x with
     | C50  -> of_rgb 0xfa 0xfa 0xfa
     | C100 -> of_rgb 0xf5 0xf5 0xf5
     | C200 -> of_rgb 0xee 0xee 0xee
     | C300 -> of_rgb 0xe0 0xe0 0xe0
     | C400 -> of_rgb 0xbd 0xbd 0xbd
     | C500 -> of_rgb 0x9e 0x9e 0x9e
     | C600 -> of_rgb 0x75 0x75 0x75
     | C700 -> of_rgb 0x61 0x61 0x61
     | C800 -> of_rgb 0x42 0x42 0x42
     | C900 -> of_rgb 0x21 0x21 0x21)
  | Blue_grey x ->
    (match x with
     | C50  -> of_rgb 0xec 0xef 0xf1
     | C100 -> of_rgb 0xcf 0xd8 0xdc
     | C200 -> of_rgb 0xb0 0xbe 0xc5
     | C300 -> of_rgb 0x90 0xa4 0xae
     | C400 -> of_rgb 0x78 0x90 0x9c
     | C500 -> of_rgb 0x60 0x7d 0x8b
     | C600 -> of_rgb 0x54 0x6e 0x7a
     | C700 -> of_rgb 0x45 0x5a 0x64
     | C800 -> of_rgb 0x37 0x47 0x4f
     | C900 -> of_rgb 0x26 0x32 0x38)
  | Brown x ->
    (match x with
     | C50  -> of_rgb 0xef 0xeb 0xe9
     | C100 -> of_rgb 0xd7 0xcc 0xc8
     | C200 -> of_rgb 0xbc 0xaa 0xa4
     | C300 -> of_rgb 0xa1 0x88 0x7f
     | C400 -> of_rgb 0x8d 0x6e 0x63
     | C500 -> of_rgb 0x79 0x55 0x48
     | C600 -> of_rgb 0x6d 0x4c 0x41
     | C700 -> of_rgb 0x5d 0x40 0x37
     | C800 -> of_rgb 0x4e 0x34 0x2e
     | C900 -> of_rgb 0x3e 0x27 0x23)
