        processor 18f4620

        include "p18f4620.inc"

	config	XINST = ON

CAML_STOP_AND_COPY_GC
STACK_SIZE  EQU   0x1
HEAP_SIZE   EQU   0x7

#define caml_useprim_caml_create_string
#define caml_useprim_caml_pic_clear_bit
#define caml_useprim_caml_pic_write_reg
#define caml_useprim_caml_pic_set_bit
#define caml_useprim_caml_pic_read_reg
#define caml_useprim_caml_pic_tris_of_port
#define caml_useprim_caml_ml_string_length
#define caml_useprim_caml_string_get
#define caml_useprim_caml_sleep_millis

        include "/usr/local/lib/ocapic/interp.asm"
        include "/usr/local/lib/ocapic/runtime.asm"

        org     0x1600
caml_externals:
        goto    caml_create_string
        goto    caml_pic_clear_bit
        goto    caml_pic_write_reg
        goto    caml_pic_set_bit
        goto    caml_pic_read_reg
        goto    caml_pic_tris_of_port
        goto    caml_ml_string_length
        goto    caml_string_get
        goto    caml_sleep_millis

caml_bytecode:
        db      0x54, 0x04, 0x17, 0x29, 0x2a, 0x01, 0x01, 0x87
        db      0x15, 0x00, 0x34, 0x16, 0x00, 0x55, 0x3e, 0x16
        db      0x67, 0x61, 0x00, 0x0c, 0x6e, 0x0b, 0x1a, 0x53
        db      0x28, 0x04, 0x67, 0x61, 0x00, 0x6c, 0x15, 0x00
        db      0x0d, 0x72, 0x6e, 0x0b, 0x1a, 0x53, 0x67, 0x15
        db      0x00, 0x0c, 0x71, 0x0b, 0x7f, 0xfe, 0xff, 0x32
        db      0x26, 0x08, 0x00, 0x87, 0x01, 0x00, 0x66, 0x16
        db      0x00, 0x86, 0x15, 0x00, 0x66, 0x16, 0x64, 0x54
        db      0xa7, 0x16, 0x00, 0x88, 0xed, 0xff, 0x76, 0x16
        db      0x00, 0x86, 0xc9, 0x00, 0x76, 0x16, 0x65, 0x54
        db      0xa7, 0x16, 0x00, 0x88, 0x39, 0xff, 0x86, 0x16
        db      0x00, 0x86, 0xd1, 0x07, 0x86, 0x16, 0x66, 0x54
        db      0xa7, 0x16, 0x00, 0x88, 0x31, 0xf8, 0x98, 0x16
        db      0x00, 0x86, 0x21, 0x4e, 0x98, 0x16, 0x67, 0x09
        db      0x00, 0x54, 0xa7, 0x16, 0x00, 0x88, 0xe1, 0xb1
        db      0xa4, 0x16, 0x67, 0x0b, 0x00, 0x54, 0xa7, 0x16
        db      0x67, 0x0d, 0x00, 0x0a, 0x5d, 0x00, 0x0a, 0x2c
        db      0x01, 0x01, 0x28, 0x16, 0x03, 0x87, 0x01, 0x00
        db      0xc0, 0x16, 0x03, 0x0d, 0x7f, 0xfe, 0xff, 0x0c
        db      0x22, 0x54, 0x01, 0x17, 0x35, 0x00, 0x00, 0x0e
        db      0x79, 0x56, 0xf3, 0x16, 0x67, 0x5b, 0x00, 0x68
        db      0x0d, 0x53, 0x67, 0x63, 0x00, 0x69, 0x0d, 0x53
        db      0x67, 0x6d, 0x00, 0x6a, 0x0d, 0x53, 0x67, 0x67
        db      0x00, 0x6b, 0x0d, 0x53, 0x67, 0x71, 0x00, 0x6c
        db      0x09, 0x00, 0x0d, 0x53, 0x67, 0x69, 0x00, 0x6c
        db      0x0b, 0x00, 0x0d, 0x53, 0x54, 0x01, 0x17, 0x67
        db      0x5b, 0x00, 0x68, 0x0d, 0x53, 0x03, 0x6d, 0x0d
        db      0x7f, 0xfe, 0xff, 0x0c, 0x22, 0x01, 0x28, 0x08
        db      0x67, 0x01, 0x80, 0x39, 0x00, 0x00, 0x2b, 0x01
        db      0x56, 0x16, 0x39, 0x04, 0x00, 0x54, 0x9a, 0x1b
        db      0x00, 0x36, 0x02, 0x00, 0x21, 0x0a, 0x47, 0x12
        db      0x0b, 0x47, 0x11, 0x0c, 0x47, 0x10, 0x0d, 0x47
        db      0x0f, 0x0e, 0x47, 0x0e, 0x0f, 0x47, 0x0d, 0x10
        db      0x47, 0x0c, 0x11, 0x47, 0x0b, 0x12, 0x10, 0x00
        db      0x47, 0x0a, 0x12, 0x12, 0x00, 0x47, 0x09, 0x12
        db      0x14, 0x00, 0x47, 0x08, 0x12, 0x16, 0x00, 0x47
        db      0x07, 0x12, 0x18, 0x00, 0x47, 0x06, 0x3e, 0x0d
        db      0x00, 0x28, 0x04, 0x19, 0x21, 0x43, 0x56, 0x72
        db      0x17, 0x19, 0x21, 0x46, 0x5d, 0x02, 0x63, 0x1e
        db      0x22, 0x5e, 0x04, 0x00, 0x1e, 0x21, 0x47, 0x04
        db      0x5e, 0x04, 0x19, 0x21, 0x44, 0x5d, 0x06, 0x19
        db      0x21, 0x44, 0x5d, 0x02, 0x28, 0x02, 0x19, 0x21
        db      0x46, 0x5d, 0x02, 0x67, 0x1f, 0x00, 0x1e, 0x22
        db      0x5d, 0x08, 0x73, 0x1e, 0x22, 0x5e, 0x04, 0x67
        db      0x1f, 0x00, 0x1e, 0x21, 0x47, 0x04, 0x5d, 0x08
        db      0x73, 0x0a, 0x6c, 0xe1, 0x01, 0x0d, 0x73, 0x74
        db      0x1e, 0x21, 0x47, 0x04, 0x5e, 0x04, 0x19, 0x21
        db      0x44, 0x5d, 0x06, 0x19, 0x21, 0x44, 0x5d, 0x02
        db      0x00, 0x6c, 0xe1, 0x01, 0x6c, 0x09, 0x00, 0x0e
        db      0x76, 0x73, 0x74, 0x1e, 0x21, 0x47, 0x04, 0x5e
        db      0x04, 0x19, 0x21, 0x44, 0x5d, 0x06, 0x19, 0x21
        db      0x44, 0x5d, 0x02, 0x28, 0x04, 0x19, 0x1f, 0x43
        db      0x56, 0xe6, 0x17, 0x67, 0xff, 0x01, 0x1e, 0x20
        db      0x5e, 0x04, 0x19, 0x1f, 0x46, 0x5d, 0x06, 0x19
        db      0x1f, 0x44, 0x5d, 0x06, 0x19, 0x1f, 0x47, 0x04
        db      0x5d, 0x08, 0x1e, 0x1f, 0x44, 0x5d, 0x02, 0x00
        db      0x28, 0x04, 0x67, 0xe1, 0x01, 0x1e, 0x20, 0x5d
        db      0x08, 0x74, 0x1e, 0x20, 0x5e, 0x04, 0x19, 0x1f
        db      0x46, 0x5d, 0x06, 0x19, 0x1f, 0x44, 0x5d, 0x06
        db      0x67, 0xe1, 0x01, 0x1e, 0x1f, 0x47, 0x04, 0x5d
        db      0x08, 0x73, 0x1e, 0x1f, 0x44, 0x5d, 0x02, 0x19
        db      0x1f, 0x44, 0x5d, 0x06, 0x00, 0x6c, 0x09, 0x00
        db      0x1e, 0x1f, 0x47, 0x04, 0x5d, 0x08, 0x77, 0x74
        db      0x1e, 0x1f, 0x44, 0x5d, 0x02, 0x00, 0x28, 0x06
        db      0x19, 0x1d, 0x45, 0x5d, 0x06, 0x00, 0x34, 0xfe
        db      0x25, 0x04, 0x19, 0x1b, 0x45, 0x5d, 0x02, 0x00
        db      0x34, 0xfd, 0x25, 0x04, 0x19, 0x19, 0x43, 0x56
        db      0x48, 0x18, 0x67, 0xff, 0x01, 0x1e, 0x1a, 0x5e
        db      0x04, 0x54, 0x54, 0x18, 0x67, 0xe1, 0x01, 0x1e
        db      0x1a, 0x5d, 0x08, 0x74, 0x1e, 0x1a, 0x5e, 0x04
        db      0x19, 0x19, 0x45, 0x5d, 0x02, 0x19, 0x19, 0x46
        db      0x5d, 0x06, 0x19, 0x19, 0x44, 0x5d, 0x06, 0x54
        db      0x67, 0x18, 0x63, 0x67, 0x01, 0x01, 0x1e, 0x19
        db      0x47, 0x04, 0x5d, 0x08, 0x73, 0x55, 0x66, 0x18
        db      0x63, 0x19, 0x19, 0x44, 0x5d, 0x02, 0x19, 0x19
        db      0x43, 0x68, 0x79, 0x56, 0x8c, 0x18, 0x19, 0x19
        db      0x44, 0x5d, 0x06, 0x19, 0x19, 0x44, 0x5d, 0x02
        db      0x28, 0x02, 0x63, 0x31, 0x21, 0x64, 0x34, 0xfe
        db      0x25, 0x04, 0x63, 0x34, 0xfe, 0x21, 0x65, 0x34
        db      0xfd, 0x25, 0x04, 0x19, 0x13, 0x43, 0x56, 0xc6
        db      0x18, 0x19, 0x13, 0x44, 0x5d, 0x0a, 0x5d, 0x02
        db      0x19, 0x13, 0x45, 0x5d, 0x0a, 0x5d, 0x02, 0x19
        db      0x13, 0x46, 0x5d, 0x0a, 0x5d, 0x02, 0x63, 0x34
        db      0xfd, 0x21, 0x63, 0x34, 0xfe, 0x21, 0x63, 0x31
        db      0x25, 0x04, 0x19, 0x13, 0x44, 0x5d, 0x0a, 0x5d
        db      0x02, 0x19, 0x13, 0x45, 0x5d, 0x0a, 0x5d, 0x02
        db      0x19, 0x13, 0x46, 0x5d, 0x0a, 0x5d, 0x02, 0x63
        db      0x34, 0xfd, 0x21, 0x19, 0x13, 0x45, 0x5d, 0x02
        db      0x19, 0x13, 0x46, 0x5d, 0x02, 0x67, 0x1f, 0x00
        db      0x1e, 0x14, 0x5d, 0x08, 0x73, 0x1e, 0x14, 0x5e
        db      0x04, 0x67, 0x1f, 0x00, 0x1e, 0x13, 0x47, 0x04
        db      0x5d, 0x08, 0x73, 0x6c, 0x41, 0x00, 0x74, 0x1e
        db      0x13, 0x47, 0x04, 0x5e, 0x04, 0x19, 0x13, 0x44
        db      0x5d, 0x06, 0x19, 0x13, 0x44, 0x5d, 0x02, 0x63
        db      0x34, 0xfe, 0x21, 0x63, 0x31, 0x25, 0x04, 0x29
        db      0x2a, 0x05, 0x00, 0x56, 0x27, 0x19, 0x00, 0x43
        db      0x54, 0x28, 0x19, 0x64, 0x0c, 0x56, 0x2e, 0x19
        db      0x02, 0x43, 0x0e, 0x56, 0x34, 0x19, 0x04, 0x43
        db      0x10, 0x56, 0x3d, 0x19, 0x06, 0x43, 0x54, 0x3e
        db      0x19, 0x64, 0x12, 0x10, 0x00, 0x56, 0x48, 0x19
        db      0x08, 0x10, 0x00, 0x43, 0x68, 0x34, 0xfc, 0x21
        db      0x04, 0x57, 0x04, 0x57, 0x19, 0x5d, 0x19, 0x63
        db      0x19, 0x69, 0x19, 0x67, 0x09, 0x00, 0x54, 0x6c
        db      0x19, 0x67, 0x0d, 0x00, 0x54, 0x6c, 0x19, 0x67
        db      0x0f, 0x00, 0x54, 0x6c, 0x19, 0x67, 0x0b, 0x00
        db      0x34, 0xfb, 0x21, 0x63, 0x34, 0xfc, 0x21, 0x02
        db      0x57, 0x03, 0x7c, 0x19, 0x82, 0x19, 0x88, 0x19
        db      0x67, 0x11, 0x00, 0x54, 0x8b, 0x19, 0x67, 0x15
        db      0x00, 0x54, 0x8b, 0x19, 0x67, 0x17, 0x00, 0x0e
        db      0x55, 0x95, 0x19, 0x67, 0x19, 0x00, 0x54, 0x98
        db      0x19, 0x67, 0x11, 0x00, 0x74, 0x34, 0xfb, 0x21
        db      0x63, 0x34, 0xfc, 0x21, 0x00, 0x84, 0x03, 0x00
        db      0xac, 0x19, 0x67, 0x49, 0x00, 0x54, 0xaf, 0x19
        db      0x67, 0x41, 0x00, 0x0c, 0x84, 0x03, 0x00, 0xbb
        db      0x19, 0x67, 0x51, 0x00, 0x54, 0xbe, 0x19, 0x67
        db      0x41, 0x00, 0x1e, 0x11, 0x43, 0x56, 0xca, 0x19
        db      0x67, 0x61, 0x00, 0x54, 0xcd, 0x19, 0x67, 0x41
        db      0x00, 0x74, 0x74, 0x34, 0xfb, 0x25, 0x18, 0x63
        db      0x34, 0xfb, 0x21, 0x19, 0x0f, 0x45, 0x5d, 0x02
        db      0x67, 0xff, 0x00, 0x68, 0x34, 0xf8, 0x21, 0x73
        db      0x0a, 0x87, 0xa9, 0x00, 0xf5, 0x19, 0x00, 0x7f
        db      0x58, 0xff, 0x6c, 0x09, 0x00, 0x40, 0x00, 0x28
        db      0x04, 0x00, 0x87, 0x81, 0x00, 0x04, 0x1a, 0x00
        db      0x7f, 0x80, 0xff, 0x6a, 0x40, 0x00, 0x28, 0x04
        db      0x00, 0x87, 0x29, 0x00, 0x13, 0x1a, 0x00, 0x7f
        db      0xd8, 0xff, 0x6b, 0x40, 0x00, 0x28, 0x04, 0x00
        db      0x69, 0x40, 0x00, 0x28, 0x04, 0x29, 0x2a, 0x01
        db      0x00, 0x7f, 0xfc, 0xff, 0x0a, 0x8c, 0x05, 0x00
        db      0x2a, 0x1a, 0x63, 0x54, 0x42, 0x1a, 0x00, 0x57
        db      0x03, 0x33, 0x1a, 0x39, 0x1a, 0x3f, 0x1a, 0x67
        db      0x81, 0x00, 0x54, 0x42, 0x1a, 0x67, 0x29, 0x00
        db      0x54, 0x42, 0x1a, 0x67, 0xa9, 0x00, 0x13, 0x02
        db      0x00, 0x68, 0x34, 0xfa, 0x21, 0x02, 0x0b, 0x6e
        db      0x6c, 0x01, 0x01, 0x74, 0x34, 0xf9, 0x25, 0x08
        db      0x63, 0x34, 0xf9, 0x21, 0x00, 0x55, 0x62, 0x1a
        db      0x67, 0x21, 0x00, 0x54, 0x65, 0x1a, 0x67, 0x29
        db      0x00, 0x34, 0xf8, 0x25, 0x04, 0x63, 0x34, 0xf8
        db      0x21, 0x00, 0x55, 0x77, 0x1a, 0x67, 0x31, 0x00
        db      0x54, 0x7a, 0x1a, 0x67, 0x39, 0x00, 0x34, 0xf7
        db      0x25, 0x04, 0x29, 0x2a, 0x03, 0x63, 0x34, 0xfc
        db      0x21, 0x6b, 0x6c, 0x0f, 0x00, 0x0d, 0x73, 0x76
        db      0x6c, 0x81, 0x00, 0x74, 0x34, 0xf6, 0x21, 0x67
        db      0x3f, 0x00, 0x6c, 0x0b, 0x00, 0x0e, 0x77, 0x73
        db      0x34, 0xf5, 0x21, 0x67, 0x3f, 0x00, 0x0d, 0x73
        db      0x34, 0xf5, 0x21, 0x67, 0x3f, 0x00, 0x6c, 0x15
        db      0x00, 0x0f, 0x77, 0x73, 0x34, 0xf5, 0x21, 0x67
        db      0x3f, 0x00, 0x6c, 0x0b, 0x00, 0x0f, 0x77, 0x73
        db      0x34, 0xf5, 0x21, 0x67, 0x3f, 0x00, 0x0e, 0x73
        db      0x34, 0xf5, 0x21, 0x67, 0x3f, 0x00, 0x6c, 0x15
        db      0x00, 0x10, 0x77, 0x73, 0x34, 0xf5, 0x21, 0x67
        db      0x3f, 0x00, 0x6c, 0x0b, 0x00, 0x10, 0x77, 0x73
        db      0x34, 0xf5, 0x21, 0x67, 0x3f, 0x00, 0x0f, 0x73
        db      0x34, 0xf5, 0x21, 0x00, 0x44, 0x0b, 0x43, 0x34
        db      0xfd, 0x26, 0x0e, 0x00, 0x84, 0x15, 0x00, 0x03
        db      0x1b, 0x63, 0x34, 0xfb, 0x21, 0x68, 0x0b, 0x43
        db      0x7f, 0x02, 0x00, 0x34, 0xfc, 0x26, 0x08, 0x63
        db      0x34, 0xf6, 0x21, 0x00, 0x34, 0xf4, 0x25, 0x04
        db      0x63, 0x0b, 0x5d, 0x0c, 0x7f, 0xfe, 0xff, 0x09
        db      0x0c, 0x7d, 0x55, 0x2c, 0x1b, 0x01, 0x0d, 0x5e
        db      0x0e, 0x31, 0x21, 0x01, 0x09, 0x7f, 0x02, 0x00
        db      0x14, 0x04, 0x00, 0x01, 0x7a, 0x55, 0x19, 0x1b
        db      0x63, 0x28, 0x06, 0x00, 0x36, 0x04, 0x00, 0x21
        db      0x31, 0x25, 0x04, 0x00, 0x47, 0x04, 0x5d, 0x0a
        db      0x0a, 0x0c, 0x2c, 0x11, 0x02, 0x4f, 0x17, 0xc1
        db      0x17, 0x24, 0x18, 0x2e, 0x18, 0x38, 0x18, 0x8e
        db      0x18, 0x96, 0x18, 0x9f, 0x18, 0x1c, 0x19, 0xd3
        db      0x19, 0x1a, 0x1a, 0x54, 0x1a, 0x69, 0x1a, 0x7f
        db      0x1a, 0xef, 0x1a, 0x0c, 0x1b, 0x2f, 0x1b, 0x02
        db      0x0a, 0x0c, 0x0e, 0x10, 0x12, 0x10, 0x00, 0x12
        db      0x14, 0x00, 0x12, 0x18, 0x00, 0x12, 0x1c, 0x00
        db      0x12, 0x20, 0x00, 0x12, 0x24, 0x00, 0x12, 0x28
        db      0x00, 0x12, 0x2c, 0x00, 0x12, 0x30, 0x00, 0x12
        db      0x34, 0x00, 0x12, 0x38, 0x00, 0x12, 0x3c, 0x00
        db      0x12, 0x40, 0x00, 0x12, 0x44, 0x00, 0x12, 0x48
        db      0x00, 0x3e, 0x13, 0x00, 0x28, 0x28, 0x2b, 0x01
        db      0x37, 0x1b, 0x39, 0x02, 0x00, 0x2b, 0x01, 0x14
        db      0x17, 0x39, 0x10, 0x00, 0x54, 0xcd, 0x1b, 0x63
        db      0x36, 0x06, 0x00, 0x21, 0x68, 0x36, 0x08, 0x00
        db      0x21, 0x0a, 0x0c, 0x40, 0x00, 0x28, 0x06, 0x35
        db      0x0a, 0x00, 0x36, 0x0c, 0x00, 0x21, 0x28, 0x02
        db      0x35, 0x0e, 0x00, 0x36, 0x0c, 0x00, 0x21, 0x28
        db      0x02, 0x63, 0x6c, 0x05, 0x20, 0x6c, 0x07, 0x08
        db      0x6c, 0x05, 0x10, 0x69, 0x0a, 0x0c, 0x0e, 0x10
        db      0x12, 0x10, 0x00, 0x3e, 0x05, 0x00, 0x13, 0x0a
        db      0x00, 0x36, 0x10, 0x00, 0x21, 0x0a, 0x47, 0x0a
        db      0x39, 0x0c, 0x00, 0x2b, 0x01, 0xc4, 0x1b, 0x39
        db      0x06, 0x00, 0x2b, 0x01, 0xbb, 0x1b, 0x39, 0x08
        db      0x00, 0x2b, 0x01, 0xab, 0x1b, 0x0a, 0x13, 0x02
        db      0x00, 0x09, 0x54, 0x0f, 0x1c, 0x63, 0x0b, 0x21
        db      0x64, 0x5d, 0x10, 0x64, 0x55, 0x09, 0x1c, 0x13
        db      0x04, 0x00, 0x8f
caml_globals_init_stack:
        db      0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00
        db      0x01, 0x00, 0x02, 0x00, 0x01, 0x00, 0x08, 0x00
        db      0x01, 0x00
caml_globals_init_heap:
        db      0xfc, 0x02, 0x77, 0x20, 0x00, 0x01, 0xfc, 0x02
        db      0x68, 0x20, 0x00, 0x01
caml_globals_init_end:

        include "config.asm"
        include "/usr/local/lib/ocapic/stdlib.asm"

        end