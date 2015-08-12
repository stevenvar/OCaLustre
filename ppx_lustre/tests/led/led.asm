        processor 18f4620

        include "p18f4620.inc"

	config	XINST = ON

CAML_STOP_AND_COPY_GC
STACK_SIZE  EQU   0x1
HEAP_SIZE   EQU   0x7

#define caml_useprim_caml_pic_set_bit
#define caml_useprim_caml_pic_clear_bit
#define caml_useprim_caml_pic_write_reg
#define caml_useprim_caml_sleep_millis

        include "/usr/local/lib/ocapic/interp.asm"
        include "/usr/local/lib/ocapic/runtime.asm"

        org     0x1600
caml_externals:
        goto    caml_pic_set_bit
        goto    caml_pic_clear_bit
        goto    caml_pic_write_reg
        goto    caml_sleep_millis

caml_bytecode:
        db      0x54, 0x18, 0x16, 0x00, 0x6a, 0x40, 0x00, 0x5b
        db      0x2b, 0x01, 0x13, 0x16, 0x39, 0x0c, 0x00, 0x54
        db      0x83, 0x16, 0x63, 0x36, 0x00, 0x00, 0x21, 0x88
        db      0x15, 0x00, 0x30, 0x16, 0x64, 0x54, 0x31, 0x16
        db      0x63, 0x28, 0x02, 0x35, 0x02, 0x00, 0x43, 0x36
        db      0x04, 0x00, 0x43, 0x21, 0x56, 0x43, 0x16, 0x63
        db      0x54, 0x4f, 0x16, 0x35, 0x06, 0x00, 0x43, 0x36
        db      0x04, 0x00, 0x43, 0x21, 0x7f, 0x02, 0x00, 0x36
        db      0x08, 0x00, 0x36, 0x02, 0x00, 0x49, 0x00, 0x3f
        db      0x00, 0x36, 0x06, 0x00, 0x49, 0x00, 0x28, 0x04
        db      0x29, 0x2a, 0x01, 0x01, 0x84, 0x03, 0x00, 0x6e
        db      0x16, 0x00, 0x5d, 0x00, 0x28, 0x04, 0x00, 0x5d
        db      0x02, 0x28, 0x04, 0x00, 0x56, 0x7b, 0x16, 0x00
        db      0x43, 0x28, 0x02, 0x35, 0x0a, 0x00, 0x36, 0x0c
        db      0x00, 0x25, 0x04, 0x2b, 0x01, 0x73, 0x16, 0x0a
        db      0x3f, 0x00, 0x39, 0x04, 0x00, 0x13, 0x02, 0x00
        db      0x2b, 0x01, 0x61, 0x16, 0x36, 0x0e, 0x00, 0x3f
        db      0x00, 0x39, 0x02, 0x00, 0x63, 0x3f, 0x00, 0x39
        db      0x06, 0x00, 0x2b, 0x01, 0x33, 0x16, 0x39, 0x00
        db      0x00, 0x2b, 0x01, 0x22, 0x16, 0x0a, 0x13, 0x02
        db      0x00, 0x68, 0x6c, 0x27, 0x00, 0x5e, 0x04, 0x54
        db      0xc7, 0x16, 0x63, 0x0b, 0x21, 0x6c, 0x03, 0x01
        db      0x0d, 0x22, 0x67, 0xe9, 0x03, 0x5d, 0x06, 0x64
        db      0x55, 0xba, 0x16, 0x13, 0x04, 0x00, 0x8f
caml_globals_init_stack:
        db      0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00
        db      0x02, 0x00, 0x06, 0x00, 0x01, 0x00, 0x0e, 0x00
caml_globals_init_heap:
        db      0x00, 0x01, 0x01, 0x00, 0xfc, 0x03, 0x6e, 0x6f
        db      0x6e, 0x65, 0x00, 0x01, 0x00, 0x01, 0x03, 0x00
caml_globals_init_end:

        include "config.asm"
        include "/usr/local/lib/ocapic/stdlib.asm"

        end
