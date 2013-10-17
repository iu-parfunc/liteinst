# mark_description "Intel(R) C Intel(R) 64 Compiler XE for applications running on Intel(R) 64, Version 13.0.0.079 Build 2012073";
# mark_description "1";
# mark_description "-lrt -Fa -o main.exe";
	.file "main.c"
	.text
..TXTST0:
# -- Begin  main
# mark_begin;
       .align    16,0x90
	.globl main
main:
..B1.1:                         # Preds ..B1.0
..___tag_value_main.1:                                          #13.1
        pushq     %rbp                                          #13.1
..___tag_value_main.3:                                          #
        movq      %rsp, %rbp                                    #13.1
..___tag_value_main.4:                                          #
        andq      $-128, %rsp                                   #13.1
        subq      $128, %rsp                                    #13.1
        movl      $3, %edi                                      #13.1
..___tag_value_main.6:                                          #13.1
        call      __intel_new_proc_init                         #13.1
..___tag_value_main.7:                                          #
                                # LOE rbx r12 r13 r14 r15
..B1.13:                        # Preds ..B1.1
        stmxcsr   16(%rsp)                                      #13.1
        xorl      %edi, %edi                                    #17.5
        lea       (%rsp), %rsi                                  #17.5
        orl       $32832, 16(%rsp)                              #13.1
        ldmxcsr   16(%rsp)                                      #13.1
..___tag_value_main.8:                                          #17.5
        call      clock_gettime                                 #17.5
..___tag_value_main.9:                                          #
                                # LOE rbx r12 r13 r14 r15
..B1.2:                         # Preds ..B1.13
        xorl      %eax, %eax                                    #18.10
        movq      %r12, 32(%rsp)                                #18.10
..___tag_value_main.10:                                         #
        movq      %rax, %r12                                    #18.10
                                # LOE rbx r12 r13 r14 r15
..B1.3:                         # Preds ..B1.4 ..B1.2
..___tag_value_main.11:                                         #21.2
        call      myEmptyFunc                                   #21.2
..___tag_value_main.12:                                         #
                                # LOE rbx r12 r13 r14 r15
..B1.4:                         # Preds ..B1.3
        incq      %r12                                          #18.26
        cmpq      $1000000000, %r12                             #18.19
        jb        ..B1.3        # Prob 82%                      #18.19
                                # LOE rbx r12 r13 r14 r15
..B1.5:                         # Preds ..B1.4
        xorl      %edi, %edi                                    #23.5
        lea       16(%rsp), %rsi                                #23.5
        movq      32(%rsp), %r12                                #
..___tag_value_main.13:                                         #
        call      clock_gettime                                 #23.5
..___tag_value_main.15:                                         #
                                # LOE rbx r12 r13 r14 r15
..B1.6:                         # Preds ..B1.5
        movsd     .L_2il0floatpacket.4(%rip), %xmm0             #24.51
        movl      $.L_2__STRING.2, %edi                         #25.5
        movl      $1, %eax                                      #25.5
..___tag_value_main.17:                                         #25.5
        call      printf                                        #25.5
..___tag_value_main.18:                                         #
                                # LOE rbx r12 r13 r14 r15
..B1.7:                         # Preds ..B1.6
        cvtsi2sdq 16(%rsp), %xmm0                               #26.50
        cvtsi2sdq (%rsp), %xmm1                                 #26.100
        cvtsi2sdq 24(%rsp), %xmm3                               #26.82
        cvtsi2sdq 8(%rsp), %xmm2                                #26.133
        subsd     %xmm1, %xmm0                                  #26.133
        subsd     %xmm2, %xmm3                                  #26.133
        movsd     .L_2il0floatpacket.5(%rip), %xmm4             #26.62
        movl      $.L_2__STRING.3, %edi                         #26.5
        mulsd     %xmm4, %xmm0                                  #26.133
        movl      $1, %eax                                      #26.5
        addsd     %xmm3, %xmm0                                  #26.133
        divsd     %xmm4, %xmm0                                  #26.149
        movsd     %xmm0, 40(%rsp)                               #26.5
..___tag_value_main.19:                                         #26.5
        call      printf                                        #26.5
..___tag_value_main.20:                                         #
                                # LOE rbx r12 r13 r14 r15
..B1.8:                         # Preds ..B1.7
        movsd     40(%rsp), %xmm0                               #
        movl      $.L_2__STRING.4, %edi                         #27.5
        movl      $1, %eax                                      #27.5
..___tag_value_main.21:                                         #27.5
        call      printf                                        #27.5
..___tag_value_main.22:                                         #
                                # LOE rbx r12 r13 r14 r15
..B1.9:                         # Preds ..B1.8
        movsd     40(%rsp), %xmm0                               #
        movl      $.L_2__STRING.5, %edi                         #28.5
        divsd     .L_2il0floatpacket.4(%rip), %xmm0             #28.5
        movl      $1, %eax                                      #28.5
..___tag_value_main.23:                                         #28.5
        call      printf                                        #28.5
..___tag_value_main.24:                                         #
                                # LOE rbx r12 r13 r14 r15
..B1.10:                        # Preds ..B1.9
        xorl      %eax, %eax                                    #29.12
        movq      %rbp, %rsp                                    #29.12
        popq      %rbp                                          #29.12
..___tag_value_main.25:                                         #
        ret                                                     #29.12
        .align    16,0x90
..___tag_value_main.27:                                         #
                                # LOE
# mark_end;
	.type	main,@function
	.size	main,.-main
	.data
# -- End  main
	.text
# -- Begin  myEmptyFunc
# mark_begin;
       .align    16,0x90
	.globl myEmptyFunc
myEmptyFunc:
..B2.1:                         # Preds ..B2.0
..___tag_value_myEmptyFunc.28:                                  #6.1
        movl      $1, %eax                                      #7.5
..L30:          # __notify_intrinsic ENTER int myEmptyFunc() C
                # dwarf: DW_OP_reg0
        .byte     102                                           #
        .byte     15                                            #
        .byte     31                                            #
        .byte     68                                            #
        .byte     0                                             #
        .byte     0                                             #
..L33:
                                # LOE rax rbx rbp r12 r13 r14 r15
..B2.2:                         # Preds ..B2.1
..L31:          # __notify_intrinsic LEAVE int myEmptyFunc() C
                # dwarf: DW_OP_reg0
        .byte     102                                           #
        .byte     15                                            #
        .byte     31                                            #
        .byte     68                                            #
        .byte     0                                             #
        .byte     0                                             #
..L34:
                                # LOE rbx rbp r12 r13 r14 r15
..B2.3:                         # Preds ..B2.2
        ret                                                     #9.5
        .align    16,0x90
..___tag_value_myEmptyFunc.32:                                  #
                                # LOE
# mark_end;
	.type	myEmptyFunc,@function
	.size	myEmptyFunc,.-myEmptyFunc
	.data
# -- End  myEmptyFunc
	.section .rodata, "a"
	.align 8
.L_2il0floatpacket.4:
	.long	0xab7d64b3,0x3fe12623
	.type	.L_2il0floatpacket.4,@object
	.size	.L_2il0floatpacket.4,8
	.align 8
.L_2il0floatpacket.5:
	.long	0x00000000,0x41cdcd65
	.type	.L_2il0floatpacket.5,@object
	.size	.L_2il0floatpacket.5,8
	.section .rodata.str1.4, "aMS",@progbits,1
	.align 4
.L_2__STRING.3:
	.byte	10
	.byte	32
	.byte	37
	.byte	50
	.byte	46
	.byte	49
	.byte	53
	.byte	102
	.byte	32
	.byte	114
	.byte	117
	.byte	110
	.byte	116
	.byte	105
	.byte	109
	.byte	101
	.byte	32
	.byte	105
	.byte	110
	.byte	32
	.byte	115
	.byte	101
	.byte	99
	.byte	111
	.byte	110
	.byte	100
	.byte	115
	.byte	10
	.byte	0
	.type	.L_2__STRING.3,@object
	.size	.L_2__STRING.3,29
	.space 3, 0x00 	# pad
	.align 4
.L_2__STRING.0:
	.byte	69
	.byte	78
	.byte	84
	.byte	69
	.byte	82
	.byte	32
	.byte	105
	.byte	110
	.byte	116
	.byte	32
	.byte	109
	.byte	121
	.byte	69
	.byte	109
	.byte	112
	.byte	116
	.byte	121
	.byte	70
	.byte	117
	.byte	110
	.byte	99
	.byte	40
	.byte	41
	.byte	32
	.byte	67
	.byte	0
	.type	.L_2__STRING.0,@object
	.size	.L_2__STRING.0,26
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.1:
	.byte	76
	.byte	69
	.byte	65
	.byte	86
	.byte	69
	.byte	32
	.byte	105
	.byte	110
	.byte	116
	.byte	32
	.byte	109
	.byte	121
	.byte	69
	.byte	109
	.byte	112
	.byte	116
	.byte	121
	.byte	70
	.byte	117
	.byte	110
	.byte	99
	.byte	40
	.byte	41
	.byte	32
	.byte	67
	.byte	0
	.type	.L_2__STRING.1,@object
	.size	.L_2__STRING.1,26
	.section .rodata.str1.32, "aMS",@progbits,1
	.align 4
.L_2__STRING.2:
	.byte	10
	.byte	32
	.byte	37
	.byte	50
	.byte	46
	.byte	49
	.byte	53
	.byte	102
	.byte	32
	.byte	99
	.byte	121
	.byte	99
	.byte	108
	.byte	101
	.byte	32
	.byte	116
	.byte	105
	.byte	109
	.byte	101
	.byte	32
	.byte	105
	.byte	110
	.byte	32
	.byte	110
	.byte	97
	.byte	110
	.byte	111
	.byte	32
	.byte	115
	.byte	101
	.byte	99
	.byte	111
	.byte	110
	.byte	100
	.byte	115
	.byte	10
	.byte	0
	.type	.L_2__STRING.2,@object
	.size	.L_2__STRING.2,37
	.space 3, 0x00 	# pad
	.align 4
.L_2__STRING.4:
	.byte	10
	.byte	32
	.byte	37
	.byte	50
	.byte	46
	.byte	49
	.byte	53
	.byte	102
	.byte	32
	.byte	114
	.byte	117
	.byte	110
	.byte	116
	.byte	105
	.byte	109
	.byte	101
	.byte	32
	.byte	112
	.byte	101
	.byte	114
	.byte	32
	.byte	102
	.byte	117
	.byte	110
	.byte	99
	.byte	116
	.byte	105
	.byte	111
	.byte	110
	.byte	32
	.byte	99
	.byte	97
	.byte	108
	.byte	108
	.byte	32
	.byte	105
	.byte	110
	.byte	32
	.byte	110
	.byte	97
	.byte	110
	.byte	111
	.byte	32
	.byte	115
	.byte	101
	.byte	99
	.byte	111
	.byte	110
	.byte	100
	.byte	115
	.byte	10
	.byte	0
	.type	.L_2__STRING.4,@object
	.size	.L_2__STRING.4,52
	.align 4
.L_2__STRING.5:
	.byte	10
	.byte	32
	.byte	37
	.byte	50
	.byte	46
	.byte	49
	.byte	53
	.byte	102
	.byte	32
	.byte	99
	.byte	121
	.byte	99
	.byte	108
	.byte	101
	.byte	115
	.byte	32
	.byte	112
	.byte	101
	.byte	114
	.byte	32
	.byte	102
	.byte	117
	.byte	110
	.byte	99
	.byte	116
	.byte	105
	.byte	111
	.byte	110
	.byte	32
	.byte	99
	.byte	97
	.byte	108
	.byte	108
	.byte	10
	.byte	0
	.type	.L_2__STRING.5,@object
	.size	.L_2__STRING.5,35
	.data
	.section .itt_notify_tab, "a"
..L35:
	.ascii ".itt_notify_tab\0"
	.word	257
	.word	2
	.long	..L36 - ..L35
	.long	52
	.long	..L37 - ..L35
	.long	4
	.quad	..L30
	.long	..L33 - ..L30
	.long	0
	.long	0
	.quad	..L31
	.long	..L34 - ..L31
	.long	26
	.long	2
..L36:
	.long	1163152965
	.long	1852383314
	.long	2037194868
	.long	1953525061
	.long	1853179513
	.long	539568227
	.long	1162608707
	.long	541414977
	.long	544501353
	.long	1833269613
	.long	1182364784
	.long	677604981
	.long	4399145
..L37:
	.long	1342263297
	.section .note.GNU-stack, ""
// -- Begin DWARF2 SEGMENT .eh_frame
	.section .eh_frame,"a",@progbits
.eh_frame_seg:
	.align 8
	.4byte 0x00000014
	.8byte 0x7801000100000000
	.8byte 0x0000019008070c10
	.4byte 0x00000000
	.4byte 0x00000064
	.4byte 0x0000001c
	.8byte ..___tag_value_main.1
	.8byte ..___tag_value_main.27-..___tag_value_main.1
	.byte 0x04
	.4byte ..___tag_value_main.3-..___tag_value_main.1
	.2byte 0x100e
	.byte 0x04
	.4byte ..___tag_value_main.4-..___tag_value_main.3
	.4byte 0x8610060c
	.2byte 0x0402
	.4byte ..___tag_value_main.10-..___tag_value_main.4
	.8byte 0xff800d1c380e0c10
	.8byte 0xffffffa00d1affff
	.2byte 0x0422
	.4byte ..___tag_value_main.13-..___tag_value_main.10
	.2byte 0x04cc
	.4byte ..___tag_value_main.15-..___tag_value_main.13
	.8byte 0xff800d1c380e0c10
	.8byte 0xffffffa00d1affff
	.2byte 0x0422
	.4byte ..___tag_value_main.25-..___tag_value_main.15
	.8byte 0x00000000c608070c
	.4byte 0x00000014
	.4byte 0x00000084
	.8byte ..___tag_value_myEmptyFunc.28
	.8byte ..___tag_value_myEmptyFunc.32-..___tag_value_myEmptyFunc.28
# End
