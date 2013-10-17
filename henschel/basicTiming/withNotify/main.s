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
..___tag_value_main.1:                                          #12.1
        pushq     %rbp                                          #12.1
..___tag_value_main.3:                                          #
        movq      %rsp, %rbp                                    #12.1
..___tag_value_main.4:                                          #
        andq      $-128, %rsp                                   #12.1
        pushq     %r12                                          #12.1
        subq      $120, %rsp                                    #12.1
        movl      $3, %edi                                      #12.1
..___tag_value_main.6:                                          #12.1
        call      __intel_new_proc_init                         #12.1
..___tag_value_main.7:                                          #
                                # LOE rbx r13 r14 r15
..B1.10:                        # Preds ..B1.1
        stmxcsr   16(%rsp)                                      #12.1
        xorl      %edi, %edi                                    #16.5
        lea       (%rsp), %rsi                                  #16.5
        orl       $32832, 16(%rsp)                              #12.1
        ldmxcsr   16(%rsp)                                      #12.1
..___tag_value_main.9:                                          #16.5
        call      clock_gettime                                 #16.5
..___tag_value_main.10:                                         #
                                # LOE rbx r13 r14 r15
..B1.2:                         # Preds ..B1.10
        xorl      %r12d, %r12d                                  #17.10
                                # LOE rbx r12 r13 r14 r15
..B1.3:                         # Preds ..B1.11 ..B1.2
        movl      %r12d, %edi                                   #20.6
..___tag_value_main.11:                                         #20.6
        call      myEmptyFunc                                   #20.6
..___tag_value_main.12:                                         #
                                # LOE rbx r12 r13 r14 r15 eax
..B1.11:                        # Preds ..B1.3
        incq      %r12                                          #17.26
        cmpq      $400000000, %r12                              #17.19
        jb        ..B1.3        # Prob 82%                      #17.19
                                # LOE rbx r12 r13 r14 r15 eax
..B1.4:                         # Preds ..B1.11
        xorl      %edi, %edi                                    #22.5
        lea       16(%rsp), %rsi                                #22.5
        movl      %eax, %r12d                                   #
..___tag_value_main.13:                                         #22.5
        call      clock_gettime                                 #22.5
..___tag_value_main.14:                                         #
                                # LOE rbx r13 r14 r15 r12d
..B1.5:                         # Preds ..B1.4
        cvtsi2sdq 16(%rsp), %xmm0                               #23.39
        cvtsi2sdq (%rsp), %xmm1                                 #23.89
        cvtsi2sdq 24(%rsp), %xmm3                               #23.71
        cvtsi2sdq 8(%rsp), %xmm2                                #23.122
        subsd     %xmm1, %xmm0                                  #23.122
        subsd     %xmm2, %xmm3                                  #23.122
        movsd     .L_2il0floatpacket.4(%rip), %xmm4             #23.51
        movl      $.L_2__STRING.0, %edi                         #23.5
        mulsd     %xmm4, %xmm0                                  #23.122
        movl      $1, %eax                                      #23.5
        addsd     %xmm3, %xmm0                                  #23.122
        divsd     %xmm4, %xmm0                                  #23.138
        movsd     %xmm0, 32(%rsp)                               #23.5
..___tag_value_main.15:                                         #23.5
        call      printf                                        #23.5
..___tag_value_main.16:                                         #
                                # LOE rbx r13 r14 r15 r12d
..B1.6:                         # Preds ..B1.5
        movsd     32(%rsp), %xmm0                               #
        movl      $.L_2__STRING.1, %edi                         #24.5
        divsd     .L_2il0floatpacket.5(%rip), %xmm0             #24.5
        movl      $1, %eax                                      #24.5
..___tag_value_main.17:                                         #24.5
        call      printf                                        #24.5
..___tag_value_main.18:                                         #
                                # LOE rbx r13 r14 r15 r12d
..B1.7:                         # Preds ..B1.6
        movl      %r12d, %eax                                   #25.12
        addq      $120, %rsp                                    #25.12
..___tag_value_main.19:                                         #25.12
        popq      %r12                                          #25.12
        movq      %rbp, %rsp                                    #25.12
        popq      %rbp                                          #25.12
..___tag_value_main.20:                                         #
        ret                                                     #25.12
        .align    16,0x90
..___tag_value_main.22:                                         #
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
# parameter 1: %edi
..B2.1:                         # Preds ..B2.0
..___tag_value_myEmptyFunc.23:                                  #6.1
        incl      %edi                                          #7.14
        movl      %edi, %eax                                    #7.14
        ret                                                     #7.14
        .align    16,0x90
..___tag_value_myEmptyFunc.25:                                  #
                                # LOE
# mark_end;
	.type	myEmptyFunc,@function
	.size	myEmptyFunc,.-myEmptyFunc
	.data
# -- End  myEmptyFunc
	.section .rodata, "a"
	.align 8
.L_2il0floatpacket.4:
	.long	0x00000000,0x41cdcd65
	.type	.L_2il0floatpacket.4,@object
	.size	.L_2il0floatpacket.4,8
	.align 8
.L_2il0floatpacket.5:
	.long	0x00000000,0x41b7d784
	.type	.L_2il0floatpacket.5,@object
	.size	.L_2il0floatpacket.5,8
	.section .rodata.str1.4, "aMS",@progbits,1
	.align 4
.L_2__STRING.0:
	.byte	10
	.byte	32
	.byte	37
	.byte	50
	.byte	46
	.byte	49
	.byte	53
	.byte	102
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
	.type	.L_2__STRING.0,@object
	.size	.L_2__STRING.0,18
	.space 2, 0x00 	# pad
	.align 4
.L_2__STRING.1:
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
	.byte	111
	.byte	117
	.byte	110
	.byte	116
	.byte	10
	.byte	0
	.type	.L_2__STRING.1,@object
	.size	.L_2__STRING.1,16
	.data
	.section .note.GNU-stack, ""
// -- Begin DWARF2 SEGMENT .eh_frame
	.section .eh_frame,"a",@progbits
.eh_frame_seg:
	.align 8
	.4byte 0x00000014
	.8byte 0x7801000100000000
	.8byte 0x0000019008070c10
	.4byte 0x00000000
	.4byte 0x0000004c
	.4byte 0x0000001c
	.8byte ..___tag_value_main.1
	.8byte ..___tag_value_main.22-..___tag_value_main.1
	.byte 0x04
	.4byte ..___tag_value_main.3-..___tag_value_main.1
	.2byte 0x100e
	.byte 0x04
	.4byte ..___tag_value_main.4-..___tag_value_main.3
	.4byte 0x8610060c
	.2byte 0x0402
	.4byte ..___tag_value_main.7-..___tag_value_main.4
	.8byte 0xff800d1c380e0c10
	.8byte 0xfffffff80d1affff
	.2byte 0x0422
	.4byte ..___tag_value_main.19-..___tag_value_main.7
	.2byte 0x04cc
	.4byte ..___tag_value_main.20-..___tag_value_main.19
	.4byte 0xc608070c
	.2byte 0x0000
	.4byte 0x00000014
	.4byte 0x0000006c
	.8byte ..___tag_value_myEmptyFunc.23
	.8byte ..___tag_value_myEmptyFunc.25-..___tag_value_myEmptyFunc.23
# End
