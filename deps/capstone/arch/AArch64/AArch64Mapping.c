/* Capstone Unified Disassembler Engine */
/* By Nguyen Anh Quynh <aquynh@gmail.com>, 2013> */

#include <stdio.h>	// debug
#include <string.h>

#include "../../include/arm64.h"
#include "../../utils.h"

#include "AArch64Mapping.h"

#define GET_INSTRINFO_ENUM
#include "AArch64GenInstrInfo.inc"

#ifndef CAPSTONE_DIET
static name_map reg_name_maps[] = {
	{ ARM64_REG_INVALID, NULL },
	//=========
	{ ARM64_REG_NZCV, "nzcv"},
	{ ARM64_REG_WSP, "wsp"},
	{ ARM64_REG_WZR, "wzr"},	// dummy data for array mapping order only
	{ ARM64_REG_SP, "sp"},
	{ ARM64_REG_XZR, "xzr"},	// dummy data for array mapping order only
	{ ARM64_REG_B0, "b0"},
	{ ARM64_REG_B1, "b1"},
	{ ARM64_REG_B2, "b2"},
	{ ARM64_REG_B3, "b3"},
	{ ARM64_REG_B4, "b4"},
	{ ARM64_REG_B5, "b5"},
	{ ARM64_REG_B6, "b6"},
	{ ARM64_REG_B7, "b7"},
	{ ARM64_REG_B8, "b8"},
	{ ARM64_REG_B9, "b9"},
	{ ARM64_REG_B10, "b10"},
	{ ARM64_REG_B11, "b11"},
	{ ARM64_REG_B12, "b12"},
	{ ARM64_REG_B13, "b13"},
	{ ARM64_REG_B14, "b14"},
	{ ARM64_REG_B15, "b15"},
	{ ARM64_REG_B16, "b16"},
	{ ARM64_REG_B17, "b17"},
	{ ARM64_REG_B18, "b18"},
	{ ARM64_REG_B19, "b19"},
	{ ARM64_REG_B20, "b20"},
	{ ARM64_REG_B21, "b21"},
	{ ARM64_REG_B22, "b22"},
	{ ARM64_REG_B23, "b23"},
	{ ARM64_REG_B24, "b24"},
	{ ARM64_REG_B25, "b25"},
	{ ARM64_REG_B26, "b26"},
	{ ARM64_REG_B27, "b27"},
	{ ARM64_REG_B28, "b28"},
	{ ARM64_REG_B29, "b29"},
	{ ARM64_REG_B30, "b30"},
	{ ARM64_REG_B31, "b31"},
	{ ARM64_REG_D0, "d0"},
	{ ARM64_REG_D1, "d1"},
	{ ARM64_REG_D2, "d2"},
	{ ARM64_REG_D3, "d3"},
	{ ARM64_REG_D4, "d4"},
	{ ARM64_REG_D5, "d5"},
	{ ARM64_REG_D6, "d6"},
	{ ARM64_REG_D7, "d7"},
	{ ARM64_REG_D8, "d8"},
	{ ARM64_REG_D9, "d9"},
	{ ARM64_REG_D10, "d10"},
	{ ARM64_REG_D11, "d11"},
	{ ARM64_REG_D12, "d12"},
	{ ARM64_REG_D13, "d13"},
	{ ARM64_REG_D14, "d14"},
	{ ARM64_REG_D15, "d15"},
	{ ARM64_REG_D16, "d16"},
	{ ARM64_REG_D17, "d17"},
	{ ARM64_REG_D18, "d18"},
	{ ARM64_REG_D19, "d19"},
	{ ARM64_REG_D20, "d20"},
	{ ARM64_REG_D21, "d21"},
	{ ARM64_REG_D22, "d22"},
	{ ARM64_REG_D23, "d23"},
	{ ARM64_REG_D24, "d24"},
	{ ARM64_REG_D25, "d25"},
	{ ARM64_REG_D26, "d26"},
	{ ARM64_REG_D27, "d27"},
	{ ARM64_REG_D28, "d28"},
	{ ARM64_REG_D29, "d29"},
	{ ARM64_REG_D30, "d30"},
	{ ARM64_REG_D31, "d31"},
	{ ARM64_REG_H0, "h0"},
	{ ARM64_REG_H1, "h1"},
	{ ARM64_REG_H2, "h2"},
	{ ARM64_REG_H3, "h3"},
	{ ARM64_REG_H4, "h4"},
	{ ARM64_REG_H5, "h5"},
	{ ARM64_REG_H6, "h6"},
	{ ARM64_REG_H7, "h7"},
	{ ARM64_REG_H8, "h8"},
	{ ARM64_REG_H9, "h9"},
	{ ARM64_REG_H10, "h10"},
	{ ARM64_REG_H11, "h11"},
	{ ARM64_REG_H12, "h12"},
	{ ARM64_REG_H13, "h13"},
	{ ARM64_REG_H14, "h14"},
	{ ARM64_REG_H15, "h15"},
	{ ARM64_REG_H16, "h16"},
	{ ARM64_REG_H17, "h17"},
	{ ARM64_REG_H18, "h18"},
	{ ARM64_REG_H19, "h19"},
	{ ARM64_REG_H20, "h20"},
	{ ARM64_REG_H21, "h21"},
	{ ARM64_REG_H22, "h22"},
	{ ARM64_REG_H23, "h23"},
	{ ARM64_REG_H24, "h24"},
	{ ARM64_REG_H25, "h25"},
	{ ARM64_REG_H26, "h26"},
	{ ARM64_REG_H27, "h27"},
	{ ARM64_REG_H28, "h28"},
	{ ARM64_REG_H29, "h29"},
	{ ARM64_REG_H30, "h30"},
	{ ARM64_REG_H31, "h31"},
	{ ARM64_REG_Q0, "q0"},
	{ ARM64_REG_Q1, "q1"},
	{ ARM64_REG_Q2, "q2"},
	{ ARM64_REG_Q3, "q3"},
	{ ARM64_REG_Q4, "q4"},
	{ ARM64_REG_Q5, "q5"},
	{ ARM64_REG_Q6, "q6"},
	{ ARM64_REG_Q7, "q7"},
	{ ARM64_REG_Q8, "q8"},
	{ ARM64_REG_Q9, "q9"},
	{ ARM64_REG_Q10, "q10"},
	{ ARM64_REG_Q11, "q11"},
	{ ARM64_REG_Q12, "q12"},
	{ ARM64_REG_Q13, "q13"},
	{ ARM64_REG_Q14, "q14"},
	{ ARM64_REG_Q15, "q15"},
	{ ARM64_REG_Q16, "q16"},
	{ ARM64_REG_Q17, "q17"},
	{ ARM64_REG_Q18, "q18"},
	{ ARM64_REG_Q19, "q19"},
	{ ARM64_REG_Q20, "q20"},
	{ ARM64_REG_Q21, "q21"},
	{ ARM64_REG_Q22, "q22"},
	{ ARM64_REG_Q23, "q23"},
	{ ARM64_REG_Q24, "q24"},
	{ ARM64_REG_Q25, "q25"},
	{ ARM64_REG_Q26, "q26"},
	{ ARM64_REG_Q27, "q27"},
	{ ARM64_REG_Q28, "q28"},
	{ ARM64_REG_Q29, "q29"},
	{ ARM64_REG_Q30, "q30"},
	{ ARM64_REG_Q31, "q31"},
	{ ARM64_REG_S0, "s0"},
	{ ARM64_REG_S1, "s1"},
	{ ARM64_REG_S2, "s2"},
	{ ARM64_REG_S3, "s3"},
	{ ARM64_REG_S4, "s4"},
	{ ARM64_REG_S5, "s5"},
	{ ARM64_REG_S6, "s6"},
	{ ARM64_REG_S7, "s7"},
	{ ARM64_REG_S8, "s8"},
	{ ARM64_REG_S9, "s9"},
	{ ARM64_REG_S10, "s10"},
	{ ARM64_REG_S11, "s11"},
	{ ARM64_REG_S12, "s12"},
	{ ARM64_REG_S13, "s13"},
	{ ARM64_REG_S14, "s14"},
	{ ARM64_REG_S15, "s15"},
	{ ARM64_REG_S16, "s16"},
	{ ARM64_REG_S17, "s17"},
	{ ARM64_REG_S18, "s18"},
	{ ARM64_REG_S19, "s19"},
	{ ARM64_REG_S20, "s20"},
	{ ARM64_REG_S21, "s21"},
	{ ARM64_REG_S22, "s22"},
	{ ARM64_REG_S23, "s23"},
	{ ARM64_REG_S24, "s24"},
	{ ARM64_REG_S25, "s25"},
	{ ARM64_REG_S26, "s26"},
	{ ARM64_REG_S27, "s27"},
	{ ARM64_REG_S28, "s28"},
	{ ARM64_REG_S29, "s29"},
	{ ARM64_REG_S30, "s30"},
	{ ARM64_REG_S31, "s31"},
	{ ARM64_REG_W0, "w0"},
	{ ARM64_REG_W1, "w1"},
	{ ARM64_REG_W2, "w2"},
	{ ARM64_REG_W3, "w3"},
	{ ARM64_REG_W4, "w4"},
	{ ARM64_REG_W5, "w5"},
	{ ARM64_REG_W6, "w6"},
	{ ARM64_REG_W7, "w7"},
	{ ARM64_REG_W8, "w8"},
	{ ARM64_REG_W9, "w9"},
	{ ARM64_REG_W10, "w10"},
	{ ARM64_REG_W11, "w11"},
	{ ARM64_REG_W12, "w12"},
	{ ARM64_REG_W13, "w13"},
	{ ARM64_REG_W14, "w14"},
	{ ARM64_REG_W15, "w15"},
	{ ARM64_REG_W16, "w16"},
	{ ARM64_REG_W17, "w17"},
	{ ARM64_REG_W18, "w18"},
	{ ARM64_REG_W19, "w19"},
	{ ARM64_REG_W20, "w20"},
	{ ARM64_REG_W21, "w21"},
	{ ARM64_REG_W22, "w22"},
	{ ARM64_REG_W23, "w23"},
	{ ARM64_REG_W24, "w24"},
	{ ARM64_REG_W25, "w25"},
	{ ARM64_REG_W26, "w26"},
	{ ARM64_REG_W27, "w27"},
	{ ARM64_REG_W28, "w28"},
	{ ARM64_REG_W29, "w29"},
	{ ARM64_REG_W30, "w30"},
	{ ARM64_REG_X0, "x0"},
	{ ARM64_REG_X1, "x1"},
	{ ARM64_REG_X2, "x2"},
	{ ARM64_REG_X3, "x3"},
	{ ARM64_REG_X4, "x4"},
	{ ARM64_REG_X5, "x5"},
	{ ARM64_REG_X6, "x6"},
	{ ARM64_REG_X7, "x7"},
	{ ARM64_REG_X8, "x8"},
	{ ARM64_REG_X9, "x9"},
	{ ARM64_REG_X10, "x10"},
	{ ARM64_REG_X11, "x11"},
	{ ARM64_REG_X12, "x12"},
	{ ARM64_REG_X13, "x13"},
	{ ARM64_REG_X14, "x14"},
	{ ARM64_REG_X15, "x15"},
	{ ARM64_REG_X16, "x16"},
	{ ARM64_REG_X17, "x17"},
	{ ARM64_REG_X18, "x18"},
	{ ARM64_REG_X19, "x19"},
	{ ARM64_REG_X20, "x20"},
	{ ARM64_REG_X21, "x21"},
	{ ARM64_REG_X22, "x22"},
	{ ARM64_REG_X23, "x23"},
	{ ARM64_REG_X24, "x24"},
	{ ARM64_REG_X25, "x25"},
	{ ARM64_REG_X26, "x26"},
	{ ARM64_REG_X27, "x27"},
	{ ARM64_REG_X28, "x28"},
	{ ARM64_REG_X29, "x29"},
	{ ARM64_REG_X30, "x30"},
};
#endif

const char *AArch64_reg_name(csh handle, unsigned int reg)
{
#ifndef CAPSTONE_DIET
	if (reg >= ARM64_REG_MAX)
		return NULL;

	return reg_name_maps[reg].name;
#else
	return NULL;
#endif
}

static insn_map insns[] = {
	// dummy item
	{
		0, 0,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},

	{
		AArch64_ABS16b, ARM64_INS_ABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ABS2d, ARM64_INS_ABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ABS2s, ARM64_INS_ABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ABS4h, ARM64_INS_ABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ABS4s, ARM64_INS_ABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ABS8b, ARM64_INS_ABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ABS8h, ARM64_INS_ABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ABSdd, ARM64_INS_ABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADCSwww, ARM64_INS_ADC,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADCSxxx, ARM64_INS_ADC,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADCwww, ARM64_INS_ADC,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADCxxx, ARM64_INS_ADC,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDHN2vvv_16b8h, ARM64_INS_ADDHN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDHN2vvv_4s2d, ARM64_INS_ADDHN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDHN2vvv_8h4s, ARM64_INS_ADDHN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDHNvvv_2s2d, ARM64_INS_ADDHN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDHNvvv_4h4s, ARM64_INS_ADDHN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDHNvvv_8b8h, ARM64_INS_ADDHN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDP_16B, ARM64_INS_ADDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDP_2D, ARM64_INS_ADDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDP_2S, ARM64_INS_ADDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDP_4H, ARM64_INS_ADDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDP_4S, ARM64_INS_ADDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDP_8B, ARM64_INS_ADDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDP_8H, ARM64_INS_ADDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDPvv_D_2D, ARM64_INS_ADDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSwww_asr, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSwww_lsl, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSwww_lsr, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSwww_sxtb, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSwww_sxth, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSwww_sxtw, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSwww_sxtx, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSwww_uxtb, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSwww_uxth, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSwww_uxtw, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSwww_uxtx, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSxxw_sxtb, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSxxw_sxth, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSxxw_sxtw, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSxxw_uxtb, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSxxw_uxth, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSxxw_uxtw, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSxxx_asr, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSxxx_lsl, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSxxx_lsr, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSxxx_sxtx, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDSxxx_uxtx, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDV_1b16b, ARM64_INS_ADDV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDV_1b8b, ARM64_INS_ADDV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDV_1h4h, ARM64_INS_ADDV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDV_1h8h, ARM64_INS_ADDV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDV_1s4s, ARM64_INS_ADDV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDddd, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDvvv_16B, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDvvv_2D, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDvvv_2S, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDvvv_4H, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDvvv_4S, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDvvv_8B, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDvvv_8H, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwwi_lsl0_S, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwwi_lsl0_cmp, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwwi_lsl0_s, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwwi_lsl12_S, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwwi_lsl12_cmp, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwwi_lsl12_s, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwww_asr, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwww_lsl, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwww_lsr, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwww_sxtb, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwww_sxth, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwww_sxtw, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwww_sxtx, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwww_uxtb, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwww_uxth, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwww_uxtw, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDwww_uxtx, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxi_lsl0_S, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxi_lsl0_cmp, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxi_lsl0_s, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxi_lsl12_S, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxi_lsl12_cmp, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxi_lsl12_s, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxw_sxtb, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxw_sxth, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxw_sxtw, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxw_uxtb, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxw_uxth, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxw_uxtw, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxx_asr, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxx_lsl, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxx_lsr, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxx_sxtx, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADDxxx_uxtx, ARM64_INS_ADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADRPxi, ARM64_INS_ADRP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ADRxi, ARM64_INS_ADR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_AESD, ARM64_INS_AESD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, ARM64_GRP_CRYPTO, 0 }, 0, 0
#endif
	},
	{
		AArch64_AESE, ARM64_INS_AESE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, ARM64_GRP_CRYPTO, 0 }, 0, 0
#endif
	},
	{
		AArch64_AESIMC, ARM64_INS_AESIMC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_AESMC, ARM64_INS_AESMC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDSwwi, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDSwww_asr, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDSwww_lsl, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDSwww_lsr, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDSwww_ror, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDSxxi, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDSxxx_asr, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDSxxx_lsl, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDSxxx_lsr, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDSxxx_ror, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDvvv_16B, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDvvv_8B, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDwwi, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDwww_asr, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDwww_lsl, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDwww_lsr, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDwww_ror, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDxxi, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDxxx_asr, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDxxx_lsl, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDxxx_lsr, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ANDxxx_ror, ARM64_INS_AND,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ASRVwww, ARM64_INS_ASR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ASRVxxx, ARM64_INS_ASR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ASRwwi, ARM64_INS_ASR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ASRxxi, ARM64_INS_ASR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ATix, ARM64_INS_AT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BFIwwii, ARM64_INS_BFI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BFIxxii, ARM64_INS_BFI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BFMwwii, ARM64_INS_BFM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BFMxxii, ARM64_INS_BFM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BFXILwwii, ARM64_INS_BFXIL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BFXILxxii, ARM64_INS_BFXIL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICSwww_asr, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICSwww_lsl, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICSwww_lsr, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICSwww_ror, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICSxxx_asr, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICSxxx_lsl, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICSxxx_lsr, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICSxxx_ror, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICvi_lsl_2S, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_BICvi_lsl_4H, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_BICvi_lsl_4S, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_BICvi_lsl_8H, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_BICvvv_16B, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_BICvvv_8B, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_BICwww_asr, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICwww_lsl, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICwww_lsr, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICwww_ror, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICxxx_asr, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICxxx_lsl, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICxxx_lsr, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BICxxx_ror, ARM64_INS_BIC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_BIFvvv_16B, ARM64_INS_BIF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_BIFvvv_8B, ARM64_INS_BIF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_BITvvv_16B, ARM64_INS_BIT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_BITvvv_8B, ARM64_INS_BIT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_BLRx, ARM64_INS_BLR,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_X30, 0 }, { 0 }, 1, 1
#endif
	},
	{
		AArch64_BLimm, ARM64_INS_BL,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_X30, 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_BRKi, ARM64_INS_BRK,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_BRx, ARM64_INS_BR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 1
#endif
	},
	{
		AArch64_BSLvvv_16B, ARM64_INS_BSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_BSLvvv_8B, ARM64_INS_BSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_Bcc, ARM64_INS_B,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_Bimm, ARM64_INS_B,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_CBNZw, ARM64_INS_CBNZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_CBNZx, ARM64_INS_CBNZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_CBZw, ARM64_INS_CBZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_CBZx, ARM64_INS_CBZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_CCMNwi, ARM64_INS_CCMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CCMNww, ARM64_INS_CCMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CCMNxi, ARM64_INS_CCMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CCMNxx, ARM64_INS_CCMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CCMPwi, ARM64_INS_CCMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CCMPww, ARM64_INS_CCMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CCMPxi, ARM64_INS_CCMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CCMPxx, ARM64_INS_CCMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CLREXi, ARM64_INS_CLREX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CLS16b, ARM64_INS_CLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CLS2s, ARM64_INS_CLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CLS4h, ARM64_INS_CLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CLS4s, ARM64_INS_CLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CLS8b, ARM64_INS_CLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CLS8h, ARM64_INS_CLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CLSww, ARM64_INS_CLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CLSxx, ARM64_INS_CLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CLZ16b, ARM64_INS_CLZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CLZ2s, ARM64_INS_CLZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CLZ4h, ARM64_INS_CLZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CLZ4s, ARM64_INS_CLZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CLZ8b, ARM64_INS_CLZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CLZ8h, ARM64_INS_CLZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CLZww, ARM64_INS_CLZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CLZxx, ARM64_INS_CLZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQddd, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQddi, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQvvi_16B, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQvvi_2D, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQvvi_2S, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQvvi_4H, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQvvi_4S, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQvvi_8B, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQvvi_8H, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQvvv_16B, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQvvv_2D, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQvvv_2S, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQvvv_4H, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQvvv_4S, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQvvv_8B, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMEQvvv_8H, ARM64_INS_CMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEddd, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEddi, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEvvi_16B, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEvvi_2D, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEvvi_2S, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEvvi_4H, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEvvi_4S, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEvvi_8B, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEvvi_8H, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEvvv_16B, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEvvv_2D, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEvvv_2S, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEvvv_4H, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEvvv_4S, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEvvv_8B, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGEvvv_8H, ARM64_INS_CMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTddd, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTddi, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTvvi_16B, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTvvi_2D, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTvvi_2S, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTvvi_4H, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTvvi_4S, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTvvi_8B, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTvvi_8H, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTvvv_16B, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTvvv_2D, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTvvv_2S, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTvvv_4H, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTvvv_4S, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTvvv_8B, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMGTvvv_8H, ARM64_INS_CMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHIddd, ARM64_INS_CMHI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHIvvv_16B, ARM64_INS_CMHI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHIvvv_2D, ARM64_INS_CMHI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHIvvv_2S, ARM64_INS_CMHI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHIvvv_4H, ARM64_INS_CMHI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHIvvv_4S, ARM64_INS_CMHI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHIvvv_8B, ARM64_INS_CMHI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHIvvv_8H, ARM64_INS_CMHI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHSddd, ARM64_INS_CMHS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHSvvv_16B, ARM64_INS_CMHS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHSvvv_2D, ARM64_INS_CMHS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHSvvv_2S, ARM64_INS_CMHS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHSvvv_4H, ARM64_INS_CMHS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHSvvv_4S, ARM64_INS_CMHS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHSvvv_8B, ARM64_INS_CMHS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMHSvvv_8H, ARM64_INS_CMHS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLEddi, ARM64_INS_CMLE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLEvvi_16B, ARM64_INS_CMLE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLEvvi_2D, ARM64_INS_CMLE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLEvvi_2S, ARM64_INS_CMLE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLEvvi_4H, ARM64_INS_CMLE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLEvvi_4S, ARM64_INS_CMLE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLEvvi_8B, ARM64_INS_CMLE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLEvvi_8H, ARM64_INS_CMLE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLTddi, ARM64_INS_CMLT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLTvvi_16B, ARM64_INS_CMLT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLTvvi_2D, ARM64_INS_CMLT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLTvvi_2S, ARM64_INS_CMLT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLTvvi_4H, ARM64_INS_CMLT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLTvvi_4S, ARM64_INS_CMLT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLTvvi_8B, ARM64_INS_CMLT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMLTvvi_8H, ARM64_INS_CMLT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNww_asr, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNww_lsl, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNww_lsr, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNww_sxtb, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNww_sxth, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNww_sxtw, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNww_sxtx, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNww_uxtb, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNww_uxth, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNww_uxtw, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNww_uxtx, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNxw_sxtb, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNxw_sxth, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNxw_sxtw, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNxw_uxtb, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNxw_uxth, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNxw_uxtw, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNxx_asr, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNxx_lsl, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNxx_lsr, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNxx_sxtx, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMNxx_uxtx, ARM64_INS_CMN,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPww_asr, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPww_lsl, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPww_lsr, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPww_sxtb, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPww_sxth, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPww_sxtw, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPww_sxtx, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPww_uxtb, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPww_uxth, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPww_uxtw, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPww_uxtx, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPxw_sxtb, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPxw_sxth, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPxw_sxtw, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPxw_uxtb, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPxw_uxth, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPxw_uxtw, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPxx_asr, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPxx_lsl, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPxx_lsr, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPxx_sxtx, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMPxx_uxtx, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CMTSTddd, ARM64_INS_CMTST,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMTSTvvv_16B, ARM64_INS_CMTST,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMTSTvvv_2D, ARM64_INS_CMTST,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMTSTvvv_2S, ARM64_INS_CMTST,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMTSTvvv_4H, ARM64_INS_CMTST,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMTSTvvv_4S, ARM64_INS_CMTST,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMTSTvvv_8B, ARM64_INS_CMTST,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CMTSTvvv_8H, ARM64_INS_CMTST,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CNT16b, ARM64_INS_CNT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CNT8b, ARM64_INS_CNT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_CRC32B_www, ARM64_INS_CRC32B,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CRC32CB_www, ARM64_INS_CRC32CB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CRC32CH_www, ARM64_INS_CRC32CH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CRC32CW_www, ARM64_INS_CRC32CW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CRC32CX_wwx, ARM64_INS_CRC32CX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CRC32H_www, ARM64_INS_CRC32H,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CRC32W_www, ARM64_INS_CRC32W,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CRC32X_wwx, ARM64_INS_CRC32X,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CSELwwwc, ARM64_INS_CSEL,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CSELxxxc, ARM64_INS_CSEL,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CSINCwwwc, ARM64_INS_CSINC,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CSINCxxxc, ARM64_INS_CSINC,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CSINVwwwc, ARM64_INS_CSINV,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CSINVxxxc, ARM64_INS_CSINV,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CSNEGwwwc, ARM64_INS_CSNEG,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_CSNEGxxxc, ARM64_INS_CSNEG,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_DCPS1i, ARM64_INS_DCPS1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_DCPS2i, ARM64_INS_DCPS2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_DCPS3i, ARM64_INS_DCPS3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_DCix, ARM64_INS_DC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_DMBi, ARM64_INS_DMB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_DRPS, ARM64_INS_DRPS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 1
#endif
	},
	{
		AArch64_DSBi, ARM64_INS_DSB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_DUP16b, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUP2d, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUP2s, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUP4h, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUP4s, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUP8b, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUP8h, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUPELT16b, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUPELT2d, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUPELT2s, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUPELT4h, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUPELT4s, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUPELT8b, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUPELT8h, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUPbv_B, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUPdv_D, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUPhv_H, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_DUPsv_S, ARM64_INS_DUP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_EONwww_asr, ARM64_INS_EON,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EONwww_lsl, ARM64_INS_EON,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EONwww_lsr, ARM64_INS_EON,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EONwww_ror, ARM64_INS_EON,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EONxxx_asr, ARM64_INS_EON,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EONxxx_lsl, ARM64_INS_EON,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EONxxx_lsr, ARM64_INS_EON,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EONxxx_ror, ARM64_INS_EON,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EORvvv_16B, ARM64_INS_EOR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_EORvvv_8B, ARM64_INS_EOR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_EORwwi, ARM64_INS_EOR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EORwww_asr, ARM64_INS_EOR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EORwww_lsl, ARM64_INS_EOR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EORwww_lsr, ARM64_INS_EOR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EORwww_ror, ARM64_INS_EOR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EORxxi, ARM64_INS_EOR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EORxxx_asr, ARM64_INS_EOR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EORxxx_lsl, ARM64_INS_EOR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EORxxx_lsr, ARM64_INS_EOR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EORxxx_ror, ARM64_INS_EOR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ERET, ARM64_INS_ERET,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 1
#endif
	},
	{
		AArch64_EXTRwwwi, ARM64_INS_EXTR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EXTRxxxi, ARM64_INS_EXTR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_EXTvvvi_16b, ARM64_INS_EXT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_EXTvvvi_8b, ARM64_INS_EXT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FABDddd, ARM64_INS_FABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FABDsss, ARM64_INS_FABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FABDvvv_2D, ARM64_INS_FABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FABDvvv_2S, ARM64_INS_FABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FABDvvv_4S, ARM64_INS_FABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FABS2d, ARM64_INS_FABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FABS2s, ARM64_INS_FABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FABS4s, ARM64_INS_FABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FABSdd, ARM64_INS_FABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FABSss, ARM64_INS_FABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FACGEddd, ARM64_INS_FACGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FACGEsss, ARM64_INS_FACGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FACGEvvv_2D, ARM64_INS_FACGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FACGEvvv_2S, ARM64_INS_FACGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FACGEvvv_4S, ARM64_INS_FACGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FACGTddd, ARM64_INS_FACGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FACGTsss, ARM64_INS_FACGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FACGTvvv_2D, ARM64_INS_FACGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FACGTvvv_2S, ARM64_INS_FACGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FACGTvvv_4S, ARM64_INS_FACGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FADDP_2D, ARM64_INS_FADDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FADDP_2S, ARM64_INS_FADDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FADDP_4S, ARM64_INS_FADDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FADDPvv_D_2D, ARM64_INS_FADDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FADDPvv_S_2S, ARM64_INS_FADDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FADDddd, ARM64_INS_FADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FADDsss, ARM64_INS_FADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FADDvvv_2D, ARM64_INS_FADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FADDvvv_2S, ARM64_INS_FADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FADDvvv_4S, ARM64_INS_FADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCCMPEdd, ARM64_INS_FCCMPE,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCCMPEss, ARM64_INS_FCCMPE,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCCMPdd, ARM64_INS_FCCMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCCMPss, ARM64_INS_FCCMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMEQZddi, ARM64_INS_FCMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMEQZssi, ARM64_INS_FCMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMEQddd, ARM64_INS_FCMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMEQsss, ARM64_INS_FCMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMEQvvi_2D, ARM64_INS_FCMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMEQvvi_2S, ARM64_INS_FCMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMEQvvi_4S, ARM64_INS_FCMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMEQvvv_2D, ARM64_INS_FCMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMEQvvv_2S, ARM64_INS_FCMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMEQvvv_4S, ARM64_INS_FCMEQ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGEZddi, ARM64_INS_FCMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGEZssi, ARM64_INS_FCMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGEddd, ARM64_INS_FCMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGEsss, ARM64_INS_FCMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGEvvi_2D, ARM64_INS_FCMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGEvvi_2S, ARM64_INS_FCMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGEvvi_4S, ARM64_INS_FCMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGEvvv_2D, ARM64_INS_FCMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGEvvv_2S, ARM64_INS_FCMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGEvvv_4S, ARM64_INS_FCMGE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGTZddi, ARM64_INS_FCMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGTZssi, ARM64_INS_FCMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGTddd, ARM64_INS_FCMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGTsss, ARM64_INS_FCMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGTvvi_2D, ARM64_INS_FCMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGTvvi_2S, ARM64_INS_FCMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGTvvi_4S, ARM64_INS_FCMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGTvvv_2D, ARM64_INS_FCMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGTvvv_2S, ARM64_INS_FCMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMGTvvv_4S, ARM64_INS_FCMGT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMLEZddi, ARM64_INS_FCMLE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMLEZssi, ARM64_INS_FCMLE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMLEvvi_2D, ARM64_INS_FCMLE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMLEvvi_2S, ARM64_INS_FCMLE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMLEvvi_4S, ARM64_INS_FCMLE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMLTZddi, ARM64_INS_FCMLT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMLTZssi, ARM64_INS_FCMLT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMLTvvi_2D, ARM64_INS_FCMLT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMLTvvi_2S, ARM64_INS_FCMLT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMLTvvi_4S, ARM64_INS_FCMLT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMPdd_quiet, ARM64_INS_FCMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMPdd_sig, ARM64_INS_FCMPE,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMPdi_quiet, ARM64_INS_FCMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMPdi_sig, ARM64_INS_FCMPE,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMPsi_quiet, ARM64_INS_FCMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMPsi_sig, ARM64_INS_FCMPE,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMPss_quiet, ARM64_INS_FCMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCMPss_sig, ARM64_INS_FCMPE,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCSELdddc, ARM64_INS_FCSEL,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCSELsssc, ARM64_INS_FCSEL,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTAS_2d, ARM64_INS_FCVTAS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTAS_2s, ARM64_INS_FCVTAS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTAS_4s, ARM64_INS_FCVTAS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTASdd, ARM64_INS_FCVTAS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTASss, ARM64_INS_FCVTAS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTASwd, ARM64_INS_FCVTAS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTASws, ARM64_INS_FCVTAS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTASxd, ARM64_INS_FCVTAS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTASxs, ARM64_INS_FCVTAS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTAU_2d, ARM64_INS_FCVTAU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTAU_2s, ARM64_INS_FCVTAU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTAU_4s, ARM64_INS_FCVTAU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTAUdd, ARM64_INS_FCVTAU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTAUss, ARM64_INS_FCVTAU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTAUwd, ARM64_INS_FCVTAU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTAUws, ARM64_INS_FCVTAU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTAUxd, ARM64_INS_FCVTAU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTAUxs, ARM64_INS_FCVTAU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTL2s2d, ARM64_INS_FCVTL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTL4h4s, ARM64_INS_FCVTL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTL4s2d, ARM64_INS_FCVTL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTL8h4s, ARM64_INS_FCVTL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMS_2d, ARM64_INS_FCVTMS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMS_2s, ARM64_INS_FCVTMS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMS_4s, ARM64_INS_FCVTMS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMSdd, ARM64_INS_FCVTMS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMSss, ARM64_INS_FCVTMS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMSwd, ARM64_INS_FCVTMS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMSws, ARM64_INS_FCVTMS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMSxd, ARM64_INS_FCVTMS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMSxs, ARM64_INS_FCVTMS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMU_2d, ARM64_INS_FCVTMU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMU_2s, ARM64_INS_FCVTMU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMU_4s, ARM64_INS_FCVTMU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMUdd, ARM64_INS_FCVTMU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMUss, ARM64_INS_FCVTMU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMUwd, ARM64_INS_FCVTMU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMUws, ARM64_INS_FCVTMU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMUxd, ARM64_INS_FCVTMU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTMUxs, ARM64_INS_FCVTMU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTN2d2s, ARM64_INS_FCVTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTN2d4s, ARM64_INS_FCVTN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTN4s4h, ARM64_INS_FCVTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTN4s8h, ARM64_INS_FCVTN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNS_2d, ARM64_INS_FCVTNS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNS_2s, ARM64_INS_FCVTNS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNS_4s, ARM64_INS_FCVTNS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNSdd, ARM64_INS_FCVTNS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNSss, ARM64_INS_FCVTNS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNSwd, ARM64_INS_FCVTNS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNSws, ARM64_INS_FCVTNS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNSxd, ARM64_INS_FCVTNS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNSxs, ARM64_INS_FCVTNS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNU_2d, ARM64_INS_FCVTNU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNU_2s, ARM64_INS_FCVTNU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNU_4s, ARM64_INS_FCVTNU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNUdd, ARM64_INS_FCVTNU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNUss, ARM64_INS_FCVTNU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNUwd, ARM64_INS_FCVTNU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNUws, ARM64_INS_FCVTNU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNUxd, ARM64_INS_FCVTNU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTNUxs, ARM64_INS_FCVTNU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPS_2d, ARM64_INS_FCVTPS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPS_2s, ARM64_INS_FCVTPS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPS_4s, ARM64_INS_FCVTPS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPSdd, ARM64_INS_FCVTPS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPSss, ARM64_INS_FCVTPS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPSwd, ARM64_INS_FCVTPS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPSws, ARM64_INS_FCVTPS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPSxd, ARM64_INS_FCVTPS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPSxs, ARM64_INS_FCVTPS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPU_2d, ARM64_INS_FCVTPU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPU_2s, ARM64_INS_FCVTPU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPU_4s, ARM64_INS_FCVTPU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPUdd, ARM64_INS_FCVTPU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPUss, ARM64_INS_FCVTPU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPUwd, ARM64_INS_FCVTPU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPUws, ARM64_INS_FCVTPU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPUxd, ARM64_INS_FCVTPU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTPUxs, ARM64_INS_FCVTPU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTXN, ARM64_INS_FCVTXN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTXN2d2s, ARM64_INS_FCVTXN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTXN2d4s, ARM64_INS_FCVTXN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZS_2d, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZS_2s, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZS_4s, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZS_Nddi, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZS_Nssi, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZSdd, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZSss, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZSwd, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZSwdi, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZSws, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZSwsi, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZSxd, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZSxdi, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZSxs, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZSxsi, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZU_2d, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZU_2s, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZU_4s, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZU_Nddi, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZU_Nssi, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZUdd, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZUss, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZUwd, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZUwdi, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZUws, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZUwsi, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZUxd, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZUxdi, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZUxs, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTZUxsi, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTdh, ARM64_INS_FCVT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTds, ARM64_INS_FCVT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVThd, ARM64_INS_FCVT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVThs, ARM64_INS_FCVT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTsd, ARM64_INS_FCVT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FCVTsh, ARM64_INS_FCVT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FDIVddd, ARM64_INS_FDIV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FDIVsss, ARM64_INS_FDIV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FDIVvvv_2D, ARM64_INS_FDIV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FDIVvvv_2S, ARM64_INS_FDIV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FDIVvvv_4S, ARM64_INS_FDIV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMADDdddd, ARM64_INS_FMADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMADDssss, ARM64_INS_FMADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXNMPvv_D_2D, ARM64_INS_FMAXNMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXNMPvv_S_2S, ARM64_INS_FMAXNMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXNMPvvv_2D, ARM64_INS_FMAXNMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXNMPvvv_2S, ARM64_INS_FMAXNMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXNMPvvv_4S, ARM64_INS_FMAXNMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXNMV_1s4s, ARM64_INS_FMAXNMV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXNMddd, ARM64_INS_FMAXNM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXNMsss, ARM64_INS_FMAXNM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXNMvvv_2D, ARM64_INS_FMAXNM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXNMvvv_2S, ARM64_INS_FMAXNM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXNMvvv_4S, ARM64_INS_FMAXNM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXPvv_D_2D, ARM64_INS_FMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXPvv_S_2S, ARM64_INS_FMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXPvvv_2D, ARM64_INS_FMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXPvvv_2S, ARM64_INS_FMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXPvvv_4S, ARM64_INS_FMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXV_1s4s, ARM64_INS_FMAXV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXddd, ARM64_INS_FMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXsss, ARM64_INS_FMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXvvv_2D, ARM64_INS_FMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXvvv_2S, ARM64_INS_FMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMAXvvv_4S, ARM64_INS_FMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINNMPvv_D_2D, ARM64_INS_FMINNMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINNMPvv_S_2S, ARM64_INS_FMINNMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINNMPvvv_2D, ARM64_INS_FMINNMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINNMPvvv_2S, ARM64_INS_FMINNMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINNMPvvv_4S, ARM64_INS_FMINNMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINNMV_1s4s, ARM64_INS_FMINNMV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINNMddd, ARM64_INS_FMINNM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINNMsss, ARM64_INS_FMINNM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINNMvvv_2D, ARM64_INS_FMINNM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINNMvvv_2S, ARM64_INS_FMINNM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINNMvvv_4S, ARM64_INS_FMINNM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINPvv_D_2D, ARM64_INS_FMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINPvv_S_2S, ARM64_INS_FMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINPvvv_2D, ARM64_INS_FMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINPvvv_2S, ARM64_INS_FMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINPvvv_4S, ARM64_INS_FMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINV_1s4s, ARM64_INS_FMINV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINddd, ARM64_INS_FMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINsss, ARM64_INS_FMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINvvv_2D, ARM64_INS_FMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINvvv_2S, ARM64_INS_FMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMINvvv_4S, ARM64_INS_FMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLAddv_2D, ARM64_INS_FMLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLAssv_4S, ARM64_INS_FMLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLAvve_2d2d, ARM64_INS_FMLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLAvve_2s4s, ARM64_INS_FMLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLAvve_4s4s, ARM64_INS_FMLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLAvvv_2D, ARM64_INS_FMLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLAvvv_2S, ARM64_INS_FMLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLAvvv_4S, ARM64_INS_FMLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLSddv_2D, ARM64_INS_FMLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLSssv_4S, ARM64_INS_FMLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLSvve_2d2d, ARM64_INS_FMLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLSvve_2s4s, ARM64_INS_FMLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLSvve_4s4s, ARM64_INS_FMLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLSvvv_2D, ARM64_INS_FMLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLSvvv_2S, ARM64_INS_FMLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMLSvvv_4S, ARM64_INS_FMLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMOVdd, ARM64_INS_FMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMOVdi, ARM64_INS_FMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMOVdx, ARM64_INS_FMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMOVsi, ARM64_INS_FMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMOVss, ARM64_INS_FMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMOVsw, ARM64_INS_FMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMOVvi_2D, ARM64_INS_FMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMOVvi_2S, ARM64_INS_FMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMOVvi_4S, ARM64_INS_FMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMOVvx, ARM64_INS_FMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMOVws, ARM64_INS_FMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMOVxd, ARM64_INS_FMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMOVxv, ARM64_INS_FMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMSUBdddd, ARM64_INS_FMSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMSUBssss, ARM64_INS_FMSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULXddd, ARM64_INS_FMULX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULXddv_2D, ARM64_INS_FMULX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULXsss, ARM64_INS_FMULX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULXssv_4S, ARM64_INS_FMULX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULXve_2d2d, ARM64_INS_FMULX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULXve_2s4s, ARM64_INS_FMULX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULXve_4s4s, ARM64_INS_FMULX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULXvvv_2D, ARM64_INS_FMULX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULXvvv_2S, ARM64_INS_FMULX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULXvvv_4S, ARM64_INS_FMULX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULddd, ARM64_INS_FMUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULddv_2D, ARM64_INS_FMUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULsss, ARM64_INS_FMUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULssv_4S, ARM64_INS_FMUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULve_2d2d, ARM64_INS_FMUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULve_2s4s, ARM64_INS_FMUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULve_4s4s, ARM64_INS_FMUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULvvv_2D, ARM64_INS_FMUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULvvv_2S, ARM64_INS_FMUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FMULvvv_4S, ARM64_INS_FMUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FNEG2d, ARM64_INS_FNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FNEG2s, ARM64_INS_FNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FNEG4s, ARM64_INS_FNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FNEGdd, ARM64_INS_FNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FNEGss, ARM64_INS_FNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FNMADDdddd, ARM64_INS_FNMADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FNMADDssss, ARM64_INS_FNMADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FNMSUBdddd, ARM64_INS_FNMSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FNMSUBssss, ARM64_INS_FNMSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FNMULddd, ARM64_INS_FNMUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FNMULsss, ARM64_INS_FNMUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRECPE_2d, ARM64_INS_FRECPE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRECPE_2s, ARM64_INS_FRECPE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRECPE_4s, ARM64_INS_FRECPE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRECPEdd, ARM64_INS_FRECPE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRECPEss, ARM64_INS_FRECPE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRECPSddd, ARM64_INS_FRECPS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRECPSsss, ARM64_INS_FRECPS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRECPSvvv_2D, ARM64_INS_FRECPS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRECPSvvv_2S, ARM64_INS_FRECPS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRECPSvvv_4S, ARM64_INS_FRECPS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRECPXdd, ARM64_INS_FRECPX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRECPXss, ARM64_INS_FRECPX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTA_2d, ARM64_INS_FRINTA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTA_2s, ARM64_INS_FRINTA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTA_4s, ARM64_INS_FRINTA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTAdd, ARM64_INS_FRINTA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTAss, ARM64_INS_FRINTA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTI_2d, ARM64_INS_FRINTI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTI_2s, ARM64_INS_FRINTI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTI_4s, ARM64_INS_FRINTI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTIdd, ARM64_INS_FRINTI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTIss, ARM64_INS_FRINTI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTM_2d, ARM64_INS_FRINTM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTM_2s, ARM64_INS_FRINTM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTM_4s, ARM64_INS_FRINTM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTMdd, ARM64_INS_FRINTM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTMss, ARM64_INS_FRINTM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTN_2d, ARM64_INS_FRINTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTN_2s, ARM64_INS_FRINTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTN_4s, ARM64_INS_FRINTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTNdd, ARM64_INS_FRINTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTNss, ARM64_INS_FRINTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTP_2d, ARM64_INS_FRINTP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTP_2s, ARM64_INS_FRINTP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTP_4s, ARM64_INS_FRINTP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTPdd, ARM64_INS_FRINTP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTPss, ARM64_INS_FRINTP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTX_2d, ARM64_INS_FRINTX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTX_2s, ARM64_INS_FRINTX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTX_4s, ARM64_INS_FRINTX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTXdd, ARM64_INS_FRINTX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTXss, ARM64_INS_FRINTX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTZ_2d, ARM64_INS_FRINTZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTZ_2s, ARM64_INS_FRINTZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTZ_4s, ARM64_INS_FRINTZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTZdd, ARM64_INS_FRINTZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRINTZss, ARM64_INS_FRINTZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRSQRTE_2d, ARM64_INS_FRSQRTE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRSQRTE_2s, ARM64_INS_FRSQRTE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRSQRTE_4s, ARM64_INS_FRSQRTE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRSQRTEdd, ARM64_INS_FRSQRTE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRSQRTEss, ARM64_INS_FRSQRTE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRSQRTSddd, ARM64_INS_FRSQRTS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRSQRTSsss, ARM64_INS_FRSQRTS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRSQRTSvvv_2D, ARM64_INS_FRSQRTS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRSQRTSvvv_2S, ARM64_INS_FRSQRTS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FRSQRTSvvv_4S, ARM64_INS_FRSQRTS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FSQRT_2d, ARM64_INS_FSQRT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FSQRT_2s, ARM64_INS_FSQRT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FSQRT_4s, ARM64_INS_FSQRT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FSQRTdd, ARM64_INS_FSQRT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FSQRTss, ARM64_INS_FSQRT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FSUBddd, ARM64_INS_FSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FSUBsss, ARM64_INS_FSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_FSUBvvv_2D, ARM64_INS_FSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FSUBvvv_2S, ARM64_INS_FSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_FSUBvvv_4S, ARM64_INS_FSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_HINTi, ARM64_INS_HINT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_HLTi, ARM64_INS_HLT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_HVCi, ARM64_INS_HVC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_ICi, ARM64_INS_IC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ICix, ARM64_INS_IC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_INSELb, ARM64_INS_INS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_INSELd, ARM64_INS_INS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_INSELh, ARM64_INS_INS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_INSELs, ARM64_INS_INS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_INSbw, ARM64_INS_INS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_INSdx, ARM64_INS_INS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_INShw, ARM64_INS_INS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_INSsw, ARM64_INS_INS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ISBi, ARM64_INS_ISB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1LN_B, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1LN_D, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1LN_H, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1LN_S, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1LN_WB_B_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1LN_WB_B_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1LN_WB_D_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1LN_WB_D_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1LN_WB_H_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1LN_WB_H_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1LN_WB_S_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1LN_WB_S_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_16B, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_1D, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_2D, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_2S, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_4H, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_4S, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_8B, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_8H, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_16B_fixed, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_16B_register, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_1D_fixed, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_1D_register, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_2D_fixed, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_2D_register, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_2S_fixed, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_2S_register, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_4H_fixed, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_4H_register, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_4S_fixed, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_4S_register, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_8B_fixed, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_8B_register, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_8H_fixed, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1R_WB_8H_register, ARM64_INS_LD1R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_16B_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_16B_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_1D_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_1D_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_2D_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_2D_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_2S_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_2S_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_4H_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_4H_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_4S_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_4S_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_8B_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_8B_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_8H_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1WB_8H_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1_16B, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1_1D, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1_2D, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1_2S, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1_4H, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1_4S, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1_8B, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1_8H, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_16B_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_16B_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_1D_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_1D_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_2D_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_2D_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_2S_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_2S_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_4H_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_4H_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_4S_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_4S_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_8B_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_8B_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_8H_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2WB_8H_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2_16B, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2_1D, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2_2D, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2_2S, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2_4H, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2_4S, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2_8B, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x2_8H, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_16B_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_16B_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_1D_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_1D_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_2D_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_2D_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_2S_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_2S_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_4H_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_4H_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_4S_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_4S_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_8B_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_8B_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_8H_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3WB_8H_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3_16B, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3_1D, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3_2D, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3_2S, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3_4H, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3_4S, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3_8B, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x3_8H, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_16B_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_16B_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_1D_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_1D_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_2D_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_2D_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_2S_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_2S_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_4H_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_4H_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_4S_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_4S_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_8B_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_8B_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_8H_fixed, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4WB_8H_register, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4_16B, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4_1D, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4_2D, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4_2S, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4_4H, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4_4S, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4_8B, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD1x4_8H, ARM64_INS_LD1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2LN_B, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2LN_D, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2LN_H, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2LN_S, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2LN_WB_B_fixed, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2LN_WB_B_register, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2LN_WB_D_fixed, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2LN_WB_D_register, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2LN_WB_H_fixed, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2LN_WB_H_register, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2LN_WB_S_fixed, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2LN_WB_S_register, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_16B, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_1D, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_2D, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_2S, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_4H, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_4S, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_8B, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_8H, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_16B_fixed, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_16B_register, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_1D_fixed, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_1D_register, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_2D_fixed, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_2D_register, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_2S_fixed, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_2S_register, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_4H_fixed, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_4H_register, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_4S_fixed, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_4S_register, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_8B_fixed, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_8B_register, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_8H_fixed, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2R_WB_8H_register, ARM64_INS_LD2R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2WB_16B_fixed, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2WB_16B_register, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2WB_2D_fixed, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2WB_2D_register, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2WB_2S_fixed, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2WB_2S_register, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2WB_4H_fixed, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2WB_4H_register, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2WB_4S_fixed, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2WB_4S_register, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2WB_8B_fixed, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2WB_8B_register, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2WB_8H_fixed, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2WB_8H_register, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2_16B, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2_2D, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2_2S, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2_4H, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2_4S, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2_8B, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD2_8H, ARM64_INS_LD2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3LN_B, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3LN_D, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3LN_H, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3LN_S, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3LN_WB_B_fixed, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3LN_WB_B_register, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3LN_WB_D_fixed, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3LN_WB_D_register, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3LN_WB_H_fixed, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3LN_WB_H_register, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3LN_WB_S_fixed, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3LN_WB_S_register, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_16B, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_1D, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_2D, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_2S, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_4H, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_4S, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_8B, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_8H, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_16B_fixed, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_16B_register, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_1D_fixed, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_1D_register, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_2D_fixed, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_2D_register, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_2S_fixed, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_2S_register, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_4H_fixed, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_4H_register, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_4S_fixed, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_4S_register, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_8B_fixed, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_8B_register, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_8H_fixed, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3R_WB_8H_register, ARM64_INS_LD3R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3WB_16B_fixed, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3WB_16B_register, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3WB_2D_fixed, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3WB_2D_register, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3WB_2S_fixed, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3WB_2S_register, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3WB_4H_fixed, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3WB_4H_register, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3WB_4S_fixed, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3WB_4S_register, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3WB_8B_fixed, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3WB_8B_register, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3WB_8H_fixed, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3WB_8H_register, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3_16B, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3_2D, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3_2S, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3_4H, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3_4S, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3_8B, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD3_8H, ARM64_INS_LD3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4LN_B, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4LN_D, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4LN_H, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4LN_S, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4LN_WB_B_fixed, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4LN_WB_B_register, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4LN_WB_D_fixed, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4LN_WB_D_register, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4LN_WB_H_fixed, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4LN_WB_H_register, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4LN_WB_S_fixed, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4LN_WB_S_register, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_16B, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_1D, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_2D, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_2S, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_4H, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_4S, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_8B, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_8H, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_16B_fixed, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_16B_register, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_1D_fixed, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_1D_register, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_2D_fixed, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_2D_register, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_2S_fixed, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_2S_register, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_4H_fixed, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_4H_register, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_4S_fixed, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_4S_register, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_8B_fixed, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_8B_register, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_8H_fixed, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4R_WB_8H_register, ARM64_INS_LD4R,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4WB_16B_fixed, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4WB_16B_register, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4WB_2D_fixed, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4WB_2D_register, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4WB_2S_fixed, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4WB_2S_register, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4WB_4H_fixed, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4WB_4H_register, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4WB_4S_fixed, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4WB_4S_register, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4WB_8B_fixed, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4WB_8B_register, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4WB_8H_fixed, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4WB_8H_register, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4_16B, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4_2D, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4_2S, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4_4H, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4_4S, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4_8B, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LD4_8H, ARM64_INS_LD4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_LDAR_byte, ARM64_INS_LDARB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDAR_dword, ARM64_INS_LDAR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDAR_hword, ARM64_INS_LDARH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDAR_word, ARM64_INS_LDAR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDAXP_dword, ARM64_INS_LDAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDAXP_word, ARM64_INS_LDAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDAXR_byte, ARM64_INS_LDAXRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDAXR_dword, ARM64_INS_LDAXR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDAXR_hword, ARM64_INS_LDAXRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDAXR_word, ARM64_INS_LDAXR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDPSWx, ARM64_INS_LDPSW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDPSWx_PostInd, ARM64_INS_LDPSW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDPSWx_PreInd, ARM64_INS_LDPSW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSBw, ARM64_INS_LDRSB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSBw_PostInd, ARM64_INS_LDRSB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSBw_PreInd, ARM64_INS_LDRSB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSBw_U, ARM64_INS_LDURSB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSBw_Wm_RegOffset, ARM64_INS_LDRSB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSBw_Xm_RegOffset, ARM64_INS_LDRSB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSBx, ARM64_INS_LDRSB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSBx_PostInd, ARM64_INS_LDRSB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSBx_PreInd, ARM64_INS_LDRSB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSBx_U, ARM64_INS_LDURSB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSBx_Wm_RegOffset, ARM64_INS_LDRSB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSBx_Xm_RegOffset, ARM64_INS_LDRSB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSHw, ARM64_INS_LDRSH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSHw_PostInd, ARM64_INS_LDRSH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSHw_PreInd, ARM64_INS_LDRSH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSHw_U, ARM64_INS_LDURSH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSHw_Wm_RegOffset, ARM64_INS_LDRSH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSHw_Xm_RegOffset, ARM64_INS_LDRSH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSHx, ARM64_INS_LDRSH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSHx_PostInd, ARM64_INS_LDRSH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSHx_PreInd, ARM64_INS_LDRSH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSHx_U, ARM64_INS_LDURSH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSHx_Wm_RegOffset, ARM64_INS_LDRSH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSHx_Xm_RegOffset, ARM64_INS_LDRSH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSWx, ARM64_INS_LDRSW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSWx_PostInd, ARM64_INS_LDRSW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSWx_PreInd, ARM64_INS_LDRSW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSWx_Wm_RegOffset, ARM64_INS_LDRSW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSWx_Xm_RegOffset, ARM64_INS_LDRSW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRSWx_lit, ARM64_INS_LDRSW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRd_lit, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRq_lit, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRs_lit, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRw_lit, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDRx_lit, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDTRSBw, ARM64_INS_LDTRSB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDTRSBx, ARM64_INS_LDTRSB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDTRSHw, ARM64_INS_LDTRSH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDTRSHx, ARM64_INS_LDTRSH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDTRSWx, ARM64_INS_LDTRSW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDURSWx, ARM64_INS_LDURSW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDXP_dword, ARM64_INS_LDXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDXP_word, ARM64_INS_LDXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDXR_byte, ARM64_INS_LDXRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDXR_dword, ARM64_INS_LDXR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDXR_hword, ARM64_INS_LDXRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LDXR_word, ARM64_INS_LDXR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS16_LDR, ARM64_INS_LDRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS16_LDUR, ARM64_INS_LDURH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS16_PostInd_LDR, ARM64_INS_LDRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS16_PostInd_STR, ARM64_INS_STRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS16_PreInd_LDR, ARM64_INS_LDRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS16_PreInd_STR, ARM64_INS_STRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS16_STR, ARM64_INS_STRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS16_STUR, ARM64_INS_STURH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS16_UnPriv_LDR, ARM64_INS_LDTRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS16_UnPriv_STR, ARM64_INS_STTRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS16_Wm_RegOffset_LDR, ARM64_INS_LDRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS16_Wm_RegOffset_STR, ARM64_INS_STRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS16_Xm_RegOffset_LDR, ARM64_INS_LDRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS16_Xm_RegOffset_STR, ARM64_INS_STRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS32_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS32_LDUR, ARM64_INS_LDUR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS32_PostInd_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS32_PostInd_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS32_PreInd_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS32_PreInd_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS32_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS32_STUR, ARM64_INS_STUR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS32_UnPriv_LDR, ARM64_INS_LDTR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS32_UnPriv_STR, ARM64_INS_STTR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS32_Wm_RegOffset_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS32_Wm_RegOffset_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS32_Xm_RegOffset_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS32_Xm_RegOffset_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS64_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS64_LDUR, ARM64_INS_LDUR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS64_PostInd_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS64_PostInd_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS64_PreInd_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS64_PreInd_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS64_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS64_STUR, ARM64_INS_STUR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS64_UnPriv_LDR, ARM64_INS_LDTR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS64_UnPriv_STR, ARM64_INS_STTR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS64_Wm_RegOffset_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS64_Wm_RegOffset_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS64_Xm_RegOffset_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS64_Xm_RegOffset_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS8_LDR, ARM64_INS_LDRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS8_LDUR, ARM64_INS_LDURB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS8_PostInd_LDR, ARM64_INS_LDRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS8_PostInd_STR, ARM64_INS_STRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS8_PreInd_LDR, ARM64_INS_LDRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS8_PreInd_STR, ARM64_INS_STRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS8_STR, ARM64_INS_STRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS8_STUR, ARM64_INS_STURB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS8_UnPriv_LDR, ARM64_INS_LDTRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS8_UnPriv_STR, ARM64_INS_STTRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS8_Wm_RegOffset_LDR, ARM64_INS_LDRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS8_Wm_RegOffset_STR, ARM64_INS_STRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS8_Xm_RegOffset_LDR, ARM64_INS_LDRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LS8_Xm_RegOffset_STR, ARM64_INS_STRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP128_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP128_LDUR, ARM64_INS_LDUR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP128_PostInd_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP128_PostInd_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP128_PreInd_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP128_PreInd_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP128_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP128_STUR, ARM64_INS_STUR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP128_Wm_RegOffset_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP128_Wm_RegOffset_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP128_Xm_RegOffset_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP128_Xm_RegOffset_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP16_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP16_LDUR, ARM64_INS_LDUR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP16_PostInd_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP16_PostInd_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP16_PreInd_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP16_PreInd_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP16_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP16_STUR, ARM64_INS_STUR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP16_Wm_RegOffset_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP16_Wm_RegOffset_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP16_Xm_RegOffset_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP16_Xm_RegOffset_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP32_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP32_LDUR, ARM64_INS_LDUR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP32_PostInd_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP32_PostInd_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP32_PreInd_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP32_PreInd_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP32_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP32_STUR, ARM64_INS_STUR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP32_Wm_RegOffset_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP32_Wm_RegOffset_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP32_Xm_RegOffset_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP32_Xm_RegOffset_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP64_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP64_LDUR, ARM64_INS_LDUR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP64_PostInd_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP64_PostInd_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP64_PreInd_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP64_PreInd_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP64_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP64_STUR, ARM64_INS_STUR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP64_Wm_RegOffset_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP64_Wm_RegOffset_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP64_Xm_RegOffset_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP64_Xm_RegOffset_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP8_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP8_LDUR, ARM64_INS_LDUR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP8_PostInd_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP8_PostInd_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP8_PreInd_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP8_PreInd_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP8_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP8_STUR, ARM64_INS_STUR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP8_Wm_RegOffset_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP8_Wm_RegOffset_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP8_Xm_RegOffset_LDR, ARM64_INS_LDR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFP8_Xm_RegOffset_STR, ARM64_INS_STR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair128_LDR, ARM64_INS_LDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair128_NonTemp_LDR, ARM64_INS_LDNP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair128_NonTemp_STR, ARM64_INS_STNP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair128_PostInd_LDR, ARM64_INS_LDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair128_PostInd_STR, ARM64_INS_STP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair128_PreInd_LDR, ARM64_INS_LDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair128_PreInd_STR, ARM64_INS_STP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair128_STR, ARM64_INS_STP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair32_LDR, ARM64_INS_LDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair32_NonTemp_LDR, ARM64_INS_LDNP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair32_NonTemp_STR, ARM64_INS_STNP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair32_PostInd_LDR, ARM64_INS_LDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair32_PostInd_STR, ARM64_INS_STP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair32_PreInd_LDR, ARM64_INS_LDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair32_PreInd_STR, ARM64_INS_STP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair32_STR, ARM64_INS_STP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair64_LDR, ARM64_INS_LDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair64_NonTemp_LDR, ARM64_INS_LDNP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair64_NonTemp_STR, ARM64_INS_STNP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair64_PostInd_LDR, ARM64_INS_LDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair64_PostInd_STR, ARM64_INS_STP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair64_PreInd_LDR, ARM64_INS_LDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair64_PreInd_STR, ARM64_INS_STP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSFPPair64_STR, ARM64_INS_STP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_LSLVwww, ARM64_INS_LSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSLVxxx, ARM64_INS_LSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSLwwi, ARM64_INS_LSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSLxxi, ARM64_INS_LSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair32_LDR, ARM64_INS_LDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair32_NonTemp_LDR, ARM64_INS_LDNP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair32_NonTemp_STR, ARM64_INS_STNP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair32_PostInd_LDR, ARM64_INS_LDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair32_PostInd_STR, ARM64_INS_STP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair32_PreInd_LDR, ARM64_INS_LDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair32_PreInd_STR, ARM64_INS_STP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair32_STR, ARM64_INS_STP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair64_LDR, ARM64_INS_LDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair64_NonTemp_LDR, ARM64_INS_LDNP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair64_NonTemp_STR, ARM64_INS_STNP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair64_PostInd_LDR, ARM64_INS_LDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair64_PostInd_STR, ARM64_INS_STP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair64_PreInd_LDR, ARM64_INS_LDP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair64_PreInd_STR, ARM64_INS_STP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSPair64_STR, ARM64_INS_STP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSRVwww, ARM64_INS_LSR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSRVxxx, ARM64_INS_LSR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSRwwi, ARM64_INS_LSR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_LSRxxi, ARM64_INS_LSR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MADDwwww, ARM64_INS_MADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MADDxxxx, ARM64_INS_MADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MLAvve_2s4s, ARM64_INS_MLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLAvve_4h8h, ARM64_INS_MLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLAvve_4s4s, ARM64_INS_MLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLAvve_8h8h, ARM64_INS_MLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLAvvv_16B, ARM64_INS_MLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLAvvv_2S, ARM64_INS_MLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLAvvv_4H, ARM64_INS_MLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLAvvv_4S, ARM64_INS_MLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLAvvv_8B, ARM64_INS_MLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLAvvv_8H, ARM64_INS_MLA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLSvve_2s4s, ARM64_INS_MLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLSvve_4h8h, ARM64_INS_MLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLSvve_4s4s, ARM64_INS_MLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLSvve_8h8h, ARM64_INS_MLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLSvvv_16B, ARM64_INS_MLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLSvvv_2S, ARM64_INS_MLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLSvvv_4H, ARM64_INS_MLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLSvvv_4S, ARM64_INS_MLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLSvvv_8B, ARM64_INS_MLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MLSvvv_8H, ARM64_INS_MLS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVIdi, ARM64_INS_MOVI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVIvi_16B, ARM64_INS_MOVI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVIvi_2D, ARM64_INS_MOVI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVIvi_8B, ARM64_INS_MOVI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVIvi_lsl_2S, ARM64_INS_MOVI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVIvi_lsl_4H, ARM64_INS_MOVI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVIvi_lsl_4S, ARM64_INS_MOVI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVIvi_lsl_8H, ARM64_INS_MOVI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVIvi_msl_2S, ARM64_INS_MOVI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVIvi_msl_4S, ARM64_INS_MOVI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVKwii, ARM64_INS_MOVK,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVKxii, ARM64_INS_MOVK,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVNwii, ARM64_INS_MOVN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVNxii, ARM64_INS_MOVN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVZwii, ARM64_INS_MOVZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MOVZxii, ARM64_INS_MOVZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MRSxi, ARM64_INS_MRS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MSRii, ARM64_INS_MSR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MSRix, ARM64_INS_MSR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MSUBwwww, ARM64_INS_MSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MSUBxxxx, ARM64_INS_MSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MULve_2s4s, ARM64_INS_MUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MULve_4h8h, ARM64_INS_MUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MULve_4s4s, ARM64_INS_MUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MULve_8h8h, ARM64_INS_MUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MULvvv_16B, ARM64_INS_MUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MULvvv_2S, ARM64_INS_MUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MULvvv_4H, ARM64_INS_MUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MULvvv_4S, ARM64_INS_MUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MULvvv_8B, ARM64_INS_MUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MULvvv_8H, ARM64_INS_MUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MVNIvi_lsl_2S, ARM64_INS_MVNI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MVNIvi_lsl_4H, ARM64_INS_MVNI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MVNIvi_lsl_4S, ARM64_INS_MVNI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MVNIvi_lsl_8H, ARM64_INS_MVNI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MVNIvi_msl_2S, ARM64_INS_MVNI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MVNIvi_msl_4S, ARM64_INS_MVNI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_MVNww_asr, ARM64_INS_MVN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MVNww_lsl, ARM64_INS_MVN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MVNww_lsr, ARM64_INS_MVN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MVNww_ror, ARM64_INS_MVN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MVNxx_asr, ARM64_INS_MVN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MVNxx_lsl, ARM64_INS_MVN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MVNxx_lsr, ARM64_INS_MVN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_MVNxx_ror, ARM64_INS_MVN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_NEG16b, ARM64_INS_NEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_NEG2d, ARM64_INS_NEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_NEG2s, ARM64_INS_NEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_NEG4h, ARM64_INS_NEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_NEG4s, ARM64_INS_NEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_NEG8b, ARM64_INS_NEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_NEG8h, ARM64_INS_NEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_NEGdd, ARM64_INS_NEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_NOT16b, ARM64_INS_NOT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_NOT8b, ARM64_INS_NOT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ORNvvv_16B, ARM64_INS_ORN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ORNvvv_8B, ARM64_INS_ORN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ORNwww_asr, ARM64_INS_ORN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORNwww_lsl, ARM64_INS_ORN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORNwww_lsr, ARM64_INS_ORN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORNwww_ror, ARM64_INS_ORN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORNxxx_asr, ARM64_INS_ORN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORNxxx_lsl, ARM64_INS_ORN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORNxxx_lsr, ARM64_INS_ORN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORNxxx_ror, ARM64_INS_ORN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRvi_lsl_2S, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRvi_lsl_4H, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRvi_lsl_4S, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRvi_lsl_8H, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRvvv_16B, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRvvv_8B, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRwwi, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRwww_asr, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRwww_lsl, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRwww_lsr, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRwww_ror, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRxxi, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRxxx_asr, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRxxx_lsl, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRxxx_lsr, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_ORRxxx_ror, ARM64_INS_ORR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_PMULL2vvv_1q2d, ARM64_INS_PMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_PMULL2vvv_8h16b, ARM64_INS_PMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_PMULLvvv_1q1d, ARM64_INS_PMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_PMULLvvv_8h8b, ARM64_INS_PMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_PMULvvv_16B, ARM64_INS_PMUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_PMULvvv_8B, ARM64_INS_PMUL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_PRFM, ARM64_INS_PRFM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_PRFM_Wm_RegOffset, ARM64_INS_PRFM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_PRFM_Xm_RegOffset, ARM64_INS_PRFM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_PRFM_lit, ARM64_INS_PRFM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_PRFUM, ARM64_INS_PRFUM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_QRSHRUNvvi_16B, ARM64_INS_SQRSHRUN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_QRSHRUNvvi_2S, ARM64_INS_SQRSHRUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_QRSHRUNvvi_4H, ARM64_INS_SQRSHRUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_QRSHRUNvvi_4S, ARM64_INS_SQRSHRUN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_QRSHRUNvvi_8B, ARM64_INS_SQRSHRUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_QRSHRUNvvi_8H, ARM64_INS_SQRSHRUN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_QSHRUNvvi_16B, ARM64_INS_SQSHRUN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_QSHRUNvvi_2S, ARM64_INS_SQSHRUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_QSHRUNvvi_4H, ARM64_INS_SQSHRUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_QSHRUNvvi_4S, ARM64_INS_SQSHRUN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_QSHRUNvvi_8B, ARM64_INS_SQSHRUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_QSHRUNvvi_8H, ARM64_INS_SQSHRUN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RADDHN2vvv_16b8h, ARM64_INS_RADDHN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RADDHN2vvv_4s2d, ARM64_INS_RADDHN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RADDHN2vvv_8h4s, ARM64_INS_RADDHN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RADDHNvvv_2s2d, ARM64_INS_RADDHN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RADDHNvvv_4h4s, ARM64_INS_RADDHN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RADDHNvvv_8b8h, ARM64_INS_RADDHN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RBIT16b, ARM64_INS_RBIT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RBIT8b, ARM64_INS_RBIT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RBITww, ARM64_INS_RBIT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_RBITxx, ARM64_INS_RBIT,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_RETx, ARM64_INS_RET,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 1
#endif
	},
	{
		AArch64_REV16_16b, ARM64_INS_REV16,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_REV16_8b, ARM64_INS_REV16,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_REV16ww, ARM64_INS_REV16,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_REV16xx, ARM64_INS_REV16,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_REV32_16b, ARM64_INS_REV32,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_REV32_4h, ARM64_INS_REV32,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_REV32_8b, ARM64_INS_REV32,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_REV32_8h, ARM64_INS_REV32,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_REV32xx, ARM64_INS_REV32,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_REV64_16b, ARM64_INS_REV64,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_REV64_2s, ARM64_INS_REV64,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_REV64_4h, ARM64_INS_REV64,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_REV64_4s, ARM64_INS_REV64,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_REV64_8b, ARM64_INS_REV64,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_REV64_8h, ARM64_INS_REV64,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_REVww, ARM64_INS_REV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_REVxx, ARM64_INS_REV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_RORVwww, ARM64_INS_ROR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_RORVxxx, ARM64_INS_ROR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_RSHRNvvi_16B, ARM64_INS_RSHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RSHRNvvi_2S, ARM64_INS_RSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RSHRNvvi_4H, ARM64_INS_RSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RSHRNvvi_4S, ARM64_INS_RSHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RSHRNvvi_8B, ARM64_INS_RSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RSHRNvvi_8H, ARM64_INS_RSHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RSUBHN2vvv_16b8h, ARM64_INS_RSUBHN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RSUBHN2vvv_4s2d, ARM64_INS_RSUBHN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RSUBHN2vvv_8h4s, ARM64_INS_RSUBHN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RSUBHNvvv_2s2d, ARM64_INS_RSUBHN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RSUBHNvvv_4h4s, ARM64_INS_RSUBHN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_RSUBHNvvv_8b8h, ARM64_INS_RSUBHN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABAL2vvv_2d2s, ARM64_INS_SABAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABAL2vvv_4s4h, ARM64_INS_SABAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABAL2vvv_8h8b, ARM64_INS_SABAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABALvvv_2d2s, ARM64_INS_SABAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABALvvv_4s4h, ARM64_INS_SABAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABALvvv_8h8b, ARM64_INS_SABAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABAvvv_16B, ARM64_INS_SABA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABAvvv_2S, ARM64_INS_SABA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABAvvv_4H, ARM64_INS_SABA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABAvvv_4S, ARM64_INS_SABA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABAvvv_8B, ARM64_INS_SABA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABAvvv_8H, ARM64_INS_SABA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABDL2vvv_2d2s, ARM64_INS_SABDL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABDL2vvv_4s4h, ARM64_INS_SABDL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABDL2vvv_8h8b, ARM64_INS_SABDL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABDLvvv_2d2s, ARM64_INS_SABDL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABDLvvv_4s4h, ARM64_INS_SABDL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABDLvvv_8h8b, ARM64_INS_SABDL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABDvvv_16B, ARM64_INS_SABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABDvvv_2S, ARM64_INS_SABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABDvvv_4H, ARM64_INS_SABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABDvvv_4S, ARM64_INS_SABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABDvvv_8B, ARM64_INS_SABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SABDvvv_8H, ARM64_INS_SABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADALP16b8h, ARM64_INS_SADALP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADALP2s1d, ARM64_INS_SADALP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADALP4h2s, ARM64_INS_SADALP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADALP4s2d, ARM64_INS_SADALP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADALP8b4h, ARM64_INS_SADALP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADALP8h4s, ARM64_INS_SADALP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDL2vvv_2d4s, ARM64_INS_SADDL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDL2vvv_4s8h, ARM64_INS_SADDL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDL2vvv_8h16b, ARM64_INS_SADDL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDLP16b8h, ARM64_INS_SADDLP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDLP2s1d, ARM64_INS_SADDLP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDLP4h2s, ARM64_INS_SADDLP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDLP4s2d, ARM64_INS_SADDLP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDLP8b4h, ARM64_INS_SADDLP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDLP8h4s, ARM64_INS_SADDLP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDLV_1d4s, ARM64_INS_SADDLV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDLV_1h16b, ARM64_INS_SADDLV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDLV_1h8b, ARM64_INS_SADDLV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDLV_1s4h, ARM64_INS_SADDLV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDLV_1s8h, ARM64_INS_SADDLV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDLvvv_2d2s, ARM64_INS_SADDL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDLvvv_4s4h, ARM64_INS_SADDL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDLvvv_8h8b, ARM64_INS_SADDL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDW2vvv_2d4s, ARM64_INS_SADDW2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDW2vvv_4s8h, ARM64_INS_SADDW2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDW2vvv_8h16b, ARM64_INS_SADDW2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDWvvv_2d2s, ARM64_INS_SADDW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDWvvv_4s4h, ARM64_INS_SADDW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SADDWvvv_8h8b, ARM64_INS_SADDW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SBCSwww, ARM64_INS_SBC,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SBCSxxx, ARM64_INS_SBC,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SBCwww, ARM64_INS_SBC,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SBCxxx, ARM64_INS_SBC,
#ifndef CAPSTONE_DIET
		{ ARM64_REG_NZCV, 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SBFIZwwii, ARM64_INS_SBFIZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SBFIZxxii, ARM64_INS_SBFIZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SBFMwwii, ARM64_INS_SBFM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SBFMxxii, ARM64_INS_SBFM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SBFXwwii, ARM64_INS_SBFX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SBFXxxii, ARM64_INS_SBFX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SCVTF_2d, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SCVTF_2s, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SCVTF_4s, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SCVTF_Nddi, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SCVTF_Nssi, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SCVTFdd, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SCVTFdw, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_SCVTFdwi, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_SCVTFdx, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_SCVTFdxi, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_SCVTFss, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SCVTFsw, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_SCVTFswi, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_SCVTFsx, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_SCVTFsxi, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_SDIVwww, ARM64_INS_SDIV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SDIVxxx, ARM64_INS_SDIV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SHA1C, ARM64_INS_SHA1C,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, ARM64_GRP_CRYPTO, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHA1H, ARM64_INS_SHA1H,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, ARM64_GRP_CRYPTO, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHA1M, ARM64_INS_SHA1M,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, ARM64_GRP_CRYPTO, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHA1P, ARM64_INS_SHA1P,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, ARM64_GRP_CRYPTO, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHA1SU0, ARM64_INS_SHA1SU0,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, ARM64_GRP_CRYPTO, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHA1SU1, ARM64_INS_SHA1SU1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, ARM64_GRP_CRYPTO, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHA256H, ARM64_INS_SHA256H,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, ARM64_GRP_CRYPTO, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHA256H2, ARM64_INS_SHA256H2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, ARM64_GRP_CRYPTO, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHA256SU0, ARM64_INS_SHA256SU0,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, ARM64_GRP_CRYPTO, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHA256SU1, ARM64_INS_SHA256SU1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, ARM64_GRP_CRYPTO, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHADDvvv_16B, ARM64_INS_SHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHADDvvv_2S, ARM64_INS_SHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHADDvvv_4H, ARM64_INS_SHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHADDvvv_4S, ARM64_INS_SHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHADDvvv_8B, ARM64_INS_SHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHADDvvv_8H, ARM64_INS_SHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHLL16b8h, ARM64_INS_SHLL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHLL2s2d, ARM64_INS_SHLL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHLL4h4s, ARM64_INS_SHLL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHLL4s2d, ARM64_INS_SHLL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHLL8b8h, ARM64_INS_SHLL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHLL8h4s, ARM64_INS_SHLL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHLddi, ARM64_INS_SHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHLvvi_16B, ARM64_INS_SHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHLvvi_2D, ARM64_INS_SHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHLvvi_2S, ARM64_INS_SHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHLvvi_4H, ARM64_INS_SHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHLvvi_4S, ARM64_INS_SHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHLvvi_8B, ARM64_INS_SHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHLvvi_8H, ARM64_INS_SHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHRNvvi_16B, ARM64_INS_SHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHRNvvi_2S, ARM64_INS_SHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHRNvvi_4H, ARM64_INS_SHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHRNvvi_4S, ARM64_INS_SHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHRNvvi_8B, ARM64_INS_SHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHRNvvi_8H, ARM64_INS_SHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHSUBvvv_16B, ARM64_INS_SHSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHSUBvvv_2S, ARM64_INS_SHSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHSUBvvv_4H, ARM64_INS_SHSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHSUBvvv_4S, ARM64_INS_SHSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHSUBvvv_8B, ARM64_INS_SHSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SHSUBvvv_8H, ARM64_INS_SHSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SLI, ARM64_INS_SLI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SLIvvi_16B, ARM64_INS_SLI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SLIvvi_2D, ARM64_INS_SLI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SLIvvi_2S, ARM64_INS_SLI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SLIvvi_4H, ARM64_INS_SLI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SLIvvi_4S, ARM64_INS_SLI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SLIvvi_8B, ARM64_INS_SLI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SLIvvi_8H, ARM64_INS_SLI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMADDLxwwx, ARM64_INS_SMADDL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXPvvv_16B, ARM64_INS_SMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXPvvv_2S, ARM64_INS_SMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXPvvv_4H, ARM64_INS_SMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXPvvv_4S, ARM64_INS_SMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXPvvv_8B, ARM64_INS_SMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXPvvv_8H, ARM64_INS_SMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXV_1b16b, ARM64_INS_SMAXV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXV_1b8b, ARM64_INS_SMAXV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXV_1h4h, ARM64_INS_SMAXV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXV_1h8h, ARM64_INS_SMAXV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXV_1s4s, ARM64_INS_SMAXV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXvvv_16B, ARM64_INS_SMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXvvv_2S, ARM64_INS_SMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXvvv_4H, ARM64_INS_SMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXvvv_4S, ARM64_INS_SMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXvvv_8B, ARM64_INS_SMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMAXvvv_8H, ARM64_INS_SMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMCi, ARM64_INS_SMC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_SMINPvvv_16B, ARM64_INS_SMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINPvvv_2S, ARM64_INS_SMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINPvvv_4H, ARM64_INS_SMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINPvvv_4S, ARM64_INS_SMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINPvvv_8B, ARM64_INS_SMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINPvvv_8H, ARM64_INS_SMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINV_1b16b, ARM64_INS_SMINV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINV_1b8b, ARM64_INS_SMINV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINV_1h4h, ARM64_INS_SMINV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINV_1h8h, ARM64_INS_SMINV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINV_1s4s, ARM64_INS_SMINV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINvvv_16B, ARM64_INS_SMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINvvv_2S, ARM64_INS_SMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINvvv_4H, ARM64_INS_SMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINvvv_4S, ARM64_INS_SMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINvvv_8B, ARM64_INS_SMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMINvvv_8H, ARM64_INS_SMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLAL2vvv_2d4s, ARM64_INS_SMLAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLAL2vvv_4s8h, ARM64_INS_SMLAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLAL2vvv_8h16b, ARM64_INS_SMLAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLALvve_2d2s, ARM64_INS_SMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLALvve_2d4s, ARM64_INS_SMLAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLALvve_4s4h, ARM64_INS_SMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLALvve_4s8h, ARM64_INS_SMLAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLALvvv_2d2s, ARM64_INS_SMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLALvvv_4s4h, ARM64_INS_SMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLALvvv_8h8b, ARM64_INS_SMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLSL2vvv_2d4s, ARM64_INS_SMLSL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLSL2vvv_4s8h, ARM64_INS_SMLSL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLSL2vvv_8h16b, ARM64_INS_SMLSL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLSLvve_2d2s, ARM64_INS_SMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLSLvve_2d4s, ARM64_INS_SMLSL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLSLvve_4s4h, ARM64_INS_SMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLSLvve_4s8h, ARM64_INS_SMLSL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLSLvvv_2d2s, ARM64_INS_SMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLSLvvv_4s4h, ARM64_INS_SMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMLSLvvv_8h8b, ARM64_INS_SMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMOVwb, ARM64_INS_SMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMOVwh, ARM64_INS_SMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMOVxb, ARM64_INS_SMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMOVxh, ARM64_INS_SMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMOVxs, ARM64_INS_SMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMSUBLxwwx, ARM64_INS_SMSUBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SMULHxxx, ARM64_INS_SMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SMULL2vvv_2d4s, ARM64_INS_SMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMULL2vvv_4s8h, ARM64_INS_SMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMULL2vvv_8h16b, ARM64_INS_SMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMULLve_2d2s, ARM64_INS_SMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMULLve_2d4s, ARM64_INS_SMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMULLve_4s4h, ARM64_INS_SMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMULLve_4s8h, ARM64_INS_SMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMULLvvv_2d2s, ARM64_INS_SMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMULLvvv_4s4h, ARM64_INS_SMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SMULLvvv_8h8b, ARM64_INS_SMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQABS16b, ARM64_INS_SQABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQABS2d, ARM64_INS_SQABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQABS2s, ARM64_INS_SQABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQABS4h, ARM64_INS_SQABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQABS4s, ARM64_INS_SQABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQABS8b, ARM64_INS_SQABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQABS8h, ARM64_INS_SQABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQABSbb, ARM64_INS_SQABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQABSdd, ARM64_INS_SQABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQABShh, ARM64_INS_SQABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQABSss, ARM64_INS_SQABS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQADDbbb, ARM64_INS_SQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQADDddd, ARM64_INS_SQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQADDhhh, ARM64_INS_SQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQADDsss, ARM64_INS_SQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQADDvvv_16B, ARM64_INS_SQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQADDvvv_2D, ARM64_INS_SQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQADDvvv_2S, ARM64_INS_SQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQADDvvv_4H, ARM64_INS_SQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQADDvvv_4S, ARM64_INS_SQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQADDvvv_8B, ARM64_INS_SQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQADDvvv_8H, ARM64_INS_SQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLAL2vvv_2d4s, ARM64_INS_SQDMLAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLAL2vvv_4s8h, ARM64_INS_SQDMLAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLALdss, ARM64_INS_SQDMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLALdsv_2S, ARM64_INS_SQDMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLALdsv_4S, ARM64_INS_SQDMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLALshh, ARM64_INS_SQDMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLALshv_4H, ARM64_INS_SQDMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLALshv_8H, ARM64_INS_SQDMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLALvve_2d2s, ARM64_INS_SQDMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLALvve_2d4s, ARM64_INS_SQDMLAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLALvve_4s4h, ARM64_INS_SQDMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLALvve_4s8h, ARM64_INS_SQDMLAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLALvvv_2d2s, ARM64_INS_SQDMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLALvvv_4s4h, ARM64_INS_SQDMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLSL2vvv_2d4s, ARM64_INS_SQDMLSL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLSL2vvv_4s8h, ARM64_INS_SQDMLSL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLSLdss, ARM64_INS_SQDMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLSLdsv_2S, ARM64_INS_SQDMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLSLdsv_4S, ARM64_INS_SQDMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLSLshh, ARM64_INS_SQDMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLSLshv_4H, ARM64_INS_SQDMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLSLshv_8H, ARM64_INS_SQDMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLSLvve_2d2s, ARM64_INS_SQDMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLSLvve_2d4s, ARM64_INS_SQDMLSL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLSLvve_4s4h, ARM64_INS_SQDMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLSLvve_4s8h, ARM64_INS_SQDMLSL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLSLvvv_2d2s, ARM64_INS_SQDMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMLSLvvv_4s4h, ARM64_INS_SQDMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULHhhh, ARM64_INS_SQDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULHhhv_4H, ARM64_INS_SQDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULHhhv_8H, ARM64_INS_SQDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULHsss, ARM64_INS_SQDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULHssv_2S, ARM64_INS_SQDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULHssv_4S, ARM64_INS_SQDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULHve_2s4s, ARM64_INS_SQDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULHve_4h8h, ARM64_INS_SQDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULHve_4s4s, ARM64_INS_SQDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULHve_8h8h, ARM64_INS_SQDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULHvvv_2S, ARM64_INS_SQDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULHvvv_4H, ARM64_INS_SQDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULHvvv_4S, ARM64_INS_SQDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULHvvv_8H, ARM64_INS_SQDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULL2vvv_2d4s, ARM64_INS_SQDMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULL2vvv_4s8h, ARM64_INS_SQDMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULLdss, ARM64_INS_SQDMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULLdsv_2S, ARM64_INS_SQDMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULLdsv_4S, ARM64_INS_SQDMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULLshh, ARM64_INS_SQDMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULLshv_4H, ARM64_INS_SQDMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULLshv_8H, ARM64_INS_SQDMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULLve_2d2s, ARM64_INS_SQDMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULLve_2d4s, ARM64_INS_SQDMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULLve_4s4h, ARM64_INS_SQDMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULLve_4s8h, ARM64_INS_SQDMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULLvvv_2d2s, ARM64_INS_SQDMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQDMULLvvv_4s4h, ARM64_INS_SQDMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQNEG16b, ARM64_INS_SQNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQNEG2d, ARM64_INS_SQNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQNEG2s, ARM64_INS_SQNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQNEG4h, ARM64_INS_SQNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQNEG4s, ARM64_INS_SQNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQNEG8b, ARM64_INS_SQNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQNEG8h, ARM64_INS_SQNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQNEGbb, ARM64_INS_SQNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQNEGdd, ARM64_INS_SQNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQNEGhh, ARM64_INS_SQNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQNEGss, ARM64_INS_SQNEG,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRDMULHhhh, ARM64_INS_SQRDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRDMULHhhv_4H, ARM64_INS_SQRDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRDMULHhhv_8H, ARM64_INS_SQRDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRDMULHsss, ARM64_INS_SQRDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRDMULHssv_2S, ARM64_INS_SQRDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRDMULHssv_4S, ARM64_INS_SQRDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRDMULHve_2s4s, ARM64_INS_SQRDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRDMULHve_4h8h, ARM64_INS_SQRDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRDMULHve_4s4s, ARM64_INS_SQRDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRDMULHve_8h8h, ARM64_INS_SQRDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRDMULHvvv_2S, ARM64_INS_SQRDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRDMULHvvv_4H, ARM64_INS_SQRDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRDMULHvvv_4S, ARM64_INS_SQRDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRDMULHvvv_8H, ARM64_INS_SQRDMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHLbbb, ARM64_INS_SQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHLddd, ARM64_INS_SQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHLhhh, ARM64_INS_SQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHLsss, ARM64_INS_SQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHLvvv_16B, ARM64_INS_SQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHLvvv_2D, ARM64_INS_SQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHLvvv_2S, ARM64_INS_SQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHLvvv_4H, ARM64_INS_SQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHLvvv_4S, ARM64_INS_SQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHLvvv_8B, ARM64_INS_SQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHLvvv_8H, ARM64_INS_SQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHRNbhi, ARM64_INS_SQRSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHRNhsi, ARM64_INS_SQRSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHRNsdi, ARM64_INS_SQRSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHRNvvi_16B, ARM64_INS_SQRSHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHRNvvi_2S, ARM64_INS_SQRSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHRNvvi_4H, ARM64_INS_SQRSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHRNvvi_4S, ARM64_INS_SQRSHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHRNvvi_8B, ARM64_INS_SQRSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHRNvvi_8H, ARM64_INS_SQRSHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHRUNbhi, ARM64_INS_SQRSHRUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHRUNhsi, ARM64_INS_SQRSHRUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQRSHRUNsdi, ARM64_INS_SQRSHRUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLUbbi, ARM64_INS_SQSHLU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLUddi, ARM64_INS_SQSHLU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLUhhi, ARM64_INS_SQSHLU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLUssi, ARM64_INS_SQSHLU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLUvvi_16B, ARM64_INS_SQSHLU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLUvvi_2D, ARM64_INS_SQSHLU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLUvvi_2S, ARM64_INS_SQSHLU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLUvvi_4H, ARM64_INS_SQSHLU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLUvvi_4S, ARM64_INS_SQSHLU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLUvvi_8B, ARM64_INS_SQSHLU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLUvvi_8H, ARM64_INS_SQSHLU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLbbb, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLbbi, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLddd, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLddi, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLhhh, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLhhi, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLssi, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLsss, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLvvi_16B, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLvvi_2D, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLvvi_2S, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLvvi_4H, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLvvi_4S, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLvvi_8B, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLvvi_8H, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLvvv_16B, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLvvv_2D, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLvvv_2S, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLvvv_4H, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLvvv_4S, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLvvv_8B, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHLvvv_8H, ARM64_INS_SQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHRNbhi, ARM64_INS_SQSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHRNhsi, ARM64_INS_SQSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHRNsdi, ARM64_INS_SQSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHRNvvi_16B, ARM64_INS_SQSHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHRNvvi_2S, ARM64_INS_SQSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHRNvvi_4H, ARM64_INS_SQSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHRNvvi_4S, ARM64_INS_SQSHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHRNvvi_8B, ARM64_INS_SQSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHRNvvi_8H, ARM64_INS_SQSHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHRUNbhi, ARM64_INS_SQSHRUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHRUNhsi, ARM64_INS_SQSHRUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSHRUNsdi, ARM64_INS_SQSHRUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSUBbbb, ARM64_INS_SQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSUBddd, ARM64_INS_SQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSUBhhh, ARM64_INS_SQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSUBsss, ARM64_INS_SQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSUBvvv_16B, ARM64_INS_SQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSUBvvv_2D, ARM64_INS_SQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSUBvvv_2S, ARM64_INS_SQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSUBvvv_4H, ARM64_INS_SQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSUBvvv_4S, ARM64_INS_SQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSUBvvv_8B, ARM64_INS_SQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQSUBvvv_8H, ARM64_INS_SQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTN2d2s, ARM64_INS_SQXTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTN2d4s, ARM64_INS_SQXTN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTN4s4h, ARM64_INS_SQXTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTN4s8h, ARM64_INS_SQXTN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTN8h16b, ARM64_INS_SQXTN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTN8h8b, ARM64_INS_SQXTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTNbh, ARM64_INS_SQXTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTNhs, ARM64_INS_SQXTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTNsd, ARM64_INS_SQXTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTUN2d2s, ARM64_INS_SQXTUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTUN2d4s, ARM64_INS_SQXTUN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTUN4s4h, ARM64_INS_SQXTUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTUN4s8h, ARM64_INS_SQXTUN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTUN8h16b, ARM64_INS_SQXTUN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTUN8h8b, ARM64_INS_SQXTUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTUNbh, ARM64_INS_SQXTUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTUNhs, ARM64_INS_SQXTUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SQXTUNsd, ARM64_INS_SQXTUN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRHADDvvv_16B, ARM64_INS_SRHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRHADDvvv_2S, ARM64_INS_SRHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRHADDvvv_4H, ARM64_INS_SRHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRHADDvvv_4S, ARM64_INS_SRHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRHADDvvv_8B, ARM64_INS_SRHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRHADDvvv_8H, ARM64_INS_SRHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRI, ARM64_INS_SRI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRIvvi_16B, ARM64_INS_SRI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRIvvi_2D, ARM64_INS_SRI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRIvvi_2S, ARM64_INS_SRI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRIvvi_4H, ARM64_INS_SRI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRIvvi_4S, ARM64_INS_SRI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRIvvi_8B, ARM64_INS_SRI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRIvvi_8H, ARM64_INS_SRI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHLddd, ARM64_INS_SRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHLvvv_16B, ARM64_INS_SRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHLvvv_2D, ARM64_INS_SRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHLvvv_2S, ARM64_INS_SRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHLvvv_4H, ARM64_INS_SRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHLvvv_4S, ARM64_INS_SRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHLvvv_8B, ARM64_INS_SRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHLvvv_8H, ARM64_INS_SRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHRddi, ARM64_INS_SRSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHRvvi_16B, ARM64_INS_SRSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHRvvi_2D, ARM64_INS_SRSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHRvvi_2S, ARM64_INS_SRSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHRvvi_4H, ARM64_INS_SRSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHRvvi_4S, ARM64_INS_SRSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHRvvi_8B, ARM64_INS_SRSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSHRvvi_8H, ARM64_INS_SRSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSRA, ARM64_INS_SRSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSRAvvi_16B, ARM64_INS_SRSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSRAvvi_2D, ARM64_INS_SRSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSRAvvi_2S, ARM64_INS_SRSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSRAvvi_4H, ARM64_INS_SRSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSRAvvi_4S, ARM64_INS_SRSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSRAvvi_8B, ARM64_INS_SRSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SRSRAvvi_8H, ARM64_INS_SRSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHLLvvi_16B, ARM64_INS_SSHLL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHLLvvi_2S, ARM64_INS_SSHLL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHLLvvi_4H, ARM64_INS_SSHLL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHLLvvi_4S, ARM64_INS_SSHLL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHLLvvi_8B, ARM64_INS_SSHLL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHLLvvi_8H, ARM64_INS_SSHLL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHLddd, ARM64_INS_SSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHLvvv_16B, ARM64_INS_SSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHLvvv_2D, ARM64_INS_SSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHLvvv_2S, ARM64_INS_SSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHLvvv_4H, ARM64_INS_SSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHLvvv_4S, ARM64_INS_SSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHLvvv_8B, ARM64_INS_SSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHLvvv_8H, ARM64_INS_SSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHRddi, ARM64_INS_SSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHRvvi_16B, ARM64_INS_SSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHRvvi_2D, ARM64_INS_SSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHRvvi_2S, ARM64_INS_SSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHRvvi_4H, ARM64_INS_SSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHRvvi_4S, ARM64_INS_SSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHRvvi_8B, ARM64_INS_SSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSHRvvi_8H, ARM64_INS_SSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSRA, ARM64_INS_SSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSRAvvi_16B, ARM64_INS_SSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSRAvvi_2D, ARM64_INS_SSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSRAvvi_2S, ARM64_INS_SSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSRAvvi_4H, ARM64_INS_SSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSRAvvi_4S, ARM64_INS_SSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSRAvvi_8B, ARM64_INS_SSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSRAvvi_8H, ARM64_INS_SSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSUBL2vvv_2d4s, ARM64_INS_SSUBL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSUBL2vvv_4s8h, ARM64_INS_SSUBL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSUBL2vvv_8h16b, ARM64_INS_SSUBL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSUBLvvv_2d2s, ARM64_INS_SSUBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSUBLvvv_4s4h, ARM64_INS_SSUBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSUBLvvv_8h8b, ARM64_INS_SSUBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSUBW2vvv_2d4s, ARM64_INS_SSUBW2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSUBW2vvv_4s8h, ARM64_INS_SSUBW2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSUBW2vvv_8h16b, ARM64_INS_SSUBW2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSUBWvvv_2d2s, ARM64_INS_SSUBW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSUBWvvv_4s4h, ARM64_INS_SSUBW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SSUBWvvv_8h8b, ARM64_INS_SSUBW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1LN_B, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1LN_D, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1LN_H, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1LN_S, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1LN_WB_B_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1LN_WB_B_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1LN_WB_D_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1LN_WB_D_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1LN_WB_H_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1LN_WB_H_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1LN_WB_S_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1LN_WB_S_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_16B_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_16B_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_1D_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_1D_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_2D_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_2D_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_2S_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_2S_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_4H_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_4H_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_4S_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_4S_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_8B_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_8B_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_8H_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1WB_8H_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1_16B, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1_1D, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1_2D, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1_2S, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1_4H, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1_4S, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1_8B, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1_8H, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_16B_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_16B_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_1D_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_1D_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_2D_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_2D_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_2S_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_2S_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_4H_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_4H_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_4S_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_4S_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_8B_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_8B_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_8H_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2WB_8H_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2_16B, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2_1D, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2_2D, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2_2S, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2_4H, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2_4S, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2_8B, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x2_8H, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_16B_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_16B_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_1D_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_1D_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_2D_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_2D_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_2S_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_2S_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_4H_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_4H_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_4S_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_4S_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_8B_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_8B_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_8H_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3WB_8H_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3_16B, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3_1D, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3_2D, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3_2S, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3_4H, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3_4S, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3_8B, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x3_8H, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_16B_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_16B_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_1D_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_1D_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_2D_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_2D_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_2S_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_2S_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_4H_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_4H_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_4S_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_4S_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_8B_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_8B_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_8H_fixed, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4WB_8H_register, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4_16B, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4_1D, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4_2D, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4_2S, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4_4H, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4_4S, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4_8B, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST1x4_8H, ARM64_INS_ST1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2LN_B, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2LN_D, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2LN_H, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2LN_S, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2LN_WB_B_fixed, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2LN_WB_B_register, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2LN_WB_D_fixed, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2LN_WB_D_register, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2LN_WB_H_fixed, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2LN_WB_H_register, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2LN_WB_S_fixed, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2LN_WB_S_register, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2WB_16B_fixed, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2WB_16B_register, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2WB_2D_fixed, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2WB_2D_register, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2WB_2S_fixed, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2WB_2S_register, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2WB_4H_fixed, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2WB_4H_register, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2WB_4S_fixed, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2WB_4S_register, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2WB_8B_fixed, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2WB_8B_register, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2WB_8H_fixed, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2WB_8H_register, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2_16B, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2_2D, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2_2S, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2_4H, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2_4S, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2_8B, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST2_8H, ARM64_INS_ST2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3LN_B, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3LN_D, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3LN_H, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3LN_S, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3LN_WB_B_fixed, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3LN_WB_B_register, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3LN_WB_D_fixed, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3LN_WB_D_register, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3LN_WB_H_fixed, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3LN_WB_H_register, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3LN_WB_S_fixed, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3LN_WB_S_register, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3WB_16B_fixed, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3WB_16B_register, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3WB_2D_fixed, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3WB_2D_register, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3WB_2S_fixed, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3WB_2S_register, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3WB_4H_fixed, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3WB_4H_register, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3WB_4S_fixed, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3WB_4S_register, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3WB_8B_fixed, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3WB_8B_register, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3WB_8H_fixed, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3WB_8H_register, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3_16B, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3_2D, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3_2S, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3_4H, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3_4S, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3_8B, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST3_8H, ARM64_INS_ST3,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4LN_B, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4LN_D, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4LN_H, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4LN_S, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4LN_WB_B_fixed, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4LN_WB_B_register, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4LN_WB_D_fixed, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4LN_WB_D_register, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4LN_WB_H_fixed, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4LN_WB_H_register, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4LN_WB_S_fixed, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4LN_WB_S_register, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4WB_16B_fixed, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4WB_16B_register, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4WB_2D_fixed, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4WB_2D_register, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4WB_2S_fixed, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4WB_2S_register, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4WB_4H_fixed, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4WB_4H_register, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4WB_4S_fixed, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4WB_4S_register, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4WB_8B_fixed, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4WB_8B_register, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4WB_8H_fixed, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4WB_8H_register, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4_16B, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4_2D, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4_2S, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4_4H, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4_4S, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4_8B, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ST4_8H, ARM64_INS_ST4,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_STLR_byte, ARM64_INS_STLRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_STLR_dword, ARM64_INS_STLR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_STLR_hword, ARM64_INS_STLRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_STLR_word, ARM64_INS_STLR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_STLXP_dword, ARM64_INS_STLXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_STLXP_word, ARM64_INS_STLXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_STLXR_byte, ARM64_INS_STLXRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_STLXR_dword, ARM64_INS_STLXR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_STLXR_hword, ARM64_INS_STLXRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_STLXR_word, ARM64_INS_STLXR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_STXP_dword, ARM64_INS_STXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_STXP_word, ARM64_INS_STXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_STXR_byte, ARM64_INS_STXRB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_STXR_dword, ARM64_INS_STXR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_STXR_hword, ARM64_INS_STXRH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_STXR_word, ARM64_INS_STXR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBHN2vvv_16b8h, ARM64_INS_SUBHN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBHN2vvv_4s2d, ARM64_INS_SUBHN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBHN2vvv_8h4s, ARM64_INS_SUBHN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBHNvvv_2s2d, ARM64_INS_SUBHN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBHNvvv_4h4s, ARM64_INS_SUBHN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBHNvvv_8b8h, ARM64_INS_SUBHN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSwww_asr, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSwww_lsl, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSwww_lsr, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSwww_sxtb, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSwww_sxth, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSwww_sxtw, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSwww_sxtx, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSwww_uxtb, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSwww_uxth, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSwww_uxtw, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSwww_uxtx, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSxxw_sxtb, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSxxw_sxth, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSxxw_sxtw, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSxxw_uxtb, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSxxw_uxth, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSxxw_uxtw, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSxxx_asr, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSxxx_lsl, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSxxx_lsr, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSxxx_sxtx, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBSxxx_uxtx, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBddd, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBvvv_16B, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBvvv_2D, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBvvv_2S, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBvvv_4H, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBvvv_4S, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBvvv_8B, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBvvv_8H, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwwi_lsl0_S, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwwi_lsl0_cmp, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwwi_lsl0_s, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwwi_lsl12_S, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwwi_lsl12_cmp, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwwi_lsl12_s, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwww_asr, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwww_lsl, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwww_lsr, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwww_sxtb, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwww_sxth, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwww_sxtw, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwww_sxtx, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwww_uxtb, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwww_uxth, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwww_uxtw, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBwww_uxtx, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxi_lsl0_S, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxi_lsl0_cmp, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxi_lsl0_s, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxi_lsl12_S, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxi_lsl12_cmp, ARM64_INS_CMP,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxi_lsl12_s, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxw_sxtb, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxw_sxth, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxw_sxtw, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxw_uxtb, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxw_uxth, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxw_uxtw, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxx_asr, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxx_lsl, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxx_lsr, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxx_sxtx, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUBxxx_uxtx, ARM64_INS_SUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SUQADD16b, ARM64_INS_SUQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUQADD2d, ARM64_INS_SUQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUQADD2s, ARM64_INS_SUQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUQADD4h, ARM64_INS_SUQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUQADD4s, ARM64_INS_SUQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUQADD8b, ARM64_INS_SUQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUQADD8h, ARM64_INS_SUQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUQADDbb, ARM64_INS_SUQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUQADDdd, ARM64_INS_SUQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUQADDhh, ARM64_INS_SUQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SUQADDss, ARM64_INS_SUQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_SVCi, ARM64_INS_SVC,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_SXTBww, ARM64_INS_SXTB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SXTBxw, ARM64_INS_SXTB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SXTHww, ARM64_INS_SXTH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SXTHxw, ARM64_INS_SXTH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SXTWxw, ARM64_INS_SXTW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SYSLxicci, ARM64_INS_SYSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_SYSiccix, ARM64_INS_SYS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_TBL1_16b, ARM64_INS_TBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBL1_8b, ARM64_INS_TBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBL2_16b, ARM64_INS_TBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBL2_8b, ARM64_INS_TBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBL3_16b, ARM64_INS_TBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBL3_8b, ARM64_INS_TBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBL4_16b, ARM64_INS_TBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBL4_8b, ARM64_INS_TBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBNZwii, ARM64_INS_TBNZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_TBNZxii, ARM64_INS_TBNZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_TBX1_16b, ARM64_INS_TBX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBX1_8b, ARM64_INS_TBX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBX2_16b, ARM64_INS_TBX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBX2_8b, ARM64_INS_TBX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBX3_16b, ARM64_INS_TBX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBX3_8b, ARM64_INS_TBX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBX4_16b, ARM64_INS_TBX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBX4_8b, ARM64_INS_TBX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TBZwii, ARM64_INS_TBZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_TBZxii, ARM64_INS_TBZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 1, 0
#endif
	},
	{
		AArch64_TLBIi, ARM64_INS_TLBI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_TLBIix, ARM64_INS_TLBI,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_TRN1vvv_16b, ARM64_INS_TRN1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TRN1vvv_2d, ARM64_INS_TRN1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TRN1vvv_2s, ARM64_INS_TRN1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TRN1vvv_4h, ARM64_INS_TRN1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TRN1vvv_4s, ARM64_INS_TRN1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TRN1vvv_8b, ARM64_INS_TRN1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TRN1vvv_8h, ARM64_INS_TRN1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TRN2vvv_16b, ARM64_INS_TRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TRN2vvv_2d, ARM64_INS_TRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TRN2vvv_2s, ARM64_INS_TRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TRN2vvv_4h, ARM64_INS_TRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TRN2vvv_4s, ARM64_INS_TRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TRN2vvv_8b, ARM64_INS_TRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TRN2vvv_8h, ARM64_INS_TRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_TSTww_asr, ARM64_INS_TST,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_TSTww_lsl, ARM64_INS_TST,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_TSTww_lsr, ARM64_INS_TST,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_TSTww_ror, ARM64_INS_TST,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_TSTxx_asr, ARM64_INS_TST,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_TSTxx_lsl, ARM64_INS_TST,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_TSTxx_lsr, ARM64_INS_TST,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_TSTxx_ror, ARM64_INS_TST,
#ifndef CAPSTONE_DIET
		{ 0 }, { ARM64_REG_NZCV, 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UABAL2vvv_2d2s, ARM64_INS_UABAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABAL2vvv_4s4h, ARM64_INS_UABAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABAL2vvv_8h8b, ARM64_INS_UABAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABALvvv_2d2s, ARM64_INS_UABAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABALvvv_4s4h, ARM64_INS_UABAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABALvvv_8h8b, ARM64_INS_UABAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABAvvv_16B, ARM64_INS_UABA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABAvvv_2S, ARM64_INS_UABA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABAvvv_4H, ARM64_INS_UABA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABAvvv_4S, ARM64_INS_UABA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABAvvv_8B, ARM64_INS_UABA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABAvvv_8H, ARM64_INS_UABA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABDL2vvv_2d2s, ARM64_INS_UABDL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABDL2vvv_4s4h, ARM64_INS_UABDL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABDL2vvv_8h8b, ARM64_INS_UABDL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABDLvvv_2d2s, ARM64_INS_UABDL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABDLvvv_4s4h, ARM64_INS_UABDL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABDLvvv_8h8b, ARM64_INS_UABDL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABDvvv_16B, ARM64_INS_UABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABDvvv_2S, ARM64_INS_UABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABDvvv_4H, ARM64_INS_UABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABDvvv_4S, ARM64_INS_UABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABDvvv_8B, ARM64_INS_UABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UABDvvv_8H, ARM64_INS_UABD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADALP16b8h, ARM64_INS_UADALP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADALP2s1d, ARM64_INS_UADALP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADALP4h2s, ARM64_INS_UADALP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADALP4s2d, ARM64_INS_UADALP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADALP8b4h, ARM64_INS_UADALP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADALP8h4s, ARM64_INS_UADALP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDL2vvv_2d4s, ARM64_INS_UADDL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDL2vvv_4s8h, ARM64_INS_UADDL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDL2vvv_8h16b, ARM64_INS_UADDL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDLP16b8h, ARM64_INS_UADDLP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDLP2s1d, ARM64_INS_UADDLP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDLP4h2s, ARM64_INS_UADDLP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDLP4s2d, ARM64_INS_UADDLP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDLP8b4h, ARM64_INS_UADDLP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDLP8h4s, ARM64_INS_UADDLP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDLV_1d4s, ARM64_INS_UADDLV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDLV_1h16b, ARM64_INS_UADDLV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDLV_1h8b, ARM64_INS_UADDLV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDLV_1s4h, ARM64_INS_UADDLV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDLV_1s8h, ARM64_INS_UADDLV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDLvvv_2d2s, ARM64_INS_UADDL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDLvvv_4s4h, ARM64_INS_UADDL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDLvvv_8h8b, ARM64_INS_UADDL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDW2vvv_2d4s, ARM64_INS_UADDW2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDW2vvv_4s8h, ARM64_INS_UADDW2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDW2vvv_8h16b, ARM64_INS_UADDW2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDWvvv_2d2s, ARM64_INS_UADDW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDWvvv_4s4h, ARM64_INS_UADDW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UADDWvvv_8h8b, ARM64_INS_UADDW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UBFIZwwii, ARM64_INS_UBFIZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UBFIZxxii, ARM64_INS_UBFIZ,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UBFMwwii, ARM64_INS_UBFM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UBFMxxii, ARM64_INS_UBFM,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UBFXwwii, ARM64_INS_UBFX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UBFXxxii, ARM64_INS_UBFX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UCVTF_2d, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UCVTF_2s, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UCVTF_4s, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UCVTF_Nddi, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UCVTF_Nssi, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UCVTFdd, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UCVTFdw, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_UCVTFdwi, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_UCVTFdx, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_UCVTFdxi, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_UCVTFss, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UCVTFsw, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_UCVTFswi, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_UCVTFsx, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_UCVTFsxi, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_FPARMV8, 0 }, 0, 0
#endif
	},
	{
		AArch64_UDIVwww, ARM64_INS_UDIV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UDIVxxx, ARM64_INS_UDIV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UHADDvvv_16B, ARM64_INS_UHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UHADDvvv_2S, ARM64_INS_UHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UHADDvvv_4H, ARM64_INS_UHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UHADDvvv_4S, ARM64_INS_UHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UHADDvvv_8B, ARM64_INS_UHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UHADDvvv_8H, ARM64_INS_UHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UHSUBvvv_16B, ARM64_INS_UHSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UHSUBvvv_2S, ARM64_INS_UHSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UHSUBvvv_4H, ARM64_INS_UHSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UHSUBvvv_4S, ARM64_INS_UHSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UHSUBvvv_8B, ARM64_INS_UHSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UHSUBvvv_8H, ARM64_INS_UHSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMADDLxwwx, ARM64_INS_UMADDL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXPvvv_16B, ARM64_INS_UMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXPvvv_2S, ARM64_INS_UMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXPvvv_4H, ARM64_INS_UMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXPvvv_4S, ARM64_INS_UMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXPvvv_8B, ARM64_INS_UMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXPvvv_8H, ARM64_INS_UMAXP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXV_1b16b, ARM64_INS_UMAXV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXV_1b8b, ARM64_INS_UMAXV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXV_1h4h, ARM64_INS_UMAXV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXV_1h8h, ARM64_INS_UMAXV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXV_1s4s, ARM64_INS_UMAXV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXvvv_16B, ARM64_INS_UMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXvvv_2S, ARM64_INS_UMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXvvv_4H, ARM64_INS_UMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXvvv_4S, ARM64_INS_UMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXvvv_8B, ARM64_INS_UMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMAXvvv_8H, ARM64_INS_UMAX,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINPvvv_16B, ARM64_INS_UMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINPvvv_2S, ARM64_INS_UMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINPvvv_4H, ARM64_INS_UMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINPvvv_4S, ARM64_INS_UMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINPvvv_8B, ARM64_INS_UMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINPvvv_8H, ARM64_INS_UMINP,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINV_1b16b, ARM64_INS_UMINV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINV_1b8b, ARM64_INS_UMINV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINV_1h4h, ARM64_INS_UMINV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINV_1h8h, ARM64_INS_UMINV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINV_1s4s, ARM64_INS_UMINV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINvvv_16B, ARM64_INS_UMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINvvv_2S, ARM64_INS_UMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINvvv_4H, ARM64_INS_UMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINvvv_4S, ARM64_INS_UMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINvvv_8B, ARM64_INS_UMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMINvvv_8H, ARM64_INS_UMIN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLAL2vvv_2d4s, ARM64_INS_UMLAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLAL2vvv_4s8h, ARM64_INS_UMLAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLAL2vvv_8h16b, ARM64_INS_UMLAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLALvve_2d2s, ARM64_INS_UMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLALvve_2d4s, ARM64_INS_UMLAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLALvve_4s4h, ARM64_INS_UMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLALvve_4s8h, ARM64_INS_UMLAL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLALvvv_2d2s, ARM64_INS_UMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLALvvv_4s4h, ARM64_INS_UMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLALvvv_8h8b, ARM64_INS_UMLAL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLSL2vvv_2d4s, ARM64_INS_UMLSL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLSL2vvv_4s8h, ARM64_INS_UMLSL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLSL2vvv_8h16b, ARM64_INS_UMLSL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLSLvve_2d2s, ARM64_INS_UMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLSLvve_2d4s, ARM64_INS_UMLSL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLSLvve_4s4h, ARM64_INS_UMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLSLvve_4s8h, ARM64_INS_UMLSL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLSLvvv_2d2s, ARM64_INS_UMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLSLvvv_4s4h, ARM64_INS_UMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMLSLvvv_8h8b, ARM64_INS_UMLSL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMOVwb, ARM64_INS_UMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMOVwh, ARM64_INS_UMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMOVws, ARM64_INS_UMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMOVxd, ARM64_INS_UMOV,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMSUBLxwwx, ARM64_INS_UMSUBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UMULHxxx, ARM64_INS_UMULH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UMULL2vvv_2d4s, ARM64_INS_UMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMULL2vvv_4s8h, ARM64_INS_UMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMULL2vvv_8h16b, ARM64_INS_UMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMULLve_2d2s, ARM64_INS_UMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMULLve_2d4s, ARM64_INS_UMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMULLve_4s4h, ARM64_INS_UMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMULLve_4s8h, ARM64_INS_UMULL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMULLvvv_2d2s, ARM64_INS_UMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMULLvvv_4s4h, ARM64_INS_UMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UMULLvvv_8h8b, ARM64_INS_UMULL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQADDbbb, ARM64_INS_UQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQADDddd, ARM64_INS_UQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQADDhhh, ARM64_INS_UQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQADDsss, ARM64_INS_UQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQADDvvv_16B, ARM64_INS_UQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQADDvvv_2D, ARM64_INS_UQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQADDvvv_2S, ARM64_INS_UQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQADDvvv_4H, ARM64_INS_UQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQADDvvv_4S, ARM64_INS_UQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQADDvvv_8B, ARM64_INS_UQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQADDvvv_8H, ARM64_INS_UQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHLbbb, ARM64_INS_UQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHLddd, ARM64_INS_UQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHLhhh, ARM64_INS_UQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHLsss, ARM64_INS_UQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHLvvv_16B, ARM64_INS_UQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHLvvv_2D, ARM64_INS_UQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHLvvv_2S, ARM64_INS_UQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHLvvv_4H, ARM64_INS_UQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHLvvv_4S, ARM64_INS_UQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHLvvv_8B, ARM64_INS_UQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHLvvv_8H, ARM64_INS_UQRSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHRNbhi, ARM64_INS_UQRSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHRNhsi, ARM64_INS_UQRSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHRNsdi, ARM64_INS_UQRSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHRNvvi_16B, ARM64_INS_UQRSHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHRNvvi_2S, ARM64_INS_UQRSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHRNvvi_4H, ARM64_INS_UQRSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHRNvvi_4S, ARM64_INS_UQRSHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHRNvvi_8B, ARM64_INS_UQRSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQRSHRNvvi_8H, ARM64_INS_UQRSHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLbbb, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLbbi, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLddd, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLddi, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLhhh, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLhhi, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLssi, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLsss, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLvvi_16B, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLvvi_2D, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLvvi_2S, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLvvi_4H, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLvvi_4S, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLvvi_8B, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLvvi_8H, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLvvv_16B, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLvvv_2D, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLvvv_2S, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLvvv_4H, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLvvv_4S, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLvvv_8B, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHLvvv_8H, ARM64_INS_UQSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHRNbhi, ARM64_INS_UQSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHRNhsi, ARM64_INS_UQSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHRNsdi, ARM64_INS_UQSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHRNvvi_16B, ARM64_INS_UQSHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHRNvvi_2S, ARM64_INS_UQSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHRNvvi_4H, ARM64_INS_UQSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHRNvvi_4S, ARM64_INS_UQSHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHRNvvi_8B, ARM64_INS_UQSHRN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSHRNvvi_8H, ARM64_INS_UQSHRN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSUBbbb, ARM64_INS_UQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSUBddd, ARM64_INS_UQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSUBhhh, ARM64_INS_UQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSUBsss, ARM64_INS_UQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSUBvvv_16B, ARM64_INS_UQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSUBvvv_2D, ARM64_INS_UQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSUBvvv_2S, ARM64_INS_UQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSUBvvv_4H, ARM64_INS_UQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSUBvvv_4S, ARM64_INS_UQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSUBvvv_8B, ARM64_INS_UQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQSUBvvv_8H, ARM64_INS_UQSUB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQXTN2d2s, ARM64_INS_UQXTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQXTN2d4s, ARM64_INS_UQXTN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQXTN4s4h, ARM64_INS_UQXTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQXTN4s8h, ARM64_INS_UQXTN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQXTN8h16b, ARM64_INS_UQXTN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQXTN8h8b, ARM64_INS_UQXTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQXTNbh, ARM64_INS_UQXTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQXTNhs, ARM64_INS_UQXTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UQXTNsd, ARM64_INS_UQXTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URECPE2s, ARM64_INS_URECPE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URECPE4s, ARM64_INS_URECPE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URHADDvvv_16B, ARM64_INS_URHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URHADDvvv_2S, ARM64_INS_URHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URHADDvvv_4H, ARM64_INS_URHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URHADDvvv_4S, ARM64_INS_URHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URHADDvvv_8B, ARM64_INS_URHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URHADDvvv_8H, ARM64_INS_URHADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHLddd, ARM64_INS_URSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHLvvv_16B, ARM64_INS_URSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHLvvv_2D, ARM64_INS_URSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHLvvv_2S, ARM64_INS_URSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHLvvv_4H, ARM64_INS_URSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHLvvv_4S, ARM64_INS_URSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHLvvv_8B, ARM64_INS_URSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHLvvv_8H, ARM64_INS_URSHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHRddi, ARM64_INS_URSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHRvvi_16B, ARM64_INS_URSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHRvvi_2D, ARM64_INS_URSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHRvvi_2S, ARM64_INS_URSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHRvvi_4H, ARM64_INS_URSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHRvvi_4S, ARM64_INS_URSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHRvvi_8B, ARM64_INS_URSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSHRvvi_8H, ARM64_INS_URSHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSQRTE2s, ARM64_INS_URSQRTE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSQRTE4s, ARM64_INS_URSQRTE,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSRA, ARM64_INS_URSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSRAvvi_16B, ARM64_INS_URSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSRAvvi_2D, ARM64_INS_URSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSRAvvi_2S, ARM64_INS_URSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSRAvvi_4H, ARM64_INS_URSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSRAvvi_4S, ARM64_INS_URSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSRAvvi_8B, ARM64_INS_URSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_URSRAvvi_8H, ARM64_INS_URSRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHLLvvi_16B, ARM64_INS_USHLL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHLLvvi_2S, ARM64_INS_USHLL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHLLvvi_4H, ARM64_INS_USHLL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHLLvvi_4S, ARM64_INS_USHLL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHLLvvi_8B, ARM64_INS_USHLL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHLLvvi_8H, ARM64_INS_USHLL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHLddd, ARM64_INS_USHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHLvvv_16B, ARM64_INS_USHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHLvvv_2D, ARM64_INS_USHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHLvvv_2S, ARM64_INS_USHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHLvvv_4H, ARM64_INS_USHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHLvvv_4S, ARM64_INS_USHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHLvvv_8B, ARM64_INS_USHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHLvvv_8H, ARM64_INS_USHL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHRddi, ARM64_INS_USHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHRvvi_16B, ARM64_INS_USHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHRvvi_2D, ARM64_INS_USHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHRvvi_2S, ARM64_INS_USHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHRvvi_4H, ARM64_INS_USHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHRvvi_4S, ARM64_INS_USHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHRvvi_8B, ARM64_INS_USHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USHRvvi_8H, ARM64_INS_USHR,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USQADD16b, ARM64_INS_USQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USQADD2d, ARM64_INS_USQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USQADD2s, ARM64_INS_USQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USQADD4h, ARM64_INS_USQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USQADD4s, ARM64_INS_USQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USQADD8b, ARM64_INS_USQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USQADD8h, ARM64_INS_USQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USQADDbb, ARM64_INS_USQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USQADDdd, ARM64_INS_USQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USQADDhh, ARM64_INS_USQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USQADDss, ARM64_INS_USQADD,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USRA, ARM64_INS_USRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USRAvvi_16B, ARM64_INS_USRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USRAvvi_2D, ARM64_INS_USRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USRAvvi_2S, ARM64_INS_USRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USRAvvi_4H, ARM64_INS_USRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USRAvvi_4S, ARM64_INS_USRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USRAvvi_8B, ARM64_INS_USRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USRAvvi_8H, ARM64_INS_USRA,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USUBL2vvv_2d4s, ARM64_INS_USUBL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USUBL2vvv_4s8h, ARM64_INS_USUBL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USUBL2vvv_8h16b, ARM64_INS_USUBL2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USUBLvvv_2d2s, ARM64_INS_USUBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USUBLvvv_4s4h, ARM64_INS_USUBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USUBLvvv_8h8b, ARM64_INS_USUBL,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USUBW2vvv_2d4s, ARM64_INS_USUBW2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USUBW2vvv_4s8h, ARM64_INS_USUBW2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USUBW2vvv_8h16b, ARM64_INS_USUBW2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USUBWvvv_2d2s, ARM64_INS_USUBW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USUBWvvv_4s4h, ARM64_INS_USUBW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_USUBWvvv_8h8b, ARM64_INS_USUBW,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UXTBww, ARM64_INS_UXTB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UXTBxw, ARM64_INS_UXTB,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UXTHww, ARM64_INS_UXTH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UXTHxw, ARM64_INS_UXTH,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { 0 }, 0, 0
#endif
	},
	{
		AArch64_UZP1vvv_16b, ARM64_INS_UZP1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UZP1vvv_2d, ARM64_INS_UZP1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UZP1vvv_2s, ARM64_INS_UZP1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UZP1vvv_4h, ARM64_INS_UZP1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UZP1vvv_4s, ARM64_INS_UZP1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UZP1vvv_8b, ARM64_INS_UZP1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UZP1vvv_8h, ARM64_INS_UZP1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UZP2vvv_16b, ARM64_INS_UZP2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UZP2vvv_2d, ARM64_INS_UZP2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UZP2vvv_2s, ARM64_INS_UZP2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UZP2vvv_4h, ARM64_INS_UZP2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UZP2vvv_4s, ARM64_INS_UZP2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UZP2vvv_8b, ARM64_INS_UZP2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_UZP2vvv_8h, ARM64_INS_UZP2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_VCVTf2xs_2D, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_VCVTf2xs_2S, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_VCVTf2xs_4S, ARM64_INS_FCVTZS,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_VCVTf2xu_2D, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_VCVTf2xu_2S, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_VCVTf2xu_4S, ARM64_INS_FCVTZU,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_VCVTxs2f_2D, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_VCVTxs2f_2S, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_VCVTxs2f_4S, ARM64_INS_SCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_VCVTxu2f_2D, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_VCVTxu2f_2S, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_VCVTxu2f_4S, ARM64_INS_UCVTF,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_XTN2d2s, ARM64_INS_XTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_XTN2d4s, ARM64_INS_XTN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_XTN4s4h, ARM64_INS_XTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_XTN4s8h, ARM64_INS_XTN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_XTN8h16b, ARM64_INS_XTN2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_XTN8h8b, ARM64_INS_XTN,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ZIP1vvv_16b, ARM64_INS_ZIP1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ZIP1vvv_2d, ARM64_INS_ZIP1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ZIP1vvv_2s, ARM64_INS_ZIP1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ZIP1vvv_4h, ARM64_INS_ZIP1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ZIP1vvv_4s, ARM64_INS_ZIP1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ZIP1vvv_8b, ARM64_INS_ZIP1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ZIP1vvv_8h, ARM64_INS_ZIP1,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ZIP2vvv_16b, ARM64_INS_ZIP2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ZIP2vvv_2d, ARM64_INS_ZIP2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ZIP2vvv_2s, ARM64_INS_ZIP2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ZIP2vvv_4h, ARM64_INS_ZIP2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ZIP2vvv_4s, ARM64_INS_ZIP2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ZIP2vvv_8b, ARM64_INS_ZIP2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
	{
		AArch64_ZIP2vvv_8h, ARM64_INS_ZIP2,
#ifndef CAPSTONE_DIET
		{ 0 }, { 0 }, { ARM64_GRP_NEON, 0 }, 0, 0
#endif
	},
};

// some alias instruction only need to be defined locally to satisfy
// some lookup functions
// just make sure these IDs never reuse any other IDs ARM_INS_*
#define ARM64_INS_NEGS (unsigned short)-1
#define ARM64_INS_NGCS (unsigned short)-2

// given internal insn id, return public instruction info
void AArch64_get_insn_id(cs_struct *h, cs_insn *insn, unsigned int id)
{
	int i = insn_find(insns, ARR_SIZE(insns), id, &h->insn_cache);
	if (i != 0) {
		insn->id = insns[i].mapid;

		if (h->detail) {
#ifndef CAPSTONE_DIET
			cs_struct handle;
			handle.detail = h->detail;

			memcpy(insn->detail->regs_read, insns[i].regs_use, sizeof(insns[i].regs_use));
			insn->detail->regs_read_count = (uint8_t)count_positive(insns[i].regs_use);

			memcpy(insn->detail->regs_write, insns[i].regs_mod, sizeof(insns[i].regs_mod));
			insn->detail->regs_write_count = (uint8_t)count_positive(insns[i].regs_mod);

			memcpy(insn->detail->groups, insns[i].groups, sizeof(insns[i].groups));
			insn->detail->groups_count = (uint8_t)count_positive(insns[i].groups);

			insn->detail->arm64.update_flags = cs_reg_write((csh)&handle, insn, ARM64_REG_NZCV);

			if (insns[i].branch || insns[i].indirect_branch) {
				// this insn also belongs to JUMP group. add JUMP group
				insn->detail->groups[insn->detail->groups_count] = ARM64_GRP_JUMP;
				insn->detail->groups_count++;
			}
#endif
		}
	}
}

static name_map insn_name_maps[] = {
	{ ARM64_INS_INVALID, NULL },
	//=========

	{ ARM64_INS_ABS, "abs" },
	{ ARM64_INS_ADC, "adc" },
	{ ARM64_INS_ADDHN2, "addhn2" },
	{ ARM64_INS_ADDHN, "addhn" },
	{ ARM64_INS_ADDP, "addp" },
	{ ARM64_INS_ADDV, "addv" },
	{ ARM64_INS_ADD, "add" },
	{ ARM64_INS_CMN, "cmn" },
	{ ARM64_INS_ADRP, "adrp" },
	{ ARM64_INS_ADR, "adr" },
	{ ARM64_INS_AESD, "aesd" },
	{ ARM64_INS_AESE, "aese" },
	{ ARM64_INS_AESIMC, "aesimc" },
	{ ARM64_INS_AESMC, "aesmc" },
	{ ARM64_INS_AND, "and" },
	{ ARM64_INS_ASR, "asr" },
	{ ARM64_INS_AT, "at" },
	{ ARM64_INS_BFI, "bfi" },
	{ ARM64_INS_BFM, "bfm" },
	{ ARM64_INS_BFXIL, "bfxil" },
	{ ARM64_INS_BIC, "bic" },
	{ ARM64_INS_BIF, "bif" },
	{ ARM64_INS_BIT, "bit" },
	{ ARM64_INS_BLR, "blr" },
	{ ARM64_INS_BL, "bl" },
	{ ARM64_INS_BRK, "brk" },
	{ ARM64_INS_BR, "br" },
	{ ARM64_INS_BSL, "bsl" },
	{ ARM64_INS_B, "b" },
	{ ARM64_INS_CBNZ, "cbnz" },
	{ ARM64_INS_CBZ, "cbz" },
	{ ARM64_INS_CCMN, "ccmn" },
	{ ARM64_INS_CCMP, "ccmp" },
	{ ARM64_INS_CLREX, "clrex" },
	{ ARM64_INS_CLS, "cls" },
	{ ARM64_INS_CLZ, "clz" },
	{ ARM64_INS_CMEQ, "cmeq" },
	{ ARM64_INS_CMGE, "cmge" },
	{ ARM64_INS_CMGT, "cmgt" },
	{ ARM64_INS_CMHI, "cmhi" },
	{ ARM64_INS_CMHS, "cmhs" },
	{ ARM64_INS_CMLE, "cmle" },
	{ ARM64_INS_CMLT, "cmlt" },
	{ ARM64_INS_CMP, "cmp" },
	{ ARM64_INS_CMTST, "cmtst" },
	{ ARM64_INS_CNT, "cnt" },
	{ ARM64_INS_CRC32B, "crc32b" },
	{ ARM64_INS_CRC32CB, "crc32cb" },
	{ ARM64_INS_CRC32CH, "crc32ch" },
	{ ARM64_INS_CRC32CW, "crc32cw" },
	{ ARM64_INS_CRC32CX, "crc32cx" },
	{ ARM64_INS_CRC32H, "crc32h" },
	{ ARM64_INS_CRC32W, "crc32w" },
	{ ARM64_INS_CRC32X, "crc32x" },
	{ ARM64_INS_CSEL, "csel" },
	{ ARM64_INS_CSINC, "csinc" },
	{ ARM64_INS_CSINV, "csinv" },
	{ ARM64_INS_CSNEG, "csneg" },
	{ ARM64_INS_DCPS1, "dcps1" },
	{ ARM64_INS_DCPS2, "dcps2" },
	{ ARM64_INS_DCPS3, "dcps3" },
	{ ARM64_INS_DC, "dc" },
	{ ARM64_INS_DMB, "dmb" },
	{ ARM64_INS_DRPS, "drps" },
	{ ARM64_INS_DSB, "dsb" },
	{ ARM64_INS_DUP, "dup" },
	{ ARM64_INS_EON, "eon" },
	{ ARM64_INS_EOR, "eor" },
	{ ARM64_INS_ERET, "eret" },
	{ ARM64_INS_EXTR, "extr" },
	{ ARM64_INS_EXT, "ext" },
	{ ARM64_INS_FABD, "fabd" },
	{ ARM64_INS_FABS, "fabs" },
	{ ARM64_INS_FACGE, "facge" },
	{ ARM64_INS_FACGT, "facgt" },
	{ ARM64_INS_FADDP, "faddp" },
	{ ARM64_INS_FADD, "fadd" },
	{ ARM64_INS_FCCMPE, "fccmpe" },
	{ ARM64_INS_FCCMP, "fccmp" },
	{ ARM64_INS_FCMEQ, "fcmeq" },
	{ ARM64_INS_FCMGE, "fcmge" },
	{ ARM64_INS_FCMGT, "fcmgt" },
	{ ARM64_INS_FCMLE, "fcmle" },
	{ ARM64_INS_FCMLT, "fcmlt" },
	{ ARM64_INS_FCMP, "fcmp" },
	{ ARM64_INS_FCMPE, "fcmpe" },
	{ ARM64_INS_FCSEL, "fcsel" },
	{ ARM64_INS_FCVTAS, "fcvtas" },
	{ ARM64_INS_FCVTAU, "fcvtau" },
	{ ARM64_INS_FCVTL, "fcvtl" },
	{ ARM64_INS_FCVTL2, "fcvtl2" },
	{ ARM64_INS_FCVTMS, "fcvtms" },
	{ ARM64_INS_FCVTMU, "fcvtmu" },
	{ ARM64_INS_FCVTN, "fcvtn" },
	{ ARM64_INS_FCVTN2, "fcvtn2" },
	{ ARM64_INS_FCVTNS, "fcvtns" },
	{ ARM64_INS_FCVTNU, "fcvtnu" },
	{ ARM64_INS_FCVTPS, "fcvtps" },
	{ ARM64_INS_FCVTPU, "fcvtpu" },
	{ ARM64_INS_FCVTXN, "fcvtxn" },
	{ ARM64_INS_FCVTXN2, "fcvtxn2" },
	{ ARM64_INS_FCVTZS, "fcvtzs" },
	{ ARM64_INS_FCVTZU, "fcvtzu" },
	{ ARM64_INS_FCVT, "fcvt" },
	{ ARM64_INS_FDIV, "fdiv" },
	{ ARM64_INS_FMADD, "fmadd" },
	{ ARM64_INS_FMAXNMP, "fmaxnmp" },
	{ ARM64_INS_FMAXNMV, "fmaxnmv" },
	{ ARM64_INS_FMAXNM, "fmaxnm" },
	{ ARM64_INS_FMAXP, "fmaxp" },
	{ ARM64_INS_FMAXV, "fmaxv" },
	{ ARM64_INS_FMAX, "fmax" },
	{ ARM64_INS_FMINNMP, "fminnmp" },
	{ ARM64_INS_FMINNMV, "fminnmv" },
	{ ARM64_INS_FMINNM, "fminnm" },
	{ ARM64_INS_FMINP, "fminp" },
	{ ARM64_INS_FMINV, "fminv" },
	{ ARM64_INS_FMIN, "fmin" },
	{ ARM64_INS_FMLA, "fmla" },
	{ ARM64_INS_FMLS, "fmls" },
	{ ARM64_INS_FMOV, "fmov" },
	{ ARM64_INS_FMSUB, "fmsub" },
	{ ARM64_INS_FMULX, "fmulx" },
	{ ARM64_INS_FMUL, "fmul" },
	{ ARM64_INS_FNEG, "fneg" },
	{ ARM64_INS_FNMADD, "fnmadd" },
	{ ARM64_INS_FNMSUB, "fnmsub" },
	{ ARM64_INS_FNMUL, "fnmul" },
	{ ARM64_INS_FRECPE, "frecpe" },
	{ ARM64_INS_FRECPS, "frecps" },
	{ ARM64_INS_FRECPX, "frecpx" },
	{ ARM64_INS_FRINTA, "frinta" },
	{ ARM64_INS_FRINTI, "frinti" },
	{ ARM64_INS_FRINTM, "frintm" },
	{ ARM64_INS_FRINTN, "frintn" },
	{ ARM64_INS_FRINTP, "frintp" },
	{ ARM64_INS_FRINTX, "frintx" },
	{ ARM64_INS_FRINTZ, "frintz" },
	{ ARM64_INS_FRSQRTE, "frsqrte" },
	{ ARM64_INS_FRSQRTS, "frsqrts" },
	{ ARM64_INS_FSQRT, "fsqrt" },
	{ ARM64_INS_FSUB, "fsub" },
	{ ARM64_INS_HINT, "hint" },
	{ ARM64_INS_HLT, "hlt" },
	{ ARM64_INS_HVC, "hvc" },
	{ ARM64_INS_IC, "ic" },
	{ ARM64_INS_INS, "ins" },
	{ ARM64_INS_ISB, "isb" },
	{ ARM64_INS_LD1, "ld1" },
	{ ARM64_INS_LD1R, "ld1r" },
	{ ARM64_INS_LD2, "ld2" },
	{ ARM64_INS_LD2R, "ld2r" },
	{ ARM64_INS_LD3, "ld3" },
	{ ARM64_INS_LD3R, "ld3r" },
	{ ARM64_INS_LD4, "ld4" },
	{ ARM64_INS_LD4R, "ld4r" },
	{ ARM64_INS_LDARB, "ldarb" },
	{ ARM64_INS_LDAR, "ldar" },
	{ ARM64_INS_LDARH, "ldarh" },
	{ ARM64_INS_LDAXP, "ldaxp" },
	{ ARM64_INS_LDAXRB, "ldaxrb" },
	{ ARM64_INS_LDAXR, "ldaxr" },
	{ ARM64_INS_LDAXRH, "ldaxrh" },
	{ ARM64_INS_LDPSW, "ldpsw" },
	{ ARM64_INS_LDRSB, "ldrsb" },
	{ ARM64_INS_LDURSB, "ldursb" },
	{ ARM64_INS_LDRSH, "ldrsh" },
	{ ARM64_INS_LDURSH, "ldursh" },
	{ ARM64_INS_LDRSW, "ldrsw" },
	{ ARM64_INS_LDR, "ldr" },
	{ ARM64_INS_LDTRSB, "ldtrsb" },
	{ ARM64_INS_LDTRSH, "ldtrsh" },
	{ ARM64_INS_LDTRSW, "ldtrsw" },
	{ ARM64_INS_LDURSW, "ldursw" },
	{ ARM64_INS_LDXP, "ldxp" },
	{ ARM64_INS_LDXRB, "ldxrb" },
	{ ARM64_INS_LDXR, "ldxr" },
	{ ARM64_INS_LDXRH, "ldxrh" },
	{ ARM64_INS_LDRH, "ldrh" },
	{ ARM64_INS_LDURH, "ldurh" },
	{ ARM64_INS_STRH, "strh" },
	{ ARM64_INS_STURH, "sturh" },
	{ ARM64_INS_LDTRH, "ldtrh" },
	{ ARM64_INS_STTRH, "sttrh" },
	{ ARM64_INS_LDUR, "ldur" },
	{ ARM64_INS_STR, "str" },
	{ ARM64_INS_STUR, "stur" },
	{ ARM64_INS_LDTR, "ldtr" },
	{ ARM64_INS_STTR, "sttr" },
	{ ARM64_INS_LDRB, "ldrb" },
	{ ARM64_INS_LDURB, "ldurb" },
	{ ARM64_INS_STRB, "strb" },
	{ ARM64_INS_STURB, "sturb" },
	{ ARM64_INS_LDTRB, "ldtrb" },
	{ ARM64_INS_STTRB, "sttrb" },
	{ ARM64_INS_LDP, "ldp" },
	{ ARM64_INS_LDNP, "ldnp" },
	{ ARM64_INS_STNP, "stnp" },
	{ ARM64_INS_STP, "stp" },
	{ ARM64_INS_LSL, "lsl" },
	{ ARM64_INS_LSR, "lsr" },
	{ ARM64_INS_MADD, "madd" },
	{ ARM64_INS_MLA, "mla" },
	{ ARM64_INS_MLS, "mls" },
	{ ARM64_INS_MOVI, "movi" },
	{ ARM64_INS_MOVK, "movk" },
	{ ARM64_INS_MOVN, "movn" },
	{ ARM64_INS_MOVZ, "movz" },
	{ ARM64_INS_MRS, "mrs" },
	{ ARM64_INS_MSR, "msr" },
	{ ARM64_INS_MSUB, "msub" },
	{ ARM64_INS_MUL, "mul" },
	{ ARM64_INS_MVNI, "mvni" },
	{ ARM64_INS_MVN, "mvn" },
	{ ARM64_INS_NEG, "neg" },
	{ ARM64_INS_NOT, "not" },
	{ ARM64_INS_ORN, "orn" },
	{ ARM64_INS_ORR, "orr" },
	{ ARM64_INS_PMULL2, "pmull2" },
	{ ARM64_INS_PMULL, "pmull" },
	{ ARM64_INS_PMUL, "pmul" },
	{ ARM64_INS_PRFM, "prfm" },
	{ ARM64_INS_PRFUM, "prfum" },
	{ ARM64_INS_SQRSHRUN2, "sqrshrun2" },
	{ ARM64_INS_SQRSHRUN, "sqrshrun" },
	{ ARM64_INS_SQSHRUN2, "sqshrun2" },
	{ ARM64_INS_SQSHRUN, "sqshrun" },
	{ ARM64_INS_RADDHN2, "raddhn2" },
	{ ARM64_INS_RADDHN, "raddhn" },
	{ ARM64_INS_RBIT, "rbit" },
	{ ARM64_INS_RET, "ret" },
	{ ARM64_INS_REV16, "rev16" },
	{ ARM64_INS_REV32, "rev32" },
	{ ARM64_INS_REV64, "rev64" },
	{ ARM64_INS_REV, "rev" },
	{ ARM64_INS_ROR, "ror" },
	{ ARM64_INS_RSHRN2, "rshrn2" },
	{ ARM64_INS_RSHRN, "rshrn" },
	{ ARM64_INS_RSUBHN2, "rsubhn2" },
	{ ARM64_INS_RSUBHN, "rsubhn" },
	{ ARM64_INS_SABAL2, "sabal2" },
	{ ARM64_INS_SABAL, "sabal" },
	{ ARM64_INS_SABA, "saba" },
	{ ARM64_INS_SABDL2, "sabdl2" },
	{ ARM64_INS_SABDL, "sabdl" },
	{ ARM64_INS_SABD, "sabd" },
	{ ARM64_INS_SADALP, "sadalp" },
	{ ARM64_INS_SADDL2, "saddl2" },
	{ ARM64_INS_SADDLP, "saddlp" },
	{ ARM64_INS_SADDLV, "saddlv" },
	{ ARM64_INS_SADDL, "saddl" },
	{ ARM64_INS_SADDW2, "saddw2" },
	{ ARM64_INS_SADDW, "saddw" },
	{ ARM64_INS_SBC, "sbc" },
	{ ARM64_INS_SBFIZ, "sbfiz" },
	{ ARM64_INS_SBFM, "sbfm" },
	{ ARM64_INS_SBFX, "sbfx" },
	{ ARM64_INS_SCVTF, "scvtf" },
	{ ARM64_INS_SDIV, "sdiv" },
	{ ARM64_INS_SHA1C, "sha1c" },
	{ ARM64_INS_SHA1H, "sha1h" },
	{ ARM64_INS_SHA1M, "sha1m" },
	{ ARM64_INS_SHA1P, "sha1p" },
	{ ARM64_INS_SHA1SU0, "sha1su0" },
	{ ARM64_INS_SHA1SU1, "sha1su1" },
	{ ARM64_INS_SHA256H, "sha256h" },
	{ ARM64_INS_SHA256H2, "sha256h2" },
	{ ARM64_INS_SHA256SU0, "sha256su0" },
	{ ARM64_INS_SHA256SU1, "sha256su1" },
	{ ARM64_INS_SHADD, "shadd" },
	{ ARM64_INS_SHLL2, "shll2" },
	{ ARM64_INS_SHLL, "shll" },
	{ ARM64_INS_SHL, "shl" },
	{ ARM64_INS_SHRN2, "shrn2" },
	{ ARM64_INS_SHRN, "shrn" },
	{ ARM64_INS_SHSUB, "shsub" },
	{ ARM64_INS_SLI, "sli" },
	{ ARM64_INS_SMADDL, "smaddl" },
	{ ARM64_INS_SMAXP, "smaxp" },
	{ ARM64_INS_SMAXV, "smaxv" },
	{ ARM64_INS_SMAX, "smax" },
	{ ARM64_INS_SMC, "smc" },
	{ ARM64_INS_SMINP, "sminp" },
	{ ARM64_INS_SMINV, "sminv" },
	{ ARM64_INS_SMIN, "smin" },
	{ ARM64_INS_SMLAL2, "smlal2" },
	{ ARM64_INS_SMLAL, "smlal" },
	{ ARM64_INS_SMLSL2, "smlsl2" },
	{ ARM64_INS_SMLSL, "smlsl" },
	{ ARM64_INS_SMOV, "smov" },
	{ ARM64_INS_SMSUBL, "smsubl" },
	{ ARM64_INS_SMULH, "smulh" },
	{ ARM64_INS_SMULL2, "smull2" },
	{ ARM64_INS_SMULL, "smull" },
	{ ARM64_INS_SQABS, "sqabs" },
	{ ARM64_INS_SQADD, "sqadd" },
	{ ARM64_INS_SQDMLAL2, "sqdmlal2" },
	{ ARM64_INS_SQDMLAL, "sqdmlal" },
	{ ARM64_INS_SQDMLSL2, "sqdmlsl2" },
	{ ARM64_INS_SQDMLSL, "sqdmlsl" },
	{ ARM64_INS_SQDMULH, "sqdmulh" },
	{ ARM64_INS_SQDMULL2, "sqdmull2" },
	{ ARM64_INS_SQDMULL, "sqdmull" },
	{ ARM64_INS_SQNEG, "sqneg" },
	{ ARM64_INS_SQRDMULH, "sqrdmulh" },
	{ ARM64_INS_SQRSHL, "sqrshl" },
	{ ARM64_INS_SQRSHRN, "sqrshrn" },
	{ ARM64_INS_SQRSHRN2, "sqrshrn2" },
	{ ARM64_INS_SQSHLU, "sqshlu" },
	{ ARM64_INS_SQSHL, "sqshl" },
	{ ARM64_INS_SQSHRN, "sqshrn" },
	{ ARM64_INS_SQSHRN2, "sqshrn2" },
	{ ARM64_INS_SQSUB, "sqsub" },
	{ ARM64_INS_SQXTN, "sqxtn" },
	{ ARM64_INS_SQXTN2, "sqxtn2" },
	{ ARM64_INS_SQXTUN, "sqxtun" },
	{ ARM64_INS_SQXTUN2, "sqxtun2" },
	{ ARM64_INS_SRHADD, "srhadd" },
	{ ARM64_INS_SRI, "sri" },
	{ ARM64_INS_SRSHL, "srshl" },
	{ ARM64_INS_SRSHR, "srshr" },
	{ ARM64_INS_SRSRA, "srsra" },
	{ ARM64_INS_SSHLL2, "sshll2" },
	{ ARM64_INS_SSHLL, "sshll" },
	{ ARM64_INS_SSHL, "sshl" },
	{ ARM64_INS_SSHR, "sshr" },
	{ ARM64_INS_SSRA, "ssra" },
	{ ARM64_INS_SSUBL2, "ssubl2" },
	{ ARM64_INS_SSUBL, "ssubl" },
	{ ARM64_INS_SSUBW2, "ssubw2" },
	{ ARM64_INS_SSUBW, "ssubw" },
	{ ARM64_INS_ST1, "st1" },
	{ ARM64_INS_ST2, "st2" },
	{ ARM64_INS_ST3, "st3" },
	{ ARM64_INS_ST4, "st4" },
	{ ARM64_INS_STLRB, "stlrb" },
	{ ARM64_INS_STLR, "stlr" },
	{ ARM64_INS_STLRH, "stlrh" },
	{ ARM64_INS_STLXP, "stlxp" },
	{ ARM64_INS_STLXRB, "stlxrb" },
	{ ARM64_INS_STLXR, "stlxr" },
	{ ARM64_INS_STLXRH, "stlxrh" },
	{ ARM64_INS_STXP, "stxp" },
	{ ARM64_INS_STXRB, "stxrb" },
	{ ARM64_INS_STXR, "stxr" },
	{ ARM64_INS_STXRH, "stxrh" },
	{ ARM64_INS_SUBHN2, "subhn2" },
	{ ARM64_INS_SUBHN, "subhn" },
	{ ARM64_INS_SUB, "sub" },
	{ ARM64_INS_SUQADD, "suqadd" },
	{ ARM64_INS_SVC, "svc" },
	{ ARM64_INS_SXTB, "sxtb" },
	{ ARM64_INS_SXTH, "sxth" },
	{ ARM64_INS_SXTW, "sxtw" },
	{ ARM64_INS_SYSL, "sysl" },
	{ ARM64_INS_SYS, "sys" },
	{ ARM64_INS_TBL, "tbl" },
	{ ARM64_INS_TBNZ, "tbnz" },
	{ ARM64_INS_TBX, "tbx" },
	{ ARM64_INS_TBZ, "tbz" },
	{ ARM64_INS_TLBI, "tlbi" },
	{ ARM64_INS_TRN1, "trn1" },
	{ ARM64_INS_TRN2, "trn2" },
	{ ARM64_INS_TST, "tst" },
	{ ARM64_INS_UABAL2, "uabal2" },
	{ ARM64_INS_UABAL, "uabal" },
	{ ARM64_INS_UABA, "uaba" },
	{ ARM64_INS_UABDL2, "uabdl2" },
	{ ARM64_INS_UABDL, "uabdl" },
	{ ARM64_INS_UABD, "uabd" },
	{ ARM64_INS_UADALP, "uadalp" },
	{ ARM64_INS_UADDL2, "uaddl2" },
	{ ARM64_INS_UADDLP, "uaddlp" },
	{ ARM64_INS_UADDLV, "uaddlv" },
	{ ARM64_INS_UADDL, "uaddl" },
	{ ARM64_INS_UADDW2, "uaddw2" },
	{ ARM64_INS_UADDW, "uaddw" },
	{ ARM64_INS_UBFIZ, "ubfiz" },
	{ ARM64_INS_UBFM, "ubfm" },
	{ ARM64_INS_UBFX, "ubfx" },
	{ ARM64_INS_UCVTF, "ucvtf" },
	{ ARM64_INS_UDIV, "udiv" },
	{ ARM64_INS_UHADD, "uhadd" },
	{ ARM64_INS_UHSUB, "uhsub" },
	{ ARM64_INS_UMADDL, "umaddl" },
	{ ARM64_INS_UMAXP, "umaxp" },
	{ ARM64_INS_UMAXV, "umaxv" },
	{ ARM64_INS_UMAX, "umax" },
	{ ARM64_INS_UMINP, "uminp" },
	{ ARM64_INS_UMINV, "uminv" },
	{ ARM64_INS_UMIN, "umin" },
	{ ARM64_INS_UMLAL2, "umlal2" },
	{ ARM64_INS_UMLAL, "umlal" },
	{ ARM64_INS_UMLSL2, "umlsl2" },
	{ ARM64_INS_UMLSL, "umlsl" },
	{ ARM64_INS_UMOV, "umov" },
	{ ARM64_INS_UMSUBL, "umsubl" },
	{ ARM64_INS_UMULH, "umulh" },
	{ ARM64_INS_UMULL2, "umull2" },
	{ ARM64_INS_UMULL, "umull" },
	{ ARM64_INS_UQADD, "uqadd" },
	{ ARM64_INS_UQRSHL, "uqrshl" },
	{ ARM64_INS_UQRSHRN, "uqrshrn" },
	{ ARM64_INS_UQRSHRN2, "uqrshrn2" },
	{ ARM64_INS_UQSHL, "uqshl" },
	{ ARM64_INS_UQSHRN, "uqshrn" },
	{ ARM64_INS_UQSHRN2, "uqshrn2" },
	{ ARM64_INS_UQSUB, "uqsub" },
	{ ARM64_INS_UQXTN, "uqxtn" },
	{ ARM64_INS_UQXTN2, "uqxtn2" },
	{ ARM64_INS_URECPE, "urecpe" },
	{ ARM64_INS_URHADD, "urhadd" },
	{ ARM64_INS_URSHL, "urshl" },
	{ ARM64_INS_URSHR, "urshr" },
	{ ARM64_INS_URSQRTE, "ursqrte" },
	{ ARM64_INS_URSRA, "ursra" },
	{ ARM64_INS_USHLL2, "ushll2" },
	{ ARM64_INS_USHLL, "ushll" },
	{ ARM64_INS_USHL, "ushl" },
	{ ARM64_INS_USHR, "ushr" },
	{ ARM64_INS_USQADD, "usqadd" },
	{ ARM64_INS_USRA, "usra" },
	{ ARM64_INS_USUBL2, "usubl2" },
	{ ARM64_INS_USUBL, "usubl" },
	{ ARM64_INS_USUBW2, "usubw2" },
	{ ARM64_INS_USUBW, "usubw" },
	{ ARM64_INS_UXTB, "uxtb" },
	{ ARM64_INS_UXTH, "uxth" },
	{ ARM64_INS_UZP1, "uzp1" },
	{ ARM64_INS_UZP2, "uzp2" },
	{ ARM64_INS_XTN, "xtn" },
	{ ARM64_INS_XTN2, "xtn2" },
	{ ARM64_INS_ZIP1, "zip1" },
	{ ARM64_INS_ZIP2, "zip2" },
};

// map *S & alias instructions back to original id
static name_map alias_insn_name_maps[] = {
	{ ARM64_INS_ADC, "adcs" },
	{ ARM64_INS_AND, "ands" },
	{ ARM64_INS_ADD, "adds" },
	{ ARM64_INS_BIC, "bics" },
	{ ARM64_INS_SBC, "sbcs" },
	{ ARM64_INS_SUB, "subs" },

	// alias insn
	{ ARM64_INS_MNEG, "mneg" },
	{ ARM64_INS_UMNEGL, "umnegl" },
	{ ARM64_INS_SMNEGL, "smnegl" },
	{ ARM64_INS_MOV, "mov" },
	{ ARM64_INS_NOP, "nop" },
	{ ARM64_INS_YIELD, "yield" },
	{ ARM64_INS_WFE, "wfe" },
	{ ARM64_INS_WFI, "wfi" },
	{ ARM64_INS_SEV, "sev" },
	{ ARM64_INS_SEVL, "sevl" },
	{ ARM64_INS_NGC, "ngc" },
	{ ARM64_INS_NGCS, "ngcs" },
	{ ARM64_INS_NEGS, "negs" },
};

const char *AArch64_insn_name(csh handle, unsigned int id)
{
#ifndef CAPSTONE_DIET
	if (id >= ARM64_INS_MAX)
		return NULL;

	if (id < ARR_SIZE(insn_name_maps))
		return insn_name_maps[id].name;

	// then find alias insn
	int i;
	for (i = 0; i < ARR_SIZE(alias_insn_name_maps); i++) {
		if (alias_insn_name_maps[i].id == id)
			return alias_insn_name_maps[i].name;
	}

	// not found
	return NULL;
#else
	return NULL;
#endif
}

// map instruction name to public instruction ID
arm64_reg AArch64_map_insn(const char *name)
{
	// NOTE: skip first NULL name in insn_name_maps
	int i = name2id(&insn_name_maps[1], ARR_SIZE(insn_name_maps) - 1, name);

	if (i == -1)
		// try again with 'special' insn that is not available in insn_name_maps
		i = name2id(alias_insn_name_maps, ARR_SIZE(alias_insn_name_maps), name);

	return (i != -1)? i : ARM64_REG_INVALID;
}
