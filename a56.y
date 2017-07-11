%{
/*******************************************************
 *
 *  a56 - a DSP56001 assembler
 *
 *  Written by Quinn C. Jensen
 *  July 1990
 *
 *******************************************************/

/*
 * Copyright (C) 1990-1994 Quinn C. Jensen
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  The author makes no representations
 * about the suitability of this software for any purpose.  It is
 * provided "as is" without express or implied warranty.
 *
 */

/*
 *  a56.y - The YACC grammar for the assembler.
 *
 *  Note:  This module requires a "BIG" version of YACC.  I had to
 *  recompile YACC in the largest mode available.
 *
 *  Other notes:
 *
 *  MOVEC, MOVEM and MOVEP must be used explicitly--MOVE can't yet figure
 *  out which form to use.
 *
 */

#include "a56.h"
#include <math.h>

unsigned int w0, w1;			/* workspace for the actual generated code */
BOOL uses_w1;					/* says whether w1 is alive */
unsigned int pc;				/* current program counter */
int seg;						/* current segment P: X: Y: or L: */
int expr_seg;					/* segment of current expression */

int just_rep = 0;				/* keeps track of REP instruction */
int hot_rreg = -1;				/* rreg loaded by prev inst. or -1 */
int hot_nreg = -1;				/* nreg loaded by prev inst. or -1 */
int hot_mreg = -1;				/* mreg loaded by prev inst. or -1 */
int prev_hot_rreg = -1;			/* rreg loaded by prev inst. or -1 */
int prev_hot_nreg = -1;			/* nreg loaded by prev inst. or -1 */
int prev_hot_mreg = -1;			/* mreg loaded by prev inst. or -1 */

int substatement = 0;			/* in a substatement */
BOOL long_symbolic_expr = FALSE; /* a parser flag */
char *new_include = NULL;		/* file to be included */

/* listing stuff */

char segs[] = "uPXYL*";
extern BOOL list_on_next;		/* listing to turn on or off */
BOOL list_on;					/* listing on at the moment */
extern char *cur_line;			/* points to line being lex'd */
char list_buf[1024 + 80];		/* listing buffer */
char list_buf2[1024 + 80];		/* listing buffer for two-line code */
BOOL uses_buf2 = FALSE;			/* list_buf2 is alive */
BOOL list_print_line = FALSE;	/* whether or not to print line in listing */
char *spaces(), *luntab();

struct n binary_op();
struct n unary_op();
struct n sym_ref();

#define R_R6				0x0001
#define R_R5				0x0002
#define R_R4				0x0004
#define R_DATA_ALU_ACCUM	0x0008
#define R_CTL_REG			0x0010
#define R_FUNKY_CTL_REG		0x0020
#define R_SDX				0x0040
#define R_SDY				0x0080
#define R_LSD				0x0100
#define R_AB				0x0200
#define R_XREG				0x0400
#define R_YREG				0x0800
/* registers to which short immediate move is an unsigned int */
#define R_UINT				0x1000
/* registers to which short immediate move is an signed frac */
#define R_SFRAC				0x2000
%}

%union {
	int ival;			/* integer value */
	struct n n;			/* just like in struct sym */
	double dval;		/* floating point value */
	char *sval;			/* string */
	int cval;			/* character */
	char cond;			/* condition */
	struct regs {
		int r6, r5, r4, data_alu_accum, ctl_reg, funky_ctl_reg;
		int sdx, sdy, lsd, ab, xreg, yreg;
		int flags;
	} regs;
	struct ea {
		int mode;
		int ext;
		int pp;
	} ea;
}

%token <n> CHEX CDEC FRAC 
%token <ival> AREG BREG MREG NREG RREG XREG YREG
%token <ival> OP OPA OPP
%token <cond> OP_JCC OP_JSCC OP_TCC
%token <sval> SYM
%token <sval> STRING
%token <cval> CHAR
%token XMEM
%token YMEM
%token LMEM
%token PMEM
%token AAAA
%token A10
%token BBBB
%token B10
%token AABB
%token BBAA
%token XXXX
%token YYYY
%token SR
%token MR
%token CCR
%token OMR
%token SP
%token SSH
%token SSL
%token LA
%token LC
%token EOL
%token EOS
%token LEXBAD

%token OP_ABS
%token OP_ADC
%token OP_ADD
%token OP_ADDL
%token OP_ADDR
%token OP_ASL
%token OP_ASR
%token OP_CLR
%token OP_CMP
%token OP_CMPM
%token OP_DIV
%token OP_MAC
%token OP_MACR
%token OP_MPY
%token OP_MPYR
%token OP_NEG
%token OP_NORM
%token OP_RND
%token OP_SBC
%token OP_SUB
%token OP_SUBL
%token OP_SUBR
%token OP_TFR
%token OP_TST
%token OP_AND
%token OP_ANDI
%token OP_EOR
%token OP_LSL
%token OP_LSR
%token OP_NOT
%token OP_OR
%token OP_ORI
%token OP_ROL
%token OP_ROR
%token OP_BCLR
%token OP_BSET
%token OP_BCHG
%token OP_BTST
%token OP_DO
%token OP_ENDDO
%token OP_LUA
%token OP_MOVE
%token OP_MOVEC
%token OP_MOVEM
%token OP_MOVEP
%token OP_ILLEGAL
%token OP_INCLUDE
%token OP_JMP
%token OP_JCLR
%token OP_JSET
%token OP_JSR
%token OP_JSCLR
%token OP_JSSET
%token OP_NOP
%token OP_REP
%token OP_RESET
%token OP_RTI
%token OP_RTS
%token OP_STOP
%token OP_SWI
%token OP_WAIT
%token OP_EQU
%token OP_ORG
%token OP_DC
%token OP_DS
%token OP_DSM
%token OP_END
%token OP_PAGE
%token OP_PSECT
%token OP_ALIGN
%token OP_INT
%token SHL
%token SHR
%token OP_PI
%token OP_SIN
%token OP_COS
%token OP_TAN
%token OP_ATAN
%token OP_ASIN
%token OP_ACOS
%token OP_EXP
%token OP_LN
%token OP_LOG
%token OP_POW

%type <n> num num_or_sym 
%type <n> num_or_sym_expr
%type <n> expr
%type <n> ix
%type <n> ix_long

%type <ival> abs_addr abs_short_addr io_short_addr 
%type <ival> a_b x_or_y ea b5_10111_max
%type <ival> p6_ean_a6 ea_no_ext p6_ea_a6 ea_a6 ea_a12
%type <ival> ea_short
%type <ival> prog_ctl_reg
%type <ival> op8_1 op8_2 op8_3 op8_4 op8_5 op8_6 op8_7 op8_8
%type <ival> mpy_arg mpy_srcs plus_minus
%type <ival> sd3
%type <ival> funky_ctl_reg tcc_sd space
%type <sval> opt_label

%type <regs> regs
%type <ea> movep_ea_pp

%left '|'
%left '^'
%left SHL SHR
%left '&'
%left '+' '-'
%left '*' '/' '%'
%right '~'

%start input

%%

/*%%%********************* top syntax ***********************/

input	:	/* empty */
	|	input statement
	;

statement
	:	good_stuff EOL
			{
			if(pass == 2 && list_on && list_print_line) {
				printf(ldebug ? "\n(%s|%s)\n" : "%s%s\n",
					list_buf, substatement == 0 ? luntab(cur_line) : "");
				if(uses_buf2)
					printf(ldebug ? "\n(%s|)\n" : "%s\n",
						list_buf2);
				list_buf[0] = list_buf2[0] = '\0';
			}
			curline++;
			uses_buf2 = FALSE;
			list_print_line = TRUE;
			list_on = list_on_next;
			substatement = 0;
			if(NOT check_psect(seg, pc) && pass == 2)
				yyerror("%04X: psect violation", pc);
			}
	|	good_stuff EOS
			{
			if(pass == 2 && list_on && list_print_line) {
				printf(ldebug ? "\n(%s" : "%s", list_buf);
				if(substatement == 0)
					printf(ldebug ? "|%s)\n" : "%s\n", luntab(cur_line));
				else
					printf(ldebug ? ")\n" : "\n");
				if(uses_buf2)
					printf(ldebug ? "\n(%s|)\n" : "%s\n",
						list_buf2);
				list_buf[0] = list_buf2[0] = '\0';
			}
			substatement++;
			uses_buf2 = FALSE;
			list_print_line = TRUE;
			list_on = list_on_next;
			if(NOT check_psect(seg, pc) && pass == 2)
				yyerror("%04X: psect violation", pc);
			}
	|	error EOL
			{curline++; substatement = 0;}
	;

good_stuff
	:	/* empty */
			{sprintf(list_buf, "%s", spaces(0));}
	|	cpp_droppings
			{list_print_line = FALSE;}
	|	assembler_ops
			{long_symbolic_expr = FALSE;}
	|	label_field operation_field
			{char *printcode();
			if(pass == 2) {
				gencode(seg, pc, w0);
				sprintf(list_buf, "%c:%04X %s ", segs[seg], pc, printcode(w0));
				pc++;
				if(uses_w1) {
					gencode(seg, pc, w1);
					sprintf(list_buf2, "%c:%04X %s", segs[seg], pc,
	       					printcode(w1 & 0xFFFFFF));
					uses_buf2++;
					pc++;
				}
			} else {
				pc++;
				if(uses_w1)
					pc++;
			}
			w0 = w1 = 0; uses_w1 = FALSE; 
			long_symbolic_expr = FALSE;}
	|	SYM 
			{sym_def($1, INT, seg, pc);
			free($1);
			if(pass == 2 && list_on) {
				sprintf(list_buf, "%c:%04X%s", segs[seg], pc, spaces(14-8));
			long_symbolic_expr = FALSE;
			}}
	;

cpp_droppings
	:	'#' num STRING
			{if(strlen($3) > 0)
				curfile = $3;
			else
				curfile = "<stdin>";
			curline = $2.val.i - 1;}
	;

assembler_ops
	:	SYM OP_EQU expr
			{sym_def($1, $3.type, ANY, $3.val.i, $3.val.f);
			free($1);
			if(pass == 2 && list_on) {
				if($3.type == INT)
					sprintf(list_buf, "%06X%s",
						$3.val.i & 0xFFFFFF,
						spaces(14-8));
				else
					sprintf(list_buf, "%10g%s", $3.val.f,
						spaces(14-4));
			}}
	|	OP_ALIGN expr
			{int ival = n2int($2);
			if($2.type == UNDEF) {
				yyerror("illegal forward reference");
			} else if (ival <= 1) {
				yyerror("%d: illegal alignment", ival);
			} else {
				if(pc % ival != 0)
					pc += ival - pc % ival;
			}
			if(pass == 2 && list_on)
				sprintf(list_buf, "%c:%04X%s", segs[seg], pc, 
					spaces(14-8));
			}
	|	OP_PSECT SYM
			{struct psect *pp = find_psect($2);
			if(NOT pp) {
				if(pass == 2)
					yyerror("%s: undefined psect", $2);
			} else {
				seg = pp->seg;
				pc = pp->pc;
				set_psect(pp);
				if(pass == 2 && list_on)
					sprintf(list_buf, "%c:%04X%s", segs[seg], pc,
						spaces(14-8));
			}
			free($2);}
	|	OP_PSECT SYM space expr ':' expr
			{new_psect($2, $3, n2int($4), n2int($6));
			if(pass == 2 && list_on)
				sprintf(list_buf, "%c:%04X %04X%s", 
					segs[$3], n2int($4), n2int($6), spaces(14-8+4+1));
			}
	|	OP_ORG space expr
			{pc = n2int($3);
			seg = $2;
			if(pass == 2 && list_on)
				sprintf(list_buf, "%c:%04X%s", segs[seg], pc, 
					spaces(14-8));
			}
	|	OP_ORG space expr ',' space expr
			{pc = n2int($3);
			seg = $2;
			if(pass == 2 && list_on)
				sprintf(list_buf, "%c:%04X%s", segs[seg], pc, 
					spaces(14-8));
			}
	|	label_field OP_DC dc_list
	|	label_field OP_DS expr
			{pc += n2int($3);
			if(pass == 2 && list_on)
				sprintf(list_buf, "%c:%04X%s", segs[seg], pc, 
					spaces(14-8));
			}
	|	opt_label OP_DSM expr
			{int size = n2int($3);
			if(size)
			{    int align = 1;
			     while(align < size)
				 align <<= 1;
			     pc += (align - (pc % align));
			}
			if($1)
			{   sym_def($1, INT, seg, pc);
 			    free($1);
			}
			pc += size;
			}
	|	OP_PAGE num ',' num ',' num ',' num
			{if(pass == 2 && list_on) {
				sprintf(list_buf, "%s", spaces(0));
			}}
	|	OP_INCLUDE STRING
			{if(pass == 2 && list_on) {
				printf(ldebug ? "\n(%s|%s)\n" : "%s%s\n",
					spaces(0), luntab(cur_line));
				list_print_line = FALSE;
			}
			include($2); /* free($2); */
			}
	|	OP_END
			{if(pass == 2 && list_on) {
				sprintf(list_buf, "%s", spaces(0));
			}}
	;

dc_list
	:	dc_list ',' dc_stuff
	|	dc_stuff
	;

dc_stuff
	:	STRING
			{int len = strlen($1), i; char *cp; w0 = 0;
			if(len % 3 == 2)
				len++;	/* force empty word */
			for(i = 0, cp = $1; i < len; i++, cp++) {
				w0 |= (*cp & 0xFF) << (2 - (i % 3)) * 8;
				if(i % 3 == 2 || i == len - 1) {
					if(pass == 2) {
						if(list_on) sprintf(list_buf, "%c:%04X %06X%s",
							segs[seg], pc, w0, 
							spaces(14-6+5));
						gencode(seg, pc, w0);
					}
					pc++; w0 = 0;
				}
			}
			free($1);}
 	|	expr
			{int frac = n2frac($1);
			if(pass == 2) {
				if(list_on) {
					sprintf(list_buf, "%c:%04X %06X%s", segs[seg], pc, 
						frac & 0xFFFFFF, spaces(14-6+5));
				}
				gencode(seg, pc, frac);
			}
			pc++;}

space
	:	PMEM
			{$$ = PROG;}
	|	XMEM
			{$$ = XDATA;}
	|	YMEM
			{$$ = YDATA;}
	|	LMEM
			{$$ = LDATA;}
	;       

label_field
	:	SYM
			{sym_def($1, INT, seg, pc);
			free($1);}
	|	/* empty */
	;

opt_label
	:	SYM		{$$ = $1;}
	|	/* empty */	{$$ = NULL;}
	;

operation_field
	:	operation
			{prev_hot_rreg = hot_rreg;
			prev_hot_nreg = hot_nreg;
			prev_hot_mreg = hot_mreg;
			hot_rreg = hot_nreg = hot_mreg = -1;
			if(just_rep) 
				just_rep--;}
	;

operation
	:	no_parallel
	|	parallel_ok
			{w0 |= 0x200000;}
	|	parallel_ok parallel_move
	;

/*%%%************* instructions that allow parallel moves ****************/

parallel_ok
	:
		OP_MPY mpy_arg
			{w0 |= 0x80 | $2 << 2;}
	|	OP_MPYR mpy_arg
			{w0 |= 0x81 | $2 << 2;}
	|	OP_MAC mpy_arg
			{w0 |= 0x82 | $2 << 2;}
	|	OP_MACR mpy_arg
			{w0 |= 0x83 | $2 << 2;}

	|       OP_SUB op8_1
			{w0 |= 0x04 | $2 << 3;}
	|	OP_ADD op8_1
			{w0 |= 0x00 | $2 << 3;}
	|	OP_MOVE
			{w0 |= 0x00;}

	|	OP_TFR op8_2
			{w0 |= 0x01 | $2 << 3;}
	|	OP_CMP op8_2
			{w0 |= 0x05 | $2 << 3;}
	|	OP_CMPM op8_2
			{w0 |= 0x07 | $2 << 3;}

	|	OP_RND op8_3
			{w0 |= 0x11 | $2 << 3;}
	|	OP_ADDL op8_3
			{w0 |= 0x12 | $2 << 3;}
	|	OP_CLR op8_3
			{w0 |= 0x13 | $2 << 3;}
	|	OP_SUBL op8_3
			{w0 |= 0x16 | $2 << 3;}
	|	OP_NOT op8_3
			{w0 |= 0x17 | $2 << 3;}

	|	OP_ADDR op8_4
			{w0 |= 0x02 | $2 << 3;}
	|	OP_TST op8_4
			{w0 |= 0x03 | $2 << 3;}
	|	OP_SUBR op8_4
			{w0 |= 0x06 | $2 << 3;}

	|       OP_AND op8_5
			{w0 |= 0x46 | $2 << 3;}
	|	OP_OR op8_5
			{w0 |= 0x42 | $2 << 3;}
	|       OP_EOR op8_5
			{w0 |= 0x43 | $2 << 3;}

	|       OP_ASR op8_6
			{w0 |= 0x22 | $2 << 3;}
	|       OP_LSR op8_6
			{w0 |= 0x23 | $2 << 3;}
	|       OP_ABS op8_6
			{w0 |= 0x26 | $2 << 3;}
	|       OP_ROR op8_6
			{w0 |= 0x27 | $2 << 3;}

	|       OP_ASL op8_7
			{w0 |= 0x32 | $2 << 3;}
	|       OP_LSL op8_7
			{w0 |= 0x33 | $2 << 3;}
	|       OP_NEG op8_7
			{w0 |= 0x36 | $2 << 3;}
	|       OP_ROL op8_7
			{w0 |= 0x37 | $2 << 3;}

	|       OP_ADC op8_8
			{w0 |= 0x21 | $2 << 3;}
	|       OP_SBC op8_8
			{w0 |= 0x25 | $2 << 3;}
	;

mpy_arg	:	plus_minus mpy_srcs ',' a_b
			{$$ = $1 | $4 << 1 | $2 << 2;}
	;

plus_minus
	:	'+'    
			{$$ = 0;}
	|	'-'    
			{$$ = 1;}
	|	
			{$$ = 0;}
	;

mpy_srcs
	:       XREG ',' XREG
			{switch ($1 << 4 | $3) {
				case 0x00: $$ = 0x0; break;
				case 0x01: 
				case 0x10: $$ = 0x2; break;
				case 0x11: 
					yyerror("illegal source operands"); 
					break;
			}}      			
	|	YREG ',' YREG
			{switch ($1 << 4 | $3) {
				case 0x00: $$ = 0x1; break;
				case 0x01: 
				case 0x10: $$ = 0x3; break;
				case 0x11: 
					yyerror("illegal source operands"); 
					break;
			}}      			
	|	XREG ',' YREG
			{switch ($1 << 4 | $3) {
				case 0x00: $$ = 0x5; break;
				case 0x01: $$ = 0x4; break;
				case 0x10: $$ = 0x6; break;
				case 0x11: $$ = 0x7; break;
			}}      			
	|	YREG ',' XREG
			{switch ($1 << 4 | $3) {
				case 0x00: $$ = 0x5; break;
				case 0x01: $$ = 0x6; break;
				case 0x10: $$ = 0x4; break;
				case 0x11: $$ = 0x7; break;
			}}      			
	;

op8_1	:	BBBB ',' AAAA
			{$$ = 0x2;}
	|	AAAA ',' BBBB
			{$$ = 0x3;}
	|	XXXX ',' a_b
			{$$ = 0x4 | $3;}
	|	YYYY ',' a_b
			{$$ = 0x6 | $3;}
	|	XREG ',' a_b
			{$$ = 0x8 | $1 << 2 | $3;}
	|	YREG ',' a_b
			{$$ = 0xA | $1 << 2 | $3;}
	;

op8_2	:	BBBB ',' AAAA
			{$$ = 0x0;}
	|	AAAA ',' BBBB
			{$$ = 0x1;}
	|	XREG ',' a_b
			{$$ = 0x8 | $1 << 2 | $3;}
	|	YREG ',' a_b
			{$$ = 0xA | $1 << 2 | $3;}
	;

op8_3	:	AAAA
			{$$ = 0x0;}
	|	BBBB
			{$$ = 0x1;}
	|	BBBB ',' AAAA
			{$$ = 0x0;}
	|	AAAA ',' BBBB
			{$$ = 0x1;}
	;

op8_4	:	op8_3
			{$$ = $1;}
	;

op8_5	:	XREG ',' a_b
			{$$ = 0x0 | $1 << 2 | $3;}
	|	YREG ',' a_b
			{$$ = 0x2 | $1 << 2 | $3;}
	;

op8_6	:	a_b
			{$$ = $1;}
	;

op8_7	:	a_b
			{$$ = $1;}
	;

op8_8	:	XXXX ',' a_b
			{$$ = 0x0 | $3;}
	|	YYYY ',' a_b
			{$$ = 0x2 | $3;}
	;

a_b	:	AAAA
			{$$ = 0;}
	|	BBBB
			{$$ = 1;}
	;

no_parallel
	:	control_inst
			{if(just_rep == 1)
				yyerror("instruction not allowed after REP");}
	|	bit_inst
	|	move_inst
	|	arith_inst
	;

/*%%%************** non-parallel arithmetic and logical ********************/

arith_inst
	:	OP_NORM RREG ',' a_b
			{w0 |= 0x01D815 | $2 << 8 | $4 << 3;}
	|	OP_DIV sd3
			{w0 |= 0x018040 | $2 << 3;}
	|	or_op ix ',' funky_ctl_reg
			{w0 |= 0x0000F8 | (n2int($2) & 0xFF) << 8 | $4;}
	|	and_op ix ',' funky_ctl_reg
			{w0 |= 0x0000B8 | (n2int($2) & 0xFF) << 8 | $4;}
	;

or_op	:	OP_OR
	|	OP_ORI
	;

and_op	:	OP_AND
	|	OP_ANDI
	;

/*%%%******************************* control instructions **********************/

control_inst
	:	OP_JSCC ea_a12
			{if($2) {
				w0 |= 0x0BC0A0 | $1 << 0;
			} else {
				w0 |= 0x0F0000 | $1 << 12;
			}}
	|	OP_JCC ea_a12
			{if($2) {
				w0 |= 0x0AC0A0 | $1 << 0;
			} else {
				w0 |= 0x0E0000 | $1 << 12;
			}}
	|	OP_JSR ea_a12
			{if($2) {
				w0 |= 0x0BC080;
			} else {
				w0 |= 0x0D0000;
			}}
	|	OP_JMP ea_a12
			{if($2) {
				w0 |= 0x0AC080;
			} else {
				w0 |= 0x0C0000;
			}}

	|	OP_JSSET control_args
			{w0 |= 0x0B0020;}
	|	OP_JSCLR control_args
			{w0 |= 0x0B0000;}
	|	OP_JSET control_args
			{w0 |= 0x0A0020;}
	|	OP_JCLR control_args
			{w0 |= 0x0A0000;}

	|	OP_REP rep_args
			{just_rep = 2;}
	|	OP_DO do_args
			{uses_w1++;}
	|	OP_ENDDO
			{w0 |= 0x00008C;}
	|	OP_STOP
			{w0 |= 0x000087;}
	|	OP_WAIT
			{w0 |= 0x000086;}
	|	OP_RESET
			{w0 |= 0x000084;}
	|	OP_RTS
			{w0 |= 0x00000C;}
	|	OP_SWI
			{w0 |= 0x000006;}
	|	OP_ILLEGAL
			{w0 |= 0x000005;}
	|	OP_RTI
			{w0 |= 0x000004;}
	|	OP_NOP
			{w0 |= 0x000000;
			just_rep = 0;}
	;

do_args
	:	ix ',' abs_addr
			{int ival = n2int($1);
			w0 |= 0x060080 | (ival & 0xFF) << 8 | (ival & 0xF00)>> 8;
			if(ival > 0xFFF && pass == 2) {
				yywarning("warning: immediate operand truncated");
			}
			w1 |= $3-1;}
	|	regs ',' abs_addr
			{w0 |= 0x06C000 | $1.r6 << 8;
			hot_rreg = hot_nreg = hot_mreg = -1;
			w1 |= $3-1;}
	|	x_or_y ea_no_ext ',' abs_addr
			{w0 |= 0x064000 | $2 << 8 | $1 << 6;
			w1 |= $4-1;}
	|	x_or_y abs_short_addr ',' abs_addr	/* allow forced */
			{w0 |= 0x060000 | ($2 & 0x3F) << 8 | $1 << 6;
			/*
			 * $$$ oops, can't check expr_seg because both abs_short_addr and
			 * abs_addr touch it
			 */
			if($2 > 0x003F && pass == 2)
				yywarning("warning: address operand truncated");
			w1 |= $4-1;}
	|	x_or_y abs_addr ',' abs_addr
			{w0 |= 0x060000 | ($2 & 0x3F) << 8 | $1 << 6;
			/*
			 * $$$ oops, can't check expr_seg because both abs_short_addr and
			 * abs_addr touch it
			 */
			if($2 > 0x003F && pass == 2)
				yywarning("warning: address operand truncated");
			w1 |= $4-1;}
	;       		

rep_args
	:	ix
			{int ival = n2int($1);
			w0 |= 0x0600A0 | (ival & 0xFF) << 8 | (ival & 0xF00)>> 8;
			if(ival > 0xFFF && pass == 2) {
				yywarning("warning: immediate operand truncated");
			}}
	|	regs
			{w0 |= 0x06C020 | $1.r6 << 8;
			hot_rreg = hot_nreg = hot_mreg = -1;}
	|	x_or_y ea_no_ext
			{w0 |= 0x064020 | $1 << 6 | $2 << 8;}
	|	x_or_y abs_addr
			{w0 |= 0x060020 | $1 << 6 | ($2 & 0x3F) << 8;
			if(expr_seg != ANY && ($1 == 0 && expr_seg != XDATA ||
				$1 == 1 && expr_seg != YDATA))
				yywarning("warning: space mismatch");
			if($2 > 0x003F && pass == 2)
				yywarning("warning: address operand truncated");
			}
	|	x_or_y abs_short_addr	/* forced */
			{w0 |= 0x060020 | $1 << 6 | ($2 & 0x3F) << 8;
			if(expr_seg != ANY && ($1 == 0 && expr_seg != XDATA ||
				$1 == 1 && expr_seg != YDATA))
				yywarning("warning: space mismatch");
			if($2 > 0x003F && pass == 2)
				yywarning("warning: address operand truncated");
			}
	;

control_args
	:	b5_10111_max ',' x_or_y p6_ean_a6 ',' abs_addr
			{w0 |= $1 << 0 | $3 << 6;
			uses_w1++;
			w1 = $6;}
	|	b5_10111_max ',' regs ',' abs_addr
			{w0 |= 0x00C000 | $1 << 0 | $3.r6 << 8;
			hot_rreg = hot_nreg = hot_mreg = -1;
			uses_w1++;
			w1 = $5;}
	;

p6_ean_a6
	:	abs_addr	/* in pass 2 can always discern size. */
				/* Sometimes in pass one, too.  But since */
				/* address extension is always used for the */
				/* branch target, pass 1 can assume the */
				/* symbol value will fit; warning in pass 2 */
				/* if it doesn't */
			{if($1 != -1) {	/* symbol defined */
				w0 |= ($1 & 0x3F) << 8;
				if($1 >= 0xFFC0) {
					w0 |= 0x008080;
				} else {
					w0 |= 0x000080;
					if($1 > 0x003F && pass == 2)
						yywarning("warning: address operand truncated");
				}
			}}
	|	abs_short_addr
			{if($1 != -1) {
				if($1 > 0x3F && pass == 2)
					yywarning("warning: address operand truncated");
				w0 |= 0x000080 | ($1 & 0x3F) << 8;
			}}
	|	io_short_addr
			{if($1 != -1) {
				if($1 < 0xFFC0 && pass == 2)
					yywarning("warning: address operand truncated");
				w0 |= 0x008080 | ($1 & 0x3F) << 8;
			}}
	|	ea_no_ext
			{w0 |= 0x004080 | $1 << 8;}
	;

/*%%%**************************** bit instructions ***************************/

bit_inst
	:	OP_BTST bit_args
			{w0 |= 0x0B0020;}
	|	OP_BCHG bit_args
			{w0 |= 0x0B0000;}
	|	OP_BSET bit_args
			{w0 |= 0x0A0020;}
	|	OP_BCLR bit_args
			{w0 |= 0x0A0000;}
	;       		

bit_args
	:	b5_10111_max ',' x_or_y p6_ea_a6
			{w0 |= $1 << 0 | $3 << 6;
			}
	|	b5_10111_max ',' regs
			{w0 |= 0x00C040 | $1 << 0 | $3.r6 << 8;}
	;       	

p6_ea_a6
	:	io_short_addr	/* must be forced to tell from abs_addr */
			{if($1 != -1) {
				w0 |= ($1 & 0x3F) << 8 | 0x008000;
				if($1 < 0xFFC0 && pass == 2)
					yywarning("warning: address operand truncated");
			}}
	|	abs_short_addr	/* must be forced to tell from abs_addr */
			{if($1 != -1) {
				w0 |= ($1 & 0x3F) << 8 | 0x000000;
				if($1 > 0x003F && pass == 2)
					yywarning("warning: address operand truncated");
			}}
	|	ea	/* can use abs_addr */
			{w0 |= 0x004000;}
	;

/*%%%************************** move instructions **********************/

move_inst
	:	OP_MOVEP movep_args
	|	OP_MOVEM movem_args
	|	OP_MOVEC movec_args
	|	OP_LUA ea_short ',' regs
			{w0 |= 0x044010 | $2 << 8 | $4.r4;}
	|	OP_TCC tcc_args
			{w0 |= $1 << 12;}
	;       	

tcc_args
	:	tcc_sd
			{w0 |= 0x020000 | $1 << 3;}
	|	tcc_sd RREG ',' RREG
			{w0 |= 0x030000 | $1 << 3 | $2 << 8 | $4;}
	;

tcc_sd
	:	regs /* a_b */ ',' regs /* a_b */
			{hot_rreg = hot_nreg = hot_mreg = -1;
			if($1.flags & R_AB && $3.flags & R_AB) {
				if($1.ab == $3.ab) 
					yyerror("source and dest must be different");
				$$ = $3.ab;
			} else if($1.flags & R_XREG && $3.flags & R_AB) {
				$$ = 0x8 | $1.xreg << 2 | $3.ab;
			} else if($1.flags & R_YREG && $3.flags & R_AB) {
				$$ = 0xA | $1.yreg << 2 | $3.ab;
			} else 
				yyerror("illegal TCC operands");
			}
	;

sd3	:	regs /* XREG */ ',' regs /* a_b */
			{hot_rreg = hot_nreg = hot_mreg = -1;
			if($1.flags & R_XREG && $3.flags & R_AB) {
				$$ = $1.xreg << 2 | $3.ab;
			} else if($1.flags & R_YREG && $3.flags & R_AB) {
				$$ = $1.yreg << 2 | 2 | $3.ab;
			}}
	;

movec_args
	:	x_or_y ea ',' regs /* ctl_reg */
			{if(NOT ($4.flags & R_CTL_REG))
				yyerror("bad MOVEC target register");
			if(expr_seg != ANY && ($1 == 0 && expr_seg != XDATA ||
				$1 == 1 && expr_seg != YDATA))
				yywarning("warning: space mismatch");
			if($1 == 0) {
				w0 |= 0x05C020 | $4.ctl_reg;
			} else {
				w0 |= 0x05C060 | $4.ctl_reg;
			}}
	|	regs /* ctl_reg */ ',' x_or_y ea
			{hot_rreg = hot_nreg = hot_mreg = -1;
			if(NOT ($1.flags & R_CTL_REG))
				yyerror("bad MOVEC source register");
			if($3 == 0) {
				w0 |= 0x054020 | $1.ctl_reg;
			} else {
				w0 |= 0x054060 | $1.ctl_reg;
			}}
	|	ix ',' regs /* ctl_reg */
			{int ival = n2int($1);
			if(NOT ($3.flags & R_CTL_REG))
				yyerror("bad MOVEC target register");
			if(ival < 256 && NOT long_symbolic_expr) {
				w0 |= 0x0500A0 | (ival & 0xFF) << 8 | $3.ctl_reg; 
			} else {
				w0 |= 0x05C020 | 0x003400 | $3.ctl_reg;
				uses_w1++; w1 = ival & 0xFFFF;
			}}
	|	x_or_y abs_short_addr ',' regs /* ctl_reg */
			{if($1 == 0) {
				w0 |= 0x058020 | ($2 & 0x3F) << 8 | $4.ctl_reg;
			} else {
				w0 |= 0x058060 | ($2 & 0x3F) << 8 | $4.ctl_reg;
			}
			if(NOT ($4.flags & R_CTL_REG))
				yyerror("bad MOVEC target register");
			if($2 > 0x003F && pass == 2)
				yywarning("warning: address operand truncated");
			}
	|	regs /* ctl_reg */ ',' x_or_y abs_short_addr
			{hot_rreg = hot_nreg = hot_mreg = -1;
			if($3 == 0) {
				w0 |= 0x050020 | ($4 & 0x3F) << 8 | $1.ctl_reg;
			} else {
				w0 |= 0x050060 | ($4 & 0x3F) << 8 | $1.ctl_reg;
			}
			if(NOT ($1.flags & R_CTL_REG))
				yyerror("bad MOVEC source register");
			if($4 > 0x003F && pass == 2)
				yywarning("warning: address operand truncated");
			}
	|	regs /* ctl_reg */ ',' regs
			{if($1.flags & R_CTL_REG) {
				w0 |= 0x0440A0 | $3.r6 << 8 | $1.ctl_reg;
			} else if($3.flags & R_CTL_REG) {
				w0 |= 0x04C0A0 | $1.r6 << 8 | $3.ctl_reg;
			} else if($1.flags & $3.flags & R_CTL_REG) {
				/* bogus? $$$ */
	       			w0 |= 0x04C0A0 | ($1.ctl_reg | 0x20) << 8 | 
				$3.ctl_reg;
			} else {
				yyerror("MOVEC must reference a control reg");
			}}
	;

movep_args
	:	x_or_y movep_ea_pp ',' x_or_y movep_ea_pp
			{w0 |= 0x084080;
			switch($2.pp << 1 | $5.pp) {
				case 0:	case 3:
					yyerror("illegal MOVEP; can't move EA to EA or IO to IO");
					break;
				case 1:	/* ea, pp */
					w0 |= $4 << 16 | 1 << 15 | $1 << 6 |
						($5.ext & 0x3F);
					if($2.mode == 0x003000) {
						w0 |= 0x003000;
						uses_w1++;
						w1 = $2.ext;
					} else {
						w0 |= $2.mode;
					}
					break;
				case 2:	/* pp, ea */
					w0 |= $1 << 16 | 0 << 15 | $4 << 6 |
						($2.ext & 0x3F);
					if($5.mode == 0x003000) {
						w0 |= 0x003000;
						uses_w1++;
						w1 = $5.ext;
					} else {
						w0 |= $5.mode;
					}
					break;
			}}
	|	ix ',' x_or_y num_or_sym
			{w0 |= 0x084080;
			w0 |= $3 << 16 | 1 << 15 | 0x34 << 8 | 
				(n2int($4) & 0x3F);
			uses_w1++;
			w1 = n2int($1);}
	|	PMEM ea ',' x_or_y num_or_sym
			{w0 |= 0x084040;
			w0 |= $4 << 16 | 1 << 15 | (n2int($5) & 0x3F);}
	|	x_or_y movep_ea_pp ',' PMEM ea
			{w0 |= 0x084040;
			if($2.mode != 0x003000 && $2.mode != 0)
				yyerror("illegal MOVEP");
			w0 |= $1 << 16 | 0 << 15 | ($2.ext & 0x3F);}
	|	regs ',' x_or_y num_or_sym
			{hot_rreg = hot_nreg = hot_mreg = -1;
			w0 |= 0x084000;
			w0 |= $3 << 16 | 1 << 15 | $1.r6 << 8 | 
				(n2int($4) & 0x3F);}
	|	x_or_y movep_ea_pp ',' regs
			{hot_rreg = hot_nreg = hot_mreg = -1;
			w0 |= 0x084000;
			if(!$2.pp)
				yyerror("illegal MOVEP");
			w0 |= $1 << 16 | 0 << 15 | $4.r6 << 8 | ($2.ext & 0x3F);}
	;

movep_ea_pp
	:	abs_addr
			{if($1 != UNDEF && $1 >= 0xFFC0) {
				/* defined symbol or constant, in i/o range */
				$$.pp = 1;
				$$.mode = 0;
			} else {
				/* either out of i/o range or symbol not */
				/* yet defined:  assume ea extension */
				$$.pp = 0;
				$$.mode = 0x003000;
			}
			$$.ext = $1;}
	|	io_short_addr	/* forced i/o short */
			{$$.pp = 1;
			$$.mode = 0;
			if($1 < 0xFFC0 && pass == 2)
				yywarning("warning: address operand truncated");
			$$.ext = $1;}
	|	ea_no_ext
			{$$.pp = 0;
			$$.mode = $1 << 8;
			$$.ext = $1;}
	;

movem_args
	:	regs ',' PMEM ea_a6
			{w0 |= 0x070000 | 0 << 15 | $1.r6;}
	|	PMEM ea_a6 ',' regs
			{hot_rreg = hot_nreg = hot_mreg = -1;
			w0 |= 0x070000 | 1 << 15 | $4.r6;}
	;

/*%%%**************** memory reference fields ************************/

b5_10111_max
	:	ix
			{int ival = n2int($1);
			$$ = ival; if(ival > 0x17) 
				yyerror("%d: illegal bit number", ival);}
	;

x_or_y
	:	XMEM
			{$$ = 0;}
	|	YMEM
			{$$ = 1;}
	;

/*%%%**************** effective address fields ************************/

ea_a6
	:	ea
			{w0 |= 0x004080;}
	|	abs_short_addr
			{w0 |= ($1 & 0x3F) << 8;
			if($1 > 0x003F && pass == 2)
				yywarning("warning: address operand truncated");
			}
	;

ea_a12
	:	ea
			{$$ = 1;}
	|	abs_short_addr
			{w0 |= $1 & 0xFFF; $$ = 0;
			if($1 > 0x0FFF && pass == 2)
				yywarning("warning: address operand truncated");
			}
	;

ea	:	abs_addr
			{w0 |= 0x003000;
			uses_w1++;
			w1 |= $1;
			$$ = 0x003000;}
	|	ea_no_ext
			{w0 |= $1 << 8;
			$$ = $1 << 8;
			expr_seg = ANY;}
	;

ea_no_ext
	:	ea_short
			{$$ = $1;}
	|	'(' RREG ')'
			{if($2 == prev_hot_rreg) yywarning("warning: r%d just loaded", $2);
			$$ = 4 << 3 | $2;}
	|	'(' RREG '+' NREG ')'
			{if($2 == prev_hot_rreg) yywarning("warning: r%d just loaded", $2);
			if($4 == prev_hot_nreg) yywarning("warning: n%d just loaded", $4);
			if($2 == prev_hot_mreg) yywarning("warning: m%d just loaded", $2);
			$$ = 5 << 3 | $2;
			if($2 != $4) yyerror("Rn and Nn must be same number");}
	|	'-' '(' RREG ')'
			{if($3 == prev_hot_rreg) yywarning("warning: r%d just loaded", $3);
			if($3 == prev_hot_mreg) yywarning("warning: m%d just loaded", $3);
			$$ = 7 << 3 | $3;}
	;

ea_short
	:	'(' RREG ')' '-' NREG
			{if($2 == prev_hot_rreg) yywarning("warning: r%d just loaded", $2);
			if($5 == prev_hot_nreg) yywarning("warning: n%d just loaded", $5);
			if($2 == prev_hot_mreg) yywarning("warning: m%d just loaded", $2);
			$$ = 0 << 3 | $2;
			expr_seg = ANY;
			if($2 != $5) yyerror("Rn and Nn must be same number");}
	|	'(' RREG ')' '+' NREG
			{if($2 == prev_hot_rreg) yywarning("warning: r%d just loaded", $2);
			if($5 == prev_hot_nreg) yywarning("warning: n%d just loaded", $5);
			if($2 == prev_hot_mreg) yywarning("warning: m%d just loaded", $2);
			$$ = 1 << 3 | $2;
			expr_seg = ANY;
			if($2 != $5) yyerror("Rn and Nn must be same number");}
	|	'(' RREG ')' '-'
			{if($2 == prev_hot_rreg) yywarning("warning: r%d just loaded", $2);
			if($2 == prev_hot_mreg) yywarning("warning: m%d just loaded", $2);
			expr_seg = ANY;
			$$ = 2 << 3 | $2;}
	|	'(' RREG ')' '+'
			{if($2 == prev_hot_rreg) yywarning("warning: r%d just loaded", $2);
			if($2 == prev_hot_mreg) yywarning("warning: m%d just loaded", $2);
			expr_seg = ANY;
			$$ = 3 << 3 | $2;}
	;

/*%%%******************* register fields ******************************/

regs
	:	XREG
			{$$.r6 = $$.r5 = 0x04 | $1;
			$$.sdx = $1;
			$$.xreg = $1;
			$$.flags = R_R6|R_R5|R_XREG|R_SDX|R_SFRAC;}
	|	YREG
			{$$.r6 = $$.r5 = 0x06 | $1;
			$$.sdy = $1;
			$$.yreg = $1;
			$$.flags = R_R6|R_R5|R_SDY|R_YREG|R_SFRAC;}
	|	AREG
			{switch($1) {
				case 0: 
					$$.r6 = $$.r5 = 0x08 | 0; 
					break;
				case 1: 
					$$.r6 = $$.r5 = 0x08 | 4; 
					break;
				case 2: 
					$$.r6 = $$.r5 = 0x08 | 2; 
					break;
			}
			$$.flags = R_R6|R_R5|R_UINT;}
	|	BREG
			{switch($1) {
				case 0: 
					$$.r6 = $$.r5 = 0x08 | 1; break;
				case 1: 
					$$.r6 = $$.r5 = 0x08 | 5; break;
				case 2: 
					$$.r6 = $$.r5 = 0x08 | 3; break;
			}
			$$.flags = R_R6|R_R5|R_UINT;}
	|	AAAA
			{$$.r6 = $$.r5 = 0x0E;
			$$.sdx = $$.sdy = 0x2;
			$$.ab = 0;
			$$.lsd = 4;
			$$.flags = R_R6|R_R5|R_SDX|R_SDY|R_AB|R_LSD|R_SFRAC;}
	|	BBBB
			{$$.r6 = $$.r5 = 0x0F;
			$$.sdx = $$.sdy = 0x3;
			$$.ab = 1;
			$$.lsd = 5;
			$$.flags = R_R6|R_R5|R_SDX|R_SDY|R_AB|R_LSD|R_SFRAC;}
	|	RREG
			{$$.r6 = $$.r5 = 0x10 | $1;
			$$.r4 = 0x00 | $1;
			$$.flags = R_R6|R_R5|R_R4|R_UINT;
			hot_rreg = $1;}
	|	NREG
			{$$.r6 = $$.r5 = 0x18 | $1;
			$$.r4 = 0x08 | $1;
			$$.flags = R_R6|R_R5|R_R4|R_UINT;
			hot_nreg = $1;}
	|	MREG
			{$$.r6 = 0x20 | $1;
			$$.r5 = -1;
			$$.ctl_reg = $1;
			$$.flags = R_R6|R_R5|R_CTL_REG|R_UINT;
			hot_mreg = $1;}
	|	prog_ctl_reg
			{$$.r6 = 0x38 | $1;
			$$.r5 = -1;
			$$.ctl_reg = 0x18 | $1;
			$$.flags = R_R6|R_R5|R_CTL_REG|R_UINT;}
	|	A10
			{$$.lsd  = 0;
			$$.flags = R_LSD;}
	|	B10
			{$$.lsd = 1;
			$$.flags = R_LSD;}
	|	XXXX
			{$$.lsd = 2;
			$$.flags = R_LSD;}
	|	YYYY
			{$$.lsd = 3;
			$$.flags = R_LSD;}
	|	AABB
			{$$.lsd = 6;
			$$.flags = R_LSD;}
	|	BBAA
			{$$.lsd = 7;
			$$.flags = R_LSD;}
	;

prog_ctl_reg
	:	SR
			{$$ = 1;}
	|	OMR
			{$$ = 2;}
	|	SP
			{$$ = 3;}
	|	SSH
			{$$ = 4;}
	|	SSL
			{$$ = 5;}
	|	LA
			{$$ = 6;}
	|	LC
			{$$ = 7;}
	;

funky_ctl_reg
	:	MR
			{$$ = 0;}
	|	CCR
			{$$ = 1;}
	|	OMR
			{$$ = 2;}
	;

/*%%%************************* parallel moves *************/

parallel_move
	:	i_move
	|	u_move
	|	x_or_y_move
	|	xr_move
	|	ry_move
	|	r_move
	|	xy_move
	|	l_move
	;

i_move  :	ix ',' regs
			{int ival = n2int($1);
			int frac = n2frac($1);
			int value;
			BOOL shortform = FALSE;
			if($3.flags & R_CTL_REG) {
				yyerror("please use MOVEC for control register moves");
				break;
			}
			if(($3.flags & R_SFRAC) && $1.type == FLT) {
				if((frac & 0xFFFF) == 0 && 
					NOT long_symbolic_expr) {
					value = frac >> 16;
					shortform++;
				} else {
					value = frac;
				}
			} else {
				if(ival <= 0xFF && ival >= -0xFF && NOT long_symbolic_expr) {
					value = ival;
					shortform++;
				} else {
					value = ival;
				}
			}

			if(shortform) {
				w0 |= 0x200000 | (value & 0xFF) << 8 |
					$3.r5 << 16;
			} else {
				w0 |= 0x400000 | 0x00F400 |
					($3.r5 >> 3 & 3) << 20 | 
					($3.r5 & 7) << 16;
				uses_w1++; w1 = value;
			}}
	;

r_move	:	regs ',' regs
			{
				if($3.flags & R_CTL_REG) {
					yyerror("please use MOVEC for control register moves");
					break;
				}
				if($1.flags & R_R5 & $3.flags) 
					w0 |= 0x200000 | $3.r5 << 8 | $1.r5 << 13;
				else
					yyerror("illegal R move");
			}	
	;

u_move	:	ea_short
			{w0 |= 0x204000 | $1 << 8;}
	;

x_or_y_move
	:	x_or_y ea ',' regs
			{w0 |= 0x40C000 | $1 << 19;
			if(expr_seg != ANY && ($1 == 0 && expr_seg != XDATA ||
				$1 == 1 && expr_seg != YDATA))
				yywarning("warning: space mismatch");
			if($4.flags & R_CTL_REG) {
				yyerror("please use MOVEC for control register moves");
				break;
			}
			w0 |= ($4.r5 >> 3 & 3) << 20 | ($4.r5 & 7) << 16;}
	|	x_or_y abs_short_addr ',' regs
			{w0 |= 0x408000 | $1 << 19 | ($2 & 0x3F) << 8;
			if(expr_seg != ANY && ($1 == 0 && expr_seg != XDATA ||
				$1 == 1 && expr_seg != YDATA))
				yywarning("warning: space mismatch");
			if($4.flags & R_CTL_REG) {
				yyerror("please use MOVEC for control register moves");
				break;
			}
			if($2 > 0x003F && pass == 2)
				yywarning("warning: address operand truncated");
			w0 |= ($4.r5>> 3 & 3) << 20 | ($4.r5 & 7) << 16;}
	|	regs ',' x_or_y ea
			{hot_rreg = hot_nreg = hot_mreg = -1;
			w0 |= 0x404000 | $3 << 19;
			if(expr_seg != ANY && ($3 == 0 && expr_seg != XDATA ||
				$3 == 1 && expr_seg != YDATA))
				yywarning("warning: space mismatch");
			if($1.flags & R_CTL_REG) {
				yyerror("please use MOVEC for control register moves");
				break;
			}
			w0 |= ($1.r5 >> 3 & 3) << 20 | ($1.r5 & 7) << 16;}
	|	regs ',' x_or_y abs_short_addr
			{hot_rreg = hot_nreg = hot_mreg = -1;
			w0 |= 0x400000 | $3 << 19 | ($4 & 0x3F) << 8;
			if($1.flags & R_CTL_REG) {
				yyerror("please use MOVEC for control register moves");
				break;
			}
			if(expr_seg != ANY && ($3 == 0 && expr_seg != XDATA ||
				$3 == 1 && expr_seg != YDATA))
				yywarning("warning: space mismatch");
			if($4 > 0x003F && pass == 2)
				yywarning("warning: address operand truncated");
			w0 |= ($1.r5 >> 3 & 3) << 20 | ($1.r5 & 7) << 16;}
	|	ix_long ',' regs
			{w0 |= 0x400000 | 0x00F400 | ($3.r5 >> 3 & 3) << 20 |
			    ($3.r5 & 7) << 16;
			if($3.flags & R_CTL_REG) {
				yyerror("please use MOVEC for control register moves");
				break;
			}
			uses_w1++; w1 = n2frac($1);
			}
	;

xr_move
	:	x_or_y /* XMEM */ ea ',' regs	regs /* a_b */ ',' YREG
			{hot_rreg = hot_nreg = hot_mreg = -1;
			if(expr_seg != ANY && ($1 == 0 && expr_seg != XDATA ||
				$1 == 1 && expr_seg != YDATA))
				yywarning("warning: space mismatch");
			if($1 == 0 && $5.flags & R_AB) {
				w0 |= 0x108000 | $4.sdx << 18 | $5.ab << 17 |
					$7 << 16;
			} else {
				yyerror("illegal X:R move");
			}}
	| 	ix ',' regs		regs /* a_b */ ',' YREG
			{hot_rreg = hot_nreg = hot_mreg = -1;
			if($4.flags & R_AB) {
				w0 |= 0x10B400 | $3.sdx << 18 | $4.ab << 17 |
					$6 << 16;
				uses_w1++;
				w1 |= n2frac($1) & 0xFFFFFF;
			} else {
				yyerror("illegal X:R move");
			}}
	|	regs ',' x_or_y /* XMEM */ ea	regs /* a_b */ ',' regs/*YREG*/
			{hot_rreg = hot_nreg = hot_mreg = -1;
			if(expr_seg != ANY && ($3 == 0 && expr_seg != XDATA ||
				$3 == 1 && expr_seg != YDATA))
				yywarning("warning: space mismatch");
			if($1.flags & R_SDX && $3 == 0 && $5.flags & R_AB &&
				$7.flags & R_YREG) {
				w0 |= 0x100000 | $1.sdx << 18 | $5.ab << 17 |
					$7.yreg << 16;
			} else if($1.flags & R_AB && $3 == 0 && 
				$5.flags & R_XREG && $7.flags & R_AB) {
				if($5.xreg != 0) yyerror("must use X0");
				if($1.ab == 0 && $7.ab == 0)
					w0 |= 0x080000;
				else if($1.ab == 1 && $7.ab == 1)
					w0 |= 0x090000;
				else
					yyerror("illegal X:R move");
			} else {
				yyerror("illegal X:R move");
			}}
	;

ry_move	:	regs /* a_b */ ',' regs /* XREG	*/      YMEM ea ',' regs
			{hot_rreg = hot_nreg = hot_mreg = -1;
			if($3.flags & R_XREG && $7.flags & (R_YREG|R_AB)) {
				w0 |= 0x10C000 | $1.ab << 19 | $3.xreg << 18 |
					$7.sdy << 16;
			} else {
				yyerror("illegal R:Y move");
			}}
	|	regs /* a_b */ ',' regs /* XREG	*/	ix ',' regs
			{hot_rreg = hot_nreg = hot_mreg = -1;
			if($3.flags & R_XREG && $6.flags & (R_YREG|R_AB)) {
				w0 |= 0x10F400 | $1.ab << 19 | $3.xreg << 18 |
					$6.sdy << 16;
				uses_w1++;
				w1 |= n2frac($4) & 0xFFFFFF;
			} else {
				yyerror("illegal R:Y move");
			}}
	|	regs /* a_b */ ',' regs /* XREG	*/	regs ',' YMEM ea
			{hot_rreg = hot_nreg = hot_mreg = -1;
			if($1.flags & R_AB && $3.flags & R_XREG) {
				w0 |= 0x104000 | $1.ab << 19 | $3.xreg << 18 |
				$4.sdy << 16;
			} else if ($1.flags & R_YREG && $3.flags & R_AB &&
				$4.flags & R_AB) {
				if($1.yreg != 0) yyerror("must use Y0");
				if($3.ab == 0 && $4.ab == 0)
					w0 |= 0x088000;
				else if($3.ab == 1 && $4.ab == 1)
					w0 |= 0x098000;
				else
					yyerror("illegal R:Y move");
			} else {
				yyerror("illegal R:Y move");
			}}
	;

l_move
	:	LMEM ea ',' regs /* lsd */
			{if($4.flags & R_CTL_REG) {
				yyerror("please use MOVEC for control register moves");
				break;
			}
			w0 |= 0x40C000 | ($4.lsd & 3) << 16 | ($4.lsd >> 2) << 19;}
	|	regs /* lsd */ ',' LMEM ea
			{hot_rreg = hot_nreg = hot_mreg = -1;
			if($1.flags & R_CTL_REG) {
				yyerror("please use MOVEC for control register moves");
				break;
			}
			w0 |= 0x404000 | ($1.lsd & 3) << 16 | ($1.lsd >> 2) << 19;}
	|	LMEM abs_short_addr ',' regs /* lsd */
			{w0 |= 0x408000 | ($4.lsd & 3) << 16 | ($4.lsd >> 2) << 19;
			if($4.flags & R_CTL_REG) {
				yyerror("please use MOVEC for control register moves");
				break;
			}
			if($2 > 0x003F && pass == 2)
				yywarning("warning: address operand truncated");
			w0 |= ($2 & 0x3F) << 8;}
	|	regs /* lsd */ ',' LMEM abs_short_addr
			{hot_rreg = hot_nreg = hot_mreg = -1;
			w0 |= 0x400000 | ($1.lsd & 3) << 16 | ($1.lsd >> 2) << 19;
			if($1.flags & R_CTL_REG) {
				yyerror("please use MOVEC for control register moves");
				break;
			}
			if($4 > 0x003F && pass == 2)
				yywarning("warning: address operand truncated");
			w0 |= ($4 & 0x3F) << 8;}
	;

xy_move
	:	x_or_y /*XMEM*/ ea /*ea_strange*/ ',' regs	YMEM ea /*ea_strange*/ ',' regs
			{int eax = $2, eay = $6,
			     regx = ($4.flags & R_AB) ? $4.ab | 2 : $4.xreg,
			     regy = ($8.flags & R_AB) ? $8.ab | 2 : $8.yreg;
			hot_rreg = hot_nreg = hot_mreg = -1;
			if((eax & 0x400) == (eay & 0x400))
				yyerror("registers must be in opposite halves");
			if(!($4.flags & (R_AB | R_XREG)))
				yyerror("invalid X move register");
			if(!($8.flags & (R_AB | R_YREG)))
				yyerror("invalid Y move register");
			if($4.flags & R_AB &&
			   $8.flags & R_AB &&
			   $4.ab == $8.ab)
				yyerror("duplicate destination register");
			w0 = w0 & 0xFF | 0xC08000;	/* both write */
			w0 |= eax & 0x1f00 | (eay & 0x300) << 5 | (eay & 0x1800) << 9 | regx << 18 | regy << 16;}
	|	x_or_y /*XMEM*/ ea /*ea_strange*/ ',' regs	regs ',' YMEM ea /*ea_strange*/
			{int eax = $2, eay = $8,
			     regx = ($4.flags & R_AB) ? $4.ab | 2 : $4.xreg,
			     regy = ($5.flags & R_AB) ? $5.ab | 2 : $5.yreg;
			hot_rreg = hot_nreg = hot_mreg = -1;
			if((eax & 0x400) == (eay & 0x400))
				yyerror("registers must be in opposite halves");
			if(!($4.flags & (R_AB | R_XREG)))
				yyerror("invalid X move register");
			if(!($5.flags & (R_AB | R_YREG)))
				yyerror("invalid Y move register");
			w0 = w0 & 0xFF | 0x808000;	/* X:write, Y:read */
			w0 |= eax & 0x1f00 | (eay & 0x300) << 5 | (eay & 0x1800) << 9 | regx << 18 | regy << 16;}
	|	regs ',' x_or_y /*XMEM*/ ea /*ea_strange*/	YMEM ea /*ea_strange*/ ',' regs
			{int eax = $4, eay = $6,
			     regx = ($1.flags & R_AB) ? $1.ab | 2 : $1.xreg,
			     regy = ($8.flags & R_AB) ? $8.ab | 2 : $8.yreg;
			hot_rreg = hot_nreg = hot_mreg = -1;
			if((eax & 0x400) == (eay & 0x400))
				yyerror("registers must be in opposite halves");
			if(!($1.flags & (R_AB | R_XREG)))
				yyerror("invalid X move register");
			if(!($8.flags & (R_AB | R_YREG)))
				yyerror("invalid Y move register");
	      		w0 = w0 & 0xFF | 0xC00000;	/* X:read, Y:write */
			w0 |= eax & 0x1f00 | (eay & 0x300) << 5 | (eay & 0x1800) << 9 | regx << 18 | regy << 16;}
	|	regs ',' x_or_y /*XMEM*/ ea /*ea_strange*/	regs ',' YMEM ea /*ea_strange*/
			{int eax = $4, eay = $8,
			     regx = ($1.flags & R_AB) ? $1.ab | 2 : $1.xreg,
			     regy = ($5.flags & R_AB) ? $5.ab | 2 : $5.yreg;
			hot_rreg = hot_nreg = hot_mreg = -1;
			if((eax & 0x400) == (eay & 0x400))
				yyerror("registers must be in opposite halves");
			if(!($1.flags & (R_AB | R_XREG)))
				yyerror("invalid X move register");
			if(!($5.flags & (R_AB | R_YREG)))
				yyerror("invalid Y move register");
	      		w0 = w0 & 0xFF | 0x800000;	/* both read */
			w0 |= eax & 0x1f00 | (eay & 0x300) << 5 | (eay & 0x1800) << 9 | regx << 18 | regy << 16;}
	;

/*%%%******* absolute address and immediate data fields ************/

num	:	CHEX
			{$$ = $1;}
	|	CDEC
			{$$ = $1;}
	|	FRAC
			{$$ = $1;}
	;

ix	:	'#' expr
			{$$ = $2; expr_seg = ANY;}
	|	'#' '<' expr
			{$$.val.i = n2int($3) & 0xFF;
			$$.type = INT;
			expr_seg = ANY;
			long_symbolic_expr = FALSE;}
	;

ix_long	:	'#' '>' expr
			{$$ = $3; expr_seg = ANY;}
	;

abs_addr
	:	expr
			{$$ = n2int($1);
			expr_seg = $1.seg;}
	;

abs_short_addr
	:	'<' expr
			{$$ = n2int($2);
			expr_seg = $2.seg;}
	;

io_short_addr
	:	SHL expr
			{$$ = n2int($2);
			expr_seg = $2.seg;} 
	;

num_or_sym
	:	num
			{$$ = $1;}
	|	SYM
			{$$ = sym_ref($1); free($1);}
	|	io_short_addr
			{$$.type = INT; $$.val.i = $1; $$.seg = expr_seg;}
	;

num_or_sym_expr
	:	num
			{$$ = $1; expr_seg = $1.seg;}
	|	SYM
			{$$ = sym_ref($1);
			free($1);
			long_symbolic_expr++;
			expr_seg = $$.seg;
			}
	|	CHAR
			{$$.type = INT; $$.val.i = $1 & 0xFFFFFF; expr_seg = $$.seg = ANY;}
	|	'*'
			{$$.type = INT; $$.val.i = pc; expr_seg = $$.seg = ANY;}
	|	OP_PI
			{$$.type = FLT; $$.val.f = acos(-1.0); expr_seg = $$.seg = ANY;}
	;

expr
	:	OP_INT '(' expr ')'
			{$$.type = INT; 
			if($3.type == INT)
				$$.val.i = $3.val.i;
			else
				$$.val.i = $3.val.f;
			$$.seg = $3.seg;
			}
	|	expr '|' expr
			{$$ = binary_op($1, '|', $3);}
	|	expr '^' expr
			{$$ = binary_op($1, '^', $3);}
	|	expr '&' expr
			{$$ = binary_op($1, '&', $3);}
	|	expr SHR expr
			{$$ = binary_op($1, SHR, $3);}
	|	expr SHL expr
			{$$ = binary_op($1, SHL, $3);}
	|	expr '-' expr
			{$$ = binary_op($1, '-', $3);}
	|	expr '+' expr
			{$$ = binary_op($1, '+', $3);}
	|	expr '%' expr
			{$$ = binary_op($1, '%', $3);}
	|	expr '/' expr
			{$$ = binary_op($1, '/', $3);}
	|	expr '*' expr
			{$$ = binary_op($1, '*', $3);}
	|	'-' expr %prec '~'
			{$$ = unary_op('-', $2);}
	|	'~' expr
			{$$ = unary_op('~', $2);}
	|	OP_SIN '(' expr ')'
			{$$ = unary_op('s', $3);}
	|	OP_COS '(' expr ')'
			{$$ = unary_op('c', $3);}
	|	OP_TAN '(' expr ')'
			{$$ = unary_op('t', $3);}
	|	OP_ASIN '(' expr ')'
			{$$ = unary_op('S', $3);}
	|	OP_ACOS '(' expr ')'
			{$$ = unary_op('C', $3);}
	|	OP_ATAN '(' expr ')'
			{$$ = unary_op('T', $3);}
	|	OP_EXP '(' expr ')'
			{$$ = unary_op('e', $3);}
	|	OP_LN '(' expr ')'
			{$$ = unary_op('l', $3);}
	|	OP_LOG '(' expr ')'
			{$$ = unary_op('L', $3);}
	|	OP_ABS '(' expr ')'
			{$$ = unary_op('a', $3);}
	|	OP_POW '(' expr ',' expr ')'
			{$$ = binary_op($3, 'p', $5);}
	|	'(' expr ')'
			{$$ = $2;}
	|	num_or_sym_expr
			{$$ = $1;}
	;

/*%%%****************** end ******************************/

%%

#include <stdio.h>
#include <setjmp.h>
#include <sys/signal.h>

int yydebug;

struct n binary_op(a1, op, a2)
struct n a1, a2;
int op;
{
	struct n result;
	int iarg1, iarg2;
	double farg1, farg2;

	if(a1.type == UNDEF || a2.type == UNDEF) {
		result.type = UNDEF;
		return result;
	}

	iarg1 = a1.type == INT ? a1.val.i : a1.val.f;
	iarg2 = a2.type == INT ? a2.val.i : a2.val.f;

	farg1 = a1.type == INT ? a1.val.i : a1.val.f;
	farg2 = a2.type == INT ? a2.val.i : a2.val.f;

	/* figure out target segment */

	if(a1.seg == a2.seg)
		result.seg = a1.seg;
	else if(a1.seg == ANY)
		result.seg = a2.seg;
	else if(a2.seg == ANY)
		result.seg = a1.seg;
	else
		result.seg = NONE;

	/* promote to float automatically */

	if(a1.type != a2.type) {
		if(a1.type == INT) {
			a1.val.f = a1.val.i;		/* truncate */
			a1.type = FLT;
		} else {
			a2.val.f = a2.val.i;		/* truncate */
		}
	}
	result.type = a1.type;

	/* do the op */

	switch(op) {
	case '+':
		if(result.type == INT) result.val.i = a1.val.i + a2.val.i;
		else result.val.f = a1.val.f + a2.val.f;
		break;
	case '-':
		if(result.type == INT) result.val.i = a1.val.i - a2.val.i;
		else result.val.f = a1.val.f - a2.val.f;
		break;
	case '*':
		if(result.type == INT) result.val.i = a1.val.i * a2.val.i;
		else result.val.f = a1.val.f * a2.val.f;
		break;
	case '/':
		if(result.type == INT) result.val.i = a1.val.i / a2.val.i;
		else result.val.f = a1.val.f / a2.val.f;
		break;
	case '%':
		result.val.i = iarg1 % iarg2;
		result.type = INT;
		break;
	case SHL:
		result.val.i = iarg1 << iarg2;
		result.type = INT;
		break;
	case SHR:
		result.val.i = iarg1 >> iarg2;
		result.type = INT;
		break;
	case '|':
		result.val.i = iarg1 | iarg2;
		result.type = INT;
		break;
	case '&':
		result.val.i = iarg1 & iarg2;
		result.type = INT;
		break;
	case '^':
		result.val.i = iarg1 ^ iarg2;
		result.type = INT;
		break;
	case 'p':
		result.val.f = pow(farg1, farg2);
		result.type = FLT;
	}

	return result;
}

jmp_buf unary_env;

void
sigfpu()
{
	longjmp(unary_env, 1);
}

char *unary_name(op)
{
	switch(op) {
	case 's':	return "sin";
	case 'c':	return "cos";
	case 't':	return "tan";
	case 'S':	return "asin";
	case 'C':	return "acos";
	case 'T':	return "atan";
	case 'e':	return "exp";
   	case 'l':	return "ln";
	case 'L':	return "log";
	case 'a':	return "abs";
	}
}

struct n unary_op(op, a1)
int op;
struct n a1;
{
	struct n result;
	void (*orig)();
	double farg;

	if(a1.type == UNDEF) {
		result.type = UNDEF;
		return result;
	}

	result.seg = a1.seg;

	/* do the op */

	orig = signal(SIGFPE, sigfpu);
	if(setjmp(unary_env)) {
		yyerror("floating point exception in function %s", unary_name(op));
		result.val.i = result.val.f = 0;
	} else {
		farg = a1.type == INT ? (double)a1.val.i : a1.val.f;
		switch(op) {
		case '-':
			result.type = a1.type;
			if(a1.type == INT) result.val.i = -a1.val.i;
			else result.val.f = -a1.val.f;
			break;
		case '~':
			result.type = a1.type;
			if(a1.type == INT) result.val.i = ~a1.val.i;
			else result.val.f = ~(int)a1.val.f;
			break;
		case 'a':
			result.type = a1.type;
			if(a1.type == INT) result.val.i = a1.val.i < 0 ? -a1.val.i : a1.val.i;
			else result.val.f = result.val.f = a1.val.f < 0 ? -a1.val.f : a1.val.f;
			break;
		case 's':
			result.type = FLT;
			result.val.f = sin(farg);
			break;
		case 'c':
			result.type = FLT;
			result.val.f = cos(farg);
			break;
		case 't':
			result.type = FLT;
			result.val.f = tan(farg);
			break;
		case 'S':
			result.type = FLT;
			result.val.f = asin(farg);
			break;
		case 'C':
			result.type = FLT;
			result.val.f = acos(farg);
			break;
		case 'T':
			result.type = FLT;
			result.val.f = atan(farg);
			break;
		case 'e':
			result.type = FLT;
			result.val.f = exp(farg);
			break;
	   	case 'l':
			result.type = FLT;
			result.val.f = log(farg);
			break;
		case 'L':
			result.type = FLT;
			result.val.f = log10(farg);
			break;
		}
	}
	signal(SIGFPE, orig);

	return result;
}

n2int(n)
struct n n;
{
	if(n.type == UNDEF)
		return UNDEF;
	else if(n.type == INT)
		return n.val.i;
	else
		return n.val.f;
}

n2frac(n)
struct n n;
{
	double adval = n.val.f >= 0.0 ? n.val.f : -n.val.f;

	if(n.type == UNDEF)
		return UNDEF;
	else if(n.type == INT)
		return n.val.i;
	else if(n.val.f == -1.0)
		return 0x800000;

	adval -= (double)(int)adval;
	adval *= (double)0x800000;
	adval += 0.5;

	if(n.val.f >= 0.0)
		return adval;
	else
		return -adval;
}

extern struct {int n; char *name;} tok_tab[];
extern int n_tok;

char *tok_print(tok)
int tok;
{
	int i;
	static char buf[32];

	if(tok == '1') {
		i = 1;
	}

	if(tok < 256) {
		sprintf(buf, tok < ' ' ? "\\z%02X" : "%c", tok & 0xFF);
		return buf;
	} else {
		for(i = 0; i < n_tok; i++) {
			if(tok == tok_tab[i].n)
				return tok_tab[i].name;
		}
	}
	return "*bogus*";
}

yyerror(s, a0, a1, a2, a3)
char *s, *a0, *a1, *a2, *a3;
{
	extern int error;
	char buf[1024];

	error++;
	sprintf(buf, s, a0, a1, a2, a3);

	if(pass == 2) {
		fprintf(stderr, "%s: line %d: %s (tok=%s)\n", curfile, curline,
			buf, tok_print(yychar));
		fprintf(stderr, "%s\n", cur_line);
		printf("%s: line %d: %s (tok=%s)\n", curfile, curline,
			buf, tok_print(yychar));
	}
}

yywarning(s, a0, a1, a2, a3)
char *s, *a0, *a1, *a2, *a3;
{
	extern int warning;
	char buf[1024];

	warning++;
	sprintf(buf, s, a0, a1, a2, a3);

	if(pass == 2) {
		fprintf(stderr, "%s: line %d: %s (tok=%s)\n", curfile, curline,
			buf, tok_print(yychar));
		fprintf(stderr, "%s\n", cur_line);
		printf("%s: line %d: %s (tok=%s)\n", curfile, curline,
			buf, tok_print(yychar));
	}
}

char *luntab(s)
char *s;
{
	static char buf[1024];
	int p;

	strcpy(buf, s);

	untab(buf);
	p = strlen(buf);

	if(buf[p - 1] == '\n')
		buf[p - 1] = '\0';

	return buf;
}
