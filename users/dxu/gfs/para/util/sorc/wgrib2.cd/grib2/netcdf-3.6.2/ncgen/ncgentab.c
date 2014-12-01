#ifndef lint
static char const 
yyrcsid[] = "$FreeBSD: src/usr.bin/yacc/skeleton.c,v 1.28 2000/01/17 02:04:06 bde Exp $";
#endif
#include <stdlib.h>
#define YYBYACC 1
#define YYMAJOR 1
#define YYMINOR 9
#define YYLEX yylex()
#define YYEMPTY -1
#define yyclearin (yychar=(YYEMPTY))
#define yyerrok (yyerrflag=0)
#define YYRECOVERING() (yyerrflag!=0)
static int yygrowstack();
#define YYPREFIX "yy"
#line 10 "ncgen.y"
#ifndef lint
static char SccsId[] = "$Id: ncgentab.c,v 1.15 2005/02/17 23:24:25 ed Exp $";
#endif
#include        <string.h>
#include	<stdlib.h>
#include	<netcdf.h>
#include 	"generic.h"
#include        "ncgen.h"
#include	"genlib.h"	/* for grow_darray() et al */

typedef struct Symbol {		/* symbol table entry */
	char    	*name;
	struct Symbol   *next;
	unsigned	is_dim : 1;	/* appears as netCDF dimension */
	unsigned	is_var : 1;	/* appears as netCDF variable */
	unsigned	is_att : 1;	/* appears as netCDF attribute */
	int             dnum;	        /* handle as a dimension */
	int             vnum;	        /* handle as a variable */
	} *YYSTYPE1;

/* True if string a equals string b*/
#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)
#define NC_UNSPECIFIED ((nc_type)0)	/* unspecified (as yet) type */

#define YYSTYPE YYSTYPE1
YYSTYPE symlist;		/* symbol table: linked list */

extern int derror_count;	/* counts errors in netcdf definition */
extern int lineno;		/* line number for error messages */

static int not_a_string;	/* whether last constant read was a string */
static char termstring[MAXTRST]; /* last terminal string read */
static double double_val;	/* last double value read */
static float float_val;		/* last float value read */
static int int_val;		/* last int value read */
static short short_val;		/* last short value read */
static char char_val;		/* last char value read */
static signed char byte_val;	/* last byte value read */

static nc_type type_code;	/* holds declared type for variables */
static nc_type atype_code;	/* holds derived type for attributes */
static char *netcdfname;	/* to construct netcdf file name */
static void *att_space;		/* pointer to block for attribute values */
static nc_type valtype;		/* type code for list of attribute values  */

static char *char_valp;		/* pointers used to accumulate data values */
static signed char *byte_valp;
static short *short_valp;
static int *int_valp;
static float *float_valp;
static double *double_valp;
static void *rec_cur;		/* pointer to where next data value goes */
static void *rec_start;		/* start of space for data */
#line 71 "y.tab.c"
#define YYERRCODE 256
#define NC_UNLIMITED_K 257
#define BYTE_K 258
#define CHAR_K 259
#define SHORT_K 260
#define INT_K 261
#define FLOAT_K 262
#define DOUBLE_K 263
#define IDENT 264
#define TERMSTRING 265
#define BYTE_CONST 266
#define CHAR_CONST 267
#define SHORT_CONST 268
#define INT_CONST 269
#define FLOAT_CONST 270
#define DOUBLE_CONST 271
#define DIMENSIONS 272
#define VARIABLES 273
#define NETCDF 274
#define DATA 275
#define FILLVALUE 276
const short yylhs[] = {                                        -1,
    2,    5,    0,    1,    1,    6,    6,    7,    7,    8,
    8,    8,    9,   10,    3,    3,    3,   11,   11,   13,
   13,   12,   12,   16,   14,   17,   17,   17,   17,   17,
   17,   18,   18,   22,   19,   20,   21,   21,   23,   23,
   24,   26,   15,   25,   25,   28,   29,   27,   27,   30,
   31,   31,   31,   31,   31,   31,   31,    4,    4,    4,
   32,   32,   34,   33,   35,   35,   38,   36,   37,   37,
   37,   37,   37,   37,   37,   37,
};
const short yylen[] = {                                         2,
    0,    0,    8,    0,    2,    2,    3,    1,    3,    3,
    3,    3,    1,    1,    0,    2,    1,    2,    3,    1,
    1,    2,    3,    1,    2,    1,    1,    1,    1,    1,
    1,    1,    3,    0,    3,    1,    0,    3,    1,    3,
    1,    0,    4,    3,    2,    1,    1,    1,    3,    1,
    1,    1,    1,    1,    1,    1,    1,    0,    2,    1,
    2,    3,    0,    4,    1,    3,    0,    2,    1,    1,
    1,    1,    1,    1,    1,    1,
};
const short yydefred[] = {                                      0,
    0,    0,    1,    0,    0,    0,   14,    0,    0,    8,
    0,   13,   36,    0,    0,    2,    0,   24,    0,   46,
   42,    0,    0,    6,    0,    0,   26,   27,   28,   29,
   30,   31,    0,    0,   20,   21,    0,   47,   45,    0,
    0,   22,    0,    0,    7,    9,   12,   10,   11,    0,
   18,    0,   32,   34,    0,    0,   23,    0,   44,   19,
    0,    0,   63,    0,    0,    3,   52,   53,   51,   54,
   55,   56,   57,    0,   48,   50,   33,    0,   35,    0,
    0,   61,    0,   41,    0,   39,   67,   62,   49,    0,
   38,    0,   65,    0,   40,   67,   70,   71,   69,   72,
   73,   74,   75,   76,   68,   66,
};
const short yydgoto[] = {                                       2,
    6,    4,   16,   56,   40,    8,    9,   10,   11,   12,
   33,   17,   34,   35,   18,   19,   37,   52,   53,   20,
   79,   62,   85,   86,   21,   43,   74,   22,   39,   75,
   76,   64,   65,   80,   92,   93,  105,   94,
};
const short yysindex[] = {                                   -261,
 -109,    0,    0, -253, -242,  -54,    0, -242,  -35,    0,
  -22,    0,    0,  -38, -224,    0,  -56,    0,  -36,    0,
    0,  -17,  -34,    0, -242, -241,    0,    0,    0,    0,
    0,    0,  -38,   -8,    0,    0, -222,    0,    0, -223,
   -5,    0,   -4, -224,    0,    0,    0,    0,    0,   -3,
    0,    9,    0,    0, -222,  -70,    0, -221,    0,    0,
 -222,   18,    0, -222,    1,    0,    0,    0,    0,    0,
    0,    0,    0,   15,    0,    0,    0, -242,    0,    2,
    3,    0, -221,    0,  -26,    0,    0,    0,    0, -242,
    0,   17,    0, -233,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,
};
const short yyrindex[] = {                                      0,
    0,    0,    0,  -58,    0, -122,    0,  -57,    0,    0,
    0,    0,    0,    0,    0,    0, -120,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0, -119,    0,    0,    0,    0,    0,    0,  -60,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    5,    0,    0,  -59,    0,    0,    0,    0,    0,
    0,  -33,    0,  -55,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   10,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   12,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,
};
const short yygindex[] = {                                      0,
    0,    0,    0,    0,    0,    0,   64,   48,    0,  -61,
    0,    0,   41,    0,   -6,   58,    0,    0,   16,  -30,
    0,    0,    0,  -14,    0,    0,    0,  -43,   34,   -2,
    0,    0,   19,    0,    0,  -16,    0,    0,
};
#define YYTABLESIZE 226
const short yytable[] = {                                       4,
    5,   15,   15,   15,   17,   16,   54,   36,   25,   25,
   37,   63,    1,    3,   91,   47,   84,   90,    5,   15,
   63,    7,   42,   24,   45,   37,   36,   48,   84,   49,
   54,   97,   98,   99,  100,  101,  102,  103,   26,   38,
   44,   13,  104,   67,   68,   69,   70,   71,   72,   73,
   51,   55,   61,   57,   66,   60,   58,   78,   83,   82,
   96,   88,   87,   25,   58,   60,    4,    5,   43,   59,
   64,   23,   46,   50,   41,   95,   77,   59,    0,  106,
   89,    0,   81,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   15,    0,   17,   16,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    4,    0,   13,    0,   13,
    0,    0,    0,    0,    4,    5,    4,    5,   14,   27,
   28,   29,   30,   31,   32,   13,
};
const short yycheck[] = {                                      58,
   58,   58,  125,   58,  125,  125,   37,   14,   44,   44,
   44,   55,  274,  123,   41,  257,   78,   44,  272,   58,
   64,  264,   59,   59,   59,   59,   33,  269,   90,  271,
   61,  265,  266,  267,  268,  269,  270,  271,   61,  264,
   58,  264,  276,  265,  266,  267,  268,  269,  270,  271,
   59,  275,   44,   59,  125,   59,   61,   40,   44,   59,
   44,   59,   61,   59,  125,  125,  125,  125,   59,  125,
   59,    8,   25,   33,   17,   90,   61,   44,   -1,   96,
   83,   -1,   64,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  275,   -1,  275,  275,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  264,   -1,  264,   -1,  264,
   -1,   -1,   -1,   -1,  273,  273,  275,  275,  273,  258,
  259,  260,  261,  262,  263,  264,
};
#define YYFINAL 2
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 276
#if YYDEBUG
const char * const yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,"'('","')'",0,0,"','",0,0,0,0,0,0,0,0,0,0,0,0,0,"':'","';'",0,"'='",
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'{'",0,"'}'",0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
"NC_UNLIMITED_K","BYTE_K","CHAR_K","SHORT_K","INT_K","FLOAT_K","DOUBLE_K",
"IDENT","TERMSTRING","BYTE_CONST","CHAR_CONST","SHORT_CONST","INT_CONST",
"FLOAT_CONST","DOUBLE_CONST","DIMENSIONS","VARIABLES","NETCDF","DATA",
"FILLVALUE",
};
const char * const yyrule[] = {
"$accept : ncdesc",
"$$1 :",
"$$2 :",
"ncdesc : NETCDF '{' $$1 dimsection vasection $$2 datasection '}'",
"dimsection :",
"dimsection : DIMENSIONS dimdecls",
"dimdecls : dimdecline ';'",
"dimdecls : dimdecls dimdecline ';'",
"dimdecline : dimdecl",
"dimdecline : dimdecline ',' dimdecl",
"dimdecl : dimd '=' INT_CONST",
"dimdecl : dimd '=' DOUBLE_CONST",
"dimdecl : dimd '=' NC_UNLIMITED_K",
"dimd : dim",
"dim : IDENT",
"vasection :",
"vasection : VARIABLES vadecls",
"vasection : gattdecls",
"vadecls : vadecl ';'",
"vadecls : vadecls vadecl ';'",
"vadecl : vardecl",
"vadecl : attdecl",
"gattdecls : gattdecl ';'",
"gattdecls : gattdecls gattdecl ';'",
"gattdecl : attdecl",
"vardecl : type varlist",
"type : BYTE_K",
"type : CHAR_K",
"type : SHORT_K",
"type : INT_K",
"type : FLOAT_K",
"type : DOUBLE_K",
"varlist : varspec",
"varlist : varlist ',' varspec",
"$$3 :",
"varspec : var $$3 dimspec",
"var : IDENT",
"dimspec :",
"dimspec : '(' dimlist ')'",
"dimlist : vdim",
"dimlist : dimlist ',' vdim",
"vdim : dim",
"$$4 :",
"attdecl : att $$4 '=' attvallist",
"att : avar ':' attr",
"att : ':' attr",
"avar : var",
"attr : IDENT",
"attvallist : aconst",
"attvallist : attvallist ',' aconst",
"aconst : attconst",
"attconst : CHAR_CONST",
"attconst : TERMSTRING",
"attconst : BYTE_CONST",
"attconst : SHORT_CONST",
"attconst : INT_CONST",
"attconst : FLOAT_CONST",
"attconst : DOUBLE_CONST",
"datasection :",
"datasection : DATA datadecls",
"datasection : DATA",
"datadecls : datadecl ';'",
"datadecls : datadecls datadecl ';'",
"$$5 :",
"datadecl : avar $$5 '=' constlist",
"constlist : dconst",
"constlist : constlist ',' dconst",
"$$6 :",
"dconst : $$6 const",
"const : CHAR_CONST",
"const : TERMSTRING",
"const : BYTE_CONST",
"const : SHORT_CONST",
"const : INT_CONST",
"const : FLOAT_CONST",
"const : DOUBLE_CONST",
"const : FILLVALUE",
};
#endif
#ifndef YYSTYPE
typedef int YYSTYPE;
#endif
#if YYDEBUG
#include <stdio.h>
#endif
#ifdef YYSTACKSIZE
#undef YYMAXDEPTH
#define YYMAXDEPTH YYSTACKSIZE
#else
#ifdef YYMAXDEPTH
#define YYSTACKSIZE YYMAXDEPTH
#else
#define YYSTACKSIZE 10000
#define YYMAXDEPTH 10000
#endif
#endif
#define YYINITSTACKSIZE 200
int yydebug;
int yynerrs;
int yyerrflag;
int yychar;
short *yyssp;
YYSTYPE *yyvsp;
YYSTYPE yyval;
YYSTYPE yylval;
short *yyss;
short *yysslim;
YYSTYPE *yyvs;
int yystacksize;
#line 767 "ncgen.y"

/* PROGRAMS */

#ifdef vms
void
#else
int
#endif
yyerror(	/* called for yacc syntax error */
     char *s)
{
	derror(s);
#ifndef vms
	return -1;
#endif
}

/* undefine yywrap macro, in case we are using bison instead of yacc */
#ifdef yywrap
#undef yywrap
#endif

int
yywrap(void)			/* returns 1 on EOF if no more input */
{
    return  1;
}


/* Symbol table operations for ncgen tool */

YYSTYPE lookup(       /* find sname in symbol table (linear search) */
	const char *sname)
{
    YYSTYPE sp;
    for (sp = symlist; sp != (YYSTYPE) 0; sp = sp -> next)
	if (STREQ(sp -> name, sname)) {
	    return sp;
	}
    return 0;			/* 0 ==> not found */
}

YYSTYPE install(  /* install sname in symbol table */
	const char *sname)
{
    YYSTYPE sp;

    sp = (YYSTYPE) emalloc (sizeof (struct Symbol));
    sp -> name = (char *) emalloc (strlen (sname) + 1);/* +1 for '\0' */
    (void) strcpy (sp -> name, sname);
    sp -> next = symlist;	/* put at front of list */
    sp -> is_dim = 0;
    sp -> is_var = 0;
    sp -> is_att = 0;
    symlist = sp;
    return sp;
}

void
clearout(void)	/* reset symbol table to empty */
{
    YYSTYPE sp, tp;
    for (sp = symlist; sp != (YYSTYPE) 0;) {
	tp = sp -> next;
	free (sp -> name);
	free ((char *) sp);
	sp = tp;
    }
    symlist = 0;
}

/* get lexical input routine generated by lex  */
#include "ncgenyy.c"
#line 418 "y.tab.c"
/* allocate initial stack or double stack size, up to YYMAXDEPTH */
static int yygrowstack()
{
    int newsize, i;
    short *newss;
    YYSTYPE *newvs;

    if ((newsize = yystacksize) == 0)
        newsize = YYINITSTACKSIZE;
    else if (newsize >= YYMAXDEPTH)
        return -1;
    else if ((newsize *= 2) > YYMAXDEPTH)
        newsize = YYMAXDEPTH;
    i = yyssp - yyss;
    newss = yyss ? (short *)realloc(yyss, newsize * sizeof *newss) :
      (short *)malloc(newsize * sizeof *newss);
    if (newss == NULL)
        return -1;
    yyss = newss;
    yyssp = newss + i;
    newvs = yyvs ? (YYSTYPE *)realloc(yyvs, newsize * sizeof *newvs) :
      (YYSTYPE *)malloc(newsize * sizeof *newvs);
    if (newvs == NULL)
        return -1;
    yyvs = newvs;
    yyvsp = newvs + i;
    yystacksize = newsize;
    yysslim = yyss + newsize - 1;
    return 0;
}

#define YYABORT goto yyabort
#define YYREJECT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR goto yyerrlab

#ifndef YYPARSE_PARAM
#if defined(__cplusplus) || __STDC__
#define YYPARSE_PARAM_ARG void
#define YYPARSE_PARAM_DECL
#else	/* ! ANSI-C/C++ */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif	/* ANSI-C/C++ */
#else	/* YYPARSE_PARAM */
#ifndef YYPARSE_PARAM_TYPE
#define YYPARSE_PARAM_TYPE void *
#endif
#if defined(__cplusplus) || __STDC__
#define YYPARSE_PARAM_ARG YYPARSE_PARAM_TYPE YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else	/* ! ANSI-C/C++ */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL YYPARSE_PARAM_TYPE YYPARSE_PARAM;
#endif	/* ANSI-C/C++ */
#endif	/* ! YYPARSE_PARAM */

int
yyparse (YYPARSE_PARAM_ARG)
    YYPARSE_PARAM_DECL
{
    register int yym, yyn, yystate;
#if YYDEBUG
    register const char *yys;

    if ((yys = getenv("YYDEBUG")))
    {
        yyn = *yys;
        if (yyn >= '0' && yyn <= '9')
            yydebug = yyn - '0';
    }
#endif

    yynerrs = 0;
    yyerrflag = 0;
    yychar = (-1);

    if (yyss == NULL && yygrowstack()) goto yyoverflow;
    yyssp = yyss;
    yyvsp = yyvs;
    *yyssp = yystate = 0;

yyloop:
    if ((yyn = yydefred[yystate])) goto yyreduce;
    if (yychar < 0)
    {
        if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, reading %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
    }
    if ((yyn = yysindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: state %d, shifting to state %d\n",
                    YYPREFIX, yystate, yytable[yyn]);
#endif
        if (yyssp >= yysslim && yygrowstack())
        {
            goto yyoverflow;
        }
        *++yyssp = yystate = yytable[yyn];
        *++yyvsp = yylval;
        yychar = (-1);
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }
    if ((yyn = yyrindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
        yyn = yytable[yyn];
        goto yyreduce;
    }
    if (yyerrflag) goto yyinrecovery;
#if defined(lint) || defined(__GNUC__)
    goto yynewerror;
#endif
yynewerror:
    yyerror("syntax error");
#if defined(lint) || defined(__GNUC__)
    goto yyerrlab;
#endif
yyerrlab:
    ++yynerrs;
yyinrecovery:
    if (yyerrflag < 3)
    {
        yyerrflag = 3;
        for (;;)
        {
            if ((yyn = yysindex[*yyssp]) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && yycheck[yyn] == YYERRCODE)
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: state %d, error recovery shifting\
 to state %d\n", YYPREFIX, *yyssp, yytable[yyn]);
#endif
                if (yyssp >= yysslim && yygrowstack())
                {
                    goto yyoverflow;
                }
                *++yyssp = yystate = yytable[yyn];
                *++yyvsp = yylval;
                goto yyloop;
            }
            else
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: error recovery discarding state %d\n",
                            YYPREFIX, *yyssp);
#endif
                if (yyssp <= yyss) goto yyabort;
                --yyssp;
                --yyvsp;
            }
        }
    }
    else
    {
        if (yychar == 0) goto yyabort;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, error recovery discards token %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
        yychar = (-1);
        goto yyloop;
    }
yyreduce:
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: state %d, reducing by rule %d (%s)\n",
                YYPREFIX, yystate, yyn, yyrule[yyn]);
#endif
    yym = yylen[yyn];
    yyval = yyvsp[1-yym];
    switch (yyn)
    {
case 1:
#line 97 "ncgen.y"
{ init_netcdf(); }
break;
case 2:
#line 100 "ncgen.y"
{
		       if (derror_count == 0)
			 define_netcdf(netcdfname);
		       if (derror_count > 0)
			   exit(6);
		   }
break;
case 3:
#line 108 "ncgen.y"
{
		       if (derror_count == 0)
			 close_netcdf();
		   }
break;
case 10:
#line 123 "ncgen.y"
{ if (int_val <= 0)
			 derror("dimension length must be positive");
		     dims[ndims].size = int_val;
		     ndims++;
		   }
break;
case 11:
#line 129 "ncgen.y"
{ /* for rare case where 2^31 < dimsize < 2^32 */
		       if (double_val <= 0)
			 derror("dimension length must be positive");
		       if (double_val > 4294967295.0)
			 derror("dimension too large");
		       if (double_val - (size_t) double_val > 0)
			 derror("dimension length must be an integer");
		       dims[ndims].size = (size_t) double_val;
		       ndims++;
                   }
break;
case 12:
#line 140 "ncgen.y"
{  if (rec_dim != -1)
			 derror("only one NC_UNLIMITED dimension allowed");
		     rec_dim = ndims; /* the unlimited (record) dimension */
		     dims[ndims].size = NC_UNLIMITED;
		     ndims++;
		   }
break;
case 13:
#line 148 "ncgen.y"
{ if (yyvsp[0]->is_dim == 1) {
		        derror( "duplicate dimension declaration for %s",
		                yyvsp[0]->name);
		     }
	             yyvsp[0]->is_dim = 1;
		     yyvsp[0]->dnum = ndims;
		     /* make sure dims array will hold dimensions */
		     grow_darray(ndims,  /* must hold ndims+1 dims */
				 &dims); /* grow as needed */
		     dims[ndims].name = (char *) emalloc(strlen(yyvsp[0]->name)+1);
		     (void) strcpy(dims[ndims].name, yyvsp[0]->name);
		     /* name for use in generated Fortran and C variables */
		     dims[ndims].lname = decodify(yyvsp[0]->name);
		   }
break;
case 26:
#line 181 "ncgen.y"
{ type_code = NC_BYTE; }
break;
case 27:
#line 182 "ncgen.y"
{ type_code = NC_CHAR; }
break;
case 28:
#line 183 "ncgen.y"
{ type_code = NC_SHORT; }
break;
case 29:
#line 184 "ncgen.y"
{ type_code = NC_INT; }
break;
case 30:
#line 185 "ncgen.y"
{ type_code = NC_FLOAT; }
break;
case 31:
#line 186 "ncgen.y"
{ type_code = NC_DOUBLE; }
break;
case 34:
#line 192 "ncgen.y"
{
		    static struct vars dummyvar;

		    dummyvar.name = "dummy";
		    dummyvar.type = NC_DOUBLE;
		    dummyvar.ndims = 0;
		    dummyvar.dims = 0;
		    dummyvar.fill_value.doublev = NC_FILL_DOUBLE;
		    dummyvar.has_data = 0;

		    nvdims = 0;
		    /* make sure variable not re-declared */
		    if (yyvsp[0]->is_var == 1) {
		       derror( "duplicate variable declaration for %s",
		               yyvsp[0]->name);
		    }
	            yyvsp[0]->is_var = 1;
		    yyvsp[0]->vnum = nvars;
		    /* make sure vars array will hold variables */
		    grow_varray(nvars,  /* must hold nvars+1 vars */
				&vars); /* grow as needed */
		    vars[nvars] = dummyvar; /* to make Purify happy */
		    vars[nvars].name = (char *) emalloc(strlen(yyvsp[0]->name)+1);
		    (void) strcpy(vars[nvars].name, yyvsp[0]->name);
		    /* name for use in generated Fortran and C variables */
		    vars[nvars].lname = decodify(yyvsp[0]->name);
		    vars[nvars].type = type_code;
		    /* set default fill value.  You can override this with
		     * the variable attribute "_FillValue". */
		    nc_getfill(type_code, &vars[nvars].fill_value);
		    vars[nvars].has_data = 0; /* has no data (yet) */
		   }
break;
case 35:
#line 225 "ncgen.y"
{
		    vars[nvars].ndims = nvdims;
		    nvars++;
		   }
break;
case 41:
#line 239 "ncgen.y"
{
		    if (nvdims >= NC_MAX_VAR_DIMS) {
		       derror("%s has too many dimensions",vars[nvars].name);
		    }
		    if (yyvsp[0]->is_dim == 1)
		       dimnum = yyvsp[0]->dnum;
		    else {
		       derror( "%s is not declared as a dimension",
			       yyvsp[0]->name);
	               dimnum = ndims;
		    }
		    if (rec_dim != -1 && dimnum == rec_dim && nvdims != 0) {
		       derror("unlimited dimension must be first");
		    }
		    grow_iarray(nvdims, /* must hold nvdims+1 ints */
				&vars[nvars].dims); /* grow as needed */
		    vars[nvars].dims[nvdims] = dimnum;
                    nvdims++;
		   }
break;
case 42:
#line 260 "ncgen.y"
{
		       valnum = 0;
		       valtype = NC_UNSPECIFIED;
		       /* get a large block for attributes, realloc later */
		       att_space = emalloc(MAX_NC_ATTSIZE);
		       /* make all kinds of pointers point to it */
		       char_valp = (char *) att_space;
		       byte_valp = (signed char *) att_space;
		       short_valp = (short *) att_space;
		       int_valp = (int *) att_space;
		       float_valp = (float *) att_space;
		       double_valp = (double *) att_space;
		   }
break;
case 43:
#line 274 "ncgen.y"
{
		       {	/* check if duplicate attribute for this var */
			   int i;
			   for(i=0; i<natts; i++) { /* expensive */
			       if(atts[i].var == varnum &&
				  STREQ(atts[i].name,atts[natts].name)) {
				   derror("duplicate attribute %s:%s",
					  vars[varnum].name,atts[natts].name);
			       }
			   }
		       }
		       atts[natts].var = varnum ;
		       atts[natts].type = valtype;
		       atts[natts].len = valnum;
		       /* shrink space down to what was really needed */
		       att_space = erealloc(att_space, valnum*nctypesize(valtype));
		       atts[natts].val = att_space;
		       if (STREQ(atts[natts].name, _FillValue) &&
			   atts[natts].var != NC_GLOBAL) {
			   nc_putfill(atts[natts].type,
				       atts[natts].val,
				       &vars[atts[natts].var].fill_value);
			   if(atts[natts].type != vars[atts[natts].var].type) {
			       derror("variable %s: %s type mismatch",
				      vars[atts[natts].var].name, _FillValue);
			   }
		       }
		       natts++;
		   }
break;
case 45:
#line 306 "ncgen.y"
{
		    varnum = NC_GLOBAL;  /* handle of "global" attribute */
		   }
break;
case 46:
#line 312 "ncgen.y"
{ if (yyvsp[0]->is_var == 1)
		       varnum = yyvsp[0]->vnum;
		    else {
		      derror("%s not declared as a variable, fatal error",
			     yyvsp[0]->name);
		      YYABORT;
		      }
		   }
break;
case 47:
#line 322 "ncgen.y"
{
		       /* make sure atts array will hold attributes */
		       grow_aarray(natts,  /* must hold natts+1 atts */
				   &atts); /* grow as needed */
		       atts[natts].name = (char *) emalloc(strlen(yyvsp[0]->name)+1);
		       (void) strcpy(atts[natts].name,yyvsp[0]->name);
		       /* name for use in generated Fortran and C variables */
		       atts[natts].lname = decodify(yyvsp[0]->name);
		   }
break;
case 50:
#line 336 "ncgen.y"
{
		    if (valtype == NC_UNSPECIFIED)
		      valtype = atype_code;
		    if (valtype != atype_code)
		      derror("values for attribute must be all of same type");
		   }
break;
case 51:
#line 345 "ncgen.y"
{
		       atype_code = NC_CHAR;
		       *char_valp++ = char_val;
		       valnum++;
		   }
break;
case 52:
#line 351 "ncgen.y"
{
		       atype_code = NC_CHAR;
		       {
			   /* don't null-terminate attribute strings */
			   size_t len = strlen(termstring);
			   if (len == 0) /* need null if that's only value */
			       len = 1;
			   (void)strncpy(char_valp,termstring,len);
			   valnum += len;
			   char_valp += len;
		       }
		   }
break;
case 53:
#line 364 "ncgen.y"
{
		       atype_code = NC_BYTE;
		       *byte_valp++ = byte_val;
		       valnum++;
		   }
break;
case 54:
#line 370 "ncgen.y"
{
		       atype_code = NC_SHORT;
		       *short_valp++ = short_val;
		       valnum++;
		   }
break;
case 55:
#line 376 "ncgen.y"
{
		       atype_code = NC_INT;
		       *int_valp++ = int_val;
		       valnum++;
		   }
break;
case 56:
#line 382 "ncgen.y"
{
		       atype_code = NC_FLOAT;
		       *float_valp++ = float_val;
		       valnum++;
		   }
break;
case 57:
#line 388 "ncgen.y"
{
		       atype_code = NC_DOUBLE;
		       *double_valp++ = double_val;
		       valnum++;
		   }
break;
case 63:
#line 404 "ncgen.y"
{
		       valtype = vars[varnum].type; /* variable type */
		       valnum = 0;	/* values accumulated for variable */
		       vars[varnum].has_data = 1;
		       /* compute dimensions product */
		       var_size = nctypesize(valtype);
		       if (vars[varnum].ndims == 0) { /* scalar */
			   var_len = 1;
		       } else if (vars[varnum].dims[0] == rec_dim) {
			   var_len = 1; /* one record for unlimited vars */
		       } else {
			   var_len = dims[vars[varnum].dims[0]].size;
		       }
		       for(dimnum = 1; dimnum < vars[varnum].ndims; dimnum++)
			 var_len = var_len*dims[vars[varnum].dims[dimnum]].size;
		       /* allocate memory for variable data */
		       if (var_len*var_size != (size_t)(var_len*var_size)) {
			   derror("variable %s too large for memory",
				  vars[varnum].name);
			   exit(9);
		       }
		       rec_len = var_len;
		       rec_start = malloc ((size_t)(rec_len*var_size));
		       if (rec_start == 0) {
			   derror ("out of memory\n");
			   exit(3);
		       }
		       rec_cur = rec_start;
		       switch (valtype) {
			 case NC_CHAR:
			   char_valp = (char *) rec_start;
			   break;
			 case NC_BYTE:
			   byte_valp = (signed char *) rec_start;
			   break;
			 case NC_SHORT:
			   short_valp = (short *) rec_start;
			   break;
			 case NC_INT:
			   int_valp = (int *) rec_start;
			   break;
			 case NC_FLOAT:
			   float_valp = (float *) rec_start;
			   break;
			 case NC_DOUBLE:
			   double_valp = (double *) rec_start;
			   break;
		       }
		 }
break;
case 64:
#line 454 "ncgen.y"
{
		       if (valnum < var_len) { /* leftovers */
			   nc_fill(valtype,
				    var_len - valnum,
				    rec_cur,
				    vars[varnum].fill_value);
		       }
		       /* put out var_len values */
		       /* vars[varnum].nrecs = valnum / rec_len; */
		       vars[varnum].nrecs = var_len / rec_len;
		       if (derror_count == 0)
			   put_variable(rec_start);
		       free ((char *) rec_start);
		 }
break;
case 67:
#line 473 "ncgen.y"
{
		       if(valnum >= var_len) {
			   if (vars[varnum].dims[0] != rec_dim) { /* not recvar */
			       derror("too many values for this variable, %d >= %d",
				      valnum, var_len);
			       exit (4);
			   } else { /* a record variable, so grow data
				      container and increment var_len by
				      multiple of record size */
			       ptrdiff_t rec_inc = (char *)rec_cur
				   - (char *)rec_start;
			       var_len = rec_len * (1 + valnum / rec_len);
			       rec_start = erealloc(rec_start, var_len*var_size);
			       rec_cur = (char *)rec_start + rec_inc;
			       char_valp = (char *) rec_cur;
			       byte_valp = (signed char *) rec_cur;
			       short_valp = (short *) rec_cur;
			       int_valp = (int *) rec_cur;
			       float_valp = (float *) rec_cur;
			       double_valp = (double *) rec_cur;
			   }
		       }
		       not_a_string = 1;
                   }
break;
case 68:
#line 498 "ncgen.y"
{
		       if (not_a_string) {
			   switch (valtype) {
			     case NC_CHAR:
			       rec_cur = (void *) char_valp;
			       break;
			     case NC_BYTE:
			       rec_cur = (void *) byte_valp;
			       break;
			     case NC_SHORT:
			       rec_cur = (void *) short_valp;
			       break;
			     case NC_INT:
			       rec_cur = (void *) int_valp;
			       break;
			     case NC_FLOAT:
			       rec_cur = (void *) float_valp;
			       break;
			     case NC_DOUBLE:
			       rec_cur = (void *) double_valp;
			       break;
			   }
		       }
		   }
break;
case 69:
#line 525 "ncgen.y"
{
		       atype_code = NC_CHAR;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = char_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = char_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = char_val;
			   break;
			 case NC_INT:
			   *int_valp++ = char_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = char_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = char_val;
			   break;
		       }
		       valnum++;
		   }
break;
case 70:
#line 550 "ncgen.y"
{
		       not_a_string = 0;
		       atype_code = NC_CHAR;
		       {
			   size_t len = strlen(termstring);

			   if(valnum + len > var_len) {
			       if (vars[varnum].dims[0] != rec_dim) {
				   derror("too many values for this variable, %d>%d", 
					  valnum+len, var_len);
				   exit (5);
			       } else {/* a record variable so grow it */
				   ptrdiff_t rec_inc = (char *)rec_cur
				       - (char *)rec_start;
				   var_len += rec_len * (len + valnum - var_len)/rec_len;
				   rec_start = erealloc(rec_start, var_len*var_size);
				   rec_cur = (char *)rec_start + rec_inc;
				   char_valp = (char *) rec_cur;
			       }
			   }
			   switch (valtype) {
			     case NC_CHAR:
			       {
				   int ld;
				   size_t i, sl;
				   (void)strncpy(char_valp,termstring,len);
				   ld = vars[varnum].ndims-1;
				   if (ld > 0) {/* null-fill to size of last dim */
				       sl = dims[vars[varnum].dims[ld]].size;
				       for (i =len;i<sl;i++)
					   char_valp[i] = '\0';
				       if (sl < len)
					   sl = len;
				       valnum += sl;
				       char_valp += sl;
				   } else { /* scalar or 1D strings */
				       valnum += len;
				       char_valp += len;
				   }
				   rec_cur = (void *) char_valp;
			       }
			       break;
			     case NC_BYTE:
			     case NC_SHORT:
			     case NC_INT:
			     case NC_FLOAT:
			     case NC_DOUBLE:
			       derror("string value invalid for %s variable",
				      nctype(valtype));
			       break;
			   }
		       }
		   }
break;
case 71:
#line 604 "ncgen.y"
{
		       atype_code = NC_BYTE;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = byte_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = byte_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = byte_val;
			   break;
			 case NC_INT:
			   *int_valp++ = byte_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = byte_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = byte_val;
			   break;
		       }
		       valnum++;
		   }
break;
case 72:
#line 629 "ncgen.y"
{
		       atype_code = NC_SHORT;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = short_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = short_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = short_val;
			   break;
			 case NC_INT:
			   *int_valp++ = short_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = short_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = short_val;
			   break;
		       }
		       valnum++;
		   }
break;
case 73:
#line 654 "ncgen.y"
{
		       atype_code = NC_INT;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = int_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = int_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = int_val;
			   break;
			 case NC_INT:
			   *int_valp++ = int_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = int_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = int_val;
			   break;
		       }
		       valnum++;
		   }
break;
case 74:
#line 679 "ncgen.y"
{
		       atype_code = NC_FLOAT;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = float_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = float_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = float_val;
			   break;
			 case NC_INT:
			   *int_valp++ = float_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = float_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = float_val;
			   break;
		       }
		       valnum++;
		   }
break;
case 75:
#line 704 "ncgen.y"
{
		       atype_code = NC_DOUBLE;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = double_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = double_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = double_val;
			   break;
			 case NC_INT:
			   *int_valp++ = double_val;
			   break;
			 case NC_FLOAT:
			   if (double_val == NC_FILL_DOUBLE)
			     *float_valp++ = NC_FILL_FLOAT;
			   else
			     *float_valp++ = double_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = double_val;
			   break;
		       }
		       valnum++;
		   }
break;
case 76:
#line 732 "ncgen.y"
{
		       /* store fill_value */
		       switch (valtype) {
		       case NC_CHAR:
			   nc_fill(valtype, 1, (void *)char_valp++,
				   vars[varnum].fill_value);
			   break;
		       case NC_BYTE:
			   nc_fill(valtype, 1, (void *)byte_valp++,
				   vars[varnum].fill_value);
			   break;
		       case NC_SHORT:
			   nc_fill(valtype, 1, (void *)short_valp++,
				   vars[varnum].fill_value);
			   break;
		       case NC_INT:
			   nc_fill(valtype, 1, (void *)int_valp++,
				   vars[varnum].fill_value);
			   break;
		       case NC_FLOAT:
			   nc_fill(valtype, 1, (void *)float_valp++,
				   vars[varnum].fill_value);
			   break;
		       case NC_DOUBLE:
			   nc_fill(valtype, 1, (void *)double_valp++,
				   vars[varnum].fill_value);
			   break;
		       }
		       valnum++;
		   }
break;
#line 1294 "y.tab.c"
    }
    yyssp -= yym;
    yystate = *yyssp;
    yyvsp -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: after reduction, shifting from state 0 to\
 state %d\n", YYPREFIX, YYFINAL);
#endif
        yystate = YYFINAL;
        *++yyssp = YYFINAL;
        *++yyvsp = yyval;
        if (yychar < 0)
        {
            if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
            if (yydebug)
            {
                yys = 0;
                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
                if (!yys) yys = "illegal-symbol";
                printf("%sdebug: state %d, reading %d (%s)\n",
                        YYPREFIX, YYFINAL, yychar, yys);
            }
#endif
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    if ((yyn = yygindex[yym]) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yystate)
        yystate = yytable[yyn];
    else
        yystate = yydgoto[yym];
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: after reduction, shifting from state %d \
to state %d\n", YYPREFIX, *yyssp, yystate);
#endif
    if (yyssp >= yysslim && yygrowstack())
    {
        goto yyoverflow;
    }
    *++yyssp = yystate;
    *++yyvsp = yyval;
    goto yyloop;
yyoverflow:
    yyerror("yacc stack overflow");
yyabort:
    return (1);
yyaccept:
    return (0);
}

