/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         op_ruleparse
#define yylex           op_rulelex
#define yyerror         op_ruleerror
#define yydebug         op_ruledebug
#define yynerrs         op_rulenerrs

#define yylval          op_rulelval
#define yychar          op_rulechar

/* Copy the first part of user declarations.  */


/* Parser specification for operating rules 

   This file requires flex and bison to compile. The executables
   should be on path. In MSC projects, this file should be 
   associated with a custom build rule whose command is: 
      bison.exe -l -p $(InputName) -d $(InputName).y
      copy $(InputName)_tab.c $(InputName)_tab.cpp
   and output is:
      $(InputName)_tab.cpp
      $(InputName)_tab.h
*/
#include "oprule/parser/ParseSymbolManagement.h"			   
 
#define _SETD add_temp_symbol<DoubleNodePtr>
#define _SETB add_temp_symbol<BoolNodePtr>
#define _GETD(NDX) get_express_index()[NDX].dblval
#define _GETB(NDX) get_express_index()[NDX].boolval
#define _GETS(NDX) get_express_index()[NDX]




# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "op_rule.tab.h".  */
#ifndef YY_OP_RULE_OP_RULE_TAB_H_INCLUDED
# define YY_OP_RULE_OP_RULE_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int op_ruledebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    NAME = 258,
    IFNAME = 259,
    IFPARAM = 260,
    EXPRESS = 261,
    EXPRESSPARAM = 262,
    NAMEDVAL = 263,
    BOOLNAMEDVAL = 264,
    LT = 265,
    LE = 266,
    GT = 267,
    GE = 268,
    EQ = 269,
    NE = 270,
    MAX2 = 271,
    MIN2 = 272,
    MAX3 = 273,
    MIN3 = 274,
    PID = 275,
    IPID = 276,
    LOOKUP = 277,
    IFELSE = 278,
    ACCUMULATE = 279,
    PREDICT = 280,
    LINEAR = 281,
    QUAD = 282,
    SQRT = 283,
    LOG = 284,
    LN = 285,
    EXP = 286,
    ABS = 287,
    SET = 288,
    TO = 289,
    WHEN = 290,
    WHILE = 291,
    THEN = 292,
    RAMP = 293,
    STEP = 294,
    YEAR = 295,
    MONTH = 296,
    DAY = 297,
    HOUR = 298,
    MINDAY = 299,
    MIN = 300,
    DT = 301,
    SEASON = 302,
    DATETIME = 303,
    REFDATE = 304,
    REFTIME = 305,
    REFSEASON = 306,
    NUMBER = 307,
    QUOTEDSTRING = 308,
    ASSIGN = 309,
    FALSE = 310,
    TRUE = 311,
    OR = 312,
    AND = 313,
    NOT = 314,
    DEFINE = 315,
    NEG = 316
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{


   int intval;
   double dval;
   int stringval;
   int pnode;
   int pbnode;
   int dvectorndx;
   int transition;
   int trigval;
   int opaction;
   int opruleval;


};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE op_rulelval;

int op_ruleparse (void);

#endif /* !YY_OP_RULE_OP_RULE_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */



#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  90
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   757

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  74
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  23
/* YYNRULES -- Number of rules.  */
#define YYNRULES  96
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  267

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   316

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      68,    69,    63,    61,    71,    62,     2,    64,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    67,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    70,     2,    72,    65,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,    73,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    66
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   101,   101,   108,   116,   122,   128,   144,   158,   162,
     169,   172,   177,   188,   195,   196,   202,   208,   211,   221,
     230,   233,   236,   243,   245,   246,   247,   248,   253,   254,
     255,   256,   260,   264,   265,   269,   271,   273,   275,   277,
     279,   290,   291,   292,   293,   294,   295,   296,   297,   298,
     302,   306,   310,   311,   312,   313,   314,   316,   318,   323,
     333,   334,   335,   336,   337,   338,   339,   340,   341,   342,
     347,   359,   360,   361,   365,   368,   372,   379,   380,   383,
     384,   385,   391,   395,   396,   397,   398,   399,   400,   401,
     405,   424,   438,   439,   458,   469,   476
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "NAME", "IFNAME", "IFPARAM", "EXPRESS",
  "EXPRESSPARAM", "NAMEDVAL", "BOOLNAMEDVAL", "LT", "LE", "GT", "GE", "EQ",
  "NE", "MAX2", "MIN2", "MAX3", "MIN3", "PID", "IPID", "LOOKUP", "IFELSE",
  "ACCUMULATE", "PREDICT", "LINEAR", "QUAD", "SQRT", "LOG", "LN", "EXP",
  "ABS", "SET", "TO", "WHEN", "WHILE", "THEN", "RAMP", "STEP", "YEAR",
  "MONTH", "DAY", "HOUR", "MINDAY", "MIN", "DT", "SEASON", "DATETIME",
  "REFDATE", "REFTIME", "REFSEASON", "NUMBER", "QUOTEDSTRING", "ASSIGN",
  "FALSE", "TRUE", "OR", "AND", "NOT", "DEFINE", "'+'", "'-'", "'*'",
  "'/'", "'^'", "NEG", "';'", "'('", "')'", "'['", "','", "']'", "'t'",
  "$accept", "line", "reassignment", "oprule", "trigger", "action",
  "modelaction", "actionspec", "targetspec", "transitionspec", "term",
  "boolterm", "array", "unary", "expression", "boolexpression",
  "laggedval", "arglist", "arg", "date", "interface", "namedval",
  "namedboolval", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,    43,    45,    42,    47,    94,   316,    59,    40,    41,
      91,    44,    93,   116
};
# endif

#define YYPACT_NINF -73

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-73)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     262,   -36,   -45,   -73,   -48,   -73,   -16,   -73,   -73,    -6,
      20,    42,    62,    65,    89,   100,   109,   113,   114,   115,
     116,   117,   119,   126,     9,   -73,   -73,   -73,   -73,   -73,
     -73,   -73,   -73,   -73,   -23,   -11,   -73,   -73,   -73,   394,
     118,   328,    80,   -73,   129,    82,   -73,   179,   -73,   -73,
     -73,   141,   -42,   -73,   -73,   -73,   -73,   -73,   -73,   328,
     142,   214,   214,   118,   118,   118,   118,   118,   118,   118,
     394,   118,   118,   118,   118,   118,   118,   118,   -73,   -73,
     -73,   146,   394,   211,   -73,   118,    64,   -32,   178,   -47,
     -73,   -73,   394,   -30,   -30,   -73,   118,   180,   118,   118,
     118,   118,   118,   118,   -73,   118,   118,   118,   118,   118,
     -73,   -73,   394,   394,   -73,   152,   417,     3,   158,   175,
     -41,   -73,    12,   -29,   -18,   136,   396,   412,   424,   429,
     -50,   440,   445,    51,   589,   598,   607,   616,   625,   -73,
     -73,   -73,    -9,   -30,   190,   -73,    29,   192,   -73,    29,
      29,    29,    29,    29,    29,    64,    64,   183,   183,   -73,
     193,   -73,   -73,   -73,   -73,   200,    -2,   214,   -73,   214,
     -73,   118,   118,   118,   118,   118,   118,   185,   118,   118,
      30,   -73,   -73,   -73,   -73,   -73,   215,   187,   -73,   -73,
     -73,   -73,   -73,   634,   643,   456,   461,   472,   477,   207,
      73,   488,   167,   206,   217,   -73,   -73,   -73,   -73,   118,
     118,   118,   118,   -73,   -46,   -73,   118,   -73,   394,   212,
     237,   652,   661,   493,   504,   -73,    15,   670,   -40,   251,
     252,   -73,   -73,   118,   118,   -73,   246,   -73,   -73,   230,
     231,   509,   520,   -73,   -73,   118,   118,   525,   536,   118,
     118,   541,   552,   118,   118,   557,   568,   118,   118,   573,
     584,   118,   118,   679,   688,   -73,   -73
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,    91,     0,    94,     0,    92,    96,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,    84,    85,    86,    87,
      88,    89,    80,    77,    79,    82,    23,    29,    30,     0,
       0,     0,     0,     8,     0,     0,    14,     0,    41,    60,
      24,     0,     0,    27,    25,    95,    26,    31,     9,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    20,    78,
      81,     0,     0,     0,    69,     0,    47,     0,     0,     0,
       1,     7,     0,     0,     0,    12,     0,    19,     0,     0,
       0,     0,     0,     0,    11,     0,     0,     0,     0,     0,
       5,    10,     0,     0,     4,     0,     0,     0,     0,     0,
       0,    71,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    17,
      48,    28,    13,     0,    16,    15,    21,     0,    18,    64,
      61,    63,    62,    65,    66,    45,    46,    43,    44,    42,
      67,    68,     6,     3,     2,     0,     0,     0,    90,     0,
      93,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    35,    37,    38,    39,    36,     0,     0,    76,    74,
      75,    73,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    22,    70,    55,    54,     0,
       0,     0,     0,    32,     0,    34,     0,    51,     0,     0,
       0,     0,     0,     0,     0,    33,     0,     0,     0,     0,
       0,    57,    56,     0,     0,    40,     0,    49,    50,     0,
       0,     0,     0,    52,    53,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,    59
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -73,   -73,   -73,   242,   -73,   -39,   -73,   -73,   -73,   -73,
     -73,   -73,    98,   -73,     0,    19,   -73,   253,   -72,   -73,
     292,   -73,   -73
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    42,    43,    44,    95,    45,    46,    47,    97,   148,
      48,    49,   200,    50,    83,    89,    53,   120,   121,    54,
      55,    56,    57
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint16 yytable[] =
{
      51,   188,    87,    24,    93,    94,   225,   112,   113,    59,
     112,   113,   111,     3,     4,   112,   113,   112,   113,    52,
      61,   178,   141,    60,   199,   114,   167,    79,   168,   238,
     169,    58,   105,   106,   107,   108,   109,   139,   143,    80,
      86,    88,   171,   105,   106,   107,   108,   109,   112,   113,
     189,   190,    62,   172,   144,   145,   203,   204,    84,   116,
     112,   113,    63,   123,   124,   125,   126,   127,   128,   129,
     164,   131,   132,   133,   134,   135,   136,   137,   117,   167,
      90,   170,    88,   169,   235,   138,   236,   215,    64,   130,
     105,   106,   107,   108,   109,   191,   146,   192,   149,   150,
     151,   152,   153,   154,    87,   155,   156,   157,   158,   159,
      65,   142,   105,   106,   107,   108,   109,    92,    93,    94,
     181,    81,     3,     4,     5,     6,     7,   107,   108,   109,
      66,   160,   161,    67,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,   214,   215,    19,    20,    21,    22,
      23,    98,    99,   100,   101,   102,   103,    68,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    69,    35,
      36,   193,   194,   195,   196,   197,   198,    70,   201,   202,
      40,    71,    72,    73,    74,    75,    85,    76,    98,    99,
     100,   101,   102,   103,    77,   104,    91,   105,   106,   107,
     108,   109,   105,   106,   107,   108,   109,   173,   110,   221,
     222,   223,   224,    96,    60,   118,   227,   119,   147,   162,
     165,    98,    99,   100,   101,   102,   103,    94,   105,   106,
     107,   108,   109,   241,   242,   166,   217,   228,   218,   105,
     106,   107,   108,   109,   186,   247,   248,   140,   109,   251,
     252,   113,   187,   255,   256,   199,   206,   259,   260,   213,
     205,   263,   264,     1,   229,     2,     3,     4,     5,     6,
       7,     8,   105,   106,   107,   108,   109,   219,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,   220,   230,
      19,    20,    21,    22,    23,    24,   239,   240,   225,   243,
     244,   115,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,   226,    35,    36,   122,    78,    37,    38,     0,
       0,    39,     0,     0,    40,     0,     0,     0,     0,     0,
      41,    81,     3,     4,     5,     6,     7,     8,     0,     0,
       0,     0,     0,     0,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    19,    20,    21,    22,
      23,    24,     0,     0,     0,     0,     0,     0,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,     0,    35,
      36,     0,     0,    37,    38,     0,     0,    39,     0,     0,
      40,     0,     0,     0,     0,     0,    41,    81,     3,     4,
       5,     6,     7,     8,     0,     0,     0,     0,     0,     0,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    19,    20,    21,    22,    23,    98,    99,   100,
     101,   102,   103,     0,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,     0,    35,    36,     0,     0,    37,
      38,     0,     0,    39,     0,     0,    40,   105,   106,   107,
     108,   109,    82,     0,     0,     0,     0,   174,     0,     0,
       0,     0,     0,   105,   106,   107,   108,   109,   105,   106,
     107,   108,   109,   175,   163,   105,   106,   107,   108,   109,
     105,   106,   107,   108,   109,   176,     0,     0,     0,     0,
     177,   105,   106,   107,   108,   109,   105,   106,   107,   108,
     109,   179,     0,     0,     0,     0,   180,   105,   106,   107,
     108,   109,   105,   106,   107,   108,   109,   209,     0,     0,
       0,     0,   210,   105,   106,   107,   108,   109,   105,   106,
     107,   108,   109,   211,     0,     0,     0,     0,   212,   105,
     106,   107,   108,   109,   105,   106,   107,   108,   109,   216,
       0,     0,     0,     0,   233,   105,   106,   107,   108,   109,
     105,   106,   107,   108,   109,   234,     0,     0,     0,     0,
     245,   105,   106,   107,   108,   109,   105,   106,   107,   108,
     109,   246,     0,     0,     0,     0,   249,   105,   106,   107,
     108,   109,   105,   106,   107,   108,   109,   250,     0,     0,
       0,     0,   253,   105,   106,   107,   108,   109,   105,   106,
     107,   108,   109,   254,     0,     0,     0,     0,   257,   105,
     106,   107,   108,   109,   105,   106,   107,   108,   109,   258,
       0,     0,     0,     0,   261,   105,   106,   107,   108,   109,
     105,   106,   107,   108,   109,   262,     0,     0,   182,   105,
     106,   107,   108,   109,     0,     0,     0,   183,   105,   106,
     107,   108,   109,     0,     0,     0,   184,   105,   106,   107,
     108,   109,     0,     0,     0,   185,   105,   106,   107,   108,
     109,     0,     0,     0,   140,   105,   106,   107,   108,   109,
       0,     0,     0,   207,   105,   106,   107,   108,   109,     0,
       0,     0,   208,   105,   106,   107,   108,   109,     0,     0,
       0,   231,   105,   106,   107,   108,   109,     0,     0,     0,
     232,   105,   106,   107,   108,   109,     0,     0,     0,   237,
     105,   106,   107,   108,   109,     0,     0,     0,   265,   105,
     106,   107,   108,   109,     0,     0,     0,   266
};

static const yytype_int16 yycheck[] =
{
       0,     3,    41,    33,    36,    37,    52,    57,    58,    54,
      57,    58,    54,     4,     5,    57,    58,    57,    58,     0,
      68,    71,    69,    68,    70,    67,    67,    50,    69,    69,
      71,    67,    61,    62,    63,    64,    65,    69,    68,    50,
      40,    41,    71,    61,    62,    63,    64,    65,    57,    58,
      52,    53,    68,    71,    93,    94,    26,    27,    39,    59,
      57,    58,    68,    63,    64,    65,    66,    67,    68,    69,
      67,    71,    72,    73,    74,    75,    76,    77,    59,    67,
       0,    69,    82,    71,    69,    85,    71,    72,    68,    70,
      61,    62,    63,    64,    65,   167,    96,   169,    98,    99,
     100,   101,   102,   103,   143,   105,   106,   107,   108,   109,
      68,    92,    61,    62,    63,    64,    65,    35,    36,    37,
      69,     3,     4,     5,     6,     7,     8,    63,    64,    65,
      68,   112,   113,    68,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    71,    72,    28,    29,    30,    31,
      32,    10,    11,    12,    13,    14,    15,    68,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    68,    51,
      52,   171,   172,   173,   174,   175,   176,    68,   178,   179,
      62,    68,    68,    68,    68,    68,    68,    68,    10,    11,
      12,    13,    14,    15,    68,    54,    67,    61,    62,    63,
      64,    65,    61,    62,    63,    64,    65,    71,    67,   209,
     210,   211,   212,    34,    68,    73,   216,     3,    38,    67,
      62,    10,    11,    12,    13,    14,    15,    37,    61,    62,
      63,    64,    65,   233,   234,    60,    69,   218,    71,    61,
      62,    63,    64,    65,    52,   245,   246,    69,    65,   249,
     250,    58,    52,   253,   254,    70,    69,   257,   258,    52,
      45,   261,   262,     1,    52,     3,     4,     5,     6,     7,
       8,     9,    61,    62,    63,    64,    65,    71,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    71,    52,
      28,    29,    30,    31,    32,    33,    45,    45,    52,    69,
      69,    59,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,   214,    51,    52,    62,    24,    55,    56,    -1,
      -1,    59,    -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,
      68,     3,     4,     5,     6,     7,     8,     9,    -1,    -1,
      -1,    -1,    -1,    -1,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    -1,    -1,    28,    29,    30,    31,
      32,    33,    -1,    -1,    -1,    -1,    -1,    -1,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    -1,    51,
      52,    -1,    -1,    55,    56,    -1,    -1,    59,    -1,    -1,
      62,    -1,    -1,    -1,    -1,    -1,    68,     3,     4,     5,
       6,     7,     8,     9,    -1,    -1,    -1,    -1,    -1,    -1,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      -1,    -1,    28,    29,    30,    31,    32,    10,    11,    12,
      13,    14,    15,    -1,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    -1,    51,    52,    -1,    -1,    55,
      56,    -1,    -1,    59,    -1,    -1,    62,    61,    62,    63,
      64,    65,    68,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,    61,    62,    63,    64,    65,    61,    62,
      63,    64,    65,    71,    67,    61,    62,    63,    64,    65,
      61,    62,    63,    64,    65,    71,    -1,    -1,    -1,    -1,
      71,    61,    62,    63,    64,    65,    61,    62,    63,    64,
      65,    71,    -1,    -1,    -1,    -1,    71,    61,    62,    63,
      64,    65,    61,    62,    63,    64,    65,    71,    -1,    -1,
      -1,    -1,    71,    61,    62,    63,    64,    65,    61,    62,
      63,    64,    65,    71,    -1,    -1,    -1,    -1,    71,    61,
      62,    63,    64,    65,    61,    62,    63,    64,    65,    71,
      -1,    -1,    -1,    -1,    71,    61,    62,    63,    64,    65,
      61,    62,    63,    64,    65,    71,    -1,    -1,    -1,    -1,
      71,    61,    62,    63,    64,    65,    61,    62,    63,    64,
      65,    71,    -1,    -1,    -1,    -1,    71,    61,    62,    63,
      64,    65,    61,    62,    63,    64,    65,    71,    -1,    -1,
      -1,    -1,    71,    61,    62,    63,    64,    65,    61,    62,
      63,    64,    65,    71,    -1,    -1,    -1,    -1,    71,    61,
      62,    63,    64,    65,    61,    62,    63,    64,    65,    71,
      -1,    -1,    -1,    -1,    71,    61,    62,    63,    64,    65,
      61,    62,    63,    64,    65,    71,    -1,    -1,    69,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    69,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    69,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    69,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      69,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,     3,     4,     5,     6,     7,     8,     9,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    28,
      29,    30,    31,    32,    33,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    51,    52,    55,    56,    59,
      62,    68,    75,    76,    77,    79,    80,    81,    84,    85,
      87,    88,    89,    90,    93,    94,    95,    96,    67,    54,
      68,    68,    68,    68,    68,    68,    68,    68,    68,    68,
      68,    68,    68,    68,    68,    68,    68,    68,    94,    50,
      50,     3,    68,    88,    89,    68,    88,    79,    88,    89,
       0,    67,    35,    36,    37,    78,    34,    82,    10,    11,
      12,    13,    14,    15,    54,    61,    62,    63,    64,    65,
      67,    54,    57,    58,    67,    77,    88,    89,    73,     3,
      91,    92,    91,    88,    88,    88,    88,    88,    88,    88,
      89,    88,    88,    88,    88,    88,    88,    88,    88,    69,
      69,    69,    89,    68,    79,    79,    88,    38,    83,    88,
      88,    88,    88,    88,    88,    88,    88,    88,    88,    88,
      89,    89,    67,    67,    67,    62,    60,    67,    69,    71,
      69,    71,    71,    71,    71,    71,    71,    71,    71,    71,
      71,    69,    69,    69,    69,    69,    52,    52,     3,    52,
      53,    92,    92,    88,    88,    88,    88,    88,    88,    70,
      86,    88,    88,    26,    27,    45,    69,    69,    69,    71,
      71,    71,    71,    52,    71,    72,    71,    69,    71,    71,
      71,    88,    88,    88,    88,    52,    86,    88,    89,    52,
      52,    69,    69,    71,    71,    69,    71,    69,    69,    45,
      45,    88,    88,    69,    69,    71,    71,    88,    88,    71,
      71,    88,    88,    71,    71,    88,    88,    71,    71,    88,
      88,    71,    71,    88,    88,    69,    69
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    74,    75,    75,    75,    75,    75,    75,    75,    75,
      76,    76,    77,    78,    79,    79,    79,    79,    80,    80,
      81,    82,    83,    84,    84,    84,    84,    84,    85,    85,
      85,    85,    86,    86,    86,    87,    87,    87,    87,    87,
      87,    88,    88,    88,    88,    88,    88,    88,    88,    88,
      88,    88,    88,    88,    88,    88,    88,    88,    88,    88,
      89,    89,    89,    89,    89,    89,    89,    89,    89,    89,
      90,    91,    91,    91,    92,    92,    92,    93,    93,    93,
      93,    93,    93,    93,    93,    93,    93,    93,    93,    93,
      94,    94,    95,    95,    95,    95,    96
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     4,     4,     2,     2,     4,     2,     1,     2,
       2,     2,     2,     2,     1,     3,     3,     3,     3,     2,
       2,     2,     3,     1,     1,     1,     1,     1,     3,     1,
       1,     1,     2,     3,     2,     4,     4,     4,     4,     4,
       8,     1,     3,     3,     3,     3,     3,     2,     3,     8,
       8,     6,     9,     9,     6,     6,     8,     8,    20,    20,
       1,     3,     3,     3,     3,     3,     3,     3,     3,     2,
       6,     1,     3,     3,     3,     3,     3,     1,     2,     1,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     1,     1,     4,     1,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

    {
	   string dbkey=get_temp_symbol((yyvsp[-3].stringval)).stringval;
	   add_symbol(dbkey,_GETS((yyvsp[-1].pbnode)));
	   set_parsed_type(oprule::parser::BOOL_EXPRESS);
	   clear_string_list();
	   clear_temp_expr();
        }

    break;

  case 3:

    {
	   string dbkey=get_temp_symbol((yyvsp[-3].stringval)).stringval;
	   add_symbol(dbkey,_GETS((yyvsp[-1].pnode)));
	   apply_lagged_values(dbkey,_GETD((yyvsp[-1].pnode)));
	   set_parsed_type(oprule::parser::NUMERICAL_EXPRESS);
	   clear_string_list();
	   clear_temp_expr();
        }

    break;

  case 4:

    {
       string dbkey=EVAL_STR;
	   add_symbol(dbkey,_GETS((yyvsp[-1].pbnode)));
	   set_parsed_type(oprule::parser::BOOL_EXPRESS);
	   clear_string_list();	   
	    }

    break;

  case 5:

    {
       string dbkey=EVAL_STR;
	   add_symbol(dbkey,_GETS((yyvsp[-1].pnode)));
	   set_parsed_type(oprule::parser::NUMERICAL_EXPRESS);
	   clear_string_list();	   
	   }

    break;

  case 6:

    {
	    string rulename=get_temp_symbol((yyvsp[-3].stringval)).stringval;
	    OperatingRulePtr rulePtr = get_temp_symbol((yyvsp[-1].opruleval)).rule;
        rulePtr->setName(rulename);
		if (add_rule(rulename,rulePtr)){
		  set_parsed_type(oprule::parser::OP_RULE);  	    
        }else{
		  op_ruleerror(
		    ("Operating rule name " + rulename + " used more than once.").c_str());
          YYABORT;
		 }
	    clear_string_list();
	    clear_temp_expr();
	    clear_arg_map();

        }

    break;

  case 7:

    {
		static int ruleno;
        char num[16];
        //_itoa_s(ruleno++,num,16,10);  // -- windows only
 	snprintf(num,16,"%d",ruleno++); // -- linux equivalent FIXME: needs ifdef unix here
        OperatingRulePtr opPtr = get_temp_symbol((yyvsp[-1].opruleval)).rule;
        string rulename = string("OpRule") + num;
        opPtr->setName(rulename);
        add_rule(rulename,opPtr);
	    set_parsed_type(oprule::parser::OP_RULE);
	    clear_string_list();
	    clear_temp_expr();
	    clear_arg_map();
		}

    break;

  case 8:

    {               // really just captures error
        set_parsed_type(oprule::parser::REASSIGNMENT);
		}

    break;

  case 9:

    {
                cerr << "Error parsing expression." << endl;
				YYABORT;}

    break;

  case 10:

    {
	     op_ruleerror("reassignment of (boolean-valued) variable");
	                       }

    break;

  case 11:

    {
         op_ruleerror("reassignment of variable");
		                   }

    break;

  case 12:

    {
     OperationActionPtr  act     = get_temp_symbol((yyvsp[-1].opaction)).action;
     TriggerPtr trigger = get_temp_symbol((yyvsp[0].trigval)).trigger;
	 OperatingRulePtr op(new OperatingRule(act,trigger));
	 symbol s(op);
	 (yyval.opruleval) = add_temp_symbol(op);
     add_rule(EVAL_STR,op);
   }

    break;

  case 13:

    {
              TriggerPtr trigger(new ExpressionTrigger(_GETB((yyvsp[0].pbnode))->copy()));
              symbol s(trigger);
              (yyval.trigval)=add_temp_symbol(s);
			  }

    break;

  case 14:

    { (yyval.opaction)=(yyvsp[0].opaction); }

    break;

  case 15:

    { 
            OperationActionPtr firstPtr = get_temp_symbol((yyvsp[-2].opaction)).action;
            OperationActionPtr secondPtr = get_temp_symbol((yyvsp[0].opaction)).action;
            OperationActionPtr chain = chain_actions(firstPtr,secondPtr);
            (yyval.opaction) = add_temp_symbol(chain);
            }

    break;

  case 16:

    {
            OperationActionPtr firstPtr = get_temp_symbol((yyvsp[-2].opaction)).action;
            OperationActionPtr secondPtr = get_temp_symbol((yyvsp[0].opaction)).action;
            OperationActionPtr chain = group_actions(firstPtr,secondPtr);
            (yyval.opaction) = add_temp_symbol(chain);
            }

    break;

  case 17:

    { (yyval.opaction)=(yyvsp[-1].opaction); }

    break;

  case 18:

    { 
			 ModelInterface<double>::NodePtr ifc
			   =boost::static_pointer_cast<ModelInterface<double> >( _GETD((yyvsp[-2].pnode)) );
			 TransitionPtr transition = get_temp_symbol((yyvsp[0].transition)).transition;
			 OperationActionPtr act( new ModelAction<double>(ifc,
			                        _GETD((yyvsp[-1].pnode))->copy(),
			                        transition));
			 (yyval.opaction)=add_temp_symbol(act); //was ModelAction, also below was too
	}

    break;

  case 19:

    { 
			 ModelInterface<double>::NodePtr ifc
			   =boost::static_pointer_cast<ModelInterface<double> >( _GETD((yyvsp[-1].pnode)) );
			 TransitionPtr abrupt(new AbruptTransition());
			 OperationActionPtr act( new ModelAction<double>(ifc,_GETD((yyvsp[0].pnode))->copy(), abrupt));
			 (yyval.opaction)=add_temp_symbol(act);
			}

    break;

  case 20:

    {(yyval.pnode)=(yyvsp[0].pnode);}

    break;

  case 21:

    {(yyval.pnode)=(yyvsp[0].pnode);}

    break;

  case 22:

    {
                    TransitionPtr t(new LinearTransition((yyvsp[-1].dval)*60.));
                    (yyval.transition)=add_temp_symbol(t);
                    }

    break;

  case 23:

    { 
      (yyval.pnode) =_SETD(DoubleScalarNode::create((yyvsp[0].dval))); }

    break;

  case 24:

    { (yyval.pnode)=(yyvsp[0].pnode); }

    break;

  case 25:

    { (yyval.pnode)=(yyvsp[0].pnode); }

    break;

  case 26:

    { (yyval.pnode)=(yyvsp[0].pnode);}

    break;

  case 27:

    { (yyval.pnode)=(yyvsp[0].pnode);}

    break;

  case 28:

    { (yyval.pbnode) = (yyvsp[-1].pbnode);}

    break;

  case 29:

    { (yyval.pbnode)=_SETB(BoolScalarNode::create(false)); }

    break;

  case 30:

    { (yyval.pbnode)=_SETB(BoolScalarNode::create(true));  }

    break;

  case 31:

    { (yyval.pbnode) = (yyvsp[0].pbnode);}

    break;

  case 32:

    {
                  (yyval.dvectorndx) = add_array_vector();
                  get_array_vector((yyval.dvectorndx)).push_back((yyvsp[0].dval)); 
                }

    break;

  case 33:

    {get_array_vector((yyvsp[-2].dvectorndx)).push_back((yyvsp[0].dval));}

    break;

  case 35:

    {  
            (yyval.pnode)=_SETD(UnaryOpNode<sqrt_func>::create(_GETD((yyvsp[-1].pnode))));}

    break;

  case 36:

    {
           (yyval.pnode)=_SETD(UnaryOpNode<abs_func>::create(_GETD((yyvsp[-1].pnode))));}

    break;

  case 37:

    {
           (yyval.pnode)=_SETD(UnaryOpNode<log10_func>::create(_GETD((yyvsp[-1].pnode))));}

    break;

  case 38:

    {
           (yyval.pnode)=_SETD(UnaryOpNode<ln_func>::create(_GETD((yyvsp[-1].pnode))));}

    break;

  case 39:

    {
           (yyval.pnode)=_SETD(UnaryOpNode<exp_func>::create(_GETD((yyvsp[-1].pnode))));}

    break;

  case 40:

    {
           (yyval.pnode)=_SETD(LookupNode::create(_GETD((yyvsp[-5].pnode)), 
                                       get_array_vector((yyvsp[-3].dvectorndx)), 
                                       get_array_vector((yyvsp[-1].dvectorndx))));
           get_array_vector((yyvsp[-3].dvectorndx)).clear();
           get_array_vector((yyvsp[-1].dvectorndx)).clear();
           }

    break;

  case 41:

    { (yyval.pnode) = (yyvsp[0].pnode); }

    break;

  case 42:

    { (yyval.pnode)=_SETD(BinaryOpNode<power_func >::create(_GETD((yyvsp[-2].pnode)),_GETD((yyvsp[0].pnode))));}

    break;

  case 43:

    { (yyval.pnode)=_SETD(BinaryOpNode<multiplies<double> >::create(_GETD((yyvsp[-2].pnode)),_GETD((yyvsp[0].pnode))));}

    break;

  case 44:

    { (yyval.pnode)=_SETD(BinaryOpNode<divides<double> >::create(_GETD((yyvsp[-2].pnode)),_GETD((yyvsp[0].pnode))));}

    break;

  case 45:

    { (yyval.pnode)=_SETD(BinaryOpNode<plus<double> >::create(_GETD((yyvsp[-2].pnode)),_GETD((yyvsp[0].pnode))));}

    break;

  case 46:

    { (yyval.pnode)=_SETD(BinaryOpNode<minus<double> >::create(_GETD((yyvsp[-2].pnode)),_GETD((yyvsp[0].pnode))));}

    break;

  case 47:

    { (yyval.pnode)=_SETD(UnaryOpNode<negate<double> >::create(_GETD((yyvsp[0].pnode))));}

    break;

  case 48:

    {(yyval.pnode)=(yyvsp[-1].pnode);}

    break;

  case 49:

    {
                    (yyval.pnode)=_SETD(TernaryOpNode<double>::create(_GETB((yyvsp[-5].pbnode)),
                                                           _GETD((yyvsp[-3].pnode)),
                                                           _GETD((yyvsp[-1].pnode)))); }

    break;

  case 50:

    {
					(yyval.pnode)=_SETD(AccumulationNode<double>::create(_GETD((yyvsp[-5].pnode)),
					                                          _GETD((yyvsp[-3].pnode)),
					                                          _GETB((yyvsp[-1].pbnode))));}

    break;

  case 51:

    {
					(yyval.pnode)=_SETD(AccumulationNode<double>::create(_GETD((yyvsp[-3].pnode)),
					                                          _GETD((yyvsp[-1].pnode)),
					                                          BoolScalarNode::create(false))); }

    break;

  case 52:

    {(yyval.pnode)=_SETD(LinearExtrapolationNode<double>::create(_GETD((yyvsp[-6].pnode)), (yyvsp[-2].dval) * 60.));}

    break;

  case 53:

    {(yyval.pnode)=_SETD(QuadExtrapolationNode::create(_GETD((yyvsp[-6].pnode)), (yyvsp[-2].dval) * 60. ));}

    break;

  case 54:

    {(yyval.pnode)=_SETD(BinaryOpNode<min2 >::create(_GETD((yyvsp[-3].pnode)),_GETD((yyvsp[-1].pnode))));}

    break;

  case 55:

    {(yyval.pnode)=_SETD(BinaryOpNode<max2 >::create(_GETD((yyvsp[-3].pnode)),_GETD((yyvsp[-1].pnode))));}

    break;

  case 56:

    {
        (yyval.pnode)=_SETD(Min3Node<double>::create( _GETD((yyvsp[-5].pnode)),_GETD((yyvsp[-3].pnode)),_GETD((yyvsp[-1].pnode))));}

    break;

  case 57:

    {
        (yyval.pnode)=_SETD(Max3Node<double>::create( _GETD((yyvsp[-5].pnode)),_GETD((yyvsp[-3].pnode)),_GETD((yyvsp[-1].pnode))));}

    break;

  case 58:

    {  
        (yyval.pnode)=_SETD(PIDNode::create( _GETD((yyvsp[-17].pnode)),_GETD((yyvsp[-15].pnode)),_GETD((yyvsp[-13].pnode))->eval(),_GETD((yyvsp[-11].pnode))->eval(),_GETD((yyvsp[-9].pnode))->eval(),_GETD((yyvsp[-7].pnode))->eval(),
                                  _GETD((yyvsp[-5].pnode))->eval(),_GETD((yyvsp[-3].pnode))->eval(),_GETD((yyvsp[-1].pnode))->eval()));
        }

    break;

  case 59:

    {  
        (yyval.pnode)=_SETD(IncrementalPIDNode::create( _GETD((yyvsp[-17].pnode)),_GETD((yyvsp[-15].pnode)),_GETD((yyvsp[-13].pnode)),_GETD((yyvsp[-11].pnode))->eval(),_GETD((yyvsp[-9].pnode))->eval(),_GETD((yyvsp[-7].pnode))->eval(),
                                  _GETD((yyvsp[-5].pnode))->eval(),_GETD((yyvsp[-3].pnode))->eval(),_GETD((yyvsp[-1].pnode))->eval() ));
        }

    break;

  case 60:

    {(yyval.pbnode)= (yyvsp[0].pbnode);}

    break;

  case 61:

    { (yyval.pbnode)=_SETB(BinaryOpNode<less_equal<double> >::create(_GETD((yyvsp[-2].pnode)), _GETD((yyvsp[0].pnode))));}

    break;

  case 62:

    { (yyval.pbnode)=_SETB(BinaryOpNode<greater_equal<double> >::create(_GETD((yyvsp[-2].pnode)), _GETD((yyvsp[0].pnode))));}

    break;

  case 63:

    { (yyval.pbnode)=_SETB(BinaryOpNode<greater<double> >::create(_GETD((yyvsp[-2].pnode)), _GETD((yyvsp[0].pnode)))); }

    break;

  case 64:

    { (yyval.pbnode)=_SETB(BinaryOpNode<less<double> >::create(_GETD((yyvsp[-2].pnode)), _GETD((yyvsp[0].pnode)))); }

    break;

  case 65:

    { (yyval.pbnode)=_SETB(BinaryOpNode<equal_to<double> >::create(_GETD((yyvsp[-2].pnode)), _GETD((yyvsp[0].pnode)))); }

    break;

  case 66:

    { (yyval.pbnode)=_SETB(BinaryOpNode<not_equal_to<double> >::create(_GETD((yyvsp[-2].pnode)), _GETD((yyvsp[0].pnode)))); }

    break;

  case 67:

    { (yyval.pbnode)=_SETB(BinaryOpNode<logical_or<bool > >::create(_GETB((yyvsp[-2].pbnode)), _GETB((yyvsp[0].pbnode))));}

    break;

  case 68:

    { (yyval.pbnode)=_SETB(BinaryOpNode<logical_and<bool > >::create(_GETB((yyvsp[-2].pbnode)), _GETB((yyvsp[0].pbnode))));}

    break;

  case 69:

    { (yyval.pbnode)=_SETB(UnaryOpNode<logical_not<bool> >::create(_GETB((yyvsp[0].pbnode))));}

    break;

  case 70:

    { 
              LaggedExpressionNode<double>::NodePtr 
			     laggedNode=LaggedDoubleNode::create((int)(yyvsp[-1].dval));
               const string nm=get_temp_symbol((yyvsp[-5].stringval)).stringval;;
               get_lagged_vals().push_back(
			      make_pair(nm,laggedNode));
				(yyval.pnode)=_SETD(laggedNode);
				  }

    break;

  case 74:

    { 
                         get_arg_map()[get_temp_symbol((yyvsp[-2].stringval)).stringval]=ToString<double>((yyvsp[0].dval));
					   }

    break;

  case 75:

    { 
					     get_arg_map()[get_temp_symbol((yyvsp[-2].stringval)).stringval]=get_temp_symbol((yyvsp[0].stringval)).stringval;
						}

    break;

  case 76:

    { 
                        get_arg_map()[get_temp_symbol((yyvsp[-2].stringval)).stringval]=get_temp_symbol((yyvsp[0].stringval)).stringval;
                       }

    break;

  case 77:

    {(yyval.pnode)=_SETD(get_time_factory()->getDateTimeNode());}

    break;

  case 78:

    {(yyval.pnode)=_SETD(get_time_factory()->getDateTimeNode(get_temp_symbol((yyvsp[-1].stringval)).stringval,
                                                                 get_temp_symbol((yyvsp[0].stringval)).stringval));
                   }

    break;

  case 79:

    { (yyval.pnode)=_SETD(get_time_factory()->getDateTimeNode(get_temp_symbol((yyvsp[0].stringval)).stringval,"00:00"));}

    break;

  case 80:

    { (yyval.pnode)=_SETD(get_time_factory()->getSeasonNode()); }

    break;

  case 81:

    { int mon=month_to_int(get_temp_symbol((yyvsp[-1].stringval)).stringval.substr(2,3));
                       int day=padded_string_to_int(get_temp_symbol((yyvsp[-1].stringval)).stringval.substr(0,2));
                       int hour=padded_string_to_int(get_temp_symbol((yyvsp[0].stringval)).stringval.substr(2,3));
                       int min=padded_string_to_int(get_temp_symbol((yyvsp[0].stringval)).stringval.substr(3,2));
                       (yyval.pnode)=_SETD(get_time_factory()->getReferenceSeasonNode(mon,day,hour,min));
					   }

    break;

  case 82:

    { int mon=month_to_int(get_temp_symbol((yyvsp[0].stringval)).stringval.substr(2,3));
                       int day=padded_string_to_int(get_temp_symbol((yyvsp[0].stringval)).stringval.substr(0,2));
                       (yyval.pnode)=_SETD(get_time_factory()->getReferenceSeasonNode(mon,day,0,0));
					   }

    break;

  case 83:

    { (yyval.pnode)=_SETD(get_time_factory()->getYearNode()); }

    break;

  case 84:

    { (yyval.pnode)=_SETD(get_time_factory()->getMonthNode()); }

    break;

  case 85:

    { (yyval.pnode)=_SETD(get_time_factory()->getDayNode()); }

    break;

  case 86:

    { (yyval.pnode)=_SETD(get_time_factory()->getHourNode()); }

    break;

  case 87:

    { (yyval.pnode)=_SETD(get_time_factory()->getMinOfDayNode()); }

    break;

  case 88:

    { (yyval.pnode)=_SETD(get_time_factory()->getMinNode()); }

    break;

  case 89:

    { (yyval.pnode)=_SETD(get_time_factory()->getTimeStepNode()); }

    break;

  case 90:

    {
                         string ifname=get_temp_symbol((yyvsp[-3].stringval)).stringval;
                         get_arg_map()["modelname"]=ifname;
						 try{
						   (yyval.pnode)=_SETD(get_lookup()->getModelExpression(ifname,get_arg_map()));
						 }catch(oprule::parser::MissingIdentifier& e){
						    string message("Missing identifier: ");
						    op_ruleerror((message+e.what()).c_str());
							YYERROR;
						 }catch(oprule::parser::InvalidIdentifier& e){
						    string message("Invalid identifier: ");
						    op_ruleerror((message+e.what()).c_str());
							YYERROR;
						 }catch(oprule::parser::ModelNameNotFound& e){
						    string message("Model name not found: ");
						    op_ruleerror((message+e.what()).c_str());
							YYERROR;
						 }
			}

    break;

  case 91:

    { 
             string ifname=get_temp_symbol((yyvsp[0].stringval)).stringval;
             get_arg_map()["modelname"]=ifname;
			 try{
                (yyval.pnode)=_SETD(get_lookup()->getModelExpression(ifname,get_arg_map()));
			 }catch(oprule::parser::ModelNameNotFound& e){
						    string message("Model name not found: ");
						    op_ruleerror((message+e.what()).c_str());
							YYERROR;
			 } 
           }

    break;

  case 92:

    { (yyval.pnode)=(yyvsp[0].pnode); }

    break;

  case 93:

    { 
                         string valname=get_temp_symbol((yyvsp[-3].stringval)).stringval;
                         get_arg_map()["modelname"]=valname;
						 try{
						    (yyval.pnode)=_SETD(get_lookup()->getModelExpression(valname,get_arg_map()));
						 }catch(oprule::parser::MissingIdentifier& e){
						    string message("Missing identifier in ");
						    op_ruleerror((message+valname+":  "+e.what()).c_str());
							YYERROR;
						 }catch(oprule::parser::InvalidIdentifier& e){
						    string message("Invalid identifier: ");
						    op_ruleerror((message+e.what()).c_str());
							YYERROR;
						 }catch(oprule::parser::ModelNameNotFound& e){
						    string message("Model name not found: ");
						    op_ruleerror((message+e.what()).c_str());
							YYERROR;
						 }
			}

    break;

  case 94:

    { 
             string expressname=get_temp_symbol((yyvsp[0].stringval)).stringval;
             get_arg_map()["modelname"]=expressname;
			 try{
                (yyval.pnode)=_SETD(get_lookup()->getModelExpression(expressname,get_arg_map()));
			 }catch(oprule::parser::ModelNameNotFound& e){
						    string message("Model name not found: ");
						    op_ruleerror((message+e.what()).c_str());
							YYERROR;
			 }
           }

    break;

  case 95:

    { (yyval.pnode)=(yyvsp[0].pnode); }

    break;

  case 96:

    { 
		 (yyval.pbnode) = (yyvsp[0].pbnode);
	}

    break;



      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}






