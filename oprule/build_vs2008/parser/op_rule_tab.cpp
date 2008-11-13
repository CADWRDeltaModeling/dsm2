/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse op_ruleparse
#define yylex   op_rulelex
#define yyerror op_ruleerror
#define yylval  op_rulelval
#define yychar  op_rulechar
#define yydebug op_ruledebug
#define yynerrs op_rulenerrs


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
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
     IFELSE = 275,
     ACCUMULATE = 276,
     PREDICT = 277,
     LINEAR = 278,
     QUAD = 279,
     SQRT = 280,
     LOG = 281,
     LN = 282,
     EXP = 283,
     SET = 284,
     TO = 285,
     WHEN = 286,
     WHILE = 287,
     THEN = 288,
     RAMP = 289,
     YEAR = 290,
     MONTH = 291,
     DAY = 292,
     HOUR = 293,
     MINDAY = 294,
     MIN = 295,
     SEASON = 296,
     DATETIME = 297,
     REFDATE = 298,
     REFTIME = 299,
     REFSEASON = 300,
     NUMBER = 301,
     QUOTEDSTRING = 302,
     ASSIGN = 303,
     FALSE = 304,
     TRUE = 305,
     OR = 306,
     AND = 307,
     NOT = 308,
     DEFINE = 309,
     NEG = 310
   };
#endif
/* Tokens.  */
#define NAME 258
#define IFNAME 259
#define IFPARAM 260
#define EXPRESS 261
#define EXPRESSPARAM 262
#define NAMEDVAL 263
#define BOOLNAMEDVAL 264
#define LT 265
#define LE 266
#define GT 267
#define GE 268
#define EQ 269
#define NE 270
#define MAX2 271
#define MIN2 272
#define MAX3 273
#define MIN3 274
#define IFELSE 275
#define ACCUMULATE 276
#define PREDICT 277
#define LINEAR 278
#define QUAD 279
#define SQRT 280
#define LOG 281
#define LN 282
#define EXP 283
#define SET 284
#define TO 285
#define WHEN 286
#define WHILE 287
#define THEN 288
#define RAMP 289
#define YEAR 290
#define MONTH 291
#define DAY 292
#define HOUR 293
#define MINDAY 294
#define MIN 295
#define SEASON 296
#define DATETIME 297
#define REFDATE 298
#define REFTIME 299
#define REFSEASON 300
#define NUMBER 301
#define QUOTEDSTRING 302
#define ASSIGN 303
#define FALSE 304
#define TRUE 305
#define OR 306
#define AND 307
#define NOT 308
#define DEFINE 309
#define NEG 310




/* Copy the first part of user declarations.  */


// The following pragmas disable stupid warnings in MSVC
#pragma warning (disable:4786)   // identifier truncated to '255' characters
#pragma warning (disable:4065)   // switch contains 'default'
#define NOMINMAX
#include <map>
#include <functional>
#include <string>
#include <sstream>
#include <iostream>
#include <malloc.h>
#include <vector>

#include "oprule/expression/ValueNode.h"
#include "oprule/rule/ModelInterface.h"
#include "oprule/expression/TernaryOpNode.h"
#include "oprule/expression/BinaryOpNode.h"
#include "oprule/expression/UnaryOpNode.h"
#include "oprule/expression/LaggedExpressionNode.h"
#include "oprule/expression/AccumulationNode.h"
#include "oprule/expression/LinearExtrapolationNode.h"
#include "oprule/expression/QuadExtrapolationNode.h"
#include "oprule/expression/ExpressionPtr.h"

#include "oprule/parser/Symbol.h"
#include "oprule/parser/ModelNameParseError.h"

#include "oprule/rule/ExpressionTrigger.h"
#include "oprule/rule/OperatingRule.h"
#include "oprule/parser/NamedValueLookup.h"
#include "oprule/rule/OperationAction.h"
#include "oprule/rule/ActionSet.h"
#include "oprule/rule/ActionChain.h"
#include "oprule/parser/ModelTimeNodeFactory.h"
#include "oprule/parser/ModelActionFactory.h"
//#include "Parser.h"
#include "oprule/parser/ParseResultType.h"
#include "oprule/parser/ptr_wrapper.h"



using namespace std;
using namespace oprule::rule;
using namespace oprule::parser;
using namespace oprule::expression;



#define EVAL "eval__"
#define yylval op_rulelval

NamedValueLookup* lookup;
ModelTimeNodeFactory* appModelTimeFactory;


typedef map<string, symbol > express_map;
typedef map<string, symbol >::iterator expressIterType;
typedef pair<string, symbol> symbol_pair;

#define _EI express_index
vector<symbol > express_index;
typedef vector<symbol >::iterator indexIterType;

template<typename T>
int reg_temp_expr(typename ExpressionNode<T>::NodePtr ptr){
   symbol s=symbol(ptr);
   express_index.push_back(s);
   return express_index.size() - 1;
}

template int reg_temp_expr(ExpressionNode<double>::NodePtr);
template int reg_temp_expr(ExpressionNode<bool>::NodePtr);

#define _SETD reg_temp_expr
#define _SETB reg_temp_expr
#define _GETD(NDX) express_index[NDX].dblval
#define _GETB(NDX) express_index[NDX].boolval
#define _GETS(NDX) express_index[NDX]





void clear_temp_expr(void);
void clear_arg_map();

typedef map<string, OperatingRule*> rule_map;
typedef map<pair<string, int>, double> initial_value_map;
typedef pair<string, int> initializer_pair;


void init_lookup( NamedValueLookup*);
void init_model_time_factory(ModelTimeNodeFactory * );
void init_action_factory(ModelActionFactory *);
void init_expression();
void init_rule_names();
void apply_lagged_values(const string&name, DoubleNodePtr expression);

void set_parsed_type(oprule::parser::parse_type);
oprule::parser::parse_type get_parsed_type();
bool add_symbol(const string & dbkey, symbol& expression);
bool add_rule(const string& name, OperatingRule* rule);

OperationAction* chain_actions( OperationAction* op1, OperationAction* op2);
OperationAction* chain_actions( ActionChain* op1, OperationAction* op2);
OperationAction* group_actions( OperationAction* op1, OperationAction* op2);


symbol make_symbol(DoubleNodePtr dnode);
symbol make_symbol(BoolNodePtr bnode);
symbol name_lookup(const string& name);
extern void lexer_init(); 


const string EVAL_STR=string(EVAL);

int op_ruleerror(const char* s);
int month_to_int(const string&);
extern int op_rulelex();
express_map express;
rule_map rulenames;
initial_value_map init_cond;
typedef pair<string, OE_NODE_PTR(LaggedExpressionNode<double> >) lagged_expression_pair;

typedef vector< lagged_expression_pair >lagged_expression_list;
lagged_expression_list laggedvals;

ModelActionFactory* action_factory;
NamedValueLookup::ArgList arglist;
NamedValueLookup::ArgMap argmap;
OperatingRule * eval_rule;
oprule::parser::parse_type _parsed_type;

template<class T>
string ToString(const T& val){
   std::stringstream stream;
   stream << val;
   return stream.str();
}

long padded_string_to_int(const string& str){
   char* end;
   if(str.substr(0,1) == "0"){
      return strtol(str.substr(1,1).c_str(),&end,0);
   }else{ 
      return strtol(str.substr(0,2).c_str(),&end,0);
	}
}				   
 




/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE

{
   oprule::parser::symbol* symval;
   int intval;
   double dval;
   char * strval;
   std::string* strgval;
   //oprule::parser::ptr_wrapper<oprule::expression::DoubleNode> pnode;
   //oprule::parser::ptr_wrapper<oprule::expression::BoolNode> bnode;  
   //oprule::expression::DoubleNodePtr * pnode;
   //oprule::expression::BoolNodePtr * pbnode;
   int pnode;
   int pbnode;
   oprule::rule::ModelInterfaceDouble * modelif;
   oprule::rule::ExpressionTrigger * trigval;
   oprule::rule::OperationAction * opaction;
   oprule::rule::OperatingRule * opruleval;
}
/* Line 187 of yacc.c.  */

	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */


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
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
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
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
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
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
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
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
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
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  81
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   576

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  66
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  22
/* YYNRULES -- Number of rules.  */
#define YYNRULES  88
/* YYNRULES -- Number of states.  */
#define YYNSTATES  209

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   310

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      62,    63,    57,    55,    64,    56,     2,    58,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    61,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    59,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,    65,     2,     2,     2,
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
      60
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     8,    13,    16,    19,    24,    27,    29,
      32,    35,    38,    41,    44,    46,    50,    54,    58,    62,
      65,    68,    71,    75,    77,    79,    81,    83,    85,    89,
      91,    93,    95,   100,   105,   110,   115,   117,   121,   125,
     129,   133,   137,   140,   144,   153,   162,   169,   179,   189,
     196,   203,   212,   221,   223,   227,   231,   235,   239,   243,
     247,   251,   255,   258,   265,   267,   271,   275,   279,   283,
     287,   289,   292,   294,   296,   299,   301,   303,   305,   307,
     309,   311,   313,   318,   320,   322,   327,   329,   331
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      67,     0,    -1,     3,    48,    80,    61,    -1,     3,    48,
      79,    61,    -1,    80,    61,    -1,    79,    61,    -1,     3,
      48,    69,    61,    -1,    69,    61,    -1,    68,    -1,     1,
      61,    -1,    80,    48,    -1,    79,    48,    -1,    71,    70,
      -1,    31,    80,    -1,    72,    -1,    71,    33,    71,    -1,
      71,    32,    71,    -1,    62,    71,    63,    -1,    73,    74,
      75,    -1,    73,    74,    -1,    29,    85,    -1,    30,    79,
      -1,    34,    46,    40,    -1,    46,    -1,    78,    -1,    84,
      -1,    86,    -1,    81,    -1,    62,    80,    63,    -1,    49,
      -1,    50,    -1,    87,    -1,    25,    62,    79,    63,    -1,
      26,    62,    79,    63,    -1,    27,    62,    79,    63,    -1,
      28,    62,    79,    63,    -1,    76,    -1,    79,    59,    79,
      -1,    79,    57,    79,    -1,    79,    58,    79,    -1,    79,
      55,    79,    -1,    79,    56,    79,    -1,    56,    79,    -1,
      62,    79,    63,    -1,    20,    62,    80,    64,    79,    64,
      79,    63,    -1,    21,    62,    79,    64,    79,    64,    80,
      63,    -1,    21,    62,    79,    64,    79,    63,    -1,    22,
      62,    79,    64,    23,    64,    46,    40,    63,    -1,    22,
      62,    79,    64,    24,    64,    46,    40,    63,    -1,    17,
      62,    79,    64,    79,    63,    -1,    16,    62,    79,    64,
      79,    63,    -1,    19,    62,    79,    64,    79,    64,    79,
      63,    -1,    18,    62,    79,    64,    79,    64,    79,    63,
      -1,    77,    -1,    79,    11,    79,    -1,    79,    13,    79,
      -1,    79,    12,    79,    -1,    79,    10,    79,    -1,    79,
      14,    79,    -1,    79,    15,    79,    -1,    80,    51,    80,
      -1,    80,    52,    80,    -1,    53,    80,    -1,     3,    62,
      65,    56,    46,    63,    -1,    83,    -1,    82,    64,    83,
      -1,    82,    61,    83,    -1,     3,    54,    46,    -1,     3,
      54,    47,    -1,     3,    54,     3,    -1,    42,    -1,    43,
      44,    -1,    43,    -1,    41,    -1,    45,    44,    -1,    45,
      -1,    35,    -1,    36,    -1,    37,    -1,    38,    -1,    39,
      -1,    40,    -1,     5,    62,    82,    63,    -1,     4,    -1,
       8,    -1,     7,    62,    82,    63,    -1,     6,    -1,    85,
      -1,     9,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   248,   248,   253,   259,   264,   269,   281,   289,   293,
     300,   303,   308,   314,   319,   320,   321,   322,   325,   331,
     338,   341,   344,   347,   349,   350,   351,   352,   357,   358,
     359,   360,   365,   367,   369,   371,   377,   378,   379,   380,
     381,   382,   383,   384,   385,   387,   389,   392,   393,   394,
     395,   396,   398,   404,   405,   406,   407,   408,   409,   410,
     411,   412,   413,   418,   432,   433,   434,   438,   441,   445,
     452,   453,   454,   455,   456,   462,   466,   467,   468,   469,
     470,   471,   475,   493,   506,   507,   525,   535,   542
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "NAME", "IFNAME", "IFPARAM", "EXPRESS",
  "EXPRESSPARAM", "NAMEDVAL", "BOOLNAMEDVAL", "LT", "LE", "GT", "GE", "EQ",
  "NE", "MAX2", "MIN2", "MAX3", "MIN3", "IFELSE", "ACCUMULATE", "PREDICT",
  "LINEAR", "QUAD", "SQRT", "LOG", "LN", "EXP", "SET", "TO", "WHEN",
  "WHILE", "THEN", "RAMP", "YEAR", "MONTH", "DAY", "HOUR", "MINDAY", "MIN",
  "SEASON", "DATETIME", "REFDATE", "REFTIME", "REFSEASON", "NUMBER",
  "QUOTEDSTRING", "ASSIGN", "FALSE", "TRUE", "OR", "AND", "NOT", "DEFINE",
  "'+'", "'-'", "'*'", "'/'", "'^'", "NEG", "';'", "'('", "')'", "','",
  "'t'", "$accept", "line", "reassignment", "oprule", "triggerexpression",
  "action", "modelaction", "actionspec", "targetspec", "transitionspec",
  "term", "boolterm", "unary", "expression", "boolexpression", "laggedval",
  "arglist", "arg", "date", "interface", "namedval", "namedboolval", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,    43,    45,    42,    47,    94,
     310,    59,    40,    41,    44,   116
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    66,    67,    67,    67,    67,    67,    67,    67,    67,
      68,    68,    69,    70,    71,    71,    71,    71,    72,    72,
      73,    74,    75,    76,    76,    76,    76,    76,    77,    77,
      77,    77,    78,    78,    78,    78,    79,    79,    79,    79,
      79,    79,    79,    79,    79,    79,    79,    79,    79,    79,
      79,    79,    79,    80,    80,    80,    80,    80,    80,    80,
      80,    80,    80,    81,    82,    82,    82,    83,    83,    83,
      84,    84,    84,    84,    84,    84,    84,    84,    84,    84,
      84,    84,    85,    85,    86,    86,    86,    86,    87
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     4,     4,     2,     2,     4,     2,     1,     2,
       2,     2,     2,     2,     1,     3,     3,     3,     3,     2,
       2,     2,     3,     1,     1,     1,     1,     1,     3,     1,
       1,     1,     4,     4,     4,     4,     1,     3,     3,     3,
       3,     3,     2,     3,     8,     8,     6,     9,     9,     6,
       6,     8,     8,     1,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     6,     1,     3,     3,     3,     3,     3,
       1,     2,     1,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     4,     1,     1,     4,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,    83,     0,    86,     0,    84,    88,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,    77,    78,    79,    80,    81,    73,    70,    72,
      75,    23,    29,    30,     0,     0,     0,     0,     8,     0,
       0,    14,     0,    36,    53,    24,     0,     0,    27,    25,
      87,    26,    31,     9,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    20,
      71,    74,     0,     0,     0,    62,     0,    42,     0,     0,
       0,     1,     7,     0,     0,     0,    12,     0,    19,     0,
       0,     0,     0,     0,     0,    11,     0,     0,     0,     0,
       0,     5,    10,     0,     0,     4,     0,     0,     0,     0,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    17,    43,    28,    13,
       0,    16,    15,    21,     0,    18,    57,    54,    56,    55,
      58,    59,    40,    41,    38,    39,    37,    60,    61,     6,
       3,     2,     0,     0,     0,    82,     0,    85,     0,     0,
       0,     0,     0,     0,     0,    32,    33,    34,    35,     0,
       0,    69,    67,    68,    66,    65,     0,     0,     0,     0,
       0,     0,     0,     0,    22,    63,    50,    49,     0,     0,
       0,    46,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    52,    51,    44,    45,     0,     0,    47,    48
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    37,    38,    39,    86,    40,    41,    42,    88,   135,
      43,    44,    45,    74,    80,    48,   111,   112,    49,    50,
      51,    52
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -65
static const yytype_int16 yypact[] =
{
     196,   -32,   -42,   -65,   -29,   -65,   -18,   -65,   -65,    -7,
      29,    31,    34,    47,    54,    68,    76,    77,    78,    79,
       6,   -65,   -65,   -65,   -65,   -65,   -65,   -65,   -65,    -4,
       4,   -65,   -65,   -65,   316,   129,   256,    59,   -65,    44,
     -17,   -65,    99,   -65,   -65,   -65,   335,   -39,   -65,   -65,
     -65,   -65,   -65,   -65,   256,    87,   139,   139,   129,   129,
     129,   129,   316,   129,   129,   129,   129,   129,   129,   -65,
     -65,   -65,    81,   316,   387,   -65,   129,    -6,   -28,    13,
     -44,   -65,   -65,   316,   -26,   -26,   -65,   129,   110,   129,
     129,   129,   129,   129,   129,   -65,   129,   129,   129,   129,
     129,   -65,   -65,   316,   316,   -65,    92,   374,   -14,   102,
     105,   -22,   -65,    -3,    67,   131,   318,   348,   -34,   358,
     392,   432,   441,   450,   459,   468,   -65,   -65,   -65,    22,
     -26,    45,   127,     8,   115,   -65,     8,     8,     8,     8,
       8,     8,    -6,    -6,   103,   103,   -65,   111,   -65,   -65,
     -65,   -65,   130,    -1,   139,   -65,   139,   -65,   129,   129,
     129,   129,   129,   129,   104,   -65,   -65,   -65,   -65,   133,
     114,   -65,   -65,   -65,   -65,   -65,   477,   486,   402,   412,
     422,    43,   132,   134,   -65,   -65,   -65,   -65,   129,   129,
     129,   -65,   316,   147,   148,   495,   504,   513,   -20,   138,
     166,   -65,   -65,   -65,   -65,   144,   145,   -65,   -65
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -65,   -65,   -65,   165,   -65,   -35,   -65,   -65,   -65,   -65,
     -65,   -65,   -65,    21,     0,   -65,   163,   -64,   -65,   206,
     -65,   -65
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_int16 yytable[] =
{
      47,    78,   171,    20,    84,    85,    54,   103,   104,   102,
       3,     4,   103,   104,    83,    84,    85,   103,   104,   128,
      55,    46,   105,    89,    90,    91,    92,    93,    94,    53,
     162,   103,   104,    56,    75,   126,   130,   103,   104,   154,
      70,   155,   156,   204,    57,   172,   173,   151,    71,   131,
     132,    98,    99,   100,   108,    58,    77,    79,   154,    81,
     157,   156,   118,    96,    97,    98,    99,   100,    96,    97,
      98,    99,   100,   103,   104,   107,   127,    -1,    85,   114,
     115,   116,   117,   129,   119,   120,   121,   122,   123,   124,
     174,    59,   175,    60,    79,    78,    61,   125,    96,    97,
      98,    99,   100,   147,   148,    82,   191,   192,   133,    62,
     136,   137,   138,   139,   140,   141,    63,   142,   143,   144,
     145,   146,    96,    97,    98,    99,   100,   182,   183,    87,
      64,   158,    72,     3,     4,     5,     6,     7,    65,    66,
      67,    68,   110,    55,   134,     9,    10,    11,    12,    13,
      14,    15,   109,   149,    16,    17,    18,    19,   152,   153,
      -1,   169,   100,   104,    21,    22,    23,    24,    25,    26,
      27,    28,    29,   184,    30,    31,   170,   185,   205,   176,
     177,   178,   179,   180,   181,    35,    96,    97,    98,    99,
     100,    76,   198,   199,   200,   159,   193,     1,   194,     2,
       3,     4,     5,     6,     7,     8,   206,   207,   208,   195,
     196,   197,     9,    10,    11,    12,    13,    14,    15,   106,
     113,    16,    17,    18,    19,    20,    69,     0,     0,     0,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
       0,    30,    31,     0,     0,    32,    33,     0,     0,    34,
       0,     0,    35,     0,     0,     0,     0,     0,    36,    72,
       3,     4,     5,     6,     7,     8,     0,     0,     0,     0,
       0,     0,     9,    10,    11,    12,    13,    14,    15,     0,
       0,    16,    17,    18,    19,    20,     0,     0,     0,     0,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
       0,    30,    31,     0,     0,    32,    33,     0,     0,    34,
       0,     0,    35,     0,     0,     0,     0,     0,    36,    72,
       3,     4,     5,     6,     7,     8,     0,     0,     0,     0,
       0,     0,     9,    10,    11,    12,    13,    14,    15,     0,
       0,    16,    17,    18,    19,    89,    90,    91,    92,    93,
      94,    21,    22,    23,    24,    25,    26,    27,    28,    29,
       0,    30,    31,     0,     0,    32,    33,     0,     0,    34,
       0,     0,    35,    96,    97,    98,    99,   100,    73,     0,
       0,     0,   160,    95,    89,    90,    91,    92,    93,    94,
      96,    97,    98,    99,   100,     0,   101,    89,    90,    91,
      92,    93,    94,    96,    97,    98,    99,   100,     0,     0,
       0,     0,   161,    96,    97,    98,    99,   100,     0,     0,
       0,     0,   163,     0,     0,     0,     0,     0,     0,    96,
      97,    98,    99,   100,     0,   150,     0,     0,     0,     0,
       0,     0,    96,    97,    98,    99,   100,    96,    97,    98,
      99,   100,     0,     0,     0,     0,   164,    96,    97,    98,
      99,   100,     0,     0,     0,     0,   188,    96,    97,    98,
      99,   100,     0,     0,     0,     0,   189,    96,    97,    98,
      99,   100,     0,     0,     0,     0,   190,    96,    97,    98,
      99,   100,     0,     0,     0,   165,    96,    97,    98,    99,
     100,     0,     0,     0,   166,    96,    97,    98,    99,   100,
       0,     0,     0,   167,    96,    97,    98,    99,   100,     0,
       0,     0,   168,    96,    97,    98,    99,   100,     0,     0,
       0,   127,    96,    97,    98,    99,   100,     0,     0,     0,
     186,    96,    97,    98,    99,   100,     0,     0,     0,   187,
      96,    97,    98,    99,   100,     0,     0,     0,   201,    96,
      97,    98,    99,   100,     0,     0,     0,   202,    96,    97,
      98,    99,   100,     0,     0,     0,   203
};

static const yytype_int16 yycheck[] =
{
       0,    36,     3,    29,    32,    33,    48,    51,    52,    48,
       4,     5,    51,    52,    31,    32,    33,    51,    52,    63,
      62,     0,    61,    10,    11,    12,    13,    14,    15,    61,
      64,    51,    52,    62,    34,    63,    62,    51,    52,    61,
      44,    63,    64,    63,    62,    46,    47,    61,    44,    84,
      85,    57,    58,    59,    54,    62,    35,    36,    61,     0,
      63,    64,    62,    55,    56,    57,    58,    59,    55,    56,
      57,    58,    59,    51,    52,    54,    63,    32,    33,    58,
      59,    60,    61,    83,    63,    64,    65,    66,    67,    68,
     154,    62,   156,    62,    73,   130,    62,    76,    55,    56,
      57,    58,    59,   103,   104,    61,    63,    64,    87,    62,
      89,    90,    91,    92,    93,    94,    62,    96,    97,    98,
      99,   100,    55,    56,    57,    58,    59,    23,    24,    30,
      62,    64,     3,     4,     5,     6,     7,     8,    62,    62,
      62,    62,     3,    62,    34,    16,    17,    18,    19,    20,
      21,    22,    65,    61,    25,    26,    27,    28,    56,    54,
      33,    46,    59,    52,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    40,    45,    46,    46,    63,    40,   158,
     159,   160,   161,   162,   163,    56,    55,    56,    57,    58,
      59,    62,   192,    46,    46,    64,    64,     1,    64,     3,
       4,     5,     6,     7,     8,     9,    40,    63,    63,   188,
     189,   190,    16,    17,    18,    19,    20,    21,    22,    54,
      57,    25,    26,    27,    28,    29,    20,    -1,    -1,    -1,
      -1,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      -1,    45,    46,    -1,    -1,    49,    50,    -1,    -1,    53,
      -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    62,     3,
       4,     5,     6,     7,     8,     9,    -1,    -1,    -1,    -1,
      -1,    -1,    16,    17,    18,    19,    20,    21,    22,    -1,
      -1,    25,    26,    27,    28,    29,    -1,    -1,    -1,    -1,
      -1,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      -1,    45,    46,    -1,    -1,    49,    50,    -1,    -1,    53,
      -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    62,     3,
       4,     5,     6,     7,     8,     9,    -1,    -1,    -1,    -1,
      -1,    -1,    16,    17,    18,    19,    20,    21,    22,    -1,
      -1,    25,    26,    27,    28,    10,    11,    12,    13,    14,
      15,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      -1,    45,    46,    -1,    -1,    49,    50,    -1,    -1,    53,
      -1,    -1,    56,    55,    56,    57,    58,    59,    62,    -1,
      -1,    -1,    64,    48,    10,    11,    12,    13,    14,    15,
      55,    56,    57,    58,    59,    -1,    61,    10,    11,    12,
      13,    14,    15,    55,    56,    57,    58,    59,    -1,    -1,
      -1,    -1,    64,    55,    56,    57,    58,    59,    -1,    -1,
      -1,    -1,    64,    -1,    -1,    -1,    -1,    -1,    -1,    55,
      56,    57,    58,    59,    -1,    61,    -1,    -1,    -1,    -1,
      -1,    -1,    55,    56,    57,    58,    59,    55,    56,    57,
      58,    59,    -1,    -1,    -1,    -1,    64,    55,    56,    57,
      58,    59,    -1,    -1,    -1,    -1,    64,    55,    56,    57,
      58,    59,    -1,    -1,    -1,    -1,    64,    55,    56,    57,
      58,    59,    -1,    -1,    -1,    -1,    64,    55,    56,    57,
      58,    59,    -1,    -1,    -1,    63,    55,    56,    57,    58,
      59,    -1,    -1,    -1,    63,    55,    56,    57,    58,    59,
      -1,    -1,    -1,    63,    55,    56,    57,    58,    59,    -1,
      -1,    -1,    63,    55,    56,    57,    58,    59,    -1,    -1,
      -1,    63,    55,    56,    57,    58,    59,    -1,    -1,    -1,
      63,    55,    56,    57,    58,    59,    -1,    -1,    -1,    63,
      55,    56,    57,    58,    59,    -1,    -1,    -1,    63,    55,
      56,    57,    58,    59,    -1,    -1,    -1,    63,    55,    56,
      57,    58,    59,    -1,    -1,    -1,    63
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,     3,     4,     5,     6,     7,     8,     9,    16,
      17,    18,    19,    20,    21,    22,    25,    26,    27,    28,
      29,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      45,    46,    49,    50,    53,    56,    62,    67,    68,    69,
      71,    72,    73,    76,    77,    78,    79,    80,    81,    84,
      85,    86,    87,    61,    48,    62,    62,    62,    62,    62,
      62,    62,    62,    62,    62,    62,    62,    62,    62,    85,
      44,    44,     3,    62,    79,    80,    62,    79,    71,    79,
      80,     0,    61,    31,    32,    33,    70,    30,    74,    10,
      11,    12,    13,    14,    15,    48,    55,    56,    57,    58,
      59,    61,    48,    51,    52,    61,    69,    79,    80,    65,
       3,    82,    83,    82,    79,    79,    79,    79,    80,    79,
      79,    79,    79,    79,    79,    79,    63,    63,    63,    80,
      62,    71,    71,    79,    34,    75,    79,    79,    79,    79,
      79,    79,    79,    79,    79,    79,    79,    80,    80,    61,
      61,    61,    56,    54,    61,    63,    64,    63,    64,    64,
      64,    64,    64,    64,    64,    63,    63,    63,    63,    46,
      46,     3,    46,    47,    83,    83,    79,    79,    79,    79,
      79,    79,    23,    24,    40,    63,    63,    63,    64,    64,
      64,    63,    64,    64,    64,    79,    79,    79,    80,    46,
      46,    63,    63,    63,    63,    40,    40,    63,    63
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

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
#ifndef	YYINITDEPTH
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
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
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
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
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

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

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
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

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

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
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
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

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
     `$$ = $1'.

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
	   string dbkey=*(yyvsp[(1) - (4)].strgval);
	   add_symbol(dbkey,_GETS((yyvsp[(3) - (4)].pbnode)));
	   set_parsed_type(oprule::parser::BOOL_EXPRESS);
        ;}
    break;

  case 3:

    {
	   string dbkey=*(yyvsp[(1) - (4)].strgval);
	   add_symbol(dbkey,_GETS((yyvsp[(3) - (4)].pnode)));
	   apply_lagged_values(*(yyvsp[(1) - (4)].strgval),_GETD((yyvsp[(3) - (4)].pnode)));
	   set_parsed_type(oprule::parser::NUMERICAL_EXPRESS);
        ;}
    break;

  case 4:

    {
       string dbkey=EVAL_STR;
	   add_symbol(dbkey,_GETS((yyvsp[(1) - (2)].pbnode)));
	   set_parsed_type(oprule::parser::BOOL_EXPRESS);
	    ;}
    break;

  case 5:

    {
       string dbkey=EVAL_STR;
	   add_symbol(dbkey,_GETS((yyvsp[(1) - (2)].pnode)));
	   set_parsed_type(oprule::parser::NUMERICAL_EXPRESS);
	   ;}
    break;

  case 6:

    {
        eval_rule=(yyvsp[(3) - (4)].opruleval);
	    string rulename=*(yyvsp[(1) - (4)].strgval);
		if (add_rule(rulename,eval_rule)){
          ((yyvsp[(3) - (4)].opruleval))->setName(rulename);
  	      set_parsed_type(oprule::parser::OP_RULE);   
        }else{
		  op_ruleerror(
		    ("Operating rule name " + rulename + " used more than once.").c_str());
          YYABORT;
		 }
        ;}
    break;

  case 7:

    {
        eval_rule=(yyvsp[(1) - (2)].opruleval);
		static int ruleno;
        char * num=0;
        itoa(ruleno++,num,10);
		eval_rule->setName(string("OpRule") +num);
	    set_parsed_type(oprule::parser::OP_RULE);
		;}
    break;

  case 8:

    {               // really just captures error
        set_parsed_type(oprule::parser::REASSIGNMENT);
		;}
    break;

  case 9:

    {
                cout << "Error parsing expression." << endl;
				YYABORT;}
    break;

  case 10:

    {
	     op_ruleerror("reassignment of (boolean-valued) variable");
	                       ;}
    break;

  case 11:

    {
         op_ruleerror("reassignment of variable");
		                   ;}
    break;

  case 12:

    {
	 (yyval.opruleval)=new OperatingRule((yyvsp[(1) - (2)].opaction),(yyvsp[(2) - (2)].trigval));
   ;}
    break;

  case 13:

    {
			  (yyval.trigval)=new ExpressionTrigger(_GETB((yyvsp[(2) - (2)].pbnode)));
			  ;}
    break;

  case 14:

    { (yyval.opaction)=(yyvsp[(1) - (1)].opaction); ;}
    break;

  case 15:

    {(yyval.opaction)=chain_actions((yyvsp[(1) - (3)].opaction), (yyvsp[(3) - (3)].opaction));  ;}
    break;

  case 16:

    { (yyval.opaction)=group_actions((yyvsp[(1) - (3)].opaction), (yyvsp[(3) - (3)].opaction));;}
    break;

  case 17:

    { (yyval.opaction)=(yyvsp[(2) - (3)].opaction); ;}
    break;

  case 18:

    { 
			 ModelInterface<double>::NodePtr ifc
			   =boost::static_pointer_cast<ModelInterface<double> >( _GETD((yyvsp[(1) - (3)].pnode)) );
			 (yyval.opaction)=action_factory->createModelAction(ifc,_GETD((yyvsp[(2) - (3)].pnode)),(int)(yyvsp[(3) - (3)].dval),0);
	;}
    break;

  case 19:

    { 
			 ModelInterface<double>::NodePtr ifc
			   =boost::static_pointer_cast<ModelInterface<double> >( _GETD((yyvsp[(1) - (2)].pnode)) );
			 (yyval.opaction)=action_factory->createModelAction(ifc,_GETD((yyvsp[(2) - (2)].pnode)), 0, 0);
			;}
    break;

  case 20:

    {(yyval.pnode)=(yyvsp[(2) - (2)].pnode);;}
    break;

  case 21:

    {(yyval.pnode)=(yyvsp[(2) - (2)].pnode);;}
    break;

  case 22:

    {(yyval.dval)=(yyvsp[(2) - (3)].dval);;}
    break;

  case 23:

    { 
      (yyval.pnode) =_SETD(DoubleScalarNode::create((yyvsp[(1) - (1)].dval))); ;}
    break;

  case 24:

    { (yyval.pnode)=(yyvsp[(1) - (1)].pnode); ;}
    break;

  case 25:

    { (yyval.pnode)=(yyvsp[(1) - (1)].pnode); ;}
    break;

  case 26:

    { (yyval.pnode)=(yyvsp[(1) - (1)].pnode);;}
    break;

  case 27:

    { (yyval.pnode)=(yyvsp[(1) - (1)].pnode);;}
    break;

  case 28:

    { (yyval.pbnode) = (yyvsp[(2) - (3)].pbnode);;}
    break;

  case 29:

    { (yyval.pbnode)=_SETB(BoolScalarNode::create(false)); ;}
    break;

  case 30:

    { (yyval.pbnode)=_SETB(BoolScalarNode::create(true));  ;}
    break;

  case 31:

    { (yyval.pbnode) = (yyvsp[(1) - (1)].pbnode);;}
    break;

  case 32:

    {  
            (yyval.pnode)=_SETD(UnaryOpNode<sqrt_func>::create(_GETD((yyvsp[(3) - (4)].pnode))));;}
    break;

  case 33:

    {
           (yyval.pnode)=_SETD(UnaryOpNode<log10_func>::create(_GETD((yyvsp[(3) - (4)].pnode))));;}
    break;

  case 34:

    {
           (yyval.pnode)=_SETD(UnaryOpNode<ln_func>::create(_GETD((yyvsp[(3) - (4)].pnode))));;}
    break;

  case 35:

    {
           (yyval.pnode)=_SETD(UnaryOpNode<exp_func>::create(_GETD((yyvsp[(3) - (4)].pnode))));;}
    break;

  case 36:

    { (yyval.pnode) = (yyvsp[(1) - (1)].pnode); ;}
    break;

  case 37:

    { (yyval.pnode)=_SETD(BinaryOpNode<power_func >::create(_GETD((yyvsp[(1) - (3)].pnode)),_GETD((yyvsp[(3) - (3)].pnode))));;}
    break;

  case 38:

    { (yyval.pnode)=_SETD(BinaryOpNode<multiplies<double> >::create(_GETD((yyvsp[(1) - (3)].pnode)),_GETD((yyvsp[(3) - (3)].pnode))));;}
    break;

  case 39:

    { (yyval.pnode)=_SETD(BinaryOpNode<divides<double> >::create(_GETD((yyvsp[(1) - (3)].pnode)),_GETD((yyvsp[(3) - (3)].pnode))));;}
    break;

  case 40:

    { (yyval.pnode)=_SETD(BinaryOpNode<plus<double> >::create(_GETD((yyvsp[(1) - (3)].pnode)),_GETD((yyvsp[(3) - (3)].pnode))));;}
    break;

  case 41:

    { (yyval.pnode)=_SETD(BinaryOpNode<minus<double> >::create(_GETD((yyvsp[(1) - (3)].pnode)),_GETD((yyvsp[(3) - (3)].pnode))));;}
    break;

  case 42:

    { (yyval.pnode)=_SETD(UnaryOpNode<negate<double> >::create(_GETD((yyvsp[(2) - (2)].pnode))));;}
    break;

  case 43:

    {(yyval.pnode)=(yyvsp[(2) - (3)].pnode);;}
    break;

  case 44:

    {
                    (yyval.pnode)=_SETD(TernaryOpNode<double>::create(_GETB((yyvsp[(3) - (8)].pbnode)),_GETD((yyvsp[(5) - (8)].pnode)),_GETD((yyvsp[(7) - (8)].pnode)))); ;}
    break;

  case 45:

    {
					(yyval.pnode)=_SETD(AccumulationNode<double>::create(_GETD((yyvsp[(3) - (8)].pnode)),_GETD((yyvsp[(5) - (8)].pnode)),_GETB((yyvsp[(7) - (8)].pbnode))));;}
    break;

  case 46:

    {
                    BoolScalarNode::NodePtr f = BoolScalarNode::create(false);
					(yyval.pnode)=_SETD(AccumulationNode<double>::create(_GETD((yyvsp[(3) - (6)].pnode)),_GETD((yyvsp[(5) - (6)].pnode)),f)); ;}
    break;

  case 47:

    {(yyval.pnode)=_SETD(LinearExtrapolationNode<double>::create(_GETD((yyvsp[(3) - (9)].pnode)), (yyvsp[(7) - (9)].dval) * 60./appModelTimeFactory->getStepSizeSeconds()));;}
    break;

  case 48:

    {(yyval.pnode)=_SETD(QuadExtrapolationNode::create(_GETD((yyvsp[(3) - (9)].pnode)), (yyvsp[(7) - (9)].dval) * 60./appModelTimeFactory->getStepSizeSeconds()));;}
    break;

  case 49:

    {(yyval.pnode)=_SETD(BinaryOpNode<min2 >::create(_GETD((yyvsp[(3) - (6)].pnode)),_GETD((yyvsp[(5) - (6)].pnode))));;}
    break;

  case 50:

    {(yyval.pnode)=_SETD(BinaryOpNode<max2 >::create(_GETD((yyvsp[(3) - (6)].pnode)),_GETD((yyvsp[(5) - (6)].pnode))));;}
    break;

  case 51:

    {
        (yyval.pnode)=_SETD(Min3Node<double>::create( _GETD((yyvsp[(3) - (8)].pnode)),_GETD((yyvsp[(5) - (8)].pnode)),_GETD((yyvsp[(7) - (8)].pnode))));;}
    break;

  case 52:

    {
        (yyval.pnode)=_SETD(Max3Node<double>::create( _GETD((yyvsp[(3) - (8)].pnode)),_GETD((yyvsp[(5) - (8)].pnode)),_GETD((yyvsp[(7) - (8)].pnode))));;}
    break;

  case 53:

    {(yyval.pbnode)= (yyvsp[(1) - (1)].pbnode);;}
    break;

  case 54:

    { (yyval.pbnode)=_SETB(BinaryOpNode<less_equal<double> >::create(_GETD((yyvsp[(1) - (3)].pnode)), _GETD((yyvsp[(3) - (3)].pnode))));;}
    break;

  case 55:

    { (yyval.pbnode)=_SETB(BinaryOpNode<greater_equal<double> >::create(_GETD((yyvsp[(1) - (3)].pnode)), _GETD((yyvsp[(3) - (3)].pnode))));;}
    break;

  case 56:

    { (yyval.pbnode)=_SETB(BinaryOpNode<greater<double> >::create(_GETD((yyvsp[(1) - (3)].pnode)), _GETD((yyvsp[(3) - (3)].pnode)))); ;}
    break;

  case 57:

    { (yyval.pbnode)=_SETB(BinaryOpNode<less<double> >::create(_GETD((yyvsp[(1) - (3)].pnode)), _GETD((yyvsp[(3) - (3)].pnode)))); ;}
    break;

  case 58:

    { (yyval.pbnode)=_SETB(BinaryOpNode<equal_to<double> >::create(_GETD((yyvsp[(1) - (3)].pnode)), _GETD((yyvsp[(3) - (3)].pnode)))); ;}
    break;

  case 59:

    { (yyval.pbnode)=_SETB(BinaryOpNode<not_equal_to<double> >::create(_GETD((yyvsp[(1) - (3)].pnode)), _GETD((yyvsp[(3) - (3)].pnode)))); ;}
    break;

  case 60:

    { (yyval.pbnode)=_SETB(BinaryOpNode<logical_or<bool > >::create(_GETB((yyvsp[(1) - (3)].pbnode)), _GETB((yyvsp[(3) - (3)].pbnode))));;}
    break;

  case 61:

    { (yyval.pbnode)=_SETB(BinaryOpNode<logical_and<bool > >::create(_GETB((yyvsp[(1) - (3)].pbnode)), _GETB((yyvsp[(3) - (3)].pbnode))));;}
    break;

  case 62:

    { (yyval.pbnode)=_SETB(UnaryOpNode<logical_not<bool> >::create(_GETB((yyvsp[(2) - (2)].pbnode))));;}
    break;

  case 63:

    { 
              LaggedExpressionNode<double>::NodePtr 
			     laggedNode=LaggedDoubleNode::create((int)(yyvsp[(5) - (6)].dval));
               const string nm(*((yyvsp[(1) - (6)].strgval)));
               laggedvals.push_back(
			      make_pair<string, LaggedExpressionNode<double>::NodePtr>(
				     nm,laggedNode));
				(yyval.pnode)=_SETD(laggedNode);
				  ;}
    break;

  case 67:

    { 
                         argmap[*(yyvsp[(1) - (3)].strgval)]=ToString<double>((yyvsp[(3) - (3)].dval));
					   ;}
    break;

  case 68:

    { 
					     argmap[*(yyvsp[(1) - (3)].strgval)]=*(yyvsp[(3) - (3)].strgval);
						;}
    break;

  case 69:

    { 
                        argmap[*(yyvsp[(1) - (3)].strgval)]=*(yyvsp[(3) - (3)].strgval);
                       ;}
    break;

  case 70:

    {(yyval.pnode)=_SETD(appModelTimeFactory->getDateTimeNode());;}
    break;

  case 71:

    {(yyval.pnode)=_SETD(appModelTimeFactory->getDateTimeNode(*(yyvsp[(1) - (2)].strgval),*(yyvsp[(2) - (2)].strgval)));;}
    break;

  case 72:

    { (yyval.pnode)=_SETD(appModelTimeFactory->getDateTimeNode(*(yyvsp[(1) - (1)].strgval),"00:00"));;}
    break;

  case 73:

    { (yyval.pnode)=_SETD(appModelTimeFactory->getSeasonNode()); ;}
    break;

  case 74:

    { int mon=month_to_int((yyvsp[(1) - (2)].strgval)->substr(2,3));
                       int day=padded_string_to_int((yyvsp[(1) - (2)].strgval)->substr(0,2));
                       int hour=padded_string_to_int((yyvsp[(2) - (2)].strgval)->substr(2,3));
                       int min=padded_string_to_int((yyvsp[(2) - (2)].strgval)->substr(3,2));
                       (yyval.pnode)=_SETD(appModelTimeFactory->getReferenceSeasonNode(mon,day,hour,min));
					   ;}
    break;

  case 75:

    { int mon=month_to_int((yyvsp[(1) - (1)].strgval)->substr(2,3));
                       int day=padded_string_to_int((yyvsp[(1) - (1)].strgval)->substr(0,2));
                       (yyval.pnode)=_SETD(appModelTimeFactory->getReferenceSeasonNode(mon,day,0,0));
					   ;}
    break;

  case 76:

    { (yyval.pnode)=_SETD(appModelTimeFactory->getYearNode()); ;}
    break;

  case 77:

    { (yyval.pnode)=_SETD(appModelTimeFactory->getMonthNode()); ;}
    break;

  case 78:

    { (yyval.pnode)=_SETD(appModelTimeFactory->getDayNode()); ;}
    break;

  case 79:

    { (yyval.pnode)=_SETD(appModelTimeFactory->getHourNode()); ;}
    break;

  case 80:

    { (yyval.pnode)=_SETD(appModelTimeFactory->getMinOfDayNode()); ;}
    break;

  case 81:

    { (yyval.pnode)=_SETD(appModelTimeFactory->getMinNode()); ;}
    break;

  case 82:

    {
                         argmap["modelname"]=*(yyvsp[(1) - (4)].strgval);
						 try{
						   (yyval.pnode)=_SETD(lookup->getModelExpression(*(yyvsp[(1) - (4)].strgval),argmap));
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
			;}
    break;

  case 83:

    { 
             argmap["modelname"]=*(yyvsp[(1) - (1)].strgval);
			 try{
                (yyval.pnode)=_SETD(lookup->getModelExpression(*(yyvsp[(1) - (1)].strgval),argmap));
			 }catch(oprule::parser::ModelNameNotFound& e){
						    string message("Model name not found: ");
						    op_ruleerror((message+e.what()).c_str());
							YYERROR;
			 } 
           ;}
    break;

  case 84:

    { (yyval.pnode)=(yyvsp[(1) - (1)].pnode); ;}
    break;

  case 85:

    { 
                         argmap["modelname"]=*(yyvsp[(1) - (4)].strgval);
						 try{
						    (yyval.pnode)=_SETD(lookup->getModelExpression(*(yyvsp[(1) - (4)].strgval),argmap));
						 }catch(oprule::parser::MissingIdentifier& e){
						    string message("Missing identifier in ");
						    op_ruleerror((message+*(yyvsp[(1) - (4)].strgval)+":  "+e.what()).c_str());
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
			;}
    break;

  case 86:

    { 
             argmap["modelname"]=*(yyvsp[(1) - (1)].strgval);
			 try{
                (yyval.pnode)=_SETD(lookup->getModelExpression(*(yyvsp[(1) - (1)].strgval),argmap));
			 }catch(oprule::parser::ModelNameNotFound& e){
						    string message("Model name not found: ");
						    op_ruleerror((message+e.what()).c_str());
							YYERROR;
			 }
           ;}
    break;

  case 87:

    { (yyval.pnode)=(yyvsp[(1) - (1)].pnode); ;}
    break;

  case 88:

    { 
		 (yyval.pbnode) = (yyvsp[(1) - (1)].pbnode);
	;}
    break;


/* Line 1267 of yacc.c.  */

      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
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

  /* Else will try to reuse look-ahead token after shifting the error
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

  /* Do not reclaim the symbols of the rule which action triggered
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
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
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

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


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

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
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
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}






BoolNodePtr getBoolExpression(const char * name = 0){
  if (name == 0){
    return express[EVAL_STR].boolval;
  }
  else
  { 
    string sb=string(name);
    return express[sb].boolval;
  }
}

DoubleNodePtr getDoubleExpression(const char *name = 0){
  if (name ==0)
  {
     return express[EVAL_STR].dblval;
  }
  else 
  { 
     string sd=string(name);
     return express[sd].dblval;
  }
}

OperatingRule* getOperatingRule(){
  return eval_rule;
}


template<class T1>
bool add_to_collection(T1 &collection, 
                       const typename T1::key_type key,
                       typename T1::referent_type& item){
   typedef T1::iterator IterType;
   typedef T1::referent_type ReferentType;
   typedef T1::key_type KeyType;
   if (key == EVAL_STR){
      collection[EVAL_STR]=item;
	  return true;
   }
   IterType i = collection.find(key);
   if (i == collection.end())
   {
      pair<KeyType,ReferentType> vb = make_pair<KeyType,ReferentType>(key, item);
	  collection.insert(vb);
      return true;
   }else{
      return false;
   }
}



bool add_symbol( const string & key, symbol& expression){
   return add_to_collection<express_map>( express, key, expression);
}

bool add_rule(const string & name, OperatingRule * rule){
  return add_to_collection<rule_map>(rulenames, name, rule);
}

void init_lookup(NamedValueLookup * a_lookup){
   lookup=a_lookup;
}

void init_model_time_factory(ModelTimeNodeFactory * tnf){
   appModelTimeFactory=tnf;
}

void init_action_factory(ModelActionFactory* factory){
  action_factory=factory;
}

void init_rule_names(){rulenames.clear();}


void init_expression(){
  if (! express.empty()){
    //for_each(express.begin(),express.end(), symbol_deleter());
     express.clear(); //erase(remove(express)); //@todo is erase-remove idiom correct for map?
   }
} 

NamedValueLookup& get_lookup(){
   return *lookup;
}

symbol name_lookup(const string& name){
   
   // attempt to look up previously defined and named oprule expression
   expressIterType i = express.find(name);            
   // attempt oprule (previously parsed) expression
   if (i != express.end()) {
     return ((*i).second);
   }else{
     return symbol();
   }   
     
}


symbol make_symbol(DoubleNodePtr dnode){
  symbol result(dnode);
  return result; 
}

symbol make_symbol(BoolNodePtr bnode){
  symbol result(bnode);
  return result; 
 }


OperationAction* chain_actions( ActionChain* op1, OperationAction* op2){
  op1->pushBackAction(op2);
  return op1;
} 

OperationAction* chain_actions( OperationAction* op1, OperationAction* op2){
   ActionChain * returnChain=new ActionChain();
   returnChain->pushBackAction(op1);
   returnChain->pushBackAction(op2);
   return returnChain;
}


OperationAction* group_actions( OperationAction* op1, OperationAction* op2){
  assert(op1 != NULL);
  assert(op1->isApplicable());
  ActionSet* setptr=dynamic_cast<ActionSet*>(op1);
  if (setptr){ 
     setptr->addAction(op2);
     return setptr;
  }
  else{ ActionSet * returnSet=new ActionSet();
        returnSet->addAction(op1);
        returnSet->addAction(op2);
		return returnSet;
  }
}


void clear_arg_map(){ argmap.clear();}

void add_expression(const string& name, 
                    DoubleNodePtr expr){
  oprule::parser::symbol asymbol(expr);
     add_symbol(name, asymbol);
}


void apply_lagged_values(const string& name, DoubleNodePtr expr){
  for(lagged_expression_list::iterator i =laggedvals.begin(); 
                           i < laggedvals.end(); i++){
    if (i->first == name){
        i->second->setExpression(expr);
	}else{
        i->second->setExpression(name_lookup(name).dblval);
    }
  }
}


void set_parsed_type(oprule::parser::parse_type ptype){ _parsed_type = ptype; }


oprule::parser::parse_type get_parsed_type(){ return _parsed_type; }


void clear_temp_expr(void){
  express_index.clear();
}




