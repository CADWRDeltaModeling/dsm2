/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

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
