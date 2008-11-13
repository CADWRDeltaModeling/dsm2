/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

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
/* Line 1489 of yacc.c.  */

	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE op_rulelval;

