/* A Bison parser, made by GNU Bison 1.875b.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

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
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     NAME = 258,
     NAMEDVAL = 259,
     BOOLNAMEDVAL = 260,
     LT = 261,
     LE = 262,
     GT = 263,
     GE = 264,
     EQ = 265,
     NE = 266,
     IFELSE = 267,
     SQRT = 268,
     LOG = 269,
     LN = 270,
     EXP = 271,
     SET = 272,
     TO = 273,
     WHERE = 274,
     WHILE = 275,
     THEN = 276,
     RAMP = 277,
     YEAR = 278,
     MONTH = 279,
     DAY = 280,
     HOUR = 281,
     MINDAY = 282,
     MIN = 283,
     SEASON = 284,
     QGATE = 285,
     NUMBER = 286,
     QUOTEDSTRING = 287,
     ASSIGN = 288,
     FALSE = 289,
     TRUE = 290,
     OR = 291,
     AND = 292,
     NOT = 293,
     DEFINE = 294,
     NEG = 295
   };
#endif
#define NAME 258
#define NAMEDVAL 259
#define BOOLNAMEDVAL 260
#define LT 261
#define LE 262
#define GT 263
#define GE 264
#define EQ 265
#define NE 266
#define IFELSE 267
#define SQRT 268
#define LOG 269
#define LN 270
#define EXP 271
#define SET 272
#define TO 273
#define WHERE 274
#define WHILE 275
#define THEN 276
#define RAMP 277
#define YEAR 278
#define MONTH 279
#define DAY 280
#define HOUR 281
#define MINDAY 282
#define MIN 283
#define SEASON 284
#define QGATE 285
#define NUMBER 286
#define QUOTEDSTRING 287
#define ASSIGN 288
#define FALSE 289
#define TRUE 290
#define OR 291
#define AND 292
#define NOT 293
#define DEFINE 294
#define NEG 295




#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)

typedef union YYSTYPE {
   oprule::symbol* symval;
   int intval;
   double dval;
   char * strval;
   std::string* strgval;
   DoubleNodePtr pnode;
   BoolNodePtr pbnode;
   ModelInterfaceDouble * modelif;
   ExpressionTrigger * trigval;
   OperationAction * opaction;
   OperatingRule * opruleval;
} YYSTYPE;
/* Line 1252 of yacc.c.  */

# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE op_rulelval;



