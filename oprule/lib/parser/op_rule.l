
/* Lexer specification for operating rules*/

%option never-interactive case-insensitive

%{

// The following pragmas disable stupid warnings in MSVC
#pragma warning (disable:4786)   // identifier truncated to '255' characters
#pragma warning (disable:4065)   // switch contains 'default'


#include<stdlib.h>
#include<string.h>
#include<iostream>
#include<string>
//#include<cstddef>
#include<map>
#include<vector>
#include "stddef.h"
#include "oprule/expression/ExpressionNode.h"
#include "oprule/parser/Symbol.h"
#include "oprule/rule/ModelInterface.h"
#include "oprule/rule/ModelAction.h"
#include "oprule/rule/Transition.h"
#include "oprule/rule/OperatingRule.h"
#include "oprule/parser/NamedValueLookup.h"
#include "oprule/rule/OperationAction.h"
#include "oprule/rule/ExpressionTrigger.h"
#include "op_rule_tab.h"


using namespace std;
using namespace oprule::parser;
using namespace oprule::rule;
using namespace oprule::expression;


#define YYPARSE op_ruleparse
#define YYLEX op_rulelex
#undef YY_INPUT
#define YY_INPUT(b, r, ms) (r = yaccstring(b, ms))
#define yylval op_rulelval

extern symbol name_lookup(const std::string&);
extern NamedValueLookup& get_lookup();


//extern template int reg_temp_expr<double>( ExpressionNode<double>::NodePtr);
//extern template int reg_temp_expr<bool>( ExpressionNode<bool>::NodePtr);

template<typename T>
int reg_temp_expr(typename ExpressionNode<T>::NodePtr ptr);

extern void clear_temp_expr();
extern void clear_arg_map();

#define _SETD reg_temp_expr<double>
#define _SETB reg_temp_expr<bool>

char *strinput;
char *strinputptr; 
char *inputlim;
map<string,int > monthmap;
double month_to_double(const string& mstr);
int month_to_int(const string& mstr);



int yaccstring(char* buf, int max_size)
{
   int n=__min(max_size,inputlim-strinputptr);
   if(n>0){
      memcpy(buf, strinputptr, n);
	  strinputptr += n;
   }
   return n;
}

%}

mon JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC|jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec


%%

         /* Puctuation and operators*/

         /* Math operations */ 


[-+*/:()\[\],.^t;] {return yytext[0];}

"<>"    {return NE;}
"=="    {return EQ;}
">="    {return GE;}
"<="    {return LE;}
"<"     {return LT;  }
">"     {return GT;}

sqrt {return SQRT;}
exp {return EXP;}
log {return LOG;}
ln  {return LN;}
max2 {return MAX2;}
min2 {return MIN2;}
max3 {return MAX3;}
min3 {return MIN3;}
lookup|LOOKUP {return LOOKUP;}

false|FALSE {return FALSE;}
true|TRUE { return TRUE;}

ifelse|IFELSE {return IFELSE;}
or|OR    {return OR;}   
and|AND   {return AND;}
not|NOT   {return NOT;}

SET   { return SET; }
TO    { return TO; }
WHEN { return WHEN;}
WHILE { return WHILE;}
THEN  { return THEN;}
RAMP  { return RAMP;}
STEP  { return STEP;}
ACCUMULATE|accumulate   {return ACCUMULATE;}
PREDICT|predict|Predict {return PREDICT;}
LINEAR|linear|Linear    {return LINEAR; }
QUAD|quad|Quad          {return QUAD;   }
PID|pid                 {return PID;   }


":="    {return ASSIGN;}
"="     {return DEFINE; } 



 /* Terminal quanitities: named time series, numbers, etc. */
[0-3][0-9]{mon}[123][0-9]{3}  { yylval.strgval=new string(yytext); 
							   return REFDATE;  }
[0-2][0-9]:[0-5][0-9]       { yylval.strgval=new string(yytext); 
							    return REFTIME; }

[0-3][0-9]{mon}               { yylval.strgval=new string(yytext); return REFSEASON; }
{mon}   { yylval.dval=month_to_double(yytext); return NUMBER; }

date|DATE|DATETIME {return DATETIME;}
season|SEASON {return SEASON;}

year|YEAR    {yylval.strval=yytext; return YEAR;}
month|MONTH   {yylval.strval=yytext; return MONTH;}
day|DAY     {yylval.strval=yytext; return DAY;}
hour|HOUR    {yylval.strval=yytext; return HOUR;}
min|MIN     {yylval.strval=yytext; return MIN;}
dt|DT       {yylval.strval=yytext; return DT;} 

(([0-9]+)|([0-9]*\.[0-9]*)([eE][-+]?[0-9]+)?)  { 
             /* number, possibly with exponent*/
             yylval.dval = atof(yytext); return NUMBER; }  
[\t];                             /* ignore whitespace */

[\"\'][A-Za-z][ _@A-Za-z0-9\-]*[\"\'] {
            //cout << "lexing quoted name: " << yytext <<endl;
			string parsetext(yytext);
			size_t beg=parsetext.find_first_not_of(" \"\'");
			size_t end=parsetext.find_last_not_of(" \"\'");
			yylval.strgval=new string(parsetext,beg, end-beg+1);
            return QUOTEDSTRING;
			}


[A-Za-z][_@A-Za-z0-9]*  {
            // A generic name, not a reserved word in the oprule language:
			// We will look it up in this order:
			//     1. named oprule expressions
			//     2. model variables and names (including constants, time
			//        series, etc...anything the model provider designates as
			//        a named, retrievable variable.
			//     3. a new name -- we give the parser the name as a string
            std::string * name=new std::string(yytext);
			symbol sym=name_lookup(*name);
			// First shot: look up names that have been defined
			// earlier in oprule expressions 
			if (sym.type() != symbol.EMPTY ){
			   if (sym.type() == symbol.DOUBLE) { 
			      yylval.pnode=_SETD(sym.dblval); 
				  return NAMEDVAL;}                  
			   else if(sym.type()== symbol.BOOL){ 
			      yylval.pbnode=_SETB(sym.boolval); return BOOLNAMEDVAL; 
			   }
			 }
			// Second shot: ask the model-provided NamedValueLookup 
			// to interpret the name
            NamedValueLookup & lookup = get_lookup();

            if ( lookup.isModelName(*name) ){
               if (lookup.readWriteType(*name)== NamedValueLookup::READWRITE){
                 if(lookup.takesArguments(*name)){
				    clear_arg_map();
				    yylval.strgval=name;
                    return IFPARAM;
                 }else{
				    yylval.strgval=name;
	                return IFNAME;
                 }
               }else{
			     assert( lookup.readWriteType(*name)==NamedValueLookup::READONLY);
                 if(lookup.takesArguments(*name)){
				   clear_arg_map();
                   yylval.strgval=name;
				   return EXPRESSPARAM;
                 }else{
				    yylval.strgval=name;
	                return EXPRESS;
                 }
			     assert(false);
			     return NAME; // should never get here
               }
			}

			// Last shot: just return the name.
			yylval.strgval=new string(yytext);
            return NAME;
		}


.|\n { if (strcmp(yytext," ") != 0) cout << "Unmatched: " << yytext << ";" << endl;}


%%
#ifndef yaccinwrap
int op_rulewrap() { return 1; }
#endif

void set_input_string(char* input){
   strinput = input;
   strinputptr = input;
   inputlim=&strinput[strlen(strinput)-1]+1;
}

void set_input_string(string & input){
   set_input_string( const_cast<char*>(input.c_str()) );
}



int op_ruleerror(const char * s){
   cerr << endl << "Error: " << s << "\nin expression  " << strinput << endl;
   cerr << "Error at or before token: '" << yytext << "'"<< endl;
   return 1;
}



void fillmonthmap(){ 
 typedef pair<string,int > strgint;
 monthmap.insert( strgint("JAN",1));
 monthmap.insert( strgint("FEB",2));
 monthmap.insert( strgint("MAR",3));
 monthmap.insert( strgint("APR",4));
 monthmap.insert( strgint("MAY",5));
 monthmap.insert( strgint("JUN",6));
 monthmap.insert( strgint("JUL",7));
 monthmap.insert( strgint("AUG",8));
 monthmap.insert( strgint("SEP",9));
 monthmap.insert( strgint("OCT",10));
 monthmap.insert( strgint("NOV",11));
 monthmap.insert( strgint("DEC",12));
}

double month_to_double(const string& mstr){
   return (double)monthmap[mstr];
}

int month_to_int(const string& mstr){
   return monthmap[mstr];
}


void lexer_init(){
  fillmonthmap();
}