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


%{
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
#include "oprule/rule/ModelAction.h"
#include "oprule/rule/Transition.h"
#include "oprule/expression/TernaryOpNode.h"
#include "oprule/expression/BinaryOpNode.h"
#include "oprule/expression/UnaryOpNode.h"
#include "oprule/expression/LookupNode.h"
#include "oprule/expression/LaggedExpressionNode.h"
#include "oprule/expression/AccumulationNode.h"
#include "oprule/expression/LinearExtrapolationNode.h"
#include "oprule/expression/QuadExtrapolationNode.h"
#include "oprule/expression/ExpressionPtr.h"
#include "oprule/expression/PIDNode.h"

#include "oprule/parser/Symbol.h"
#include "oprule/parser/ModelNameParseError.h"

#include "oprule/rule/ExpressionTrigger.h"
#include "oprule/rule/OperatingRule.h"
#include "oprule/parser/NamedValueLookup.h"
#include "oprule/rule/OperationAction.h"
#include "oprule/rule/ActionSet.h"
#include "oprule/rule/ActionChain.h"
#include "oprule/parser/ModelTimeNodeFactory.h"
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


#define _SETD reg_temp_expr<double>
#define _SETB reg_temp_expr<bool>
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
 


%}

%error-verbose
%union {
   oprule::parser::symbol* symval;
   int intval;
   double dval;
   char * strval;
   std::string* strgval;
   int pnode;
   int pbnode;
   std::vector<double>* dvector;
   oprule::rule::Transition * ptransition;
   oprule::rule::ModelInterfaceDouble * modelif;
   oprule::rule::ExpressionTrigger * trigval;
   oprule::rule::OperationAction * opaction;
   oprule::rule::OperatingRule * opruleval;
}
%token <strgval> NAME
%token <strgval> IFNAME IFPARAM EXPRESS EXPRESSPARAM
%token <pnode> NAMEDVAL
%token <pbnode> BOOLNAMEDVAL
%token <intval> LT LE GT GE EQ NE
%token <intval> MAX2 MIN2 MAX3 MIN3
%token <intval> PID
%token <intval> LOOKUP
%token <intval> IFELSE
%token <intval> ACCUMULATE
%token <intval> PREDICT LINEAR QUAD
%token <intval> SQRT LOG LN EXP
%token <intval> SET TO WHEN WHILE THEN RAMP STEP
%token <intval> YEAR MONTH DAY HOUR MINDAY MIN DT
%token <intval> SEASON
%token <intval> DATETIME
%token <strgval> REFDATE REFTIME REFSEASON
%token <dval> NUMBER
%token <strgval> QUOTEDSTRING
%token <intval> ASSIGN
%token <intval> FALSE TRUE
%token <intval> OR AND
%token <intval> NOT
%token <intval> DEFINE


%left LE GE NE EQ GT LT
%left OR
%left AND
%right NOT
%left '+' '-'
%left '*' '/'
%left '^'
%right NEG

%left WHILE
%left THEN


%type <pnode> term
%type <pbnode> boolterm
%type <pnode> unary
%type <pnode> date
%type <pnode> expression
%type <pbnode> boolexpression
%type <intval> line
%type <pnode> interface
%type <pnode> namedval
%type <pnode> laggedval
%type <pnode> actionspec
%type <pnode> targetspec
%type <ptransition>  transitionspec
%type <pbnode> namedboolval
%type <symval> arg
%type <symval> arglist
%type <trigval> triggerexpression
%type <opaction> action
%type <opaction> modelaction
%type <opruleval> oprule
%type <dvector> array

%type <intval> reassignment
%%


line:
    NAME ASSIGN boolexpression ';' {
	   string dbkey=*$1;
	   add_symbol(dbkey,_GETS($3));
	   set_parsed_type(oprule::parser::BOOL_EXPRESS);
        }
   | NAME ASSIGN expression ';' {
	   string dbkey=*$1;
	   add_symbol(dbkey,_GETS($3));
	   apply_lagged_values(*$1,_GETD($3));
	   set_parsed_type(oprule::parser::NUMERICAL_EXPRESS);
        } 
   | boolexpression ';'{
       string dbkey=EVAL_STR;
	   add_symbol(dbkey,_GETS($1));
	   set_parsed_type(oprule::parser::BOOL_EXPRESS);
	    }
   | expression ';'{
       string dbkey=EVAL_STR;
	   add_symbol(dbkey,_GETS($1));
	   set_parsed_type(oprule::parser::NUMERICAL_EXPRESS);
	   }
   | NAME ASSIGN oprule ';' {
        eval_rule=$3;
	    string rulename=*$1;
		if (add_rule(rulename,eval_rule)){
          ($3)->setName(rulename);
  	      set_parsed_type(oprule::parser::OP_RULE);   
        }else{
		  op_ruleerror(
		    ("Operating rule name " + rulename + " used more than once.").c_str());
          YYABORT;
		 }
        } 
   | oprule ';' {
        eval_rule=$1;
		static int ruleno;
        char * num=0;
        itoa(ruleno++,num,10);
		eval_rule->setName(string("OpRule") +num);
	    set_parsed_type(oprule::parser::OP_RULE);
		}
   | reassignment {               // really just captures error
        set_parsed_type(oprule::parser::REASSIGNMENT);
		}

   | error ';' {
                cout << "Error parsing expression." << endl;
				YYABORT}

; 

reassignment:
     boolexpression ASSIGN {
	     op_ruleerror("reassignment of (boolean-valued) variable");
	                       }
   | expression     ASSIGN {
         op_ruleerror("reassignment of variable");
		                   }

oprule:
action triggerexpression{
	 $$=new OperatingRule($1,$2);
   }


triggerexpression: 
WHEN boolexpression{
			  $$=new ExpressionTrigger(_GETB($2)->copy());
			  }

action:
	modelaction { $$=$1; }
  | action THEN action {$$=chain_actions($1, $3);  }
  | action WHILE action { $$=group_actions($1, $3);}
  | '(' action ')' { $$=$2; }

modelaction:
    actionspec targetspec transitionspec{ 
			 ModelInterface<double>::NodePtr ifc
			   =boost::static_pointer_cast<ModelInterface<double> >( _GETD($1) );
			 $$=new ModelAction<double>(ifc,_GETD($2)->copy(),*$3);
			 delete $3;
	}

  | actionspec targetspec{ 
			 ModelInterface<double>::NodePtr ifc
			   =boost::static_pointer_cast<ModelInterface<double> >( _GETD($1) );
			 $$=new ModelAction<double>(ifc,_GETD($2)->copy(), AbruptTransition());
			}

actionspec:
   SET interface {$$=$2;}
;
targetspec:
   TO expression {$$=$2;}
;
transitionspec:
   RAMP NUMBER MIN {$$=new LinearTransition($2*60.);}

;
term:
   NUMBER { 
      $$ =_SETD(DoubleScalarNode::create($1)); }
 | unary    { $$=$1; }
 | date     { $$=$1; }
 | namedval { $$=$1;}
 | laggedval{ $$=$1;}
;

 
boolterm:
   '(' boolexpression ')' { $$ = $2;}
 |  FALSE { $$=_SETB(BoolScalarNode::create(false)); }
 |  TRUE  { $$=_SETB(BoolScalarNode::create(true));  }
 |  namedboolval { $$ = $1;}
 ;

array:
   '[' NUMBER    {if ($$ != NULL){
                    $$->clear(); 
                    delete $$;
                  }
                  $$ = new vector<double>; 
                  $$->push_back($2); 
                  }
 | array ',' NUMBER {$1->push_back($3)}
 | array ']'
;

unary:
   SQRT '(' expression ')' {  
            $$=_SETD(UnaryOpNode<sqrt_func>::create(_GETD($3)));}
 | LOG '(' expression ')'  {
           $$=_SETD(UnaryOpNode<log10_func>::create(_GETD($3)));}
 | LN '(' expression ')'   {
           $$=_SETD(UnaryOpNode<ln_func>::create(_GETD($3)));}
 | EXP '(' expression ')'  {
           $$=_SETD(UnaryOpNode<exp_func>::create(_GETD($3)));}
 | LOOKUP '(' expression ',' array ',' array ')' {
           $$=_SETD(LookupNode::create(_GETD($3), *$5, *$7));
           $5->clear();
           $7->clear();
           delete $5;
           delete $7;
           }
;


expression:
    term { $$ = $1; }
 |  expression '^' expression { $$=_SETD(BinaryOpNode<power_func >::create(_GETD($1),_GETD($3)));}
 |  expression '*' expression { $$=_SETD(BinaryOpNode<multiplies<double> >::create(_GETD($1),_GETD($3)));}
 |  expression '/' expression { $$=_SETD(BinaryOpNode<divides<double> >::create(_GETD($1),_GETD($3)));}
 |  expression '+' expression { $$=_SETD(BinaryOpNode<plus<double> >::create(_GETD($1),_GETD($3)));}
 |  expression '-' expression { $$=_SETD(BinaryOpNode<minus<double> >::create(_GETD($1),_GETD($3)));}
 | '-' expression { $$=_SETD(UnaryOpNode<negate<double> >::create(_GETD($2)));}
 | '(' expression ')' {$$=$2;}
 |  IFELSE '(' boolexpression ',' expression ',' expression ')' {
                    $$=_SETD(TernaryOpNode<double>::create(_GETB($3),
                                                           _GETD($5),
                                                           _GETD($7))); }  
 |  ACCUMULATE '(' expression ',' expression ',' boolexpression ')' {
					$$=_SETD(AccumulationNode<double>::create(_GETD($3),
					                                          _GETD($5),
					                                          _GETB($7)));} 
 |  ACCUMULATE '(' expression ',' expression ')' {
					$$=_SETD(AccumulationNode<double>::create(_GETD($3),
					                                          _GETD($5),
					                                          BoolScalarNode::create(false))); }
 |  PREDICT '(' expression ',' LINEAR ',' NUMBER MIN  ')'  {$$=_SETD(LinearExtrapolationNode<double>::create(_GETD($3), $7 * 60.));}
 |  PREDICT '(' expression ',' QUAD   ',' NUMBER MIN  ')'  {$$=_SETD(QuadExtrapolationNode::create(_GETD($3), $7 * 60. ));}
 |  MIN2 '(' expression ',' expression ')' {$$=_SETD(BinaryOpNode<min2 >::create(_GETD($3),_GETD($5)));}
 |  MAX2 '(' expression ',' expression ')' {$$=_SETD(BinaryOpNode<max2 >::create(_GETD($3),_GETD($5)));}
 |  MIN3 '(' expression ',' expression ',' expression ')' {
        $$=_SETD(Min3Node<double>::create( _GETD($3),_GETD($5),_GETD($7)));}
 |  MAX3 '(' expression ',' expression ',' expression ')' {
        $$=_SETD(Max3Node<double>::create( _GETD($3),_GETD($5),_GETD($7)));}
 |  PID '(' expression ',' expression ',' NUMBER ',' NUMBER ',' NUMBER ',' NUMBER ',' NUMBER ')'
        {  //yset,yobs,k,ti,td,tt,b
        $$=_SETD(PIDNode::create( _GETD($3),_GETD($5),$7,$9,$11,$13,$15));
        }
 

;

boolexpression:
    boolterm {$$= $1;}
 |  expression LE expression { $$=_SETB(BinaryOpNode<less_equal<double> >::create(_GETD($1), _GETD($3)));}
 |  expression GE expression { $$=_SETB(BinaryOpNode<greater_equal<double> >::create(_GETD($1), _GETD($3)));}
 |  expression GT expression { $$=_SETB(BinaryOpNode<greater<double> >::create(_GETD($1), _GETD($3))); }
 |  expression LT expression { $$=_SETB(BinaryOpNode<less<double> >::create(_GETD($1), _GETD($3))); }
 |  expression EQ expression { $$=_SETB(BinaryOpNode<equal_to<double> >::create(_GETD($1), _GETD($3))); }
 |  expression NE expression { $$=_SETB(BinaryOpNode<not_equal_to<double> >::create(_GETD($1), _GETD($3))); }
 |  boolexpression OR boolexpression  { $$=_SETB(BinaryOpNode<logical_or<bool > >::create(_GETB($1), _GETB($3)));}
 |  boolexpression AND boolexpression { $$=_SETB(BinaryOpNode<logical_and<bool > >::create(_GETB($1), _GETB($3)));}
 |  NOT boolexpression                { $$=_SETB(UnaryOpNode<logical_not<bool> >::create(_GETB($2)));}
;


laggedval:
   NAME '(' 't' '-' NUMBER ')' { 
              LaggedExpressionNode<double>::NodePtr 
			     laggedNode=LaggedDoubleNode::create((int)$5);
               const string nm(*($1));
               laggedvals.push_back(
			      make_pair<string, LaggedExpressionNode<double>::NodePtr>(
				     nm,laggedNode));
				$$=_SETD(laggedNode);
				  }
;


arglist:
    arg
 |  arglist ',' arg
 |  arglist ';' arg 
;
 
arg:
   NAME DEFINE NUMBER  { 
                         argmap[*$1]=ToString<double>($3);
					   }
  | NAME DEFINE QUOTEDSTRING
                       { 
					     argmap[*$1]=*$3;
						}
  | NAME DEFINE NAME   { 
                        argmap[*$1]=*$3;
                       }
;


date:
   DATETIME {$$=_SETD(appModelTimeFactory->getDateTimeNode());}
 | REFDATE REFTIME {$$=_SETD(appModelTimeFactory->getDateTimeNode(*$1,*$2));}
 | REFDATE { $$=_SETD(appModelTimeFactory->getDateTimeNode(*$1,"00:00"));}
 | SEASON { $$=_SETD(appModelTimeFactory->getSeasonNode()); }
 | REFSEASON REFTIME { int mon=month_to_int($1->substr(2,3));
                       int day=padded_string_to_int($1->substr(0,2));
                       int hour=padded_string_to_int($2->substr(2,3));
                       int min=padded_string_to_int($2->substr(3,2));
                       $$=_SETD(appModelTimeFactory->getReferenceSeasonNode(mon,day,hour,min));
					   }
 | REFSEASON         { int mon=month_to_int($1->substr(2,3));
                       int day=padded_string_to_int($1->substr(0,2));
                       $$=_SETD(appModelTimeFactory->getReferenceSeasonNode(mon,day,0,0));
					   }
 | YEAR   { $$=_SETD(appModelTimeFactory->getYearNode()); }
 | MONTH  { $$=_SETD(appModelTimeFactory->getMonthNode()); }
 | DAY    { $$=_SETD(appModelTimeFactory->getDayNode()); }
 | HOUR   { $$=_SETD(appModelTimeFactory->getHourNode()); }
 | MINDAY { $$=_SETD(appModelTimeFactory->getMinOfDayNode()); }
 | MIN    { $$=_SETD(appModelTimeFactory->getMinNode()); }  
 | DT     { $$=_SETD(appModelTimeFactory->getTimeStepNode()); }   
;

interface:
   IFPARAM '(' arglist ')' {
                         argmap["modelname"]=*$1;
						 try{
						   $$=_SETD(lookup->getModelExpression(*$1,argmap));
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
  | IFNAME { 
             argmap["modelname"]=*$1;
			 try{
                $$=_SETD(lookup->getModelExpression(*$1,argmap));
			 }catch(oprule::parser::ModelNameNotFound& e){
						    string message("Model name not found: ");
						    op_ruleerror((message+e.what()).c_str());
							YYERROR;
			 } 
           }


namedval:
    NAMEDVAL { $$=$1; }
  | EXPRESSPARAM '(' arglist ')' { 
                         argmap["modelname"]=*$1;
						 try{
						    $$=_SETD(lookup->getModelExpression(*$1,argmap));
						 }catch(oprule::parser::MissingIdentifier& e){
						    string message("Missing identifier in ");
						    op_ruleerror((message+*$1+":  "+e.what()).c_str());
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
  | EXPRESS { 
             argmap["modelname"]=*$1;
			 try{
                $$=_SETD(lookup->getModelExpression(*$1,argmap));
			 }catch(oprule::parser::ModelNameNotFound& e){
						    string message("Model name not found: ");
						    op_ruleerror((message+e.what()).c_str());
							YYERROR;
			 }
           }
  | interface { $$=$1; }
   


;

namedboolval:
   BOOLNAMEDVAL { 
		 $$ = $1;
	}
;

%%


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



