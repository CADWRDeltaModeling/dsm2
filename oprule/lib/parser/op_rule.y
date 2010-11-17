%{
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

%}

%error-verbose
%union {
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
}
%token <stringval> NAME
%token <stringval> IFNAME IFPARAM EXPRESS EXPRESSPARAM
%token <pnode> NAMEDVAL
%token <pbnode> BOOLNAMEDVAL
%token <intval> LT LE GT GE EQ NE
%token <intval> MAX2 MIN2 MAX3 MIN3
%token <intval> PID IPID
%token <intval> LOOKUP
%token <intval> IFELSE
%token <intval> ACCUMULATE
%token <intval> PREDICT LINEAR QUAD
%token <intval> SQRT LOG LN EXP ABS
%token <intval> SET TO WHEN WHILE THEN RAMP STEP
%token <intval> YEAR MONTH DAY HOUR MINDAY MIN DT
%token <intval> SEASON
%token <intval> DATETIME
%token <stringval> REFDATE REFTIME REFSEASON
%token <dval> NUMBER
%token <stringval> QUOTEDSTRING
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
%type <transition>  transitionspec
%type <pbnode> namedboolval
%type <symval> arg
%type <symval> arglist
%type <trigval> trigger
%type <opaction> action
%type <opaction> modelaction
%type <opruleval> oprule
%type <dvectorndx> array
%type <intval> reassignment
%%


line:
    NAME ASSIGN boolexpression ';' {
	   string dbkey=get_temp_symbol($1).stringval;
	   add_symbol(dbkey,_GETS($3));
	   set_parsed_type(oprule::parser::BOOL_EXPRESS);
	   clear_string_list();
	   clear_temp_expr();
        }
   | NAME ASSIGN expression ';' {
	   string dbkey=get_temp_symbol($1).stringval;
	   add_symbol(dbkey,_GETS($3));
	   apply_lagged_values(dbkey,_GETD($3));
	   set_parsed_type(oprule::parser::NUMERICAL_EXPRESS);
	   clear_string_list();
	   clear_temp_expr();
        } 
   | boolexpression ';'{
       string dbkey=EVAL_STR;
	   add_symbol(dbkey,_GETS($1));
	   set_parsed_type(oprule::parser::BOOL_EXPRESS);
	   clear_string_list();	   
	    }
   | expression ';'{
       string dbkey=EVAL_STR;
	   add_symbol(dbkey,_GETS($1));
	   set_parsed_type(oprule::parser::NUMERICAL_EXPRESS);
	   clear_string_list();	   
	   }
   | NAME ASSIGN oprule ';' {
	    string rulename=get_temp_symbol($1).stringval;
	    OperatingRulePtr rulePtr = get_temp_symbol($3).rule;
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
   | oprule ';' {
		static int ruleno;
        char num[16];
        _itoa_s(ruleno++,num,16,10);
        OperatingRulePtr opPtr = get_temp_symbol($1).rule;
        string rulename = string("OpRule") + num;
        opPtr->setName(rulename);
        add_rule(rulename,opPtr);
	    set_parsed_type(oprule::parser::OP_RULE);
	    clear_string_list();
	    clear_temp_expr();
	    clear_arg_map();
		}
   | reassignment {               // really just captures error
        set_parsed_type(oprule::parser::REASSIGNMENT);
		}

   | error ';' {
                cerr << "Error parsing expression." << endl;
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
action trigger{
     OperationActionPtr  act     = get_temp_symbol($1).action;
     TriggerPtr trigger = get_temp_symbol($2).trigger;
	 OperatingRulePtr op(new OperatingRule(act,trigger));
	 symbol s(op);
	 $$ = add_temp_symbol(op);
     add_rule(EVAL_STR,op);
   }


trigger: 
WHEN boolexpression{
              TriggerPtr trigger(new ExpressionTrigger(_GETB($2)->copy()));
              symbol s(trigger);
              $$=add_temp_symbol(s);
			  }

action:
	modelaction { $$=$1; }
  | action THEN action { 
            OperationActionPtr firstPtr = get_temp_symbol($1).action;
            OperationActionPtr secondPtr = get_temp_symbol($3).action;
            OperationActionPtr chain = chain_actions(firstPtr,secondPtr);
            $$ = add_temp_symbol(chain);
            }
  | action WHILE action {
            OperationActionPtr firstPtr = get_temp_symbol($1).action;
            OperationActionPtr secondPtr = get_temp_symbol($3).action;
            OperationActionPtr chain = group_actions(firstPtr,secondPtr);
            $$ = add_temp_symbol(chain);
            }
  | '(' action ')' { $$=$2; }

modelaction:
    actionspec targetspec transitionspec{ 
			 ModelInterface<double>::NodePtr ifc
			   =boost::static_pointer_cast<ModelInterface<double> >( _GETD($1) );
			 TransitionPtr transition = get_temp_symbol($3).transition;
			 OperationActionPtr act( new ModelAction<double>(ifc,
			                        _GETD($2)->copy(),
			                        transition));
			 $$=add_temp_symbol(act); //was ModelAction, also below was too
	}

  | actionspec targetspec{ 
			 ModelInterface<double>::NodePtr ifc
			   =boost::static_pointer_cast<ModelInterface<double> >( _GETD($1) );
			 TransitionPtr abrupt(new AbruptTransition());
			 OperationActionPtr act( new ModelAction<double>(ifc,_GETD($2)->copy(), abrupt));
			 $$=add_temp_symbol(act);
			}

actionspec:
   SET interface {$$=$2;}
;
targetspec:
   TO expression {$$=$2;}
;
transitionspec:
   RAMP NUMBER MIN {
                    TransitionPtr t(new LinearTransition($2*60.));
                    $$=add_temp_symbol(t);
                    }

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
   '[' NUMBER   {
                  $$ = add_array_vector();
                  get_array_vector($$).push_back($2); 
                }
 | array ',' NUMBER {get_array_vector($1).push_back($3)}
 | array ']'
;

unary:
   SQRT '(' expression ')' {  
            $$=_SETD(UnaryOpNode<sqrt_func>::create(_GETD($3)));}
 | ABS '(' expression ')'  {
           $$=_SETD(UnaryOpNode<abs_func>::create(_GETD($3)));}
 | LOG '(' expression ')'  {
           $$=_SETD(UnaryOpNode<log10_func>::create(_GETD($3)));}
 | LN '(' expression ')'   {
           $$=_SETD(UnaryOpNode<ln_func>::create(_GETD($3)));}
 | EXP '(' expression ')'  {
           $$=_SETD(UnaryOpNode<exp_func>::create(_GETD($3)));}
 | LOOKUP '(' expression ',' array ',' array ')' {
           $$=_SETD(LookupNode::create(_GETD($3), 
                                       get_array_vector($5), 
                                       get_array_vector($7)));
           get_array_vector($5).clear();
           get_array_vector($7).clear();
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
 |  PID '(' expression ',' expression ',' expression ',' expression ',' expression ',' expression ',' expression ',' expression ',' expression ')'
        {  
        $$=_SETD(PIDNode::create( _GETD($3),_GETD($5),_GETD($7)->eval(),_GETD($9)->eval(),_GETD($11)->eval(),_GETD($13)->eval(),
                                  _GETD($15)->eval(),_GETD($17)->eval(),_GETD($19)->eval()));
        }
 |  IPID '(' expression ',' expression ',' expression ',' expression ',' expression ',' expression ',' expression ',' expression ',' expression ')'
        {  
        $$=_SETD(IncrementalPIDNode::create( _GETD($3),_GETD($5),_GETD($7),_GETD($9)->eval(),_GETD($11)->eval(),_GETD($13)->eval(),
                                  _GETD($15)->eval(),_GETD($17)->eval(),_GETD($19)->eval() ));
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
               const string nm=get_temp_symbol($1).stringval;;
               get_lagged_vals().push_back(
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
                         get_arg_map()[get_temp_symbol($1).stringval]=ToString<double>($3);
					   }
  | NAME DEFINE QUOTEDSTRING
                       { 
					     get_arg_map()[get_temp_symbol($1).stringval]=get_temp_symbol($3).stringval;
						}
  | NAME DEFINE NAME   { 
                        get_arg_map()[get_temp_symbol($1).stringval]=get_temp_symbol($3).stringval;
                       }
;


date:
   DATETIME {$$=_SETD(get_time_factory()->getDateTimeNode());}
 | REFDATE REFTIME {$$=_SETD(get_time_factory()->getDateTimeNode(get_temp_symbol($1).stringval,
                                                                 get_temp_symbol($2).stringval));
                   }
 | REFDATE { $$=_SETD(get_time_factory()->getDateTimeNode(get_temp_symbol($1).stringval,"00:00"));}
 | SEASON { $$=_SETD(get_time_factory()->getSeasonNode()); }
 | REFSEASON REFTIME { int mon=month_to_int(get_temp_symbol($1).stringval.substr(2,3));
                       int day=padded_string_to_int(get_temp_symbol($1).stringval.substr(0,2));
                       int hour=padded_string_to_int(get_temp_symbol($2).stringval.substr(2,3));
                       int min=padded_string_to_int(get_temp_symbol($2).stringval.substr(3,2));
                       $$=_SETD(get_time_factory()->getReferenceSeasonNode(mon,day,hour,min));
					   }
 | REFSEASON         { int mon=month_to_int(get_temp_symbol($1).stringval.substr(2,3));
                       int day=padded_string_to_int(get_temp_symbol($1).stringval.substr(0,2));
                       $$=_SETD(get_time_factory()->getReferenceSeasonNode(mon,day,0,0));
					   }
 | YEAR   { $$=_SETD(get_time_factory()->getYearNode()); }
 | MONTH  { $$=_SETD(get_time_factory()->getMonthNode()); }
 | DAY    { $$=_SETD(get_time_factory()->getDayNode()); }
 | HOUR   { $$=_SETD(get_time_factory()->getHourNode()); }
 | MINDAY { $$=_SETD(get_time_factory()->getMinOfDayNode()); }
 | MIN    { $$=_SETD(get_time_factory()->getMinNode()); }  
 | DT     { $$=_SETD(get_time_factory()->getTimeStepNode()); }   
;

interface:
   IFPARAM '(' arglist ')' {
                         string ifname=get_temp_symbol($1).stringval;
                         get_arg_map()["modelname"]=ifname;
						 try{
						   $$=_SETD(get_lookup()->getModelExpression(ifname,get_arg_map()));
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
             string ifname=get_temp_symbol($1).stringval;
             get_arg_map()["modelname"]=ifname;
			 try{
                $$=_SETD(get_lookup()->getModelExpression(ifname,get_arg_map()));
			 }catch(oprule::parser::ModelNameNotFound& e){
						    string message("Model name not found: ");
						    op_ruleerror((message+e.what()).c_str());
							YYERROR;
			 } 
           }


namedval:
    NAMEDVAL { $$=$1; }
  | EXPRESSPARAM '(' arglist ')' { 
                         string valname=get_temp_symbol($1).stringval;
                         get_arg_map()["modelname"]=valname;
						 try{
						    $$=_SETD(get_lookup()->getModelExpression(valname,get_arg_map()));
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
  | EXPRESS { 
             string expressname=get_temp_symbol($1).stringval;
             get_arg_map()["modelname"]=expressname;
			 try{
                $$=_SETD(get_lookup()->getModelExpression(expressname,get_arg_map()));
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




