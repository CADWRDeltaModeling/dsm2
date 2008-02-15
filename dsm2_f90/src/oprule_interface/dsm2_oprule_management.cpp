#include <string>
#include <iostream>
#include "oprule/expression/ExpressionNode.h" 
#include "oprule/parser/NamedValueLookup.h"
#include "oprule/parser/ModelTimeNodeFactory.h"
#include "oprule/parser/ModelActionFactory.h"
#include "oprule/rule/OperatingRule.h"
#include "oprule/rule/OperationManager.h"
#include "oprule/parser/ParseResultType.h"
#include "oprule/rule/ModelInterfaceActionResolver.h"

#include "dsm2_named_value_lookup.h"
#include "dsm2_time_node_factory.h"
#include "dsm2_model_action_factory.h"
#include "dsm2_time_interface.h"
#include "dsm2_model_interface_resolver.h"

using namespace oprule::rule;
using namespace oprule::parser;
using namespace oprule::expression;
using namespace std;

extern parse_type get_parsed_type();
extern void init_expression();
extern void lexer_init();
extern void add_expression(const string& name, 
                           oprule::expression::DoubleNodePtr expr);
extern void init_lookup( NamedValueLookup*);
extern void init_model_time_factory(ModelTimeNodeFactory * );
extern void init_action_factory(ModelActionFactory *);
extern void set_input_string(string &);
extern void set_input_string(char*);
extern int op_ruleparse();
extern OperatingRule* getOperatingRule();

#define STDCALL 
#define init_parser_f STDCALL INIT_PARSER_F
#define advance_op_rules STDCALL ADVANCEOPRULES
#define test_rule_activation STDCALL TESTOPRULEACTIVATION
#define parse_rule PARSE_RULE



ModelInterfaceActionResolver<DSM2ModelTimer, DSM2Resolver,
                             DSM2ModelInterfaceResolver> resolver;
OperationManager dsm2_op_manager(resolver);

using namespace std;
extern "C"{
void init_parser_f(){
     lexer_init();
     init_expression();
     init_lookup(new DSM2HydroNamedValueLookup());
     init_model_time_factory(new  DSM2HydroTimeNodeFactory());
     init_action_factory(new dsm2_model_action_factory());
}

bool advance_op_rules(){
   dsm2_op_manager.advanceStep();
   return true;
}

bool test_rule_activation(){
   dsm2_op_manager.manageActivation();
   return true;
}

bool parse_rule(char* text, int len){
   string parse_str(text,len);
   set_input_string(parse_str);
   int parseok=op_ruleparse(); // make sure it was a rule??
   if( !(parseok==0) || get_parsed_type() == oprule::parser::PARSE_ERROR)return false;
   if (get_parsed_type() == oprule::parser::OP_RULE){
     OperatingRule * rule=getOperatingRule();
     dsm2_op_manager.addRule(rule);
   }
   return true;
}
}


