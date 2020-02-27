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
#include "oprule/parser/ParseSymbolManagement.h"

#include "dsm2_named_value_lookup.h"
#include "dsm2_time_node_factory.h"
#include "dsm2_time_interface.h"
#include "dsm2_model_interface_resolver.h"

using namespace oprule::rule;
using namespace oprule::parser;
using namespace oprule::expression;
using namespace std;

extern void lexer_init();
extern void set_input_string(string &);
extern void set_input_string(char*);
extern int op_ruleparse();

#ifdef _WIN32
#define STDCALL 
#define init_parser_f STDCALL INIT_PARSER_F
#define advance_oprule_actions STDCALL ADVANCEOPRULEACTIONS
#define step_oprule_expressions STDCALL STEPOPRULEEXPRESSIONS
#define test_rule_activation STDCALL TESTOPRULEACTIVATION
#define parse_rule PARSE_RULE
#else
#define STDCALL 
#define init_parser_f STDCALL init_parser_f_
#define advance_oprule_actions STDCALL advanceopruleactions_
#define step_oprule_expressions STDCALL stepopruleexpressions_
#define test_rule_activation STDCALL testopruleactivation_
#define parse_rule parse_rule_ 
#endif


ModelInterfaceActionResolver<DSM2Resolver,
                             DSM2ModelInterfaceResolver> resolver;
OperationManager dsm2_op_manager(resolver);

using namespace std;
extern "C"{
    void init_parser_f(){
        lexer_init();
        init_expression();
        init_lookup(new DSM2HydroNamedValueLookup());
        init_model_time_factory(new  DSM2HydroTimeNodeFactory());
    }

    void step_oprule_expressions(double* dt_sec){
        //cout << "Advancing expressions " << *dt_sec << endl;
        dsm2_op_manager.stepExpressions(*dt_sec);
        //return 0.0;
    }

    void advance_oprule_actions(double* dt_sec){
        //cout << "Advancing actions " << *dt_sec << endl;
        dsm2_op_manager.advanceActions(*dt_sec);
        //return true;
    }

    void test_rule_activation(){
        dsm2_op_manager.manageActivation();
        //return true;
    }

    bool parse_rule(char* text, int len){
        string parse_str(text,len);
        set_input_string(parse_str);
        int parseok=op_ruleparse(); // make sure it was a rule??
        if( !(parseok==0) || get_parsed_type() == oprule::parser::PARSE_ERROR)
            return false;
        if (get_parsed_type() == oprule::parser::OP_RULE){
            OperatingRulePtr rule=getOperatingRule();
            dsm2_op_manager.addRule(rule);
        }
        return true;
    }
}


