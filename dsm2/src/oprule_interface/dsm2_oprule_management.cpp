#include <string>
#include <iostream>
#include <cstdio>
#include "oprule/expression/ExpressionNode.h"
#include "oprule/parser/NamedValueLookup.h"
#include "oprule/parser/ModelTimeNodeFactory.h"
#include "oprule/parser/ModelActionFactory.h"
#include "oprule/rule/OperatingRule.h"
#include "oprule/rule/OperationManager.h"
#include "oprule/parser/ParseResultType.h"
#include "oprule/rule/ModelInterfaceActionResolver.h"
#include "oprule/rule/RuleLog.h"
#include "oprule/parser/ParseSymbolManagement.h"

#include "dsm2_named_value_lookup.h"
#include "dsm2_time_node_factory.h"
#include "dsm2_time_interface.h"
#include "dsm2_time_interface_fortran.h"
#include "dsm2_interface_fortran.h"
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

namespace {
const char* const DEFAULT_LOG_FILE = "oprule_log.txt";
bool g_log_configured = false;   // the log is set up once, when the first rule is parsed
bool g_run_started = false;      // set by the first advance; before that there is no model time

// Model time at the end of the current step, as the log's time label.
std::string model_time_label(){
    if (!g_run_started) return "init";
    char buf[32];
    std::snprintf(buf, sizeof(buf), "%04d-%02d-%02d %02d:%02d",
                  get_model_year(), get_model_month(), get_model_day(),
                  get_model_hour(), get_model_minute());
    return buf;
}

// Name before ":=" in the text of a rule or named expression.
std::string name_before_assignment(const std::string& text){
    std::string::size_type pos = text.find(":=");
    std::string name = text.substr(0, pos);
    std::string::size_type b = name.find_first_not_of(" \t");
    std::string::size_type e = name.find_last_not_of(" \t");
    return b == std::string::npos ? std::string() : name.substr(b, e - b + 1);
}
}

// Reads the level from the model and opens the log file when it is above 0.
void configure_oprule_log_from_model(const std::string& path){
    RuleLog::setTimeSource(&model_time_label);
    int level = get_oprule_log_level();
    if (level <= 0) { RuleLog::setLevel(RuleLog::OFF); return; }
    if (!RuleLog::open(path)){
        cerr << "Warning: could not open the oprule log file " << path << "; oprule logging is off." << endl;
        RuleLog::setLevel(RuleLog::OFF);
        return;
    }
    RuleLog::setLevel(level);
}

using namespace std;
extern "C"{
    void init_parser_f(){
        g_log_configured = false;
        g_run_started = false;
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
        g_run_started = true;
        dsm2_op_manager.advanceActions(*dt_sec);
        //return true;
    }

    void test_rule_activation(){
        dsm2_op_manager.manageActivation();
        //return true;
    }

    bool parse_rule(char* text, int len){
        // a sink installed by the caller (tests) is left alone
        if (!g_log_configured){
            g_log_configured = true;
            if (!RuleLog::hasSink()) configure_oprule_log_from_model(DEFAULT_LOG_FILE);
        }
        string parse_str(text,len);
        set_input_string(parse_str);
        int parseok=op_ruleparse(); // make sure it was a rule??
        if( !(parseok==0) || get_parsed_type() == oprule::parser::PARSE_ERROR)
            return false;
        if (get_parsed_type() == oprule::parser::OP_RULE){
            OperatingRulePtr rule=getOperatingRule();
            dsm2_op_manager.addRule(rule);
            RuleLog::write(RuleLog::EVENTS, "RULE_LOADED", rule->getName(), "");
        }else{
            RuleLog::write(RuleLog::EVENTS, "EXPRESSION_LOADED", name_before_assignment(parse_str), "");
        }
        return true;
    }
}


