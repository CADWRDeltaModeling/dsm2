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
#include "dsm2_device_sampler.h"
#ifdef OPRULE_WITH_HDF5
#include "oprule/rule/Hdf5LogSink.h"
#endif

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
#define finish_oprule_log_f STDCALL FINISH_OPRULE_LOG_F
#define advance_oprule_actions STDCALL ADVANCEOPRULEACTIONS
#define step_oprule_expressions STDCALL STEPOPRULEEXPRESSIONS
#define test_rule_activation STDCALL TESTOPRULEACTIVATION
#define parse_rule PARSE_RULE
#else
#define STDCALL
#define init_parser_f STDCALL init_parser_f_
#define finish_oprule_log_f STDCALL finish_oprule_log_f_
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
bool g_sample_devices = true;    // oprule_log_devices
long g_flush_minutes = 24 * 60;  // oprule_log_flush_hours
long g_last_flush = 0;
DeviceSampler g_sampler;

// Model time at the end of the current step, as the log's time label.
std::string model_time_label(){
    if (!g_run_started) return "init";
    char buf[32];
    std::snprintf(buf, sizeof(buf), "%04d-%02d-%02d %02d:%02d",
                  get_model_year(), get_model_month(), get_model_day(),
                  get_model_hour(), get_model_minute());
    return buf;
}

// Model time in julian minutes (0 before the first advance).
long model_julmin(){
    return g_run_started ? static_cast<long>(get_model_ticks()) : 0L;
}

// Rule text on one line with runs of white space collapsed, for the load records.
std::string one_line(const std::string& text){
    std::string out;
    bool space = false;
    for (std::string::size_type i = 0; i < text.size(); ++i){
        char c = text[i];
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r'){ space = !out.empty(); continue; }
        if (space){ out += ' '; space = false; }
        out += c;
    }
    return out;
}

// Name before ":=" in the text of a rule or named expression.
std::string name_before_assignment(const std::string& text){
    std::string::size_type pos = text.find(":=");
    std::string name = text.substr(0, pos);
    std::string::size_type b = name.find_first_not_of(" \t");
    std::string::size_type e = name.find_last_not_of(" \t");
    return b == std::string::npos ? std::string() : name.substr(b, e - b + 1);
}

#ifdef OPRULE_WITH_HDF5
Hdf5LogSink* g_hdf5 = NULL;

// The oprule_log_file scalar (a bare name goes next to the tide file), or <tide file name without its
// extension>_oprule_log.h5 next to the tide file; oprule_log.h5 in the working directory if there is no tide file.
std::string hdf5_log_path(){
    char buf[512];
    get_hydro_tidefile_name(buf, 512);
    const std::string tide = buf;
    get_oprule_log_file(buf, 512);
    const std::string name = buf;
    if (!name.empty() && name.find_first_of("/\\") != std::string::npos) return name;
    const std::string::size_type slash = tide.find_last_of("/\\");
    const std::string dir = slash == std::string::npos ? std::string() : tide.substr(0, slash + 1);
    if (!name.empty()) return dir + name;
    if (tide.empty()) return "oprule_log.h5";
    std::string stem = slash == std::string::npos ? tide : tide.substr(slash + 1);
    const std::string::size_type dot = stem.find_last_of('.');
    if (dot != std::string::npos) stem = stem.substr(0, dot);
    return dir + stem + "_oprule_log.h5";
}
#endif
}

// Reads the level from the model and opens the log file when it is above 0.
void configure_oprule_log_from_model(const std::string& path){
#ifdef OPRULE_WITH_HDF5
    if (g_hdf5){ RuleLog::removeSink(g_hdf5); delete g_hdf5; g_hdf5 = NULL; }
#endif
    RuleLog::setTimeSource(&model_time_label);
    RuleLog::setClock(&model_julmin);
    g_sample_devices = get_oprule_log_devices() != 0;
    g_flush_minutes = static_cast<long>(get_oprule_log_flush_hours() * 60.0);
    g_sampler.configure(get_oprule_log_tol_op(), get_oprule_log_tol_dim(), get_oprule_log_context() != 0);
    int level = get_oprule_log_level();
    if (level <= 0) { RuleLog::setLevel(RuleLog::OFF); return; }
    bool haveSink = false;
    if (get_oprule_log_text() != 0){
        if (!RuleLog::open(path)){
            cerr << "Warning: could not open the oprule log file " << path << "; oprule logging is off." << endl;
            RuleLog::setLevel(RuleLog::OFF);
            return;
        }
        haveSink = true;
    }
#ifdef OPRULE_WITH_HDF5
    g_hdf5 = new Hdf5LogSink(hdf5_log_path(), level);
    if (g_hdf5->ok()){
        RuleLog::addSink(g_hdf5);
        haveSink = true;
    }else{
        cerr << "Warning: could not create the oprule HDF5 log " << g_hdf5->path() << endl;
        delete g_hdf5;
        g_hdf5 = NULL;
    }
#endif
    if (!haveSink){
        cerr << "Warning: no oprule log could be created; oprule logging is off." << endl;
        RuleLog::setLevel(RuleLog::OFF);
        return;
    }
    RuleLog::setLevel(level);
    if (get_oprule_log_trace_interval() > 0)
        cerr << "Note: oprule_log_trace_interval is read but the dense trace is not written yet." << endl;
}

using namespace std;
extern "C"{
    void init_parser_f(){
        g_log_configured = false;
        g_run_started = false;
        g_last_flush = 0;
        g_sampler.reset();
        RuleLog::reset();
        lexer_init();
        init_expression();
        init_lookup(new DSM2HydroNamedValueLookup());
        init_model_time_factory(new  DSM2HydroTimeNodeFactory());
    }

    // End of the run: closes the open intervals and flushes the log files.
    void finish_oprule_log_f(){
        RuleLog::finish();
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
        if (g_sample_devices) g_sampler.sample();
        const long now = model_julmin();
        if (now - g_last_flush >= g_flush_minutes){
            g_last_flush = now;
            RuleLog::flush();
        }
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
            RuleLog::loaded(rule->getName(), false, one_line(parse_str));
        }else{
            RuleLog::loaded(name_before_assignment(parse_str), true, one_line(parse_str));
        }
        return true;
    }
}


