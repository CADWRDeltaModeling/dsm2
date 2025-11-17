#include <string>
#include "oprule/parser/ParseSymbolManagement.h"

#include "dsm2_hydrogtm_named_value_lookup.h"
#include "dsm2_time_node_factory.h"

using namespace std;

extern void lexer_init();
extern void set_input_string(string &);
extern void set_input_string(char*);
extern int op_ruleparse();

extern "C" void init_parser_hydrogtm() {
    lexer_init();
    init_expression();
    init_lookup(new DSM2HydrogtmNamedValueLookup());
    init_model_time_factory(new DSM2HydroTimeNodeFactory());
}