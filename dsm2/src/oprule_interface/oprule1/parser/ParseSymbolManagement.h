// The following pragmas disable stupid warnings in MSVC 6
#ifndef PARSESYMBOLMANAGEMENT_H__
#define PARSESYMBOLMANAGEMENT_H__

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
#include "oprule/parser/ParseResultType.h"
#include "oprule/parser/ptr_wrapper.h"


using namespace std;
using namespace oprule::rule;
using namespace oprule::parser;
using namespace oprule::expression;

#define EVAL "eval__"
const string EVAL_STR=string(EVAL);

#define yylval op_rulelval

extern void lexer_init(); 


typedef map<string, symbol > express_map;
typedef map<string, symbol >::iterator expressIterType;
typedef pair<string, symbol> symbol_pair;
//typedef boost::shared_ptr<OperatingRule> OperatingRulePtr;
typedef map<string, oprule::rule::OperatingRulePtr> rule_map;
typedef map<pair<string, int>, double> initial_value_map;
typedef pair<string, int> initializer_pair;

typedef pair<string, OE_NODE_PTR(LaggedExpressionNode<double> >) lagged_expression_pair;
typedef vector< lagged_expression_pair >lagged_expression_list;

vector<symbol> & get_express_index();

#define _EI express_index
typedef vector<symbol >::iterator indexIterType;

void clear_temp_expr();
void clear_arg_map();
void clear_string_list();

void init_lookup( NamedValueLookup*);
void init_model_time_factory(ModelTimeNodeFactory * );
void init_expression();
void init_rule_names();
void apply_lagged_values(const string&name, DoubleNodePtr expression);

NamedValueLookup          * get_lookup();
ModelTimeNodeFactory      * get_time_factory();

void set_parsed_type(oprule::parser::parse_type);
oprule::parser::parse_type get_parsed_type();
bool add_symbol(const string & dbkey, symbol& expression);
bool add_rule(const string& name, OperatingRulePtr rule);
void add_expression(const std::string&, DoubleNodePtr);

/** Get a real-valued expression stored by its name */
DoubleNodePtr getDoubleExpression(const char *name = 0);
/** Get a boolean valued expression stored by its name */
BoolNodePtr getBoolExpression(const char * name = 0);

OperatingRulePtr getOperatingRule(const string & name=EVAL_STR);

OperationActionPtr chain_actions( OperationActionPtr op1, OperationActionPtr op2);
OperationActionPtr group_actions( OperationActionPtr op1, OperationActionPtr op2);


symbol make_symbol(DoubleNodePtr dnode);
symbol make_symbol(BoolNodePtr bnode);
symbol name_lookup(const string& name);




int op_ruleerror(const char* s);
int month_to_int(const string&);
extern int op_rulelex();



express_map               & get_express_map();
rule_map                  & get_rule_map();
lagged_expression_list    & get_lagged_vals();
initial_value_map         & get_init_cond();
NamedValueLookup::ArgList & get_arg_list();
NamedValueLookup::ArgMap  & get_arg_map();
int                         add_to_string_list(const string& a_string);

/** Register a temporary symbol and return an index to it*/
template<typename T>
int add_temp_symbol(T item){
   symbol s(item);
   get_express_index().push_back(s);
   return get_express_index().size() - 1;
}


symbol & get_temp_symbol(int index);

int add_array_vector();
vector<double>& get_array_vector(int index);
void clear_array_vectors();


// String manipulation function that converts an object to 
// string using the relevant operator<< function.
template<class T>
string ToString(const T& val){
   std::stringstream stream;
   stream << val;
   string out = stream.str();
   return out;
}

// Convert a string to an integer, even if it has
// leading zeros
long padded_string_to_int(const string& str);


#endif  //include guard
