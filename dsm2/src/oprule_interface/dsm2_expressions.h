
#ifndef dsm2_expressions_h
#define dsm2_expressions_h
#include "oprule/expression/ExpressionNode.h"

#define STDCALL
extern "C" double STDCALL get_expression_data(int* express);

int register_express_for_data_source(
     oprule::expression::DoubleNodePtr expr);
#endif