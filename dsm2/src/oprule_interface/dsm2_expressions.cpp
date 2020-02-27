#include "dsm2_expressions.h"
#include "oprule/expression/ExpressionNode.h"
#include<vector>
#include<algorithm>
#ifdef _WIN32
#define get_expression_data GET_EXPRESSION_DATA
#else
#define get_expression_data get_expression_data_
#endif

typedef std::vector<oprule::expression::DoubleNodePtr> data_expr_container;
data_expr_container expr_used_as_data;
typedef data_expr_container::iterator data_expr_iter;

extern "C" double STDCALL get_expression_data(int* express){
   return (expr_used_as_data[*express])->eval(); 
}

int register_express_for_data_source(
   oprule::expression::DoubleNode::NodePtr expr){
   data_expr_iter it=std::find(expr_used_as_data.begin(),
                          expr_used_as_data.end(),expr);
   if ( it != expr_used_as_data.end()){
      return (int)(it - expr_used_as_data.begin()); //@todo: is this safe?
   }else{
      expr_used_as_data.push_back(expr);
      return ((int)expr_used_as_data.size())-1;     //@todo: is this safe?
   }
}
