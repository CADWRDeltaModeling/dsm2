#ifndef oprule_parser_PARSERESULTTYPE_H__INCLUDED_
#define oprule_parser_PARSERESULTTYPE_H__INCLUDED_

namespace oprule{
namespace parser{

/**Possible results of parsing a string in the oprule parser*/
enum parse_type{
  BOOL_EXPRESS=0,
  NUMERICAL_EXPRESS=1,
  OP_RULE=2,
  REASSIGNMENT=3,
  PARSE_ERROR=4
};


}}     //namespace
#endif //include guard













