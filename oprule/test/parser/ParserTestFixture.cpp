#include "ParserTestFixture.h"
#include "oprule/parser/ModelNameParseError.h"

using namespace oprule::parser;
using namespace oprule::rule;
using namespace oprule::expression;

double TestModelInterface::val=-17;
double TestModelInterface2::val=-18;
double TestModelInterface3::val=-19;


void resetInterfaces(){
   TestModelInterface::val=-17;
   TestModelInterface2::val=-18;
   TestModelInterface3::val=-19;
}


oprule::expression::DoubleNodePtr
expression_factory(const NamedValueLookup::ArgMap &argmap){
  NamedValueLookup::ArgMap::const_iterator iter=argmap.find("modelname");
  if(iter==argmap.end())throw MissingIdentifier("No name");
  assert(iter->second=="test_state");
  iter=argmap.find("first_arg");
  if (iter == argmap.end())throw oprule::parser::MissingIdentifier("first_arg not provided");
  bool valid=iter->second=="4";
  iter=argmap.find("second_arg");
  valid=valid && iter->second == "quoted string";
  if (valid) {
	  return oprule::expression::DoubleScalarNode::create(2.);
  }else{
    throw InvalidIdentifier("Wrong arguments");
  }
}

oprule::expression::DoubleNode::NodePtr
lookup_factory(const NamedValueLookup::ArgMap &argmap){
	return oprule::expression::DoubleScalarNode::create(2.);
}


oprule::expression::DoubleNode::NodePtr
interface_factory(const NamedValueLookup::ArgMap &argmap){
  NamedValueLookup::ArgMap::const_iterator iter=argmap.find("modelname");
  if(iter==argmap.end())throw MissingIdentifier("No name");
  assert(iter->second=="test_interface");
  iter=argmap.find("first_arg");
  bool valid=iter->second=="quoted string with spaces";
  iter=argmap.find("second_arg");
  valid=valid && iter->second == "0";
  iter=argmap.find("third_arg");
  valid=valid && iter->second == "unquoted";
  if (valid) {
    return TestModelInterface::create();;
  }else{
    throw InvalidIdentifier("Wrong arguments");
  }
}

oprule::expression::DoubleNode::NodePtr
interface2_factory(const NamedValueLookup::ArgMap &argmap){
  NamedValueLookup::ArgMap::const_iterator iter=argmap.find("modelname");
  if(iter==argmap.end())throw MissingIdentifier("No name");
  assert(iter->second=="second_interface");
  iter=argmap.find("first_arg");
  bool valid=iter->second=="0";
  if (valid) {
    return TestModelInterface2::create();;
  }else{
    throw InvalidIdentifier("Wrong arguments");
  }
}

oprule::expression::DoubleNode::NodePtr
interface3_factory(const NamedValueLookup::ArgMap &argmap){
  NamedValueLookup::ArgMap::const_iterator iter=argmap.find("modelname");
  if(iter==argmap.end())throw MissingIdentifier("No name");
  assert(iter->second=="third_interface");
  iter=argmap.find("first_arg");
  bool valid=iter->second=="0";
  if (valid) {
    return TestModelInterface3::create();;
  }else{
    throw InvalidIdentifier("Wrong arguments");
  }
}