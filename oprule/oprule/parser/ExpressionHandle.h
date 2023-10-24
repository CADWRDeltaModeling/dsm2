#ifndef oprule_EXPRESSIONHANDLE_H__INCLUDED_
#define oprule_EXPRESSIONHANDLE_H__INCLUDED_

#include<string>


namespace oprule{
namespace parser{

/** Handle Class to facilitate holding named expressions for parsing.
   @todo change this to something safer.*/
class ExpressionHandle {
public:
   /** Copy an ExpressionHandle */
   ExpressionHandle(const ExpressionHandle& s) : name(s.name), boolval(s.boolval), dblval(s.dblval){}
   /** Create a ExpressionHandle that holds a double node*/
   ExpressionHandle(oprule::expression::DoubleNodePtr dval) : dblval(dval),boolval(0){}
   /** Create a ExpressionHandle that holds a bool node*/
   ExpressionHandle(oprule::expression::BoolNodePtr bval) : boolval(bval),dblval(0){}
   /** Create a ExpressionHandle that holds a string*/
   ExpressionHandle(const std::string& name) : name(name), boolval(0),dblval(0){}

   /**value in case this ExpressionHandle is a name.*/
   std::string name;
   /**value in case this ExpressionHandle is a bool.*/
   oprule::expression::BoolNodePtr boolval;
   /**value in case this ExpressionHandle is a double.*/
	oprule::expression::DoubleNodePtr dblval;
};

}}      //namespace

#endif //include guard