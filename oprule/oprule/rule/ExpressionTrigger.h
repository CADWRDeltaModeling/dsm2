#ifndef oprule_rule_EXPRESSIONTRIGGER_H__INCLUDED_
#define oprule_rule_EXPRESSIONTRIGGER_H__INCLUDED_

#include "oprule/rule/Trigger.h"
#include "oprule/expression/ExpressionNode.h"
#include "oprule/expression/ValueNode.h"

namespace oprule {
namespace rule {


/** Trigger that is based on the evaluation of a boolean expression. */
class ExpressionTrigger : public Trigger
{
public:
   ExpressionTrigger(oprule::expression::BoolNodePtr expression)
         : _expression(expression){}

   virtual ~ExpressionTrigger(){
      OE_NODE_DELETE( _expression );
   }
   virtual bool test(){
      return _expression->eval();
   }
   virtual void step(double dt){
	   _expression->step(dt);
   }

private:
   oprule::expression::BoolNodePtr _expression;
};

}}     //namespace
#endif // !include guard

