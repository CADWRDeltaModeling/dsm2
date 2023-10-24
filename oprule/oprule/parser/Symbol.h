#ifndef oprule_SYMBOL_H__INCLUDED_
#define oprule_SYMBOL_H__INCLUDED_

#include<assert.h>
#include<string>
#include "oprule/expression/ExpressionNode.h"
#include "oprule/rule/Trigger.h"
#include "oprule/rule/ModelAction.h"
#include "oprule/rule/OperatingRule.h"
#include <iostream>
namespace oprule{
   namespace parser{

/** Class to facilitate having a single type of named symbols. Basically this is
    just a union, and it isn't very safe.
   @todo change this to something safer.*/
class symbol {
   typedef oprule::expression::BoolNodePtr  BoolNodePtr;
   typedef oprule::expression::DoubleNodePtr DoubleNodePtr;
   typedef oprule::rule::OperatingRulePtr OperatingRulePtr;
   typedef oprule::rule::OperationActionPtr OperationActionPtr;
   typedef oprule::rule::TransitionPtr TransitionPtr;
   typedef oprule::rule::TriggerPtr TriggerPtr;
public:
   enum type_id{ EMPTY,DOUBLE, BOOL, STRING, ACTION, RULE, TRANSITION, TRIGGER };
   type_id _t;

   /** Create a symbol with no value */
   symbol(): _t(EMPTY){}
   /** Create a symbol that holds a double node*/
   symbol(oprule::expression::DoubleNodePtr dval) : dblval(dval), _t(DOUBLE){}
   /** Create a symbol that holds a bool node*/
   symbol(oprule::expression::BoolNodePtr bval) : boolval(bval), _t(BOOL){}
   /** Create a symbol that holds a string*/
   symbol(const std::string& stringval) : stringval(stringval), _t(STRING){}

   symbol(oprule::rule::OperatingRulePtr& oprule) : rule(oprule), _t(RULE){}

   symbol(oprule::rule::OperationActionPtr& act) : action(act),_t(ACTION){}

   symbol(oprule::rule::TransitionPtr& trans) : transition(trans),_t(TRANSITION){}

   symbol(oprule::rule::TriggerPtr& trig) : trigger(trig),_t(TRIGGER){}

   symbol& operator=(const symbol& rhs){
      if (&rhs != this){
         _t = rhs._t;
         dblval=rhs.dblval;
         boolval=rhs.boolval;
         stringval=rhs.stringval;
         action = rhs.action;
         rule = rhs.rule;
         transition = rhs.transition;
         trigger = rhs.trigger;
      }
      return *this;
   }

   type_id type(){return _t;}

   /**value in case this symbol is a name.*/
   std::string stringval;
   /**value in case this symbol is a bool.*/
   BoolNodePtr boolval;
   /**value in case this symbol is a double.*/
   DoubleNodePtr dblval;
   OperatingRulePtr rule;
   OperationActionPtr action;
   TransitionPtr transition;
   TriggerPtr trigger;

   void erase(){
      OE_NODE_DELETE(boolval);
      OE_NODE_DELETE(dblval);
      OE_NODE_DELETE(rule);
      OE_NODE_DELETE(action);
   }

   /** Copy a symbol */
   symbol(const symbol& s) : _t(s._t),
                             stringval(s.stringval),
                             boolval(s.boolval),
                             dblval(s.dblval),
                             rule(s.rule),
                             action(s.action),
                             transition(s.transition),
                             trigger(s.trigger){}


};


}}// namespace oprule


#endif