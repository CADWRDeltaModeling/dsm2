#ifndef DSM2_MODEL_ACTION_FACTORY_H_5463F__INCLUDED_
#define DSM2_MODEL_ACTION_FACTORY_H_5463F__INCLUDED_
#include "oprule/parser/ModelActionFactory.h"
#include "oprule/expression/ExpressionNode.h"
#include "dsm2_time_interface.h"

class dsm2_model_action_factory : public oprule::parser::ModelActionFactory{
public:
 
   dsm2_model_action_factory(){}
   virtual ~dsm2_model_action_factory(){}

   enum { MIN=0, HOUR=1 };

   oprule::rule::OperationAction* createModelAction(
      oprule::rule::ModelInterface<double>::NodePtr modelIF,
         oprule::expression::DoubleNodePtr express, 
         int rampingLength, 
         int rampingUnit){
      return new oprule::rule::ModelAction<DSM2ModelTimer, double >(
         _timer, modelIF, express, _timer.minutes(rampingLength));

   }

private:
   DSM2ModelTimer _timer;
};

#endif // !include guard