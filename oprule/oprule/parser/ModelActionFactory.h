#ifndef oprule_parser_MODELACTIONFACTORY_H__INCLUDED_
#define oprule_parser_MODELACTIONFACTORY_H__INCLUDED_

#include "oprule/rule/ModelAction.h"
#include "oprule/rule/Transition.h"
#include "oprule/rule/ModelInterface.h"



namespace oprule{
namespace parser{

/** Factory method to provide model actions.
  *@todo is this needed? main thing it does is finesse the model timer,
  * which is a nuisance as a template parameter
  */
class ModelActionFactory
{
public:
   ModelActionFactory(){};
   virtual ~ModelActionFactory(){};
   virtual oprule::rule::OperationAction* createModelAction(
      oprule::rule::ModelInterface<double>::NodePtr modelIF,
      oprule::expression::DoubleNodePtr express,
      const oprule::rule::Transition& transition)=0;
};

}} //namespace
#endif // !include guard
