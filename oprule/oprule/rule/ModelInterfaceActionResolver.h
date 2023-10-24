#ifndef oprule_rule_MODELITERFACEACTIONRESOLVER_H__INCLUDED_
#define oprule_rule_MODELITERFACEACTIONRESOLVER_H__INCLUDED_
#include "oprule/rule/ModelInterface.h"
#include "oprule/rule/ActionResolver.h"


namespace oprule {
namespace rule {



/** ActionResolver implementation using double dispatch using Loki.
 *  The actual dispatcher has to be created for particular models, since
 *  the concept of a conflict between actions is application specific.
 *  The dispatcher needs the Actions to be of a common base class.
 */
template<class InterfaceDispatcher,class InterfaceResolver >
class ModelInterfaceActionResolver : public ActionResolver{
public:
   virtual bool overlap(OperationAction &act1,
                        OperationAction& act2){
      return InterfaceDispatcher::Go(
        (*(static_cast<ModelAction<double>* >(&act1))->getModelInterface()),
        (*(static_cast<ModelAction<double>* >(&act2))->getModelInterface()),
        resolver);
   }

   virtual ActionResolver::RulePriority resolve(){
      return ActionResolver::DEFER_NEW_RULE;
   }
private:
   InterfaceResolver resolver;
};


}}
#endif