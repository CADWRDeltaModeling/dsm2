#ifndef oprule_rule_TRIGGER_H__INCLUDED_
#define oprule_rule_TRIGGER_H__INCLUDED_
#include "boost/shared_ptr.hpp"

namespace oprule {
namespace rule {

/** Trigger activation of an operating rule
 * Trigger that observes the state of the model and signifies the
 * activation of an operating rule
 */
class Trigger
{

public:

   /** Test trigger for activation 
   *@return true if the trigger expression evaluates to true
   *@todo clarify difference between this and "newly triggered"
   */
   virtual bool test()=0;

   /**
   * Inform trigger that the model is taking a step.
   */
   virtual void step(double dt)=0;

};
typedef boost::shared_ptr<Trigger> TriggerPtr;

}}     //namespace
#endif // !include guard