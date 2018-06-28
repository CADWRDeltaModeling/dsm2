#ifndef oprule_rule_OPERATINGRULE_H__INCLUDED_
#define oprule_rule_OPERATINGRULE_H__INCLUDED_

#include "OperationAction.h"
#include "Trigger.h"
#include<iostream> //debug
#include<string> //debug

namespace oprule {
namespace rule {


/** Complete operating rule (action with trigger/applicability info)
 *  Operating rule 
 */
class OperatingRule
{

public:
   /**
    Create an OperatingRule given an action. Trigger will be trivial.
   */
	OperatingRule(OperationActionPtr opact);

   /**
    * Create an OperatingRule given an action and a trigger.
    */
	OperatingRule(OperationActionPtr opact, TriggerPtr trigger);

   /**
    * Assign a name to this operating rule
    */
   void setName(std::string name){_name=name;}

   /**
    * Get the name of this rule as a string
    */
   std::string getName(){return _name;}


   /**
    * Set this rule active or inactive according to the argument
    */ 
   void setActive(bool active){ 
	   _action->setActive(active); 

   }

   /**
    * Query if this rule is active
    */
   bool isActive(){ return _action->isActive();}


   /**
    * Tests rule for applicability in current model context
    * @todo delegate to action?
    */
   bool isActionApplicable();

   /**
    * Advances the action by a step
    * @todo delegate to action?
    */
	void advanceAction(double dt);

   /**
   *  Tests the trigger for current value
   */
   bool testTrigger();

   /**
    * Performs updates required when the model advances a ste
	*/
   void step(double dt);

   /**
   * Tests whether this trigger was activated since the last
   * time testNewlyTriggered was called. This function should
   * not be called more than once per time step, because it
   * has the side effect of replacing the record of the previous
   * trigger value. 
   */
   bool testNewlyTriggered();

   /** Prevents activation of the rule. 
   * This method stops activation of the rule, but also leaves the rule
   * in a state that is ready for activation (namely, the trigger is
   * tricked into thinking that it returns false). This method is
   * useful when the policy for conflicting rules is to defer the newly triggered
   * rule
   */
   void deferActivation();

   /**
   * Gets an exhaustive, but not particularly well organized, list
   * of all ModelActions contained in this operating rule (which may be
   * more than one action because of compound actions).
   */
   virtual OperationAction::ActionListType& getActionList();   
	
   /** Virtual destructor */
   virtual ~OperatingRule();

private:
   OperationActionPtr _action;
   OperationAction::ActionListType _actionList;
   TriggerPtr _trigger;
   bool _prevTriggerValue;   // todo: move this to trigger
   std::string _name;
};

typedef boost::shared_ptr<OperatingRule> OperatingRulePtr;

}}     //namespace
#endif // !include guard
