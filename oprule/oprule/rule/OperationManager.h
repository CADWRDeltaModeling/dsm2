#ifndef oprule_rule_OPERATIONMANAGER_H__INCLUDED_
#define oprule_rule_OPERATIONMANAGER_H__INCLUDED_

#include <list>
#include "OperationAction.h"
#include "OperatingRule.h"
#include "ActionResolver.h"


namespace oprule {
namespace rule {

/** Controller for operating rules.
 * Controller for testing, activating and deactivating operating rules
 */
class OperationManager
{

public:
   /**
   * Create operation manager.
   * @param resolver Used by this manager to identify action conflicts.
   */
   OperationManager(ActionResolver& resolver);

   /** Virtual destructor*/
   virtual ~OperationManager(){};

   /** Type of collection used to keep pools of inactive and active rules*/
   typedef std::list<OperatingRulePtr> OpPool ;

   /** Add a rule to be managed.
    * @param rule the new rule
    */
	virtual void addRule(OperatingRulePtr rule);

   /** Handle activation of inactive rules.
    * Tests inactive rules for activation by testing their triggers.
    * If the triggers test true, the inactive rules are compared to
    * already active operating rules and conflicts are handled.
    */
   virtual void manageActivation();

   /** Inform expressions of the completion of a time step.
   */
   virtual void stepExpressions(double dt);


   /** Request advance of active rules.
   * All active rules are advanced to the current time step. Rules that
   * finish become inactive.
   */
   virtual void advanceActions(double dt);

   /** Compare newly activated rule to an old one for priority.
    * Evaluates a new and old rule and determines their
    * compatibility/priority. Calls actionsOverlap.
    * @return member of the enum RulePriority showing outcome
    */
   virtual ActionResolver::RulePriority checkActionPriority(
		OperatingRule& oldRule,	OperatingRule& newRule);

   /** Compare newly activated rule to an old one for overlapping actions.
    * Evaluates a new and old rule and determines their
    * compatibility/priority.
    * @return boolean indicating whether rules overlap
    */
   virtual bool actionsOverlap(
	   OperatingRule& oldRule, OperatingRule& newRule);


   /** Query whether rule is in the active pool
   * @return true if the rule is in the active pool
   */
   virtual bool isActive(OperatingRulePtr);


private:

   OpPool pool;
   ActionResolver& _resolver;
};

}}     //namespace
#endif // include guard