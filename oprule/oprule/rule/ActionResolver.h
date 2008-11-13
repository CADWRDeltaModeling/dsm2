#ifndef oprule_rule_ACTIONRESOLVER_H__INCLUDED_
#define oprule_rule_ACTIONRESOLVER_H__INCLUDED_

#include "oprule/rule/OperationAction.h"
#include "oprule/rule/ModelInterface.h"

#pragma warning (disable:4786)   // identifier truncated to '255' characters

namespace oprule {
namespace rule {

/** Discovers actions that possibly conflict.
 *  The meaning of a <i>conflict</i> is application specific. The best way to
 *  resolve differences is with double dispatch -- developers may want to use
 *  the class ModelInterfaceActionResolver as a template with double dispatch 
 *  using Loki.
 *  @todo name? -- discovers conflicts resolve them.
 */
class ActionResolver{
public:

/** 
  *Enumeration of responses when an old rule and new rule are compared.
  */
	enum RulePriority {
        RULES_COMPATIBLE, // Two actions are mutually compatible
		REPLACE_OLD_RULE, // New rule supercedes the old one
        RECONCILE_RULES,
		IGNORE_NEW_RULE,  // Old rule blocks the new one
        DEFER_NEW_RULE    // Old rule blocks the new one, but the new one "forgets" that its
                        // trigger status changed
	};

   /**
    * Tests whether two actions overlap, which generally means they will
    * affect the same controllable parameter or affect overlapping subdomains.
    * A challenge here is that both inputs are generic OperationActions.
    * This means virtual function mechanisms will not work right --
    * see any discussion of the double dispatch problem.
    * @param act1 first action to compare
    * @param act2 second action to compare
    * @return true if the two actions overlap (exact meaning is left to the implementation)
    */
   virtual bool overlap(OperationAction& act1,
                        OperationAction& act2)=0;
   
   /**
    * Provides a policy for resolving situations when two rules conflict.
    */
   virtual RulePriority resolve()=0;

   virtual ~ActionResolver(){};
};


}}     //namespace
#endif //include guard