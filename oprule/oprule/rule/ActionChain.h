#ifndef oprule_rule_ACTIONCHAIN_H__INCLUDED_
#define oprule_rule_ACTIONCHAIN_H__INCLUDED_

#include "OperationAction.h"
#include "ActionSet.h"
#include <deque>



namespace oprule {
namespace rule {

/** Collection comprised of consecutive sub-actions to be executed in series.
 * As each subaction ends, the next is activated. The subactions
 * contained in the chain may themselves be compound actions
 * (collections such as ActionChain or ActionSet) or may be actions that
 * directly manipulate the model.
 */

class ActionChain : public OperationAction
{

public:
	ActionChain();
    virtual ~ActionChain();

   /** sequence type used to store subactions*/
   typedef std::deque<OperationActionPtr> ActionSequence;

   /** Add action to end of chain.
    * @param action action to be added
    */
	void pushBackAction(OperationActionPtr action);

   /** Add action to front of chain.
    * @param action action to be added
    */
   void pushFrontAction(OperationActionPtr action);

   /** Get the action in chain that is currently active.
   */
	OperationActionPtr getCurrentAction();

   /** Advance the chain in time.
    * Will advance the chain in time. This may cause one of the serial
    * actions to complete and the following one to activate.
    */
   void advance(double dt);

   void step(double dt);


   /** Query if action is active
    * @return true if the action is in progress
    */
	bool isActive();

   /** Action to
    *
    */
	void onActivate();
    void setActive(bool active);

   virtual bool hasSubActions(){
      return true;
   }
   virtual void appendToActionList( OperationAction::ActionListType& listToConstruct);
   virtual void childComplete();

private:
   ActionChain::ActionSequence actionSequence;
   ActionChain::ActionSequence::iterator actionIterator;
   bool _active;
};

}}     //namespace
#endif // include guard