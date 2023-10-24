#ifndef oprule_rule_ACTIONSET_H__INCLUDED_
#define oprule_rule_ACTIONSET_H__INCLUDED_

#include "OperationAction.h"
#include <vector>

namespace oprule {
namespace rule {

/** Group of actions executed together
 * A collection of OperationActions executed in parallel. Actions
 * contained in the collection may themselves be compound actions
 * (collections) or may directly manipulate the model.
 */
class ActionSet : public OperationAction
{
public:
    ActionSet();
    virtual ~ActionSet();
    /** Type used to list subactions*/
    typedef std::vector<OperationActionPtr> ActionList;

    /** Add an action to the set
    * @param operationAction action to append to set.
    */
    void addAction(OperationActionPtr operationAction);
    void advance(double dt);
    void step(double dt);

    void setActive(bool active);
    bool isActive();
    virtual bool hasSubActions(){ return true; }
    virtual void childComplete();
    virtual void appendSubActionsToList( OperationAction::ActionListType& listToConstruct);


private:
    ActionList subactions;
    bool _active;
};

}}     //namespace
#endif // include guard