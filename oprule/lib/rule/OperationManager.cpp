#include "oprule/rule/OperationManager.h"
#include "oprule/rule/ModelAction.h"
#include "oprule/rule/RuleLog.h"
#include "assert.h"
#include<algorithm>
#include<iostream>  // debug
#include<string>   //debug
using namespace std;
using namespace oprule::rule;


OperationManager::OperationManager(ActionResolver & resolver)
    : _resolver(resolver){}

void OperationManager::addRule(OperatingRulePtr rule){
	rule->setActive(false);
    this->pool.push_back(rule);
}


ActionResolver::RulePriority
OperationManager::checkActionPriority(
	  OperatingRule& oldRule, OperatingRule& newRule){
     if  (actionsOverlap(oldRule,newRule)){
        return ActionResolver::DEFER_NEW_RULE;
     }else{
        return ActionResolver::RULES_COMPATIBLE;
     }
}





bool OperationManager::isActive(OperatingRulePtr rule){
   return rule->isActive();
}

void OperationManager::manageActivation(){
 //test if rules were triggered
   for( OpPool::iterator it=pool.begin() ;
        it != pool.end() ;
        it++){
	   OperatingRulePtr rulePtr=(*it);
       if (rulePtr->isActive())continue;
       //cout << rulePtr->getName() << " start test trigger" <<endl;
	   bool newlyTriggered = rulePtr->testNewlyTriggered();
       // Only changes of the trigger value are logged, each with the inputs that produced it. The
       // value was computed by the test above; logging must never test the trigger again.
       OperatingRule::TriggerChange change = rulePtr->getTriggerChange();
       if (change != OperatingRule::NO_CHANGE && RuleLog::enabled(RuleLog::EVENTS)){
          const int event = change == OperatingRule::RISING ? LogEvent::TRIGGERED :
                            change == OperatingRule::FALLING ? LogEvent::TRIGGER_CLEARED : LogEvent::TRIGGER_INITIAL;
          LogValues inputs;
          const bool ok = rulePtr->describeTrigger(inputs);
          RuleLog::event(event, rulePtr->getName(), "", &inputs, !ok);
       }
       if (change == OperatingRule::FALLING && _deferredLogged.erase(rulePtr.get())){
          RuleLog::event(LogEvent::DEFER_ENDED, rulePtr->getName(), "");
       }
	   if (newlyTriggered){
           //cout << rulePtr->getName()<< " triggered" << endl;
		  // rule is triggered, test whether action is valid in context
		   bool applicable = rulePtr->isActionApplicable();
          if (!applicable){
             RuleLog::event(LogEvent::NOT_APPLICABLE, rulePtr->getName(), "");
          }
		   if ( applicable ){
            //cout << "Attempting to activate " << rulePtr->getName()<<endl;
            // Check whether other rules will block it;
            // Nothing is changed during check, because if any rule blocks this
            // one, we want to guarantee that nothing will happen except
            // deferring the rule.
			bool useNewRule=true;
            bool isConflicting=false;
            for( OpPool::iterator activeiter = pool.begin() ;
                                 activeiter != pool.end() ;
                                 ++activeiter){
                OperatingRulePtr activePtr = (*activeiter);
                //cout << "testing against rule :" << activePtr->getName()<<endl;

                if ((!activePtr->isActive()) || rulePtr == activePtr)continue;
                int response=this->checkActionPriority(*activePtr,*rulePtr);
                if (response == ActionResolver::IGNORE_NEW_RULE){
                   useNewRule=false;
                   isConflicting=true;
                   RuleLog::event(LogEvent::IGNORED, rulePtr->getName(), activePtr->getName());
                   break;
                }
				if (response == ActionResolver::DEFER_NEW_RULE){
					useNewRule=false;
					isConflicting=true;
                    if (_deferredLogged.insert(rulePtr.get()).second){
                       RuleLog::event(LogEvent::DEFERRED, rulePtr->getName(), activePtr->getName());
                    }
                    rulePtr->deferActivation();
				}
                if (response == ActionResolver::REPLACE_OLD_RULE){
					useNewRule=true;
					isConflicting=true;
                    //cout << "replacing rule :" << activePtr->getName()<<endl;
				}
            }

            // Now bump the old rules and install new rule
            if (useNewRule){
               if(isConflicting){
				   for( OpPool::iterator activeiter = pool.begin() ;
                           activeiter != pool.end() ;
                           ++activeiter){
                     int response=this->checkActionPriority(**activeiter,**it);
						   if (response == ActionResolver::REPLACE_OLD_RULE){
                       (*activeiter)->setActive(false);
                       RuleLog::event(LogEvent::REPLACED, (*activeiter)->getName(), rulePtr->getName());
                     }
                  }
               }
               //cout << "Activating " << (*it)->getName()<<endl;
               (*it)->setActive(true);
               _deferredLogged.erase(rulePtr.get());
               if (RuleLog::enabled(RuleLog::EVENTS)){
                  std::vector<ActionInfo> actions;
                  const bool ok = rulePtr->describeAction(actions);
                  RuleLog::event(LogEvent::ACTIVATED, rulePtr->getName(), "", 0, !ok, &actions);
               }
               //cout << "Activated " << (*it)->getName()<<endl;
            }
         } // rule is applicable
      } // trigger tested positive
   }  // loop through inactive rules

}


void OperationManager::advanceActions(double dt){
    RuleLog::beginStep();
    for (OpPool::iterator it=pool.begin() ;
        it != pool.end(); it++){
            if((*it)->isActive()){
                (*it)->advanceAction(dt);
                if(!(*it)->isActive()){
                   RuleLog::event(LogEvent::COMPLETED, (*it)->getName(), "");
                }
            }
    }
}

void OperationManager::stepExpressions(double dt){
   for (OpPool::iterator it=pool.begin() ;
	    it != pool.end(); it++){
      (*it)->step(dt);
   }
}

class overlapper
 : public std::binary_function<OperationActionPtr,OperationActionPtr,bool>{
public:
    overlapper(ActionResolver& res) : m_resolver(res){}
    bool operator()(OperationActionPtr oldAct,
                   OperationActionPtr newAct)const
    {
        return m_resolver.overlap(*oldAct,*newAct);
    }
private:
    ActionResolver & m_resolver;
};


class list_overlapper
 : public std::binary_function<OperationActionPtr,
                               OperationAction::ActionListType*,
                               bool>{
public:
   list_overlapper(ActionResolver& res) : m_resolver(res){}
   bool operator()(OperationActionPtr act,
                   OperationAction::ActionListType* alist)const{
      return find_if(alist->begin(),
                     alist->end(),
                     std::bind2nd(overlapper(m_resolver),act))
             != alist->end(); }
private:
   ActionResolver & m_resolver;
};


bool OperationManager::actionsOverlap(
	   OperatingRule& oldRule,  OperatingRule& newRule){
   OperationAction::ActionListType oldActions(oldRule.getActionList());
   OperationAction::ActionListType newActions(newRule.getActionList());

   return
      find_if(newActions.begin(),
           newActions.end(),
           bind2nd(list_overlapper(_resolver), &oldActions)) != newActions.end();
}