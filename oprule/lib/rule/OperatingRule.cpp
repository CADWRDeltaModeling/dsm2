///////////////////////////////////////////////////////////
//  OperatingRule.cpp
//  Implementation of the Class OperatingRule
//  Created on:      16-Jan-2004 11:24:16 PM
//  Original author: Eli Ateljevich
///////////////////////////////////////////////////////////
#include<algorithm>
#include<functional>
#include "oprule/rule/OperatingRule.h"
#include "oprule/rule/RuleLog.h"


using namespace oprule::rule;

OperatingRule::OperatingRule(OperationActionPtr opact) :
   _action(opact), _prevTriggerValue(false), _lastTriggerValue(false),
   _testedBefore(false), _triggerChange(NO_CHANGE){
}

OperatingRule::OperatingRule(OperationActionPtr opact,TriggerPtr trigger) :
   _action(opact), _trigger(trigger), _prevTriggerValue(false), _lastTriggerValue(false),
   _testedBefore(false), _triggerChange(NO_CHANGE){
}

void OperatingRule::advanceAction(double dt){
    RuleLog::setContext(_name);
    _action->advance(dt);
    RuleLog::setContext("");
}

bool OperatingRule::isActionApplicable(){
    return _action->isApplicable();
}

bool OperatingRule::testTrigger(){
    return _trigger->test();
}


//todo: this embeds the prev=current logic
bool OperatingRule::testNewlyTriggered(){
   bool current=_trigger->test();
   if (!_testedBefore){
      _triggerChange = current ? RISING : INITIAL_FALSE;
   }else if (current == _lastTriggerValue){
      _triggerChange = NO_CHANGE;
   }else{
      _triggerChange = current ? RISING : FALLING;
   }
   _testedBefore=true;
   _lastTriggerValue=current;
   bool ret= (! _prevTriggerValue) && current;
   _prevTriggerValue=current;
   return ret;
}

void OperatingRule::step(double dt){
   _action->step(dt);
   _trigger->step(dt);
}

void OperatingRule::deferActivation(){
   _prevTriggerValue=false;
}


OperationAction::ActionListType& OperatingRule::getActionList(){
   if (this->_action == NULL) throw new std::domain_error(
      "Op rule action not initialized when action list requested.");

   if (_actionList.empty())
   {
       if(_action->hasSubActions())
       {
         _action->appendSubActionsToList( _actionList );
       }
       else
       {
           _actionList.push_back(_action);
       }
   }
   return _actionList;
}

OperatingRule::~OperatingRule(){
  //delete _trigger;
}

bool OperatingRule::describeTrigger(LogValues& out){
   try {
      if (_trigger) _trigger->collectState(out);
      return true;
   } catch (...) {
      out.clear();
      return false;
   }
}

bool OperatingRule::describeAction(std::vector<ActionInfo>& out){
   try {
      if (_action) _action->describeActions(out);
      return true;
   } catch (...) {
      out.clear();
      return false;
   }
}

