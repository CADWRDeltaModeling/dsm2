///////////////////////////////////////////////////////////
//  OperatingRule.cpp
//  Implementation of the Class OperatingRule
//  Created on:      16-Jan-2004 11:24:16 PM
//  Original author: Eli Ateljevich
///////////////////////////////////////////////////////////
#include<algorithm>
#include<functional>
#include "oprule/rule/OperatingRule.h"


using namespace oprule::rule;

OperatingRule::OperatingRule(OperationActionPtr opact) :
   _action(opact), _prevTriggerValue(false){
}

OperatingRule::OperatingRule(OperationActionPtr opact,TriggerPtr trigger) :
   _action(opact), _trigger(trigger), _prevTriggerValue(false){
}

void OperatingRule::advanceAction(double dt){
    _action->advance(dt);
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

