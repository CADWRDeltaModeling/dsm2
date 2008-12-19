///////////////////////////////////////////////////////////
//  ActionSet.cpp
//  Implementation of the Class ActionSet
//  Created on:      16-Jan-2004 11:24:15 PM
//  Original author: Administrator  To change the template for this generated type comment go to Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
///////////////////////////////////////////////////////////

#include "oprule/rule/ActionSet.h"
#include <algorithm>
#include <functional>

using namespace std;
using namespace oprule::rule;

ActionSet::ActionSet()
{
  m_collectionType=OperationAction::SET_COLLECTION;
}

ActionSet::~ActionSet(){ subactions.clear();
}

void ActionSet::addAction(OperationActionPtr action){
   assert(! action->isActive());
   //action->registerParent(this);
   subactions.push_back(action);
}

void ActionSet::advance(double dt){
	for(ActionList::iterator it=subactions.begin() ;
	   it != subactions.end() ; it++){
	   (*it)->advance(dt);
	}
}


void ActionSet::setActive(bool active){
	for (ActionList::iterator it=subactions.begin() ;
	  it != subactions.end() ; it++){
     if (! (*it)->isActive() == active){
       (*it)->setActive(active);
     }
	}
   _active=active;
}


bool rule_active(OperationActionPtr op){ return op->isActive(); }


bool ActionSet::isActive(){
  return _active;
}


void ActionSet::childComplete(){
	ActionList::iterator it = find_if(subactions.begin(),
		subactions.end(), ptr_fun(rule_active));
   bool inprogress=(it!=subactions.end()) ;
   if (! inprogress){
      this->setActive(false);
      this->onCompletion();
   }
}

void ActionSet::appendSubActionsToList(ActionListType& listToConstruct){
   for (ActionSet::ActionList::iterator it = subactions.begin();
         it != subactions.end(); ++it){
             OperationActionPtr actPtr = *it;
             if ( actPtr->hasSubActions())
             {
                 actPtr->appendSubActionsToList(listToConstruct);
             }
             else
             {
                 listToConstruct.push_back(actPtr);
             }
   }
}


