///////////////////////////////////////////////////////////
//  ActionChain.cpp
//  Implementation of the Class ActionChain
//  Created on:      16-Jan-2004 11:24:15 PM
//  Original author: Eli Ateljevich
//////////////////////////////////////////////////////////////

#include "oprule/rule/ActionChain.h"

using namespace oprule::rule;

ActionChain::ActionChain() :
  _active(false)
{
  m_collectionType=OperationAction::CHAIN_COLLECTION;
}

ActionChain::~ActionChain()
{
    actionSequence.clear();
}

OperationActionPtr ActionChain::getCurrentAction(){
    return *(this->actionIterator);
}


void ActionChain::pushBackAction(OperationActionPtr opAction){	
   //opAction->registerParent(this); //was a good idea -- circular?
   this->actionSequence.push_back(opAction);
}

void ActionChain::pushFrontAction(OperationActionPtr opAction){
   //opAction->registerParent(this);   
   this->actionSequence.push_front(opAction);
}

void ActionChain::advance(double dt){
   getCurrentAction()->advance(dt);
}

void ActionChain::setActive(bool active){
   if(active){
     actionIterator=actionSequence.begin();
     assert( getCurrentAction());
   }
   getCurrentAction()->setActive(active);
   _active=active;
}

bool ActionChain::isActive(){
   return _active;
}


void ActionChain::onActivate(){
}

void ActionChain::childComplete(){
   ++actionIterator;
   if(this->actionIterator==actionSequence.end()){
      this->_active=false;
      this->onCompletion();
   }else{
      getCurrentAction()->setActive(true);
      getCurrentAction()->advance(0.);   //todo: urgent
   }
}


void ActionChain::appendToActionList(OperationAction::ActionListType& listToConstruct)
{
   std::cout <<"Appending chain to actionlist" <<std::endl;
   for (ActionChain::ActionSequence::iterator it = actionSequence.begin();
         it != actionSequence.end(); ++it)
   {
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

