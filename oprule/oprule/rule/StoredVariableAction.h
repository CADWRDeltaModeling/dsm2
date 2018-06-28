#ifndef oprule_rule_StoredVariableAction_H_BC40__INCLUDED_
#define oprule_rule_StoredVariableAction_H_BC40__INCLUDED_


#include "OperationAction.h"
#include "oprule/expression/ExpressionNode.h"
#include "oprule/rule/ModelInterface.h"
#include<iostream>
#include<assert.h>


namespace oprule {
namespace rule {


/** Manipuates stored variable(s)
 *  for the operating rule framework
 *  
 */
class StoredVariableAction : public OperationAction  
{ 
 public:

  /** Create the model action.
   * @todo cheeseball to have transition be fixed -- make it a policy.
   */
  StoredVariableAction(string variableName, ExpressionNode) :
    _active(false),
	_variableName(variableName)
  {setActive(false);}
  
  /** Virtual destructor */
  virtual ~StoredVariableAction(){};

  /** Advance this action.
   *  Sets the value of the variable 
   */
  virtual void advance(){



  /** Decide if this action is relevant
   * Intent was that actions might realize they are not applicable because their
   * action is redundant given the current model state.
   * @return true if the action is applicable and not redundant
   * @todo doubt we are using this
   */
  virtual bool isApplicable(){
 // parameters assignment is not applicable if
 // the parameter is already at the target level (?)
 //    if (! _expression->isTimeDependent() ){
 //       return (_interface->eval() != _expression->eval());
 //    }      //todo: time varying part
      //todo: do we want this behavior ?
     return true;
  }

  /** Set this action active.
   * Used by a compound action or rule to set the action active. Any testing is presumed
   * done -- this just sets it going.
   */
  virtual void setActive(bool active);

  /** Do the action represented by this action
   * @todo doubt this is used -- think it is superceded by advance().
   */
  virtual void doAction(){
    _interface->set(_currentState);
  }


  virtual bool isActive() { 
    return _active; 
  }
  virtual void onCompletion();

  /**
   * Inform this Action that a step has taken place. The reason for this is just to
   * pass the information on to the underlying (target) expression. This does not
   * cause the Action to do anything!!!
   */
  virtual void step(){_expression->step();}

  /**Get the model interface used by this action to manipulate the model.
   * @return the model interface
   */
  typename ModelInterface<ModelIFType>::NodePtr getModelInterface(){
    return _interface;
  }   

 private:
	
  typename ModelInterface<ModelIFType>::NodePtr _interface;
  typename ExprType::NodePtr _expression;
  ModelTimer _timer;
  typename ModelTimer::TimeType _initTime;
  typename ModelTimer::DurationType _transDur;
  ModelIFType _initState;
  ModelIFType _currentState;
  ModelIFType _baseState;
  bool _active;
  double _transFraction;
};




template<class ModelTimer, class ModelIFType >
  void StoredVariableAction<ModelTimer, ModelIFType >::setActive(bool active){
  if (active){
    _initTime=_timer.ticks();
    if (! _interface->isTimeDependent()){
      // The parameter is stored in a single, static storage variable,
      // advance() is going to be changing it, so 
      // we need a snapshot. 
      _initState=_interface->eval();
    }
    _active=true;   
  }else{
    _active=false;
  }
}


template<class ModelTimer, class ModelIFType >
  void StoredVariableAction<ModelTimer, ModelIFType>::onCompletion(){
  if (_interface->isTimeDependent()){ 
    // permanently fix expression as data source
    _interface->setDataExpression(_expression);
  }
  // then do all the normal stuff in the parent implementation
  OperationAction::onCompletion();
}


template<class ModelTimer, class ModelIFType >
  void StoredVariableAction<ModelTimer, ModelIFType >::advance(){
  assert(_active);
  if ( !_interface->isTimeDependent()){
    _baseState=_initState;
  }else{
    _baseState=getModelInterface()->eval();
  }
  double _transFraction=0.;
  ModelTimer::TimeType _dur=(_timer.ticks() - _initTime);
  if ( _timer.ticksFromDuration(_transDur) > 0){
    _transFraction=((double)_dur)/((double)_timer.ticksFromDuration(_transDur));
    if(_transFraction > 1.0) _transFraction=1.0;
  }else{
    _transFraction=1.0;
  }
  _currentState=_baseState*(1.0 - _transFraction) + 
    _expression->eval()*_transFraction;
  _interface->set(_currentState);
  if(_transFraction == 1.0){
    setActive(false);
    this->onCompletion();
  }
}



}}     //namespace
#endif // !include guard
