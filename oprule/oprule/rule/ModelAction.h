#ifndef oprule_rule_MODELACTION_H__INCLUDED_
#define oprule_rule_MODELACTION_H__INCLUDED_

#include "OperationAction.h"
#include "oprule/expression/ExpressionNode.h"
#include "oprule/rule/ModelInterface.h"
#include "oprule/rule/Transition.h"
#include "oprule/rule/RuleLog.h"
#include<iostream>
#include<sstream>
#include<assert.h>


namespace oprule {
namespace rule {

/** Action that manipuates model variable(s).
*
*/
template<typename T >
class ModelAction : public OperationAction
{
public:

    /** Create the model action.
    * @param model interface used to manipulate and monitor the model.
    * @param expression used as target value for the model variable.
    * @param transitionDuration length of any ramping.
    * @todo transition could be a policy.
    */
    ModelAction(
        typename ModelInterface<T>::NodePtr  model,
        typename ExpressionNode<T>::NodePtr  expression,
        const TransitionPtr                  transition
        ) :
        _interface(model),
        _expression(expression),
        _transDuration(transition->getDuration()) ,
        _elapsed(0.),
        _transition(transition->copy())
    {
        _initState = T();
        _currentState = T();
        _baseState = T();
        _transFraction = 0.;
        setActive(false);
    }

    /** Virtual destructor */
    virtual ~ModelAction()
    {
    delete _transition;
    };

    /** Inform this action of a time step.
    *  Advances the expression.
    */
    virtual void step(double dt){
        _expression->step(dt);
    }


    /** Advance this action.
    *  Effect of advance depends on what this action does. If it gradually
    *  changes the value of a static parameter, for instance, then advance will
    *  be one step in this change.
    */
    virtual void advance(double dt);


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

    /** Description for the rule log (see OperationAction::describeState). */
    virtual std::string describeState(){
        const bool dynamic = _interface->isTimeDependent();
        std::ostringstream s;
        s << "interface=" << _interface->describe()
          << " mode=" << (dynamic ? "time_dependent" : "static")
          << " init=" << (dynamic ? std::string("live") : RuleLog::number(static_cast<double>(_initState)))
          << " duration=" << RuleLog::number(_transDuration)
          << " elapsed=" << RuleLog::number(_elapsed)
          << " target_inputs=" << targetInputs();
        return s.str();
    }


    /**Get the model interface used by this action to manipulate the model.
    * @return the model interface
    */
    typename ModelInterface<T>::NodePtr getModelInterface(){
        return _interface;
    }

private:

    // The model variables the target expression reads, as "[a=1; b=2]"; never fails the model run.
    std::string targetInputs(){
        try {
            oprule::expression::StateList inputs;
            _expression->collectState(inputs);
            return RuleLog::state(inputs);
        } catch (...) {
            return "unavailable";
        }
    }

    typename ModelInterface<T>::NodePtr _interface;
    typename ExpressionNode<T>::NodePtr _expression;
    double _elapsed;          // elapsed time in transition
    double _transDuration;    // total time of transition

    T _initState;
    T _currentState;
    T _baseState;
    bool _active;
    double _transFraction;
    Transition* _transition;
};


template<class T >
void ModelAction<T >::setActive(bool active){
    if (active){
        _elapsed=0.0;
        if (! _interface->isTimeDependent()){
            //snapshot of state at beginning of transition
            _initState=_interface->eval();
        }
        _active=true;
    }else{
        _active=false;
    }
}


template<class T >
void ModelAction<T>::onCompletion(){
    if (_interface->isTimeDependent()){
        // permanently fix expression as data source
        _interface->setDataExpression(_expression);
    }
    // then do all the normal stuff in the parent implementation
    OperationAction::onCompletion();
}


template<class T >
void ModelAction<T >::advance(double dt){
    assert(_active);
    if ( !_interface->isTimeDependent()){
        _baseState=_initState;
    }else{
        _baseState=getModelInterface()->eval();
    }
    _elapsed += dt;
    double remainTime = 0.;
    if( _elapsed < _transDuration){
        remainTime = 0.;
        _transFraction = (*_transition)(_elapsed);
    }else{
        _transFraction = 1.0;
        remainTime = _elapsed - _transDuration;
    }

    T target = _expression->eval();   // evaluated once, so logging adds no evaluation
    _currentState=_baseState*(1.0 - _transFraction) +
        target*_transFraction;
    _interface->set(_currentState);
    if (RuleLog::enabled(RuleLog::ACTIONS)){
        std::ostringstream detail;
        detail << "interface=" << _interface->describe()
               << " elapsed=" << RuleLog::number(_elapsed)
               << " fraction=" << RuleLog::number(_transFraction)
               << " base=" << RuleLog::number(static_cast<double>(_baseState))
               << " target=" << RuleLog::number(static_cast<double>(target))
               << " value=" << RuleLog::number(static_cast<double>(_currentState))
               << " duration=" << RuleLog::number(_transDuration)
               << " init=" << (_interface->isTimeDependent() ? std::string("live") : RuleLog::number(static_cast<double>(_initState)))
               << " target_inputs=" << targetInputs();
        RuleLog::write(RuleLog::ACTIONS, "ACTION", RuleLog::context(), detail.str());
    }
    if(_transFraction == 1.0){
        setActive(false);
        this->onCompletion();
    }
}

}}     //namespace
#endif // !include guard
