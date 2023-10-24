#ifndef ModelStateTestFixture_H_32KR3__INCLUDED_
#define ModelStateTestFixture_H_32KR3__INCLUDED_

#include "oprule/rule/ModelInterface.h"
#include<iostream>

#define INIT_STEP 13
#define INIT_STATE 2
#define INIT_DYN_STATE 4
#define INIT_TIME 100
#define DT 15
#define DURATION 30
#define EXPRESSION_SET 1000

// A pseudo model that includes some static variables. The model interfaces
// below manipulate these static variables.
namespace test_model{
  typedef __int64 tick_t;
  static double dbl_static1;
  static double dbl_static2;
  static double dbl_dyn1;
  static double base_dyn1;
  static int int_static1;
  static int step;
  static tick_t time;

  //Initialize the pseudo model
  void model_init(){
    dbl_static1=INIT_STATE;
    dbl_static2=INIT_STATE;
    base_dyn1=INIT_DYN_STATE;
    dbl_dyn1=INIT_DYN_STATE;
    step=INIT_STEP;              // test doesn't necessarily start on "step 1"
    time=DT*INIT_STEP+INIT_TIME; // add some randomness
  }

  void model_goto_step(int newstep){
    step=INIT_STEP+newstep;
    time=step*DT+INIT_TIME;
    // just increments by 1 every step. This simulates the value
    // being set by a time series, before the action does anything.
    // The action will "permanently set the data source" by changing
    // base_dyn_state
    dbl_dyn1=base_dyn1+newstep;

  }
}

/**
 * A model state interface that manipulates dbl_static1
 */
class StaticModelState : public oprule::rule::ModelInterface<double>{
 public:
  typedef double StateType;
  typedef StaticModelState NodeType;
  typedef OE_NODE_PTR(StaticModelState) NodePtr;

  virtual bool isTimeDependent() const{return false;}
  StaticModelState() {}
  static NodePtr create(){return NodePtr(new StaticModelState());}
  virtual ExpressionNode<StateType>::NodePtr copy(){
	  return NodePtr(new StaticModelState());
  }
  StateType eval(){return test_model::dbl_static1;}
  void set(StateType val){test_model::dbl_static1=val;}
  bool operator==(const StaticModelState& other){return true;}

};

/**
 * A model state interface that manipulates dbl_static2
 */
class AnotherStaticModelState : public oprule::rule::ModelInterface<double>{
 public:
  typedef double StateType;
  typedef AnotherStaticModelState NodeType;
  typedef OE_NODE_PTR(AnotherStaticModelState) NodePtr;
  virtual bool isTimeDependent() const{return false;}
  AnotherStaticModelState() {}
  static NodePtr create(){
	  return NodePtr(new AnotherStaticModelState());
  }
  virtual ExpressionNode<StateType>::NodePtr copy(){
	  return NodePtr(new AnotherStaticModelState());
  }
  StateType eval(){return test_model::dbl_static2;}
  void set(StateType val){test_model::dbl_static2=val;}
  bool operator==(const AnotherStaticModelState& other){return true;}
};

/**
 * A model state interface that manipulates int_static1
 */
class IntStaticModelState : public oprule::rule::ModelInterface<int>{
 public:
  typedef int StateType;
  typedef IntStaticModelState NodeType;
  typedef OE_NODE_PTR(IntStaticModelState) NodePtr;

  virtual bool isTimeDependent() const{return false;}
  IntStaticModelState() {}
  static NodePtr create(){return NodePtr(new IntStaticModelState());}
  virtual ExpressionNode<StateType>::NodePtr copy(){
	  return NodePtr(new IntStaticModelState());
  }

  StateType eval(){return test_model::int_static1;}
  void set(StateType val){test_model::int_static1;}
  bool operator==(const IntStaticModelState& other){return true;}
};

/**
 * A model state interface that manipulates dbl_dyn1
 */
class DynamicModelState : public oprule::rule::ModelInterface<double>{
 public:
  typedef double StateType;
  typedef DynamicModelState NodeType;
  typedef OE_NODE_PTR(DynamicModelState) NodePtr;
  typedef ExpressionNode<StateType>::NodePtr ExpressionNodePtr;

  virtual bool isTimeDependent() const{return true; }
  DynamicModelState(){}
  static NodePtr create(){return NodePtr(new DynamicModelState());}
  virtual ExpressionNodePtr copy(){return NodePtr(new DynamicModelState());}

  StateType eval(){return test_model::dbl_dyn1;}
  virtual void setDataExpression(
	  oprule::expression::ExpressionNode<StateType>::NodePtr val  ){
    test_model::base_dyn1=EXPRESSION_SET;
  }
  virtual void set(StateType val){test_model::dbl_dyn1=val;}
  virtual bool operator==(const DynamicModelState& other){return true;}

};


#endif  // INCLUDE GUARD

