#include "LinearExtrapolationNode.h"
#include <math.h>  //only for HUGE_VAL
#include <assert.h>

LinearExtrapolationNode::~LinearExtrapolationNode(){}

LinearExtrapolationNode::LinearExtrapolationNode(
                  DoubleNode& node, 
                  const double& stepsProjected)
                  : _oldVal(HUGE_VAL), _node(node), 
                     c0(1.+stepsProjected), 
                     c1(-stepsProjected){}

void LinearExtrapolationNode::init(){
  _newVal=_node.eval();
  _oldVal=_newVal;
}

double LinearExtrapolationNode::eval(){ 
      assert(_oldVal != HUGE_VAL); //assure that initialize() has been called
      return c0*_newVal + c1*_oldVal;
}

void LinearExtrapolationNode::step(){
  _oldVal=_newVal;
  _newVal=_node.eval();
}



