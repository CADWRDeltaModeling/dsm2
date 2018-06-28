
#include "QuadExtrapolationNode.h"
#include <math.h>  //only for HUGE_VAL
#include <assert.h>
QuadExtrapolationNode::~QuadExtrapolationNode(){}

QuadExtrapolationNode::QuadExtrapolationNode(
                  DoubleNode& node, 
                  const double& steps)
  : _oldVal(HUGE_VAL), _node(node),
    c0(steps*steps)/2. + 1.5*steps +1.),
    c1(-steps*steps - 2.0*steps),
    c2( steps*steps/2. + steps/2.){}

void QuadExtrapolationNode::init(){
  _oldVal=_node.eval();  //initialize _newVal
  _prevVal=_oldVal;
}

double QuadExtrapolationNode::eval(){ 
      assert(_oldVal != HUGE_VAL); //assure that initialize() has been called
      return c0*_newVal + c1*_oldVal + c2*_prevVal;
}

void QuadExtrapolationNode::step(){ 
      _prevVal=_oldVal;
      _oldVal=_node.eval();
}



