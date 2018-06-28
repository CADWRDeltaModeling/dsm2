#ifndef oprule_expression_LINEAR_EXTRAPOLATIONNODE_H__INCLUDED_
#define oprule_expression_LINEAR_EXTRAPOLATIONNODE_H__INCLUDED_

#include "oprule/expression/ValueNode.h"
//#include<iostream>
namespace oprule{
namespace expression{

/**Node that evaluates to the linearly forecasted value of another, provided node
 @todo base this on LaggedValueHolder
 @todo probably NOT based on LaggedValueHolder, but why is this hardwired to Double?
*/
template<typename T>
class LinearExtrapolationNode : public ExpressionNode<T>{
public:
  typedef LinearExtrapolationNode NodeType;
  typedef OE_NODE_PTR(LinearExtrapolationNode<T>) NodePtr;
  typedef ExpressionNode<T> ExpressionNode;
  typedef OE_NODE_PTR(ExpressionNode) ExpressionNodePtr;

   /** Create the node given the expression node it will extrapolate.
    *@param node node to extrapolate
    *@param stepsProjected number of steps to project ahead
    */

   LinearExtrapolationNode(
                  ExpressionNodePtr node, 
                  const double time):
                    _node(node),
                    _newVal(HUGE_VAL),
                    _oldVal(HUGE_VAL),
                    _extrap(HUGE_VAL),
                    _time(time)
				    {}

   static NodePtr create(ExpressionNodePtr expr, double time){ 
     return NodePtr(new NodeType(expr,time));
   }
   
   virtual ExpressionNodePtr copy(){
     return NodePtr(new NodeType(_node->copy(),_time));
   }
 
    void init(){
       _newVal=_node->eval();
       _oldVal=_newVal;
    }

     double eval(){ 
      assert(_oldVal != HUGE_VAL); //assure that initialize() has been called
      return _extrap;
     }

    void step(double dt){
   	  _node->step(dt);
	  _oldVal=_newVal;
      _newVal=_node->eval();
      _extrap=_newVal+(_newVal-_oldVal)*(_time/dt);
    }



    virtual ~LinearExtrapolationNode(){
       OE_NODE_DELETE(_node);
    }


   virtual bool isTimeDependent() const {return true;}

private:
   void lagVals();
   ExpressionNodePtr _node;
   double _newVal;
   double _oldVal;
   double _extrap;
   double _time;
};

 }}    // namespace
#endif // include guard
