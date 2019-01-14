#ifndef oprule_expression_LAGGED_EXPRESSION_NODE_H__INCLUDED_
#define oprule_expression_LAGGED_EXPRESSION_NODE_H__INCLUDED_
#include <stdexcept>
#include "oprule/expression/ExpressionNode.h"
#include "oprule/expression/LaggedValueHolder.h"

namespace oprule{
namespace expression{

/** 
 Expression node representing a value that is lagged in time.
*/
template<typename T>
class LaggedExpressionNode  : public ExpressionNode<T>, 
                              public LaggedValueHolder<T>
{
 public:
  
  typedef LaggedExpressionNode<T> NodeType;
  typedef OE_NODE_PTR(NodeType) NodePtr;
  typedef typename ExpressionNode<T>::NodePtr ExpressionNodePtr;

 /** Create a lagged expression with the stipulated number of lags.
   * @param lags number of lags to store.
   */
  LaggedExpressionNode(int lags) 
    : LaggedValueHolder<T>(lags){} //vector holding lagged values in

  LaggedExpressionNode(NodePtr expression, int lags)
    : LaggedValueHolder<T>(lags), _val(expression){}

  static NodePtr create(int lags){
    return NodePtr(new NodeType(lags));
  }

  virtual ExpressionNodePtr copy(){
    throw new std::logic_error("NOT IMPLEMENTED!");
    //	  return create(nlag);
  }

  virtual ~LaggedExpressionNode(){}


  /**
   Set the expression that gives the current value (lag zero)
   for this node.
   @param value the expression whose evaluation will give the current value.
  */
  virtual void setExpression(ExpressionNodePtr value){_val=value;}


  /**Inform this node that a step has been taken.
  */
  virtual void step(double dt){
	  _val->step(dt);
          throw new std::logic_error("NOT IMPLEMENTED!");
	  // advanceStep();
  }

  /** Get current value*/
  virtual T newVal(){return _val->eval();}

  /** Get lagged value (maximum lag)*/
  virtual T eval(){
          throw new std::logic_error("NOT IMPLEMENTED!");
	  return _val->eval();
	  //return getLaggedValue(maxLag());
  }

  virtual bool isTimeDependent() const{ return true; }
 
 private:
  ExpressionNodePtr _val;

};
/** Shorthand for lagged double node.*/
typedef LaggedExpressionNode<double> LaggedDoubleNode; 
/** Shorthand for lagged bool.*/
typedef LaggedExpressionNode<bool> LaggedBoolNode;


 }}    // namespace
#endif // include guard

