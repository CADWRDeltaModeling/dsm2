#ifndef oprule_expression_EXPRESSIONNODE_H__INCLUDED_
#define oprule_expression_EXPRESSIONNODE_H__INCLUDED_
#include<iostream>
#include "oprule/expression/ExpressionPtr.h"

namespace oprule{
namespace expression{

/** Expression node representing a value.
 */
template<typename T>
class ExpressionNode  
{
public:
   typedef ExpressionNode<T> NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;

   ExpressionNode(){};
   virtual ~ExpressionNode(){}

   /**
   * Virtual copy constructor idiom. Creates a
   * copy of the node. Didn't give default implementation because the
   * naive impl is almost always wrong.
   */
   virtual NodePtr copy()=0;

   /** Evaluate the node and return its current value.
    * @return current value
    */
	virtual T eval()=0;
   
   /**Test whether the expression is time dependent.
    * @return true if the expression is time dependent
   */
   virtual bool isTimeDependent() const=0;
   
   /**Perform any initialization for this node.*/
   virtual void init(){};

   /**Inform the expression that a step is being taken.
   * Generally this needs to be overridden when the expression
   * does some sort of cache or aggregation over time, or when
   * it contains sub-expressions (in case they need it).
   */
   virtual void step(double dt){};


};
/** shorthand for double expression*/
typedef ExpressionNode<double> DoubleNode;
typedef OE_NODE_PTR(ExpressionNode<double>) DoubleNodePtr;
/** shorthand for bool expression*/
typedef ExpressionNode<bool> BoolNode;
typedef OE_NODE_PTR(ExpressionNode<bool>) BoolNodePtr;
 }} // namespace
#endif // include guard
