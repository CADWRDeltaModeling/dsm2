#ifndef oprule_expression_EXPRESSIONNODE_H__INCLUDED_
#define oprule_expression_EXPRESSIONNODE_H__INCLUDED_
#include<iostream>
#include<string>
#include<utility>
#include<vector>
#include "oprule/expression/ExpressionPtr.h"

namespace oprule{
namespace expression{

/** Named values reported for logging: model variables a node reads and the internal state of
 *  stateful nodes (see ExpressionNode::collectState).
 */
typedef std::vector<std::pair<std::string,double> > StateList;

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

   /** Short readable label for a node that reads a model variable (for example
   *  "chan_stage(channel=12,dist=0)"); empty for every other node.
   */
   virtual std::string describe() const { return std::string(); }

   /** Report what this node sees, for the rule log.
   * A node with a label reports its current value. A composite node asks its children. A stateful
   * node (accumulate, predict, pid) reports its internal variables and asks its children. Only labelled
   * leaves call eval(), and those are plain reads of the model, so logging never evaluates a stateful
   * node and never changes what a rule sees.
   */
   virtual void collectState(StateList& out){
      std::string label = describe();
      if (!label.empty()) out.push_back(std::make_pair(label, static_cast<double>(eval())));
   }


};
/** shorthand for double expression*/
typedef ExpressionNode<double> DoubleNode;
typedef OE_NODE_PTR(ExpressionNode<double>) DoubleNodePtr;
/** shorthand for bool expression*/
typedef ExpressionNode<bool> BoolNode;
typedef OE_NODE_PTR(ExpressionNode<bool>) BoolNodePtr;
 }} // namespace
#endif // include guard
