#ifndef oprule_expression_BINARYOPNODE_H__INCLUDED_
#define oprule_expression_BINARYOPNODE_H__INCLUDED_

#include <boost/shared_ptr.hpp>
#include "oprule/expression/ExpressionNode.h"
#include "oprule/expression/ExpressionPtr.h"

#include<functional>
#include<math.h>
#include<iostream>

namespace oprule{
namespace expression{

/** Expression node that returns values based on evaluating the 
  * result of a binary op on two expression nodes.
  */
template<class BinaryFunc> 
class BinaryOpNode : public ExpressionNode<typename BinaryFunc::result_type >  
{
public:
   /** Argument types*/
   typedef typename BinaryFunc::first_argument_type FirstArgType;
   typedef typename BinaryFunc::second_argument_type SecondArgType;
   typedef ExpressionNode<FirstArgType> FirstArgNode;
   typedef ExpressionNode<SecondArgType> SecondArgNode;
   typedef OE_NODE_PTR(FirstArgNode) FirstArgPtr;
   typedef OE_NODE_PTR(SecondArgNode) SecondArgPtr;

   /** Result type*/
   typedef typename BinaryFunc::result_type ResultType;
   typedef BinaryOpNode<BinaryFunc> NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;
   typedef OE_NODE_PTR(ExpressionNode<ResultType>) ExpressionNodePtr;
   

   /** Create a binary op node based on the arguments to the function it will evaluate.
    * @param pLeft first argument to the binary function.
    * @param pRight second argument to the binary function
    */
   BinaryOpNode(FirstArgPtr pLeft, 
		          SecondArgPtr pRight)
     : _pLeft (pLeft), 
       _pRight (pRight),
       _timeDependent(_pLeft->isTimeDependent() ||
		              _pRight->isTimeDependent()) {}
	
    virtual ~BinaryOpNode()
	{
    OE_NODE_DELETE(_pLeft);
    OE_NODE_DELETE(_Right);
	};

   static NodePtr create(FirstArgPtr pLeft, 
		          SecondArgPtr pRight)
   {
     return NodePtr(new NodeType(pLeft,pRight));
   }

   virtual ExpressionNodePtr copy(){ 
     return NodePtr(new NodeType(_pLeft->copy(),_pRight->copy()));
  }

  /**Inform this node that a step has been taken.
  */
  virtual void step(double dt){
	  _pLeft->step(dt);
	  _pRight->step(dt);
  }


    virtual ResultType eval() {
		return func(_pLeft->eval(), _pRight->eval());
    }

    virtual bool isTimeDependent() const{return _timeDependent;}
    virtual void init(){ _pLeft->init(); _pRight->init();}

private: 
    FirstArgPtr _pLeft;
    SecondArgPtr _pRight;
	 BinaryFunc func;
    bool _timeDependent;
};

/** binary function based on power function. */
class power_func : public std::binary_function<double,double,double>
{
public:
   /**Evaluate the power of base to the exponent given
    * @param base base of exponent
    * @param expon parameter to which base is raised
    * @return result of power calculation
    */
   double operator()(double base, double expon)
   {
      return pow(base,expon);
   }
};

/** max of two variables. */
class max2 : public std::binary_function<double,double,double>
{
public:
   /**Evaluate the power of base to the exponent given
    * @param base base of exponent
    * @param expon parameter to which base is raised
    * @return result of power calculation
    */
   double operator()(double first, double second)
   {
	   return first > second ? first : second;
   }
};

/** max of two variables. */
class min2 : public std::binary_function<double,double,double>
{
public:
   /**Evaluate the power of base to the exponent given
    * @param base base of exponent
    * @param expon parameter to which base is raised
    * @return result of power calculation
    */
   double operator()(double first, double second)
   {
	   return first < second ? first : second;
   }
};


}}     //namespace
#endif // !include guard
