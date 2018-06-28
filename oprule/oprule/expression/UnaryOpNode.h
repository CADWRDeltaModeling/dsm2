#ifndef oprule_expression_UNARYOPNODE_H__INCLUDED_
#define oprule_expression_UNARYOPNODE_H__INCLUDED_

#include "oprule/expression/ExpressionNode.h"
#include "oprule/expression/ExpressionPtr.h"
#include<numeric>

namespace oprule{
namespace expression{

/** ValueNode based on the result of a unary operation on a ValueNode. */
template<class UnaryFunc> 
struct UnaryOpNode : public ExpressionNode<typename UnaryFunc::result_type >  
{
   typedef UnaryOpNode<UnaryFunc> NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;


   /** Type of the argument in the function represented by this node*/
   typedef typename UnaryFunc::argument_type ArgType;
   typedef ExpressionNode<ArgType> ArgNodeType;
   typedef OE_NODE_PTR(ExpressionNode<ArgType>) ArgNodePtr;

   /** Type returned by the function represented by this node*/
   typedef typename UnaryFunc::result_type ResultType;
   typedef OE_NODE_PTR(ExpressionNode<ResultType>) BaseNodePtr;

   /** Create the unary op node
    * @param pArg node holding the argument to the function evaluated by this node.
    */
	UnaryOpNode( ArgNodePtr pArg) 
      : _pArg (pArg) {}


   static NodePtr create(ArgNodePtr arg){ 
     return NodePtr(new NodeType(arg));
  }

   virtual BaseNodePtr copy(){
	   return NodePtr(new NodeType(_pArg->copy()));
   }


   ResultType eval() {	return func(_pArg->eval());}

    virtual bool isTimeDependent() const{
        return _pArg->isTimeDependent();
    }

	virtual ~UnaryOpNode(){
       OE_NODE_DELETE(_pArg); 
    }
    virtual void init(){
        _pArg->init();
    }

    virtual void step(double dt){
        _pArg->step(dt);
    }

private: 
   ArgNodePtr _pArg;
	UnaryFunc func;
};


/** functor for absolute value*/
struct abs_func : public std::unary_function<double,double>
{  
   /**evaluate function 
    @param arg argument to function 
    @return result of function
   */
   double operator()(double arg)
   {
      return abs(arg);
   }
};




/** functor for square roots*/
struct sqrt_func : public std::unary_function<double,double>
{  
   /**evaluate function 
    @param arg argument to function 
    @return result of function
   */
   double operator()(double arg)
   {
      return sqrt(arg);
   }
};

/** functor for natural log */
struct ln_func : public std::unary_function<double,double>
{
   /**evaluate function 
    @param arg argument to function 
    @return result of function
   */
   double operator()(double arg)
   {
      return log(arg);
   }
};

/** functor for exponential function */
struct exp_func : public std::unary_function<double,double>
{
   /**evaluate function 
    @param arg argument to function 
    @return result of function
   */   
   double operator()(double arg)
   {
      return exp(arg);
   }
};

/** functor for log10 */
struct log10_func : public std::unary_function<double,double>
{  
   /**evaluate function 
    @param arg argument to function 
    @return result of function
   */   
   double operator()(double arg)
   {
      return log10(arg);
   }
};


}}     // namespace
#endif //include guard
