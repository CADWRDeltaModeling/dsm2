#ifndef oprule_expression_LOOKUPNODE_H__INCLUDED_
#define oprule_expression_LOOKUPNODE_H__INCLUDED_

#include "oprule/expression/ExpressionNode.h"
#include "oprule/expression/ExpressionPtr.h"
#include<iostream>

namespace oprule{
namespace expression{

/** ValueNode based on the result of a unary operation on a ValueNode. */
class LookupNode : public ExpressionNode<double >
{
public:
   typedef LookupNode NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;

   /** Type of the argument in the function represented by this node*/
   typedef double ArgType;
   typedef ExpressionNode<ArgType> ArgNodeType;
   typedef OE_NODE_PTR(ExpressionNode<ArgType>) ArgNodePtr;

   /** Type returned by the function represented by this node*/
   typedef double TypeResult;

   /** Create the node
    * @param pArg node holding the argument to the function evaluated by this node.
    * @levels a vector of lookup thresholds
    * @values a vector of values
    */
  LookupNode(ArgNodePtr pArg,
	     std::vector<double>& levels,
	     std::vector<double>& values)
    :
    _pArg (pArg),
    _levels(levels),
    _values(values)
    {
      if (_levels.size() != (_values.size()+1)){
		std::cerr << "# Levels: " << _levels.size() << "# Values: " << _values.size() << std::endl;
		throw std::domain_error("Number of values must be one fewer than number of levels");
      }
      for (size_t i = 1; i < _levels.size() ; ++i){
	if (_levels[i] < _levels[i-1]){
	  throw std::domain_error("Levels must be monotonically nondecreasing");
	}
      }
    }

  static NodePtr create(ArgNodePtr arg,
			std::vector<double>& levels,
			std::vector<double>& values)
  {
    return NodePtr(new NodeType(arg,
		                        levels,
								values));
  }

  virtual ExpressionNode<double >::NodePtr copy()
    {
      return NodePtr(
		     new NodeType(_pArg->copy(),
				  _levels,
				  _values)
		     );
    }

    void step(double dt){
	  _pArg->step(dt);
	}

  /**
     Returns the item in the value vector corresponding to the
     first index where the argument is >= to the
     corresponding lookup vector. Evaluation of argments
     less than the first lookup item or greater than
     the last lookup item are a domain_error
  */
  double eval()
    {
      double arg = _pArg->eval();
      if (arg < _levels[0] || arg > _levels[_levels.size()-1]){
	throw std::domain_error("Value not in range.");
      }
      double out;
      for( size_t i = 0; i < _levels.size() ; ++i){
	if (arg >= _levels[i])
	  {
	    out = _values[i];
	  }
      }
      return out;
    }
    virtual bool isTimeDependent() const{ return _pArg->isTimeDependent(); }
    virtual ~LookupNode(){
       OE_NODE_DELETE(_pArg);
    }
    virtual void init(){ _pArg->init();}

private:
   ArgNodePtr _pArg;
   std::vector<double> _levels;
   std::vector<double> _values;
};


}}     // namespace
#endif //include guard
