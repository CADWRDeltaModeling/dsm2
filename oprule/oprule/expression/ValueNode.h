#ifndef oprule_expression_VALUE_NODE_H__INCLUDED_
#define oprule_expression_VALUE_NODE_H__INCLUDED_

#include "oprule/expression/ExpressionNode.h"
#include "boost/shared_ptr.hpp"
namespace oprule{
namespace expression{

/** Expression node that holds and returns an unchanging value.
 */
template<typename T>
class ValueNode  : public ExpressionNode<T>
{
public:
    typedef ValueNode<T> NodeType;
    typedef OE_NODE_PTR(NodeType) NodePtr;

    /** Create the node and initialize it to the given value
    * @param value the value that the node will represent.
    */
    ValueNode(T value) : _val(value){};

    static NodePtr create(T arg){
        NodeType * t = new NodeType(arg);
        return NodePtr(t);
    }
    //todo: should be const
    virtual typename ExpressionNode<T>::NodePtr copy(){
        return NodePtr(new NodeType(_val));
    }

    virtual ~ValueNode(){};
    virtual T eval(){ 
        return _val;
    };
    virtual bool isTimeDependent() const{ 
        return false; 
    }

private:
    T _val;

};

/** Shorthand for value node returning a double*/
typedef ValueNode<double> DoubleScalarNode; 

/** Shorthand for value node returning a bool*/
typedef ValueNode<bool> BoolScalarNode;

 }}    // namespace
#endif // include guard
