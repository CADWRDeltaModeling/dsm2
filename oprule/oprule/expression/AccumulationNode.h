#ifndef oprule_expression_ACCUMULATION_NODE_H__INCLUDED_
#define oprule_expression_ACCUMULATION_NODE_H__INCLUDED_

#include "oprule/expression/ExpressionNode.h"
#include "oprule/expression/ExpressionPtr.h"

namespace oprule{
namespace expression{

/**
 * Expression node representing accumulation over time.
 * It can be used to accumulate counts or integrate
*/
template<typename T>
class AccumulationNode  : public ExpressionNode<T>
{
public:
    typedef OE_NODE_PTR(ExpressionNode<T>) ExpressionNodePtr;
    typedef OE_NODE_PTR(AccumulationNode<T>) NodePtr;


    /** Create a lagged expression with the stipulated number of lags.
    * @param expression the expression being accumulated
    * @param init_value: the initial value for the parameter
    * @param reset_condition: resets the expression to init_value
    */
    AccumulationNode(ExpressionNodePtr expression,
        ExpressionNodePtr init_value,
        BoolNodePtr reset_condition)
        : _express(expression),
        _initializer(init_value),
        _resetter(reset_condition){
            _accum=_initializer->eval();
    }

    virtual ~AccumulationNode(){
        OE_NODE_DELETE(_resetter);
        OE_NODE_DELETE(_initializer);
        OE_NODE_DELETE(_express);
    }

    static NodePtr create(ExpressionNodePtr expr,
        ExpressionNodePtr init_value,
        BoolNodePtr reset){
            return NodePtr(new AccumulationNode<T>(expr,init_value,reset));
    }

    virtual ExpressionNodePtr copy(){
        return NodePtr(new AccumulationNode<T>(_express->copy(),
            _initializer->copy(),
            _resetter->copy())
            );
    }

    virtual void init(){
        _accum=_initializer->eval();
    }

    /**Inform this node that a step has been taken.
    *
    */
    virtual void step(double dt){
        _resetter->step(dt);
        _initializer->step(dt);
        _express->step(dt);
        if (_resetter->eval()){
            _accum=_initializer->eval();
        }
        _accum+=_express->eval(); //todo: urgent decide this

    }

    /** Get lagged value (maximum lag)*/
    virtual T eval(){return _accum;}

    virtual bool isTimeDependent() const{ return true; }

private:
    T _accum;
    ExpressionNodePtr _express;
    ExpressionNodePtr _initializer;
    BoolNodePtr _resetter;
};


}}     //namespace

#endif //include guard
