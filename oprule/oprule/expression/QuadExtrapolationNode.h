#ifndef oprule_expression_QUADEXTRAPOLATIONNODE_H__INCLUDED_
#define oprule_expression_QUADEXTRAPOLATIONNODE_H__INCLUDED_
#include "oprule/expression/ExpressionNode.h"

#include <math.h>  //only for HUGE_VAL
#include <assert.h>
#include "boost/shared_ptr.hpp"

namespace oprule{
namespace expression{

/**Node that evaluates to the quadratically forecast
 * It is currently not complete.
 */

class QuadExtrapolationNode : public oprule::expression::DoubleNode{
    typedef QuadExtrapolationNode NodeType;
    typedef boost::shared_ptr<QuadExtrapolationNode> NodePtr;
    typedef ExpressionNode<double> ExpressionNode;
    typedef OE_NODE_PTR(ExpressionNode) ExpressionNodePtr;


public:
    /** Create the node given the expression node it will extrapolate.
    * NOTE: this is unfinished for variable steps
    *@param node node to extrapolate
    *@param time length to project 
    *           
    */
    QuadExtrapolationNode(ExpressionNodePtr node, 
                          double time) :
        _node(node),
        _time(time),
        _newVal(HUGE_VAL),
        _oldVal(HUGE_VAL), 
        _prevVal(HUGE_VAL),
        _step(HUGE_VAL),
        _oldStep(HUGE_VAL){

        }
    virtual ~QuadExtrapolationNode(){
        OE_NODE_DELETE(_node);
    };


    static NodePtr create(DoubleNodePtr expr, double steps){ 
        return boost::shared_ptr<QuadExtrapolationNode>(new QuadExtrapolationNode(expr,steps));
    }

    virtual DoubleNodePtr copy(){
        return NodePtr(new QuadExtrapolationNode(_node->copy(),_time));
    }

    void init(){
        _newVal=_node->eval();
        _oldVal=_newVal;
        _prevVal=_newVal;
        _step=0.;
        _oldStep=0.;
        _extrap=_newVal;

    }

    double eval(){
        assert(_oldVal != HUGE_VAL); //assure that initialize() has been called
        assert(_prevVal != HUGE_VAL);
        return _extrap;
    }

    void step(double dt){
        //todo: needs real dt
        _node->step(dt);
        _prevVal=_oldVal;
        _oldVal=_newVal;
        _newVal=_node->eval();
        _oldStep=_step;
        _step=dt;
        if (_step != _oldStep && _oldStep != 0){
          throw std::logic_error("Variable step sizes not supported yet");
        }
        double steps = _time/dt;
        c0= (steps*steps)/2.+ 1.5*steps +1.;
        c1=-steps*steps - 2.0*steps;
        c2= steps*steps/2. + steps/2.;
        _extrap=c0*_newVal + c1*_oldVal + c2*_prevVal;
        
    }

    virtual bool isTimeDependent()const { return true; }

private:

    void lagVals();
    ExpressionNodePtr _node;
    double _time;
    double _newVal;
    double _oldVal;
    double _prevVal;
    double _step;
    double _oldStep;
    double c0,c1,c2;
    double _extrap;

};

}}     // namespace
#endif // include guard
