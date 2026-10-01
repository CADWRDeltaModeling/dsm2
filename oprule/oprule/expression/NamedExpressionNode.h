#ifndef oprule_expression_NAMEDEXPRESSIONNODE_H__INCLUDED_
#define oprule_expression_NAMEDEXPRESSIONNODE_H__INCLUDED_

#include <string>
#include "oprule/expression/ExpressionNode.h"
#include "oprule/expression/ExpressionPtr.h"

namespace oprule{
namespace expression{

/** Reference to a named expression (for example "mscs_calc") inside a rule.
 * It behaves exactly like the expression it wraps. It only keeps the name, so the rule log can report the
 * value of each named expression a trigger or a target is built from, not just the model variables at the
 * bottom of it.
 */
template<typename T>
class NamedExpressionNode : public ExpressionNode<T>
{
public:
    typedef OE_NODE_PTR(ExpressionNode<T>) ExpressionNodePtr;

    NamedExpressionNode(const std::string& name, ExpressionNodePtr expression)
        : _name(name), _express(expression){}

    virtual ~NamedExpressionNode(){}

    virtual ExpressionNodePtr copy(){
        return ExpressionNodePtr(new NamedExpressionNode<T>(_name, _express->copy()));
    }

    virtual T eval(){ return _express->eval(); }
    virtual bool isTimeDependent() const{ return _express->isTimeDependent(); }
    virtual void init(){ _express->init(); }
    virtual void step(double dt){ _express->step(dt); }

    /** The name, then whatever the wrapped expression reports. */
    virtual void collectState(StateList& out){
        out.push_back(std::make_pair(_name, static_cast<double>(_express->eval())));
        _express->collectState(out);
    }

private:
    std::string _name;
    ExpressionNodePtr _express;
};

}}     //namespace
#endif // include guard
