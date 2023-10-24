#ifndef oprule_rule_ACTIONSTATE_H__INCLUDED_
#define oprule_rule_ACTIONSTATE_H__INCLUDED_

namespace oprule {
namespace rule {

class ModelActionState{
public:

    virtual void advance(double dt)=0;

    virtual bool isBlocking()=0;

    virtual void exitState()=0;
}

}}     //namespace
#endif // include guard