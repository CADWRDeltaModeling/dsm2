#ifndef oprule_rule_TRANSITION_H__INCLUDED_
#define oprule_rule_TRANSITION_H__INCLUDED_
#include<math.h>
#include "boost/shared_ptr.hpp"
/**
 * Function objects representing transitions. They map a
 * fraction of completion in time to a fraction of
 * completion of a task
 */

namespace oprule {
namespace rule {


class Transition{
public:
    Transition(const double& duration): _duration(duration){;}
    virtual Transition* copy() const =0;
    double getDuration() const {return _duration;}
    virtual double operator()(const double& elapsed) const =0;
protected:
    double _duration;
};

class AbruptTransition : public Transition {
public:
    AbruptTransition():  Transition(0.){};
    Transition* copy() const { return new AbruptTransition();}
    double operator()(const double& elapsed) const {
        return 1.0;
    }
};

class LinearTransition : public Transition{
public:
    LinearTransition(const double& duration): Transition(duration){;}
    Transition* copy() const { return new LinearTransition(*this);}
    double operator()(const double& elapsed) const {
        return elapsed/_duration;
    }
};

class SmoothStepTransition : public Transition{
public:
    SmoothStepTransition(double _duration, double k) :
      Transition(_duration),
      tc(k){}
    Transition* copy() const { return new SmoothStepTransition(*this);}
    double operator()(const double& elapsed) const  {
        return 1./(1.+exp(-2.*elapsed*tc/_duration));
    }
private:
    double tc;
};

typedef boost::shared_ptr<Transition> TransitionPtr;

}}     //namespace
#endif // !include guard