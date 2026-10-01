#ifndef DSM2_DEVICE_SAMPLER_H_INCLUDED
#define DSM2_DEVICE_SAMPLER_H_INCLUDED

#include <map>
#include <string>
#include "oprule/rule/LogTypes.h"

/** Finds the changes of gate device properties and reports them to the rule log
 *  (OPRULE_LOG_HDF5_PLAN.md, sections 3.9 and 3.11).
 *
 *  sample() is called once per step after the rule actions have been advanced and before the solve,
 *  so it sees the values the solver will use: those set by rule actions and those loaded from data
 *  sources. It only reads the model.
 *
 *  A change is written when
 *   - a rule wrote the property in this step (any change; a ramp only at its start and its end), or
 *   - the value changed by more than the tolerance of its property (a source-driven change).
 *  Tolerances are measured from the last value written, so a slow drift is not lost.
 */
class DeviceSampler {
public:
    DeviceSampler();

    /** Set the options and forget everything sampled. */
    void configure(double tolOp, double tolDim, bool context);

    /** Forget everything sampled. */
    void reset();

    /** Read all gate devices and report the changes. Does nothing when the log is off. */
    void sample();

private:
    struct Last {
        Last() : value(0.), have(false) {}
        double value;
        bool have;
    };

    void start(int ngate);
    void check(int gate, int device, int property);
    double tolerance(int property) const;
    std::string sourceLabel(int gate, int device, int property) const;
    void fillContext(int gate, oprule::rule::DeviceTransition& t) const;

    double _tolOp, _tolDim;
    bool _context;
    bool _started;
    std::map<long, Last> _last;
};

#endif
