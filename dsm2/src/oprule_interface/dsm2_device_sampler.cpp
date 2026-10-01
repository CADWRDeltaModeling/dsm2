#include "dsm2_device_sampler.h"

#include <cmath>
#include <sstream>
#include <vector>

#include "oprule/rule/RuleLog.h"
#include "dsm2_interface_fortran.h"

using namespace oprule::rule;

namespace {
const int OBJ_CHANNEL = 1;      // constants.f90
const int OBJ_RESERVOIR = 3;

long key(int gate, int device, int property){
    return (static_cast<long>(gate) * 100 + device) * 10 + property;
}
}

DeviceSampler::DeviceSampler() : _tolOp(0.001), _tolDim(0.01), _context(true), _started(false){}

void DeviceSampler::configure(double tolOp, double tolDim, bool context){
    _tolOp = tolOp;
    _tolDim = tolDim;
    _context = context;
    reset();
}

void DeviceSampler::reset(){
    _started = false;
    _last.clear();
}

double DeviceSampler::tolerance(int property) const{
    switch (property){
    case PROP_OP_TO_NODE:
    case PROP_OP_FROM_NODE: return _tolOp;
    case PROP_HEIGHT:
    case PROP_ELEV:
    case PROP_WIDTH: return _tolDim;
    }
    return 0.0;   // nDuplicate and install: any change
}

std::string DeviceSampler::sourceLabel(int gate, int device, int property) const{
    char buf[128];
    const int type = get_device_source(gate, device, property, buf, 128);
    if (type == 1) return "constant";
    if (type == 2) return std::string("series:") + buf;
    if (type == 3) return buf;
    return std::string();
}

void DeviceSampler::fillContext(int gate, DeviceTransition& t) const{
    int type = 0, id = 0, comp = 0, nodeComp = 0;
    get_gate_connection(gate, type, id, comp, nodeComp);
    // as in gate_calc.f90: z1 is the stage in the water body, z2 the stage at the node
    if (type == OBJ_CHANNEL) t.zUp = get_surf_elev(comp);
    else if (type == OBJ_RESERVOIR) t.zUp = get_res_surf_elev(id);
    t.zDown = get_surf_elev(nodeComp);
    t.gateFlow = get_gate_flow(gate);
    t.contextValid = true;
}

void DeviceSampler::start(int ngate){
    _started = true;
    std::vector<GateInfo> gates;
    std::vector<DeviceInfo> devices;
    char buf[128];
    int flat = 0;
    for (int g = 1; g <= ngate; ++g){
        GateInfo gi;
        gi.id = g;
        get_gate_name(g, buf, 128);
        gi.name = buf;
        gi.nDevices = get_gate_device_count(g);
        std::ostringstream node;
        node << get_gate_node_id(g);
        gi.node = node.str();
        get_gate_object_name(g, buf, 128);
        gi.object = buf;
        gates.push_back(gi);
        for (int d = 1; d <= gi.nDevices; ++d){
            DeviceInfo di;
            di.id = ++flat;
            di.gate = g;
            di.index = d;
            get_device_name(g, d, buf, 128);
            di.name = buf;
            di.structureType = get_device_structure_type(g, d);
            devices.push_back(di);
        }
    }
    RuleLog::gates(gates, devices);
}

void DeviceSampler::check(int gate, int device, int property){
    const double value = get_device_property(gate, device, property);
    Last& last = _last[key(gate, device, property)];
    DeviceTransition t;
    t.gate = gate;
    t.device = device;
    t.property = property;
    t.newValue = value;

    if (!last.have){
        last.have = true;
        last.value = value;
        t.oldValue = value;
        t.kind = TRANSITION_INITIAL;
        t.source = sourceLabel(gate, device, property);
        if (_context) fillContext(gate, t);
        RuleLog::transition(t);
        return;
    }

    RuleLog::WriteNote note;
    const bool written = RuleLog::lastWrite(gate, device, property, note) && note.step == RuleLog::step();
    t.oldValue = last.value;
    if (written){
        if (note.kind < 0){ last.value = value; return; }     // a step in the middle of a ramp
        if (value == last.value) return;
        t.kind = note.kind;
        t.ruleId = note.ruleId;
        t.episodeId = note.episodeId;
        t.targetValue = note.target;
    }else{
        if (!(std::fabs(value - last.value) > tolerance(property))) return;
        t.kind = TRANSITION_SOURCE;
        int owner = 0;
        long episode = 0;
        if (RuleLog::sourceOwner(gate, device, property, owner, episode)){
            t.ruleId = owner;
            t.episodeId = episode;
        }
        t.source = sourceLabel(gate, device, property);
    }
    last.value = value;
    if (_context) fillContext(gate, t);
    RuleLog::transition(t);
}

void DeviceSampler::sample(){
    if (!RuleLog::enabled(RuleLog::EVENTS)) return;
    const int ngate = get_gate_count();
    if (!_started) start(ngate);
    for (int g = 1; g <= ngate; ++g){
        check(g, 0, PROP_INSTALL);
        const int ndev = get_gate_device_count(g);
        for (int d = 1; d <= ndev; ++d)
            for (int p = PROP_OP_TO_NODE; p <= PROP_NDUPLICATE; ++p) check(g, d, p);
    }
}
