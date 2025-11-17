#ifndef DSM2_NAMED_VALUE_FACTORIES_INCLUDED__
#define DSM2_NAMED_VALUE_FACTORIES_INCLUDED__
// declarations of factories for DSM2 named values
#include "oprule/expression/ExpressionNode.h"
#include "oprule/expression/ValueNode.h"
#include "oprule/parser/NamedValueLookup.h"
#include "oprule/rule/ModelInterface.h"

using oprule::parser::NamedValueLookup;




#define EXPRESSION_FACTORY(X) \
oprule::expression::DoubleNode::NodePtr \
X(const NamedValueLookup::ArgMap& argmap);

#define INTERFACE_FACTORY(X) \
oprule::expression::DoubleNode::NodePtr \
X(const NamedValueLookup::ArgMap& argmap);


////channel related
EXPRESSION_FACTORY(chan_flow_factory)
EXPRESSION_FACTORY(chan_surf_factory)
EXPRESSION_FACTORY(chan_vel_factory)


////reservoir related
EXPRESSION_FACTORY(reservoir_flow_factory)
EXPRESSION_FACTORY(reservoir_surf_factory)

EXPRESSION_FACTORY(constant0_factory)
EXPRESSION_FACTORY(constant1_factory)

EXPRESSION_FACTORY(ts_factory)

///////////Gate related
INTERFACE_FACTORY(gate_install_factory);
INTERFACE_FACTORY(device_op_factory);
INTERFACE_FACTORY(device_height_factory);
INTERFACE_FACTORY(device_nduplicate_factory);
INTERFACE_FACTORY(device_elev_factory);
INTERFACE_FACTORY(device_width_factory);
INTERFACE_FACTORY(device_coef_factory);

/////////Boundary and transfer
INTERFACE_FACTORY(external_flow_factory);
INTERFACE_FACTORY(transfer_flow_factory);







#endif