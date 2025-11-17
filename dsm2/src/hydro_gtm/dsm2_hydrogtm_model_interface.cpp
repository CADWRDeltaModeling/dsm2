#include "dsm2_hydrogtm_model_interface.h"
#include "dsm2_expressions.h"
#include "dsm2_hydrogtm_interface_fortran.h"
#include "oprule/expression/ExpressionNode.h"

ChannelECNode::ChannelECNode(
    const int intchan,
    const double dist) : channel(intchan),
                         distance(dist)
{
    int points[2];
    double weights[2];
}
inline double ChannelECNode::eval()
{
    double temp = chan_ec_val(channel, distance);
    return chan_ec_val(channel, distance);
}
