#include "dsm2_hydrogtm_model_interface.h"
#include "dsm2_hydrogtm_named_value_factories.h"

oprule::expression::DoubleNode::NodePtr
chan_ec_factory(const NamedValueLookup::ArgMap &argmap)
{
    int chan = -901;
    double dist = -901.;
    convert_channel_identifiers(argmap, chan, dist);
    if (chan != -901 && dist != -901.)
        return ChannelECNode::create(chan, dist);
    else
        return oprule::expression::DoubleNode::NodePtr();
}