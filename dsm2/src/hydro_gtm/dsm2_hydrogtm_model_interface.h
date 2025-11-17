#ifndef DSM2_HYDROGTM_MODEL_INTERFACE_H
#define DSM2_HYDROGTM_MODEL_INTERFACE_H
#pragma warning(disable : 4786)

#include "oprule/expression/ExpressionNode.h"
#include "oprule/expression/ExpressionPtr.h"
#include "oprule/rule/ModelInterface.h"

class ChannelECNode : public oprule::expression::ExpressionNode<double>
{
public:
    typedef ChannelECNode NodeType;
    typedef OE_NODE_PTR(NodeType) NodePtr;

    ChannelECNode(const int intchan,
                  const double dist);

    static NodePtr create(const int intchan, const double dist)
    {
        return NodePtr(new NodeType(intchan, dist));
    }
    virtual oprule::expression::DoubleNodePtr copy()
    {
        return NodePtr(new NodeType(channel, distance));
    }
    virtual double eval();
    virtual bool isTimeDependent() const { return true; }

private:
    int channel;
    double distance;
};

#endif
