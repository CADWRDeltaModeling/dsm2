#ifndef DSM2_MODEL_INTERFACE_H_87Y_INCLUDED
#define DSM2_MODEL_INTERFACE_H_87Y_INCLUDED
#pragma warning(disable:4786)

/**
 * classes that implement ModelInterface (read and write-able variables)
 * or ExpressionNode (read only) interfaces
 * for DSM2 model variables
 */


#include <assert.h>

#include<string>
#include<vector>
#include "oprule/expression/ExpressionNode.h"
#include "oprule/expression/ExpressionPtr.h"

#include "oprule/rule/ModelInterface.h"

class DSM2ModelInterfaceResolver;


class ExternalFlowInterface : public oprule::rule::ModelInterface<double> {
public:
   typedef ExternalFlowInterface NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;

   friend class DSM2ModelInterfaceResolver;
   ExternalFlowInterface(const int index);
   static NodePtr create(const int index){
      return NodePtr(new NodeType(index));
   }
   virtual oprule::expression::DoubleNodePtr copy(){
      return NodePtr(new NodeType(ndx));
   }
   virtual double eval();
   virtual void set(double val);
   virtual bool isTimeDependent() const{ return true; }
   virtual void setDataExpression(oprule::expression::DoubleNodePtr express);
   virtual bool operator==( const ExternalFlowInterface& rhs);
private:
   int ndx;
};


class TransferFlowInterface : public oprule::rule::ModelInterface<double> {
public:
   typedef TransferFlowInterface NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;

   friend class DSM2ModelInterfaceResolver;
   TransferFlowInterface(const int index);
   static NodePtr create(int index){
      return NodePtr(new NodeType(index));
   }
   virtual oprule::expression::DoubleNodePtr copy(){
      return NodePtr(new NodeType(ndx));
   }

   virtual double eval();
   virtual void set(double val);
   virtual bool isTimeDependent() const{ return true; }
   virtual void setDataExpression(oprule::expression::DoubleNodePtr express);
   virtual bool operator==( const TransferFlowInterface& rhs);
private:
   int ndx;
};

class ChannelECNode : public oprule::expression::ExpressionNode<double> {
public:
   typedef ChannelECNode NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;

   ChannelECNode(const int intchan,
                   const double dist);

   static NodePtr create(const int intchan, const double dist){
      return NodePtr(new NodeType(intchan,dist));
   }
   virtual oprule::expression::DoubleNodePtr copy(){
      return NodePtr(new NodeType(channel,distance));
   }
   virtual double eval();
   virtual bool isTimeDependent() const{ return true; }
private:
   int channel;
   double distance;
};

class ChannelFlowNode : public oprule::expression::ExpressionNode<double> {
public:
   typedef ChannelFlowNode NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;

   ChannelFlowNode(const int intchan,
                   const double dist);

   static NodePtr create(const int intchan, const double dist){
      return NodePtr(new NodeType(intchan,dist));
   }
   virtual oprule::expression::DoubleNodePtr copy(){
      return NodePtr(new NodeType(channel,distance));
   }
   virtual double eval();
   virtual bool isTimeDependent() const{ return true; }
private:
   int channel;
   int distance;
   int upCompPt;
   int downCompPt;
   double upWt;
   double downWt;
};



class ChannelWSNode : public oprule::expression::ExpressionNode<double> {
public:
   typedef ChannelWSNode NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;

   ChannelWSNode(const int intchan,
                 const double dist );
   static NodePtr create(const int intchan, const double dist){
      return NodePtr(new NodeType(intchan, dist));
   }
   virtual oprule::expression::DoubleNodePtr copy(){
      return NodePtr(new NodeType(channel,distance));
   }
   double eval();
   virtual bool isTimeDependent() const{ return true; }
private:
   int channel;
   int distance;
   int upCompPt;
   int downCompPt;
   double upWt;
   double downWt;
};

class ChannelVelocityNode : public oprule::expression::ExpressionNode<double> {
public:
   typedef ChannelVelocityNode NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;

   ChannelVelocityNode(const int intchan,
                       const double dist );
   static NodePtr create(const int intchan, const double dist){
      return NodePtr(new NodeType(intchan,dist));
   }
   virtual oprule::expression::DoubleNodePtr copy(){
      return NodePtr(new NodeType(channel,distance));
   }
   double eval();
   virtual bool isTimeDependent() const{ return true; }
private:
   int upCompPt;
   int downCompPt;
   double upWt;
   double downWt;
   double distance;  //todo: get rid of channel and distance and improve the calc of velocity
   int channel;
};

class ReservoirFlowNode : public oprule::expression::ExpressionNode<double> {
public:
   typedef ReservoirFlowNode NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;

   ReservoirFlowNode( const int res,
                      const int connect);
   static NodePtr create(const int res, const int connect){
      return NodePtr(new NodeType(res,connect));
   }
   virtual oprule::expression::DoubleNodePtr copy(){
      return NodePtr(new NodeType(_res,_conn));
   }
   double eval();
   virtual bool isTimeDependent() const{ return true; }
private:
   int _res;
   int _conn;
};

class ReservoirWSNode : public oprule::expression::ExpressionNode<double>{
public:
   typedef ReservoirWSNode NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;

   ReservoirWSNode(const int res);
   static NodePtr create(int res){
      return NodePtr(new NodeType(res));
   }
   virtual oprule::expression::DoubleNodePtr copy(){
      return NodePtr(new NodeType(_res));
   }
   double eval();
   virtual bool isTimeDependent() const{ return true; }
private:
   int _res;
};




#endif
