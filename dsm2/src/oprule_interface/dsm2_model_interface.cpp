#include "dsm2_model_interface.h"
#include "dsm2_expressions.h"
#include "dsm2_interface_fortran.h"
#include "oprule/expression/ExpressionNode.h"

using namespace std;
using namespace oprule::expression;
using namespace oprule::rule;

ExternalFlowInterface::ExternalFlowInterface(const int index) : ndx(index){}

double ExternalFlowInterface::eval(){ return get_external_flow(ndx); }
void ExternalFlowInterface::set(double val){ set_external_flow(ndx,val);}
void ExternalFlowInterface::setDataExpression(DoubleNode::NodePtr express){
   set_external_flow_datasource( ndx, register_express_for_data_source(express),
   express->eval(), express->isTimeDependent());
}

bool ExternalFlowInterface::operator==( const ExternalFlowInterface& rhs){
   return (ndx == rhs.ndx);
}

//////////////

TransferFlowInterface::TransferFlowInterface(const int index) : ndx(index){}
double TransferFlowInterface::eval(){ return get_transfer_flow(ndx); }
void TransferFlowInterface::set(double val){ set_transfer_flow(ndx,val);}
void TransferFlowInterface::setDataExpression(DoubleNode::NodePtr express){
   set_transfer_flow_datasource( ndx, register_express_for_data_source(express),
      express->eval(), express->isTimeDependent());
}

bool TransferFlowInterface::operator==( const TransferFlowInterface& rhs){
   return (ndx == rhs.ndx);
}


//////////////////

ChannelFlowNode::ChannelFlowNode(
                   const int intchan, 
				   const double dist) :
  channel(intchan),
  distance(dist)
  {
  int points[2];
  double weights[2];
  chan_comp_point(intchan, dist, points, weights);
  upCompPt=points[0]; 
  downCompPt=points[1];
  upWt=weights[0];
  downWt=weights[1];
}

inline double ChannelFlowNode::eval(){
  return upWt*get_flow(upCompPt)+downWt*get_flow(downCompPt);
}

   
ChannelWSNode::ChannelWSNode(
                 const int intchan, 
                 const double dist):
  channel(intchan),
  distance(dist)
{
  int points[2];
  double weights[2];
  chan_comp_point(intchan, dist, points, weights);
  upCompPt=points[0]; 
  downCompPt=points[1];
  upWt=weights[0];
  downWt=weights[1];
}

double ChannelWSNode::eval(){
  return upWt*get_surf_elev(upCompPt)
         +downWt*get_surf_elev(downCompPt);
}
////////////////
//fixme: this stinks, but keep it for now for comparison to old op rules
ChannelVelocityNode::ChannelVelocityNode(
                 const int intchan, 
                 const double dist) : 
      channel(intchan), 
	  distance(dist) 
{
  int points[2];
  double weights[2];
  chan_comp_point(intchan, dist, points, weights);
  upCompPt=points[0]; 
  downCompPt=points[1];
  upWt=weights[0];
  downWt=weights[1];      
}

double ChannelVelocityNode::eval(){
  return get_chan_velocity(channel,distance);
}
////////////////

ReservoirFlowNode::ReservoirFlowNode( 
                      const int res, 
                      const int connect)  
                      : _res(res), 
					    _conn(connect){
}

double ReservoirFlowNode::eval(){
  return get_res_flow(_res, _conn);
}


ReservoirWSNode::ReservoirWSNode(const int res) 
 : _res(res){}

inline double ReservoirWSNode::eval(){ return get_res_surf_elev(_res); }
