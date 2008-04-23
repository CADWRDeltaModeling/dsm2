#include "dsm2_named_value_factories.h"
#include "dsm2_model_interface.h"
#include "dsm2_model_interface_gate.h"
#include "dsm2_interface_fortran.h"
#include "dsm2_time_series_node.h"
#include "oprule/expression/ExpressionNode.h"
#include "oprule/parser/ModelNameParseError.h"
#include<string>
#include "boost/shared_ptr.hpp"
#include "assert.h"
using namespace std;
using namespace oprule::expression;
using namespace oprule::rule;

string lowerCase(const string& s) {
   char* buf = new char[s.length()];
   s.copy(buf,s.length());
   for (int i=0; i<s.length(); i++){
      buf[i] = tolower(buf[i]);
   }
   std::string r(buf,s.length());
   delete buf;
   return r;
}

void convert_channel_identifiers(
            const NamedValueLookup::ArgMap & argmap, 
            int &intchan, 
            double &dist){
   
   NamedValueLookup::ArgMap::const_iterator iter
       = argmap.find("channel");

   if(iter == argmap.end()){
	   throw oprule::parser::MissingIdentifier("Channel not identified");
      return;
   }
   std::string chanarg=iter->second;
   int extchan=atoi(chanarg.c_str());
   intchan=ext2int(extchan);
   iter = argmap.find("dist");
   if (iter == argmap.end() ) throw oprule::parser::MissingIdentifier(
	   "Channel distance (e.g., dist=2311 or dist=length) not given for channel.");
   else{
  	    double chanlen=channel_length(intchan);
        const std::string adist("dist");
        // here we mean "length" as a surrogate for length of channel,
        // ie., argmap["dist"]="length"
        if (lowerCase(iter->second)=="length"){  
           dist = chanlen;  //@todo: test this 
        }else{
           dist=atof(iter->second.c_str());
		   if (dist > chanlen || dist <0.) 
			   throw oprule::parser::InvalidIdentifier("Channel dist argument greater than length or negative");
        }
     }
     return;
}


oprule::expression::DoubleNode::NodePtr 
chan_flow_factory(const NamedValueLookup::ArgMap& argmap){
  int chan=-901; 
  double dist=-901.;
  convert_channel_identifiers(argmap, chan, dist);
  if (chan != -901 && dist != -901.) return 
     ChannelFlowNode::create(chan,dist);
  else return oprule::expression::DoubleNode::NodePtr();
}

oprule::expression::DoubleNode::NodePtr 
chan_surf_factory(const NamedValueLookup::ArgMap& argmap){
  int chan=-901; 
  double dist=-901.;
  convert_channel_identifiers(argmap, chan,dist);
  if (chan != -901 && dist != -901.) return 
     ChannelWSNode::create(chan,dist);
  else return oprule::expression::DoubleNode::NodePtr();
}

oprule::expression::DoubleNode::NodePtr 
chan_vel_factory(const NamedValueLookup::ArgMap& argmap){
  int chan=-901; 
  double dist=-901.;
  convert_channel_identifiers(argmap, chan,dist);
  if (chan != -901 && dist != -901.) return 
     ChannelVelocityNode::create(chan,dist);
  else return oprule::expression::DoubleNode::NodePtr();
}


void convert_reservoir_identifiers(
            const NamedValueLookup::ArgMap & argmap, 
            int &resno, 
            int &connect,
			bool conn_requested){
   
   NamedValueLookup::ArgMap::const_iterator iter
       = argmap.find("res");

   if(iter == argmap.end()){
	   throw oprule::parser::MissingIdentifier("Reservoir (res=name) identifier not provided");
   }

   string name=iter->second;
   resno=reservoir_index(name.c_str(),name.length());
   iter = argmap.find("connect");
   if (iter == argmap.end() ){
	   if (conn_requested){
 	     throw oprule::parser::MissingIdentifier(
			 "Reservoir (res=name) identifier not provided");
	   }else{
	     connect=-901;
	   }
   }else{ 
	   std::string resarg=iter->second;
       int node=atoi(resarg.c_str());
	   connect=ext2intnode(node);
	   if (connect == -901 && conn_requested)
		   throw oprule::parser::InvalidIdentifier("Requested reservoir connection (node number) not found: " + resarg);
   }
}


oprule::expression::DoubleNode::NodePtr 
reservoir_surf_factory(const NamedValueLookup::ArgMap& argmap){
  int reservoir=-901; 
  int connect=-901;
  convert_reservoir_identifiers(argmap, reservoir, connect, false);
  if (reservoir != -901) return 
     ReservoirWSNode::create(reservoir);
  else return oprule::expression::DoubleNode::NodePtr();
}

oprule::expression::DoubleNode::NodePtr 
reservoir_flow_factory(const NamedValueLookup::ArgMap& argmap){
  int reservoir=-901; 
  int connect=-901;
  convert_reservoir_identifiers(argmap, reservoir, connect, true);
  if (reservoir != -901 && connect != -901) return 
     ReservoirFlowNode::create(reservoir,connect);
  else return oprule::expression::DoubleNode::NodePtr();
}


//////////////////////////////////////////




void convert_gate_identifiers(
            const NamedValueLookup::ArgMap & argmap, 
            int &gatendx, 
            int &devndx){
   
   NamedValueLookup::ArgMap::const_iterator iter
       = argmap.find("gate");
   if(iter == argmap.end()){
	 throw oprule::parser::MissingIdentifier("Gate name not given");
     return;
   }
   string name=iter->second;
   gatendx=gate_index(name.c_str(),name.length());
   if(gatendx == -901)
       throw oprule::parser::InvalidIdentifier("Gate name not found: "+ name);
   iter = argmap.find("device");
   if (iter == argmap.end() ) {
	   devndx=-901;
   }else{
      string device=iter->second;
      devndx=device_index(gatendx,device.c_str(),device.length());
	  if(devndx == -901){
	     string message = "Gate " + name + ", Device " + device + " not found.";
		 throw oprule::parser::InvalidIdentifier(message);

	  }
   }
}


oprule::expression::ExpressionNode<double>::NodePtr
gate_install_factory(const NamedValueLookup::ArgMap& argmap){
  int gatendx=-901; 
  int devndx=-901;
  convert_gate_identifiers(argmap, gatendx, devndx);
  return GateInstallInterface::create(gatendx);
}

#define GATE_DEV_FACTORY(_FACTORYNAME,_IFNAME) \
oprule::expression::ExpressionNode<double>::NodePtr  \
_FACTORYNAME(const NamedValueLookup::ArgMap& argmap){ \
  int gatendx=-901;                                   \
  int devndx=-901;                                    \
  convert_gate_identifiers(argmap, gatendx, devndx);  \
  if (gatendx != -901 && devndx != -901) return       \
     _IFNAME::create(gatendx,devndx);                         \
  else throw oprule::parser::InvalidIdentifier("Gate or device not found"); \
}


//@todo: fix next
GATE_DEV_FACTORY(device_position_factory, DevicePositionInterface);
GATE_DEV_FACTORY(device_nduplicate_factory, DeviceNDuplicateInterface);
GATE_DEV_FACTORY(device_width_factory, DeviceWidthInterface);
GATE_DEV_FACTORY(device_elev_factory, DeviceElevInterface);

oprule::expression::ExpressionNode<double>::NodePtr
device_coef_factory(const NamedValueLookup::ArgMap& argmap){
  int gatendx=-901; 
  int devndx=-901;
  int direct=0;
  convert_gate_identifiers(argmap, gatendx, devndx);
  NamedValueLookup::ArgMap::const_iterator iter
       = argmap.find("direction");
  if(iter == argmap.end()){
     throw oprule::parser::MissingIdentifier("Flow direction not specified");
  }else{ 
     string dstr=iter->second;
     if (dstr == "to_node") direct=direct_to_node();
     else if (dstr == "from_node") direct=direct_from_node();
	 else if (dstr == "both") direct=direct_to_from_node();
     else throw oprule::parser::InvalidIdentifier("Illegal op direction: " + dstr);
  }

  if (gatendx != -901 && devndx != -901 && direct!= -901) return 
    DeviceFlowCoefInterface::create(gatendx,devndx,direct);
  else return oprule::expression::DoubleNode::NodePtr();
}



////////
oprule::expression::ExpressionNode<double>::NodePtr
device_op_factory(const NamedValueLookup::ArgMap& argmap){
  int gatendx=-901; 
  int devndx=-901;
  int direct=0;
  convert_gate_identifiers(argmap, gatendx, devndx);
  NamedValueLookup::ArgMap::const_iterator iter
       = argmap.find("direction");
  if(iter == argmap.end()){
     throw oprule::parser::MissingIdentifier("Flow direction not specified");
  }else{ 
     string dstr=iter->second;
     if (dstr == "to_node"){ 
		 direct=direct_to_node();
	 }else if (dstr == "from_node") {
		 direct=direct_from_node();
	 }else if (dstr == "to_from_node" || dstr == "bidir") {
		 direct=direct_to_from_node();
	 }else throw oprule::parser::InvalidIdentifier("Illegal op direction: " + dstr);
  }

  if (gatendx != -901 && devndx != -901 && direct!= -901) return 
     DeviceOpInterface::create(gatendx,devndx,direct);
  else throw oprule::parser::InvalidIdentifier("Unknown gate and device");
}



///////////////////////////////////////
oprule::expression::ExpressionNode<double>::NodePtr 
external_flow_factory(const NamedValueLookup::ArgMap& argmap){
   NamedValueLookup::ArgMap::const_iterator iter
       = argmap.find("name");
  if(iter == argmap.end()){
     cerr << "name missing" << endl;
     throw oprule::parser::MissingIdentifier("external flow name not specified");
  }else{ 
     string name=iter->second;
     int ndx=qext_index(name.c_str(),name.length());
     if (ndx > 0) return 
       ExternalFlowInterface::create(ndx);
     else throw oprule::parser::InvalidIdentifier("Unknown external flow: "+name);
  }
}

oprule::expression::ExpressionNode<double>::NodePtr 
transfer_flow_factory(const NamedValueLookup::ArgMap& argmap){
   NamedValueLookup::ArgMap::const_iterator iter
       = argmap.find("transfer");
  if(iter == argmap.end()){
	 throw oprule::parser::MissingIdentifier("transfer argument not supplied in transfer_flow(..)");   //@todo better error
  }else{ 
     string name=iter->second;
     int ndx=transfer_index(name.c_str(),name.length());
     if (ndx > 0) return TransferFlowInterface::create(ndx);
     throw oprule::parser::InvalidIdentifier("transfer argument not matched in transfer_flow(..) "+name); 
  }
}

oprule::expression::ExpressionNode<double>::NodePtr 
ts_factory(const NamedValueLookup::ArgMap& argmap){
   NamedValueLookup::ArgMap::const_iterator iter
       = argmap.find("name");
  if(iter == argmap.end()){
     throw oprule::parser::MissingIdentifier("transfer argument not supplied in transfer_flow(..)");   //@todo better error
  }else{
	 string name=iter->second;
     int pathindex=ts_index(name.c_str(),name.length());
     if (pathindex > -1){ 
         return DSM2TimeSeriesNode::create(name,pathindex);
      }
     throw oprule::parser::InvalidIdentifier("Transfer name not found"+name); 

   }
}

oprule::expression::ExpressionNode<double>::NodePtr 
constant0_factory(const NamedValueLookup::ArgMap& argmap){
	return oprule::expression::DoubleScalarNode::create(0.);
}

oprule::expression::ExpressionNode<double>::NodePtr 
constant1_factory(const NamedValueLookup::ArgMap& argmap){
	return oprule::expression::DoubleScalarNode::create(1.);
}