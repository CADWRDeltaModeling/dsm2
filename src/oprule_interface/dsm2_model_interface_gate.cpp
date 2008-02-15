#include "oprule/rule/ModelInterface.h"
#include "dsm2_model_interface_gate.h"
#include "dsm2_expressions.h"
#include "dsm2_interface_fortran.h"
#include<iostream>
using namespace std;
using namespace oprule::expression;
using namespace oprule::rule;
using namespace oprule::parser;


void GateInstallInterface::set(double val){
   set_gate_install(ndx, val);
}
double GateInstallInterface::eval(){
   return is_gate_install(ndx); 
}

bool GateInstallInterface::operator==( const GateInstallInterface& rhs){
   return (ndx == rhs.ndx);
}

//void GateInstallInterface::setDataExpression(DoubleNodePtr express){
//   set_gate_install_datasource( ndx, register_express_for_data_source(express),
//      express->eval(), express->isTimeDependent()); //todo: finish this up
//}
///////////////////////////

void DeviceOpInterface::set(double val){ 
   set_device_op_coef(ndx,devndx,direction,val); 
}

void DeviceOpInterface::setDataExpression(DoubleNodePtr express){
   set_device_op_datasource( ndx, 
                             devndx, 
							 direction, 
							 register_express_for_data_source(express),
      express->eval(), express->isTimeDependent()); //todo: finish this up
}

double DeviceOpInterface::eval(){
	if (direction == 0)
	{
	   throw logic_error("Gate op cannot be requested from 'both' directions");
	}
   return get_device_op_coef(ndx,devndx,direction);
}

bool DeviceOpInterface::operator==( const DeviceOpInterface& rhs){
   return ndx == rhs.ndx && devndx == rhs.ndx && direction == rhs.direction;
}



////////////////////

void DevicePositionInterface::set(double val){ 
   set_device_position(ndx,devndx,val); 
}

void DevicePositionInterface::setDataExpression(DoubleNodePtr express){
   set_device_position_datasource( ndx, devndx, register_express_for_data_source(express),
      express->eval(), express->isTimeDependent()); //todo: finish this up
}

double DevicePositionInterface::eval(){
   return get_device_position(ndx,devndx);
}

bool DevicePositionInterface::operator==( const DevicePositionInterface& rhs){
   return ndx == rhs.ndx && devndx == rhs.ndx;
}

//////////////////////
void DeviceNDuplicateInterface::set(double val){ 
   set_device_nduplicate(ndx,devndx,val); 
}


double DeviceNDuplicateInterface::eval(){
   return get_device_nduplicate(ndx,devndx);
}

bool DeviceNDuplicateInterface::operator==( const DeviceNDuplicateInterface& rhs){
   return ndx == rhs.ndx && devndx == rhs.ndx;
}


//////////////////////


void DeviceHeightInterface::set(double val){ 
   set_device_height(ndx,devndx,val); 
}

double DeviceHeightInterface::eval(){
   return get_device_height(ndx,devndx);
}

bool DeviceHeightInterface::operator==( const DeviceHeightInterface& rhs){
   return ndx == rhs.ndx && devndx == rhs.ndx;
}


//////////////////


void DeviceElevInterface::set(double val){ 
   set_device_elev(ndx,devndx,val); 
}

double DeviceElevInterface::eval(){
   return get_device_elev(ndx,devndx);
}

bool DeviceElevInterface::operator==( const DeviceElevInterface& rhs){
   return ndx == rhs.ndx && devndx == rhs.ndx;
}

//////////////////


void DeviceWidthInterface::set(double val){ 
   set_device_width(ndx,devndx,val); 
}

double DeviceWidthInterface::eval(){
   return get_device_width(ndx,devndx);
}

bool DeviceWidthInterface::operator==( const DeviceWidthInterface& rhs){
   return ndx == rhs.ndx && devndx == rhs.ndx;
}

//////////////////


void DeviceFlowCoefInterface::set(double val){
   set_device_flow_coef(ndx,devndx,direction,val); 
}

double DeviceFlowCoefInterface::eval(){
   return get_device_flow_coef(ndx,devndx,direction);
}

bool DeviceFlowCoefInterface::operator==( const DeviceFlowCoefInterface& rhs){
   return ndx == rhs.ndx && devndx == rhs.ndx;
}

