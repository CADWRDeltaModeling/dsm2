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

/*void GateInstallInterface::setDataExpression(DoubleNodePtr express){
   set_gate_install_datasource( ndx,
						 register_express_for_data_source(express),
                         express->eval(),
                         express->isTimeDependent()); //todo: finish this up
}*/


bool GateInstallInterface::operator==( const GateInstallInterface& rhs){
   return (ndx == rhs.ndx);
}

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
    int dir=direction;
   return get_device_op_coef(ndx,devndx,direction);
}

bool DeviceOpInterface::operator==( const DeviceOpInterface& rhs){
   return ndx == rhs.ndx && devndx == rhs.ndx && direction == rhs.direction;
}





//////////////////////
void DeviceNDuplicateInterface::set(double val){
   set_device_nduplicate(ndx,devndx,val);
}


double DeviceNDuplicateInterface::eval(){
   return get_device_nduplicate(ndx,devndx);
}

void DeviceNDuplicateInterface::setDataExpression(DoubleNodePtr express){
   set_device_nduplicate_datasource( ndx, devndx,
       register_express_for_data_source(express),
      express->eval(), express->isTimeDependent()); //todo: finish this up
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

void DeviceHeightInterface::setDataExpression(DoubleNodePtr express){
   set_device_height_datasource( ndx, devndx,
       register_express_for_data_source(express),
      express->eval(), express->isTimeDependent()); //todo: finish this up
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

void DeviceElevInterface::setDataExpression(DoubleNodePtr express){
   set_device_elev_datasource( ndx, devndx,
       register_express_for_data_source(express),
      express->eval(), express->isTimeDependent()); //todo: finish this up
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

void DeviceWidthInterface::setDataExpression(DoubleNodePtr express){
   set_device_width_datasource( ndx, devndx,
       register_express_for_data_source(express),
      express->eval(), express->isTimeDependent()); //todo: finish this up
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

