// dsm2_hydro_named_value_lookup.cpp: implementation of the DSM2HydroNamedValueLookup class.
//
//////////////////////////////////////////////////////////////////////

#include "dsm2_named_value_factories.h"
#include "dsm2_named_value_lookup.h"


#include "dsm2_model_interface.h"
#include "dsm2_model_interface_gate.h"
#include "dsm2_interface_fortran.h"

#include "oprule/expression/ExpressionNode.h"
#include "oprule/parser/NamedValueLookup.h"
#include "oprule/parser/NamedValueLookupImpl.h"

#include<string>
#include<vector>
#include "assert.h"


#define _VEC_ASSG1(_NAME,_VEC,_TYPE,S1) \
_TYPE _NAME[]={ S1}; \
_VEC.assign(_NAME,_NAME+1);

#define _VEC_ASSG2(_NAME,_VEC,_TYPE,S1,S2) \
_TYPE _NAME[]={ S1,S2 }; \
_VEC.assign(_NAME,_NAME +2);

#define _VEC_ASSG3(_NAME,_VEC,_TYPE,S1,S2,S3) \
_TYPE _NAME[]={ S1,S2,S3 }; \
_VEC.assign(_NAME,_NAME+3);


/*name,type,params,factory*/
#define ADD_EXPRESS_0ARG(_INFO,_NAME,_TYPE,_FACTORY) \
_INFO.name=#_NAME; \
_INFO.type=NamedValueLookup::_TYPE; \
_INFO.params=std::vector<std::string>(); \
_INFO.factory=_FACTORY; \
add(#_NAME,_INFO);


/*name,type,params,factory*/
#define ADD_EXPRESS_1ARG(_INFO,_NAME,_TYPE,_FACTORY,_ARG1) \
_INFO.name=#_NAME; \
_INFO.type=NamedValueLookup::_TYPE; \
_VEC_ASSG1(_NAME,_INFO.params,std::string,#_ARG1); \
_INFO.factory=_FACTORY; \
add(#_NAME,_INFO);


/*name,type,params,factory*/
#define ADD_EXPRESS_2ARG(_INFO,_NAME,_TYPE,_FACTORY,_ARG1,_ARG2) \
_INFO.name=#_NAME; \
_INFO.type=NamedValueLookup::_TYPE; \
_VEC_ASSG2(_NAME,info.params,std::string,#_ARG1,#_ARG2); \
_INFO.factory=_FACTORY; \
add(#_NAME,_INFO); 

/*name,type,params,factory*/
#define ADD_EXPRESS_3ARG(_INFO,_NAME,_TYPE,_FACTORY,_ARG1,_ARG2,_ARG3) \
_INFO.name=#_NAME; \
_INFO.type=NamedValueLookup::_TYPE; \
_VEC_ASSG3(_NAME,_INFO.params,std::string,#_ARG1,#_ARG2,#_ARG3); \
_INFO.factory=_FACTORY; \
add(#_NAME,_INFO); 


using namespace oprule::parser;

DSM2HydroNamedValueLookup::DSM2HydroNamedValueLookup(){
  ModelNameInfo info;
  ADD_EXPRESS_2ARG(info,chan_stage,READONLY,&chan_surf_factory,channel,dist);
  ADD_EXPRESS_2ARG(info,chan_flow,READONLY,&chan_flow_factory,channel,dist);
  ADD_EXPRESS_2ARG(info,chan_vel,READONLY,&chan_vel_factory,channel,dist);
  ADD_EXPRESS_2ARG(info,res_flow,READONLY,&reservoir_flow_factory,res,connect);
  ADD_EXPRESS_1ARG(info,res_stage,READONLY,&reservoir_surf_factory,res);
  ADD_EXPRESS_1ARG(info,ext_flow,READWRITE,&external_flow_factory,name);
  ADD_EXPRESS_1ARG(info,transfer_flow,READWRITE,&transfer_flow_factory,transfer);
  ADD_EXPRESS_1ARG(info,gate_install,READWRITE,&gate_install_factory,gate);
  ADD_EXPRESS_2ARG(info,gate_op,READWRITE,&device_op_factory,gate,device);
  ADD_EXPRESS_2ARG(info,gate_height,READWRITE,&device_height_factory,gate,device);
  ADD_EXPRESS_2ARG(info,gate_nduplicate,READWRITE,&device_nduplicate_factory,gate,device);
  ADD_EXPRESS_2ARG(info,gate_elev,READWRITE,&device_elev_factory,gate,device);
  ADD_EXPRESS_2ARG(info,gate_width,READWRITE,&device_width_factory,gate,device);
  ADD_EXPRESS_2ARG(info,gate_coef,READWRITE,&device_coef_factory,gate,device);
  ADD_EXPRESS_1ARG(info,ts,READONLY,&ts_factory,name);
  ADD_EXPRESS_0ARG(info,INSTALL,READONLY,&constant1_factory);
  ADD_EXPRESS_0ARG(info,REMOVE,READONLY,&constant0_factory);
  ADD_EXPRESS_0ARG(info,OPEN,READONLY,&constant1_factory);
  ADD_EXPRESS_0ARG(info,CLOSE,READONLY,&constant0_factory);
}



