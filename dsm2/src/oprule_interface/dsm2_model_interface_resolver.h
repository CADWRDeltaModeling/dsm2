#ifndef DSM2_MODEL_INTERFACE_RESOLVER_H_52617__INCLUDED_
#define DSM2_MODEL_INTERFACE_RESOLVER_H_52617__INCLUDED_

#include "MultiMethods.h"
#include "dsm2_model_interface.h"
#include "dsm2_model_interface_gate.h"


class DSM2ModelInterfaceResolver{
public:
   bool Fire(const ExternalFlowInterface & lhs, const ExternalFlowInterface & rhs){
        return lhs.ndx == rhs.ndx;
   }
   bool Fire(const ExternalFlowInterface & lhs, 
             const oprule::rule::ModelInterface<double> & rhs){
        return false;
   }
   bool Fire(const TransferFlowInterface & lhs, const TransferFlowInterface & rhs){
        return lhs.ndx == rhs.ndx;
   }
   bool Fire(const TransferFlowInterface & lhs, 
             const oprule::rule::ModelInterface<double> & rhs){
        return false;
   }
   bool Fire(const GateInstallInterface & lhs, const GateInstallInterface &rhs){
      return lhs.ndx == rhs.ndx;
   }
   bool Fire(const GateInstallInterface & lhs, const DeviceInterface &rhs){
      return lhs.ndx == rhs.ndx;
   }
   bool Fire(const GateInstallInterface & lhs, 
             const oprule::rule::ModelInterface<double> &rhs){
      return false;
   }
   bool Fire(const DeviceInterface & lhs, const DeviceInterface &rhs){
      return lhs.ndx == rhs.ndx && lhs.devndx == rhs.devndx;
   }
   bool Fire(const DeviceInterface & lhs, 
             const oprule::rule::ModelInterface<double> &rhs){
      return false;
   }
   bool OnError(const oprule::rule::ModelInterface<double>& lhs, 
                const oprule::rule::ModelInterface<double>& rhs){
      std::cerr<< " Default"<< std::endl;
      return false;
   }
                
};

typedef ::Loki::StaticDispatcher<
        DSM2ModelInterfaceResolver,
        oprule::rule::ModelInterface<double>,
        LOKI_TYPELIST_3(GateInstallInterface,DeviceInterface,ExternalFlowInterface),
        true,
        oprule::rule::ModelInterface<double>,
        LOKI_TYPELIST_3(GateInstallInterface,DeviceInterface,ExternalFlowInterface),
        bool > DSM2Resolver;


#endif //include guard