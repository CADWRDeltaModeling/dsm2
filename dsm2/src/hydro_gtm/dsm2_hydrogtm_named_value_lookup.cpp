#include"dsm2_hydrogtm_named_value_factories.h"
#include "dsm2_hydrogtm_named_value_lookup.h"

#include "oprule/parser/NamedValueLookup.h"
#include "oprule/parser/NamedValueLookupImpl.h"

#define _VEC_ASSG2(_NAME,_VEC,_TYPE,S1,S2) \
_TYPE _NAME[]={ S1,S2 }; \
_VEC.assign(_NAME,_NAME +2);

/*name,type,params,factory*/
#define ADD_EXPRESS_2ARG(_INFO, _NAME, _TYPE, _FACTORY, _ARG1, _ARG2) \
    _INFO.name = #_NAME;                                              \
    _INFO.type = NamedValueLookup::_TYPE;                             \
    _VEC_ASSG2(_NAME, info.params, std::string, #_ARG1, #_ARG2);      \
    _INFO.factory = _FACTORY;                                         \
    add(#_NAME, _INFO);

using namespace oprule::parser;

DSM2HydrogtmNamedValueLookup::DSM2HydrogtmNamedValueLookup() : DSM2HydroNamedValueLookup()
{
    ModelNameInfo info;
    ADD_EXPRESS_2ARG(info, chan_ec, READONLY, &chan_ec_factory, channel, dist);
}