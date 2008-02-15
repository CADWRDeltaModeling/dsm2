// dsm2_named_value_lookup.h: interface for the DSM2HydroNamedValueLookup class.
//
//////////////////////////////////////////////////////////////////////

#ifndef DSM2_NAMED_VALUE_LOOKUP_H__INCLUDED_
#define DSM2_NAMED_VALUE_LOOKUP_H__INCLUDED_

#include "oprule/parser/NamedValueLookup.h"
#include "oprule/parser/NamedValueLookupImpl.h"

#include "oprule/expression/ExpressionNode.h"
#include "dsm2_time_series_node.h"
#include "dsm2_interface_fortran.h"


class DSM2HydroNamedValueLookup : public oprule::parser::NamedValueLookupImpl  
{
public:

	DSM2HydroNamedValueLookup();

   virtual ~DSM2HydroNamedValueLookup(){}

};




#endif // !include guard
