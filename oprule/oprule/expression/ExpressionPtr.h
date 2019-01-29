#ifndef oprule_parser_EXPRESSIONPTR_H__INCLUDED_
#define oprule_expression_EXPRESSIONPTR_H__INCLUDED_
#include "boost/shared_ptr.hpp"
// trailing space is critical in template <> marks
#define OE_NODE_PTR(x) boost::shared_ptr<x >
#define OE_NODE_NULL(x) x()
#define OE_NODE_PTR_CREATE(x) boost::shared_ptr( x )
#define OE_NODE_DELETE(x) 
#endif
