#ifndef DSM2TIMESERIESNODE_H_INCLUDED
#define DSM2TIMESERIESNODE_H_INCLUDED

#include "oprule/expression/ExpressionNode.h"
#include <string>
#define STDCALL
#ifdef _WIN32
#define value_from_inputpath VALUE_FROM_INPUTPATH
#else
#define value_from_inputpath value_from_inputpath_
#endif
extern "C" double STDCALL value_from_inputpath(const int*);

class DSM2TimeSeriesNode : public oprule::expression::DoubleNode
{
public:
   typedef DSM2TimeSeriesNode NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;
   DSM2TimeSeriesNode(const std::string& tsname,
                      const int path_ndx )
      : _name(tsname),_ndx(path_ndx){
	};
	virtual ~DSM2TimeSeriesNode(){};

   static NodePtr create(const std::string& tsname,
	                     const int path_ndx){
      return NodePtr(new NodeType(tsname,
		                          path_ndx)
					 );
   }
   virtual oprule::expression::DoubleNode::NodePtr copy(){
      return NodePtr(new NodeType(_name,_ndx));
   }
   std::string getSeriesName(){ return _name; }
   virtual double eval(){
      return value_from_inputpath(&_ndx);}
   virtual bool isTimeDependent() const{ return true; }
private:
   std::string _name;
   int _ndx;
};

#endif // !include guard
