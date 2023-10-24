#ifndef DSM2_TIME_INTERFACE_H_RT3YJ_INCLUDED
#define DSM2_TIME_INTERFACE_H_RT3YJ_INCLUDED
#pragma warning(disable:4786)


#include "oprule/expression/ExpressionNode.h"
#include "oprule/rule/ModelInterface.h"
#include "dsm2_time_interface_fortran.h"

//////// Classes which retrieve model time
#define TIMECLASS(c_name,c_getter)                     \
class c_name : public oprule::expression::DoubleNode{  \
public:                                                \
   typedef c_name NodeType;                            \
   typedef OE_NODE_PTR(NodeType) NodePtr;              \
   static NodePtr create(){ return NodePtr(new NodeType());} \
   virtual oprule::expression::DoubleNode::NodePtr copy(){ \
      return NodePtr(new NodeType());\
   } \
   virtual double eval(){ return (double) c_getter(); }\
   virtual bool isTimeDependent() const{return true;}  \
}\

TIMECLASS(DSM2HydroYearNode,get_model_year);
TIMECLASS(DSM2HydroSeasonNode, get_model_minute_of_year); //???
TIMECLASS(DSM2HydroMonthNode,get_model_month);
TIMECLASS(DSM2HydroDayNode,get_model_day);
TIMECLASS(DSM2HydroHourNode,get_model_hour);
TIMECLASS(DSM2HydroMinuteNode,get_model_minute);
TIMECLASS(DSM2HydroMinuteOfDayNode,get_model_minute_of_day);
TIMECLASS(DSM2HydroDateTimeNode,get_model_ticks);
TIMECLASS(DSM2HydroTimeStepNode,time_step_seconds);


///////// Class which expresses a fixed reference time in a way
//        that can be compared to a model time retrieved above
class DSM2HydroReferenceSeasonNode : public oprule::expression::DoubleNode{
public:
   typedef DSM2HydroReferenceSeasonNode NodeType;
   typedef OE_NODE_PTR(NodeType) NodePtr;

   DSM2HydroReferenceSeasonNode(int mon, int day, int hour, int min) :
      _mon(mon),_day(day),_hour(hour),_min(min){}
   static NodePtr create(const int mo, const int d,
                         const int hr, const int min){
      return NodePtr(new NodeType(mo,d,hr,min));}
   virtual oprule::expression::DoubleNode::NodePtr copy(){
      return NodePtr(new NodeType(_mon,_day,_hour,_min));
   }


   virtual double eval(){ return (double) get_reference_minute_of_year(_mon,_day,_hour,_min); }
   virtual bool isTimeDependent() const{return true;}
private:
   int _mon,_day,_hour,_min;
};



#endif
