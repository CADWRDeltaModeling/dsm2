#ifndef DSM2_TIME_NODE_FACTORY_H_INCLUDED
#define DSM2_TIME_NODE_FACTORY_H_INCLUDED

#include "oprule/parser/ModelTimeNodeFactory.h"
#include "dsm2_time_interface.h"
#include "dsm2_time_interface_fortran.h"
#include <string>
#include "oprule/expression/ValueNode.h"
#include <iostream> //print statements
#include <assert.h>

class DSM2HydroTimeNodeFactory : public oprule::parser::ModelTimeNodeFactory
{
public:
   
   virtual oprule::expression::DoubleNodePtr getDateTimeNode(){
      return new DSM2HydroDateTimeNode();
   }

   virtual oprule::expression::DoubleNodePtr getDateTimeNode(
               const std::string& dt, const std::string& tm){
      std::string adjtime(tm);
      if(adjtime.length() == 5){
         assert (adjtime.substr(2,1) == ":");
         adjtime=adjtime.erase(2,1);
      }else{
         assert(adjtime.length()==4);
      }
      std::string datetime(dt);
      datetime += " ";
      datetime += adjtime;
      int juldt=cdate_to_jul_min(datetime.c_str(),datetime.length());
      return new oprule::expression::DoubleScalarNode((double) juldt);
   }

   virtual oprule::expression::DoubleNodePtr getSeasonNode(){ 
      return DSM2HydroSeasonNode::create(); 
   }

   virtual oprule::expression::DoubleNodePtr getReferenceSeasonNode(int mon, int day, int hour, int min){
      return DSM2HydroReferenceSeasonNode::create(mon,day,hour,min);
   }

   virtual oprule::expression::DoubleNodePtr getYearNode(){ 
      return DSM2HydroYearNode::create(); 
   }
   virtual oprule::expression::DoubleNodePtr getMonthNode(){
      return DSM2HydroMonthNode::create(); 
   }
   virtual oprule::expression::DoubleNodePtr getDayNode(){
      return DSM2HydroDayNode::create(); 
   }
   virtual oprule::expression::DoubleNodePtr getHourNode(){
      return DSM2HydroHourNode::create(); 
   } 
   virtual oprule::expression::DoubleNodePtr getMinOfDayNode(){
      return DSM2HydroMinuteOfDayNode::create(); 
   }
   virtual oprule::expression::DoubleNodePtr getMinNode(){
      return DSM2HydroMinuteNode::create(); }

   virtual bool isFixedStepSize(){ return true; }

   virtual double getStepSizeSeconds(){ return (double) time_step_seconds(); }
};


#endif
