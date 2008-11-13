#ifndef oprule_parser_MODELTIMENODEFACTORY_H__INCLUDED_
#define oprule_parser_MODELTIMENODEFACTORY_H__INCLUDED_

#include "oprule/expression/ExpressionNode.h"
#include<string>
namespace oprule{
namespace parser{


/**
 * Abstract factory for building nodes having to do with time.
 */
class ModelTimeNodeFactory  
{

public:
   /** Shorthand for double node here -- rather than use it from here,
    *  probably best to duplicate the typedef in a small scope.*/
   typedef oprule::expression::DoubleNode DoubleNode;
   typedef oprule::expression::DoubleNodePtr DoubleNodePtr;

   ModelTimeNodeFactory(){};
   virtual ~ModelTimeNodeFactory(){};

   /** Get a node that represents the given reference datetime. 
    *  Datetime is in a model-compatible double format 
    *  (ie, as model ticks).
    *  Should be suitable for comparison to datetimes from the other
    *  overloaded getModelTimeNode() methods
    *  @todo is double really appropriate??
    *  @return a node that returns a reference datetime
    */ 
   virtual DoubleNodePtr getDateTimeNode(const std::string & dt,
      const std::string & tm)=0;
  
   /** Get a node that represents current model time.
    *  Should be suitable for comparison to datetimes from the other
    *  overloaded getModelTimeNode() methods
    */
   virtual DoubleNodePtr getDateTimeNode()=0;

   /** Get a node that represents current model time as ticks into the year.
    */ 
   virtual DoubleNodePtr getSeasonNode()=0;
   
   /** Get a node that expresses the given reference mon-day-hour-min 
    *date as ticks into the current model year.
    */
   virtual DoubleNodePtr getReferenceSeasonNode(int mon, int day, int hour, int min)=0;

   /** Get a node that will return the current model year as a double */
   virtual DoubleNodePtr getYearNode()=0;

   /** Get a node that will return the current model month as a double */
   virtual DoubleNodePtr getMonthNode()=0;

   /** Get a node that will return the current model day (of month) as a double */
   virtual DoubleNodePtr getDayNode()=0;

   /** Get a node that will return the current model hour (0 = midnight, 13=1PM
    *  as a double 
    */
   virtual DoubleNodePtr getHourNode()=0;
   
   /** Get a node that will return the current model minute of the day*/
   virtual DoubleNodePtr getMinOfDayNode()=0;

   /** Get a node that will return the current model minute (of the hour)*/
   virtual DoubleNodePtr getMinNode()=0;

   /** Get a node that will return the current model minute (of the hour)*/
   virtual DoubleNodePtr getTimeStepNode()=0;

   /** Find out if model step is fixed  */
   virtual bool isFixedStepSize()=0;

   //virtual double getStepSizeSeconds()=0;


};

}} //namespace
#endif // !include guard