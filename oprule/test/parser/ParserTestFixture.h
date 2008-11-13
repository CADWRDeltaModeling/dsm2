#ifndef PARSER_TEST_FIXTURE_H_INCLUDED__
#define PARSER_TEST_FIXTURE_H_INCLUDED__

#include "oprule/parser/NamedValueLookup.h"
#include "oprule/parser/NamedValueLookupImpl.h"

#include "oprule/parser/ModelTimeNodeFactory.h"
#include "oprule/expression/ExpressionPtr.h"
#include<string>

using namespace oprule::parser;
using namespace oprule::rule;
using namespace oprule::expression;

void resetInterfaces();


class TestModelInterface : public oprule::rule::ModelInterface<double>
{
public:
   typedef TestModelInterface NodeType;
   typedef OE_NODE_PTR(TestModelInterface) NodeTypePtr;
   typedef ExpressionNode<double>::NodePtr BaseNodePtr;

   TestModelInterface() { 
   }

   static NodePtr create(){
      return NodePtr(new NodeType());
   }

   virtual BaseNodePtr copy(){ return create(); }

   static double val;
   virtual double eval(){return val;}
   virtual bool isTimeDependent() const{ return false;}
   virtual void set(double value){
      val=value;
   }
};

class TestModelInterface2 : public oprule::rule::ModelInterface<double>
{
public:
   typedef TestModelInterface2 NodeType;
   typedef OE_NODE_PTR(TestModelInterface) NodeTypePtr;
   typedef ExpressionNode<double>::NodePtr BaseNodePtr;      
   
   TestModelInterface2() {}

   static NodePtr create(){
      return NodePtr(new NodeType());
   }

   virtual BaseNodePtr copy(){
      return NodePtr(new NodeType());
   }

   static double val;
   virtual bool isTimeDependent() const{ return false;}
   virtual double eval(){return val;}
   virtual void set(double value){
      val=value;
   }
};

class TestModelInterface3 : public oprule::rule::ModelInterface<double>
{
public:
   typedef TestModelInterface3 NodeType;
   typedef OE_NODE_PTR(TestModelInterface) NodeTypePtr;  
   typedef ExpressionNode<double>::NodePtr BaseNodePtr;

   TestModelInterface3() {}

   static NodePtr create(){
      return NodePtr(new NodeType());
   }
   virtual BaseNodePtr copy(){
      return NodePtr(new NodeType());
   }

   static double val;
   virtual bool isTimeDependent() const{ return false;}
   virtual double eval(){return val;}
   virtual void set(double value){
      val=value;
   }
};


oprule::expression::DoubleNodePtr
expression_factory(const NamedValueLookup::ArgMap &argmap);

oprule::expression::DoubleNode::NodePtr
lookup_factory(const NamedValueLookup::ArgMap &argmap);

oprule::expression::DoubleNode::NodePtr
interface_factory(const NamedValueLookup::ArgMap &argmap);

oprule::expression::DoubleNode::NodePtr
interface2_factory(const NamedValueLookup::ArgMap &argmap);

oprule::expression::DoubleNode::NodePtr
interface3_factory(const NamedValueLookup::ArgMap &argmap);

#define _VEC_ASSG1(_NAME,_VEC,_TYPE,S1) \
_TYPE _NAME[]={ S1}; \
_VEC.assign(_NAME,_NAME+1);

#define _VEC_ASSG2(_NAME,_VEC,_TYPE,S1,S2) \
_TYPE _NAME[]={ S1,S2 }; \
_VEC.assign(_NAME,_NAME +2);

#define _VEC_ASSG3(_NAME,_VEC,_TYPE,S1,S2,S3) \
_TYPE _NAME[]={ S1,S2,S3 }; \
_VEC.assign(_NAME,_NAME+3);

class TestNamedValueLookup : public oprule::parser::NamedValueLookupImpl
{
public:   
   TestNamedValueLookup(){
   ModelNameInfo info;

   info.name="test_state";
   info.type=NamedValueLookup::READONLY;
   _VEC_ASSG2(test_state,info.params,std::string,"first_arg","second_arg");
   info.factory=&expression_factory;
   add("test_state",info);

   
   info.name="LookupName";
   info.type=NamedValueLookup::READONLY;
   info.params.clear();
   info.factory=&lookup_factory;
   add("LookupName",info);
   
   info.name="test_interface";   
   info.type=NamedValueLookup::READWRITE;
   info.factory=&interface_factory;
   _VEC_ASSG3(test_interface,info.params,std::string,"first_arg","second_arg","third_arg");
   add("test_interface",info);

   info.name="second_interface";   
   info.type=NamedValueLookup::READWRITE;
   info.factory=&interface2_factory;
   _VEC_ASSG1(second_interface,info.params,std::string,"first_arg");
   add("second_interface",info);

   info.name="third_interface";   
   info.type=NamedValueLookup::READWRITE;
   info.factory=&interface3_factory;
   _VEC_ASSG1(third_interface,info.params,std::string,"first_arg");
   add("third_interface",info);
}

   virtual ~TestNamedValueLookup(){}   
};



/**
 * Dummy ModelTimeNodeFactory that can only reproduce the date used
 * its constructor.
 */
class TrivialModelTimeNodeFactory  : public oprule::parser::ModelTimeNodeFactory
{
public:
   TrivialModelTimeNodeFactory(int yr,int mo, int dy, int hr, int mn) :
      year(yr), mon(mo), day(dy), hour(hr), min(mn) {}
   ~TrivialModelTimeNodeFactory(){};
   typedef OE_NODE_PTR(oprule::expression::DoubleNode) DNodePtr;
   typedef oprule::expression::DoubleScalarNode DScalarNode;
   virtual DNodePtr getSeasonNode(){return DScalarNode::create(0.);}
   virtual DNodePtr getReferenceSeasonNode(int _mon, int _day, int _hour, int _min){
      return DScalarNode::create(0.);}
   virtual DNodePtr getDateTimeNode(){return DScalarNode::create(0);}
   virtual DNodePtr getDateTimeNode(const std::string& dt, const std::string& tm){ 
      return DScalarNode::create(0.);}
   virtual DNodePtr getYearNode(){return DScalarNode::create(year);}
   virtual DNodePtr getMonthNode(){return DScalarNode::create(mon);}
   virtual DNodePtr getDayNode(){return DScalarNode::create(day);}
   virtual DNodePtr getHourNode(){return DScalarNode::create(hour);}
   virtual DNodePtr getMinOfDayNode(){return DScalarNode::create(min);} //@fixme:??
   virtual DNodePtr getMinNode(){return DScalarNode::create(min);} //@fixme:??
   virtual DNodePtr getTimeStepNode(){return DScalarNode::create(60.*15.);} //@fixme:??
   virtual bool  isFixedStepSize(){ return true; }
private:
   int year;
   int mon;
   int day;
   int hour;
   int min;
};


#endif