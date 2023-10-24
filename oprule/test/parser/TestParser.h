#include "oprule/parser/ModelActionFactory.h"
#include "oprule/parser/NamedValueLookup.h"
#include "oprule/parser/ModelTimeNodeFactory.h"
#include<string>

using namespace oprule::parser;
using namespace oprule::rule;
using namespace oprule::expression;

class TestModelInterface : public oprule::rule::ModelInterface<double>
{
public:
   TestModelInterface() {
      //cout << "creating TestModelInterface" << endl;
   }
   static double val;
   virtual double eval(){return val;}
   virtual bool isStaticParameter() const{return true;}
   virtual bool isTimeDependent() const{ return false;}
   virtual void set(double value){
      val=value;
      //cout << "setting val " << value << " with TestModelInterface" << endl;
   }
};
double TestModelInterface::val=-17;

class TestModelInterface2 : public oprule::rule::ModelInterface<double>
{
public:
   TestModelInterface2() {
      //cout << "creating TestModelInterface2" << endl;
   }
   static double val;
   virtual bool isStaticParameter() const{return true;}
   virtual bool isTimeDependent() const{ return false;}
   virtual double eval(){return val;}
   virtual void set(double value){
      val=value;
      //cout << "setting val " << value << " with TestModelInterface2" << endl;
   }
};
double TestModelInterface2::val=-18;

void resetInterfaces(){
   TestModelInterface::val=-17;
   TestModelInterface2::val=-18;
}

class TestNamedValueLookup : public oprule::parser::NamedValueLookup
{
public:
   virtual oprule::expression::DoubleNodePtr getNamedValue(
        const std::string& name){
      if (name=="Lookup") return new oprule::expression::ValueNode<double>(2.);
      else return 0;
   }

   virtual bool isModelName(const std::string& name){
      return (name=="test_state");
   }

   virtual oprule::expression::DoubleNodePtr getModelVariableNode(
       const ArgListType& identifiers){
      if (  identifiers[0]=="test_state"
         && identifiers[1]=="first_arg"
         && identifiers[2]=="4"
         && identifiers[3] == "second_arg"
         && identifiers[4] == "quoted string"
         ){
         return new oprule::expression::DoubleScalarNode(2.);
      }else{
         cout << "Something stinks" << identifiers[0] << " "<<
         identifiers[1] << " " << identifiers[2] << endl;
         return 0;
      }
   }

   virtual ModelInterface<double> * getModelInterface(const ArgListType& identifiers){
      if (  identifiers[0]=="test_interface"
         && identifiers[1]=="first_arg"
         && identifiers[2]=="quoted string with spaces"
         && identifiers[3] == "second_arg"
         && identifiers[4] == "0"
         && identifiers[5] == "third_arg"
         && identifiers[6] == "unquoted"
         ){ return new TestModelInterface();}
      else{
         if (identifiers[0]=="second_interface"
         && identifiers[1]=="first_arg"
         && identifiers[2]=="0"){
           return new TestModelInterface2();
         }else{
           cout << "Something stinks" << identifiers[0] << " "<<
           identifiers[1] << " " << identifiers[2] << endl;
           return 0;
         }
      }
   }
};



/**
 * Dummy ModelTimeNodeFactory that can only reproduce the date used
 * its constructor.
 */
class TrivialModelTimeNodeFactory  : public oprule::parser::ModelTimeNodeFactory
{
public:
   TrivialModelTimeNodeFactory(int yr,int mo, int dy, int hr=0, int mn=0) :
      year(yr), mon(mn), day(dy), hour(hr), min(mn) {}
   ~TrivialModelTimeNodeFactory(){};
   typedef oprule::expression::DoubleNode DNode;
   typedef oprule::expression::DoubleScalarNode DScalarNode;
   virtual DNode * getSeasonNode(){return new DScalarNode(0.);}
   virtual DNode * getReferenceSeasonNode(int _mon, int _day, int _hour, int _min){
      return new DScalarNode(0.);}
   virtual DNode * getDateTimeNode(){return new DScalarNode(0);}
   virtual DNode * getDateTimeNode(const string& dt, const string& tm){ return new DScalarNode(0.);}
   virtual DNode * getYearNode(){return new DScalarNode(year);}
   virtual DNode * getMonthNode(){return new DScalarNode(mon);}
   virtual DNode * getDayNode(){return new DScalarNode(day);}
   virtual DNode * getHourNode(){return new DScalarNode(hour);}
   virtual DNode * getMinOfDayNode(){return new DScalarNode(min);}
   virtual DNode * getMinNode(){return new DScalarNode(min);}
   virtual bool  isFixedStepSize(){ return true; }
   virtual double getStepSizeSeconds(){ return 2.; }
private:
   int year;
   int mon;
   int day;
   int hour;
   int min;
};


typedef int TestingTimeRep;

class TestingModelTimer{
public:
   typedef TestingTimeRep TimeType;
   typedef TestingTimeRep DurationType;
   enum TimeConstants {
     MinPerHour=60,
     MinPerDay=60*24,
     TicksPerMinute=1,
     TicksPerHour=TicksPerMinute*MinPerHour,
     TicksPerDay=TicksPerMinute*MinPerDay
   };
   TimeType ticks(){ return 10; }
   TimeType ticksFromDuration(DurationType d){ return d;}
   DurationType minutes( int nmin ){
      return nmin*TicksPerMinute;
   }
   DurationType hours( int nhour ) {
      return nhour*TicksPerHour;
   }
   DurationType days( int nday) {
      return nday*TicksPerDay;
   }
};



class TestingActionFactory : public ModelActionFactory
{
public:

   TestingActionFactory(){}
   virtual ~TestingActionFactory(){}

   enum { MIN, HOUR };

   OperationAction* createModelAction(ModelInterface<double>& modelIF,
      DoubleNodePtr express, int rampingLength, int rampingUnit){
      return new ModelAction<TestingModelTimer, double >(
         _timer,
         (TestModelInterface&)(modelIF),
         express, _timer.minutes(rampingLength));
   }

private:
   TestingModelTimer _timer;
};
