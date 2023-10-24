// Boost.Test
#include "boost/test/unit_test.hpp"
#include "boost/test/floating_point_comparison.hpp"
#include "boost/shared_ptr.hpp"
using boost::unit_test_framework::test_suite;

#include "oprule/rule/ActionResolver.h"
#include "oprule/rule/OperationManager.h"
#include "oprule/rule/OperationAction.h"
#include<typeinfo>
using namespace oprule::rule;

class TrivialOperationAction : public oprule::rule::OperationAction
{

public:
   TrivialOperationAction(int i) : m_int(i), _active(false){}
   virtual ~TrivialOperationAction(){}
   virtual void advance(double dt){};
   virtual void step(double dt){};
   virtual void setActive(bool active){ _active=active;}
   virtual bool isActive() { return _active; }
   virtual void onCompletion(){ }
   virtual bool hasSubActions(){ return false; }
   bool operator==(const TrivialOperationAction& rhs){
	   return this->m_int == rhs.m_int;}
private:
   bool _active;
   int m_int;
};

class EveryNTimeTrigger : public oprule::rule::Trigger{
public:
   EveryNTimeTrigger(int n) : mi(0), mn(n){}
   bool test(){return ((++mi) % mn)==0;}
   void step(double dt){}
   void reset(){mi=0;}
private:
   int mi;
   int mn;
};

class TrivialActionResolver : public oprule::rule::ActionResolver{
public:
   TrivialActionResolver(){}
   virtual bool overlap(OperationAction& act1,OperationAction& act2){
      try{
         TrivialOperationAction& left = dynamic_cast<TrivialOperationAction&>(act1);
         TrivialOperationAction& right = dynamic_cast<TrivialOperationAction&>(act2);
         return left == right;
      } catch (std::bad_cast&){
         return false;
      }
   }
   virtual ActionResolver::RulePriority resolve(){ return ActionResolver::IGNORE_NEW_RULE;}
};


/**
 * The triggers in the test rules go off every n times, n given in the constructor.
 * The actions store a (fixed) number, given in the constructor and
 * if that number is the same for two actions then they conflict (e.g. 1&2).
*/

class OperationManagerTest{
public:
   typedef boost::shared_ptr<TrivialOperationAction> TrivialOperationActionPtr ;
   typedef boost::shared_ptr<EveryNTimeTrigger> EveryNTimeTriggerPtr;
   OperationManagerTest(): act1(new TrivialOperationAction(1)),
						   trig1(new EveryNTimeTrigger(2)),
						   rule1(new OperatingRule(act1,trig1)),
	                       act2(new TrivialOperationAction(1)),
						   trig2(new EveryNTimeTrigger(3)),
						   rule2(new OperatingRule(act2,trig2)),
	                       act3(new TrivialOperationAction(2)),
						   trig3(new EveryNTimeTrigger(2)),
						   rule3(new OperatingRule(act3,trig3)),
						   resolve(), manager(resolve)
                           {}
   void testConstruction(){
      BOOST_CHECK(!rule1->isActive());
      BOOST_CHECK(!rule2->isActive());
      BOOST_CHECK(!rule3->isActive());
      BOOST_CHECK(*act1 == *act2);
      BOOST_CHECK(!(*act1 == *act3));
   };

   void testAddRule(){
      manager.addRule(rule1);
      BOOST_CHECK(! manager.isActive(rule1) && !manager.isActive(rule2));
      manager.addRule(rule2);
      manager.addRule(rule3);
      BOOST_CHECK( ! rule2->isActive() &&
                   ! rule3->isActive());
   };

   void testManageActivation(){
      manager.manageActivation();  // none active yet, n=1
      BOOST_CHECK( ! rule1->isActive() );
      BOOST_CHECK( ! rule2->isActive());
      BOOST_CHECK( ! rule3->isActive());
      manager.manageActivation();  // 1 and 3 active  because n=2 and blocking is not an issue
      BOOST_CHECK( manager.isActive(rule1));
      BOOST_CHECK( !manager.isActive(rule2));
      BOOST_CHECK( manager.isActive(rule3));
      manager.manageActivation();  // 1 and 3 active. Two attempts to activate, but is blocked
      BOOST_CHECK( manager.isActive(rule1));
      BOOST_CHECK( !manager.isActive(rule2));
      BOOST_CHECK( manager.isActive(rule3));
   };

   void testAdvanceStep(){
      trig1->reset();
      trig2->reset();
      trig3->reset();
      manager.manageActivation();
      manager.manageActivation(); //1 and 3
      rule1->setActive(false);
      manager.advanceActions(HUGE_VAL);  //todo: urgent
      manager.manageActivation();
      BOOST_CHECK( !manager.isActive(rule1));
      BOOST_CHECK( manager.isActive(rule2));
      BOOST_CHECK( manager.isActive(rule3));
   };

   void testCheckActionPriority(){
	  BOOST_CHECK_EQUAL(manager.checkActionPriority(*rule1,*rule2),
		  ActionResolver::DEFER_NEW_RULE);
      BOOST_CHECK_EQUAL(manager.checkActionPriority(*rule3,*rule2),
		  ActionResolver::RULES_COMPATIBLE);
   };

   TrivialActionResolver resolve;
   OperationManager manager;
   TrivialOperationActionPtr act1, act2, act3;
   EveryNTimeTriggerPtr trig1, trig2, trig3;
   oprule::rule::OperatingRulePtr rule1, rule2, rule3;

};


struct OperationManagerTestSuite : public test_suite {
  OperationManagerTestSuite() : test_suite("OperationManager Test Suite") {
     boost::shared_ptr<OperationManagerTest> instance( new OperationManagerTest() );
    add( BOOST_CLASS_TEST_CASE( &OperationManagerTest::testConstruction, instance ));
    add( BOOST_CLASS_TEST_CASE( &OperationManagerTest::testAddRule, instance ));
    add( BOOST_CLASS_TEST_CASE( &OperationManagerTest::testManageActivation, instance ));
    add( BOOST_CLASS_TEST_CASE( &OperationManagerTest::testAdvanceStep, instance ));
    add( BOOST_CLASS_TEST_CASE( &OperationManagerTest::testCheckActionPriority, instance ));
    }
};

test_suite* getOperationManagerTestSuite(){ return new OperationManagerTestSuite(); }





