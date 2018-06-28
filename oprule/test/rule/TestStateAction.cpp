
// Boost.Test
#include <boost/test/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
using boost::unit_test_framework::test_suite;

// STL
#include <iostream>

// Project
#include "OperatingRule/ModelAction.h"
#include "oprule/expression/ValueNode.h"
#include "ModelStateTestFixture.h"

/**
 *@todo make a test action that has duration of zero
 */

class ModelActionTest {
public:
    ModelActionTest() : _staticModelState(new StaticModelState()),
	   	                    _staticModelState2(new AnotherStaticModelState()),
                             _dynModelState(new DynamicModelState()),
							        _timer(new TestTimer())
	{
		DoubleNode expression(TARGET_STATE);
		DoubleNode expression2(TARGET_DYNAMIC_STATE);
		TestTimer::DurationType duration1=minutes(EVEN_RAMP_DURATION);
		TestTimer::DurationType duration2=minutes(INEXACT_RAMP_DURATION);
		_actionEven=new ModelAction<TestTimer, StaticModelState >(
		   *_timer,
		   *_staticModelState,
		   expression,
           duration1
		);
		_actionInexact=new ModelAction<TestTimer,AnotherStaticModelState>(
		   *_timer,
		   *_staticModelState2,
		   expression,
           duration2
		);

        
		_actionDynamic = new ModelAction<TestTimer,DynamicModelState>(
			*_timer,
			*_dynModelState,
			expression2,
			duration1
		);		
	}

	~ModelActionTest(){ 
		delete _actionEven;
      delete _actionInexact;
      delete _actionDynamic;
	}


    void testConstruction()
    {   
        BOOST_CHECK_EQUAL(_staticModelState->get(),INIT_STATE);
		BOOST_CHECK(!(_actionEven->isActive()));
	}

	void testActivate(){
        _actionEven->setActive(true);
		_actionInexact->setActive(true);
		BOOST_CHECK(_actionEven->isActive());
		BOOST_CHECK_EQUAL(_staticModelState->get(),INIT_STATE);
	}

    void testAdvanceStatic(){
		// first time step
		_timer->advanceStep();   //1
        _actionEven->advance();
		_actionEven->doAction();
		BOOST_CHECK(_actionEven->isActive());
		BOOST_CHECK_CLOSE(
			 _staticModelState->get(),
			(StaticModelState::StateType)INTERMEDIATE_STATE,
		     1e-8);
        _actionInexact->advance();
		_actionInexact->doAction();

		BOOST_CHECK(_actionEven->isActive());
		// This test assumes INEXACT_RAMP_DURATION is 
		// a small but not infinitessimal amount greater than
		// EVEN_RAMP_DURATION.
		BOOST_CHECK_CLOSE(
			_staticModelState2->get(),
			(((AnotherStaticModelState::StateType)INTERMEDIATE_STATE) - 1.e-2),
			9.e-3);
        // two more steps, exact action will finish, inexact is not
		// quite finished
		_timer->advanceStep();   //2
		_timer->advanceStep();   //3
		
		_actionEven->advance();  
	   _actionEven->doAction();

		BOOST_CHECK(! _actionEven->isActive());
		BOOST_CHECK( _actionInexact->isActive());
      
		BOOST_CHECK_EQUAL(
			_staticModelState->get(),
			(StaticModelState::StateType)TARGET_STATE);

		BOOST_CHECK(_staticModelState2->get() < TARGET_STATE);
        
		// final advance, inexact should be finished now
		_timer->advanceStep();
		_actionInexact->advance(); //4
		_actionInexact->doAction();
		BOOST_CHECK_EQUAL(_staticModelState2->get(), TARGET_STATE);
      BOOST_CHECK( !_actionInexact->isActive());
	}

    void testAdvanceDynamic(){
        
		TestTimer::resetTimer();
		
		// first time step
		_actionDynamic->setActive(true);
		BOOST_CHECK(_actionDynamic->isActive());
		DynamicModelState::StateType origin=_dynModelState->get();

		BOOST_CHECK_CLOSE(
			 _dynModelState->get(),
			 origin,
		     1e-8);


		_timer->advanceStep();   //1
		BOOST_CHECK_CLOSE(
			 _dynModelState->get(),
			 origin+1,
		     1e-8);
        _actionDynamic->advance();
		_actionDynamic->doAction();
        //Note this hardwires 3 steps in duration
		BOOST_CHECK_CLOSE(
			 _dynModelState->get(),
			 (DynamicModelState::StateType) ( (origin+1) +
			    (TARGET_DYNAMIC_STATE - (origin+1))/3.),
		     1e-8);

		_timer->advanceStep();   //2
		BOOST_CHECK_CLOSE(
			 _dynModelState->get(),
			 origin+2,
		     1e-8);
        _actionDynamic->advance();
		_actionDynamic->doAction();
		BOOST_CHECK_CLOSE(
			 _dynModelState->get(),
			 (DynamicModelState::StateType) (origin+2) +
			    (TARGET_DYNAMIC_STATE - (origin+2))*2./3.,
		     1e-8);

		_timer->advanceStep();   //3
		BOOST_CHECK_CLOSE(
			 _dynModelState->get(),
			 origin+3,
		     1e-8);
        _actionDynamic->advance();
		_actionDynamic->doAction();
		BOOST_CHECK_CLOSE(
			 _dynModelState->get(),
			 (DynamicModelState::StateType)TARGET_DYNAMIC_STATE,
		     1e-8);

		_timer->advanceStep();   //4 (redundant)
		BOOST_CHECK_CLOSE(
			 _dynModelState->get(),
			 origin+4,
		     1e-8);
        _actionDynamic->advance();
		_actionDynamic->doAction();
		BOOST_CHECK_CLOSE(
			 _dynModelState->get(),
			 (DynamicModelState::StateType)
			    TARGET_DYNAMIC_STATE,
		     1e-8);
      
      BOOST_CHECK(! _actionDynamic->isActive());
    }


private:
	 std::auto_ptr<TestTimer> _timer;
	 std::auto_ptr<DynamicModelState> _dynModelState;
	 std::auto_ptr<StaticModelState> _staticModelState;
	 std::auto_ptr<AnotherStaticModelState> _staticModelState2;
	 // Action with a ramping duration that is an exact 
	 // multiple of the model time step
	 ModelAction<TestTimer,StaticModelState>* _actionEven;
	 // Action with a ramping duration that is an inexact multiple
     ModelAction<TestTimer,AnotherStaticModelState>* _actionInexact;
     // Action that manipulates a dynamic model state
	 ModelAction<TestTimer,DynamicModelState>* _actionDynamic;
};


class ActionSetTest : public test_suite {

public:
   ActionSetTest()    
            : _staticModelState(new StaticModelState()),
		        _staticModelState2(new AnotherStaticModelState()),
              _dynModelState(new DynamicModelState()),
	  	        _timer(new TestTimer())
   {
		ValueNode<double> expression(TARGET_STATE);
		ValueNode<double> expression2(TARGET_DYNAMIC_STATE);
		TestTimer::DurationType duration1=minutes(EVEN_RAMP_DURATION);
		TestTimer::DurationType duration2=minutes(INEXACT_RAMP_DURATION);
		_actionEven=new ModelAction<TestTimer,StaticModelState>(
		   *_timer,
		   *_staticModelState,
		   expression,
           duration1
		);
		_actionInexact=new ModelAction<TestTimer,AnotherStaticModelState>(
		   *_timer,
		   *_staticModelState2,
		   expression,
           duration2
		);
		_actionDynamic = new ModelAction<TestTimer,DynamicModelState>(
			*_timer,
			*_dynModelState,
			expression2,
			duration1
		);		
	}
	~ActionSetTest(){ 
		delete _actionEven;
      delete _actionInexact;
      delete _actionDynamic;
	}
private:
    std::auto_ptr<TestTimer> _timer;
	 std::auto_ptr<DynamicModelState> _dynModelState;
	 std::auto_ptr<StaticModelState> _staticModelState;
	 std::auto_ptr<AnotherStaticModelState> _staticModelState2;
	 // Action with a ramping duration that is an exact 
	 // multiple of the model time step
	 ModelAction<TestTimer,StaticModelState>* _actionEven;
	 // Action with a ramping duration that is an inexact multiple
     ModelAction<TestTimer,AnotherStaticModelState>* _actionInexact;
     // Action that manipulates a dynamic model state
	 ModelAction<TestTimer,DynamicModelState>* _actionDynamic;
};




struct ModelActionTestSuite : public test_suite {
    ModelActionTestSuite() : test_suite("ModelActionTestSuite") {
        // add member function test cases to a test suite
        boost::shared_ptr<ModelActionTest> instance( new ModelActionTest() );
        add( BOOST_CLASS_TEST_CASE( &ModelActionTest::testConstruction, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelActionTest::testActivate, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelActionTest::testAdvanceStatic, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelActionTest::testAdvanceDynamic, instance ));
    }
};
 
test_suite*
init_unit_test_suite( int argc, char * argv[] ) {
    std::auto_ptr<test_suite> test( BOOST_TEST_SUITE( "Test for model state action" ) );
    test->add( new ModelActionTestSuite() );
    return test.release();
}

// EOF
