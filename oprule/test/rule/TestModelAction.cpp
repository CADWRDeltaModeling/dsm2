// Boost.Test
#include "boost/lexical_cast.hpp"
#include "boost/test/unit_test.hpp"
#include "boost/test/floating_point_comparison.hpp"
#include "boost/shared_ptr.hpp"
using boost::unit_test_framework::test_suite;

// STL
#include <iostream>
#include <functional>
#include <memory>


// Project
#include "oprule/rule/ModelAction.h"
#include "oprule/rule/Transition.h"
#include "oprule/rule/ActionSet.h"
#include "oprule/rule/ActionChain.h"
#include "oprule/expression/ExpressionNode.h"
#include "oprule/expression/ValueNode.h"
#include "oprule/expression/LaggedExpressionNode.h"

#include "ModelStateTestFixture.h"

#define EVEN_RAMP_DURATION 45
#define INEXACT_RAMP_DURATION 46
#define TARGET_STATE (INIT_STATE +3)
#define INTERMEDIATE_STATE (INIT_STATE +1)
#define TARGET_DYNAMIC_STATE 12

using boost::unit_test_framework::test_suite;
using namespace oprule::rule;

/**
*@todo make a test action that has duration of zero
*@todo chains and sets
*/

class ModelActionTest {
public:
    ModelActionTest() :
        _staticModelState(new StaticModelState()),
        _staticModelState2(new AnotherStaticModelState()),
        _dynModelState(new DynamicModelState()){
            typedef oprule::expression::DoubleNode DoubleNode;
            typedef oprule::expression::DoubleScalarNode DoubleScalarNode;
            DoubleNode::NodePtr expression=DoubleScalarNode::create(TARGET_STATE);
            DoubleNode::NodePtr expression2=DoubleScalarNode::create(TARGET_DYNAMIC_STATE);
            double duration1=EVEN_RAMP_DURATION;
            double duration2=INEXACT_RAMP_DURATION;
            TransitionPtr trans1 = TransitionPtr(new LinearTransition(duration1));

            _actionEven=OperationActionPtr( new ModelAction<double >(
                _staticModelState,
                expression,
                trans1));

            TransitionPtr trans2 = TransitionPtr(new LinearTransition(duration2));
            _actionInexact=OperationActionPtr(new ModelAction<double>(
                _staticModelState2,
                expression,
                trans2
                ));

            _actionDynamic=OperationActionPtr(new ModelAction<double>(
                _dynModelState,
                expression2,
                trans1
                ));
    }

    ~ModelActionTest(){
        //delete _actionEven;
        //delete _actionInexact;
        //delete _actionDynamic;
    }


    void testConstruction()
    {
        test_model::model_init();
        BOOST_CHECK_EQUAL(_staticModelState->eval(),INIT_STATE);
        BOOST_CHECK(!(_actionEven->isActive()));
    }

    void testActivate(){
        test_model::model_init();
        _actionEven->setActive(true);
        _actionInexact->setActive(true);
        BOOST_CHECK(_actionEven->isActive());
        BOOST_CHECK_EQUAL(_staticModelState->eval(),INIT_STATE);
    }

    void testAdvanceStatic(){
        // first time step
        test_model::model_init();
        test_model::model_goto_step(1);
        _actionEven->advance(DT);
        BOOST_CHECK(_actionEven->isActive());
        BOOST_CHECK_CLOSE(
            _staticModelState->eval(),
            (StaticModelState::StateType)INTERMEDIATE_STATE,1e-8);
        _actionInexact->advance(DT);
        BOOST_CHECK(_actionEven->isActive());

        // This test assumes INEXACT_RAMP_DURATION is
        // a small but not infinitessimal amount greater than
        // EVEN_RAMP_DURATION.
        BOOST_CHECK_CLOSE(
            _staticModelState2->eval(),
            INIT_STATE+3*(((AnotherStaticModelState::StateType)(15./46.))),
            1.e-8);
        // two more steps, exact action will finish, inexact is not
        // quite finished
        test_model::model_goto_step(2);
        _actionEven->advance(DT);
        _actionInexact->advance(DT);

        test_model::model_goto_step(3);
        _actionEven->advance(DT);
        _actionInexact->advance(DT);

        BOOST_CHECK(! _actionEven->isActive());
        BOOST_CHECK( _actionInexact->isActive());

        BOOST_CHECK_EQUAL(
            _staticModelState->eval(),
            (StaticModelState::StateType)TARGET_STATE);

        BOOST_CHECK(_staticModelState2->eval() < TARGET_STATE);

        // final advance, inexact should be finished now

        test_model::model_goto_step(4);; //4
        _actionInexact->advance(DT);
        BOOST_CHECK_EQUAL(_staticModelState2->eval(), TARGET_STATE);
        BOOST_CHECK( !_actionInexact->isActive());
    }

    void testAdvanceDynamic(){
        test_model::model_init();
        _actionDynamic->setActive(true);
        BOOST_CHECK(_actionDynamic->isActive());
        DynamicModelState::StateType origin=_dynModelState->eval();
        BOOST_CHECK_CLOSE(
            _dynModelState->eval(),
            origin,
            1e-8);

        test_model::model_goto_step(1);
        BOOST_CHECK_CLOSE(
            _dynModelState->eval(),
            origin+1,
            1e-8);

        // First of 3 steps in duration
        _actionDynamic->advance(DT);
        BOOST_CHECK_CLOSE(
            _dynModelState->eval(),
            (DynamicModelState::StateType) 7.3333333333,
            // @todo: below may be the intended behavior. The difference is
            // whether the "from" part of the ramping changes with time
            // so that you are ramping from one time series to another) or
            // whether the "from" value is a snapshot at activation (the below)
            //( origin + 1 +
            // (TARGET_DYNAMIC_STATE - origin)/3.),
            1e-8);

        test_model::model_goto_step(2); // current value is origin + 2
                                        // the previous action is replaced by this step
        // before action:
        BOOST_CHECK_CLOSE(
            _dynModelState->eval(),
            origin+2,
            1e-8);

        //after action:
        _actionDynamic->advance(DT);    // 2/3 of transition between original series (now 6)
                                        // and the destination 12
        BOOST_CHECK_CLOSE(
            _dynModelState->eval(),
            10.,
            1e-8);

        test_model::model_goto_step(3);
        _actionDynamic->advance(DT);
        BOOST_CHECK_CLOSE(
            _dynModelState->eval(),
            (DynamicModelState::StateType)TARGET_DYNAMIC_STATE,
            1e-8);
        // on completion of the action, the origin is changed to EXPRESSION_SET
        test_model::model_goto_step(4);
        BOOST_CHECK_CLOSE(
            _dynModelState->eval(),
            (DynamicModelState::StateType)EXPRESSION_SET+4,
            1e-8);
        BOOST_CHECK(! _actionDynamic->isActive());
    }


    void testAdvanceLaggedDynamic(){
        /* todo:urgent		test_model::model_init();
        DynamicModelState::NodePtr dns=DynamicModelState::create();
        DynamicModelState::StateType origin=dns->eval();
        oprule::expression::LaggedDoubleNode lagged(2);
        lagged.setExpression(dns);
        lagged.set(0,origin);
        lagged.set(1,origin);
        lagged.set(2,origin);
        BOOST_CHECK_CLOSE(
        lagged.eval(),
        origin,
        1e-8);

        test_model::model_goto_step(1);
        lagged.advanceStep();
        BOOST_CHECK_CLOSE(
        lagged.eval(),
        origin,
        1e-8);

        test_model::model_goto_step(2);
        lagged.advanceStep();
        BOOST_CHECK_CLOSE(
        lagged.eval(),
        (DynamicModelState::StateType) ( origin),
        1e-8);

        test_model::model_goto_step(3);
        lagged.advanceStep();
        //      Note this assumes 3 steps in duration
        BOOST_CHECK_CLOSE(
        lagged.eval(),
        (DynamicModelState::StateType) ( origin + 1.),
        1e-8);

        test_model::model_goto_step(4);
        lagged.advanceStep();
        //      Note this assumes 3 steps in duration
        BOOST_CHECK_CLOSE(
        lagged.eval(),
        (DynamicModelState::StateType) ( origin + 2.),
        1e-8);
        */
    }


    void testActionChain(){
        test_model::model_init();
        BOOST_CHECK_EQUAL(
            _staticModelState->eval(),
            (StaticModelState::StateType)INIT_STATE);
        DynamicModelState::StateType origin=_dynModelState->eval();


        boost::shared_ptr<ActionChain> chainPtr(new ActionChain);
        ActionChain& chain = *chainPtr;
        _actionEven->registerParent(chainPtr);
        chain.pushBackAction(_actionEven);

        _actionDynamic->registerParent(chainPtr);
        chain.pushBackAction(_actionDynamic);

        chain.setActive(true);
        test_model::model_goto_step(3);
        chain.advance(HUGE_VAL);
        BOOST_CHECK(! _actionEven->isActive());
        BOOST_CHECK( _actionDynamic->isActive());
        BOOST_CHECK( chain.isActive());

        BOOST_CHECK_EQUAL(
            _staticModelState->eval(),
            (StaticModelState::StateType)TARGET_STATE);
        BOOST_CHECK_CLOSE(
            _dynModelState->eval(),
            origin+3,
            1e-8);

        test_model::model_goto_step(6);
        chain.advance(HUGE_VAL);
        BOOST_CHECK_EQUAL(
            _staticModelState->eval(),
            (StaticModelState::StateType)TARGET_STATE);
        BOOST_CHECK_CLOSE(
            _dynModelState->eval(),
            (DynamicModelState::StateType)TARGET_DYNAMIC_STATE,
            1e-8);

        BOOST_CHECK(! _actionEven->isActive());
        BOOST_CHECK(! _actionInexact->isActive());
        BOOST_CHECK(! chain.isActive());
        test_model::model_goto_step(7);
        //chain.advance();
        BOOST_CHECK_EQUAL(
            _staticModelState->eval(),
            (StaticModelState::StateType)TARGET_STATE);
        BOOST_CHECK_CLOSE(
            _dynModelState->eval(),
            (DynamicModelState::StateType)EXPRESSION_SET+7,
            1e-8);
        BOOST_CHECK(! chain.isActive());


    }

private:
    boost::shared_ptr<DynamicModelState> _dynModelState;
    boost::shared_ptr<StaticModelState> _staticModelState;
    boost::shared_ptr<AnotherStaticModelState> _staticModelState2;
    // Action with a ramping duration that is an exact
    // multiple of the model time step
    OperationActionPtr _actionEven;
    // Action with a ramping duration that is an inexact multiple
    OperationActionPtr _actionInexact;
    // Action that manipulates a dynamic model state
    OperationActionPtr _actionDynamic;

};


class ActionSetTest{

public:
    typedef oprule::expression::ValueNode<double> DoubleScalarNode;
    ActionSetTest()
        : _staticModelState(new StaticModelState()),
        _staticModelState2(new AnotherStaticModelState()),
        _dynModelState(new DynamicModelState())
    {
        DoubleScalarNode::NodePtr expression=DoubleScalarNode::create(TARGET_STATE);
        DoubleScalarNode::NodePtr expression2=DoubleScalarNode::create(TARGET_DYNAMIC_STATE);
        double duration1=EVEN_RAMP_DURATION;
        double duration2=INEXACT_RAMP_DURATION;
        TransitionPtr trans1 = TransitionPtr(new LinearTransition(duration1));
        TransitionPtr trans2 = TransitionPtr(new LinearTransition(duration2));
        _actionEven=OperationActionPtr(new ModelAction<double>(
            _staticModelState,
            expression,
            trans1
            ));
        _actionInexact=OperationActionPtr(new ModelAction<double>(
            _staticModelState2,
            expression,
            trans2
            ));
        _actionDynamic = OperationActionPtr(new ModelAction<double>(
            _dynModelState,
            expression2,
            trans1
            ));
    }
    ~ActionSetTest(){
    }
private:
    boost::shared_ptr<DynamicModelState> _dynModelState;
    boost::shared_ptr<StaticModelState> _staticModelState;
    boost::shared_ptr<AnotherStaticModelState> _staticModelState2;
    // Action with a ramping duration that is an exact
    // multiple of the model time step
    OperationActionPtr _actionEven;
    //ModelAction<double>* _actionEven;
    // Action with a ramping duration that is an inexact multiple
    //ModelAction<double>* _actionInexact;
    OperationActionPtr _actionInexact;
    // Action that manipulates a dynamic model state
    OperationActionPtr _actionDynamic;
};




struct ModelActionTestSuite : public test_suite {
    ModelActionTestSuite() : test_suite("ModelActionTestSuite") {
        // add member function test cases to a test suite
        boost::shared_ptr<ModelActionTest> instance( new ModelActionTest() );
        add( BOOST_CLASS_TEST_CASE( &ModelActionTest::testConstruction, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelActionTest::testActivate, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelActionTest::testAdvanceStatic, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelActionTest::testAdvanceDynamic, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelActionTest::testAdvanceLaggedDynamic, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelActionTest::testActionChain, instance ));
    }
};

test_suite* getModelActionTestSuite(){ return new ModelActionTestSuite(); }


