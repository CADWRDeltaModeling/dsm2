#include <functional>

// Boost.Test
#include <boost/test/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

using boost::unit_test_framework::test_suite;
using namespace std;

// STL
#include <string>
#include <vector>
#include "oprule/expression/ValueNode.h"
#include "oprule/expression/LookupNode.h"
#include "oprule/expression/BinaryOpNode.h"
#include "oprule/expression/UnaryOpNode.h"
#include "oprule/expression/AccumulationNode.h"
#include "oprule/expression/LinearExtrapolationNode.h"
#include "oprule/expression/QuadExtrapolationNode.h"


#include "ParserTestFixture.h"  //some classes associated with test fixture

#define FLOAT_TOL 1.e-8

#define BIN_OP_TEST_BASE(X,OP,Y,COMPARE,ANS,NEG) \
    BOOST_CHECK(  NEG== \
    BinaryOpNode<COMPARE<double> > ( \
    BinaryOpNode<OP<double> >::create( \
    DoubleScalarNode::create(X), DoubleScalarNode::create(Y) ), \
    DoubleScalarNode::create(ANS)  \
    ).eval() \
    )
#define BIN_OP_TEST(X,OP,Y,COMPARE,ANS) \
    BIN_OP_TEST_BASE(X,OP,Y,COMPARE,ANS,true )

#define BIN_OP_TEST_NOT(X,OP,Y,COMPARE,ANS) \
    BIN_OP_TEST_BASE(X,OP,Y,COMPARE,ANS,false)


#define UNARY_OP_TEST_BASE(X,OP,COMPARE,ANS,NEG) \
    BOOST_CHECK(  NEG == \
    BinaryOpNode<COMPARE<double> >( \
    UnaryOpNode<OP >::create( \
    DoubleScalarNode::create(X) ), \
    DoubleScalarNode::create(ANS)  \
    ).eval() \
    )
#define UNARY_OP_TEST(X,OP,COMPARE,ANS) \
    UNARY_OP_TEST_BASE(X,OP,COMPARE,ANS,true )

#define UNARY_OP_TEST_NOT(X,OP,COMPARE,ANS) \
    UNARY_OP_TEST_BASE(X,OP,COMPARE,ANS,false)


class ExpressionTest {


public:
    ExpressionTest(){}

    ~ExpressionTest(){}

    void testUnaryDouble(){
        UNARY_OP_TEST(16.,sqrt_func,std::equal_to,4.0);
        UNARY_OP_TEST_NOT(16.,sqrt_func,std::equal_to,5.0);
        UNARY_OP_TEST(100.,log10_func,std::equal_to,2.0);
        UNARY_OP_TEST(1.,ln_func,std::equal_to,0.0);
        UNARY_OP_TEST(1.,exp_func,std::greater_equal,2.7182);
        UNARY_OP_TEST(1.,exp_func,std::less_equal,2.7183);
        UNARY_OP_TEST(2.718, ln_func, std::less, 1.);
        UNARY_OP_TEST(2.718, ln_func, std::greater, 0.999);

    }

    void testLookup(){
        vector<double> lookup;
        vector<double> vals;
        lookup.push_back(1000.);
        lookup.push_back(2000.);
        lookup.push_back(3000.);
        vals.push_back(1.);
        vals.push_back(2.);
        DoubleScalarNode::NodePtr val1 = DoubleScalarNode::create(1000.);
        DoubleScalarNode::NodePtr val2 = DoubleScalarNode::create(2000.);
        DoubleScalarNode::NodePtr val29 = DoubleScalarNode::create(2999.);

        LookupNode look1=LookupNode(val1,lookup,vals);
        LookupNode look2=LookupNode(val2,lookup,vals);
        LookupNode look29=LookupNode(val29,lookup,vals);

        BOOST_CHECK_CLOSE(look1.eval(),1.,FLOAT_TOL);
        BOOST_CHECK_CLOSE(look2.eval(),2.,FLOAT_TOL);
        BOOST_CHECK_CLOSE(look29.eval(),2.,FLOAT_TOL);

    }

    void testBinaryDouble(){
        BIN_OP_TEST(10.,std::divides,2.0,std::less,7.0);
        BIN_OP_TEST(6.,std::divides,2.0,std::equal_to,3.0);
        BIN_OP_TEST_NOT(6.,std::divides,2.0,std::equal_to,2.0);

        BIN_OP_TEST(6.,std::divides,2.0,std::less_equal,3.0);
        BIN_OP_TEST(6.,std::divides,2.0,std::greater_equal,3.0);
        BIN_OP_TEST(6.,std::divides,2.0,std::greater,2.0);
        BIN_OP_TEST(6.,std::divides,2.0,std::not_equal_to,2.0);

        BIN_OP_TEST(10.,std::multiplies,2.0,std::equal_to,20.0);
        BIN_OP_TEST(10.,std::plus,2.0,std::equal_to,12.0);
        BIN_OP_TEST(10.,std::minus,2.0,std::equal_to,8.0);
        BIN_OP_TEST_NOT(10.,std::minus,2.0,std::equal_to,7.0);
    }

    void testUnaryCopy(){
        BOOST_CHECK_CLOSE(  UnaryOpNode<ln_func > (
            DoubleScalarNode::create(1.)).copy()->eval(),
            0.0,
            FLOAT_TOL);
    }

    void testBinaryCopy(){
        BOOST_CHECK_CLOSE(  BinaryOpNode<std::plus<double> > (
            DoubleScalarNode::create(2.)->copy(),
            DoubleScalarNode::create(2.) ).copy()->eval(),
            4.0,
            FLOAT_TOL);
    }


    void testAccumulate(){
        resetInterfaces();
        ModelInterface<double>::NodePtr expr1=TestModelInterface::create();
        ModelInterface<double>::NodePtr expr2=TestModelInterface2::create();
        expr1->set(3.);
        expr2->set(4.);
        BOOST_CHECK_CLOSE(expr1->eval(),3.,FLOAT_TOL);
        BoolNodePtr isfour= BinaryOpNode<std::equal_to<double> >::create(
            expr2, DoubleScalarNode::create(4.))->copy();

        DoubleScalarNode::NodePtr expr = DoubleScalarNode::create(2.);
        AccumulationNode<double> accum(expr,expr1, isfour);
        accum.init();
        BOOST_CHECK_CLOSE(accum.eval(),3.,FLOAT_TOL);
        accum.step(1.);   // this is due to reset+increment
        BOOST_CHECK_CLOSE(accum.eval(),5.,FLOAT_TOL);
        expr2->set(3.);  // isfour will be false, no resets
        accum.step(1.);
        BOOST_CHECK_CLOSE(accum.eval(),7.,FLOAT_TOL);
        accum.step(1.);
        BOOST_CHECK_CLOSE(accum.eval(),9.,FLOAT_TOL);
        expr2->set(4.);  // isfour is true, reset
        accum.step(0.);
        BOOST_CHECK_CLOSE(accum.eval(),5.,FLOAT_TOL);
    }

    void testLinearPredict(){
        resetInterfaces();
        ModelInterface<double>::NodePtr expr=TestModelInterface::create();
        expr->set(4.);
        oprule::expression::LinearExtrapolationNode<double> predict(expr, 2.0);
        predict.init();
        predict.step(1.);
        BOOST_CHECK_CLOSE(predict.eval(), expr->eval(),FLOAT_TOL);
        expr->set(6.);
        predict.step(1.); // dt=1.
        BOOST_CHECK_CLOSE(predict.eval(),10.,FLOAT_TOL);
    }

    void testQuadPredict(){
        resetInterfaces();
        ModelInterface<double>::NodePtr expr=TestModelInterface::create();
        oprule::expression::QuadExtrapolationNode qpredict(expr, 2.0);
        expr->set(4.);
        qpredict.init();
        BOOST_CHECK_CLOSE(qpredict.eval(),4.,FLOAT_TOL);
        expr->set(5.);
        qpredict.step(1.);
        BOOST_CHECK_CLOSE(qpredict.eval(),10.,FLOAT_TOL);
        expr->set(8.);
        qpredict.step(1.);
        BOOST_CHECK_CLOSE(qpredict.eval(),20.,FLOAT_TOL);

    }

};


struct ExpressionTestSuite : public test_suite {
    ExpressionTestSuite() : test_suite("ExpressionTestSuite") {
        // add member function test cases to a test suite
        boost::shared_ptr<ExpressionTest> instance( new ExpressionTest() );
        add( BOOST_CLASS_TEST_CASE( &ExpressionTest::testBinaryDouble, instance ));
        add( BOOST_CLASS_TEST_CASE( &ExpressionTest::testUnaryDouble, instance ));
        add( BOOST_CLASS_TEST_CASE( &ExpressionTest::testUnaryCopy, instance ));
        add( BOOST_CLASS_TEST_CASE( &ExpressionTest::testBinaryCopy, instance ));
        add( BOOST_CLASS_TEST_CASE( &ExpressionTest::testLookup, instance ));
        add( BOOST_CLASS_TEST_CASE( &ExpressionTest::testAccumulate, instance ));
        add( BOOST_CLASS_TEST_CASE( &ExpressionTest::testLinearPredict, instance ));
        add( BOOST_CLASS_TEST_CASE( &ExpressionTest::testQuadPredict, instance ));

    }
};

test_suite*
getExpressionTestSuite() {
    test_suite* test = BOOST_TEST_SUITE( "Expression test suite" );
    test->add( new ExpressionTestSuite() );
    return test;
}





// EOF
