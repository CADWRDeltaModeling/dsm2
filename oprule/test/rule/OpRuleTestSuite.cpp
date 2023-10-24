#include "boost/test/unit_test.hpp"
using boost::unit_test_framework::test_suite;

test_suite* getModelActionTestSuite();
test_suite* getOperationManagerTestSuite();

test_suite*
init_unit_test_suite( int argc, char * argv[] ) {

	test_suite* test = BOOST_TEST_SUITE( "Oprule test suite" );
    test->add( getModelActionTestSuite() );
    test->add( getOperationManagerTestSuite() );

    return test;
}