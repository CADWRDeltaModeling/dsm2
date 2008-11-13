#include "boost/test/unit_test.hpp"
using boost::unit_test_framework::test_suite;

test_suite* getParserTestSuite();
test_suite* getExpressionTestSuite();

test_suite*
init_unit_test_suite( int argc, char * argv[] ) {
	test_suite* test = BOOST_TEST_SUITE( "Parser test suite" );
    test->add( getParserTestSuite() );
    test->add( getExpressionTestSuite() );
    return test;
}
