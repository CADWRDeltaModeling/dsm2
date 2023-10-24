//Local
#include "test_interface_f.h"
#include "dsm2_time_node_factory.h"
#include "dsm2_interface_fortran.h"
#include "dsm2_model_interface.h"
#include "dsm2_time_interface_fortran.h"
#include "dsm2_time_interface.h"
#include "dsm2_named_value_lookup.h"
#include "oprule/rule/ModelInterface.h"
#include "oprule/parser/ModelNameParseError.h"

#define doubletol 1E-8

// Boost.Test
#include <boost/test/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/shared_ptr.hpp>
using boost::unit_test_framework::test_suite;

// STL
#include <iostream>
#include <string>
#include <vector>
#include <deque>
#define TOLER 1.e-08
using namespace std;
using oprule::parser::InvalidIdentifier;
using oprule::parser::MissingIdentifier;

class ModelInterfaceTest {

public:
   ModelInterfaceTest()
   {
       test_setup_f();
	}

	~ModelInterfaceTest(){
	}

    void testDate(){
       //first test date nodes without factory
       BOOST_CHECK(year.eval() == 1990.);
       BOOST_CHECK(month.eval() == 3.);
       BOOST_CHECK(day.eval() == 7.);
       BOOST_CHECK(min.eval() == 15.);

       //now through factory
       oprule::expression::DoubleNodePtr ynode=DSM2TimeNodeFactory.getYearNode();
       BOOST_CHECK_CLOSE(ynode->eval(), 1990., TOLER);
       oprule::expression::DoubleNodePtr mnode=DSM2TimeNodeFactory.getMonthNode();
       BOOST_CHECK_CLOSE(mnode->eval(),3., TOLER);
       oprule::expression::DoubleNodePtr dnode=DSM2TimeNodeFactory.getDayNode();
       BOOST_CHECK_CLOSE(dnode->eval(), 7., TOLER);
       oprule::expression::DoubleNodePtr minnode=DSM2TimeNodeFactory.getMinNode();
       BOOST_CHECK_CLOSE(minnode->eval(), 15., TOLER);


       string dt="07MAR1990";
       string tm="00:15";
       oprule::expression::DoubleNodePtr dtnode=DSM2TimeNodeFactory.getDateTimeNode(dt,tm);
       BOOST_CHECK_CLOSE(dtnode->eval(), DSM2HydroDateTimeNode().eval(),TOLER);
       string tm2="00:00";
       oprule::expression::DoubleNodePtr dtnode2=DSM2TimeNodeFactory.getDateTimeNode(dt,"00:00");
       string tm3="24:00";
       string dt2="06MAR1990";
       oprule::expression::DoubleNodePtr dtnode3=DSM2TimeNodeFactory.getDateTimeNode(dt2,tm3);
       BOOST_CHECK_CLOSE(dtnode2->eval(), dtnode3->eval(), TOLER);

       oprule::expression::DoubleNodePtr refseasnode1=DSM2TimeNodeFactory.getReferenceSeasonNode(3,7,0,0);
       oprule::expression::DoubleNodePtr refseasnode2=DSM2TimeNodeFactory.getReferenceSeasonNode(3,8,0,0);
       oprule::expression::DoubleNodePtr refseasnodeexact=DSM2TimeNodeFactory.getReferenceSeasonNode(3,7,0,15);

       oprule::expression::DoubleNodePtr seasnode=DSM2TimeNodeFactory.getSeasonNode();

       BOOST_CHECK_CLOSE(seasnode->eval(), refseasnodeexact->eval(), TOLER);
       BOOST_CHECK(seasnode->eval() > refseasnode1->eval());
       BOOST_CHECK(seasnode->eval() < refseasnode2->eval());
    }

    void testGateDeviceIndex(){
        string gateName("Gate 2");
        int gindex=gate_index(gateName.c_str(),gateName.length());
        BOOST_CHECK(gindex == 2);
        string weirName("Weir 1");
        int windex=device_index(gindex,weirName.c_str(),weirName.length());
        BOOST_CHECK(windex == 1);
    }

    void testGateDeviceInterface(){
      map<string,string> identifiers;
	   identifiers["modelname"]="gate_op";
	   identifiers["gate"]="Gate 1";
	   identifiers["device"]="Weir 2";
      identifiers["direction"]="to_node";

       oprule::rule::ModelInterface<double>::NodePtr msi
          =DSM2lookup.getModelInterface("gate_op",identifiers);
       BOOST_CHECK(msi->eval() == 1);
       msi->set(0.5);
       BOOST_CHECK_CLOSE(msi->eval(), 0.5, TOLER);

       identifiers["direction"]="from_node";
       msi=DSM2lookup.getModelInterface("gate_op",identifiers);
       BOOST_CHECK(msi->eval() == 1);
       msi->set(0.5);
       BOOST_CHECK_CLOSE(msi->eval(), 0.5, TOLER);

    }

    void testGateDeviceInterfaceBadInput(){
       map<string,string> identifiers;
	   identifiers["modelname"]="gate_op";
	   identifiers["gate"]="Gate 1";
	   identifiers["device"]="Weir 22";
      oprule::expression::DoubleNodePtr dn;
      try {
		   dn
		     = DSM2lookup.getModelExpression("gate_op",identifiers);
           BOOST_ERROR("Exception InvalidIdentifier is expected");
	   }catch( oprule::parser::InvalidIdentifier const& ){
           BOOST_CHECK_MESSAGE( true, "exception InvalidIdentifier is caught" );
	   }
	   identifiers["device"]="Weir 2";
      try {
		   dn
		     = DSM2lookup.getModelExpression("gate_op",identifiers);
           BOOST_ERROR("Exception MissingIdentifier is expected");
	   }catch( oprule::parser::MissingIdentifier const& ){
           BOOST_CHECK_MESSAGE( true, "exception MissingIdentifier is caught" );
	   }


    }


    void testGateInstallInterface(){
      map<string,string> identifiers;
	   identifiers["modelname"]="gate_install";
	   identifiers["gate"]="Gate 1";

       oprule::rule::ModelInterface<double>::NodePtr msi
          =DSM2lookup.getModelInterface("gate_install",identifiers);
       BOOST_CHECK(msi->eval() == 1.);
       msi->set(0.0);
       BOOST_CHECK_CLOSE(msi->eval(), 0.0, TOLER);
       msi->set(1.0);
       BOOST_CHECK_CLOSE(msi->eval(), 1.0, TOLER);
    }

    void testGateInstallInterfaceBadInput(){
       map<string,string> identifiers;
	   identifiers["modelname"]="gate_install";
	   identifiers["xxxxgate"]="Gate 111";
      oprule::expression::DoubleNodePtr dn;
      try {
		   dn
		     = DSM2lookup.getModelExpression("gate_install",identifiers);
           BOOST_ERROR("Exception MissingIdentifier is expected");
	   }catch( oprule::parser::MissingIdentifier const& ){
           BOOST_CHECK_MESSAGE( true, "exception MissingIdentifier is caught" );
	   }
	   identifiers["gate"]="Weir xxxxx";
      try {
		   dn
		     = DSM2lookup.getModelExpression("gate_install",identifiers);
           BOOST_ERROR("Exception InvalidIdentifier is expected");
	   }catch( oprule::parser::InvalidIdentifier const& ){
           BOOST_CHECK_MESSAGE( true, "exception InvalidIdentifier is caught" );
	   }
    }



    void testFlowNode(){
       map<string,string> identifiers;
	   identifiers["modelname"]="chan_flow";
	   identifiers["channel"]="1";
	   identifiers["dist"]="3000";

       // Channel length is 15000, up and down flows are 1000,2000
       //identifiers.push_back(string("3000"));
       //identifiers.push_back(string("0"));
       oprule::expression::DoubleNodePtr dn =
		   DSM2lookup.getModelExpression("chan_flow",identifiers);
       BOOST_CHECK_CLOSE( dn->eval(), 1400., TOLER );
       identifiers["channel"]="4";
       identifiers["dist"]="5000";
       // Channel length is 5000, 8 is the last comp. point
       oprule::expression::DoubleNodePtr dn2
		   = DSM2lookup.getModelExpression("chan_flow",identifiers);
       BOOST_CHECK_CLOSE( dn2->eval(), 128000., TOLER);
    }

    void testWaterSurfaceNode(){
       map<string,string> identifiers;
       identifiers["modelname"]="chan_stage";
       identifiers["channel"]="1";
       identifiers["dist"]="3000";
       // Channel length is 15000, up and down stage is 1.0,2.0
       oprule::expression::DoubleNodePtr dn
		   = DSM2lookup.getModelExpression("chan_stage",identifiers);
       BOOST_CHECK_CLOSE( dn->eval(), 1.4, TOLER );
       // Channel length is 5000, 8 is the last comp. point
       identifiers["channel"]="4";
       identifiers["dist"]="5000";
       oprule::expression::DoubleNodePtr dn2
		   = DSM2lookup.getModelExpression("chan_stage",identifiers);
       BOOST_CHECK_CLOSE( dn2->eval(), 8., TOLER);
       identifiers["channel"]="4";
       identifiers["dist"]="length";
       oprule::expression::DoubleNodePtr dn3
		   = DSM2lookup.getModelExpression("chan_stage",identifiers);
       BOOST_CHECK_CLOSE( dn3->eval(), 8., TOLER);
    }

    void testChannelBadInput(){
       map<string,string> identifiers;
       identifiers["modelname"]="chan_stage";
       identifiers["channel"]="999";
       identifiers["dist"]="3000";
       // Channel length is 15000, up and down stage is 1.0,2.0
       try {
		   oprule::expression::DoubleNodePtr dn
		     = DSM2lookup.getModelExpression("chan_stage",identifiers);
           BOOST_ERROR("Exception InvalidIdentifier is expected");
	   }catch( InvalidIdentifier const& ){
           BOOST_CHECK_MESSAGE( true, "exception InvalidIdentifier is caught" );
	   }
       identifiers["modelname"]="chan_stage";
       identifiers["channel"]="1";
       identifiers["dist"]="99999";
       try {
		   oprule::expression::DoubleNodePtr dn
		     = DSM2lookup.getModelExpression("chan_stage",identifiers);
           BOOST_ERROR("Exception InvalidIdentifier is expected");
	   }catch( InvalidIdentifier const& ){
           BOOST_CHECK_MESSAGE( true, "exception InvalidIdentifier is caught" );
	   }
       identifiers["modelname"]="chan_stage";
       identifiers["channel"]="-1";
       identifiers["dist"]="99999";
       try {
		   oprule::expression::DoubleNodePtr dn
		     = DSM2lookup.getModelExpression("chan_stage",identifiers);
           BOOST_ERROR("Exception InvalidIdentifier is expected");
	   }catch( InvalidIdentifier const& ){
           BOOST_CHECK_MESSAGE( true, "exception InvalidIdentifier is caught" );
	   }
       identifiers["modelname"]="chan_stage";
       identifiers["channel"]="-1";
       identifiers["dist"]="-99";
       try {
		   oprule::expression::DoubleNodePtr dn
		     = DSM2lookup.getModelExpression("chan_stage",identifiers);
           BOOST_ERROR("Exception InvalidIdentifier is expected");
	   }catch( InvalidIdentifier const& ){
           BOOST_CHECK_MESSAGE( true, "exception InvalidIdentifier is caught" );
	   }
	}

    void testTransferInterface(){
       map<string,string> identifiers;
	   identifiers["modelname"]="transfer_flow";
	   identifiers["transfer"]="Transfer 1";

       oprule::rule::ModelInterface<double>::NodePtr msi
          =DSM2lookup.getModelInterface("transfer_flow",identifiers);
       BOOST_CHECK(msi->eval() == 2000);
       msi->set(0.5);
       BOOST_CHECK_CLOSE(msi->eval(), 0.5, TOLER);
    }

    void testTransferInterfaceBadInput(){
       map<string,string> identifiers;
	   identifiers["modelname"]="transfer_flow";
	   identifiers["transfer"]="TXXXansfer 1";
       try {
  	      oprule::expression::DoubleNodePtr dn
		     = DSM2lookup.getModelExpression("transfer_flow",identifiers);
          BOOST_ERROR("Exception InvalidIdentifier is expected");
	   }catch( InvalidIdentifier const& ){
           BOOST_CHECK_MESSAGE( true, "exception InvalidIdentifier is caught" );
	   }
    }

    void testTimeSeriesLookup(){
       map<string,string> identifiers;
	   identifiers["modelname"]="ts";
	   identifiers["name"]="TS2";
		oprule::expression::DoubleNodePtr dn2
		     = DSM2lookup.getModelExpression("ts",identifiers);
       BOOST_CHECK_CLOSE( dn2->eval(),2., TOLER);
    }

private:
   DSM2HydroYearNode year;
   DSM2HydroMonthNode month;
   DSM2HydroDayNode day;
   DSM2HydroMinuteNode min;
   DSM2HydroNamedValueLookup DSM2lookup;
   DSM2HydroTimeNodeFactory DSM2TimeNodeFactory;
};


struct ModelInterfaceTestSuite : public test_suite {
    ModelInterfaceTestSuite() : test_suite("ModelInterfaceTestSuite") {
        // add member function test cases to a test suite
        boost::shared_ptr<ModelInterfaceTest> instance( new ModelInterfaceTest() );
        add( BOOST_CLASS_TEST_CASE( &ModelInterfaceTest::testDate, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelInterfaceTest::testGateDeviceIndex, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelInterfaceTest::testGateDeviceInterface, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelInterfaceTest::testGateDeviceInterfaceBadInput, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelInterfaceTest::testGateInstallInterface, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelInterfaceTest::testGateInstallInterfaceBadInput, instance ));

        add( BOOST_CLASS_TEST_CASE( &ModelInterfaceTest::testFlowNode, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelInterfaceTest::testChannelBadInput, instance ));

        add( BOOST_CLASS_TEST_CASE( &ModelInterfaceTest::testWaterSurfaceNode, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelInterfaceTest::testTransferInterface, instance ));
        add( BOOST_CLASS_TEST_CASE( &ModelInterfaceTest::testTransferInterfaceBadInput, instance ));

        add( BOOST_CLASS_TEST_CASE( &ModelInterfaceTest::testTimeSeriesLookup, instance ));

    }
};

test_suite*
init_unit_test_suite( int argc, char * argv[] ) {
    std::auto_ptr<test_suite> test( BOOST_TEST_SUITE( "Test for model state interface" ) );
    test->add( new ModelInterfaceTestSuite() );
    return test.release();
}

// EOF
