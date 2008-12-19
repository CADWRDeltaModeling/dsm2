// Boost.Test
#include <boost/test/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>

using boost::unit_test_framework::test_suite;
using namespace std;

// STL
#include <iostream>
#include <string>
#include <vector>
#include "oprule/expression/ValueNode.h"
#include "oprule/parser/NamedValueLookup.h"
#include "oprule/rule/ModelInterface.h"
#include "oprule/rule/OperatingRule.h"
#include "oprule/parser/ModelTimeNodeFactory.h"
#include "oprule/parser/ParseSymbolManagement.h"
#include "ParserTestFixture.h"  //some classes associated with test fixture

/*
extern BoolNodePtr getBoolExpression(    const char * name = 0);
extern DoubleNodePtr getDoubleExpression(const char * name = 0);
extern OperatingRule* getOperatingRule();
extern void init_lookup( NamedValueLookup*);
extern void init_model_time_factory(ModelTimeNodeFactory*);
extern void init_expression();
extern void add_expression(const std::string&, DoubleNodePtr);

extern void init_rule_names();
extern void lexer_init();
*/
extern void set_input_string(string & input);
extern int op_ruleparse(void);


#define DOUBLETOL 1E-8
void clear_temp_expr(void);

void app_init_expression(){
      init_expression();
      init_rule_names();
      string tsname("tsname");
      DoubleNodePtr tsnode = DoubleScalarNode::create(2.);
      add_symbol(tsname,symbol(tsnode));
      string ifname("interface");
      DoubleNodePtr tifnode = TestModelInterface::create();
      add_symbol(ifname,symbol(tifnode));
      string tsname4("tsname4");
      DoubleNodePtr tsnode4 = DoubleScalarNode::create(4.);
      add_symbol(tsname4,symbol(tsnode4));
}

class ParserTest {

public:   
   ParserTest()
       : testLookup(new TestNamedValueLookup),
         timeFactory(new TrivialModelTimeNodeFactory(2001,4,15,7,1))
   {
      lexer_init();
      app_init_expression();
      init_lookup(testLookup);
      init_model_time_factory(timeFactory);
   }

	~ParserTest(){
      clear_temp_expr();
      clear_arg_map();
      init_expression();
      init_rule_names();
      delete testLookup;
      delete timeFactory;
    }


////////////////////////////////////////
   // Helper that includes the original string which
   // Prints a better message on failure BOOST_CHECK_CLOSE
   template<typename T>
   T expression_result(const char* expression, const T realresult){
      return realresult;
   }


#define DTEST(INPUT,RESULT)   \
      instr=INPUT;            \
      set_input_string(instr);      \
	  parseok=op_ruleparse();          \
	  BOOST_CHECK(parseok == 0);       \
      BOOST_CHECK_CLOSE( expression_result( \
      INPUT,getDoubleExpression()->eval()), RESULT, DOUBLETOL); \

   
    void testParseNumeric(){
      int parseok=88; //init to failure
      std::string instr;

      instr=" 30.0;";
      set_input_string(instr);
	  parseok=op_ruleparse();
	  BOOST_CHECK(parseok == 0);
      BOOST_CHECK_CLOSE( expression_result(" 30.0;",
                         getDoubleExpression()->eval()), 30., 1.e-8);

      DTEST(" 30.0;", 30.);
      DTEST(" (20.0 + 10.);", 30.);
      DTEST(" (20. - 10.);", 10.);
      DTEST("(((20. - 10.)));", 10.);
      DTEST(" (2.0 * 10.);", 20.);
      DTEST(" (20. / 10.);", 2.);
      DTEST(" sqrt(9.);", 3.);
      DTEST(" -20. / 10.;", -2.);

      DTEST(" ( sqrt(9.) / 3.);", 1.);
      DTEST("3. + 3. + 1.;", 7.);
      DTEST(" (-20.0 + -10)/3. + 3. /3.+1.;",-8.);
      DTEST(" -(20. - 10.0)/5.;", -2.);
      DTEST("2. + 2 * 2 * 2 - 2 / 2 - 2 / 2;", 8.);
      DTEST("exp(ln(9));",9.);
      DTEST("IFELSE( 2>3, 5, 6);", 6.);
      DTEST("IFELSE( 3>2, 5, 6);", 5.);
      DTEST("ln(exp(3));",3.);
      DTEST("sqrt(3^2.);",3.);
      DTEST("(sqrt(3.0))^2.;",3.0);
      DTEST("(sqrt(4.0));",2.0);
      DTEST("10^log(3.);",3.0);
	  DTEST("max2(3.,2.);", 3.0);
	  DTEST("min2(3.,2.);", 2.0);
	  DTEST("max2(2.,3.);", 3.0);
	  DTEST("min2(2.,3.);", 2.0);
	  DTEST("max3(3.,4.,1.);",4.0);
	  DTEST("max3(3.,1.,4.);",4.0);
	  DTEST("max3(4.,1.,3.);",4.0);
	  DTEST("max3(3.,1.,3.);",3.0);
	  DTEST("min3(3.,1.,4.);",1.0);
	  DTEST("min3(3.,1.,4.);",1.0);
	}

    void testLookupNumeric(){
      int parseok=88; //init to failure
      std::string instr;

      DTEST("LookupName*5*test_state(first_arg=4;second_arg='quoted string');",20.0); 
	}


    void testParseMissingIdentifier(){
      int parseok=88;
      std::string instr;

      instr="LookupName*5*test_state(first_xxx=4;second_arg='quoted string');";
      set_input_string(instr);
      try{
		 BOOST_MESSAGE("****  Parsing error messages below are expected");
	     parseok=op_ruleparse();
		 BOOST_MESSAGE("\n****  End of expected error messages.");
         BOOST_CHECK(parseok != 0);
      }catch(std::logic_error e){
         BOOST_FAIL("Parser should handle error, no exception here.");         
      }
    }


#define BTEST(INPUT,RESULT)   \
      instr=INPUT;            \
      set_input_string(instr);      \
	  parseok=op_ruleparse();          \
	  BOOST_CHECK(parseok == 0);       \
	  BOOST_CHECK( expression_result(INPUT,getBoolExpression()->eval()) == RESULT); \



    void testParseBoolean(){
      int parseok=88; //init to failure
      BoolNodePtr outnode(new BoolScalarNode(true));
      std::string instr;

      BTEST("((20.0 + 10.) <=30.) AND true;", true);
      BTEST("(20.0 + 10.) <=30.;", true);
      BTEST("(20.0 + 10.) >=30.;", true);
      BTEST("(20.0 + 10.) ==30.;", true);
      BTEST("(20.0 + 10.) <> 30.;", false);
      BTEST("(20.0 + 10.) <30.;", false);
      BTEST("(20.0 + 10.) >30.;", false);
      BTEST("((20. / 10.) < 3.);", true);
      BTEST(" true ;", true);
      BTEST("3+2+1 == 1+2+3;", true);
      BTEST("((20. / 10.) < 3.) OR (1. > 3.);", true);
      BTEST("(20. / 10.) > 3. OR (1. < 3.);", true);
      BTEST("(20. / 10.) < 3. OR (1. < 3.);", true);
      BTEST("((20. / 10.) <= 3.) AND (1. < 3.);", true);
      BTEST("(20. / 10.) < 1. and (1. > 3.);", false);
      BTEST("(20. / 10.) < 1. AND (1. < 3.);", false);
      BTEST("(20. / 10.) > 1. AND (1. < 3.);", true);
      BTEST("NOT((20. / 10.) < 3. AND (1. > 3.));", true);
      BTEST(" NOT(false OR false);", true);
      BTEST("true AND true AND true OR false;",true); 
      BTEST("true AND true AND true;",true); 
      BTEST("false OR (true AND true AND true) ;",true);
      BTEST("DATE == 01JAN2004 00:00;", true);
      BTEST("DATE == 01JAN2004;", true);
      BTEST("SEASON == 01JAN 00:00;",true);
      BTEST("SEASON == 01JAN;",true);
    }

    void testParseDate(){
      int parseok=88; //init to failure
      BoolNodePtr outnode(new BoolScalarNode(true));
      std::string instr;
      DTEST(" YEAR+1.;", 2002.);
      DTEST(" MONTH;", 4.);
      DTEST(" DAY +1 ;", 16.);
      DTEST(" HOUR + 1. ;", 8.);
      DTEST(" MIN+1;",2.);
    }

#define D_ASSIGN_TEST(INPUT,RESULT,NAME)    \
      instr=INPUT;                          \
      set_input_string(instr);              \
      parseok=op_ruleparse();               \
      BOOST_CHECK(parseok == 0);            \
      BOOST_CHECK_CLOSE( expression_result( \
      INPUT,getDoubleExpression(NAME)->eval()), RESULT, DOUBLETOL);

#define B_ASSIGN_TEST(INPUT,RESULT,NAME)   \
      instr=INPUT;                         \
      set_input_string(instr);             \
	  parseok=op_ruleparse();              \
	  BOOST_CHECK(parseok == 0);           \
      BOOST_CHECK( expression_result(      \
      INPUT,getBoolExpression(NAME)->eval()) == RESULT);




    void testParseAssign(){
      app_init_expression();
      int parseok=88; //init to failure
      std::string instr;
      D_ASSIGN_TEST("A:= 7.25+3.75;", 11., "A");
      instr="B:= A+1.;";
      set_input_string(instr);
      parseok=op_ruleparse();
      BOOST_CHECK(parseok == 0);
      BOOST_CHECK_CLOSE( expression_result(
          "B:= A+1.;",getDoubleExpression("B")->eval()), 12.0, 1.e-08);

      //D_ASSIGN_TEST("B:= A+1.;",12.,"B")
      B_ASSIGN_TEST("ABool:= (3.*A== 33.);",true,"ABool")      
      B_ASSIGN_TEST("BBool:= ABool AND (B+B + 2*B)==48.;",true,"BBool")  
      B_ASSIGN_TEST("DBool:= NOT ABool AND (B+B + 2*B)==48.;",false,"DBool")  
      B_ASSIGN_TEST("1.==1.;",true,"BBool")  
      B_ASSIGN_TEST("CBool:= BBool;",true,"CBool")
      clear_temp_expr();
      init_expression();
    }

    void testParseLagged(){
      app_init_expression();
      int parseok=88; //init to failure
      std::string instr;
      D_ASSIGN_TEST("A:= 7.25+3.75;", 11., "A");
      instr="B:= A+1.;";
      set_input_string(instr);
      parseok=op_ruleparse();
      BOOST_CHECK(parseok == 0);
      BOOST_CHECK_CLOSE( expression_result(
          "B:= A+1.;",getDoubleExpression("B")->eval()), 12.0, 1.e-08);

      //D_ASSIGN_TEST("B:= A+1.;",12.,"B")
      D_ASSIGN_TEST("D:= C(t-1)+B;",12.,"D")
      clear_temp_expr();
      init_expression();
    }



    void testParseModelTime(){
      app_init_expression();
      int parseok=88; //init to failure
      DoubleNodePtr doutnode;
      BoolNodePtr boutnode;
      std::string instr;
      vector<BoolNodePtr> bnodes;

      B_ASSIGN_TEST("BBool:= MONTH <= APR;",true,"BBool")
      B_ASSIGN_TEST("CBool:= MONTH > 4.1;",false,"CBool")
    }




    void testParseAccumulate(){
      //app_init_expression();
      int parseok=88; //init to failure
      DoubleNodePtr doutnode;
      std::string instr("ACCUMULATE(3.,1.);");

      set_input_string(instr);
	   parseok=op_ruleparse();
		BOOST_CHECK(parseok == 0);
      doutnode=getDoubleExpression();
      doutnode->init();
      BOOST_CHECK_CLOSE( doutnode->eval(), 1.,DOUBLETOL);
      doutnode->step(1.); //todo: this should not be a 1, doesn't test dt vs #step
      BOOST_CHECK_CLOSE( doutnode->eval(), 4.,DOUBLETOL);
      doutnode->step(1.);
      BOOST_CHECK_CLOSE( doutnode->eval(), 7.,DOUBLETOL);
      
      instr="ACCUMULATE(3.,1.,FALSE);";
      set_input_string(instr);
	   parseok=op_ruleparse();
		BOOST_CHECK(parseok == 0);
      doutnode=getDoubleExpression();
      doutnode->init();
      BOOST_CHECK_CLOSE( doutnode->eval(), 1.,DOUBLETOL);
      //todo: doesn't expose difference between dt and #steps
      doutnode->step(1.);
      BOOST_CHECK_CLOSE( doutnode->eval(), 4.,DOUBLETOL);
      doutnode->step(1.);
      BOOST_CHECK_CLOSE( doutnode->eval(), 7.,DOUBLETOL);
      //delete doutnode;

      instr="ACCUMULATE(3.,1.,TRUE);";
      set_input_string(instr);
	   parseok=op_ruleparse();
		BOOST_CHECK(parseok == 0);
      doutnode=getDoubleExpression();
      doutnode->init();
      BOOST_CHECK_CLOSE( doutnode->eval(), 1.,DOUBLETOL);
      doutnode->step(1.);
      BOOST_CHECK_CLOSE( doutnode->eval(), 4.,DOUBLETOL);

    }


    void testParseLookup(){
      int parseok=88; //init to failure
      std::string instr;
	  DTEST("lookup(3.,[1.,3.,7.],[1.,4.]);",4.0);
	  DTEST("lookup(2.,[1.,3.,9.],[1.,4.]);",1.0);
	  DTEST("lookup(60.,[10.,30.,90.],[1.,4.]);",4.0);

      instr="lookup(60.,[10.,30.],[1.,4.]);";
      set_input_string(instr);
      
      try{
		 //BOOST_TEST_MESSAGE("****  Parsing error messages below are expected");
	     parseok=op_ruleparse(); 
		 //BOOST_TEST_MESSAGE("\n****  End of expected error messages.");
         BOOST_CHECK(parseok != 0);
      }catch(std::domain_error e){
		  BOOST_TEST_MESSAGE("\n****  End of expected error messages.");
         // BOOST_FAIL("Parser handled error, no exception here.");         
      }

      instr="lookup(60.,[10.,30.,55.],[1.,4.]);";
      set_input_string(instr);
	  try{
		 BOOST_MESSAGE("****  Parsing error messages below are expected");
	     parseok=op_ruleparse(); 
		 BOOST_MESSAGE("\n****  End of expected error messages.");
         BOOST_CHECK(parseok != 0);
      }catch(std::domain_error e){
         BOOST_MESSAGE("\n****  End of expected error messages.");
		  //BOOST_FAIL("Parser handled error, no exception here.");         
      }
      init_expression();
      clear_temp_expr();
	}

    void testParsePredict(){
      //app_init_expression();
      int parseok=88; //init to failure
      DoubleNodePtr doutnode;
      std::string instr("PREDICT(3., LINEAR, 15MIN);");

      set_input_string(instr);
	   parseok=op_ruleparse();
		BOOST_CHECK(parseok == 0);
      doutnode=getDoubleExpression();
      doutnode->init();
      BOOST_CHECK_CLOSE( doutnode->eval(), 3.,DOUBLETOL);
      doutnode->step(0.);
      BOOST_CHECK_CLOSE( doutnode->eval(), 3.,DOUBLETOL);
      doutnode->step(0.);
      BOOST_CHECK_CLOSE( doutnode->eval(), 3.,DOUBLETOL);

      //delete doutnode;
      
      resetInterfaces();
      TestModelInterface::val=4.;
      instr="PREDICT(interface, QUAD, 30MIN);";
      set_input_string(instr);
	   parseok=op_ruleparse();
		BOOST_CHECK(parseok == 0);
      doutnode=getDoubleExpression();
      doutnode->init();
      BOOST_CHECK_CLOSE( doutnode->eval(), 4.,DOUBLETOL);
      TestModelInterface::val=5.;      
      doutnode->step(0.);
      BOOST_CHECK_CLOSE( doutnode->eval(), 10.,DOUBLETOL);
      TestModelInterface::val=8.;      
      doutnode->step(0.);
      BOOST_CHECK_CLOSE( doutnode->eval(), 20.,DOUBLETOL);
      clear_temp_expr();
    }

    void testParseRule(){
        int junk=0;
        string testRule=
           "myrule0 := SET test_interface(first_arg='quoted string with spaces' ,"
           "second_arg=0, third_arg=unquoted ) TO 3. WHEN true;";
        set_input_string(testRule);
        int parseok=op_ruleparse();
        BOOST_CHECK(parseok==0);
        OperatingRulePtr rule=getOperatingRule();
        BOOST_CHECK( rule->testTrigger() );
        rule->setActive(true);
        rule->advanceAction(HUGE_VAL);
        BOOST_CHECK_CLOSE(TestModelInterface::val,3.,DOUBLETOL);
        init_expression();
        init_rule_names();
        clear_temp_expr();
    }

    void testParseRuleWithActionSet(){ 
       string testRule=
           "myrule1 := (SET test_interface(first_arg='quoted string with spaces' ,"
           "second_arg=0, third_arg=unquoted ) TO 3. WHILE "
           "SET second_interface(first_arg=0) TO 4. ) WHEN true;";
        set_input_string(testRule);
        int parseok=op_ruleparse();
        BOOST_CHECK(parseok==0);
        OperatingRulePtr rule=getOperatingRule("myrule1");
        BOOST_CHECK( rule->testTrigger() );
        rule->setActive(true);
        rule->advanceAction(HUGE_VAL);
        BOOST_CHECK_CLOSE(TestModelInterface::val,3.,DOUBLETOL);
        BOOST_CHECK_CLOSE(TestModelInterface2::val,4.,DOUBLETOL);
        init_expression();
        init_rule_names();
        clear_temp_expr();
    }

    void testParseRuleWithActionChain(){
       resetInterfaces();
       string testRule=
           "myrule3 := SET test_interface(first_arg='quoted string with spaces' ,"
           "second_arg=0, third_arg=unquoted ) TO 13. WHILE "
		   "SET second_interface(first_arg=0) TO 14. THEN "
		   "SET third_interface(first_arg=0) TO 15. "
           "THEN SET second_interface(first_arg=0) TO 15. WHEN true;";
        set_input_string(testRule);
        int parseok=op_ruleparse();
        BOOST_CHECK(parseok==0);
        OperatingRulePtr rule=getOperatingRule();
        BOOST_CHECK( rule->testTrigger() );
        rule->setActive(true);
        rule->advanceAction(HUGE_VAL);
        BOOST_CHECK_CLOSE(TestModelInterface::val,13.,DOUBLETOL);
        BOOST_CHECK_CLOSE(TestModelInterface2::val,15.,DOUBLETOL);
        BOOST_CHECK_CLOSE(TestModelInterface3::val,15.,DOUBLETOL);

        /* This second pass is a regression test of a problem case */
        resetInterfaces();
        testRule=
           "myrule2 := (SET test_interface(first_arg='quoted string with spaces' ,"
           "second_arg=0, third_arg=unquoted ) TO 13. WHILE "
           "SET second_interface(first_arg=0) TO 14. )  WHEN true;";
        set_input_string(testRule);
        parseok=op_ruleparse();
        BOOST_CHECK(parseok==0);
        rule=getOperatingRule();
        BOOST_CHECK( rule->testTrigger() );
        BOOST_CHECK( rule->getName() == "myrule2");
        rule->setActive(true);
        rule->advanceAction(HUGE_VAL);
        BOOST_CHECK_CLOSE(TestModelInterface::val,13.,DOUBLETOL);
        BOOST_CHECK_CLOSE(TestModelInterface2::val,14.,DOUBLETOL);

        init_expression();
        init_rule_names();
        clear_temp_expr();
	}

private:
   TestNamedValueLookup* testLookup;
   TrivialModelTimeNodeFactory* timeFactory;
};


struct ParserTestSuite : public test_suite {
    ParserTestSuite() : test_suite("ParserTestSuite") {
        // add member function test cases to a test suite
        boost::shared_ptr<ParserTest> instance( new ParserTest() );
        add( BOOST_CLASS_TEST_CASE( &ParserTest::testParseNumeric, instance ));

        add( BOOST_CLASS_TEST_CASE( &ParserTest::testParseMissingIdentifier, instance ));
        add( BOOST_CLASS_TEST_CASE( &ParserTest::testParseBoolean, instance ));

        add( BOOST_CLASS_TEST_CASE( &ParserTest::testParseAccumulate, instance ));
        add( BOOST_CLASS_TEST_CASE( &ParserTest::testParseDate, instance ));

        add( BOOST_CLASS_TEST_CASE( &ParserTest::testParseAssign, instance )); 
        //add( BOOST_CLASS_TEST_CASE( &ParserTest::testParseLagged, instance ));

        add( BOOST_CLASS_TEST_CASE( &ParserTest::testParseLookup, instance ));
        
        
        add( BOOST_CLASS_TEST_CASE( &ParserTest::testParseModelTime, instance ));
        add( BOOST_CLASS_TEST_CASE( &ParserTest::testParseRule, instance));
        add( BOOST_CLASS_TEST_CASE( &ParserTest::testParseRuleWithActionSet, instance));
        add( BOOST_CLASS_TEST_CASE( &ParserTest::testParseRuleWithActionChain, instance));
        
    }
};
 
test_suite*
getParserTestSuite() {

	test_suite* test = BOOST_TEST_SUITE( "Parser test suite" );
    test->add( new ParserTestSuite() );
    return test;
}





// EOF

