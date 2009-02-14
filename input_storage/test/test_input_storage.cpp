#include "input_storage.h"
#include "InputState.h"
#include "FileInputState.h"
#include "ItemInputState.h"
#include "InsertFileState.h"
#include "EnvSubstitution.h"
#include <stdlib.h>
#include <sstream>
#include <iostream>
#include <functional>
#include <algorithm>

#include <boost/test/unit_test.hpp>


using namespace boost::unit_test;


void test_envvar()
{
  EnvSubstitution sub;
#ifdef _WIN32
  _putenv_s("TESTEXPR","answer");
#else
  setenv("TESTEXPR","answer",0);
#endif
  sub.add("_@EXPR%ESS1","one");
  sub.add("NESTED_1","nested ${_@EXPR%ESS1}");
  string teststr("a test the ${TESTEXPR} now again ${TESTEXPR} now another ${_@EXPR%ESS1} and a ${NESTED_1} and the end.");
  string out = sub(teststr);
  std::cout << out << std::endl;
  sub.add("TESTEXPR","newanswer");
  out = sub(teststr);
  std::cout << out << std::endl;
  sub.add("TESTEXPR","redefine");  
  out = sub(teststr);
  std::cout << out << std::endl;
}


/** Test streaming output and input of classes with various
    constituent data types
*/
void test_stream()
{
  channel cOut = channel(1, 0.0022, 2,3);
  ostringstream os;
  os << cOut;
  string outStr = os.str();
  channel cIn; 
  istringstream is(outStr);
  is >> cIn;
  
  BOOST_CHECK( !(cIn < cOut || cOut < cIn));
  BOOST_CHECK( cIn.manning == cOut.manning);
  BOOST_CHECK( cIn.upnode == cOut.upnode);
  BOOST_CHECK( cIn.downnode == cOut.downnode);
  BOOST_CHECK( cIn.chan_no == cOut.chan_no);
  BOOST_CHECK( cIn.identifier() == cOut.identifier());
  BOOST_CHECK( cIn.layer == cOut.layer);
}

/** Test streaming with bad input //todo: need more of this
*/
void test_bad_stream_input()
{
  string datastring("12 0.0022 3.4 5");
  istringstream is(datastring);
  channel chan;
  BOOST_CHECK_THROW(is >> chan, invalid_argument);

  datastring="12 0.0022 apple 5";
  is.clear();
  is.str(datastring);
  BOOST_CHECK_THROW(is >> chan, invalid_argument);

}


void test_setup_buffer()
{ 
    //Create a new file using default properties.
    channel_clear_buffer_f();

    int channo=525;  // deliberately out of order
    int layer=2;
    double manning=0.035;
    int upnode=5;
    int downnode=6;
    channel_append_to_buffer_f(&channo,&manning,&upnode,&downnode);

    channo = 1;
    layer = 1;
    manning=0.035;
    upnode = 1;
    downnode = 2;
    channo=1;
    channel_append_to_buffer_f(&channo,&manning,&upnode,&downnode);
    channo=2;
    layer=1;
    upnode=2;
    downnode = 3;
    channel_append_to_buffer_f(&channo,&manning,&upnode,&downnode);
    channo=2;
    layer=2;
    manning=0.04;    
    channel_append_to_buffer_f(&channo,&manning,&upnode,&downnode);
    channo=3;
    layer=1;
    manning=0.035;
    upnode=3;
    downnode = 4;
    channel_append_to_buffer_f(&channo,&manning,&upnode,&downnode);
    channo=14;
    layer=1;
    manning=0.035;
    upnode=4;
    downnode = 5;
    channel_append_to_buffer_f(&channo,&manning,&upnode,&downnode);

    xsect_clear_buffer_f();
    channo=1;
    layer=1;
    double dist = 0.800;
    char* file = "1_0_800.txt";
    xsect_append_to_buffer_f(&channo,&dist,file, 11);

    channo=1;
    layer=1;
    dist = 0.200;   // deliberately out of order
    file = "1_0_200.txt";
    xsect_append_to_buffer_f(&channo,&dist,file, 11);

    channo = 2;
    layer=1;
    dist = 0.200;
    file = "2_0_200.txt";
    xsect_append_to_buffer_f(&channo,&dist,file, 11);
    channo = 2;
    layer=2;
    dist = 0.200;
    file = "2_0_200b.txt";
    xsect_append_to_buffer_f(&channo,&dist,file, 11);
    channo = 2;
    layer=2;
    dist = 0.400;
    file = "2_0_400b.txt";
    xsect_append_to_buffer_f(&channo,&dist,file, 11);
    channo = 3;
    layer=1;
    dist = 0.200;
    file = "3_0_200.txt";
    xsect_append_to_buffer_f(&channo,&dist,file, 11);
    channo = 14;
    layer=1;
    dist = 0.200;
    file = "14_0_200.txt";
    xsect_append_to_buffer_f(&channo,&dist,file, 11);
    channo = 525;
    layer=1;
    dist = 0.200;
    file = "525_0_200.txt";
    xsect_append_to_buffer_f(&channo,&dist,file, 11);
    //todo: what if there were accidently a chan 525 xsect on level 2?
}

void test_prioritize_buffer()
{
  cout << "Testing priorities" << endl;
  //channel_prioritize_buffer();
  //xsect_prioritize_buffer();
}


// Test writing and read of buffer to hdf5. This test is dependent on test_setup_buffer
void test_hdf()
{
    cout << "Testing hdf" <<endl;
    hid_t file_id = H5Fcreate( "chan.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );
    channel_write_buffer_to_hdf5_f(&file_id);
    xsect_write_buffer_to_hdf5_f(&file_id);
    channel_clear_buffer_f();
    channel_read_buffer_from_hdf5_f(&file_id);;

    int channo_in=-2;
    double manning_in=0.0;
    int layer_in = -1;
    int upnode_in = -1;
    int use_it=0;
    //cout << channel_query_from_buffer(2,&channo_in,&layer_in,&manning_in,&upnode_in) <<endl;
    //cout<< channo_in << " " << layer_in << " " << manning_in << " " << upnode_in << " " << endl;
    //Close the file. 
    H5Fclose( file_id );
}


void test_input_reader()
{
  HDFTableManager<channel>::instance().buffer().clear();
  HDFTableManager<xsect>::instance().buffer().clear();

  vector<string> contextItems;
  contextItems.push_back("CHANNEL");
  contextItems.push_back("XSECT");
  contextItems.push_back("RESERVOIR");
  contextItems.push_back("ENVVAR");

  vector<string> noSubItems;
  map<string, InputStatePtr> emptyMap;
  map<string, InputStatePtr> inputMap;
  InputStatePtr channelPtr(new ItemInputState<channel>());
  InputStatePtr xsectPtr(new ItemInputState<xsect>());
  InputStatePtr gatePtr(new ItemInputState<channel>());
  InputStatePtr resPtr(new ItemInputState<channel>());
  InputStatePtr envPtr(new ItemInputState<envvar>());
  InputStatePtr includePtr(new InsertFileState(inputMap,contextItems));

  inputMap["CHANNEL"] = channelPtr;
  inputMap["XSECT"]   = xsectPtr;
  inputMap["GATE"] = gatePtr;
  inputMap["RESERVOIR"] = resPtr;
  inputMap["ENVVAR"] = envPtr;
  inputMap["INCLUDE"] = includePtr;

  vector<string> envvarActive;
  envvarActive.push_back("ENVVAR");

  vector<string> active;
  active.push_back("CHANNEL");
  active.push_back("XSECT");
  active.push_back("INCLUDE");

  InputStatePtr startState(new FileInputState(inputMap,contextItems,"test.txt"));
  InputStatePtr currentState(startState);


  ifstream input("test.txt");
  // First pass with envvars
  currentState->setActiveItems(envvarActive);
  while (!currentState->isEndOfFile())
    {
      currentState = currentState->process(input);
    }

  cout<<"\nENVVARS" << endl;
  EnvSubstitution sub;
  vector<envvar> &envvars = HDFTableManager<envvar>::instance().buffer();


  for ( size_t i = 0 ; i < envvars.size();i++)
    {
      sub.add(envvars[i].name, envvars[i].value);
    }

  for(InputState::InputStateMap::iterator it = inputMap.begin();
      it != inputMap.end() ; ++it)
    {
      it->second->setEnvSubstitution(sub);
    }

  //rewind
  input.clear();
  input.seekg(0, ios::beg);
  input.clear();


  currentState=startState;
  currentState->setActiveItems(active);

  while (!currentState->isEndOfFile())
    {
      currentState = currentState->process(input);
    }

  vector<channel> & chans = HDFTableManager<channel>::instance().buffer();

  channel_prioritize_buffer_f();
  xsect_prioritize_buffer_f();

  cout << "# Channels=" << chans.size()<< endl;
  for (size_t i = 0 ; i < chans.size() ; ++ i)
    {
      cout << chans[i] << endl;
    }

  vector<xsect> & xsects = HDFTableManager<xsect>::instance().buffer();
  cout << "# Xsect=" << xsects.size()<< endl;
  for (size_t  i = 0 ; i < xsects.size() ; ++ i)
    {
      cout << xsects[i] << endl;
    }
}



void test_bad_input()
{
  envvar env;
  istringstream in("superlongnamesolongitshouldcertainlycauseanerror  modestlengthvalue");  
  //BOOST_CHECK_THROW( in >> env;,  std::logic_error )
}


test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    test_suite* test= BOOST_TEST_SUITE( "io generation tests" );
    test->add( BOOST_TEST_CASE( &test_envvar ) );
    test->add( BOOST_TEST_CASE( &test_stream ) );
    test->add( BOOST_TEST_CASE( &test_bad_stream_input ) );
    test->add( BOOST_TEST_CASE( &test_setup_buffer ) );
    test->add( BOOST_TEST_CASE( &test_prioritize_buffer ) );
    test->add( BOOST_TEST_CASE( &test_hdf ) );
    test->add( BOOST_TEST_CASE( &test_input_reader ) );
    test->add( BOOST_TEST_CASE( &test_bad_input ) );

    return test;
}

// EOF
