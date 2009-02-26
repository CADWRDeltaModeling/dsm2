#include "input_storage.h"
#include "InputState.h"
#include "FileInputState.h"
#include "ItemInputState.h"
#include "InsertFileState.h"
#include "EnvSubstitution.h"
#include "ApplicationTextReader.h"
#include <stdlib.h>
#include <sstream>
#include <iostream>
#include <functional>
#include <algorithm>

#include <boost/test/unit_test.hpp>
#include<boost/filesystem/operations.hpp>


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

/** Add thing to the buffer using the FORTRAN API for appends.
    todo: the fortran API does not add layer numbers, so this
    function doesn't work when there are duplicate entries -- 
    prioritize_buffer will throw an exception
*/
void append_buffer_fortran_api()
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
    string file = "1_0_800.txt";
    xsect_append_to_buffer_f(&channo,&dist,file.c_str(), 11);

    channo=1;
    layer=1;
    dist = 0.200;   // deliberately out of order
    file = "1_0_200.txt";
    xsect_append_to_buffer_f(&channo,&dist,file.c_str(), 11);

    channo = 2;
    layer=1;
    dist = 0.200;
    file = "2_0_200.txt";
    xsect_append_to_buffer_f(&channo,&dist,file.c_str(), 11);
    channo = 2;
    layer=2;
    dist = 0.200;
    file = "2_0_200b.txt";
    xsect_append_to_buffer_f(&channo,&dist,file.c_str(), 11);
    channo = 2;
    layer=2;
    dist = 0.400;
    file = "2_0_400b.txt";
    xsect_append_to_buffer_f(&channo,&dist,file.c_str(), 11);
    channo = 3;
    layer=1;
    dist = 0.200;
    file = "3_0_200.txt";
    xsect_append_to_buffer_f(&channo,&dist,file.c_str(), 11);
    channo = 14;
    layer=1;
    dist = 0.200;
    file = "14_0_200.txt";
    xsect_append_to_buffer_f(&channo,&dist,file.c_str(), 11);
    channo = 525;
    layer=1;
    dist = 0.200;
    file = "525_0_200.txt";
    xsect_append_to_buffer_f(&channo,&dist,file.c_str(), 11);
    
    //todo: what if there were accidently a chan 525 xsect on level 2?
}



/** Predictable test fixture for channels and cross sections. The 
    buffer that is set up here includes some overridden items, so it 
    is necessary to prioritize the buffer.
*/
void setup_buffer()
{ 
    //Create a new file using default properties.
    channel_clear_buffer_f();
    
    vector<channel> & cbuffer = HDFTableManager<channel>::instance().buffer();
    cbuffer.push_back(channel(525,0.035,5,6,true,1));
    cbuffer.push_back(channel(1,0.035,1,2,true,0));
    cbuffer.push_back(channel(2,0.035,2,3,true,0));
    cbuffer.push_back(channel(2,0.04,2,3,true,1));
    cbuffer.push_back(channel(3,0.035,3,4,true,0));
    cbuffer.push_back(channel(14,0.35,4,5,true,0));

    xsect_clear_buffer_f();
    vector<xsect> & xbuffer = HDFTableManager<xsect>::instance().buffer();
    string file = "1_0_800.txt";
    int channo=1;
    int layer = 0;
    double dist = 0.800;
    bool use = true;
    xbuffer.push_back(xsect(channo,dist,file.c_str(),use,layer));

    channo=1;
    layer=0;
    dist = 0.200;   // deliberately out of order
    file = "1_0_200.txt";
    xbuffer.push_back(xsect(channo,dist,file.c_str(),use,layer));

    channo = 2;
    layer=1;
    dist = 0.200;
    file = "2_0_200.txt";
    xbuffer.push_back(xsect(channo,dist,file.c_str(),use,layer));
    channo = 2;
    layer=2;
    dist = 0.200;
    file = "2_0_200b.txt";
    xbuffer.push_back(xsect(channo,dist,file.c_str(),use,layer));
    channo = 2;
    layer=2;
    dist = 0.400;
    file = "2_0_400b.txt";
    xbuffer.push_back(xsect(channo,dist,file.c_str(),use,layer));
    channo = 3;
    layer=1;
    dist = 0.200;
    file = "3_0_200.txt";
    xbuffer.push_back(xsect(channo,dist,file.c_str(),use,layer));
    channo = 14;
    layer=1;
    dist = 0.200;
    file = "14_0_200.txt";
    xbuffer.push_back(xsect(channo,dist,file.c_str(),use,layer));
    channo = 525;
    layer=1;
    dist = 0.200;
    file = "525_0_200.txt";
    xbuffer.push_back(xsect(channo,dist,file.c_str(),use,layer));
    
    //todo: what if there were accidently a chan 525 xsect on level 2?
}

void test_prioritize_buffer_duplicates()
{
    setup_buffer();
    // deliberately add a duplicate of the first entry at the bottom
    int channo=525;
    int layer=2;
    double manning=0.035;
    int upnode=5;
    int downnode=6;
    channel_append_to_buffer_f(&channo,&manning,&upnode,&downnode);
    BOOST_CHECK_THROW(channel_prioritize_buffer_f(),runtime_error);
}



void test_setup_buffer()
{
   setup_buffer();
}


void test_prioritize_buffer()
{
  cout << "Testing priorities" << endl;
  channel_prioritize_buffer_f();
  xsect_prioritize_buffer_f();
}




// Test writing and read of buffer to hdf5. This test is dependent on test_setup_buffer
void test_hdf()
{
    cout << "Testing hdf" <<endl;
    hid_t file_id = H5Fcreate( "test_cpp.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );
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
    cout << "Done testing hdf" <<endl;

}


void test_input_reader()
{
  cout << "Testing input reader" <<endl;

  HDFTableManager<channel>::instance().buffer().clear();
  HDFTableManager<xsect>::instance().buffer().clear();
  HDFTableManager<envvar>::instance().buffer().clear();

  vector<string> contextItems;
  contextItems.push_back("CHANNEL");
  contextItems.push_back("XSECT");
  contextItems.push_back("ENVVAR");

  vector<string> noSubItems;
  map<string, InputStatePtr> inputMap;
  inputMap["CHANNEL"] = InputStatePtr(new ItemInputState<channel>());
  inputMap["XSECT"]   = InputStatePtr(new ItemInputState<xsect>());
  inputMap["ENVVAR"]  = InputStatePtr(new ItemInputState<envvar>());
  inputMap["INCLUDE"] = InputStatePtr(new InsertFileState(contextItems));
  ApplicationTextReader::instance().setInputStateMap(inputMap);


  vector<string> envvarActive;
  envvarActive.push_back("ENVVAR");

  string filename("test.txt");
  boost::filesystem::path p(filename);
  BOOST_CHECK(boost::filesystem::exists(p));
  std::ifstream input(filename.c_str());

  InputStatePtr startState(new FileInputState(contextItems,filename));
  InputStatePtr currentState(startState);

  // First pass with envvars
  
  currentState->setActiveItems(envvarActive);
  while (!currentState->isEndOfFile())
    {
      currentState = currentState->process(input);
    }
  input.close();
  
  vector<envvar> &envvars = HDFTableManager<envvar>::instance().buffer();

  EnvSubstitution sub;
  vector<string> active;
  active.push_back("CHANNEL");
  active.push_back("XSECT");
  active.push_back("INCLUDE");

  
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

  //InputStatePtr currentState(startState);
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
  cout << "Done testing input reader" <<endl;

  inputMap.clear();
  contextItems.clear();
}

void test_input_reader_duplicate()
{
  cout << "Testing input reader duplicates" <<endl;
  HDFTableManager<channel>::instance().buffer().clear();
  HDFTableManager<xsect>::instance().buffer().clear();
  HDFTableManager<envvar>::instance().buffer().clear();

  vector<string> contextItems;
  contextItems.push_back("CHANNEL");
  contextItems.push_back("XSECT");
  contextItems.push_back("ENVVAR");

  map<string, InputStatePtr> inputMap;
  inputMap["ENVVAR"]  = InputStatePtr(new ItemInputState<envvar>());
  inputMap["CHANNEL"] = InputStatePtr(new ItemInputState<channel>());
  inputMap["XSECT"]   = InputStatePtr(new ItemInputState<xsect>());
  ApplicationTextReader::instance().setInputStateMap(inputMap);
  vector<string> active;
  active.push_back("ENVVAR");
  active.push_back("CHANNEL");
  active.push_back("XSECT");
  ApplicationTextReader::instance().setActiveItems(active);
  string filename("test_duplicates.txt");
  ApplicationTextReader::instance().processInput(filename);

  vector<envvar> & envvars =  HDFTableManager<envvar>::instance().buffer();
  // check that it finds duplicates in strangely ordered character identifier
  BOOST_CHECK_THROW(envvar_prioritize_buffer_f(), runtime_error);
  //for(size_t i =0; i < envvars.size() ; ++i) cerr << envvars[i] <<endl;

  // and with integer identifier
  vector<channel> & chans = HDFTableManager<channel>::instance().buffer();
  BOOST_CHECK_THROW(channel_prioritize_buffer_f(), runtime_error);
  //xsect_prioritize_buffer_f();

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
    test->add( BOOST_TEST_CASE( &test_input_reader_duplicate ) );
    test->add( BOOST_TEST_CASE( &test_bad_input ) );

    return test;
}

// EOF
