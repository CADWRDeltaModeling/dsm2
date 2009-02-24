#pragma warning(disable: 4996)
#include <boost/test/unit_test.hpp>
#include<boost/filesystem/operations.hpp>

#include <sstream>
#include <iostream>
#include <functional>
#include <algorithm>
#include <stdlib.h>

#include "InputState.h"
#include "FileInputState.h"
#include "ItemInputState.h"
#include "InsertFileState.h"
#include "input_storage.h"
#include "EnvSubstitution.h"
#include "ApplicationTextReader.h"

using namespace boost::unit_test;


void setup_envvar()
{
  EnvSubstitution sub;
  sub.add("_@EXPR%ESS1","one");
  sub.add("NESTED_1","nested ${_@EXPR%ESS1}");
  sub.add("TESTEXPR","newanswer");
  sub.add("TESTEXPR","redefine");  
}





// Test writing and read of buffer to hdf5. This test is dependent on test_setup_buffer
void write_hdf()
{
    cout << "Testing hdf" <<endl;
    hid_t file_id = H5Fcreate( "cpp_example.h5", 
		                        H5F_ACC_TRUNC, 
								H5P_DEFAULT, 
								H5P_DEFAULT );
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
  map<string, InputStatePtr> inputMap;
  InputStatePtr channelPtr(new ItemInputState<channel>());
  InputStatePtr xsectPtr(new ItemInputState<xsect>());
  InputStatePtr gatePtr(new ItemInputState<channel>());
  InputStatePtr resPtr(new ItemInputState<channel>());
  InputStatePtr envPtr(new ItemInputState<envvar>());
  InputStatePtr includePtr(new InsertFileState(contextItems));

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

  ApplicationTextReader::instance().setInputStateMap(inputMap);
  InputStatePtr startState(new FileInputState(contextItems,"example.txt"));
  InputStatePtr currentState(startState);

  string filename("test.txt");
  boost::filesystem::path p(filename);
  BOOST_CHECK(boost::filesystem::exists(p));
  std::ifstream input(filename.c_str());


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
  for (size_t i = 0 ; i < xsects.size() ; ++ i)
    {
      cout << xsects[i] << endl;
    }
}




test_suite*
init_unit_test_suite( int argc, char* argv[] )
{
    setup_envvar();
    test_suite* test= BOOST_TEST_SUITE( "io generation tests" );
    test->add( BOOST_TEST_CASE( &test_input_reader ) );
    return test;
}

// EOF