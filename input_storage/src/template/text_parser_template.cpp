#include "input_storage.h"
#include "ApplicationTextReader.h"
#include "ItemInputState.h"
#include "InsertFileState.h"
#define FCALL extern "C"

// This function must be provided by the client application for the library to link properly
ApplicationTextReader::InputStateMap input_state_map();


///////////////////
FCALL void init_text_substitution_f(const char* include_block_name, int includelen)
{
  ApplicationTextReader & reader = ApplicationTextReader::instance();
  string keywordName("$TEXTSUBTYPE");
  to_upper(keywordName);

  InputStatePtr envPtr(new ItemInputState<$TEXTSUBTYPE>());
  map<string,InputStatePtr> statemap = input_state_map();

  vector<string> activeItems;
  activeItems.push_back(keywordName);

  if(includelen  > 0)
    {
      string includeName(include_block_name,includelen);
      activeItems.push_back(includeName);   //todo:case
    }
  reader.setInputStateMap(statemap);        // Initialize for text substitution only
  reader.setActiveItems(activeItems);      // Only two keywords are defined, both are active
  reader.getTextSubstitution().setEnabled(false);
}


////////////////////
FCALL void process_text_substitution_f(const char* startfile, int startfilelen)
{

    ApplicationTextReader & reader = ApplicationTextReader::instance();
    string filename(startfile,startfilelen);
    reader.processInput(filename);
    EnvSubstitution sub;
    vector<envvar> &envvars = HDFTableManager<envvar>::instance().buffer();
    for ( size_t i = 0 ; i < envvars.size();i++)
      {
	sub.add(envvars[i].name, envvars[i].value);
      }
    reader.setTextSubstitution(sub);
}

//////////////////////
FCALL void init_file_reader_f()
{
  //ApplicationTextReader::InputStateMap states = input_state_map();
  ApplicationTextReader & reader = ApplicationTextReader::instance();
  reader.setInputStateMap(input_state_map());
  reader.getTextSubstitution().setEnabled(true);

}

/////////////////////
FCALL void read_buffer_from_text_f(const char* startfile, int startfilelen)
{
    ApplicationTextReader & reader = ApplicationTextReader::instance();
    reader.setAllActive();
    string filename(startfile,startfilelen);
    reader.processInput(filename);
}


