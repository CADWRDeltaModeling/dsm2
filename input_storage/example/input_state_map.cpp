#include "input_storage.h"
#include "ApplicationTextReader.h"
#include "InputState.h"
#include "ItemInputState.h"
#include "IncludeFileState.h"
#include <string>


//////////////////////
ApplicationTextReader::InputStateMap input_state_map()
{ 
  ApplicationTextReader::InputStateMap inputMap;

  //Add all available item readers to map
   InputStatePtr channelPtr(new ItemInputState<channel>());
    inputMap["CHANNEL"] = channelPtr;

   InputStatePtr xsectPtr(new ItemInputState<xsect>());
    inputMap["XSECT"] = xsectPtr;

   InputStatePtr envvarPtr(new ItemInputState<envvar>());
    inputMap["ENVVAR"] = envvarPtr;

  

  // Include file definitions
    vector<string> includeContextItems;
    includeContextItems.push_back("CHANNEL");
    includeContextItems.push_back("XSECT");
    InputStatePtr includePtr(new IncludeFileState(includeContextItems));
    inputMap["INCLUDE"] = includePtr;


  return inputMap;
}    


const std::vector<std::string> profile(const std::string& name)
{  
     std::vector<std::string> out;

    if(name =="envvar")
    {
        out.push_back("ENVVAR");
    }
    
    if(name =="all")
    {
        out.push_back("ENVVAR");
        out.push_back("CHANNEL");
        out.push_back("XSECT");
        out.push_back("INCLUDE");
    }
    
    
    if (out.empty()){
       std::string message("Input profile name not found or is empty (case sensitivity?):\n");
       message += name;
       
       throw logic_error(message);
    }  
    return out;
}




