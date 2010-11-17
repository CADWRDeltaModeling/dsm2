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
// Item readers DO NOT ALTER THIS LINE AT ALL
  

  // Include file definitions
// Include definitions DO NOT ALTER THIS LINE AT ALL

  return inputMap;
}    


const std::vector<std::string> profile(const std::string& name)
{  
     std::vector<std::string> out;
// Profile definitions DO NOT ALTER THIS LINE AT ALL
    
    if (out.empty()){
       std::string message("Input profile name not found or is empty (case sensitivity?):\n");
       message += name;
       
       throw logic_error(message);
    }  
    return out;
}




