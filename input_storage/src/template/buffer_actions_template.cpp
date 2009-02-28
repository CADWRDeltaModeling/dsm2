#include “boost/filesystem.hpp”
#include "input_storage.h"
#include "ApplicationTextReader.h"
#include "InputState.h"
#include "ItemInputState.h"
#include "InsertFileState.h"
#include <string>
#define FCALL extern "C"

//////////////////////
FCALL void clear_all_buffers_f()
{ 
  //Clear all buffers
// Clear all buffers DO NOT ALTER THIS LINE AT ALL

}    


//////////////////////
FCALL void prioritize_all_buffers_f()
{ 
  //Prioritize all buffers
// Prioritize all buffers DO NOT ALTER THIS LINE AT ALL

}    

//////////////////////
FCALL void write_all_buffers_to_text_f(const char* file, const bool* append, int filelen)
{ 
  string filename(file,filelen);
  boost::filesystem::path path(filename.c_str()); 
  if (exists(path) && (! remove(path))
  {
     //odo  this is an error that should be reported
  }

  //Write out all buffers
// Write text all buffers DO NOT ALTER THIS LINE AT ALL

}    



