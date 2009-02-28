#define _CRT_SECURE_NO_DEPRECATE  // viz studio deprecation warnings
#include "boost/filesystem.hpp"
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
FCALL void write_all_buffers_to_text_f(const char*file, const bool* append, int filelen)
{ 
  string filename(file,filelen);
  boost::filesystem::path path(filename.c_str()); 
  if (boost::filesystem::exists(path))
  {
     boost::filesystem::remove(path);
     //todo  failure is an error that should be reported
  }
  //Write out all buffers to text
// Write text all buffers DO NOT ALTER THIS LINE AT ALL
}    

//////////////////////
FCALL void write_all_buffers_to_hdf5_f(hid_t* file_id)
/** both makes the table and writes the contents of the buffer to it */
{ 
  //Write out all buffers to hdf5
// Write hdf5 all buffers DO NOT ALTER THIS LINE AT ALL
}    



