#define _CRT_SECURE_NO_DEPRECATE  // viz studio deprecation warnings
#include "boost/filesystem.hpp"
#include "exception_trapping.h"
#include "input_storage.h"
#include "ApplicationTextReader.h"
#include "InputState.h"
#include "ItemInputState.h"
#include "InsertFileState.h"
#include <string>
#define FCALL extern "C"

//////////////////////
FCALL void clear_all_buffers_f(int* ierror)
{
_TRAP_EXCEPT(*ierror,
  //Clear all buffers
// Clear all buffers DO NOT ALTER THIS LINE AT ALL
) // end exception trap    
}    


//////////////////////
FCALL void prioritize_all_buffers_f(int* ierror)
{ 
_TRAP_EXCEPT(*ierror,
  //Prioritize all buffers
// Prioritize all buffers DO NOT ALTER THIS LINE AT ALL
) // end exception trap    
}    

//////////////////////
FCALL void write_all_buffers_to_text_f(const char*file, 
                                       const bool* append,
                                       int* ierror, 
                                       int filelen)
{ 
_TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path path(filename.c_str()); 
  if (boost::filesystem::exists(path))
  {
     boost::filesystem::remove(path);
     //todo  failure is an error that should be reported
  }
  //Write out all buffers to text
// Write text all buffers DO NOT ALTER THIS LINE AT ALL
) // end exception trap    
}    

//////////////////////
FCALL void write_all_buffers_to_hdf5_f(hid_t* file_id, int* ierror)
/** both makes the table and writes the contents of the buffer to it */
{
_TRAP_EXCEPT(*ierror,
  //Write out all buffers to hdf5
// Write hdf5 all buffers DO NOT ALTER THIS LINE AT ALL
) // end exception trap    
}    



