#define _CRT_SECURE_NO_DEPRECATE  // viz studio deprecation warnings
#include "boost/filesystem.hpp"
#include "boost/algorithm/string/case_conv.hpp"
#include "exception_trapping.h"
#include "input_storage.h"
#include "input_state_map.h"
#include "ApplicationTextReader.h"
#include "InputState.h"
#include "ItemInputState.h"
#include "InsertFileState.h"
#include <string>
#include "LayerManager.h"
#define FCALL extern "C"

//////////////////////
FCALL void clear_all_buffers_f(int* ierror)
{
_TRAP_EXCEPT(*ierror,
  //Clear all buffers
// Clear all buffers DO NOT ALTER THIS LINE AT ALL
LayerManager::instance().clearAllLayer(); //todo: make this separate?
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

FCALL void write_buffer_to_text(const char*buffer,
                                const char*file, 
                                const bool* append,
                                int* ierror, 
                                int bufferlen,
                                int filelen)
{
    string buffer_name(buffer,bufferlen);
// Write text one buffer DO NOT ALTER THIS LINE AT ALL
}

//////////////////////
FCALL void write_buffer_profile_to_text_f(const char*profilename,
                                          const char*file, 
                                          const bool* append,
                                          int* ierror, 
                                          int profilelen,
                                          int filelen)
{ 
_TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path path(filename.c_str()); 
  bool doAppend=*append;
  if (boost::filesystem::exists(path))
  {
     boost::filesystem::remove(path);
     //todo  failure is an error that should be reported
  }
  // write out all layer names
  //todo: maybe this should be a separate function
  ofstream outfile;
  if (doAppend)
  {
      outfile.open(file,std::ios_base::app); 
  }
  else
  {
      outfile.open(file);
  }
  outfile << "\n# ====================== Files used in simulation ========================\n"; 
  outfile << endl;
  LayerManager::instance().writeToStream(outfile,"# ");

  outfile << endl;
  outfile << "\n#===================      Simulation input data    =====================\n"; 
  outfile << endl;
  outfile.close();

  doAppend = true;  
  string name(profilename,profilelen);
  const std::vector<std::string> bufs=profile(name);
  for(size_t ibuf = 0 ; ibuf < bufs.size() ; ++ibuf)
    {  
        string buf=bufs[ibuf];
        to_lower(buf);
        write_buffer_to_text(buf.c_str(),file,&doAppend,ierror,(int)buf.size(),filelen);
    }
  ) // end exception trap
} 


FCALL void write_buffer_to_hdf5(const char*buffer,
                                const hid_t* file_id, 
                                int* ierror, 
                                int bufferlen)
{
    string buffer_name(buffer,bufferlen);
// Write hdf5 one buffer DO NOT ALTER THIS LINE AT ALL
}

FCALL void read_buffer_from_hdf5(const char*buffer,
                                 const hid_t* file_id, 
                                  int* ierror, 
                                  int bufferlen)
{
    string buffer_name(buffer,bufferlen);
// Read hdf5 one buffer DO NOT ALTER THIS LINE AT ALL
}


//////////////////////
FCALL void write_all_buffers_to_hdf5_f(const hid_t* file_id, 
                                       int* ierror)
/** both makes the table and writes the contents of the buffer to it */
{
_TRAP_EXCEPT(*ierror,
  //Write out all buffers to hdf5
// Write hdf5 all buffers DO NOT ALTER THIS LINE AT ALL
) // end exception trap    
}    

//////////////////////

FCALL void write_buffer_profile_to_hdf5_f(const char*profilename,
                                          const hid_t* file_id,
                                          int* ierror, 
                                          int profilelen)
{
_TRAP_EXCEPT(*ierror,
  string name(profilename,profilelen);
  const std::vector<std::string> bufs=profile(name);
  for(size_t ibuf = 0 ; ibuf < bufs.size() ; ++ibuf)
    {  
        string buf=bufs[ibuf];
        to_lower(buf);
        write_buffer_to_hdf5(buf.c_str(),file_id,ierror,(int)buf.size());
    }
   LayerManager::instance().writeToHdf5(*file_id,"hydro/input/layers");
)
}   


FCALL void read_buffer_profile_from_hdf5_f(const char*profilename,
                                           const hid_t* file_id,
                                           int* ierror, 
                                           int profilelen)
{
_TRAP_EXCEPT(*ierror,
  string name(profilename,profilelen);
  const std::vector<std::string> bufs=profile(name);
  for(size_t ibuf = 0 ; ibuf < bufs.size() ; ++ibuf)
    {  
        string buf=bufs[ibuf];
        to_lower(buf);
        // todo: this is hardwired as a quick fix -- gotta get rid of the
		// direct reference
		if (buf == "xsect") continue;
        read_buffer_from_hdf5(buf.c_str(),file_id,ierror,(int)buf.size());
    }
)
}   
