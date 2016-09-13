#define _CRT_SECURE_NO_DEPRECATE  // viz studio deprecation warnings
#include "boost/filesystem.hpp"
#include "boost/algorithm/string/case_conv.hpp"
#include "exception_trapping.h"
#include "input_storage.h"
#include "input_state_map.h"
#include "ApplicationTextReader.h"
#include "InputState.h"
#include "ItemInputState.h"
#include "IncludeFileState.h"
#include <string>
#include "LayerManager.h"
#define FCALL extern "C"

//////////////////////
FCALL void clear_all_buffers_f(int* ierror)
{
_TRAP_EXCEPT(*ierror,
  //Clear all buffers
HDFTableManager<envvar>::instance().buffer().clear();
HDFTableManager<scalar>::instance().buffer().clear();
HDFTableManager<channel>::instance().buffer().clear();
HDFTableManager<xsect>::instance().buffer().clear();
HDFTableManager<xsect_layer>::instance().buffer().clear();
HDFTableManager<reservoir>::instance().buffer().clear();
HDFTableManager<reservoir_connection>::instance().buffer().clear();
HDFTableManager<gate>::instance().buffer().clear();
HDFTableManager<gate_pipe_device>::instance().buffer().clear();
HDFTableManager<gate_weir_device>::instance().buffer().clear();
HDFTableManager<transfer>::instance().buffer().clear();
HDFTableManager<io_file>::instance().buffer().clear();
HDFTableManager<tidefile>::instance().buffer().clear();
HDFTableManager<group>::instance().buffer().clear();
HDFTableManager<group_member>::instance().buffer().clear();
HDFTableManager<channel_ic>::instance().buffer().clear();
HDFTableManager<reservoir_ic>::instance().buffer().clear();
HDFTableManager<operating_rule>::instance().buffer().clear();
HDFTableManager<oprule_expression>::instance().buffer().clear();
HDFTableManager<oprule_time_series>::instance().buffer().clear();
HDFTableManager<rate_coefficient>::instance().buffer().clear();
HDFTableManager<group_variable>::instance().buffer().clear();
HDFTableManager<particle_insertion>::instance().buffer().clear();
HDFTableManager<particle_filter>::instance().buffer().clear();
HDFTableManager<particle_res_filter>::instance().buffer().clear();
HDFTableManager<particle_flux_output>::instance().buffer().clear();
HDFTableManager<particle_group_output>::instance().buffer().clear();
HDFTableManager<input_climate>::instance().buffer().clear();
HDFTableManager<input_transfer_flow>::instance().buffer().clear();
HDFTableManager<input_gate>::instance().buffer().clear();
HDFTableManager<boundary_stage>::instance().buffer().clear();
HDFTableManager<boundary_flow>::instance().buffer().clear();
HDFTableManager<source_flow>::instance().buffer().clear();
HDFTableManager<source_flow_reservoir>::instance().buffer().clear();
HDFTableManager<node_concentration>::instance().buffer().clear();
HDFTableManager<reservoir_concentration>::instance().buffer().clear();
HDFTableManager<input_time_series>::instance().buffer().clear();
HDFTableManager<output_channel>::instance().buffer().clear();
HDFTableManager<output_reservoir>::instance().buffer().clear();
HDFTableManager<output_channel_source_track>::instance().buffer().clear();
HDFTableManager<output_reservoir_source_track>::instance().buffer().clear();
HDFTableManager<output_gate>::instance().buffer().clear();
HDFTableManager<suspended_sediment_type>::instance().buffer().clear();
HDFTableManager<suspended_sediment_boundary>::instance().buffer().clear();
LayerManager::instance().clearAllLayer(); //todo: make this separate?
) // end exception trap    
}    


//////////////////////
FCALL void prioritize_all_buffers_f(int* ierror)
{ 
_TRAP_EXCEPT(*ierror,
  //Prioritize all buffers
HDFTableManager<envvar>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<scalar>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<channel>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<xsect>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<xsect_layer>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<reservoir>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<reservoir_connection>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<gate>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<gate_pipe_device>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<gate_weir_device>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<transfer>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<io_file>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<tidefile>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<group>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<group_member>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<channel_ic>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<reservoir_ic>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<operating_rule>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<oprule_expression>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<oprule_time_series>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<rate_coefficient>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<group_variable>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<particle_insertion>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<particle_filter>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<particle_res_filter>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<particle_flux_output>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<particle_group_output>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<input_climate>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<input_transfer_flow>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<input_gate>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<boundary_stage>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<boundary_flow>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<source_flow>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<source_flow_reservoir>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<node_concentration>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<reservoir_concentration>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<input_time_series>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<output_channel>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<output_reservoir>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<output_channel_source_track>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<output_reservoir_source_track>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<output_gate>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<suspended_sediment_type>::instance().prioritize_buffer();
     if(*ierror != 0) return;
HDFTableManager<suspended_sediment_boundary>::instance().prioritize_buffer();
     if(*ierror != 0) return;
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
envvar_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
scalar_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
channel_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
xsect_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
xsect_layer_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
reservoir_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
reservoir_connection_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
gate_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
gate_pipe_device_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
gate_weir_device_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
transfer_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
io_file_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
tidefile_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
group_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
group_member_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
channel_ic_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
reservoir_ic_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
operating_rule_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
oprule_expression_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
oprule_time_series_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
rate_coefficient_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
group_variable_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
particle_insertion_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
particle_filter_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
particle_res_filter_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
particle_flux_output_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
particle_group_output_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
input_climate_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
input_transfer_flow_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
input_gate_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
boundary_stage_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
boundary_flow_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
source_flow_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
source_flow_reservoir_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
node_concentration_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
reservoir_concentration_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
input_time_series_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
output_channel_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
output_reservoir_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
output_channel_source_track_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
output_reservoir_source_track_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
output_gate_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
suspended_sediment_type_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
suspended_sediment_boundary_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;
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
if(buffer_name == "envvar"){envvar_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "scalar"){scalar_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "channel"){channel_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "xsect"){xsect_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "xsect_layer"){xsect_layer_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "reservoir"){reservoir_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "reservoir_connection"){reservoir_connection_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "gate"){gate_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "gate_pipe_device"){gate_pipe_device_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "gate_weir_device"){gate_weir_device_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "transfer"){transfer_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "io_file"){io_file_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "tidefile"){tidefile_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "group"){group_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "group_member"){group_member_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "channel_ic"){channel_ic_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "reservoir_ic"){reservoir_ic_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "operating_rule"){operating_rule_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "oprule_expression"){oprule_expression_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "oprule_time_series"){oprule_time_series_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "rate_coefficient"){rate_coefficient_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "group_variable"){group_variable_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "particle_insertion"){particle_insertion_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "particle_filter"){particle_filter_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "particle_res_filter"){particle_res_filter_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "particle_flux_output"){particle_flux_output_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "particle_group_output"){particle_group_output_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "input_climate"){input_climate_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "input_transfer_flow"){input_transfer_flow_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "input_gate"){input_gate_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "boundary_stage"){boundary_stage_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "boundary_flow"){boundary_flow_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "source_flow"){source_flow_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "source_flow_reservoir"){source_flow_reservoir_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "node_concentration"){node_concentration_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "reservoir_concentration"){reservoir_concentration_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "input_time_series"){input_time_series_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "output_channel"){output_channel_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "output_reservoir"){output_reservoir_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "output_channel_source_track"){output_channel_source_track_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "output_reservoir_source_track"){output_reservoir_source_track_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "output_gate"){output_gate_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "suspended_sediment_type"){suspended_sediment_type_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
if(buffer_name == "suspended_sediment_boundary"){suspended_sediment_boundary_write_buffer_to_text_f(file,append,ierror,filelen);
     if(*ierror != 0) return;}
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
if(buffer_name == "envvar"){envvar_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "scalar"){scalar_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "channel"){channel_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "xsect"){xsect_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "xsect_layer"){xsect_layer_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "reservoir"){reservoir_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "reservoir_connection"){reservoir_connection_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "gate"){gate_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "gate_pipe_device"){gate_pipe_device_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "gate_weir_device"){gate_weir_device_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "transfer"){transfer_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "io_file"){io_file_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "tidefile"){tidefile_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "group"){group_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "group_member"){group_member_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "channel_ic"){channel_ic_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "reservoir_ic"){reservoir_ic_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "operating_rule"){operating_rule_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "oprule_expression"){oprule_expression_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "oprule_time_series"){oprule_time_series_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "rate_coefficient"){rate_coefficient_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "group_variable"){group_variable_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "particle_insertion"){particle_insertion_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "particle_filter"){particle_filter_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "particle_res_filter"){particle_res_filter_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "particle_flux_output"){particle_flux_output_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "particle_group_output"){particle_group_output_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "input_climate"){input_climate_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "input_transfer_flow"){input_transfer_flow_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "input_gate"){input_gate_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "boundary_stage"){boundary_stage_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "boundary_flow"){boundary_flow_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "source_flow"){source_flow_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "source_flow_reservoir"){source_flow_reservoir_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "node_concentration"){node_concentration_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "reservoir_concentration"){reservoir_concentration_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "input_time_series"){input_time_series_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "output_channel"){output_channel_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "output_reservoir"){output_reservoir_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "output_channel_source_track"){output_channel_source_track_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "output_reservoir_source_track"){output_reservoir_source_track_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "output_gate"){output_gate_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "suspended_sediment_type"){suspended_sediment_type_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "suspended_sediment_boundary"){suspended_sediment_boundary_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
}

FCALL void read_buffer_from_hdf5(const char*buffer,
                                 const hid_t* file_id, 
                                  int* ierror, 
                                  int bufferlen)
{
    string buffer_name(buffer,bufferlen);
if(buffer_name == "envvar"){envvar_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "scalar"){scalar_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "channel"){channel_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "xsect"){xsect_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "xsect_layer"){xsect_layer_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "reservoir"){reservoir_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "reservoir_connection"){reservoir_connection_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "gate"){gate_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "gate_pipe_device"){gate_pipe_device_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "gate_weir_device"){gate_weir_device_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "transfer"){transfer_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "io_file"){io_file_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "tidefile"){tidefile_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "group"){group_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "group_member"){group_member_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "channel_ic"){channel_ic_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "reservoir_ic"){reservoir_ic_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "operating_rule"){operating_rule_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "oprule_expression"){oprule_expression_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "oprule_time_series"){oprule_time_series_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "rate_coefficient"){rate_coefficient_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "group_variable"){group_variable_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "particle_insertion"){particle_insertion_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "particle_filter"){particle_filter_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "particle_res_filter"){particle_res_filter_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "particle_flux_output"){particle_flux_output_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "particle_group_output"){particle_group_output_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "input_climate"){input_climate_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "input_transfer_flow"){input_transfer_flow_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "input_gate"){input_gate_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "boundary_stage"){boundary_stage_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "boundary_flow"){boundary_flow_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "source_flow"){source_flow_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "source_flow_reservoir"){source_flow_reservoir_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "node_concentration"){node_concentration_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "reservoir_concentration"){reservoir_concentration_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "input_time_series"){input_time_series_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "output_channel"){output_channel_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "output_reservoir"){output_reservoir_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "output_channel_source_track"){output_channel_source_track_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "output_reservoir_source_track"){output_reservoir_source_track_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "output_gate"){output_gate_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "suspended_sediment_type"){suspended_sediment_type_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
if(buffer_name == "suspended_sediment_boundary"){suspended_sediment_boundary_read_buffer_from_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;}
}


//////////////////////
FCALL void write_all_buffers_to_hdf5_f(const hid_t* file_id, 
                                       int* ierror)
/** both makes the table and writes the contents of the buffer to it */
{
_TRAP_EXCEPT(*ierror,
  //Write out all buffers to hdf5
envvar_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
scalar_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
channel_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
xsect_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
xsect_layer_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
reservoir_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
reservoir_connection_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
gate_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
gate_pipe_device_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
gate_weir_device_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
transfer_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
io_file_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
tidefile_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
group_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
group_member_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
channel_ic_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
reservoir_ic_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
operating_rule_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
oprule_expression_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
oprule_time_series_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
rate_coefficient_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
group_variable_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
particle_insertion_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
particle_filter_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
particle_res_filter_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
particle_flux_output_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
particle_group_output_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
input_climate_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
input_transfer_flow_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
input_gate_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
boundary_stage_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
boundary_flow_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
source_flow_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
source_flow_reservoir_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
node_concentration_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
reservoir_concentration_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
input_time_series_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
output_channel_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
output_reservoir_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
output_channel_source_track_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
output_reservoir_source_track_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
output_gate_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
suspended_sediment_type_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
suspended_sediment_boundary_write_buffer_to_hdf5_f(file_id,ierror);
     if(*ierror != 0) return;
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
