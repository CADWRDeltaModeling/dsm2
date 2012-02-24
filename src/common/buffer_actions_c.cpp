/**
 * Author: Kijin Nam
 * Wrapper of buffer_action functions in input_storage
 */

#include "buffer_actions_c.h"
#include "buffer_actions.h"

namespace PTM2
{

int clear_all_buffers_c()
{
  int ierror;
  clear_all_buffers_f(&ierror);
  return ierror;
}

int prioritize_all_buffers_c()
{
  int ierror;
  prioritize_all_buffers_f(&ierror);
  return ierror;
}

int read_buffer_profile_from_hdf5_c(const std::string& profile_name,
                                    const hid_t& file_id)
{
  int ierror;
  read_buffer_profile_from_hdf5_f(profile_name.c_str(),
                                  &file_id,
                                  &ierror, 
                                  profile_name.size());
  return ierror;
}

int read_buffer_from_hdf5_c(const std::string& name, const hid_t& file_id)
{
  int ierror;
  read_buffer_from_hdf5(name.c_str(), &file_id, &ierror, name.size());
  return ierror;
}

}
