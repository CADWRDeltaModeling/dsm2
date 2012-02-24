/**
 * Author: Kijin Nam, knam@water.ca.gov
 * Description: Functions to wrap buffer_actions                       
 */

#ifndef _buffer_actions_c_h_
#define _buffer_actions_c_h_

#include <string>
#include "hdf5.h"

namespace PTM2
{
int clear_all_buffers_c();
int prioritize_all_buffers_c();
int read_buffer_profile_from_hdf5_c(const std::string& profile_name,
                                    const hid_t& file_id);
int read_buffer_from_hdf5_c(const std::string& name, const hid_t& file_id);
}

#endif 