#ifndef BUFFER_ACTIONS
#define BUFFER_ACTIONS

#define FCALL extern "C"

FCALL void clear_all_buffers_f(int* ierror);

FCALL void prioritize_all_buffers_f(int* ierror);

FCALL void write_all_buffers_to_text_f(const char*file, 
                                       const bool* append,
                                       int* ierror, 
                                       int filelen);

FCALL void write_buffer_to_text(const char*buffer,
                                const char*file, 
                                const bool* append,
                                int* ierror, 
                                int bufferlen,
                                int filelen);

FCALL void write_buffer_profile_to_text_f(const char*profilename,
                                          const char*file, 
                                          const bool* append,
                                          int* ierror, 
                                          int profilelen,
                                          int filelen);

FCALL void write_buffer_to_hdf5(const char*buffer,
                                const hid_t* file_id, 
                                int* ierror, 
                                int bufferlen);

FCALL void read_buffer_from_hdf5(const char*buffer,
                                 const hid_t* file_id, 
                                 int* ierror, 
                                 int bufferlen);


FCALL void write_all_buffers_to_hdf5_f(const hid_t* file_id, 
                                       int* ierror);

FCALL void write_buffer_profile_to_hdf5_f(const char*profilename,
                                          const hid_t* file_id,
                                          int* ierror, 
                                          int profilelen);

FCALL void read_buffer_profile_from_hdf5_f(const char*profilename,
                                           const hid_t* file_id,
                                           int* ierror, 
                                           int profilelen);

#endif // BUFFER_ACTIONS
