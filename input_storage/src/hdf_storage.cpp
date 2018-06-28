#include "hdf_storage.h"

hid_t string_type(size_t n){
   hid_t _type = H5Tcopy(H5T_C_S1);
   H5Tset_size(_type, n);
   return _type;
};
