#ifndef stage_boundaries_STORAGE_H__
#define stage_boundaries_STORAGE_H__
/**
WARNING: THIS FILE WAS AUTOMATICALLY GENERATED USING A SCRIPT AND A TEMPLATE
DO NOT CHANGE THE CODE HERE.
IF THE CODE IS INCORRECT, FIX THE TEMPLATE OR SCRIPT
IF YOU WANT TO ADD NEW ITEMS, ADD THEM TO THE SCRIPT INPUT FILE AND RUN IT AFRESH
*/
#define _CRT_SECURE_NO_DEPRECATE  // viz studio deprecation warnings
#include "hdf5.h"
#include "hdf5_hl.h"
#include "hdf_storage.h"
#include "HDFTableManager.h"
#include "TableDescription.h"
#include "TableItemFunctors.h"
#include "boost/tuple/tuple_comparison.hpp"
#include "boost/tuple/tuple_io.hpp"
#include<iostream>
#include<vector>
#include<algorithm>
#include<string.h>
#include<iostream>


using namespace std;
using namespace boost;

/** Structure representing input of type stage_boundaries.
    This class is autogenerated by a script given
    a description of the object
    \ingroup userdata
*/
class stage_boundaries
{
public:

  /** Data type stage_boundaries, default constructor */
  typedef const boost::tuple<const std::string>  identifier_type;

  stage_boundaries() :
    int_node_no(-901),
    ext_node_no(-901),
    used(true),
    layer(0)
  {
    fill_n(name,32,'\0');
  };

  /** Construct a stage_boundaries with actual data values */
  stage_boundaries(const  char a_name[32],const int & a_int_node_no,const int & a_ext_node_no, bool a_used=true, int a_layer = 0) :
    int_node_no(a_int_node_no),
    ext_node_no(a_ext_node_no),
    used(a_used),
    layer(a_layer)
  {
    memcpy(name,a_name,32);
  }

  /**Copy constructor)
   */
  stage_boundaries (const stage_boundaries & other) :
    int_node_no(other.int_node_no),
    ext_node_no(other.ext_node_no),
    used(other.used),
    layer(other.layer)
  {
    memcpy(name,other.name,32);
  }

  /** Identifier that distinguishes whether two entries are distinct */
  identifier_type identifier()  const
  {
     return identifier_type( name );
  }

  void set_identifier(identifier_type identifier)
  {
     memcpy(name,identifier.get<0>().c_str(),32);
  }

  /** Parent object class name.
      If this is a child item belonging to a parent, returns
      the name of the parent class. Otherwise returns the name
      of this class.
  */
  stage_boundaries::identifier_type parent_identifier()  const
  {
     return stage_boundaries::identifier_type( name );
  }

  /** Return the version/layer number of the parent object */
  int parent_version()  const
  {
    vector<stage_boundaries>& pbuf = HDFTableManager<stage_boundaries>::instance().buffer();
    stage_boundaries parent;
    parent.set_identifier(parent_identifier());
    vector<stage_boundaries>::iterator loc = lower_bound(pbuf.begin(),
                                                pbuf.end(),
                                                parent,
                                                identifier_compare<stage_boundaries>());
    bool found = (loc!=pbuf.end()) && loc->identifier() == parent.identifier();
    if (found && loc->used){ return loc->layer; }
    else{ return -1; }
  }

  /** Return true if this layer of this object matches the layer of the parent object that will be use in the model.*/
  bool parent_valid()  const
  {
    return this->layer == parent_version();
  }

  /** Less-than operator based on the identifier plus (for parent objects) layer number*/
  bool operator< (const stage_boundaries & other) const
  {

     if(this->identifier() != other.identifier())
	 {
		 return this->identifier() < other.identifier();
	 }
	 // todo: make this a policy
	 bool layerOutranks = (this->layer == 0 && other.layer != 0) ||
		                  (this->layer > other.layer && other.layer != 0);
     return layerOutranks;

  }

  /** Less-than operator based on the identifier plus (for parent objects) layer number*/
  bool operator== (const stage_boundaries & other) const
  {
     return ((*this < other ) || (other < *this)) ? false : true;
  }

  /** Assignment that includes all the data plus the used and layer fields */
  stage_boundaries& operator=(const stage_boundaries& rhs)
  {
    strcpy(this->name,rhs.name);
    this->int_node_no=rhs.int_node_no;
    this->ext_node_no=rhs.ext_node_no;
    used = rhs.used;
    layer = rhs.layer;
    return *this;
  }

  /** Return the class name of this object (stage_boundaries) */
  string objectName() const
  {
    return "stage_boundaries";
  }


  char name[32];
  int int_node_no;
  int ext_node_no;
  /** indicator that the entry is used (true if not marked deleted by user)*/
  bool used;
  /** layer (version number) of this entry */
  int layer;
};

typedef HDFTableManager<stage_boundaries> stage_boundaries_table;

hid_t string_type(size_t n);

TableDescription stage_boundaries_table_description();

istream& operator>> (istream& stream, stage_boundaries & obj);
ostream& operator<<(ostream & stream, const stage_boundaries & obj);



////////// FORTRAN-LINKABLE API //////////
#define FCALL extern "C"


/**
  Clear the buffer, compatible with fortran
*/
FCALL void stage_boundaries_clear_buffer_f();

/** query number of records being stored in buffer */
FCALL int stage_boundaries_buffer_size_f();


/** append to buffer, compatible with fortran, returns new size*/
FCALL void stage_boundaries_append_to_buffer_f(const  char a_name[32],const int * a_int_node_no,const int * a_ext_node_no, int * ierror,
              const int name_len);

/** both makes the table and writes the contents of the buffer to it */
FCALL void stage_boundaries_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror);

/** reads the table in from a file into the buffer*/
FCALL void stage_boundaries_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror);

/** query size information about the table in hdf5
*/
FCALL void stage_boundaries_number_rows_hdf5_f(const hid_t* file_id, hsize_t* nrecords, int* ierror);


/** get one row worth of information from the buffer */
FCALL void stage_boundaries_query_from_buffer_f(int32_t* row,
                         char a_name[32],int * a_int_node_no,int * a_ext_node_no, int * ierror,
              int name_len);
/**
  prioritize buffer by layers, delete unused items and sort
  */
FCALL void stage_boundaries_prioritize_buffer_f(int* ierror);
/**
   write buffer to the given text file. File will be appended if exists and append flag is set to true.
   otherwise the file will be created or overwritten.
 */
FCALL void stage_boundaries_write_buffer_to_text_f(const char* file, const bool* append, int* ierror, int filelen);


#endif

