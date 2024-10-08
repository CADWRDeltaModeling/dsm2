#ifndef reservoir_flow_connections_STORAGE_H__
#define reservoir_flow_connections_STORAGE_H__
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

/** Structure representing input of type reservoir_flow_connections.
    This class is autogenerated by a script given
    a description of the object
    \ingroup userdata
*/
class reservoir_flow_connections
{
public:

  /** Data type reservoir_flow_connections, default constructor */
  typedef const boost::tuple<const int&>  identifier_type;

  reservoir_flow_connections() :
    connection_index(-901),
    res_index(-901),
    res_flow_index(-901),
    flow_index(-901),
    used(true),
    layer(0)
  {
    fill_n(res_name,32,'\0');
    fill_n(flow_name,32,'\0');
    fill_n(flow_type,8,'\0');
  };

  /** Construct a reservoir_flow_connections with actual data values */
  reservoir_flow_connections(const int & a_connection_index,const  char a_res_name[32],const int & a_res_index,const int & a_res_flow_index,const int & a_flow_index,const  char a_flow_name[32],const  char a_flow_type[8], bool a_used=true, int a_layer = 0) :
    connection_index(a_connection_index),
    res_index(a_res_index),
    res_flow_index(a_res_flow_index),
    flow_index(a_flow_index),
    used(a_used),
    layer(a_layer)
  {
    memcpy(res_name,a_res_name,32);
    memcpy(flow_name,a_flow_name,32);
    memcpy(flow_type,a_flow_type,8);
  }

  /**Copy constructor)
   */
  reservoir_flow_connections (const reservoir_flow_connections & other) :
    connection_index(other.connection_index),
    res_index(other.res_index),
    res_flow_index(other.res_flow_index),
    flow_index(other.flow_index),
    used(other.used),
    layer(other.layer)
  {
    memcpy(res_name,other.res_name,32);
    memcpy(flow_name,other.flow_name,32);
    memcpy(flow_type,other.flow_type,8);
  }

  /** Identifier that distinguishes whether two entries are distinct */
  identifier_type identifier()  const
  {
     return identifier_type( res_flow_index );
  }

  void set_identifier(identifier_type identifier)
  {
     res_flow_index=identifier.get<0>();
  }

  /** Parent object class name.
      If this is a child item belonging to a parent, returns
      the name of the parent class. Otherwise returns the name
      of this class.
  */
  reservoir_flow_connections::identifier_type parent_identifier()  const
  {
     return reservoir_flow_connections::identifier_type( res_flow_index );
  }

  /** Return the version/layer number of the parent object */
  int parent_version()  const
  {
    vector<reservoir_flow_connections>& pbuf = HDFTableManager<reservoir_flow_connections>::instance().buffer();
    reservoir_flow_connections parent;
    parent.set_identifier(parent_identifier());
    vector<reservoir_flow_connections>::iterator loc = lower_bound(pbuf.begin(),
                                                pbuf.end(),
                                                parent,
                                                identifier_compare<reservoir_flow_connections>());
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
  bool operator< (const reservoir_flow_connections & other) const
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
  bool operator== (const reservoir_flow_connections & other) const
  {
     return ((*this < other ) || (other < *this)) ? false : true;
  }

  /** Assignment that includes all the data plus the used and layer fields */
  reservoir_flow_connections& operator=(const reservoir_flow_connections& rhs)
  {
    this->connection_index=rhs.connection_index;
    strcpy(this->res_name,rhs.res_name);
    this->res_index=rhs.res_index;
    this->res_flow_index=rhs.res_flow_index;
    this->flow_index=rhs.flow_index;
    strcpy(this->flow_name,rhs.flow_name);
    strcpy(this->flow_type,rhs.flow_type);
    used = rhs.used;
    layer = rhs.layer;
    return *this;
  }

  /** Return the class name of this object (reservoir_flow_connections) */
  string objectName() const
  {
    return "reservoir_flow_connections";
  }


  int connection_index;
  char res_name[32];
  int res_index;
  int res_flow_index;
  int flow_index;
  char flow_name[32];
  char flow_type[8];
  /** indicator that the entry is used (true if not marked deleted by user)*/
  bool used;
  /** layer (version number) of this entry */
  int layer;
};

typedef HDFTableManager<reservoir_flow_connections> reservoir_flow_connections_table;

hid_t string_type(size_t n);

TableDescription reservoir_flow_connections_table_description();

istream& operator>> (istream& stream, reservoir_flow_connections & obj);
ostream& operator<<(ostream & stream, const reservoir_flow_connections & obj);



////////// FORTRAN-LINKABLE API //////////
#define FCALL extern "C"


/**
  Clear the buffer, compatible with fortran
*/
FCALL void reservoir_flow_connections_clear_buffer_f();

/** query number of records being stored in buffer */
FCALL int reservoir_flow_connections_buffer_size_f();


/** append to buffer, compatible with fortran, returns new size*/
FCALL void reservoir_flow_connections_append_to_buffer_f(const int * a_connection_index,const  char a_res_name[32],const int * a_res_index,const int * a_res_flow_index,const int * a_flow_index,const  char a_flow_name[32],const  char a_flow_type[8], int * ierror,
              const int res_name_len,const int flow_name_len,const int flow_type_len);

/** both makes the table and writes the contents of the buffer to it */
FCALL void reservoir_flow_connections_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror);

/** reads the table in from a file into the buffer*/
FCALL void reservoir_flow_connections_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror);

/** query size information about the table in hdf5
*/
FCALL void reservoir_flow_connections_number_rows_hdf5_f(const hid_t* file_id, hsize_t* nrecords, int* ierror);


/** get one row worth of information from the buffer */
FCALL void reservoir_flow_connections_query_from_buffer_f(int32_t* row,
                        int * a_connection_index, char a_res_name[32],int * a_res_index,int * a_res_flow_index,int * a_flow_index, char a_flow_name[32], char a_flow_type[8], int * ierror,
              int res_name_len,int flow_name_len,int flow_type_len);
/**
  prioritize buffer by layers, delete unused items and sort
  */
FCALL void reservoir_flow_connections_prioritize_buffer_f(int* ierror);
/**
   write buffer to the given text file. File will be appended if exists and append flag is set to true.
   otherwise the file will be created or overwritten.
 */
FCALL void reservoir_flow_connections_write_buffer_to_text_f(const char* file, const bool* append, int* ierror, int filelen);


#endif

