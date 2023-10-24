#ifndef TableDescription_H__
#define TableDescription_H__
#define _CRT_SECURE_NO_DEPRECATE
// for strcpy in windows
#pragma warning (disable : 4996)
#include "hdf5.h"
#include<vector>
#include<string>
#include<iostream>
#include<cstring>
using namespace std;

/** Description of a table of user defined objects, including field (column) descriptions required by the HDF5 Table API.

    The data in this class describes the names and types of fields
    in the table. Enough descriptive data is included to use the HDF5 high-level
    Table API to create a table in HDF to store the objects, one row per object. There is some
    HDF5-specific information here, but the class does not actually hold objects of
    the data type it describes.

    The TableDescription is not something the client programmer should have to create.
    It can be generated from Python based on the python description of the stored object.
*/
class TableDescription
{
public:
  /** Create a new table description given the necessary descriptive information */
  TableDescription(const string& a_title,
                   const size_t & a_struct_size,
          		   const hsize_t & a_nfields,
                   const char * a_field_names[],
                   const hid_t  a_field_types[],
                   const size_t a_field_offsets[],
                   const size_t  a_field_sizes[],
                   const size_t & a_chunk_size) :
    title(a_title),
    struct_size(a_struct_size),
    nfields(a_nfields),
    field_names(new const char*[static_cast<size_t>(nfields)]),
    field_types(new hid_t[static_cast<size_t>(nfields)]),
    field_offsets(new size_t[static_cast<size_t>(nfields)]),
    field_sizes(new size_t[static_cast<size_t>(nfields)]),
    chunk_size(a_chunk_size)
    {
      size_t nf = static_cast<size_t>(nfields);
      for(size_t i = 0; i<nf ; ++i)
	  {
            field_names[i] = new char[strlen(a_field_names[i])+1];
            strcpy(const_cast<char*>(field_names[i]),a_field_names[i]);
            const_cast<char*>(field_names[i])[strlen(field_names[i])] = char(0);
            //header.push_back(string(temp));
	  }

	  memcpy(field_types,a_field_types,nf*sizeof(hid_t));
  	  memcpy(field_offsets,a_field_offsets,nf*sizeof(size_t));
	  memcpy(field_sizes,a_field_sizes,nf*sizeof(size_t));
    }

    /** Copy constructor*/
    TableDescription(const TableDescription & other) :
      title(other.title),
      struct_size(other.struct_size),
      nfields(other.nfields),
      field_names(new const char*[static_cast<size_t>(nfields)]),
      field_types(new hid_t[static_cast<size_t>(nfields)]),
      field_offsets(new size_t[static_cast<size_t>(nfields)]),
      field_sizes(new size_t[static_cast<size_t>(nfields)]),
      chunk_size(other.chunk_size)
	{
      size_t nf = static_cast<size_t>(nfields);
      for(size_t i = 0; i<nf ; ++i)
	    {
            field_names[i] = new char[strlen(other.field_names[i])+1];
            strcpy(const_cast<char*>(field_names[i]),other.field_names[i]);
            const_cast<char*>(field_names[i])[strlen(field_names[i])] = char(0);
          //header.push_back(string(temp));
        }
	  memcpy(field_types,other.field_types,nf*sizeof(hid_t));
	  memcpy(field_offsets,other.field_offsets,nf*sizeof(size_t));
	  memcpy(field_sizes,other.field_sizes,nf*sizeof(size_t));
	}



  ~TableDescription()
{
   size_t nf = static_cast<size_t>(nfields);
   for(size_t i = 0; i<nf ; ++i)
      {
	     delete []field_names[i];
         field_names[i]=NULL;
      }

  delete []field_names;
  delete []field_offsets;
  delete []field_sizes;
  delete []field_types;
}

string title;               /**< Table title */
const size_t struct_size;  /**< Structure size of table item */
const hsize_t nfields;     /**< Number of fields/columns in table */
const char** field_names;  /**< Names of fields/columns in table */
hid_t * field_types;       /**< Data types of fields */
size_t * field_offsets;    /**< Offset of fields from beginning of structure */
size_t * field_sizes;      /**< Size in bytes of each field */
hsize_t chunk_size;        /**< HDF Chunk size for table*/
//vector<string> header;
};


#endif
