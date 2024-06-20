/**
WARNING: THIS FILE WAS AUTOMATICALLY GENERATED USING A SCRIPT AND A TEMPLATE
DO NOT CHANGE THE CODE HERE.
IF THE CODE IS INCORRECT, FIX THE TEMPLATE OR SCRIPT
IF YOU WANT TO ADD NEW ITEMS, ADD THEM TO THE SCRIPT INPUT FILE AND RUN IT AFRESH
*/

/**
  READ case:
  1. Clear the buffer.
  2. Append items to the buffer one at a time from fortran.
  3. Write the buffer to file.
  4. Clear the buffer.

  WRITE case:
  1. Clear the buffer.
  2. Read table from file.
  3. Query number of items in table.
  3. Query items one at a time by row.
  4. Clear the buffer.
*/
#include "dsm2_tidefile_input_storage_reservoir_flow_connections.h"
#include "exception_trapping.h"
#include "LayerManager.h"
#include<iostream>
#include<sstream>
#include<fstream>
#include<iomanip>
#include "boost/tuple/tuple_comparison.hpp"
#include "boost/tokenizer.hpp"
#include "boost/iterator/filter_iterator.hpp"
#include "ParseValidationFunctors.h"
#include "boost/filesystem/operations.hpp"
#include "boost/algorithm/string/case_conv.hpp"
#include "boost/scoped_array.hpp"

using namespace std;
using namespace boost;

/** Write the table item to an output stream */
ostream& operator<<(ostream & stream, const reservoir_flow_connections & obj)
{
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<  setw(18)
            << setfill(' ')
            << left
            << obj.connection_index
        <<
            setw(max(4+32,(int)(4+strlen(obj.res_name))))
            << setfill(' ')
            << left
            << quote_spaces(obj.res_name, 32)
        << setw(11)
            << setfill(' ')
            << left
            << obj.res_index
        << setw(16)
            << setfill(' ')
            << left
            << obj.res_flow_index
        << setw(12)
            << setfill(' ')
            << left
            << obj.flow_index
        <<
            setw(max(4+32,(int)(4+strlen(obj.flow_name))))
            << setfill(' ')
            << left
            << quote_spaces(obj.flow_name, 32)
        <<
            setw(max(4+8,(int)(4+strlen(obj.flow_type))))
            << setfill(' ')
            << left
            << quote_spaces(obj.flow_type, 8)
        ;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, reservoir_flow_connections & obj)
{
  string str;
  getline(stream,str);

  boost::escaped_list_separator<char> xsep("\\", " \t","\"");
  typedef tokenizer<escaped_list_separator<char> > EscTokenizer;
  EscTokenizer xtok(str,xsep);

  is_not_empty predicate;
  typedef boost::filter_iterator<is_not_empty, EscTokenizer::iterator> FilterIter;

  FilterIter beg(predicate, xtok.begin(),xtok.end());
  FilterIter end(predicate, xtok.end());
  istringstream tokenstrm;
  string tempstr;


        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.connection_index;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert connection_index to correct data type:"+tempstr);
        }


   if (beg == end)
   {
     throw runtime_error("Fewer input fields received than expected");
   }
   if(beg->size()<= 32)
   {
        strcpy(obj.res_name, (beg++)->c_str());
   }
   else
   {
      cout << "fatal error" <<endl;
         throw logic_error("String too long (max width 32):" + (*beg));
   }


        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.res_index;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert res_index to correct data type:"+tempstr);
        }


        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.res_flow_index;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert res_flow_index to correct data type:"+tempstr);
        }


        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.flow_index;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert flow_index to correct data type:"+tempstr);
        }


   if (beg == end)
   {
     throw runtime_error("Fewer input fields received than expected");
   }
   if(beg->size()<= 32)
   {
        strcpy(obj.flow_name, (beg++)->c_str());
   }
   else
   {
      cout << "fatal error" <<endl;
         throw logic_error("String too long (max width 32):" + (*beg));
   }


   if (beg == end)
   {
     throw runtime_error("Fewer input fields received than expected");
   }
   if(beg->size()<= 8)
   {
        strcpy(obj.flow_type, (beg++)->c_str());
   }
   else
   {
      cout << "fatal error" <<endl;
         throw logic_error("String too long (max width 8):" + (*beg));
   }
   ;
  return stream;
}

template<>
HDFTableManager<reservoir_flow_connections>::HDFTableManager() :
    description(reservoir_flow_connections_table_description()),
    m_default_fill(reservoir_flow_connections(-901,"",-901,-901,-901,"","")){}

template<>
void HDFTableManager<reservoir_flow_connections>::prioritize_buffer()
{

    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    vector<reservoir_flow_connections>::const_iterator dupl = adjacent_find(buffer().begin(),buffer().end());
    if ( dupl != buffer().end())
    {
        string message = "Duplicate identifiers in the same input layer (or the same file has been included more than once):";
        stringstream messagestrm;
        messagestrm << message << endl << *dupl << " (" << (*dupl).objectName() <<")" << endl;
        messagestrm << "Layer: " << LayerManager::instance().layerName((*dupl).layer);
        throw runtime_error(messagestrm.str());
    }
    // Eliminate duplicates. Because of prior ordering,
    // this will eliminate lower layers
    buffer().erase(unique(buffer().begin(),buffer().end(),identifier_equal<reservoir_flow_connections>()),buffer().end());
    // Eliminate items that are not used. This must be done after lower layers have been removed
    buffer().erase(remove_if(buffer().begin(), buffer().end(),not1(entry_used<reservoir_flow_connections>())), buffer().end());

}

TableDescription reservoir_flow_connections_table_description(){
  const char* title = "reservoir_flow_connections";
  const size_t size = sizeof(reservoir_flow_connections);
  const size_t nfields = 7;
  reservoir_flow_connections default_struct = reservoir_flow_connections(-901,"",-901,-901,-901,"","");
  const char* fnames[] =  {"connection_index","res_name","res_index","res_flow_index","flow_index","flow_name","flow_type"};
  const hid_t ftypes[] =  {
            H5T_NATIVE_INT,string_type(32),H5T_NATIVE_INT,H5T_NATIVE_INT,H5T_NATIVE_INT,string_type(32),string_type(8)
               };

  const size_t foffsets[] ={
             (size_t)((char*)&default_struct.connection_index - (char*)&default_struct),
             (size_t)((char*)&default_struct.res_name - (char*)&default_struct),
             (size_t)((char*)&default_struct.res_index - (char*)&default_struct),
             (size_t)((char*)&default_struct.res_flow_index - (char*)&default_struct),
             (size_t)((char*)&default_struct.flow_index - (char*)&default_struct),
             (size_t)((char*)&default_struct.flow_name - (char*)&default_struct),
             (size_t)((char*)&default_struct.flow_type - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.connection_index ),
         sizeof( default_struct.res_name ),
         sizeof( default_struct.res_index ),
         sizeof( default_struct.res_flow_index ),
         sizeof( default_struct.flow_index ),
         sizeof( default_struct.flow_name ),
         sizeof( default_struct.flow_type )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type reservoir_flow_connections
*/
void reservoir_flow_connections_clear_buffer_f(){
  //reservoir_flow_connections_table::instance().buffer().destroy();
  reservoir_flow_connections_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void reservoir_flow_connections_append_to_buffer_f(const int * a_connection_index,const  char a_res_name[32],const int * a_res_index,const int * a_res_flow_index,const int * a_flow_index,const  char a_flow_name[32],const  char a_flow_type[8], int * ierror,
              const int res_name_len,const int flow_name_len,const int flow_type_len)
{
 _TRAP_EXCEPT(*ierror,
   reservoir_flow_connections_table::instance().buffer().push_back(
                                      reservoir_flow_connections(
                                      *a_connection_index,a_res_name,*a_res_index,*a_res_flow_index,*a_flow_index,a_flow_name,a_flow_type
                                      ));
 ) // end of exception trap
}

/** both makes the table and writes the contents of the buffer to it */
void reservoir_flow_connections_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  reservoir_flow_connections_table & table = reservoir_flow_connections_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( reservoir_flow_connections_table::instance().description.title.c_str(),
                                              *file_id,
		                                      table.description.title.c_str(),
                                              table.description.nfields,
                                              table.buffer().size(),
                                              table.description.struct_size,
                                              table.description.field_names,
                                              table.description.field_offsets,
                                              table.description.field_types,
                                              table.description.chunk_size,
		                                     &table.default_fill(), //fill data
		                                       1,                     //reservoir_flow_connections_table::instance().description.compress,
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void reservoir_flow_connections_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    reservoir_flow_connections_table & table = reservoir_flow_connections_table::instance();
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id,
                               table.description.title.c_str(),
                               &nfields,
			                   &nrecords ));
    if ( *ierror < 0) return;

    if (nfields != table.description.nfields){ *ierror = LOGIC_ERROR; return;}

    table.buffer().resize(static_cast<int>(nrecords));

	if (nrecords > 0)
	{
		*ierror = static_cast<int>( H5TBread_table(*file_id,
			                        table.description.title.c_str(),
			                        table.description.struct_size,
			                        table.description.field_offsets,
			                        table.description.field_sizes,
			                        &(table.buffer()[0])));
	}
 ) // end of exception trap
}

/** query size information about the table */
void reservoir_flow_connections_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id,
				     reservoir_flow_connections_table::instance().description.title.c_str(),
				     &nfields,
				     nrecords));
 ) // end of exception trap
}



/** get one row worth of information from the buffer */
void reservoir_flow_connections_query_from_buffer_f(int32_t* row,
                        int * a_connection_index, char a_res_name[32],int * a_res_index,int * a_res_flow_index,int * a_flow_index, char a_flow_name[32], char a_flow_type[8], int * ierror,
              int res_name_len,int flow_name_len,int flow_type_len
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > reservoir_flow_connections_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    reservoir_flow_connections obj =reservoir_flow_connections_table::instance().buffer()[ndx];
    *a_connection_index=obj.connection_index;
    memcpy(a_res_name,obj.res_name,32);
    *a_res_index=obj.res_index;
    *a_res_flow_index=obj.res_flow_index;
    *a_flow_index=obj.flow_index;
    memcpy(a_flow_name,obj.flow_name,32);
    memcpy(a_flow_type,obj.flow_type,8);
    if (strlen(a_res_name) < 32)fill(a_res_name+strlen(a_res_name),a_res_name+32,' ');
    if (strlen(a_flow_name) < 32)fill(a_flow_name+strlen(a_flow_name),a_flow_name+32,' ');
    if (strlen(a_flow_type) < 8)fill(a_flow_type+strlen(a_flow_type),a_flow_type+8,' ');
    res_name_len=(int)strlen(a_res_name);
        flow_name_len=(int)strlen(a_flow_name);
        flow_type_len=(int)strlen(a_flow_type);
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void reservoir_flow_connections_prioritize_buffer_f(int* ierror)
{
 _TRAP_EXCEPT(*ierror,
  reservoir_flow_connections_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type reservoir_flow_connections */
int reservoir_flow_connections_buffer_size_f()
{
  return (int) reservoir_flow_connections_table::instance().buffer().size();
}

void reservoir_flow_connections_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("reservoir_flow_connections");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<reservoir_flow_connections> & obs = reservoir_flow_connections_table::instance().buffer();
   reservoir_flow_connections_table& table = reservoir_flow_connections_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount)
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<reservoir_flow_connections>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {
           const reservoir_flow_connections & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void reservoir_flow_connections_write_buffer_to_text_f(const char* file,
                                      const bool* append,
                                      int* ierror,
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);

  reservoir_flow_connections_write_buffer_to_stream(out,*append);
  ) // end of exception trap
}






