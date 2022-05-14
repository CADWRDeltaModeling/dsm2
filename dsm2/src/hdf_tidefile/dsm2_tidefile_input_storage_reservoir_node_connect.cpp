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
#include "dsm2_tidefile_input_storage_reservoir_node_connect.h"
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
ostream& operator<<(ostream & stream, const reservoir_node_connect & obj)
{  
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<  setw(16)
            << setfill(' ')
            << left
            << obj.res_node_index  
        << 
            setw(max(4+32,(int)(4+strlen(obj.res_name))))
            << setfill(' ')
            << left
            << quote_spaces(obj.res_name, 32)  
        << setw(11)
            << setfill(' ')
            << left
            << obj.res_index  
        << setw(15)
            << setfill(' ')
            << left
            << obj.connect_index  
        << setw(9)
            << setfill(' ')
            << left
            << obj.node_no  
        << setw(13)
            << setfill(' ')
            << left
            << obj.ext_node_no  
        << 
            setw(max(4+8,(int)(4+strlen(obj.connection_type))))
            << setfill(' ')
            << left
            << quote_spaces(obj.connection_type, 8)  
        ;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, reservoir_node_connect & obj)
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
        tokenstrm >> obj.res_node_index;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert res_node_index to correct data type:"+tempstr);
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
        tokenstrm >> obj.connect_index;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert connect_index to correct data type:"+tempstr);
        }
        

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.node_no;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert node_no to correct data type:"+tempstr);
        }
        

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.ext_node_no;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert ext_node_no to correct data type:"+tempstr);
        }
        

   if (beg == end)
   {
     throw runtime_error("Fewer input fields received than expected");
   }        
   if(beg->size()<= 8)
   {
        strcpy(obj.connection_type, (beg++)->c_str());
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
HDFTableManager<reservoir_node_connect>::HDFTableManager() :
    description(reservoir_node_connect_table_description()),  
    m_default_fill(reservoir_node_connect(-901,"",-901,-901,-901,-901,"")){}

template<>
void HDFTableManager<reservoir_node_connect>::prioritize_buffer()
{

    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    vector<reservoir_node_connect>::const_iterator dupl = adjacent_find(buffer().begin(),buffer().end());
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
    buffer().erase(unique(buffer().begin(),buffer().end(),identifier_equal<reservoir_node_connect>()),buffer().end());
    // Eliminate items that are not used. This must be done after lower layers have been removed
    buffer().erase(remove_if(buffer().begin(), buffer().end(),not1(entry_used<reservoir_node_connect>())), buffer().end());
    
}

TableDescription reservoir_node_connect_table_description(){
  const char* title = "reservoir_node_connect";
  const size_t size = sizeof(reservoir_node_connect);
  const size_t nfields = 7;
  reservoir_node_connect default_struct = reservoir_node_connect(-901,"",-901,-901,-901,-901,"");
  const char* fnames[] =  {"res_node_index","res_name","res_index","connect_index","node_no","ext_node_no","connection_type"};
  const hid_t ftypes[] =  {
            H5T_NATIVE_INT,string_type(32),H5T_NATIVE_INT,H5T_NATIVE_INT,H5T_NATIVE_INT,H5T_NATIVE_INT,string_type(8)
               };

  const size_t foffsets[] ={
             ((char*)&default_struct.res_node_index - (char*)&default_struct),
             ((char*)&default_struct.res_name - (char*)&default_struct),
             ((char*)&default_struct.res_index - (char*)&default_struct),
             ((char*)&default_struct.connect_index - (char*)&default_struct),
             ((char*)&default_struct.node_no - (char*)&default_struct),
             ((char*)&default_struct.ext_node_no - (char*)&default_struct),
             ((char*)&default_struct.connection_type - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.res_node_index ),
         sizeof( default_struct.res_name ),
         sizeof( default_struct.res_index ),
         sizeof( default_struct.connect_index ),
         sizeof( default_struct.node_no ),
         sizeof( default_struct.ext_node_no ),
         sizeof( default_struct.connection_type )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type reservoir_node_connect
*/  
void reservoir_node_connect_clear_buffer_f(){
  //reservoir_node_connect_table::instance().buffer().destroy();
  reservoir_node_connect_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void reservoir_node_connect_append_to_buffer_f(const int * a_res_node_index,const  char a_res_name[32],const int * a_res_index,const int * a_connect_index,const int * a_node_no,const int * a_ext_node_no,const  char a_connection_type[8], int * ierror, 
              const int res_name_len,const int connection_type_len)
{
 _TRAP_EXCEPT(*ierror,
   reservoir_node_connect_table::instance().buffer().push_back(
                                      reservoir_node_connect(
                                      *a_res_node_index,a_res_name,*a_res_index,*a_connect_index,*a_node_no,*a_ext_node_no,a_connection_type
                                      ));
 ) // end of exception trap
}
  
/** both makes the table and writes the contents of the buffer to it */
void reservoir_node_connect_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  reservoir_node_connect_table & table = reservoir_node_connect_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( reservoir_node_connect_table::instance().description.title.c_str(), 
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
		                                       1,                     //reservoir_node_connect_table::instance().description.compress, 
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void reservoir_node_connect_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    reservoir_node_connect_table & table = reservoir_node_connect_table::instance();
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
void reservoir_node_connect_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
				     reservoir_node_connect_table::instance().description.title.c_str(), 
				     &nfields, 
				     nrecords));
 ) // end of exception trap
}


    
/** get one row worth of information from the buffer */
void reservoir_node_connect_query_from_buffer_f(int32_t* row, 
                        int * a_res_node_index, char a_res_name[32],int * a_res_index,int * a_connect_index,int * a_node_no,int * a_ext_node_no, char a_connection_type[8], int * ierror, 
              int res_name_len,int connection_type_len
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > reservoir_node_connect_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    reservoir_node_connect obj =reservoir_node_connect_table::instance().buffer()[ndx];
    *a_res_node_index=obj.res_node_index;
    memcpy(a_res_name,obj.res_name,32);
    *a_res_index=obj.res_index;
    *a_connect_index=obj.connect_index;
    *a_node_no=obj.node_no;
    *a_ext_node_no=obj.ext_node_no;
    memcpy(a_connection_type,obj.connection_type,8);
    if (strlen(a_res_name) < 32)fill(a_res_name+strlen(a_res_name),a_res_name+32,' ');
    if (strlen(a_connection_type) < 8)fill(a_connection_type+strlen(a_connection_type),a_connection_type+8,' ');
    res_name_len=(int)strlen(a_res_name);
        connection_type_len=(int)strlen(a_connection_type);
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void reservoir_node_connect_prioritize_buffer_f(int* ierror)
{  
 _TRAP_EXCEPT(*ierror,
  reservoir_node_connect_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type reservoir_node_connect */
int reservoir_node_connect_buffer_size_f()
{ 
  return (int) reservoir_node_connect_table::instance().buffer().size();
}

void reservoir_node_connect_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("reservoir_node_connect");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<reservoir_node_connect> & obs = reservoir_node_connect_table::instance().buffer();
   reservoir_node_connect_table& table = reservoir_node_connect_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount) 
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<reservoir_node_connect>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {  
           const reservoir_node_connect & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void reservoir_node_connect_write_buffer_to_text_f(const char* file, 
                                      const bool* append, 
                                      int* ierror, 
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);
  
  reservoir_node_connect_write_buffer_to_stream(out,*append); 
  ) // end of exception trap  
}






