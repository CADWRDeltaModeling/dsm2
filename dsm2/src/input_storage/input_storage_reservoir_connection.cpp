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
#include "input_storage_reservoir_connection.h"
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
ostream& operator<<(ostream & stream, const reservoir_connection & obj)
{  
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<  
            setw(max(4+16,(int)(4+strlen(obj.res_name))))
            << setfill(' ')
            << left
            << quote_spaces(obj.res_name, 32)  
        << setw(6)
            << setfill(' ')
            << left
            << obj.node  
        <<
            setw(10)
            << setfill(' ')
            << setprecision(3)
            << left
            << obj.coef_in  
        <<
            setw(12)
            << setfill(' ')
            << setprecision(3)
            << left
            << obj.coef_out  
        ;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, reservoir_connection & obj)
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
        tokenstrm >> obj.node;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert node to correct data type:"+tempstr);
        }
        

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.coef_in = strtod((beg++)->c_str(),NULL);
         

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.coef_out = strtod((beg++)->c_str(),NULL);
         ;
  return stream;
}

template<>
HDFTableManager<reservoir_connection>::HDFTableManager() :
    description(reservoir_connection_table_description()),  
    m_default_fill(reservoir_connection("",-901,-901.0,-901.0)){}

template<>
void HDFTableManager<reservoir_connection>::prioritize_buffer()
{
 
    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    // remove if my version != parent version that is used
    buffer().erase(remove_if(buffer().begin(),
                           buffer().end(),
                           not1(mem_fun_ref(&reservoir_connection::parent_valid))),buffer().end());
    
}

TableDescription reservoir_connection_table_description(){
  const char* title = "reservoir_connection";
  const size_t size = sizeof(reservoir_connection);
  const size_t nfields = 4;
  reservoir_connection default_struct = reservoir_connection("",-901,-901.0,-901.0);
  const char* fnames[] =  {"res_name","node","coef_in","coef_out"};
  const hid_t ftypes[] =  {
            string_type(32),H5T_NATIVE_INT,H5T_NATIVE_DOUBLE,H5T_NATIVE_DOUBLE
               };

  const size_t foffsets[] ={
             ((char*)&default_struct.res_name - (char*)&default_struct),
             ((char*)&default_struct.node - (char*)&default_struct),
             ((char*)&default_struct.coef_in - (char*)&default_struct),
             ((char*)&default_struct.coef_out - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.res_name ),
         sizeof( default_struct.node ),
         sizeof( default_struct.coef_in ),
         sizeof( default_struct.coef_out )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type reservoir_connection
*/  
void reservoir_connection_clear_buffer_f(){
  //reservoir_connection_table::instance().buffer().destroy();
  reservoir_connection_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void reservoir_connection_append_to_buffer_f(const  char a_res_name[32],const int * a_node,const double * a_coef_in,const double * a_coef_out, int * ierror, 
              const int res_name_len)
{
 _TRAP_EXCEPT(*ierror,
   reservoir_connection_table::instance().buffer().push_back(
                                      reservoir_connection(
                                      a_res_name,*a_node,*a_coef_in,*a_coef_out
                                      ));
 ) // end of exception trap
}
  
/** both makes the table and writes the contents of the buffer to it */
void reservoir_connection_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  reservoir_connection_table & table = reservoir_connection_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( reservoir_connection_table::instance().description.title.c_str(), 
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
		                                       1,                     //reservoir_connection_table::instance().description.compress, 
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void reservoir_connection_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    reservoir_connection_table & table = reservoir_connection_table::instance();
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
void reservoir_connection_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
				     reservoir_connection_table::instance().description.title.c_str(), 
				     &nfields, 
				     nrecords));
 ) // end of exception trap
}


    
/** get one row worth of information from the buffer */
void reservoir_connection_query_from_buffer_f(size_t* row, 
                         char a_res_name[32],int * a_node,double * a_coef_in,double * a_coef_out, int * ierror, 
              int res_name_len
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > reservoir_connection_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    reservoir_connection obj =reservoir_connection_table::instance().buffer()[ndx];
    memcpy(a_res_name,obj.res_name,32);
    *a_node=obj.node;
    *a_coef_in=obj.coef_in;
    *a_coef_out=obj.coef_out;
    if (strlen(a_res_name) < 32)fill(a_res_name+strlen(a_res_name),a_res_name+32,' ');
    res_name_len=(int)strlen(a_res_name);
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void reservoir_connection_prioritize_buffer_f(int* ierror)
{  
 _TRAP_EXCEPT(*ierror,
  reservoir_connection_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type reservoir_connection */
int reservoir_connection_buffer_size_f()
{ 
  return (int) reservoir_connection_table::instance().buffer().size();
}

void reservoir_connection_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("reservoir_connection");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<reservoir_connection> & obs = reservoir_connection_table::instance().buffer();
   reservoir_connection_table& table = reservoir_connection_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount) 
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<reservoir_connection>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {  
           const reservoir_connection & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void reservoir_connection_write_buffer_to_text_f(const char* file, 
                                      const bool* append, 
                                      int* ierror, 
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);
  
  reservoir_connection_write_buffer_to_stream(out,*append); 
  ) // end of exception trap  
}






