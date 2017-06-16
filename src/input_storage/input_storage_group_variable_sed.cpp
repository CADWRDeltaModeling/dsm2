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
#include "input_storage_group_variable_sed.h"
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
ostream& operator<<(ostream & stream, const group_variable_sed & obj)
{  
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<  
            setw(max(4+16,(int)(4+strlen(obj.group_name))))
            << setfill(' ')
            << left
            << quote_spaces(obj.group_name, 32)  
        << setw(10)
            << setfill(' ')
            << left
            << obj.sed_zone  
        << setw(11)
            << setfill(' ')
            << left
            << obj.sed_layer  
        << 
            setw(max(4+16,(int)(4+strlen(obj.variable))))
            << setfill(' ')
            << left
            << quote_spaces(obj.variable, 16)  
        <<
            setw(16)
            << setfill(' ')
            << setprecision(4)
            << left
            << obj.value  
        ;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, group_variable_sed & obj)
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
        strcpy(obj.group_name, (beg++)->c_str());
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
        tokenstrm >> obj.sed_zone;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert sed_zone to correct data type:"+tempstr);
        }
        

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.sed_layer;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert sed_layer to correct data type:"+tempstr);
        }
        

   if (beg == end)
   {
     throw runtime_error("Fewer input fields received than expected");
   }        
   if(beg->size()<= 16)
   {
        strcpy(obj.variable, (beg++)->c_str());
   }
   else
   {
      cout << "fatal error" <<endl;
         throw logic_error("String too long (max width 16):" + (*beg));
   }
   

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.value = strtod((beg++)->c_str(),NULL);
         ;
  return stream;
}

template<>
HDFTableManager<group_variable_sed>::HDFTableManager() :
    description(group_variable_sed_table_description()),  
    m_default_fill(group_variable_sed("",-901,-901,"",-901.0)){}

template<>
void HDFTableManager<group_variable_sed>::prioritize_buffer()
{

    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    vector<group_variable_sed>::const_iterator dupl = adjacent_find(buffer().begin(),buffer().end());
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
    buffer().erase(unique(buffer().begin(),buffer().end(),identifier_equal<group_variable_sed>()),buffer().end());
    // Eliminate items that are not used. This must be done after lower layers have been removed
    buffer().erase(remove_if(buffer().begin(), buffer().end(),not1(entry_used<group_variable_sed>())), buffer().end());
    
}

TableDescription group_variable_sed_table_description(){
  const char* title = "group_variable_sed";
  const size_t size = sizeof(group_variable_sed);
  const size_t nfields = 5;
  group_variable_sed default_struct = group_variable_sed("",-901,-901,"",-901.0);
  const char* fnames[] =  {"group_name","sed_zone","sed_layer","variable","value"};
  const hid_t ftypes[] =  {
            string_type(32),H5T_NATIVE_INT,H5T_NATIVE_INT,string_type(16),H5T_NATIVE_DOUBLE
               };

  const size_t foffsets[] ={
             ((char*)&default_struct.group_name - (char*)&default_struct),
             ((char*)&default_struct.sed_zone - (char*)&default_struct),
             ((char*)&default_struct.sed_layer - (char*)&default_struct),
             ((char*)&default_struct.variable - (char*)&default_struct),
             ((char*)&default_struct.value - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.group_name ),
         sizeof( default_struct.sed_zone ),
         sizeof( default_struct.sed_layer ),
         sizeof( default_struct.variable ),
         sizeof( default_struct.value )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type group_variable_sed
*/  
void group_variable_sed_clear_buffer_f(){
  //group_variable_sed_table::instance().buffer().destroy();
  group_variable_sed_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void group_variable_sed_append_to_buffer_f(const  char a_group_name[32],const int * a_sed_zone,const int * a_sed_layer,const  char a_variable[16],const double * a_value, int * ierror, 
              const int group_name_len,const int variable_len)
{
 _TRAP_EXCEPT(*ierror,
   group_variable_sed_table::instance().buffer().push_back(
                                      group_variable_sed(
                                      a_group_name,*a_sed_zone,*a_sed_layer,a_variable,*a_value
                                      ));
 ) // end of exception trap
}
  
/** both makes the table and writes the contents of the buffer to it */
void group_variable_sed_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  group_variable_sed_table & table = group_variable_sed_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( group_variable_sed_table::instance().description.title.c_str(), 
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
		                                       1,                     //group_variable_sed_table::instance().description.compress, 
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void group_variable_sed_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    group_variable_sed_table & table = group_variable_sed_table::instance();
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
void group_variable_sed_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
				     group_variable_sed_table::instance().description.title.c_str(), 
				     &nfields, 
				     nrecords));
 ) // end of exception trap
}


    
/** get one row worth of information from the buffer */
void group_variable_sed_query_from_buffer_f(size_t* row, 
                         char a_group_name[32],int * a_sed_zone,int * a_sed_layer, char a_variable[16],double * a_value, int * ierror, 
              int group_name_len,int variable_len
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > group_variable_sed_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    group_variable_sed obj =group_variable_sed_table::instance().buffer()[ndx];
    memcpy(a_group_name,obj.group_name,32);
    *a_sed_zone=obj.sed_zone;
    *a_sed_layer=obj.sed_layer;
    memcpy(a_variable,obj.variable,16);
    *a_value=obj.value;
    if (strlen(a_group_name) < 32)fill(a_group_name+strlen(a_group_name),a_group_name+32,' ');
    if (strlen(a_variable) < 16)fill(a_variable+strlen(a_variable),a_variable+16,' ');
    group_name_len=(int)strlen(a_group_name);
        variable_len=(int)strlen(a_variable);
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void group_variable_sed_prioritize_buffer_f(int* ierror)
{  
 _TRAP_EXCEPT(*ierror,
  group_variable_sed_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type group_variable_sed */
int group_variable_sed_buffer_size_f()
{ 
  return (int) group_variable_sed_table::instance().buffer().size();
}

void group_variable_sed_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("group_variable_sed");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<group_variable_sed> & obs = group_variable_sed_table::instance().buffer();
   group_variable_sed_table& table = group_variable_sed_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount) 
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<group_variable_sed>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {  
           const group_variable_sed & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void group_variable_sed_write_buffer_to_text_f(const char* file, 
                                      const bool* append, 
                                      int* ierror, 
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);
  
  group_variable_sed_write_buffer_to_stream(out,*append); 
  ) // end of exception trap  
}






