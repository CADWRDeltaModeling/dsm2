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
#include "input_storage_particle_flux_output.h"
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
ostream& operator<<(ostream & stream, const particle_flux_output & obj)
{  
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<  
            setw(max(4+16,(int)(4+strlen(obj.name))))
            << setfill(' ')
            << left
            << quote_spaces(obj.name, 32)  
        << 
            setw(max(4+24,(int)(4+strlen(obj.from_wb))))
            << setfill(' ')
            << left
            << quote_spaces(obj.from_wb, 40)  
        << 
            setw(max(4+24,(int)(4+strlen(obj.to_wb))))
            << setfill(' ')
            << left
            << quote_spaces(obj.to_wb, 40)  
        << 
            setw(max(4+10,(int)(4+strlen(obj.interval))))
            << setfill(' ')
            << left
            << quote_spaces(obj.interval, 16)  
        << 
            setw(max(4+1,(int)(4+strlen(obj.file))))
            << setfill(' ')
            << left
            << quote_spaces(obj.file, 128)  
        ;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, particle_flux_output & obj)
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
        strcpy(obj.name, (beg++)->c_str());
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
   if(beg->size()<= 40)
   {
        strcpy(obj.from_wb, (beg++)->c_str());
   }
   else
   {
      cout << "fatal error" <<endl;
         throw logic_error("String too long (max width 40):" + (*beg));
   }
   

   if (beg == end)
   {
     throw runtime_error("Fewer input fields received than expected");
   }        
   if(beg->size()<= 40)
   {
        strcpy(obj.to_wb, (beg++)->c_str());
   }
   else
   {
      cout << "fatal error" <<endl;
         throw logic_error("String too long (max width 40):" + (*beg));
   }
   

   if (beg == end)
   {
     throw runtime_error("Fewer input fields received than expected");
   }        
   if(beg->size()<= 16)
   {
        strcpy(obj.interval, (beg++)->c_str());
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
   if(beg->size()<= 128)
   {
        strcpy(obj.file, (beg++)->c_str());
   }
   else
   {
      cout << "fatal error" <<endl;
         throw logic_error("String too long (max width 128):" + (*beg));
   }
   ;
  return stream;
}

template<>
HDFTableManager<particle_flux_output>::HDFTableManager() :
    description(particle_flux_output_table_description()),  
    m_default_fill(particle_flux_output("","","","","")){}

template<>
void HDFTableManager<particle_flux_output>::prioritize_buffer()
{

    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    vector<particle_flux_output>::const_iterator dupl = adjacent_find(buffer().begin(),buffer().end());
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
    buffer().erase(unique(buffer().begin(),buffer().end(),identifier_equal<particle_flux_output>()),buffer().end());
    // Eliminate items that are not used. This must be done after lower layers have been removed
    buffer().erase(remove_if(buffer().begin(), buffer().end(),not1(entry_used<particle_flux_output>())), buffer().end());
    
}

TableDescription particle_flux_output_table_description(){
  const char* title = "particle_flux_output";
  const size_t size = sizeof(particle_flux_output);
  const size_t nfields = 5;
  particle_flux_output default_struct = particle_flux_output("","","","","");
  const char* fnames[] =  {"name","from_wb","to_wb","interval","file"};
  const hid_t ftypes[] =  {
            string_type(32),string_type(40),string_type(40),string_type(16),string_type(128)
               };

  const size_t foffsets[] ={
             ((char*)&default_struct.name - (char*)&default_struct),
             ((char*)&default_struct.from_wb - (char*)&default_struct),
             ((char*)&default_struct.to_wb - (char*)&default_struct),
             ((char*)&default_struct.interval - (char*)&default_struct),
             ((char*)&default_struct.file - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.name ),
         sizeof( default_struct.from_wb ),
         sizeof( default_struct.to_wb ),
         sizeof( default_struct.interval ),
         sizeof( default_struct.file )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type particle_flux_output
*/  
void particle_flux_output_clear_buffer_f(){
  //particle_flux_output_table::instance().buffer().destroy();
  particle_flux_output_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void particle_flux_output_append_to_buffer_f(const  char a_name[32],const  char a_from_wb[40],const  char a_to_wb[40],const  char a_interval[16],const  char a_file[128], int * ierror, 
              const int name_len,const int from_wb_len,const int to_wb_len,const int interval_len,const int file_len)
{
 _TRAP_EXCEPT(*ierror,
   particle_flux_output_table::instance().buffer().push_back(
                                      particle_flux_output(
                                      a_name,a_from_wb,a_to_wb,a_interval,a_file
                                      ));
 ) // end of exception trap
}
  
/** both makes the table and writes the contents of the buffer to it */
void particle_flux_output_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  particle_flux_output_table & table = particle_flux_output_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( particle_flux_output_table::instance().description.title.c_str(), 
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
		                                       1,                     //particle_flux_output_table::instance().description.compress, 
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void particle_flux_output_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    particle_flux_output_table & table = particle_flux_output_table::instance();
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
void particle_flux_output_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
				     particle_flux_output_table::instance().description.title.c_str(), 
				     &nfields, 
				     nrecords));
 ) // end of exception trap
}


    
/** get one row worth of information from the buffer */
void particle_flux_output_query_from_buffer_f(size_t* row, 
                         char a_name[32], char a_from_wb[40], char a_to_wb[40], char a_interval[16], char a_file[128], int * ierror, 
              int name_len,int from_wb_len,int to_wb_len,int interval_len,int file_len
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > particle_flux_output_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    particle_flux_output obj =particle_flux_output_table::instance().buffer()[ndx];
    memcpy(a_name,obj.name,32);
    memcpy(a_from_wb,obj.from_wb,40);
    memcpy(a_to_wb,obj.to_wb,40);
    memcpy(a_interval,obj.interval,16);
    memcpy(a_file,obj.file,128);
    if (strlen(a_name) < 32)fill(a_name+strlen(a_name),a_name+32,' ');
    if (strlen(a_from_wb) < 40)fill(a_from_wb+strlen(a_from_wb),a_from_wb+40,' ');
    if (strlen(a_to_wb) < 40)fill(a_to_wb+strlen(a_to_wb),a_to_wb+40,' ');
    if (strlen(a_interval) < 16)fill(a_interval+strlen(a_interval),a_interval+16,' ');
    if (strlen(a_file) < 128)fill(a_file+strlen(a_file),a_file+128,' ');
    name_len=(int)strlen(a_name);
        from_wb_len=(int)strlen(a_from_wb);
        to_wb_len=(int)strlen(a_to_wb);
        interval_len=(int)strlen(a_interval);
        file_len=(int)strlen(a_file);
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void particle_flux_output_prioritize_buffer_f(int* ierror)
{  
 _TRAP_EXCEPT(*ierror,
  particle_flux_output_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type particle_flux_output */
int particle_flux_output_buffer_size_f()
{ 
  return (int) particle_flux_output_table::instance().buffer().size();
}

void particle_flux_output_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("particle_flux_output");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<particle_flux_output> & obs = particle_flux_output_table::instance().buffer();
   particle_flux_output_table& table = particle_flux_output_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount) 
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<particle_flux_output>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {  
           const particle_flux_output & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void particle_flux_output_write_buffer_to_text_f(const char* file, 
                                      const bool* append, 
                                      int* ierror, 
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);
  
  particle_flux_output_write_buffer_to_stream(out,*append); 
  ) // end of exception trap  
}






