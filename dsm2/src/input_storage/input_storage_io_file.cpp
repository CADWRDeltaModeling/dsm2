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
#include "input_storage_io_file.h"
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
ostream& operator<<(ostream & stream, const io_file & obj)
{  
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<  
            setw(max(4+8,(int)(4+strlen(obj.model))))
            << setfill(' ')
            << left
            << quote_spaces(obj.model, 8)  
        << 
            setw(max(4+8,(int)(4+strlen(obj.type))))
            << setfill(' ')
            << left
            << quote_spaces(obj.type, 8)  
        << 
            setw(max(4+8,(int)(4+strlen(obj.io))))
            << setfill(' ')
            << left
            << quote_spaces(obj.io, 8)  
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
istream& operator>> (istream& stream, io_file & obj)
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
   if(beg->size()<= 8)
   {
        strcpy(obj.model, (beg++)->c_str());
   }
   else
   {
      cout << "fatal error" <<endl;
         throw logic_error("String too long (max width 8):" + (*beg));
   }
   

   if (beg == end)
   {
     throw runtime_error("Fewer input fields received than expected");
   }        
   if(beg->size()<= 8)
   {
        strcpy(obj.type, (beg++)->c_str());
   }
   else
   {
      cout << "fatal error" <<endl;
         throw logic_error("String too long (max width 8):" + (*beg));
   }
   

   if (beg == end)
   {
     throw runtime_error("Fewer input fields received than expected");
   }        
   if(beg->size()<= 8)
   {
        strcpy(obj.io, (beg++)->c_str());
   }
   else
   {
      cout << "fatal error" <<endl;
         throw logic_error("String too long (max width 8):" + (*beg));
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
HDFTableManager<io_file>::HDFTableManager() :
    description(io_file_table_description()),  
    m_default_fill(io_file("","","","","")){}

template<>
void HDFTableManager<io_file>::prioritize_buffer()
{

    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    vector<io_file>::const_iterator dupl = adjacent_find(buffer().begin(),buffer().end());
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
    buffer().erase(unique(buffer().begin(),buffer().end(),identifier_equal<io_file>()),buffer().end());
    // Eliminate items that are not used. This must be done after lower layers have been removed
    buffer().erase(remove_if(buffer().begin(), buffer().end(),not1(entry_used<io_file>())), buffer().end());
    
}

TableDescription io_file_table_description(){
  const char* title = "io_file";
  const size_t size = sizeof(io_file);
  const size_t nfields = 5;
  io_file default_struct = io_file("","","","","");
  const char* fnames[] =  {"model","type","io","interval","file"};
  const hid_t ftypes[] =  {
            string_type(8),string_type(8),string_type(8),string_type(16),string_type(128)
               };

  const size_t foffsets[] ={
             ((char*)&default_struct.model - (char*)&default_struct),
             ((char*)&default_struct.type - (char*)&default_struct),
             ((char*)&default_struct.io - (char*)&default_struct),
             ((char*)&default_struct.interval - (char*)&default_struct),
             ((char*)&default_struct.file - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.model ),
         sizeof( default_struct.type ),
         sizeof( default_struct.io ),
         sizeof( default_struct.interval ),
         sizeof( default_struct.file )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type io_file
*/  
void io_file_clear_buffer_f(){
  //io_file_table::instance().buffer().destroy();
  io_file_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void io_file_append_to_buffer_f(const  char a_model[8],const  char a_type[8],const  char a_io[8],const  char a_interval[16],const  char a_file[128], int * ierror, 
              const int model_len,const int type_len,const int io_len,const int interval_len,const int file_len)
{
 _TRAP_EXCEPT(*ierror,
   io_file_table::instance().buffer().push_back(
                                      io_file(
                                      a_model,a_type,a_io,a_interval,a_file
                                      ));
 ) // end of exception trap
}
  
/** both makes the table and writes the contents of the buffer to it */
void io_file_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  io_file_table & table = io_file_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( io_file_table::instance().description.title.c_str(), 
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
		                                       1,                     //io_file_table::instance().description.compress, 
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void io_file_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    io_file_table & table = io_file_table::instance();
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
void io_file_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
				     io_file_table::instance().description.title.c_str(), 
				     &nfields, 
				     nrecords));
 ) // end of exception trap
}


    
/** get one row worth of information from the buffer */
void io_file_query_from_buffer_f(size_t* row, 
                         char a_model[8], char a_type[8], char a_io[8], char a_interval[16], char a_file[128], int * ierror, 
              int model_len,int type_len,int io_len,int interval_len,int file_len
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > io_file_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    io_file obj =io_file_table::instance().buffer()[ndx];
    memcpy(a_model,obj.model,8);
    memcpy(a_type,obj.type,8);
    memcpy(a_io,obj.io,8);
    memcpy(a_interval,obj.interval,16);
    memcpy(a_file,obj.file,128);
    if (strlen(a_model) < 8)fill(a_model+strlen(a_model),a_model+8,' ');
    if (strlen(a_type) < 8)fill(a_type+strlen(a_type),a_type+8,' ');
    if (strlen(a_io) < 8)fill(a_io+strlen(a_io),a_io+8,' ');
    if (strlen(a_interval) < 16)fill(a_interval+strlen(a_interval),a_interval+16,' ');
    if (strlen(a_file) < 128)fill(a_file+strlen(a_file),a_file+128,' ');
    model_len=(int)strlen(a_model);
        type_len=(int)strlen(a_type);
        io_len=(int)strlen(a_io);
        interval_len=(int)strlen(a_interval);
        file_len=(int)strlen(a_file);
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void io_file_prioritize_buffer_f(int* ierror)
{  
 _TRAP_EXCEPT(*ierror,
  io_file_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type io_file */
int io_file_buffer_size_f()
{ 
  return (int) io_file_table::instance().buffer().size();
}

void io_file_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("io_file");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<io_file> & obs = io_file_table::instance().buffer();
   io_file_table& table = io_file_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount) 
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<io_file>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {  
           const io_file & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void io_file_write_buffer_to_text_f(const char* file, 
                                      const bool* append, 
                                      int* ierror, 
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);
  
  io_file_write_buffer_to_stream(out,*append); 
  ) // end of exception trap  
}






