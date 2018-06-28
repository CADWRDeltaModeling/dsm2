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
#include "input_storage_particle_insertion.h"
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
ostream& operator<<(ostream & stream, const particle_insertion & obj)
{  
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<  setw(6)
            << setfill(' ')
            << left
            << obj.node  
        << setw(8)
            << setfill(' ')
            << left
            << obj.nparts  
        << 
            setw(max(4+12,(int)(4+strlen(obj.delay))))
            << setfill(' ')
            << left
            << quote_spaces(obj.delay, 8)  
        << 
            setw(max(4+12,(int)(4+strlen(obj.duration))))
            << setfill(' ')
            << left
            << quote_spaces(obj.duration, 16)  
        ;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, particle_insertion & obj)
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
        tokenstrm >> obj.node;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert node to correct data type:"+tempstr);
        }
        

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.nparts;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert nparts to correct data type:"+tempstr);
        }
        

   if (beg == end)
   {
     throw runtime_error("Fewer input fields received than expected");
   }        
   if(beg->size()<= 8)
   {
        strcpy(obj.delay, (beg++)->c_str());
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
        strcpy(obj.duration, (beg++)->c_str());
   }
   else
   {
      cout << "fatal error" <<endl;
         throw logic_error("String too long (max width 16):" + (*beg));
   }
   ;
  return stream;
}

template<>
HDFTableManager<particle_insertion>::HDFTableManager() :
    description(particle_insertion_table_description()),  
    m_default_fill(particle_insertion(-901,-901,"","")){}

template<>
void HDFTableManager<particle_insertion>::prioritize_buffer()
{

    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    vector<particle_insertion>::const_iterator dupl = adjacent_find(buffer().begin(),buffer().end());
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
    buffer().erase(unique(buffer().begin(),buffer().end(),identifier_equal<particle_insertion>()),buffer().end());
    // Eliminate items that are not used. This must be done after lower layers have been removed
    buffer().erase(remove_if(buffer().begin(), buffer().end(),not1(entry_used<particle_insertion>())), buffer().end());
    
}

TableDescription particle_insertion_table_description(){
  const char* title = "particle_insertion";
  const size_t size = sizeof(particle_insertion);
  const size_t nfields = 4;
  particle_insertion default_struct = particle_insertion(-901,-901,"","");
  const char* fnames[] =  {"node","nparts","delay","duration"};
  const hid_t ftypes[] =  {
            H5T_NATIVE_INT,H5T_NATIVE_INT,string_type(8),string_type(16)
               };

  const size_t foffsets[] ={
             ((char*)&default_struct.node - (char*)&default_struct),
             ((char*)&default_struct.nparts - (char*)&default_struct),
             ((char*)&default_struct.delay - (char*)&default_struct),
             ((char*)&default_struct.duration - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.node ),
         sizeof( default_struct.nparts ),
         sizeof( default_struct.delay ),
         sizeof( default_struct.duration )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type particle_insertion
*/  
void particle_insertion_clear_buffer_f(){
  //particle_insertion_table::instance().buffer().destroy();
  particle_insertion_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void particle_insertion_append_to_buffer_f(const int * a_node,const int * a_nparts,const  char a_delay[8],const  char a_duration[16], int * ierror, 
              const int delay_len,const int duration_len)
{
 _TRAP_EXCEPT(*ierror,
   particle_insertion_table::instance().buffer().push_back(
                                      particle_insertion(
                                      *a_node,*a_nparts,a_delay,a_duration
                                      ));
 ) // end of exception trap
}
  
/** both makes the table and writes the contents of the buffer to it */
void particle_insertion_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  particle_insertion_table & table = particle_insertion_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( particle_insertion_table::instance().description.title.c_str(), 
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
		                                       1,                     //particle_insertion_table::instance().description.compress, 
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void particle_insertion_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    particle_insertion_table & table = particle_insertion_table::instance();
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
void particle_insertion_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
				     particle_insertion_table::instance().description.title.c_str(), 
				     &nfields, 
				     nrecords));
 ) // end of exception trap
}


    
/** get one row worth of information from the buffer */
void particle_insertion_query_from_buffer_f(size_t* row, 
                        int * a_node,int * a_nparts, char a_delay[8], char a_duration[16], int * ierror, 
              int delay_len,int duration_len
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > particle_insertion_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    particle_insertion obj =particle_insertion_table::instance().buffer()[ndx];
    *a_node=obj.node;
    *a_nparts=obj.nparts;
    memcpy(a_delay,obj.delay,8);
    memcpy(a_duration,obj.duration,16);
    if (strlen(a_delay) < 8)fill(a_delay+strlen(a_delay),a_delay+8,' ');
    if (strlen(a_duration) < 16)fill(a_duration+strlen(a_duration),a_duration+16,' ');
    delay_len=(int)strlen(a_delay);
        duration_len=(int)strlen(a_duration);
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void particle_insertion_prioritize_buffer_f(int* ierror)
{  
 _TRAP_EXCEPT(*ierror,
  particle_insertion_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type particle_insertion */
int particle_insertion_buffer_size_f()
{ 
  return (int) particle_insertion_table::instance().buffer().size();
}

void particle_insertion_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("particle_insertion");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<particle_insertion> & obs = particle_insertion_table::instance().buffer();
   particle_insertion_table& table = particle_insertion_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount) 
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<particle_insertion>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {  
           const particle_insertion & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void particle_insertion_write_buffer_to_text_f(const char* file, 
                                      const bool* append, 
                                      int* ierror, 
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);
  
  particle_insertion_write_buffer_to_stream(out,*append); 
  ) // end of exception trap  
}






