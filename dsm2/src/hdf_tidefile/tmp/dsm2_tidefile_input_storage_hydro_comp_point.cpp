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
#include "dsm2_tidefile_input_storage_hydro_comp_point.h"
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
ostream& operator<<(ostream & stream, const hydro_comp_point & obj)
{  
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<  setw(12)
            << setfill(' ')
            << left
            << obj.comp_index  
        << setw(9)
            << setfill(' ')
            << left
            << obj.channel  
        <<
            setw(16)
            << setfill(' ')
            << setprecision(8)
            << left
            << obj.distance  
        ;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, hydro_comp_point & obj)
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
        tokenstrm >> obj.comp_index;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert comp_index to correct data type:"+tempstr);
        }
        

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.channel;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert channel to correct data type:"+tempstr);
        }
        

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.distance = strtod((beg++)->c_str(),NULL);
         ;
  return stream;
}

template<>
HDFTableManager<hydro_comp_point>::HDFTableManager() :
    description(hydro_comp_point_table_description()),  
    m_default_fill(hydro_comp_point(-901,-901,-901.0)){}

template<>
void HDFTableManager<hydro_comp_point>::prioritize_buffer()
{

    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    vector<hydro_comp_point>::const_iterator dupl = adjacent_find(buffer().begin(),buffer().end());
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
    buffer().erase(unique(buffer().begin(),buffer().end(),identifier_equal<hydro_comp_point>()),buffer().end());
    // Eliminate items that are not used. This must be done after lower layers have been removed
    buffer().erase(remove_if(buffer().begin(), buffer().end(),not1(entry_used<hydro_comp_point>())), buffer().end());
    
}

TableDescription hydro_comp_point_table_description(){
  const char* title = "hydro_comp_point";
  const size_t size = sizeof(hydro_comp_point);
  const size_t nfields = 3;
  hydro_comp_point default_struct = hydro_comp_point(-901,-901,-901.0);
  const char* fnames[] =  {"comp_index","channel","distance"};
  const hid_t ftypes[] =  {
            H5T_NATIVE_INT,H5T_NATIVE_INT,H5T_NATIVE_DOUBLE
               };

  const size_t foffsets[] ={
             ((char*)&default_struct.comp_index - (char*)&default_struct),
             ((char*)&default_struct.channel - (char*)&default_struct),
             ((char*)&default_struct.distance - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.comp_index ),
         sizeof( default_struct.channel ),
         sizeof( default_struct.distance )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type hydro_comp_point
*/  
void hydro_comp_point_clear_buffer_f(){
  //hydro_comp_point_table::instance().buffer().destroy();
  hydro_comp_point_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void hydro_comp_point_append_to_buffer_f(const int * a_comp_index,const int * a_channel,const double * a_distance, int * ierror)
{
 _TRAP_EXCEPT(*ierror,
   hydro_comp_point_table::instance().buffer().push_back(
                                      hydro_comp_point(
                                      *a_comp_index,*a_channel,*a_distance
                                      ));
 ) // end of exception trap
}
  
/** both makes the table and writes the contents of the buffer to it */
void hydro_comp_point_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  hydro_comp_point_table & table = hydro_comp_point_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( hydro_comp_point_table::instance().description.title.c_str(), 
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
		                                       1,                     //hydro_comp_point_table::instance().description.compress, 
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void hydro_comp_point_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    hydro_comp_point_table & table = hydro_comp_point_table::instance();
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
void hydro_comp_point_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
				     hydro_comp_point_table::instance().description.title.c_str(), 
				     &nfields, 
				     nrecords));
 ) // end of exception trap
}


    
/** get one row worth of information from the buffer */
void hydro_comp_point_query_from_buffer_f(size_t* row, 
                        int * a_comp_index,int * a_channel,double * a_distance, int * ierror
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > hydro_comp_point_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    hydro_comp_point obj =hydro_comp_point_table::instance().buffer()[ndx];
    *a_comp_index=obj.comp_index;
    *a_channel=obj.channel;
    *a_distance=obj.distance;
    
    
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void hydro_comp_point_prioritize_buffer_f(int* ierror)
{  
 _TRAP_EXCEPT(*ierror,
  hydro_comp_point_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type hydro_comp_point */
int hydro_comp_point_buffer_size_f()
{ 
  return (int) hydro_comp_point_table::instance().buffer().size();
}

void hydro_comp_point_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("hydro_comp_point");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<hydro_comp_point> & obs = hydro_comp_point_table::instance().buffer();
   hydro_comp_point_table& table = hydro_comp_point_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount) 
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<hydro_comp_point>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {  
           const hydro_comp_point & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void hydro_comp_point_write_buffer_to_text_f(const char* file, 
                                      const bool* append, 
                                      int* ierror, 
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);
  
  hydro_comp_point_write_buffer_to_stream(out,*append); 
  ) // end of exception trap  
}






