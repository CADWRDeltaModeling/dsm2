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
#include "dsm2_tidefile_input_storage_virtual_xsect.h"
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
ostream& operator<<(ostream & stream, const virtual_xsect & obj)
{  
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<  setw(9)
            << setfill(' ')
            << left
            << obj.chan_no  
        << setw(14)
            << setfill(' ')
            << left
            << obj.num_virt_sec  
        << setw(8)
            << setfill(' ')
            << left
            << obj.vsecno  
        << setw(10)
            << setfill(' ')
            << left
            << obj.num_elev  
        <<
            setw(16)
            << setfill(' ')
            << setprecision(8)
            << left
            << obj.min_elev  
        <<
            setw(16)
            << setfill(' ')
            << setprecision(8)
            << left
            << obj.elevation  
        <<
            setw(16)
            << setfill(' ')
            << setprecision(8)
            << left
            << obj.area  
        <<
            setw(16)
            << setfill(' ')
            << setprecision(8)
            << left
            << obj.wet_p  
        <<
            setw(16)
            << setfill(' ')
            << setprecision(8)
            << left
            << obj.width  
        ;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, virtual_xsect & obj)
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
        tokenstrm >> obj.chan_no;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert chan_no to correct data type:"+tempstr);
        }
        

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.num_virt_sec;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert num_virt_sec to correct data type:"+tempstr);
        }
        

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.vsecno;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert vsecno to correct data type:"+tempstr);
        }
        

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.num_elev;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert num_elev to correct data type:"+tempstr);
        }
        

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.min_elev = strtod((beg++)->c_str(),NULL);
         

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.elevation = strtod((beg++)->c_str(),NULL);
         

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.area = strtod((beg++)->c_str(),NULL);
         

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.wet_p = strtod((beg++)->c_str(),NULL);
         

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.width = strtod((beg++)->c_str(),NULL);
         ;
  return stream;
}

template<>
HDFTableManager<virtual_xsect>::HDFTableManager() :
    description(virtual_xsect_table_description()),  
    m_default_fill(virtual_xsect(-901,-901,-901,-901,-901.0,-901.0,-901.0,-901.0,-901.0)){}

template<>
void HDFTableManager<virtual_xsect>::prioritize_buffer()
{

    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    vector<virtual_xsect>::const_iterator dupl = adjacent_find(buffer().begin(),buffer().end());
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
    buffer().erase(unique(buffer().begin(),buffer().end(),identifier_equal<virtual_xsect>()),buffer().end());
    // Eliminate items that are not used. This must be done after lower layers have been removed
    buffer().erase(remove_if(buffer().begin(), buffer().end(),not1(entry_used<virtual_xsect>())), buffer().end());
    
}

TableDescription virtual_xsect_table_description(){
  const char* title = "virtual_xsect";
  const size_t size = sizeof(virtual_xsect);
  const size_t nfields = 9;
  virtual_xsect default_struct = virtual_xsect(-901,-901,-901,-901,-901.0,-901.0,-901.0,-901.0,-901.0);
  const char* fnames[] =  {"chan_no","num_virt_sec","vsecno","num_elev","min_elev","elevation","area","wet_p","width"};
  const hid_t ftypes[] =  {
            H5T_NATIVE_INT,H5T_NATIVE_INT,H5T_NATIVE_INT,H5T_NATIVE_INT,H5T_NATIVE_DOUBLE,H5T_NATIVE_DOUBLE,H5T_NATIVE_DOUBLE,H5T_NATIVE_DOUBLE,H5T_NATIVE_DOUBLE
               };

  const size_t foffsets[] ={
             ((char*)&default_struct.chan_no - (char*)&default_struct),
             ((char*)&default_struct.num_virt_sec - (char*)&default_struct),
             ((char*)&default_struct.vsecno - (char*)&default_struct),
             ((char*)&default_struct.num_elev - (char*)&default_struct),
             ((char*)&default_struct.min_elev - (char*)&default_struct),
             ((char*)&default_struct.elevation - (char*)&default_struct),
             ((char*)&default_struct.area - (char*)&default_struct),
             ((char*)&default_struct.wet_p - (char*)&default_struct),
             ((char*)&default_struct.width - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.chan_no ),
         sizeof( default_struct.num_virt_sec ),
         sizeof( default_struct.vsecno ),
         sizeof( default_struct.num_elev ),
         sizeof( default_struct.min_elev ),
         sizeof( default_struct.elevation ),
         sizeof( default_struct.area ),
         sizeof( default_struct.wet_p ),
         sizeof( default_struct.width )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type virtual_xsect
*/  
void virtual_xsect_clear_buffer_f(){
  //virtual_xsect_table::instance().buffer().destroy();
  virtual_xsect_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void virtual_xsect_append_to_buffer_f(const int * a_chan_no,const int * a_num_virt_sec,const int * a_vsecno,const int * a_num_elev,const double * a_min_elev,const double * a_elevation,const double * a_area,const double * a_wet_p,const double * a_width, int * ierror)
{
 _TRAP_EXCEPT(*ierror,
   virtual_xsect_table::instance().buffer().push_back(
                                      virtual_xsect(
                                      *a_chan_no,*a_num_virt_sec,*a_vsecno,*a_num_elev,*a_min_elev,*a_elevation,*a_area,*a_wet_p,*a_width
                                      ));
 ) // end of exception trap
}
  
/** both makes the table and writes the contents of the buffer to it */
void virtual_xsect_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  virtual_xsect_table & table = virtual_xsect_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( virtual_xsect_table::instance().description.title.c_str(), 
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
		                                       1,                     //virtual_xsect_table::instance().description.compress, 
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void virtual_xsect_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    virtual_xsect_table & table = virtual_xsect_table::instance();
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
void virtual_xsect_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
				     virtual_xsect_table::instance().description.title.c_str(), 
				     &nfields, 
				     nrecords));
 ) // end of exception trap
}


    
/** get one row worth of information from the buffer */
void virtual_xsect_query_from_buffer_f(size_t* row, 
                        int * a_chan_no,int * a_num_virt_sec,int * a_vsecno,int * a_num_elev,double * a_min_elev,double * a_elevation,double * a_area,double * a_wet_p,double * a_width, int * ierror
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > virtual_xsect_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    virtual_xsect obj =virtual_xsect_table::instance().buffer()[ndx];
    *a_chan_no=obj.chan_no;
    *a_num_virt_sec=obj.num_virt_sec;
    *a_vsecno=obj.vsecno;
    *a_num_elev=obj.num_elev;
    *a_min_elev=obj.min_elev;
    *a_elevation=obj.elevation;
    *a_area=obj.area;
    *a_wet_p=obj.wet_p;
    *a_width=obj.width;
    
    
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void virtual_xsect_prioritize_buffer_f(int* ierror)
{  
 _TRAP_EXCEPT(*ierror,
  virtual_xsect_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type virtual_xsect */
int virtual_xsect_buffer_size_f()
{ 
  return (int) virtual_xsect_table::instance().buffer().size();
}

void virtual_xsect_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("virtual_xsect");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<virtual_xsect> & obs = virtual_xsect_table::instance().buffer();
   virtual_xsect_table& table = virtual_xsect_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount) 
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<virtual_xsect>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {  
           const virtual_xsect & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void virtual_xsect_write_buffer_to_text_f(const char* file, 
                                      const bool* append, 
                                      int* ierror, 
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);
  
  virtual_xsect_write_buffer_to_stream(out,*append); 
  ) // end of exception trap  
}






