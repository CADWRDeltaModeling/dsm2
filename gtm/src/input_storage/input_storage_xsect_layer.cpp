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
#include "input_storage_xsect_layer.h"
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
ostream& operator<<(ostream & stream, const xsect_layer & obj)
{  
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<  setw(9)
            << setfill(' ')
            << left
            << obj.chan_no  
        <<
            setw(8)
            << setfill(' ')
            << setprecision(3)
            << left
            << obj.dist  
        <<
            setw(10)
            << setfill(' ')
            << setprecision(3)
            << left
            << obj.elev  
        <<
            setw(12)
            << setfill(' ')
            << setprecision(3)
            << left
            << obj.area  
        <<
            setw(12)
            << setfill(' ')
            << setprecision(3)
            << left
            << obj.width  
        <<
            setw(12)
            << setfill(' ')
            << setprecision(3)
            << left
            << obj.wet_perim  
        ;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, xsect_layer & obj)
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
        obj.dist = strtod((beg++)->c_str(),NULL);
         

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.elev = strtod((beg++)->c_str(),NULL);
         

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.area = strtod((beg++)->c_str(),NULL);
         

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.width = strtod((beg++)->c_str(),NULL);
         

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.wet_perim = strtod((beg++)->c_str(),NULL);
         ;
  return stream;
}

template<>
HDFTableManager<xsect_layer>::HDFTableManager() :
    description(xsect_layer_table_description()),  
    m_default_fill(xsect_layer(-901,-901.0,-901.0,-901.0,-901.0,-901.0)){}

template<>
void HDFTableManager<xsect_layer>::prioritize_buffer()
{
 
    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    // remove if my version != parent version that is used
    buffer().erase(remove_if(buffer().begin(),
                           buffer().end(),
                           not1(mem_fun_ref(&xsect_layer::parent_valid))),buffer().end());
    
}

TableDescription xsect_layer_table_description(){
  const char* title = "xsect_layer";
  const size_t size = sizeof(xsect_layer);
  const size_t nfields = 6;
  xsect_layer default_struct = xsect_layer(-901,-901.0,-901.0,-901.0,-901.0,-901.0);
  const char* fnames[] =  {"chan_no","dist","elev","area","width","wet_perim"};
  const hid_t ftypes[] =  {
            H5T_NATIVE_INT,H5T_NATIVE_DOUBLE,H5T_NATIVE_DOUBLE,H5T_NATIVE_DOUBLE,H5T_NATIVE_DOUBLE,H5T_NATIVE_DOUBLE
               };

  const size_t foffsets[] ={
             ((char*)&default_struct.chan_no - (char*)&default_struct),
             ((char*)&default_struct.dist - (char*)&default_struct),
             ((char*)&default_struct.elev - (char*)&default_struct),
             ((char*)&default_struct.area - (char*)&default_struct),
             ((char*)&default_struct.width - (char*)&default_struct),
             ((char*)&default_struct.wet_perim - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.chan_no ),
         sizeof( default_struct.dist ),
         sizeof( default_struct.elev ),
         sizeof( default_struct.area ),
         sizeof( default_struct.width ),
         sizeof( default_struct.wet_perim )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type xsect_layer
*/  
void xsect_layer_clear_buffer_f(){
  //xsect_layer_table::instance().buffer().destroy();
  xsect_layer_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void xsect_layer_append_to_buffer_f(const int * a_chan_no,const double * a_dist,const double * a_elev,const double * a_area,const double * a_width,const double * a_wet_perim, int * ierror)
{
 _TRAP_EXCEPT(*ierror,
   xsect_layer_table::instance().buffer().push_back(
                                      xsect_layer(
                                      *a_chan_no,*a_dist,*a_elev,*a_area,*a_width,*a_wet_perim
                                      ));
 ) // end of exception trap
}
  
/** both makes the table and writes the contents of the buffer to it */
void xsect_layer_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  xsect_layer_table & table = xsect_layer_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( xsect_layer_table::instance().description.title.c_str(), 
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
		                                       1,                     //xsect_layer_table::instance().description.compress, 
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void xsect_layer_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    xsect_layer_table & table = xsect_layer_table::instance();
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
void xsect_layer_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
				     xsect_layer_table::instance().description.title.c_str(), 
				     &nfields, 
				     nrecords));
 ) // end of exception trap
}


    
/** get one row worth of information from the buffer */
void xsect_layer_query_from_buffer_f(size_t* row, 
                        int * a_chan_no,double * a_dist,double * a_elev,double * a_area,double * a_width,double * a_wet_perim, int * ierror
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > xsect_layer_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    xsect_layer obj =xsect_layer_table::instance().buffer()[ndx];
    *a_chan_no=obj.chan_no;
    *a_dist=obj.dist;
    *a_elev=obj.elev;
    *a_area=obj.area;
    *a_width=obj.width;
    *a_wet_perim=obj.wet_perim;
    
    
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void xsect_layer_prioritize_buffer_f(int* ierror)
{  
 _TRAP_EXCEPT(*ierror,
  xsect_layer_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type xsect_layer */
int xsect_layer_buffer_size_f()
{ 
  return (int) xsect_layer_table::instance().buffer().size();
}

void xsect_layer_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("xsect_layer");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<xsect_layer> & obs = xsect_layer_table::instance().buffer();
   xsect_layer_table& table = xsect_layer_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount) 
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<xsect_layer>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {  
           const xsect_layer & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void xsect_layer_write_buffer_to_text_f(const char* file, 
                                      const bool* append, 
                                      int* ierror, 
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);
  
  xsect_layer_write_buffer_to_stream(out,*append); 
  ) // end of exception trap  
}






