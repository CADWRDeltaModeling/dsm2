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
#include "input_storage_@TABLEOBJ.h"
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
ostream& operator<<(ostream & stream, const @TABLEOBJ & obj)
{  
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream << @OUTSTREAMFMT;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, @TABLEOBJ & obj)
{
  string str;
  getline(stream,str);

  boost::escaped_list_separator<char> xsep("\\", " \t","\"");
  typedef tokenizer<escaped_list_separator<char> > EscTokenizer;
  EscTokenizer xtok(str,xsep);

  is_not_empty predicate;
  typedef boost::filter_iterator<is_not_empty, EscTokenizer::iterator> FilterIter;

  FilterIter beg(predicate, xtok.begin());
  istringstream tokenstrm;
  string tempstr;
   
  @INSTREAMFMT;
  return stream;
}

template<>
HDFTableManager<@TABLEOBJ>::HDFTableManager() :
    description(@TABLEOBJ_table_description()),  
    m_default_fill(@TABLEOBJ(@DEFAULT_MEMBER_DATA)){}

template<>
void HDFTableManager<@TABLEOBJ>::prioritize_buffer()
{
@PRIORITIZE
}

TableDescription @TABLEOBJ_table_description(){
  const char* title = "@TABLEOBJ";
  const size_t size = sizeof(@TABLEOBJ);
  const size_t nfields = @NFIELDS;
  @TABLEOBJ default_struct = @TABLEOBJ(@DEFAULT_MEMBER_DATA);
  const char* fnames[] =  {@QUOTED_MEMBERS};
  const hid_t ftypes[] =  {
            @HDFTYPES
               };

  const size_t foffsets[] ={
            @OFFSETS
                           };

  const size_t fsizes[] = {
         @SIZES
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type @TABLEOBJ
*/  
void @TABLEOBJ_clear_buffer_f(){
  //@TABLEOBJ_table::instance().buffer().destroy();
  @TABLEOBJ_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void @TABLEOBJ_append_to_buffer_f(@FORTRAN_C_INPUT_SIGNATURE)
{
 _TRAP_EXCEPT(*ierror,
   @TABLEOBJ_table::instance().buffer().push_back(
                                      @TABLEOBJ(
                                      @C_PASS_THROUGH_CALL
                                      ));
 ) // end of exception trap
}
  
/** both makes the table and writes the contents of the buffer to it */
void @TABLEOBJ_write_buffer_to_hdf5_f(hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  @TABLEOBJ_table & table = @TABLEOBJ_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( @TABLEOBJ_table::instance().description.title.c_str(), 
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
		                                       1,                     //@TABLEOBJ_table::instance().description.compress, 
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void @TABLEOBJ_read_buffer_from_hdf5_f(hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    @TABLEOBJ_table & table = @TABLEOBJ_table::instance();
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
                               table.description.title.c_str(), 
                               &nfields, 
			                   &nrecords )); 
    if ( *ierror < 0) return; 
 
    if (nfields != table.description.nfields){ *ierror = LOGIC_ERROR; return;}

    table.buffer().resize(static_cast<int>(nrecords)); 

    *ierror = static_cast<int>( H5TBread_table(*file_id, 
                                  table.description.title.c_str(), 
			                      table.description.struct_size, 
			                      table.description.field_offsets, 
			                      table.description.field_sizes,
			                      &(table.buffer()[0])));
 ) // end of exception trap                                   
}

/** query size information about the table */
void @TABLEOBJ_number_rows_hdf5_f(hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
				     @TABLEOBJ_table::instance().description.title.c_str(), 
				     &nfields, 
				     nrecords));
 ) // end of exception trap
}


    
/** get one row worth of information from the buffer */
void @TABLEOBJ_query_from_buffer_f(size_t* row, 
                        @FORTRAN_C_OUTPUT_SIGNATURE
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > @TABLEOBJ_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    @TABLEOBJ obj =@TABLEOBJ_table::instance().buffer()[ndx];
    @BUFFER_QUERY
    @STRLENASSIGN
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void @TABLEOBJ_prioritize_buffer_f(int* ierror)
{  
 _TRAP_EXCEPT(*ierror,
  @TABLEOBJ_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type @TABLEOBJ */
int @TABLEOBJ_buffer_size_f()
{ 
  return (int) @TABLEOBJ_table::instance().buffer().size();
}

void @TABLEOBJ_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("@TABLEOBJ");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<@TABLEOBJ> & obs = @TABLEOBJ_table::instance().buffer();
   @TABLEOBJ_table& table = @TABLEOBJ_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount) 
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<@TABLEOBJ>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {  
           const @TABLEOBJ & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void @TABLEOBJ_write_buffer_to_text_f(const char* file, const bool* append, int* ierror, int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);
  
  @TABLEOBJ_write_buffer_to_stream(out,*append); 
  ) // end of exception trap  
}






