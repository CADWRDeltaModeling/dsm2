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
#include "dsm2_tidefile_input_storage_qext.h"
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
ostream& operator<<(ostream & stream, const qext & obj)
{
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<
            setw(max(4+32,(int)(4+strlen(obj.name))))
            << setfill(' ')
            << left
            << quote_spaces(obj.name, 32)
        <<
            setw(max(4+32,(int)(4+strlen(obj.attach_obj_name))))
            << setfill(' ')
            << left
            << quote_spaces(obj.attach_obj_name, 32)
        << setw(19)
            << setfill(' ')
            << left
            << obj.attached_obj_type
        << setw(17)
            << setfill(' ')
            << left
            << obj.attached_obj_no
        ;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, qext & obj)
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
   if(beg->size()<= 32)
   {
        strcpy(obj.attach_obj_name, (beg++)->c_str());
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
        tokenstrm >> obj.attached_obj_type;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert attached_obj_type to correct data type:"+tempstr);
        }


        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.attached_obj_no;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert attached_obj_no to correct data type:"+tempstr);
        }
        ;
  return stream;
}

template<>
HDFTableManager<qext>::HDFTableManager() :
    description(qext_table_description()),
    m_default_fill(qext("","",-901,-901)){}

template<>
void HDFTableManager<qext>::prioritize_buffer()
{

    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    vector<qext>::const_iterator dupl = adjacent_find(buffer().begin(),buffer().end());
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
    buffer().erase(unique(buffer().begin(),buffer().end(),identifier_equal<qext>()),buffer().end());
    // Eliminate items that are not used. This must be done after lower layers have been removed
    buffer().erase(remove_if(buffer().begin(), buffer().end(),not1(entry_used<qext>())), buffer().end());

}

TableDescription qext_table_description(){
  const char* title = "qext";
  const size_t size = sizeof(qext);
  const size_t nfields = 4;
  qext default_struct = qext("","",-901,-901);
  const char* fnames[] =  {"name","attach_obj_name","attached_obj_type","attached_obj_no"};
  const hid_t ftypes[] =  {
            string_type(32),string_type(32),H5T_NATIVE_INT,H5T_NATIVE_INT
               };

  const size_t foffsets[] ={
             ((char*)&default_struct.name - (char*)&default_struct),
             ((char*)&default_struct.attach_obj_name - (char*)&default_struct),
             ((char*)&default_struct.attached_obj_type - (char*)&default_struct),
             ((char*)&default_struct.attached_obj_no - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.name ),
         sizeof( default_struct.attach_obj_name ),
         sizeof( default_struct.attached_obj_type ),
         sizeof( default_struct.attached_obj_no )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type qext
*/
void qext_clear_buffer_f(){
  //qext_table::instance().buffer().destroy();
  qext_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void qext_append_to_buffer_f(const  char a_name[32],const  char a_attach_obj_name[32],const int * a_attached_obj_type,const int * a_attached_obj_no, int * ierror,
              const int name_len,const int attach_obj_name_len)
{
 _TRAP_EXCEPT(*ierror,
   qext_table::instance().buffer().push_back(
                                      qext(
                                      a_name,a_attach_obj_name,*a_attached_obj_type,*a_attached_obj_no
                                      ));
 ) // end of exception trap
}

/** both makes the table and writes the contents of the buffer to it */
void qext_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  qext_table & table = qext_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( qext_table::instance().description.title.c_str(),
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
		                                       1,                     //qext_table::instance().description.compress,
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void qext_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    qext_table & table = qext_table::instance();
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
void qext_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id,
				     qext_table::instance().description.title.c_str(),
				     &nfields,
				     nrecords));
 ) // end of exception trap
}



/** get one row worth of information from the buffer */
void qext_query_from_buffer_f(int32_t* row,
                         char a_name[32], char a_attach_obj_name[32],int * a_attached_obj_type,int * a_attached_obj_no, int * ierror,
              int name_len,int attach_obj_name_len
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > qext_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    qext obj =qext_table::instance().buffer()[ndx];
    memcpy(a_name,obj.name,32);
    memcpy(a_attach_obj_name,obj.attach_obj_name,32);
    *a_attached_obj_type=obj.attached_obj_type;
    *a_attached_obj_no=obj.attached_obj_no;
    if (strlen(a_name) < 32)fill(a_name+strlen(a_name),a_name+32,' ');
    if (strlen(a_attach_obj_name) < 32)fill(a_attach_obj_name+strlen(a_attach_obj_name),a_attach_obj_name+32,' ');
    name_len=(int)strlen(a_name);
        attach_obj_name_len=(int)strlen(a_attach_obj_name);
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void qext_prioritize_buffer_f(int* ierror)
{
 _TRAP_EXCEPT(*ierror,
  qext_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type qext */
int qext_buffer_size_f()
{
  return (int) qext_table::instance().buffer().size();
}

void qext_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("qext");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<qext> & obs = qext_table::instance().buffer();
   qext_table& table = qext_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount)
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<qext>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {
           const qext & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void qext_write_buffer_to_text_f(const char* file,
                                      const bool* append,
                                      int* ierror,
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);

  qext_write_buffer_to_stream(out,*append);
  ) // end of exception trap
}






