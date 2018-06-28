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
#include "input_storage_channel.h"
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
ostream& operator<<(ostream & stream, const channel & obj)
{  
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<  setw(9)
            << setfill(' ')
            << left
            << obj.chan_no  
        <<
            setw(10)
            << setfill(' ')
            << setprecision(4)
            << left
            << obj.manning  
        << setw(8)
            << setfill(' ')
            << left
            << obj.upnode  
        << setw(10)
            << setfill(' ')
            << left
            << obj.downnode  
        ;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, channel & obj)
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
        obj.manning = strtod((beg++)->c_str(),NULL);
         

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.upnode;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert upnode to correct data type:"+tempstr);
        }
        

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.downnode;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert downnode to correct data type:"+tempstr);
        }
        ;
  return stream;
}

template<>
HDFTableManager<channel>::HDFTableManager() :
    description(channel_table_description()),  
    m_default_fill(channel(-901,-901.0,-901,-901)){}

template<>
void HDFTableManager<channel>::prioritize_buffer()
{

    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    vector<channel>::const_iterator dupl = adjacent_find(buffer().begin(),buffer().end());
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
    buffer().erase(unique(buffer().begin(),buffer().end(),identifier_equal<channel>()),buffer().end());
    // Eliminate items that are not used. This must be done after lower layers have been removed
    buffer().erase(remove_if(buffer().begin(), buffer().end(),not1(entry_used<channel>())), buffer().end());
    
}

TableDescription channel_table_description(){
  const char* title = "channel";
  const size_t size = sizeof(channel);
  const size_t nfields = 4;
  channel default_struct = channel(-901,-901.0,-901,-901);
  const char* fnames[] =  {"chan_no","manning","upnode","downnode"};
  const hid_t ftypes[] =  {
            H5T_NATIVE_INT,H5T_NATIVE_DOUBLE,H5T_NATIVE_INT,H5T_NATIVE_INT
               };

  const size_t foffsets[] ={
             ((char*)&default_struct.chan_no - (char*)&default_struct),
             ((char*)&default_struct.manning - (char*)&default_struct),
             ((char*)&default_struct.upnode - (char*)&default_struct),
             ((char*)&default_struct.downnode - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.chan_no ),
         sizeof( default_struct.manning ),
         sizeof( default_struct.upnode ),
         sizeof( default_struct.downnode )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type channel
*/  
void channel_clear_buffer_f(){
  //channel_table::instance().buffer().destroy();
  channel_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void channel_append_to_buffer_f(const int * a_chan_no,const double * a_manning,const int * a_upnode,const int * a_downnode, int * ierror)
{
 _TRAP_EXCEPT(*ierror,
   channel_table::instance().buffer().push_back(
                                      channel(
                                      *a_chan_no,*a_manning,*a_upnode,*a_downnode
                                      ));
 ) // end of exception trap
}
  
/** both makes the table and writes the contents of the buffer to it */
void channel_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  channel_table & table = channel_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( channel_table::instance().description.title.c_str(), 
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
		                                       1,                     //channel_table::instance().description.compress, 
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void channel_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    channel_table & table = channel_table::instance();
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
void channel_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
				     channel_table::instance().description.title.c_str(), 
				     &nfields, 
				     nrecords));
 ) // end of exception trap
}


    
/** get one row worth of information from the buffer */
void channel_query_from_buffer_f(size_t* row, 
                        int * a_chan_no,double * a_manning,int * a_upnode,int * a_downnode, int * ierror
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > channel_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    channel obj =channel_table::instance().buffer()[ndx];
    *a_chan_no=obj.chan_no;
    *a_manning=obj.manning;
    *a_upnode=obj.upnode;
    *a_downnode=obj.downnode;
    
    
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void channel_prioritize_buffer_f(int* ierror)
{  
 _TRAP_EXCEPT(*ierror,
  channel_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type channel */
int channel_buffer_size_f()
{ 
  return (int) channel_table::instance().buffer().size();
}

void channel_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("channel");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<channel> & obs = channel_table::instance().buffer();
   channel_table& table = channel_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount) 
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<channel>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {  
           const channel & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void channel_write_buffer_to_text_f(const char* file, 
                                      const bool* append, 
                                      int* ierror, 
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);
  
  channel_write_buffer_to_stream(out,*append); 
  ) // end of exception trap  
}






