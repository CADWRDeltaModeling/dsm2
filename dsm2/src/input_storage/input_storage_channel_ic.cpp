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
#include "input_storage_channel_ic.h"
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
ostream& operator<<(ostream & stream, const channel_ic & obj)
{  
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<  setw(9)
            << setfill(' ')
            << left
            << obj.chan_no  
        << 
            setw(max(4+8,(int)(4+strlen(obj.distance))))
            << setfill(' ')
            << left
            << quote_spaces(obj.distance, 8)  
        <<
            setw(10)
            << setfill(' ')
            << setprecision(4)
            << left
            << obj.stage  
        <<
            setw(12)
            << setfill(' ')
            << setprecision(4)
            << left
            << obj.flow  
        ;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, channel_ic & obj)
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
   if(beg->size()<= 8)
   {
        strcpy(obj.distance, (beg++)->c_str());
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
        obj.stage = strtod((beg++)->c_str(),NULL);
         

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.flow = strtod((beg++)->c_str(),NULL);
         ;
  return stream;
}

template<>
HDFTableManager<channel_ic>::HDFTableManager() :
    description(channel_ic_table_description()),  
    m_default_fill(channel_ic(-901,"",-901.0,-901.0)){}

template<>
void HDFTableManager<channel_ic>::prioritize_buffer()
{

    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    vector<channel_ic>::const_iterator dupl = adjacent_find(buffer().begin(),buffer().end());
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
    buffer().erase(unique(buffer().begin(),buffer().end(),identifier_equal<channel_ic>()),buffer().end());
    // Eliminate items that are not used. This must be done after lower layers have been removed
    buffer().erase(remove_if(buffer().begin(), buffer().end(),not1(entry_used<channel_ic>())), buffer().end());
    
}

TableDescription channel_ic_table_description(){
  const char* title = "channel_ic";
  const size_t size = sizeof(channel_ic);
  const size_t nfields = 4;
  channel_ic default_struct = channel_ic(-901,"",-901.0,-901.0);
  const char* fnames[] =  {"chan_no","distance","stage","flow"};
  const hid_t ftypes[] =  {
            H5T_NATIVE_INT,string_type(8),H5T_NATIVE_DOUBLE,H5T_NATIVE_DOUBLE
               };

  const size_t foffsets[] ={
             ((char*)&default_struct.chan_no - (char*)&default_struct),
             ((char*)&default_struct.distance - (char*)&default_struct),
             ((char*)&default_struct.stage - (char*)&default_struct),
             ((char*)&default_struct.flow - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.chan_no ),
         sizeof( default_struct.distance ),
         sizeof( default_struct.stage ),
         sizeof( default_struct.flow )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type channel_ic
*/  
void channel_ic_clear_buffer_f(){
  //channel_ic_table::instance().buffer().destroy();
  channel_ic_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void channel_ic_append_to_buffer_f(const int * a_chan_no,const  char a_distance[8],const double * a_stage,const double * a_flow, int * ierror, 
              const int distance_len)
{
 _TRAP_EXCEPT(*ierror,
   channel_ic_table::instance().buffer().push_back(
                                      channel_ic(
                                      *a_chan_no,a_distance,*a_stage,*a_flow
                                      ));
 ) // end of exception trap
}
  
/** both makes the table and writes the contents of the buffer to it */
void channel_ic_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  channel_ic_table & table = channel_ic_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( channel_ic_table::instance().description.title.c_str(), 
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
		                                       1,                     //channel_ic_table::instance().description.compress, 
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void channel_ic_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    channel_ic_table & table = channel_ic_table::instance();
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
void channel_ic_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
				     channel_ic_table::instance().description.title.c_str(), 
				     &nfields, 
				     nrecords));
 ) // end of exception trap
}


    
/** get one row worth of information from the buffer */
void channel_ic_query_from_buffer_f(size_t* row, 
                        int * a_chan_no, char a_distance[8],double * a_stage,double * a_flow, int * ierror, 
              int distance_len
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > channel_ic_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    channel_ic obj =channel_ic_table::instance().buffer()[ndx];
    *a_chan_no=obj.chan_no;
    memcpy(a_distance,obj.distance,8);
    *a_stage=obj.stage;
    *a_flow=obj.flow;
    if (strlen(a_distance) < 8)fill(a_distance+strlen(a_distance),a_distance+8,' ');
    distance_len=(int)strlen(a_distance);
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void channel_ic_prioritize_buffer_f(int* ierror)
{  
 _TRAP_EXCEPT(*ierror,
  channel_ic_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type channel_ic */
int channel_ic_buffer_size_f()
{ 
  return (int) channel_ic_table::instance().buffer().size();
}

void channel_ic_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("channel_ic");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<channel_ic> & obs = channel_ic_table::instance().buffer();
   channel_ic_table& table = channel_ic_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount) 
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<channel_ic>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {  
           const channel_ic & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void channel_ic_write_buffer_to_text_f(const char* file, 
                                      const bool* append, 
                                      int* ierror, 
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);
  
  channel_ic_write_buffer_to_stream(out,*append); 
  ) // end of exception trap  
}






