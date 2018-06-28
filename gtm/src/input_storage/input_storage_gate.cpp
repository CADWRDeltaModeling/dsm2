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
#include "input_storage_gate.h"
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
ostream& operator<<(ostream & stream, const gate & obj)
{  
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<  
            setw(max(4+16,(int)(4+strlen(obj.name))))
            << setfill(' ')
            << left
            << quote_spaces(obj.name, 32)  
        << 
            setw(max(4+16,(int)(4+strlen(obj.from_obj))))
            << setfill(' ')
            << left
            << quote_spaces(obj.from_obj, 16)  
        << 
            setw(max(4+16,(int)(4+strlen(obj.from_identifier))))
            << setfill(' ')
            << left
            << quote_spaces(obj.from_identifier, 32)  
        << setw(9)
            << setfill(' ')
            << left
            << obj.to_node  
        ;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, gate & obj)
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
   if(beg->size()<= 16)
   {
        strcpy(obj.from_obj, (beg++)->c_str());
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
   if(beg->size()<= 32)
   {
        strcpy(obj.from_identifier, (beg++)->c_str());
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
        tokenstrm >> obj.to_node;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert to_node to correct data type:"+tempstr);
        }
        ;
  return stream;
}

template<>
HDFTableManager<gate>::HDFTableManager() :
    description(gate_table_description()),  
    m_default_fill(gate("","","",-901)){}

template<>
void HDFTableManager<gate>::prioritize_buffer()
{

    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    vector<gate>::const_iterator dupl = adjacent_find(buffer().begin(),buffer().end());
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
    buffer().erase(unique(buffer().begin(),buffer().end(),identifier_equal<gate>()),buffer().end());
    // Eliminate items that are not used. This must be done after lower layers have been removed
    buffer().erase(remove_if(buffer().begin(), buffer().end(),not1(entry_used<gate>())), buffer().end());
    
}

TableDescription gate_table_description(){
  const char* title = "gate";
  const size_t size = sizeof(gate);
  const size_t nfields = 4;
  gate default_struct = gate("","","",-901);
  const char* fnames[] =  {"name","from_obj","from_identifier","to_node"};
  const hid_t ftypes[] =  {
            string_type(32),string_type(16),string_type(32),H5T_NATIVE_INT
               };

  const size_t foffsets[] ={
             ((char*)&default_struct.name - (char*)&default_struct),
             ((char*)&default_struct.from_obj - (char*)&default_struct),
             ((char*)&default_struct.from_identifier - (char*)&default_struct),
             ((char*)&default_struct.to_node - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.name ),
         sizeof( default_struct.from_obj ),
         sizeof( default_struct.from_identifier ),
         sizeof( default_struct.to_node )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type gate
*/  
void gate_clear_buffer_f(){
  //gate_table::instance().buffer().destroy();
  gate_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void gate_append_to_buffer_f(const  char a_name[32],const  char a_from_obj[16],const  char a_from_identifier[32],const int * a_to_node, int * ierror, 
              const int name_len,const int from_obj_len,const int from_identifier_len)
{
 _TRAP_EXCEPT(*ierror,
   gate_table::instance().buffer().push_back(
                                      gate(
                                      a_name,a_from_obj,a_from_identifier,*a_to_node
                                      ));
 ) // end of exception trap
}
  
/** both makes the table and writes the contents of the buffer to it */
void gate_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  gate_table & table = gate_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( gate_table::instance().description.title.c_str(), 
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
		                                       1,                     //gate_table::instance().description.compress, 
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void gate_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    gate_table & table = gate_table::instance();
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
void gate_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
				     gate_table::instance().description.title.c_str(), 
				     &nfields, 
				     nrecords));
 ) // end of exception trap
}


    
/** get one row worth of information from the buffer */
void gate_query_from_buffer_f(size_t* row, 
                         char a_name[32], char a_from_obj[16], char a_from_identifier[32],int * a_to_node, int * ierror, 
              int name_len,int from_obj_len,int from_identifier_len
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > gate_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    gate obj =gate_table::instance().buffer()[ndx];
    memcpy(a_name,obj.name,32);
    memcpy(a_from_obj,obj.from_obj,16);
    memcpy(a_from_identifier,obj.from_identifier,32);
    *a_to_node=obj.to_node;
    if (strlen(a_name) < 32)fill(a_name+strlen(a_name),a_name+32,' ');
    if (strlen(a_from_obj) < 16)fill(a_from_obj+strlen(a_from_obj),a_from_obj+16,' ');
    if (strlen(a_from_identifier) < 32)fill(a_from_identifier+strlen(a_from_identifier),a_from_identifier+32,' ');
    name_len=(int)strlen(a_name);
        from_obj_len=(int)strlen(a_from_obj);
        from_identifier_len=(int)strlen(a_from_identifier);
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void gate_prioritize_buffer_f(int* ierror)
{  
 _TRAP_EXCEPT(*ierror,
  gate_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type gate */
int gate_buffer_size_f()
{ 
  return (int) gate_table::instance().buffer().size();
}

void gate_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("gate");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<gate> & obs = gate_table::instance().buffer();
   gate_table& table = gate_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount) 
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<gate>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {  
           const gate & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void gate_write_buffer_to_text_f(const char* file, 
                                      const bool* append, 
                                      int* ierror, 
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);
  
  gate_write_buffer_to_stream(out,*append); 
  ) // end of exception trap  
}






