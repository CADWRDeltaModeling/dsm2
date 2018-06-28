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
#include "input_storage_node_concentration.h"
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
ostream& operator<<(ostream & stream, const node_concentration & obj)
{  
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<  
            setw(max(4+16,(int)(4+strlen(obj.name))))
            << setfill(' ')
            << left
            << quote_spaces(obj.name, 32)  
        << setw(9)
            << setfill(' ')
            << left
            << obj.node_no  
        << 
            setw(max(4+16,(int)(4+strlen(obj.variable))))
            << setfill(' ')
            << left
            << quote_spaces(obj.variable, 16)  
        << 
            setw(max(4+12,(int)(4+strlen(obj.fillin))))
            << setfill(' ')
            << left
            << quote_spaces(obj.fillin, 8)  
        << 
            setw(max(4+1,(int)(4+strlen(obj.file))))
            << setfill(' ')
            << left
            << quote_spaces(obj.file, 128)  
        << 
            setw(max(4+50,(int)(4+strlen(obj.path))))
            << setfill(' ')
            << left
            << quote_spaces(obj.path, 80)  
        ;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, node_concentration & obj)
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
        tokenstrm.clear();
        tempstr = *(beg++);
        tokenstrm.str(tempstr);
        tokenstrm >> obj.node_no;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert node_no to correct data type:"+tempstr);
        }
        

   if (beg == end)
   {
     throw runtime_error("Fewer input fields received than expected");
   }        
   if(beg->size()<= 16)
   {
        strcpy(obj.variable, (beg++)->c_str());
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
   if(beg->size()<= 8)
   {
        strcpy(obj.fillin, (beg++)->c_str());
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
   if(beg->size()<= 128)
   {
        strcpy(obj.file, (beg++)->c_str());
   }
   else
   {
      cout << "fatal error" <<endl;
         throw logic_error("String too long (max width 128):" + (*beg));
   }
   

   if (beg == end)
   {
     throw runtime_error("Fewer input fields received than expected");
   }        
   if(beg->size()<= 80)
   {
        strcpy(obj.path, (beg++)->c_str());
   }
   else
   {
      cout << "fatal error" <<endl;
         throw logic_error("String too long (max width 80):" + (*beg));
   }
   ;
  return stream;
}

template<>
HDFTableManager<node_concentration>::HDFTableManager() :
    description(node_concentration_table_description()),  
    m_default_fill(node_concentration("",-901,"","","","")){}

template<>
void HDFTableManager<node_concentration>::prioritize_buffer()
{

    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    vector<node_concentration>::const_iterator dupl = adjacent_find(buffer().begin(),buffer().end());
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
    buffer().erase(unique(buffer().begin(),buffer().end(),identifier_equal<node_concentration>()),buffer().end());
    // Eliminate items that are not used. This must be done after lower layers have been removed
    buffer().erase(remove_if(buffer().begin(), buffer().end(),not1(entry_used<node_concentration>())), buffer().end());
    
}

TableDescription node_concentration_table_description(){
  const char* title = "node_concentration";
  const size_t size = sizeof(node_concentration);
  const size_t nfields = 6;
  node_concentration default_struct = node_concentration("",-901,"","","","");
  const char* fnames[] =  {"name","node_no","variable","fillin","file","path"};
  const hid_t ftypes[] =  {
            string_type(32),H5T_NATIVE_INT,string_type(16),string_type(8),string_type(128),string_type(80)
               };

  const size_t foffsets[] ={
             ((char*)&default_struct.name - (char*)&default_struct),
             ((char*)&default_struct.node_no - (char*)&default_struct),
             ((char*)&default_struct.variable - (char*)&default_struct),
             ((char*)&default_struct.fillin - (char*)&default_struct),
             ((char*)&default_struct.file - (char*)&default_struct),
             ((char*)&default_struct.path - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.name ),
         sizeof( default_struct.node_no ),
         sizeof( default_struct.variable ),
         sizeof( default_struct.fillin ),
         sizeof( default_struct.file ),
         sizeof( default_struct.path )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type node_concentration
*/  
void node_concentration_clear_buffer_f(){
  //node_concentration_table::instance().buffer().destroy();
  node_concentration_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void node_concentration_append_to_buffer_f(const  char a_name[32],const int * a_node_no,const  char a_variable[16],const  char a_fillin[8],const  char a_file[128],const  char a_path[80], int * ierror, 
              const int name_len,const int variable_len,const int fillin_len,const int file_len,const int path_len)
{
 _TRAP_EXCEPT(*ierror,
   node_concentration_table::instance().buffer().push_back(
                                      node_concentration(
                                      a_name,*a_node_no,a_variable,a_fillin,a_file,a_path
                                      ));
 ) // end of exception trap
}
  
/** both makes the table and writes the contents of the buffer to it */
void node_concentration_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  node_concentration_table & table = node_concentration_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( node_concentration_table::instance().description.title.c_str(), 
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
		                                       1,                     //node_concentration_table::instance().description.compress, 
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void node_concentration_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    node_concentration_table & table = node_concentration_table::instance();
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
void node_concentration_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
				     node_concentration_table::instance().description.title.c_str(), 
				     &nfields, 
				     nrecords));
 ) // end of exception trap
}


    
/** get one row worth of information from the buffer */
void node_concentration_query_from_buffer_f(size_t* row, 
                         char a_name[32],int * a_node_no, char a_variable[16], char a_fillin[8], char a_file[128], char a_path[80], int * ierror, 
              int name_len,int variable_len,int fillin_len,int file_len,int path_len
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > node_concentration_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    node_concentration obj =node_concentration_table::instance().buffer()[ndx];
    memcpy(a_name,obj.name,32);
    *a_node_no=obj.node_no;
    memcpy(a_variable,obj.variable,16);
    memcpy(a_fillin,obj.fillin,8);
    memcpy(a_file,obj.file,128);
    memcpy(a_path,obj.path,80);
    if (strlen(a_name) < 32)fill(a_name+strlen(a_name),a_name+32,' ');
    if (strlen(a_variable) < 16)fill(a_variable+strlen(a_variable),a_variable+16,' ');
    if (strlen(a_fillin) < 8)fill(a_fillin+strlen(a_fillin),a_fillin+8,' ');
    if (strlen(a_file) < 128)fill(a_file+strlen(a_file),a_file+128,' ');
    if (strlen(a_path) < 80)fill(a_path+strlen(a_path),a_path+80,' ');
    name_len=(int)strlen(a_name);
        variable_len=(int)strlen(a_variable);
        fillin_len=(int)strlen(a_fillin);
        file_len=(int)strlen(a_file);
        path_len=(int)strlen(a_path);
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void node_concentration_prioritize_buffer_f(int* ierror)
{  
 _TRAP_EXCEPT(*ierror,
  node_concentration_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type node_concentration */
int node_concentration_buffer_size_f()
{ 
  return (int) node_concentration_table::instance().buffer().size();
}

void node_concentration_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("node_concentration");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<node_concentration> & obs = node_concentration_table::instance().buffer();
   node_concentration_table& table = node_concentration_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount) 
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<node_concentration>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {  
           const node_concentration & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void node_concentration_write_buffer_to_text_f(const char* file, 
                                      const bool* append, 
                                      int* ierror, 
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);
  
  node_concentration_write_buffer_to_stream(out,*append); 
  ) // end of exception trap  
}






