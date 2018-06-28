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
#include "input_storage_gate_pipe_device.h"
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
ostream& operator<<(ostream & stream, const gate_pipe_device & obj)
{  
  quote_if_spaces quote_spaces;
  stream.setf(ios_base::fixed,ios_base::floatfield);
  return stream <<  
            setw(max(4+16,(int)(4+strlen(obj.gate_name))))
            << setfill(' ')
            << left
            << quote_spaces(obj.gate_name, 32)  
        << 
            setw(max(4+16,(int)(4+strlen(obj.device))))
            << setfill(' ')
            << left
            << quote_spaces(obj.device, 32)  
        << setw(12)
            << setfill(' ')
            << left
            << obj.nduplicate  
        <<
            setw(10)
            << setfill(' ')
            << setprecision(3)
            << left
            << obj.radius  
        <<
            setw(10)
            << setfill(' ')
            << setprecision(3)
            << left
            << obj.elev  
        <<
            setw(14)
            << setfill(' ')
            << setprecision(4)
            << left
            << obj.cf_from_node  
        <<
            setw(14)
            << setfill(' ')
            << setprecision(4)
            << left
            << obj.cf_to_node  
        << 
            setw(max(4+18,(int)(4+strlen(obj.default_op))))
            << setfill(' ')
            << left
            << quote_spaces(obj.default_op, 16)  
        ;
}

/** Read the table item from an input stream */
istream& operator>> (istream& stream, gate_pipe_device & obj)
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
        strcpy(obj.gate_name, (beg++)->c_str());
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
        strcpy(obj.device, (beg++)->c_str());
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
        tokenstrm >> obj.nduplicate;  // strtol(tempStr.c_str(),NULL,10);
        if (!tokenstrm.eof())
        {
          throw invalid_argument("Could not convert nduplicate to correct data type:"+tempstr);
        }
        

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.radius = strtod((beg++)->c_str(),NULL);
         

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.elev = strtod((beg++)->c_str(),NULL);
         

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.cf_from_node = strtod((beg++)->c_str(),NULL);
         

        if (beg == end)
        {
            throw runtime_error("Fewer input fields received than expected");
        }        
        obj.cf_to_node = strtod((beg++)->c_str(),NULL);
         

   if (beg == end)
   {
     throw runtime_error("Fewer input fields received than expected");
   }        
   if(beg->size()<= 16)
   {
        strcpy(obj.default_op, (beg++)->c_str());
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
HDFTableManager<gate_pipe_device>::HDFTableManager() :
    description(gate_pipe_device_table_description()),  
    m_default_fill(gate_pipe_device("","",-901,-901.0,-901.0,-901.0,-901.0,"")){}

template<>
void HDFTableManager<gate_pipe_device>::prioritize_buffer()
{
 
    // Sort by identifier (lexicographical order) and
    // layer (decreasing order of priority)
    std::sort(buffer().begin(),buffer().end());
    // remove if my version != parent version that is used
    buffer().erase(remove_if(buffer().begin(),
                           buffer().end(),
                           not1(mem_fun_ref(&gate_pipe_device::parent_valid))),buffer().end());
    
}

TableDescription gate_pipe_device_table_description(){
  const char* title = "gate_pipe_device";
  const size_t size = sizeof(gate_pipe_device);
  const size_t nfields = 8;
  gate_pipe_device default_struct = gate_pipe_device("","",-901,-901.0,-901.0,-901.0,-901.0,"");
  const char* fnames[] =  {"gate_name","device","nduplicate","radius","elev","cf_from_node","cf_to_node","default_op"};
  const hid_t ftypes[] =  {
            string_type(32),string_type(32),H5T_NATIVE_INT,H5T_NATIVE_DOUBLE,H5T_NATIVE_DOUBLE,H5T_NATIVE_DOUBLE,H5T_NATIVE_DOUBLE,string_type(16)
               };

  const size_t foffsets[] ={
             ((char*)&default_struct.gate_name - (char*)&default_struct),
             ((char*)&default_struct.device - (char*)&default_struct),
             ((char*)&default_struct.nduplicate - (char*)&default_struct),
             ((char*)&default_struct.radius - (char*)&default_struct),
             ((char*)&default_struct.elev - (char*)&default_struct),
             ((char*)&default_struct.cf_from_node - (char*)&default_struct),
             ((char*)&default_struct.cf_to_node - (char*)&default_struct),
             ((char*)&default_struct.default_op - (char*)&default_struct)
                           };

  const size_t fsizes[] = {
         sizeof( default_struct.gate_name ),
         sizeof( default_struct.device ),
         sizeof( default_struct.nduplicate ),
         sizeof( default_struct.radius ),
         sizeof( default_struct.elev ),
         sizeof( default_struct.cf_from_node ),
         sizeof( default_struct.cf_to_node ),
         sizeof( default_struct.default_op )
                          };
  const hsize_t chunk_size = 10;
  TableDescription descr(title,size,nfields,fnames,ftypes,foffsets,fsizes,chunk_size);
  return descr;
}



/**
  Clear the storage buffer for objects of type gate_pipe_device
*/  
void gate_pipe_device_clear_buffer_f(){
  //gate_pipe_device_table::instance().buffer().destroy();
  gate_pipe_device_table::instance().buffer().clear();
}

/** append to buffer, compatible with fortran, returns new size*/
void gate_pipe_device_append_to_buffer_f(const  char a_gate_name[32],const  char a_device[32],const int * a_nduplicate,const double * a_radius,const double * a_elev,const double * a_cf_from_node,const double * a_cf_to_node,const  char a_default_op[16], int * ierror, 
              const int gate_name_len,const int device_len,const int default_op_len)
{
 _TRAP_EXCEPT(*ierror,
   gate_pipe_device_table::instance().buffer().push_back(
                                      gate_pipe_device(
                                      a_gate_name,a_device,*a_nduplicate,*a_radius,*a_elev,*a_cf_from_node,*a_cf_to_node,a_default_op
                                      ));
 ) // end of exception trap
}
  
/** both makes the table and writes the contents of the buffer to it */
void gate_pipe_device_write_buffer_to_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
  gate_pipe_device_table & table = gate_pipe_device_table::instance();
    *ierror = static_cast<int>( H5TBmake_table( gate_pipe_device_table::instance().description.title.c_str(), 
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
		                                       1,                     //gate_pipe_device_table::instance().description.compress, 
		                                      table.buffer().size() > 0 ? &table.buffer()[0] : NULL));
  ) // end of exception trap
}

/** reads the table in from a file into the buffer*/
void gate_pipe_device_read_buffer_from_hdf5_f(const hid_t* file_id, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields;
    hsize_t nrecords;
    gate_pipe_device_table & table = gate_pipe_device_table::instance();
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
void gate_pipe_device_number_rows_hdf5_f(const hid_t *file_id, hsize_t* nrecords, int* ierror){
 _TRAP_EXCEPT(*ierror,
    hsize_t nfields = 0;
    *ierror = static_cast<int>(  H5TBget_table_info (*file_id, 
				     gate_pipe_device_table::instance().description.title.c_str(), 
				     &nfields, 
				     nrecords));
 ) // end of exception trap
}


    
/** get one row worth of information from the buffer */
void gate_pipe_device_query_from_buffer_f(size_t* row, 
                         char a_gate_name[32], char a_device[32],int * a_nduplicate,double * a_radius,double * a_elev,double * a_cf_from_node,double * a_cf_to_node, char a_default_op[16], int * ierror, 
              int gate_name_len,int device_len,int default_op_len
                        )
{
 _TRAP_EXCEPT(*ierror,
    //if (row > gate_pipe_device_table::instance().buffer().size()) return -2; //todo: HDF_STORAGE_ERROR;
    size_t ndx = *row - 1;
    gate_pipe_device obj =gate_pipe_device_table::instance().buffer()[ndx];
    memcpy(a_gate_name,obj.gate_name,32);
    memcpy(a_device,obj.device,32);
    *a_nduplicate=obj.nduplicate;
    *a_radius=obj.radius;
    *a_elev=obj.elev;
    *a_cf_from_node=obj.cf_from_node;
    *a_cf_to_node=obj.cf_to_node;
    memcpy(a_default_op,obj.default_op,16);
    if (strlen(a_gate_name) < 32)fill(a_gate_name+strlen(a_gate_name),a_gate_name+32,' ');
    if (strlen(a_device) < 32)fill(a_device+strlen(a_device),a_device+32,' ');
    if (strlen(a_default_op) < 16)fill(a_default_op+strlen(a_default_op),a_default_op+16,' ');
    gate_name_len=(int)strlen(a_gate_name);
        device_len=(int)strlen(a_device);
        default_op_len=(int)strlen(a_default_op);
 ) // end of exception trap
}

/** Prioritize buffer by layers, delete unused items and sort */
void gate_pipe_device_prioritize_buffer_f(int* ierror)
{  
 _TRAP_EXCEPT(*ierror,
  gate_pipe_device_table::instance().prioritize_buffer();
   ) // end of exception trap
}

/** Query the size of the storage buffer for objects of type gate_pipe_device */
int gate_pipe_device_buffer_size_f()
{ 
  return (int) gate_pipe_device_table::instance().buffer().size();
}

void gate_pipe_device_write_buffer_to_stream(ostream & out, const bool& append)
{
   string keyword("gate_pipe_device");
   boost::to_upper(keyword);
   out << keyword <<endl;
   vector<gate_pipe_device> & obs = gate_pipe_device_table::instance().buffer();
   gate_pipe_device_table& table = gate_pipe_device_table::instance();
   for (size_t icount = 0; icount < table.description.nfields; ++ icount) 
   {
     string name = table.description.field_names[icount];
     boost::to_upper(name);
     out <<  name << "  ";
   }
   out << endl;
   for (vector<gate_pipe_device>::const_iterator it = obs.begin();
        it != obs.end(); ++it)
        {  
           const gate_pipe_device & outitem = *it;
           out << outitem << endl;
        }
   out << "END\n" << endl;
}

void gate_pipe_device_write_buffer_to_text_f(const char* file, 
                                      const bool* append, 
                                      int* ierror, 
                                      int filelen)
{
 _TRAP_EXCEPT(*ierror,
  string filename(file,filelen);
  boost::filesystem::path p(filename);
  ios_base::openmode mode = *append ? (ios::out | ios::ate | ios::app) : (ios::out | ios::trunc );
  ofstream out(filename.c_str(),mode);
  
  gate_pipe_device_write_buffer_to_stream(out,*append); 
  ) // end of exception trap  
}






