#include "LayerManager.h"
#include<string>
#include "boost/algorithm/string/trim.hpp"
#include<iostream> //debug
#include <iomanip>
using namespace std;

string LayerManager::generateLayerName(const string & fileName)
{
    string trimmed =  boost::algorithm::trim_copy(fileName);
     //todo: really use extension using boost, not this guess at 4 characters
    //string name = trimmed.substr(0,trimmed.size()-4);
    string name = trimmed;
    to_lower(name);
    return name;
}

string LayerManager::layerName(int index)
{
    return layers[index];
}

int LayerManager::addLayer(const string& name)
{ 
    if (find(layers.begin(),layers.end(),name) == layers.end())
    {
        layers.push_back(name);
        return (int) layers.size()-1;
    }
    //todo: make sure this isn't slowing us  // todo: verify it is std::find, not boost
    return (int)(std::find(layers.begin(),layers.end(),name) - layers.begin());
}   

void LayerManager::clearAllLayer()
{   
    layers.clear();
}

int LayerManager::layerIndex(string& name)
{
    return (int) (find(layers.begin(),layers.end(),name) - layers.begin());
}

int LayerManager::getNumberLayers()
{
    return (int) layers.size();
}

/* Write the ordered list of layers to an output stream 
    Adds prefix at the begining of each line (e.g., a comment) 
*/
void LayerManager::writeToStream(ostream & stream, 
                                 const std::string& prefix)
{   
    stream << prefix << "Entry file (top priority):  " << layers[0] << endl;
    stream << prefix << "Other files, increasing priority: " << endl;
    if (layers.size() == 1)
    {
        stream << prefix << "None" << endl;
    }
    else
    {
        for (size_t i = 1; i < layers.size(); ++i)
        {
            stream << prefix << setw(5) << left << i << layers[i] << endl;
        }
    }
    stream << endl;
}

void LayerManager::writeToHdf5(const hid_t & file_id, const string& group_name)
{
   const int NFIELDS=2;
   const int NAMELEN=64;
   typedef struct Layer
   {
   int    priority;
   char name[NAMELEN];
   } Layer;

   vector<Layer> data(layers.size());
   for (size_t i = 0; i<layers.size(); ++i)
       {   Layer layer;
           data.push_back(layer);
           data[i].priority=(int)i;
           strcpy(data[i].name,layers[i].c_str());
       }

   /* Calculate the size and the offsets of our struct members in memory */
   size_t dst_size =  sizeof( Layer );
   size_t dst_offset[NFIELDS] = { HOFFSET( Layer, priority ),
                                  HOFFSET( Layer, name )};

   size_t dst_sizes[NFIELDS] = { sizeof( data[0].priority),
                                 sizeof( data[0].name)};

   /* Field information */
   const char *field_names[NFIELDS] =
   { "number","name" };
  
   /* Initialize the field field_type */
   hid_t      field_type[NFIELDS];
   hid_t string_type = H5Tcopy( H5T_C_S1 );
   H5Tset_size( string_type, NAMELEN);
   field_type[0] = H5T_NATIVE_INT;
   field_type[1] = string_type;

   hsize_t    chunk_size = 10;
   int        *fill_data = NULL;
   int        compress  = 0;
   herr_t     status;


   /* make a table */
   status=H5TBmake_table("layers",file_id,
                         "layers",NFIELDS,layers.size(),
                         dst_size, field_names, 
                         dst_offset, field_type,
                         chunk_size, fill_data, 
                         compress, &data[0]);
}



