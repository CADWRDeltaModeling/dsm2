#ifndef LAYERMANAGER_H
#define LAYERMANAGER_H
#include "hdf5.h"
#include "hdf5_hl.h"
#include "boost/algorithm/string/case_conv.hpp"
#include "boost/algorithm/string/split.hpp"
#include "boost/algorithm/string/classification.hpp"
#include<vector>
#include<string>
#include<iostream>

using namespace boost::algorithm;

/** Manages an ordered list of layer names */
class LayerManager {

public:
  /** Return singleton instance */
  static LayerManager& instance() {
    static LayerManager _instance;
    return _instance;
  }
   
  /** Generate a layer name out of a filename (e.g., by dropping extension) */
  std::string generateLayerName(const std::string & fileName);

  /** Get the name of the layer with the given index */
  std::string layerName(int index);

  /** Get the number of layers registered (so far) */
  int getNumberLayers();

  /** Add a new layer to the list of layers.
      The new layer will be added in the next incremental position
  */
  int addLayer(const std::string& name);
  
  /** Get the index of the layer with the given name */
  int layerIndex(std::string& name);

  /** Clear all layer index assignments */
  void clearAllLayer();

  /** Write the ordered list of layers to an hdf5 table at the given location */
  void writeToHdf5(const hid_t & file_id, const std::string& group_name);
  
  /** Write the ordered list of layers to an output stream 
      using prefix at the begining of each line (e.g., a comment)  
  */
  void writeToStream(std::ostream & stream, const std::string& prefix);  

 private:  
  /* more (non-static, singleton - enforcing) functions here */
  LayerManager(){}
  ~LayerManager(){}
  std::vector<std::string> layers;
};


#endif
