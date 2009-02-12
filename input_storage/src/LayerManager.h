#ifndef LAYERMANAGER_H
#define LAYERMANAGER_H
#include "boost/algorithm/string/case_conv.hpp"
#include "boost/algorithm/string/split.hpp"
#include "boost/algorithm/string/classification.hpp"
#include<vector>
#include<string>

using namespace boost::algorithm;

class LayerManager {

public:

  static LayerManager& instance() {
    static LayerManager _instance;
    return _instance;
  }
   
  string layerName(const string & fileName)
  {
    string name = fileName.substr(0,fileName.size()-4); //todo: real extension
    vector<string> parts;
    split(parts,name,is_any_of("__"))[1];          //todo: not really what we want
    if (parts.size() == 1) name = parts[0];
    else name = parts[1];

    to_lower(name);
    return name ;
  }
  
  int addLayer(const string& name)
  { 
      if (find(layers.begin(),layers.end(),name) == layers.end())
	{
	  layers.push_back(name);
          return layers.size()-1;
	}
      //todo: make sure this isn't slowing us
      return (find(layers.begin(),layers.end(),name) - layers.begin());
  }   
   
  int layerIndex(string& name)
  {
      return (find(layers.begin(),layers.end(),name) - layers.begin());
  }

  string layerName(int index)
  {
    return layers[index];
  }


 private:  
  /* more (non-static, singleton - enforcing) functions here */
  LayerManager(){};
  ~LayerManager(){}


  std::vector<string> layers;
};

#endif
