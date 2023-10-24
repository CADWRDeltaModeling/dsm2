#ifndef oprule_parser_NAMEDVALUELOOKUPIMPL_H__INCLUDED_
#define oprule_parser_NAMEDVALUELOOKUPIMPL_H__INCLUDED_

#include "boost/shared_ptr.hpp"
#include "oprule/parser/NamedValueLookup.h"
#include "oprule/expression/ExpressionNode.h"
#include "oprule/rule/ModelInterface.h"
#include "oprule/parser/ModelNameParseError.h"
#include<string>
#include<deque>
#include<vector>
#include<map>
#include<exception>
#include<iostream>

namespace oprule{
namespace parser{

struct ModelNameInfo{
   typedef oprule::expression::DoubleNodePtr DoubleNodePtr;
   std::string name;
   NamedValueLookup::NameReadWriteType type;
   NamedValueLookup::ArgList params;
   DoubleNodePtr (*factory)(const NamedValueLookup::ArgMap&);

};



class NamedValueLookupImpl : public NamedValueLookup{
  typedef std::map<std::string,ModelNameInfo> NameMapType;
  typedef NameMapType::iterator NameMapIterator;

public:
   void add(const std:: string& name,
            const ModelNameInfo & info){
     if (namemap.find(name) != namemap.end())
        throw std::domain_error("Model name entered twice");
     else namemap[name] = info;
  }

  bool isModelName(const std::string & name){
    _current=namemap.find(name);
    return(_current != namemap.end());
  }

  bool takesArguments(const std::string &name){
    assert (isModelName(name)); //precondition
    ModelNameInfo& info=getModelNameInfo(name);
	namemap.find("modelname") == namemap.end();
    return !info.params.empty() && (  // must have params
		namemap.size() > 1 ||         // eliminate case where only one and it is called modelname
		(namemap.size() == 1 && namemap.find("modelname") == namemap.end())
		);
  }

  NamedValueLookup::NameReadWriteType readWriteType(const std::string &name){
    assert (isModelName(name)); //precondition
    ModelNameInfo& info=getModelNameInfo(name);
    return info.type;
  }


  oprule::expression::DoubleNode::NodePtr getModelExpression(
     const std::string & name, const ArgMap & param){
	bool found=isModelName(name);  // Don't remove -- needed for prefetch
	if (!found)
		throw oprule::parser::ModelNameNotFound(name+" not found in registry of model names");
    ModelNameInfo& info=getModelNameInfo(name);
    return (*info.factory)(param);
  }

  oprule::rule::ModelInterface<double>::NodePtr getModelInterface(
     const std::string & name, const ArgMap & param){
     return boost::static_pointer_cast<oprule::rule::ModelInterfaceDouble>(
        getModelExpression(name,param));
  }

private:
  std::vector<std::string> paramlist; //todo@what is this?
  NameMapType namemap;
  NameMapIterator _current;
  ModelNameInfo&  getModelNameInfo(const std::string & name)
  {
     if(isModelName(name))return _current->second;
     else throw std::domain_error(
        "Requested model name does not exist, which is a violation of a programming precondition. Method getModelNameInfo should only be used after isModelName.");
  }
};


}} //namespace
#endif // include guard