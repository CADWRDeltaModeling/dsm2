#include <stdint.h>
#include "oprule/parser/ParseSymbolManagement.h"
#include "boost/checked_delete.hpp"
#include "boost/shared_ptr.hpp"
#include<iostream> //debug
// static data structures housed in this module
// todo: move to a namespace or class
vector<symbol > express_index;
express_map express;
rule_map rulenames;
initial_value_map init_cond;
lagged_expression_list laggedvals;
NamedValueLookup::ArgList arglist;
NamedValueLookup::ArgMap argmap;
oprule::parser::parse_type _parsed_type;
NamedValueLookup* lookup;
ModelTimeNodeFactory* appModelTimeFactory;
vector<string*> string_list;
vector<vector<double> > arrays;

// setters and getters
vector<symbol>            & get_express_index(){return express_index;}
express_map               & get_express_map(){return express;}
rule_map                  & get_rule_map(){return rulenames;}
initial_value_map         & get_init_cond(){return init_cond;}
lagged_expression_list    & get_lagged_vals(){return laggedvals;}
NamedValueLookup::ArgList & get_arg_list(){return arglist;}
NamedValueLookup::ArgMap  & get_arg_map(){ return argmap;}
NamedValueLookup          * get_lookup(){return lookup;}
ModelTimeNodeFactory      * get_time_factory(){return appModelTimeFactory;}
string                    * add_to_string_list(char*);

void clear_string_list(){ 
    std::for_each(string_list.begin(),string_list.end(),boost::checked_delete<string>);
    string_list.clear();
}

symbol & get_temp_symbol(int index)
{
   return get_express_index()[index];
}

int add_to_string_list(const string & a_str)
{
  return add_temp_symbol(symbol(a_str));
}

int add_array_vector()
{
  arrays.push_back(vector<double>());
  return arrays.size()-1;
}

vector<double> & get_array_vector(int index)
{
  return arrays[index];
}

void clear_array_vectors()
{
  arrays.clear();
}


/** Get a boolean valued expression stored by its name */
BoolNodePtr getBoolExpression(const char * name)
{
  if (name == 0){
    return express[EVAL_STR].boolval;
  }
  else
  { 
    string sb=string(name);
    return express[sb].boolval;
  }
}

/** Get a real-valued expression stored by its name */
DoubleNodePtr getDoubleExpression(const char *name)
{
  if (name ==0)
  {
     return express[EVAL_STR].dblval;
  }
  else 
  { 
     string sd=string(name);
     return express[sd].dblval;
  }
}

OperatingRulePtr getOperatingRule(const string & name)
{
  return rulenames[name];
}


template<class T1>
bool add_to_collection(T1                         &collection, 
                       const typename T1::key_type & key,
                       typename T1::mapped_type & item)
{
   typedef typename T1::iterator IterType;
   typedef typename T1::mapped_type ReferentType;
   typedef typename T1::key_type KeyType;
   if (key == EVAL_STR){
      collection[EVAL_STR]=item;
	  return true;
   }
   IterType i = collection.find(key);
   if (i == collection.end())
   {
      //pair<KeyType,ReferentType> vb = make_pair<KeyType,ReferentType>(key, item);
	  //collection.insert(vb);

      collection[key]=item;
      return true;
   }else{
      collection[key]=item;
      return false;
   }
}



bool add_symbol( const string & key, symbol& expression)
{
	return add_to_collection<express_map>( express, key, expression);
}


bool add_rule(const string & name, OperatingRulePtr rule)
{
  return add_to_collection<rule_map>(rulenames, name, rule);
}

void init_lookup(NamedValueLookup * a_lookup)
{
   lookup=a_lookup;
}

void init_model_time_factory(ModelTimeNodeFactory * tnf)
{
   appModelTimeFactory=tnf;
}


void init_rule_names(){

    rulenames.clear();
}


void init_expression()
{
  //if (! express.empty()){
     express.clear(); 
  // }
} 


symbol name_lookup(const string& name)
{
   // attempt to look up previously defined and named oprule expression
   expressIterType i = express.find(name);            
   // attempt oprule (previously parsed) expression
   if (i != express.end()) {
       return ((*i).second);
   }else{
     return symbol();
   }   
     
}


symbol make_symbol(DoubleNodePtr dnode)
{
  symbol result(dnode);
  return result; 
}

symbol make_symbol(BoolNodePtr bnode)
{
  symbol result(bnode);
  return result; 
 }


OperationActionPtr chain_actions( OperationActionPtr op1, OperationActionPtr op2)
{ 
    if (op1->getCollectionType() == OperationAction::CHAIN_COLLECTION)
    {
       op2->registerParent(op1); // todo: circular reference?
       boost::static_pointer_cast<ActionChain,OperationAction>(op1)->pushBackAction(op2);
       return op1;
    }
    else
    {
       OperationActionPtr chain(new ActionChain());
       op1->registerParent(chain);
       op2->registerParent(chain);
       boost::static_pointer_cast<ActionChain,OperationAction>(chain)->pushBackAction(op1);
       boost::static_pointer_cast<ActionChain,OperationAction>(chain)->pushBackAction(op2);
       return chain;
    }
}


OperationActionPtr group_actions( OperationActionPtr op1, OperationActionPtr op2)
{
  assert(op1 != NULL);
  assert(op1->isApplicable());
  if (op1->getCollectionType() == OperationAction::SET_COLLECTION)
  {  
     op2->registerParent(op1);
     boost::static_pointer_cast<ActionSet,OperationAction>(op1)->addAction(op2);
     return op1;
  }
  else{ 
     OperationActionPtr set=OperationActionPtr(new ActionSet());
     op1->registerParent(set);
     op2->registerParent(set);
     boost::static_pointer_cast<ActionSet,OperationAction>(set)->addAction(op1);
     boost::static_pointer_cast<ActionSet,OperationAction>(set)->addAction(op2);
     return set;
  }
}

void apply_lagged_values(const string& name, DoubleNodePtr expr){
  for(lagged_expression_list::iterator i =laggedvals.begin(); 
                           i < laggedvals.end(); i++){
    if (i->first == name){
        i->second->setExpression(expr);
	}else{
        i->second->setExpression(name_lookup(name).dblval);
    }
  }
}

void clear_arg_map(){ argmap.clear();}

void add_expression(const string& name, 
                    DoubleNodePtr expr)
{
  oprule::parser::symbol asymbol(expr);
     add_symbol(name, asymbol);
}


void clear_temp_expr(void)
{
  express_index.clear();
}



void set_parsed_type(oprule::parser::parse_type ptype)
{
    _parsed_type = ptype; 
}

oprule::parser::parse_type get_parsed_type()
{
    return _parsed_type; 
}


long padded_string_to_int(const string& str)
{
   char* end;
   if(str.substr(0,1) == "0"){
      return strtol(str.substr(1,1).c_str(),&end,0);
   }else{ 
      return strtol(str.substr(0,2).c_str(),&end,0);
	}
}	
