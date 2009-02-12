#ifndef ENVSUBSTITUTION_H__
#define ENVSUBSTITUTION_H__
#include<string>
#include<iostream>
#include "boost/regex.hpp"
#include "boost/algorithm/string/replace.hpp"

using namespace std;
using namespace boost::algorithm;
using namespace boost;
/**
 Class that performs string substitution based on environment variables and a map
 of user-defined substitutions.
*/

class EnvSubstitution
{
 
 public:
  EnvSubstitution(){}

  void add(string key, string value);
  void remove(string key);
  string operator()(string& arg);

private:
    map<string, string> m_subMap;

};




#endif

