#include "EnvSubstitution.h"
#include<iostream>
#include<string>
#include "boost/regex.hpp"
#include "boost/algorithm/string/replace.hpp"
#define _CRT_SECURE_NO_DEPRECATE
using namespace boost::algorithm;
using namespace boost;
using namespace std;

void EnvSubstitution::add(string name, string value)
{
  if (m_subMap.find(name) != m_subMap.end())
    {
      cerr << "Warning: redefinition" << endl;
    }
  string subbedVal = (*this)(value); 
  m_subMap[name]=subbedVal;
}

void EnvSubstitution::remove(string name)
{
}


string EnvSubstitution::operator()(const std::string & arg)
{
    std::string str(arg);

    //if(arg.find_first_of("${") == string::npos) return arg;
    boost::regex expr("\\$\\{([a-zA-Z0-9_@%\\^ ]*)\\}");
    sregex_iterator m1(str.begin(), str.end(), expr);
    sregex_iterator m2;
    vector<string> subList;
    // list all matches of the ${...} pattern
    for( ; (m1 != m2) ; m1++ )
    {
        subList.push_back((*m1)[1].str());
    }
    // go through the list of envvars needing substitution
    for (size_t isub = 0; isub < subList.size(); ++isub)
    {
        string toReplace=subList[isub];
        bool found = false;
        if(m_subMap.find(toReplace) != m_subMap.end())
        {
            std::string replacement = m_subMap[toReplace];
            boost::algorithm::replace_all(str, "${"+toReplace+"}", replacement);
            found = true;
        }
        else
        { 
            const char* envvar = getenv(toReplace.c_str());
            if (envvar != NULL)
            {   
                string envWithBrace="${" + toReplace + "}";
                replace_all(str,envWithBrace,string(envvar));
                found = true;
            }
        }
//        if (!found)
//        {
//            string message("Fatal error in text substitution: envvar not found: ");
//            message=message+toReplace;
//            throw logic_error(message); // todo unify fatal error handling
//        }
    }
    return str;
}


