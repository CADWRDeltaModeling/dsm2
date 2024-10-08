#include "EnvSubstitution.h"
#include<iostream>
#include<string>
#include "boost/regex.hpp"
#include "boost/algorithm/string/replace.hpp"
#define _CRT_SECURE_NO_DEPRECATE
using namespace boost::algorithm;
using namespace boost;
using namespace std;

EnvSubstitution::~EnvSubstitution()
{
    m_subMap.clear();
}

void EnvSubstitution::add(string name, string value)
{
  if (m_subMap.find(name) != m_subMap.end())
    {   //todo: real logging
		cerr << "Warning: redefinition of text substitution variable:\n"
             << name << ": " << value << endl;
    }
  string subbedVal = (*this)(value);
  m_subMap[name]=subbedVal;
}

void EnvSubstitution::remove(string name)
{
}

void EnvSubstitution::setEnabledUser(const bool& enabled)
{
    m_enabledUser=enabled;
}

void EnvSubstitution::setEnabledEnvironment(const bool& enabled)
{
    m_enabledEnvironment=enabled;
}

void EnvSubstitution::setNotFoundIsError(const bool & isError)
{
	m_notFoundIsError=isError;
}

string EnvSubstitution::operator()(const std::string & arg)
{
    std::string str(arg);

    //if(arg.find_first_of("${") == string::npos) return arg;
	// need to add dashes to this list (I don't know
	// whether it is supposed to be without escape (-) or with escape (\-)
    boost::regex expr("\\$\\{([a-zA-Z0-9_@%\\^ \\-]*)\\}");
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
		if (m_enabledEnvironment)
		{
            const char* envvar = getenv(toReplace.c_str());
            if (envvar != NULL)
            {
                string envWithBrace="${" + toReplace + "}";
                replace_all(str,envWithBrace,string(envvar));
                found = true;
            }
		}
        if (m_enabledUser)
        {
            if(m_subMap.find(toReplace) != m_subMap.end())
            {
                std::string replacement = m_subMap[toReplace];
                boost::algorithm::replace_all(str, "${"+toReplace+"}", replacement);
                found = true;
            }
        }
        if (!found && m_notFoundIsError)
		{
			    string message("Fatal error in text substitution. Envvar not found:\n");
                message=message+toReplace;
                throw runtime_error(message); // todo unify fatal error handling
        }
    }
    if (m_notFoundIsError && str.find_first_of("$") != string::npos )
    {
        string message("Text substitution error in line.\n");
        message += string("The error is most likely a syntax error, not a missing ENVVAR.\n");
        message += string("Did you forget curly braces?\n\nProblem Line:\n");
        message += str;
        throw runtime_error(message);
    }
     return str;
}


