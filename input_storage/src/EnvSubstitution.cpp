#include "EnvSubstitution.h"

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





string EnvSubstitution::operator()(string & arg)
{
  string str = arg;
  if(arg.find_first_of("${") == string::npos) return arg;
  boost::regex expr("\\$\\{([a-zA-Z0-9_@%\\^ ]*)\\}");
  sregex_iterator m1(str.begin(), str.end(), expr);
  sregex_iterator m2;
  vector<string> subList;
  for( ; (m1 != m2) ; m1++ )
    {
      subList.push_back((*m1)[1].str());
    }
  for (int isub = 0; isub < subList.size(); ++isub)
    {
      string toReplace=subList[isub];
      bool found = false;
      if(m_subMap.find(toReplace) != m_subMap.end())
	{
	  string replacement = m_subMap[toReplace];
	  replace_all(str, "${"+toReplace+"}", replacement);
          found = true;
	}
      else
	{ 
	  const char* envvar = getenv(toReplace.c_str());
	  if (envvar != NULL)
	    {
	      replace_all(str,"${"+toReplace+"}",string(envvar));
              found = true;
	    }
	}
      if (!found)
	{
	  throw logic_error("Fatal error in text substitution: envvar not found: " + toReplace); // todo unify fatal error handling
	}
    }
  return str;
}


