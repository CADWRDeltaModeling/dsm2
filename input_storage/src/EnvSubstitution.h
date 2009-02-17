#ifndef ENVSUBSTITUTION_H__
#define ENVSUBSTITUTION_H__
#include<string>
#include<map>

/**
 Class that performs string substitution based on environment variables and a map
 of user-defined substitutions.
*/

class EnvSubstitution
{
 
 public:
  EnvSubstitution(){}
  ~EnvSubstitution()
  {
      m_subMap.clear();
  }

  void add(std::string key, std::string value);
  void remove(std::string key);
  std::string operator()(const std::string& arg);

private:
    std::map<std::string, std::string> m_subMap;
};

#endif

