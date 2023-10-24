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
  EnvSubstitution() :
	   m_enabledUser(true),
	   m_enabledEnvironment(true),
	   m_notFoundIsError(true){}

  ~EnvSubstitution();

  /** Enable substitution based on application-provided
      name-value pairs.
  */
  void setEnabledUser(const bool& enabled);

  /** Enable substitution based on OS environment variables */
  void setEnabledEnvironment(const bool& enabled);

  /** Determined whether failure to substitute is a runtime_error.
      Otherwise ignored. */
  void setNotFoundIsError(const bool & isError);

  void add(std::string key, std::string value);
  void remove(std::string key);
  std::string operator()(const std::string& arg);

private:
    std::map<std::string, std::string> m_subMap;
    bool m_enabledUser;
	bool m_enabledEnvironment;
	bool m_notFoundIsError;
};

#endif

