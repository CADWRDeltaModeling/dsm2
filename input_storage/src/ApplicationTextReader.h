#ifndef APPLICATIONTEXTREADER_H__
#define APPLICATIONTEXTREADER_H__
#include<string>
#include<vector>
#include<map>
#include "InputState.h"


/**Tne main text reader, a state machine using InputStates to traverse files of input */
class ApplicationTextReader
{
public:

  static ApplicationTextReader& instance() {
    static ApplicationTextReader _instance;
    return _instance;
  }

  typedef std::map<std::string, InputStatePtr> InputStateMap;

  /** Set the input map, completely overwriting the previous map
      The EnvSubstitution for members of the incoming map.
  */
  void setInputStateMap(const InputStateMap & a_inputMap);

  InputStateMap& getInputMap(){return m_inputMap;}

  /** Set the text substitution object for the reader */    
  void setTextSubstitution(const EnvSubstitution & textSub);

  /** Set the list of items (keywords) that will be read */
  void setActiveItems(const std::vector<std::string> & a_activeItems);

  void ApplicationTextReader::setAllActive();

  std::vector<std::string> & getActiveItems();

  /** Read text starting from the given file */
  void processInput(const std::string & filename);

 private:
  InputStateMap m_inputMap;
  std::vector<std::string> m_activeItems;
  EnvSubstitution m_sub;

};


#endif

