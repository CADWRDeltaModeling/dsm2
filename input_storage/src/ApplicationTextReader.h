#ifndef APPLICATIONTEXTREADER_H__
#define APPLICATIONTEXTREADER_H__
#include<string>
#include<vector>
#include<map>
#include "InputState.h"


/**A high level management class for using the text reader in applications.
   This class makes several
, a state machine using InputStates to traverse files of input */
class ApplicationTextReader
{
public:

  static ApplicationTextReader& instance() {
    static ApplicationTextReader _instance;
    return _instance;
  }

  /** The input state map is a map of string-reader pairs.
	  The string represents a keyword. When a keyword is 
	  encountered, the parser will look up the corresponding
	  ItemInputState and use it to parse the section.
  */
  typedef std::map<std::string, InputStatePtr> InputStateMap;

  /** Set the InputStateMap, completely overwriting the previous map
      The EnvSubstitution for members of the incoming map.
  */
  void setInputStateMap(const InputStateMap & a_inputMap);

  /** Get a reference to the current InputStateMap 
  */
  InputStateMap& getInputMap(){return m_inputMap;}

  /** Set the text substitution object for the reader
      You won't necessarily use this function in an application,
	  and there is an empty existing one automatically. The  
 	  more typical programming model would be to call getTextSubstitution()
	  to get a reference to the existing one and add/clear items.
  */    
  void setTextSubstitution(const EnvSubstitution & textSub);

  /** Get a reference to the current text substitution instance
      The text substitution object is of class EnvSubstitution
  */
  EnvSubstitution & getTextSubstitution();

  /** Set the list of items (keywords) that will be read */
  void setActiveItems(const std::vector<std::string> & a_activeItems);

  /** Get a copy of all items in reader, including include categories*/
  const std::vector<std::string> allKeywords();

  /** Get a reference to the list of active items */
  std::vector<std::string> & getActiveItems();

  /** Set the list of items (keywords) that will be read */
  void setInitialContextItems(const std::vector<std::string> & a_activeItems);

  /** Get a reference to the list of active items */
  std::vector<std::string> & getInitialContextItems();

  /** Read text starting from the given file */
  void processInput(const std::string & filename);


 private:
  InputStateMap m_inputMap;
  std::vector<std::string> m_activeItems;
  std::vector<std::string> m_initialContextItems;
  EnvSubstitution m_sub;
  
  /* Verify that the given list of items is in the input map */
  bool verifyItemsInMap(std::vector<std::string> items);

};


#endif

