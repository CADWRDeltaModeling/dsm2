#include "ApplicationTextReader.h"
#include "FileInputState.h"
#include<fstream>
#include<boost/filesystem/operations.hpp>

using namespace std;

///////////////////////
void ApplicationTextReader::setInputStateMap(const ApplicationTextReader::InputStateMap & a_inputMap)
{
  m_inputMap = a_inputMap;
  for(InputState::InputStateMap::iterator it = m_inputMap.begin();
      it != m_inputMap.end() ; ++it)
    {
      it->second->setEnvSubstitution(m_sub);
    }
}


const vector<std::string> ApplicationTextReader::allKeywords()
{
  vector<string> allKeys;
  for(InputStateMap::iterator it = m_inputMap.begin();
      it != m_inputMap.end(); ++it)
    {
      allKeys.push_back(it->first);
    }
  return allKeys;
}


EnvSubstitution & ApplicationTextReader::getTextSubstitution()
{
    return m_sub;
}


///////////////////
void ApplicationTextReader::setTextSubstitution(const EnvSubstitution & a_sub)
{
    m_sub=a_sub;   //todo: what about applying it?
    for(InputState::InputStateMap::iterator it = m_inputMap.begin();
      it != m_inputMap.end() ; ++it)
    {
      it->second->setEnvSubstitution(m_sub);
    }
}

///////////////////
void ApplicationTextReader::setActiveItems(const vector<string> & a_activeItems)
{
  m_activeItems = a_activeItems;
}

////////////////////
vector<string> & ApplicationTextReader::getActiveItems()
{
  return m_activeItems;
}

/////////////////////
void ApplicationTextReader::setInitialContextItems(const vector<string> & a_contextItems)
{
  m_initialContextItems = a_contextItems;
}

////////////////////
vector<string> & ApplicationTextReader::getInitialContextItems()
{
  return m_initialContextItems;
}


bool ApplicationTextReader::verifyItemsInMap(std::vector<std::string> items)
{
    bool verify = true;
    for (size_t i = 0; i<items.size() && verify ; ++i)
    {
        // if item not on map
        verify &= (m_inputMap.find(items[i]) != m_inputMap.end());
    }
    return verify;
}

bool ApplicationTextReader::isKeyword(const std::string & name) const
{
	 return m_inputMap.find(name) != m_inputMap.end();
}

/////////////////////
/* Read text starting from the given file */
void ApplicationTextReader::processInput(const string & filename)
{
  boost::filesystem::path p(filename);
  if (!boost::filesystem::exists(p))
    {
      //todo
      cerr << "File does not exist" << endl;
    }

  assert(! this->m_inputMap.empty());
  if (!verifyItemsInMap(m_initialContextItems))
  {
	  // m_initialContextItems are the items we are trying to set valid
	  // we just looked them up to see if they are keywords we know about and
	  // they are not. This means somehow we probably didn't load the original
	  // keywords (m_inputMap) correctly
      throw logic_error("An item being set valid for reading is not one of the ones known by the text parser");
  }
  if (!verifyItemsInMap(m_activeItems))
  {
	  // m_activeItems are the items we are trying to set active
	  // we just looked them up to see if they are keywords we know about and
	  // they are not. This means somehow we probably didn't load the original
	  // keywords (m_inputMap) correctly
	  throw logic_error("An item being set active for reading is not one of the ones known by the text parser");
  }

  // to do: assumes all active items are valid in initial file context
  InputStatePtr startState(new FileInputState(m_initialContextItems,filename));
  startState->setActiveItems(m_activeItems);
  InputStatePtr currentState(startState);
  std::ifstream input(filename.c_str());

  while (!currentState->isEndOfFile())
    {
      currentState = currentState->process(input);
    }
  input.close();

}

