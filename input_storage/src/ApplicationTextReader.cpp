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

void ApplicationTextReader::setAllActive()
{
  vector<string> activeItems;
  for(InputStateMap::iterator it = m_inputMap.begin();
      it != m_inputMap.end(); ++it)
    {
      activeItems.push_back(it->first);
    }
  m_activeItems = activeItems;
   
}

EnvSubstitution & ApplicationTextReader::getTextSubstitution()
{
    return m_sub;
}
    

///////////////////
void ApplicationTextReader::setTextSubstitution(const EnvSubstitution & a_sub)
{
    m_sub=a_sub;   //todo: what about appying it?
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
/* Read text starting from the given file */
void ApplicationTextReader::processInput(const string & filename)
{ 
  boost::filesystem::path p(filename);
  if (!boost::filesystem::exists(p))
    { 
      //todo
      cerr << "File does not exist" << endl;
    }
  
  // to do: assumes all active items are valid in initial file context
  InputStatePtr startState(new FileInputState(m_activeItems,filename));
  startState->setActiveItems(m_activeItems);  
  InputStatePtr currentState(startState);
  std::ifstream input(filename.c_str());

  while (!currentState->isEndOfFile())
    {
      currentState = currentState->process(input);
    }
  input.close();
   
}

