#include "FileInputState.h"
#include "EndOfFileState.h"
#include<iostream>
#include "boost/algorithm/string/case_conv.hpp"
#include<assert.h>

InputStatePtr FileInputState::process(istream& in)
{
  int nProcess=0;
  while(nProcess < 1000)
    {
      string line;
      getline(in,line);
      // strip comments, trailing/leading whitespace
      line = strip(line);
      if (in.eof())  //todo: last line in file?
	{ 
          if (! line.size() == 0)
	    {
	      handleFatalError("File must end in a terminal carriage return: " + m_filename);
	    }
          InputStatePtr next(new EndOfFileState(m_filename));
	  return next;
	}
      if (line.size()==0)   //skip if empty
	{ 
	  continue;
	}
      // If we got here, the line is "substantial"
      // Find the correct next state and return it
      return nextState(line);
      nProcess++;
    }
  handleFatalError("End of file not found, processing in endless loop");  
  return InputStatePtr(new EndOfFileState(m_filename));
}


InputStatePtr FileInputState::nextState(const string &line) const
{
  assert(line.size() > 0);
  string item(line);
  boost::algorithm::to_upper(item);
  InputStateMap::const_iterator it = m_inputStateMap.find(item);
  if (it == m_inputStateMap.end())
    { 
      for (InputStateMap::const_iterator mapIter = m_inputStateMap.begin() ;
           mapIter != m_inputStateMap.end() ; ++ mapIter) cout << "Item: " << mapIter->first << endl;
      handleFatalError(" Keyword not recognized:" + item + ":");
    }
   
   if (! isItemAllowed(item))
     {
       handleFatalError("Keyword  not allowed in context:\n" + item);
     }

   InputStatePtr next = it->second;
   next->setFilename(m_filename);
   next->setInputStateMap(m_inputStateMap);
   next->setContextItems(m_contextItems);
   next->setActiveItems(m_activeItems);
   next->setActive( find(m_activeItems.begin(),m_activeItems.end(),item) != m_activeItems.end());
   return next;
}
