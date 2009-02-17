#include "InsertFileState.h"
#include "FileInputState.h"
#include<boost/filesystem/operations.hpp>

#include<iostream>

InputStatePtr InsertFileState::process(istream& in)
{  
  while(true)
    {
      string line;
      getline(in,line);
      line = strip(line);             // strip comments, trailing/leading whitespace
      if (line.size()==0) continue;
      if (in.eof()){
        handleFatalError("Unexpected end of file in file: "+m_filename);
      }
      if ( isBlockEnd(line))
	{ 
	  InputStatePtr newState(new FileInputState(m_contextItems,m_filename));
          return newState;
	}

      if( isActive())
	{
	  string filename(line);
	  //todo: handle file does not exist, still check even if not active?
	  boost::filesystem::path p(filename);
	  if (!boost::filesystem::exists(p))
	    { 
	      //todo
	      handleFatalError( "File does not exist" + filename);
	    }
          
	  InputStatePtr newState ( new FileInputState(m_contextItems,filename));
	  newState->setActiveItems(m_activeItems);
	  ifstream newStream(filename.c_str());
	  int nTransition = 0;
	  while (! newState->isEndOfFile())
	    {
	      newState = newState->process(newStream);
	      nTransition++;
	      if (nTransition > 1000) handleFatalError("Too many state transitions, something is wrong in input processor"); //todo hardwired number
	    }
	}

      
    }
}



