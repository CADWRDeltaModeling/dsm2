#ifndef FILEINPUTSTATE_H__
#define FILEINPUTSTATE_H__
#include "InputState.h"
#include <iostream>
#include <fstream>

/** InputState that represents marching
    through a file looking for input blocks. The
    reader can transition to an item reader, an
    insert file reader or various end/error states.
    \ingroup inputstates
*/
class FileInputState : public InputState
{
public:


/**
 Construct a new FileInputState. The arguments represent
 a map of input states that the state can transition to as
 well as the name and line of the file being processed.
*/
FileInputState(
	           const vector<string> & a_contextItems,
	           const string         & a_fileName,
               const int            & a_lineNo=0
	   ) : 
  InputState(a_contextItems)
{  
  m_filename=a_fileName;
  m_lineNo=a_lineNo;
}

/** Determine if an item is allowed in the current
    context. Checks against all items allowed in context 
	and the list of all recognized items. 
*/
bool isItemAllowed(const string & item) const;


/** Process input stream in the general part of the input file
    scanning  for keywords. When a section is discovered, the
    state transitions to (ie returns) the state that can handle
    the corresponding input
*/
virtual InputStatePtr process(istream &in);


private:
InputStatePtr nextState(const string& line) const;

};



#endif
