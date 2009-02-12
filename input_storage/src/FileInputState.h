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
 well as a list of the ones that are activated in the current
 context and te name of the file being processed.
*/
FileInputState(const InputStateMap  & a_inputStateMap,
	       const vector<string> & a_contextItems,
	       const string         & a_fileName
	   ) :
  InputState(a_inputStateMap,a_contextItems),
  m_filename(a_fileName)
{}


/** Process input stream in the general part of the input file
    scanning  for keywords. When a section is discovered, the
    state transitions to (ie returns) the state that can handle
    the corresponding input
*/
virtual InputStatePtr process(istream &in);


private:
InputStatePtr nextState(const string& line) const;
string m_filename;

};



#endif
