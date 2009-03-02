#ifndef INSERTFILESTATE_H__
#define INSERTFILESTATE_H__
#include "InputState.h"

/** InputState for handling include files by opening the files 
    and passing them to FileInputStates.
    \ingroup inputstates
*/
class InsertFileState : public InputState
{
public:


InsertFileState(const vector<string> & a_contextItems)
  :
  InputState(a_contextItems)
{}



InsertFileState(InputStatePtr state,string & filename) :
  InputState(*state)
  {
    m_filename=filename;
  }

/** Process input by treating lines as file names, opening the files and
    delegating to new FileInputStates.
*/
virtual InputStatePtr process(istream & in);

private:
InputStatePtr nextState() const;
};



#endif
