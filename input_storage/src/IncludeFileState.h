#ifndef INCLUDEFILESTATE_H__
#define INCLUDEFILESTATE_H__
#include "InputState.h"

/** InputState for handling include files by opening the files 
    and passing them to FileInputStates.
    \ingroup inputstates
*/
class IncludeFileState : public InputState
{
public:


IncludeFileState(const vector<string> & a_contextItems)
  :
  InputState(a_contextItems)
{}


IncludeFileState(InputStatePtr state,string & filename, const int & lineNo = 0) :
  InputState(*state)
  {
    m_filename=filename;
    m_lineNo=lineNo;
  }

/** Process input by treating lines as file names, opening the files and
    delegating to new FileInputStates.
*/
virtual InputStatePtr process(istream & in);

  /** Inform new state of the items that were legal at the last state.
      This implementation caches the previous state so it can restore it.
   */
virtual void setIncomingContextItems(const vector<string> & a_contextItems)
{ 
  m_cachedContextItems = a_contextItems;
}



private:
InputStatePtr nextState() const;


vector<string> m_cachedContextItems;
};



#endif
