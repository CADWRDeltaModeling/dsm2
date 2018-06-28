#ifndef ENDOFFILESTATE_H__
#define ENDOFFILESTATE_H__
#include "InputState.h"
#include <stdexcept>

/** InputState representing the reaching of the end of a file. This state
    does no text processing. 
    \ingroup inputstates
*/
class EndOfFileState : public InputState
{
public:

/** Constructor based on filename */
EndOfFileState(const string & filename) :
    m_filename(filename)
{}

/** Attempting to process with End of File causes fatal error */
InputStatePtr process(istream& in);


/** Override of method to indicate end of file. Returns true. */
bool isEndOfFile()
 {
   return true;
 }

private:
const string m_filename;

};


#endif
