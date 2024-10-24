#ifndef INPUTSTATE_H__
#define INPUTSTATE_H__
//#include<iostream> //debug
#include<map>
#include<string>
#include<vector>
#include "boost/shared_ptr.hpp"
#include "EnvSubstitution.h"

#define END "END"
#define COMMENT "#"
#define REMOVE "^"
/** \defgroup inputstates Input Text Processing States */


class InputState;
using namespace std;
typedef boost::shared_ptr<InputState> InputStatePtr;

/**
 Abstract superclass of states that read input. The base class
 contains some code to strip lines of leading and trailing
 space, remove comments and detect that the line is intended to
 remove previous entries
 \ingroup inputstates
*/
class InputState
{
public:

typedef map<string, InputStatePtr > InputStateMap;
typedef vector<string> StringVec;

/** Construct the InputState
    @arg  a_inputStateMap A map of block headers to
	      corresponding reader InputState subclasses
    @arg a_contextItems A list of input blocks that are legal "below" this context. This list is
          used to determine which items this reader will allow itself to transition to...for instance
          a file input state would have as its context items a list of item states.
*/
InputState(const vector<string> & a_contextItems)
  :
  m_contextItems(a_contextItems),
  m_active(true),
  m_lineNo(0)
{
}

/** Construct the InputState with no context items or state map
*/
InputState() : m_active(true){}


  /** Set the list of items valid in this context
       (e.g., allowed in the file or include block */
void setContextItems(const vector<string> & a_contextItems)
{
  m_contextItems = a_contextItems;
}

  /** Inform new state of the items that were legal at the last state.
      This is used at transitions. The default is to pass on the context
	  items using setContextItems
   */
virtual void setIncomingContextItems(const vector<string> & a_contextItems)
{
  setContextItems(a_contextItems);
}


/** Get the list of items valid in this context (e.g., allowed in the file */
const vector<string>& getContextItems()
{
  return m_contextItems;
}


/** Virtual destructor */
virtual ~InputState()
 {
   m_contextItems.clear();
 }

/** Return whether this InputState is active.
*/
bool isActive() const
{
  return m_active;
}

/** Set the activation state of the InputState (default is true).
     This function is called by the text reading
     framework, not the user or client program, deactivating
     InputStates in some contexts so that input processing
     can be abbreviated or skipped.  Client programs will
     not want to set activation with this function
     (and it may not stay set if you do).
     Instead, set the list of active items using \ref setActiveItems on the initial state
     -- this list will then be passed on to subsequent model state.
*/
void setActive(const bool & a_active)
{
  m_active=a_active;
}

/** Set a list of activated item names */
virtual void setActiveItems(const StringVec & a_activeItems)
{
  m_activeItems = a_activeItems;
}


/** Process stream of input. Subclasses must
// provide processing that consumes some of the input
// and/or end in failure. The function consumes input until
// a new state is encountered.
*/
virtual InputStatePtr process(istream & in)=0;

/**
// Determine if a line represents the end of an input block.
// Subclasses can alter this, but the base implementation assumes
// there is a string constant in the macro END that
// delineates the end of the section.
*/
virtual bool isBlockEnd(string & line);

/**
// Strip a line of leading and trailing space and comments
*/
string strip(const string& line) const;

/** Perform text/environment substitution */
string substitute(const string& line) const;


/** Determine if an item is active
   The list of active items is set by the calling program. And
   forwarded to the next state upon transition.
   Active is not the same as "recognized".
   An item that is not active must still be a legal input,
   it just isn't active in the current round of parsing.
   See isItemAllowed
*/
bool isItemActive(const string & item) const;


/** Determine if an item is allowed in the current
// context. For instance, the current file name
// or include file might be restricted to include
// certain types of input. An item that fails this test
// represents bad input.
*/
virtual bool isItemAllowed(const string & item) const;

/**
// Unified way of handling fatal error
*/
void handleFatalError(const string & message,
                      const string & line,
                      const string & filename,
                      const int& lineNo) const;

/**
  Query if end of file is reached
*/
virtual bool isEndOfFile()
{ return false; }

/** Set the line number that the processor is on.
  This is for when you leave to an include file and come back
 */
void setLineNo(const int& a_lineNo)
{
    m_lineNo = a_lineNo;
}

/** Set the name of the file being processed */
void setFilename(const string & a_filename)
 {
   m_filename = a_filename;
   onFilenameSet();
 }

/** Get the name of the file being processed */
string getFilename() const
{
  return m_filename;
}

/** Set the EnvSubstitution object that will be used for text substitution */
void setEnvSubstitution(EnvSubstitution & substitution)
{
  m_substitution=substitution;
}



protected:
StringVec                       m_activeItems;
StringVec                       m_contextItems;
string                          m_filename;
bool                            m_active;
int                             m_lineNo;
EnvSubstitution                 m_substitution;

virtual void onFilenameSet(){}

};



#endif




