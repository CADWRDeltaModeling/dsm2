#ifndef ITEMINPUTSTATE_H__
#define ITEMINPUTSTATE_H__
#include "InputState.h"
#include "LayerManager.h"

/** InputState that reads concrete input items of type <T>.
    The template parameter represents the class
    that is being input. 
    \ingroup inputstates
*/
template<typename T>
class ItemInputState : public InputState
{
public:

  /*ItemInputState(const InputStateMap  & a_inputStateMap,
	       const vector<string> & a_contextItems)
  :
  InputState(a_inputStateMap,a_contextItems)
  {}*/

ItemInputState()
  : InputState()
{}

virtual ~ItemInputState(){}

/** Process a concrete item inside of an input block.
This function will validate the headers of the item and then
process subsequent lines and create new rows in the corresponding
HDFTableManager<T> buffer.
*/
InputStatePtr process(istream& in);

protected:
virtual void onFilenameSet();
int m_layerIndex;

private:
void processItem(string & line);
bool verifyHeader(string & line);

};


#include "ItemInputState.cpp"
#endif
