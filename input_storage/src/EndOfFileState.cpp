#include "EndOfFileState.h"

InputStatePtr EndOfFileState::process(istream& in)
{ 
  throw std::logic_error("End of file reached. Processing not allowed");
  return InputStatePtr();
}




