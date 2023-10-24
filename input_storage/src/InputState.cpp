#include "ApplicationTextReader.h"
#include "InputState.h"
#include<iostream>
#include<sstream>
#include <stdexcept>
#include "boost/algorithm/string/trim.hpp"


bool InputState::isBlockEnd(string & line)
{
  return line == END;
}


string InputState::strip(const string& line) const
{
  string trimmed =  boost::algorithm::trim_copy(line);
  size_t commentCol = trimmed.find_first_of(COMMENT);

  if (trimmed.size() == 0 || commentCol == 0)
    {
      return ""; // all blank or first non-blank is comment
    }
  if (commentCol != string::npos)
    {
      trimmed = trimmed.substr(0, commentCol);
      boost::algorithm::trim(trimmed);
    }
  return trimmed;
}

string InputState::substitute(const string& line) const
{
   ApplicationTextReader & reader = ApplicationTextReader::instance();
   string procline;
   try
   {
     procline = reader.getTextSubstitution()(line);
   }
   catch(std::runtime_error e)
   {
       string message = string("In file ")+this->getFilename()
                        +string(":\n\n")+string(e.what());
       throw std::runtime_error(message);
   }
   return procline;
}

// is this done yet?
// loadable items is intersection of all headers, requested set
// and items that are allowed in the current category
bool InputState::isItemActive(const string& item) const
{
  return find(m_activeItems.begin(),
	      m_activeItems.end(),item)
         != m_activeItems.end();
}


bool InputState::isItemAllowed(const string & item) const
{
    // We have no policy in general
	return true;
}

void InputState::handleFatalError(const string& message,
                                  const string& line,
                                  const string& filename,
                                  const int& lineNo) const
{
    std::stringstream errmsg;
    errmsg << message << endl
           <<"Line:"  << endl << line << endl
           << "(file: " << filename
           << " line: " << lineNo
           << ")" << endl;
    throw std::runtime_error(errmsg.str());
}
