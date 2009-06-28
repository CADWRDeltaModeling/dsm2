#include "InputState.h"
#include<iostream>
#include<sstream>
#include "boost/algorithm/string/trim.hpp"

bool InputState::isBlockEnd(string & line)
{ 
  return line == END; 
}


string InputState::strip(const string& line) const
{
  if (line.find_first_of("\t") != string::npos)
  {
      string message("Line has tabs: please remove and set your text editor to replace tabs with spaces\n");
      handleFatalError(message,line,m_filename,m_lineNo);
  }
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
  // general category or in a 
  // file starting with the item name
  // todo: should be file return( m_category == "" || m_file.startsWith(item));
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
