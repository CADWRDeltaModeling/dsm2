#include "boost/algorithm/string/case_conv.hpp"
#include "boost/algorithm/string/split.hpp"
#include "boost/algorithm/string/classification.hpp"
#include<iostream>
#include<sstream>
#include "FileInputState.h"
#include "HDFTableManager.h"
#include "ItemInputState.h"

using namespace boost::algorithm;
using namespace boost;

template<typename T>
InputStatePtr ItemInputState<T>::process(istream& in)
{ 
  // Make sure that the header is present and OK
  while (true)
    {
      string line;
      getline(in,line);
      line = strip(line);             // strip comments, trailing/leading whitespace
      if (line.size()==0) continue;
      if (verifyHeader(line))
	{
	  break;               // header is in order, move on
	}
      else
	{
          handleFatalError(string("Bad header in line: \n")+line);
	}
    }
    // Process lines until END
    while(1)
    { 
      string line;
      getline(in,line);
      line = strip(line);              // strip comments, trailing/leading whitespace
      if (line.size()==0) continue;

      if (in.eof() && ! isBlockEnd(line))
	{
	  handleFatalError("End of file reached in middle of input block");
	}
      if(isBlockEnd(line))
	{
	  InputStatePtr newState(new FileInputState(m_contextItems, m_filename));
          newState->setActiveItems(m_activeItems);
          return newState;  // graceful return to file state
	}
      if (isActive()) processItem(line);               // process the item (//todo: where are errors handled?)
    }
 }


template<typename T>
bool ItemInputState<T>::verifyHeader(string& line)
{
  string header = line;
  to_upper(header);
  StringVec splitLine;
  split( splitLine, line, is_any_of(" \t"));
  StringVec colNames = splitLine;   //todo:   get from T
  bool headersOK = splitLine.size() == colNames.size();
  if (! headersOK) return false;
  for (size_t i = 0; i < colNames.size() ; ++i)
    {
      headersOK &= (splitLine[i] == colNames[i]);
      if (!headersOK) break;
    }
  return headersOK;
}


template<typename T>
void ItemInputState<T>::processItem(string& line)
 {
   ApplicationTextReader & reader =ApplicationTextReader::instance();
   string procline;
   try
   {
     procline = reader.getTextSubstitution()(line);
   }
   catch(runtime_error e)
   {
       string message = string("In file ")+this->getFilename()
                        +string(":\n\n")+string(e.what());
       throw runtime_error(message);
   }
   bool used = true;
   if(procline.substr(0,1) == "^")
     {
       used = false;
       procline = strip(procline.substr(1));
     }
   int layerNo = m_layerIndex;
   T obj;
   stringstream s(procline);
   try
     {
       s >> obj;
     }
   catch(...)
     { 
       stringstream errmsg;
       string message("Error reading object in line: \n");
       errmsg << message << procline 
           << "\n(file: " << this->getFilename() << ")" << endl;
       handleFatalError(errmsg.str());
     }
   obj.layer = layerNo;
   obj.used = used;
   vector<T> & buf =  HDFTableManager<T>::instance().buffer();
   buf.push_back(obj);
}

template<typename T>
void ItemInputState<T>::onFilenameSet()
{ 
  string layername = LayerManager::instance().generateLayerName(m_filename);
  m_layerIndex = LayerManager::instance().addLayer(layername);
}


