#include "ApplicationTextReader.h"
#include "FileInputState.h"
#include "EndOfFileState.h"
#include<iostream>
#include "boost/algorithm/string/case_conv.hpp"
#include<assert.h>

InputStatePtr FileInputState::process(istream& in)
{
    int nProcess=0;
    while(nProcess < 1000)
    {
        string line;
        getline(in,line);
        m_lineNo++;
        // strip comments, trailing/leading whitespace
        line = strip(line);
        if (in.eof())  //todo: last line in file?
        { 
            if (! line.size() == 0)
            {
                handleFatalError("File must end in a terminal carriage return: ",
                                 line, 
                                 m_filename,
                                 m_lineNo);
            }
            InputStatePtr next(new EndOfFileState(m_filename));
            return next;
        }
        if (line.size()==0)   //skip if empty
        { 
            continue;
        }
        // If we got here, the line is "substantial"
        // Find the correct next state and return it
        return nextState(line);
        nProcess++;
    }
    throw logic_error("End of file not found, processing in endless loop");  
    return InputStatePtr(new EndOfFileState(m_filename));
}


InputStatePtr FileInputState::nextState(const string &line) const
{
    assert(line.size() > 0);
    string item(line);
    boost::algorithm::to_upper(item);
    InputStateMap & stateMap = ApplicationTextReader::instance().getInputMap();
    bool isKey = ApplicationTextReader::instance().isKeyword(item);
	InputStateMap::const_iterator it = stateMap.find(item);
    if (it == stateMap.end())
    { 
        for (InputStateMap::const_iterator mapIter = stateMap.begin() ;
            mapIter != stateMap.end() ; ++ mapIter) 
        {
            cout << "Item: " << mapIter->first << endl;
        }
        handleFatalError(" Keyword not recognized:" + item,
                         line,m_filename,m_lineNo);
    }

    if (! isItemAllowed(item))
    {   
        string message("Keyword/table not allowed in context. It may be mispelled or be included in the wrong kind of include block:");
        handleFatalError(message + item,
                     line,
                     m_filename,
                     m_lineNo);
    }

    InputStatePtr next = it->second;
    next->setFilename(m_filename);
    next->setLineNo(m_lineNo);
    next->setActiveItems(m_activeItems);
    next->setActive( find(m_activeItems.begin(),
                          m_activeItems.end(),
                          item) != m_activeItems.end());
	
    next->setIncomingContextItems(m_contextItems);
    return next;
}

bool FileInputState::isItemAllowed(const string & item) const
{
	bool isInContext = find(m_contextItems.begin(),
			                m_contextItems.end(),
				            item) != m_contextItems.end();
	// Make sure nothing is allowed in context that isn't a keyword
	assert( isInContext ? ApplicationTextReader::instance().isKeyword(item) :
		                  true);
	return isInContext;
}



