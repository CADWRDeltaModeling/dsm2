#include "InsertFileState.h"
#include "FileInputState.h"
#include<boost/filesystem/operations.hpp>

#include<iostream>
#define TOO_MANY_TRANSITIONS 1000

InputStatePtr InsertFileState::process(istream& in)
{
    while(true)
    {
        string line;
        getline(in,line);
        m_lineNo++;
        line = strip(line);      // strip comments, trailing/leading whitespace
        line = substitute(line);  // text/environmental variable substitution
        if (in.eof() && !isBlockEnd(line)){
            handleFatalError("Unexpected end of file in file",line,m_filename,m_lineNo);
        }
        if (line.size()==0) continue;
        if ( isBlockEnd(line))
        {
            InputStatePtr newState(new FileInputState(m_contextItems,m_filename,m_lineNo));
            newState->setActiveItems(m_activeItems);
            return newState;
        }

        if( isActive())
        {
            string filename(line);
            //todo: handle file does not exist, still check even if not active?
            boost::filesystem::path p(filename);
            if (!boost::filesystem::exists(p))
            {
                //todo
                handleFatalError( "File does not exist: " + filename,line,m_filename,m_lineNo);
            }

            InputStatePtr newState ( new FileInputState(m_contextItems,filename,0));
            newState->setActiveItems(m_activeItems);
            ifstream newStream(filename.c_str());
            int nTransition = 0;
            while (! newState->isEndOfFile())
            {
                newState = newState->process(newStream);
                nTransition++;
                if (nTransition > TOO_MANY_TRANSITIONS)
                {
                    throw logic_error(
                    "Too many state transitions, something is wrong in input processor");
                }
            }
        }


    }
}



