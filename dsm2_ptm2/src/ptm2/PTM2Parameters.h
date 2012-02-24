/**
 * Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _PTM2Parameters_h_
#define _PTM2Parameters_h_

#include "Parameters.h"
#include "TraceOutput.h"

namespace PTM2
{

/**
 * @brief Expanded parameter class which include PTM2 internal information
 */
class PTM2Parameters: public Parameters
{
protected:
  int traceWriterPointer_;
  TraceOutput* traceOutputPtr_;

public:
  void setTraceWriterPointer(TraceOutput* p)
  { traceOutputPtr_ = p; }
  TraceOutput* getTraceWriterPointer() const
  { return traceOutputPtr_; }
};

}

#endif
