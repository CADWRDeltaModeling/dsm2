/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _TideProvider_h_
#define _TideProvider_h_

#include "PTM2.h"
#include "Tide.h"

namespace PTM2
{

/**
 * @brief Tide information provider (Abstract)
 *
 * Time unit here is julian minute
 **/
class TideProvider
{
public:
  // Empty virtual destructor for safety
  virtual ~TideProvider() {}
  virtual void updateTideAtOrBefore(const int& julminToRead,
    Tide* tide) = 0;
  virtual void updateTideAfter(const int& julminToRead,
    Tide* tide) = 0;
};

}

#endif
