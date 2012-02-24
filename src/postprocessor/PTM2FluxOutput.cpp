/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#include "PTM2FluxOutput.h"
#include <algorithm>
#include <iostream>

namespace PTM2
{

PTM2FluxOutput::PTM2FluxOutput()
{
  ;
}

PTM2FluxOutput::PTM2FluxOutput(const std::string& name,
      const std::vector<int>& fromWaterbodyTypes,
      const std::vector<int>& fromWaterbodies,
      const std::vector<int>& toWaterbodyTypes,
      const std::vector<int>& toWaterbodies,
      const int& interval,
      const std::string& fname)
      : name_(name), 
      fromWaterbodyTypes_(fromWaterbodyTypes),
      fromWaterbodies_(fromWaterbodies),
      toWaterbodyTypes_(toWaterbodyTypes),
      toWaterbodies_(toWaterbodies),
      interval_(interval),
      fname_(fname)
{
}

bool PTM2FluxOutput::inWaterbodies(const int& wb, const int& wbT,
                                   const std::vector<int>& waterbodies,
                                   const std::vector<int>& waterbodiesT) const
{
  std::vector<int>::const_iterator it = waterbodies.begin();
  if (*it == -1) {
      if (wbT == Waterbody::CHANNEL && wbT == Waterbody::RESERVOIR) {
        return true;
      }
      else {
        return false;
      }
    }
  do {
    it = std::find(it, waterbodies.end(), wb);
    if (it != waterbodies.end()) {
      const int dist = distance(waterbodies.begin(), it);
      if (waterbodiesT[dist] == wbT)
        return true;
      ++it;
    }
  } while (it != waterbodies.end());
  return false;
}

int PTM2FluxOutput::match(const int& fromWb, const int& fromWbT,
                           const int& toWb, const int& toWbT) const
{
  int flux = 0;
  if (inWaterbodies(fromWb, fromWbT, fromWaterbodies_, fromWaterbodyTypes_)
    && inWaterbodies(toWb, toWbT, toWaterbodies_, toWaterbodyTypes_))
    flux = 1;
  else if (inWaterbodies(fromWb, fromWbT, toWaterbodies_, toWaterbodyTypes_)
    && inWaterbodies(toWb, toWbT, fromWaterbodies_, fromWaterbodyTypes_))
    flux = -1;
  return flux;
}

}
