/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#include "PTM2GroupOutput.h"
#include "Waterbody.h"
#include <algorithm>
#include <iostream>

namespace PTM2
{

PTM2GroupOutput::PTM2GroupOutput()
{
  ;
}

PTM2GroupOutput::PTM2GroupOutput(const std::string& name,
      const std::vector<int>& waterbodyTypes,
      const std::vector<int>& waterbodies,
      const int& interval,
      const std::string& fname)
      : name_(name),
      waterbodyTypes_(waterbodyTypes),
      waterbodies_(waterbodies),
      interval_(interval),
      fname_(fname)
{
}

bool PTM2GroupOutput::inWaterbodies(const int& wb, const int& wbT,
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

int PTM2GroupOutput::match(const int& fromWb, const int& fromWbT,
                           const int& toWb, const int& toWbT) const
{
  int flux = 0;
  if (inWaterbodies(fromWb, fromWbT, waterbodies_, waterbodyTypes_))
    flux = -1;
  if (inWaterbodies(toWb, toWbT, waterbodies_, waterbodyTypes_))
    flux += 1;
  return flux;
}

}
