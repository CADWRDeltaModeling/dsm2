/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _Transfer_h_
#define _Transfer_h_

#include "Waterbody.h"

namespace PTM2
{

/**
 * @brief Transfer waterbody
 **/
class Transfer : public Waterbody
{
protected:
  std::string name_;
  int fromWaterbodyId_;
  int toWaterbodyId_;

public:
  Transfer(const int& id, const std::string& name, const int& fromWaterbodyId,
    const int& toWaterbodyId)
    : Waterbody(id, 0), name_(name), fromWaterbodyId_(fromWaterbodyId), toWaterbodyId_(toWaterbodyId) {}
  int getType() const { return TRANSFER; }
};

}

#endif