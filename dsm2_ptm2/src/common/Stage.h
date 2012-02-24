/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _Stage_h_
#define _Stage_h_

#include "Waterbody.h"

namespace PTM2
{

/**
 * @brief Stage waterbody
 **/
class Stage : public Waterbody
{
protected:

public:
  Stage(const int& id, const std::string& externalId)
    : Waterbody(id, externalId)
  {}
  virtual int getType() const { return STAGE; }
};

}

#endif