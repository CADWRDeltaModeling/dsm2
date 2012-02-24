/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _ReservoirConnect_h_
#define _ReservoirConnect_h_

#include "Waterbody.h"

namespace PTM2
{

/**
 * @brief reservoirConnect waterbody
 **/
class ReservoirConnect : public Waterbody
{
protected:

public:
  ReservoirConnect(const int& id, const std::string& externalId)
    : Waterbody(id, externalId) {}

  virtual int getType() const { return RESERVOIRCONNECT; }
};

}

#endif