/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#include "Particle.h"
#include "Node.h"
#include "Channel.h"
#include <typeinfo>

namespace PTM2
{

int Particle::count_ = 0;

void Particle::moveIn(const Waterbody* waterbody, const int internalId)
{
  prevWb_ = coord_.getWaterbody();
  coord_.setWaterbody(waterbody);
  
  assert(waterbody != NULL);
  if (typeid(*waterbody) == typeid(const Channel)) {
    if (internalId == 0) {
      coord_(0) = 0.;
    }
    else {
      coord_(0) = dynamic_cast<const Channel*>(waterbody)->getLength();
      // todo: Correct the position
    }
  }
  this->setNewInWaterbody();
  return;
}

}
