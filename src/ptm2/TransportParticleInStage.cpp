/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#include "TransportParticleInStage.h"

namespace PTM2
{

bool
TransportParticleInStage::
operator()(Particle& particle, const Stage& stage)
{
  particle.setExiting();
  return true;
}

}
