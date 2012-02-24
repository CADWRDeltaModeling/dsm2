/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#include "TransportParticleInSourceFlow.h"

namespace PTM2
{

bool
TransportParticleInSourceFlow::
operator()(Particle& particle, const SourceFlow& sourceFlow)
{
  particle.setExiting();
  return true;
}

}
