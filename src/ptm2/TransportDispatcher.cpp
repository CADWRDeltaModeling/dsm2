/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#include "TransportDispatcher.h"
#include "TransportParticleInChannel.h"
//#include "TransportParticleInNode.h"
#include "TransportParticleInSourceFlow.h"
#include "TransportParticleInReservoir.h"
#include "TransportParticleInStage.h"
#include "TransportParticleInNodeWithObserver.h"

namespace PTM2
{

TransportDispatcher::TransportDispatcher(const PTM2Parameters& parameters,
                                         Tide*& tide,
                                         Tide*& tideNext)
: parameters_(parameters), tide_(tide), tideNext_(tideNext)
{
  // Turn off unsued dispatchers
  this->Add<Particle, const Channel> (TransportParticleInChannel(parameters_, tide_, tideNext_));
  this->Add<Particle, const Node> (TransportParticleInNodeWithObserver(parameters_, tide_, tideNext_));
  this->Add<Particle, const SourceFlow> (TransportParticleInSourceFlow(parameters_, tide_, tideNext_));
  this->Add<Particle, const Reservoir> (TransportParticleInReservoir(parameters_, tide_, tideNext_));
  this->Add<Particle, const Stage> (TransportParticleInStage(parameters_, tide_, tideNext_));
}

} // namespace PTM2
