/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef __TransportParticleInReservoirConnect_h__
#define __TransportParticleInReservoirConnect_h__

#include "Particle.h"
#include "Tide.h"
#include "ReservoirConnect.h"
#include "PTM2Parameters.h"
#include "TransportBase.h"

namespace PTM2
{

class TransportParticleInReservoirConnect : public TransportBase
{
  // Methods
public:
  TransportParticleInReservoirConnect(const PTM2Parameters& parameters, Tide*& tide, Tide*& tideNext)
    : TransportBase(parameters, tide, tideNext) {}
  bool operator()(Particle& p, const ReservoirConnect& wb);

private:
};

}

#endif /* __TransportParticleInReservoirConnect_h__ */
