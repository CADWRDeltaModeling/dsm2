/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _TransportParticleInStage_h_
#define _TransportParticleInStage_h_

#include "Particle.h"
#include "Tide.h"
#include "Stage.h"
#include "PTM2Parameters.h"
#include "TransportBase.h"

namespace PTM2
{

class TransportParticleInStage : public TransportBase
{
  // Methods
public:
  TransportParticleInStage(const PTM2Parameters& parameters, Tide*& tide, Tide*& tideNext)
    : TransportBase(parameters, tide, tideNext) {}
  bool operator()(Particle& p, const Stage& wb);

private:
};

}

#endif /* __TransportParticleInStage_h__ */
