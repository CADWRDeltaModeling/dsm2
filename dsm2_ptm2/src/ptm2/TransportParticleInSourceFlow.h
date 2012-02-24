/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _TransportParticleInSourceFlow_h_s
#define _TransportParticleInSourceFlow_h_

#include "Particle.h"
#include "Tide.h"
#include "SourceFlow.h"
#include "PTM2Parameters.h"
#include "TransportBase.h"

namespace PTM2
{

class TransportParticleInSourceFlow : public TransportBase
{
  // Methods
public:
  TransportParticleInSourceFlow(const PTM2Parameters& parameters, Tide*& tide, Tide*& tideNext)
    : TransportBase(parameters, tide, tideNext) {}
  bool operator()(Particle& p, const SourceFlow& wb);

private:
};

}

#endif /* __TransportParticleInSourceFlow_h__ */
