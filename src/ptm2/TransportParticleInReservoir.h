/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _TransportParticleInReservoir_h_
#define _TransportParticleInReservoir_h_

#include "Particle.h"
#include "Tide.h"
#include "Reservoir.h"
#include "PTM2Parameters.h"
#include "TransportBase.h"
#include "TraceOutput.h"


namespace PTM2
{

class TransportParticleInReservoir : public TransportBase
{
protected:
  TraceOutput* traceOutput_;

  // Methods
public:
  TransportParticleInReservoir(const PTM2Parameters& parameters, Tide*& tide, Tide*& tideNext)
    : TransportBase(parameters, tide, tideNext), traceOutput_(NULL) {}
  bool operator()(Particle& p, const Reservoir& reservoir);
};

}

#endif /* __TransportParticleInReservoir_h__ */
