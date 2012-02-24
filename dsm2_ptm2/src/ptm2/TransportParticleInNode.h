/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _TransportParticleInNode_h_
#define _TransportParticleInNode_h_

#include "Particle.h"
#include "Tide.h"
#include "Node.h"
#include "PTM2Parameters.h"
#include "TransportBase.h"

namespace PTM2
{

class TransportParticleInNode : public TransportBase
{
  // Methods
public:
  TransportParticleInNode(const PTM2Parameters& parameters, Tide*& tide, Tide*& tideNext)
    : TransportBase(parameters, tide, tideNext) {}
  virtual bool operator()(Particle& p, const Node& node);
};

}

#endif /* __TransportParticleInNode_h__ */
