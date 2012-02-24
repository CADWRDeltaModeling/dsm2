/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _TransportParticleInNodeWithObserver_h_
#define _TransportParticleInNodeWithObserver_h_

#include "TransportParticleInNode.h"
#include "TraceOutput.h"

namespace PTM2
{

class TransportParticleInNodeWithObserver : public TransportParticleInNode
{
  // Properties
protected:
  TraceOutput* traceOutput_;

  // Methods
public:
  TransportParticleInNodeWithObserver(const PTM2Parameters& parameters,
    Tide*& tide, Tide*& tideNext);
  bool operator()(Particle& p, const Node& node);
};

}

#endif /* _TransportParticleInNodeWithObserver_h_ */
