/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#include "TransportParticleInNodeWithObserver.h"

namespace PTM2 {

TransportParticleInNodeWithObserver::TransportParticleInNodeWithObserver(
  const PTM2Parameters& parameters,
  Tide*& tide, Tide*& tideNext)
  : TransportParticleInNode(parameters, tide, tideNext), traceOutput_(NULL)
{
}

bool TransportParticleInNodeWithObserver::operator()(Particle& p,
                                                     const Node& node)
{
  const Waterbody* waterbodyFrom = p.getPreviousWaterbody();
  if (waterbodyFrom == NULL) {
  	waterbodyFrom = p.getCoordinate().getWaterbody();
  }
  const bool result = TransportParticleInNode::operator ()(p, node);
  const Waterbody* waterbodyTo = p.getCoordinate().getWaterbody();
  // Write it into a trace file
  if (traceOutput_ == NULL) traceOutput_ = parameters_.getTraceWriterPointer();
  traceOutput_->writeRecord(p, waterbodyFrom, waterbodyTo);
  return result;
}

}
