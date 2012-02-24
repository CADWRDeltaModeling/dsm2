/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#include "TransportParticleInReservoir.h"
#include "RandomNumberGenerator.h"
#include "boost/foreach.hpp"

namespace PTM2
{

// Note: Reservoir might need a concept of subtime step
// to get a better time resolution.
bool TransportParticleInReservoir::operator()(Particle& particle,
                                              const Reservoir& reservoir)
{
  if (particle.getCoordinate().isReady())
    particle.getCoordinate().setReady(false);
  const int nWaterbodies = reservoir.getNumberOfConnectedWaterbodies();
  const Real& reservoirVolume = (tide_->getReservoirHeight(reservoir.getId())
      - reservoir.getBottomElevation()) * reservoir.getArea();
  assert(reservoirVolume >= 0.);
  Real& tCurrent = particle.getCurrentTime();

  Real* flows = new Real[nWaterbodies];
  const Real& timeStep = (parameters_.getTimeNextSecond() - tCurrent);
  for (int i = 0; i < nWaterbodies; ++i) {
    getFlow(flows[i], tCurrent, &reservoir, i);
    flows[i] *= -timeStep;
  }

  Real total_outflow = 0.;

  for (int i = 0; i < nWaterbodies; ++i) {
    const Real& flow = flows[i];
    if (flow > 0) {
      total_outflow += flow;
    }
  }assert(total_outflow >= 0.);

  RandomNumberGenerator& rng = RandomNumberGenerator::instance();
  const Real targetVol = rng() * reservoirVolume;
  Real sum_outflow = 0.;
  int hit = 0;
  for (int i = 0; i < nWaterbodies; ++i) {
    const Real& flow = flows[i];
    if (flow > 0) {
      sum_outflow += flow;
    }
    if (targetVol < sum_outflow) {
      break;
    }
    ++hit;
  }
  delete[] flows;

  const Real& tNext = parameters_.getTimeNextSecond();
  tCurrent = tNext;
  if (hit < nWaterbodies) {
    const Waterbody* newWaterbody = reservoir.getConnectedWaterbody(hit);
    particle.moveIn(newWaterbody,
                    reservoir.getInternalIdOfConnectedWaterbody(hit));
    if (typeid(*newWaterbody) != typeid(Node)) {
      const Waterbody* waterbodyFrom = &reservoir;
      const Waterbody* waterbodyTo = newWaterbody;
      // Write it into a trace file
      if (traceOutput_ == NULL)
        traceOutput_ = parameters_.getTraceWriterPointer();
      traceOutput_->writeRecord(particle, waterbodyFrom, waterbodyTo);
    }
    return false;
  }
  return true;
}

}
