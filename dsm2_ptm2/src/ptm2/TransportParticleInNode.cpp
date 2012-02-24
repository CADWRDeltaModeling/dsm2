/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#include "TransportParticleInNode.h"
#include "RandomNumberGenerator.h"
#include "boost/foreach.hpp"

namespace PTM2
{

bool TransportParticleInNode::operator()(Particle& particle, const Node& node)
{
  const int nWaterbodies = node.getNumberOfConnectedWaterbodies();
  // Stage handling
  for (int i = 0; i < nWaterbodies; ++i) {
    const Waterbody* wb = node.getConnectedWaterbody(i);
    assert(wb != NULL);
    if (typeid(*wb) == typeid(const Stage)) {
      particle.moveIn(wb, node.getInternalIdOfConnectedWaterbody(i));
      return false;
    }
  }
  // If there is only one waterbody associated the node
  if (nWaterbodies < 2) {
    particle.moveIn(node.getConnectedWaterbody(0),
                    node.getInternalIdOfConnectedWaterbody(0));
    return false;
  }
  const Real& tCurrent = particle.getCurrentTime();
  Real* flows = new Real[nWaterbodies];
  for (int i = 0; i < nWaterbodies; ++i) {
    getFlow(flows[i], tCurrent, node.getConnectedWaterbody(i),
            node.getInternalIdOfConnectedWaterbody(i));
  }

  Real total_outflow = 0.;
  for (int i = 0; i < nWaterbodies; ++i) {
    const Real& flow = flows[i];
    if (flow > 0) {
      total_outflow += flow;
    }
  }assert(total_outflow >= 0.);

  RandomNumberGenerator& rng = RandomNumberGenerator::instance();
  const Real targetFlow = rng() * total_outflow;
  Real sum_outflow = 0.;
  int hit = 0;
  for (int i = 0; i < nWaterbodies; ++i) {
    const Real& flow = flows[i];
    if (flow > 0) {
      sum_outflow += flow;
    }
    if (targetFlow < sum_outflow) {
      break;
    }
    ++hit;
  }
  delete[] flows;

  const Waterbody* newWaterbody = node.getConnectedWaterbody(hit);
  assert(newWaterbody != NULL);
  particle.moveIn(newWaterbody, node.getInternalIdOfConnectedWaterbody(hit));

  return false; // Always return false
}

}
