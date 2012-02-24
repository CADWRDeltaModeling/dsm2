/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#include "Transport.h"

namespace PTM2
{

bool
Transport::operator()(Particle& particle)
{
  bool done = false;
  int loopCount = 0;
  //std::cout << "particle: " << particle.getId() << "\n";
  while ( !done ) {
    if (loopCount > 100) {
      throw std::runtime_error("Transport: Too many loop iteration.");
    }
    Coord& coord = particle.getCoordinate();
    const Waterbody* waterbody = coord.getWaterbody();
    assert(waterbody != NULL);
    done = dispatcher_.Go(particle, *waterbody);
    ++loopCount;
  }
	return true;
}

}
