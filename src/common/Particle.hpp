/**
 * Particle.hpp
 *
 * Implementation of Particle class
 * Created on: Jun 16, 2009
 * Author: Kijin Nam, knam@water.ca.gov
 */

#include "Particle.h"

namespace PTM2
{

template <typename CoordsT>
void
Particle<CoordsT>::printInfo(std::ostream& os)
{
	os << id() << ": " << coord_ << '\n';
}


}
