/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _Transport_h_
#define _Transport_h_

#include "TransportDispatcher.h"
#include "Particle.h"
#include "PTM2Parameters.h"
#include "Tide.h"

namespace PTM2
{


/**
 * @brief Transport functor
 */
class Transport
{
	// Properties
private:
  const PTM2Parameters& parameters_;
  Tide*& tide_;
  Tide*& tideNext_;
	TransportDispatcher dispatcher_;

	// Methods
public:
	Transport(const PTM2Parameters& parameters, Tide*& tide, Tide*& tideNext)
    : parameters_(parameters), tide_(tide), tideNext_(tideNext), dispatcher_(parameters_, tide_, tideNext_) {}
	bool operator()(Particle& p);
};

}


#endif /* _Transport_h_ */
