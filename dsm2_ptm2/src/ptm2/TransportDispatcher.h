/**
 * Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _TransportDispatcher_h_
#define _TransportDispatcher_h_

#include "loki/MultiMethods.h"
#include "Tide.h"
#include "Particle.h"
#include "Waterbody.h"
#include "PTM2Parameters.h"

namespace PTM2
{

typedef Loki::FunctorDispatcher <
								Particle,
								const Waterbody,
								bool,
								Loki::StaticCaster
								>
		BaseTransportDispatcher;


/**
 * @brief dispatcher class
 */
class TransportDispatcher : public BaseTransportDispatcher
{
private:
  const PTM2Parameters& parameters_;
  Tide*& tide_;
  Tide*& tideNext_;

    // Methods
public:
	/**
	 * @brief Default constructor
	 *
	 * This constructor links dispatchers.
	 */
	TransportDispatcher(const PTM2Parameters& parameter, Tide*& tide, Tide*& tideNext);
};


}

#endif /* __TransportDispatcher_h__ */
