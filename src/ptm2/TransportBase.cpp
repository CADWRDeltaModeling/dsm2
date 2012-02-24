/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#include "TransportBase.h"
#include "Channel.h"
#include <typeinfo>

namespace PTM2
{

//Real TransportBase::getFlow(const Time& currentTime,
//                            const Waterbody* waterbody,
//                            const int& internalId)
//{
//  const Real flowPrev = tide_->getFlow(waterbody, internalId);
//  const Real flowNext = tideNext_->getFlow(waterbody, internalId);
//  const Time& timePrev = tide_->getTimeCursor();
//  const Time& timeNext = tideNext_->getTimeCursor();
//  return interpolateLinearly(currentTime, timePrev, timeNext, flowPrev, flowNext);
//}

void TransportBase::getFlow(Real& flow,
                            const Real& currentTime,
                            const Waterbody* waterbody,
                            const int& internalId)
{
  const Real flowPrev = tide_->getFlow(waterbody, internalId);
  const Real flowNext = tideNext_->getFlow(waterbody, internalId);
  const Real timePrev = tide_->getTimeCursor();
  const Real timeNext = tideNext_->getTimeCursor();
  this->interpolateLinearly(flow, currentTime, timePrev, timeNext, flowPrev, flowNext);
}

Real TransportBase::getVelocity(const Real& currentTime, const Coord& coord)
{
  return 0.;
}

}
