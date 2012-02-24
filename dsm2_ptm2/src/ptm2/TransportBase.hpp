/**
 *
 */

#ifndef _TransportBase_hpp_
#define _TransportBase_hpp_

namespace PTM2
{

//template<typename typeT> typeT
//  TransportBase::interpolateLinearly(const Time& currentTime, const Time& timePrev, const Time& timeNext,
//  const typeT& valuePrev, const typeT& valueNext)
//{
//  const Real theta = Real((currentTime - timePrev).ticks())
//    / Real((timeNext - timePrev).ticks());
//  assert(theta >= 0. && theta <= 1.);
//  return (1. - theta) * valuePrev + theta * valueNext;
//}

template<typename typeT> void
  TransportBase::interpolateLinearly(typeT& re, const Real& x, const Real& xPrev, const Real& xNext,
  const typeT& valuePrev, const typeT& valueNext)
{
  const Real theta = (x - xPrev) / (xNext - xPrev);
  if (theta < 0. || theta > 1.)
    std::cout << "Oops..\n";
  assert(theta >= 0. && theta <= 1.);
  re = (1. - theta) * valuePrev + theta * valueNext;
}

}

#endif
