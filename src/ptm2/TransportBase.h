/** 
 * Author: Kijin nam, knam@water.ca.gov
 */

#ifndef _TransportBase_h_
#define _TransportBase_h_

#include "PTM2.h"
#include "Coord.h"
#include "Tide.h"
#include "PTM2Parameters.h"

namespace PTM2
{

class TransportBase
{
protected:
  const PTM2Parameters& parameters_;
  Tide*& tide_;
  Tide*& tideNext_;

public:
  /**
   * @brief Constructor
   */
  TransportBase(const PTM2Parameters& parameters, Tide*& tide, Tide*& tideNext) :
      parameters_(parameters), tide_(tide), tideNext_(tideNext)
  {
  }
  /**
   * @brief Empty virtual destructor for safety
   */
  virtual ~TransportBase()
  {
  }

protected:
  /**
   * @brief get flow value in a given waterboday at a given node at a given time
   * @param nodeId node id in the waterbody
   */
  void getFlow(Real& flow,
               const Real& currentTime,
               const Waterbody* waterbody,
               const int& nodeId = 0);
//  virtual Real getFlow(const Time& currentTime, const Waterbody* waterbody, const int& nodeId = 0);
  virtual Real getVelocity(const Real& currentTime, const Coord& coord);
//  template<typename typeT> typeT interpolateLinearly(const Time& currentTime,
//                                                     const Time& timePrev, const Time& timeNext,
//                                                     const typeT& valuePrev, const typeT& valueNext);
  template<typename typeT> void
  interpolateLinearly(typeT& re,
                      const Real& x,
                      const Real& xPrev,
                      const Real& xNext,
                      const typeT& valuePrev,
                      const typeT& valueNext);
};

}

#include "TransportBase.hpp"

#endif
