/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _Tide_h_
#define _Tide_h_

#include "PTM2.h"
#include "Grid.h"
#include "hdf5.h"
#include <vector>
#include <string>
#include <vector>

namespace PTM2
{

/**
 * @brief A class to hold tide (hydrodynamics information)
 */
class Tide
{
  // Properties
protected:
  const Grid& grid_;

private:
  int nChannels_;
  int nSourceFlows_;
  int nTransfers_;
  int nReservoirs_;
  int nReservoirConnects_;
  bool allocated_;

  int julminTimeCursor_;
  Real tCursor_;

  // Raw data
  double* channelFlows_;
  double* channelStages_;
  double* channelAreas_;
  double* channelAverageAreas_;
  double* reservoirStages_;
  double* reservoirFlows_;
  double* reservoirHeights_;
  double* sourceFlows_;
  double* transferFlows_;

  // Methods
public:
  Tide(const Grid& grid);
  ~Tide();
  void setTimeCursor(const int& julmin)
  {
    julminTimeCursor_ = julmin;
    tCursor_ = Real(julminTimeCursor_) * 60.;
  }
  const int& getTimeCursorJulian() const
  {
    return julminTimeCursor_;
  }
  ;
  const Real& getTimeCursor() const
  {
    return tCursor_;
  }
  void getTimeCursor(Real& julsec) const
  {
    julsec = tCursor_;
  }
  const Grid& getGrid() const
  {
    return grid_;
  }
  bool isAllocated() const
  {
    return allocated_;
  }
  void allocateMemory();
  const int getNumberOfChannels() const
  {
    return nChannels_;
  }
  const int getNumberOfReservoirs() const
  {
    return nReservoirs_;
  }
  const int getNumberOfSourceFlows() const
  {
    return nSourceFlows_;
  }
  const int getNumberOfReservoirConnects() const
  {
    return nReservoirConnects_;
  }
  double* getChannelFlows()
  {
    return channelFlows_;
  }
  double* getChannelStages()
  {
    return channelStages_;
  }
  double* getChannelAreas()
  {
    return channelAreas_;
  }
  double* getChannelAverageAreas()
  {
    return channelAverageAreas_;
  }
  double* getReservoirFlows()
  {
    return reservoirFlows_;
  }
  double* getSourceFlows()
  {
    return sourceFlows_;
  }
  double* getTransferFlows()
  {
    return transferFlows_;
  }
  double* getReservoirHeights()
  {
    return reservoirHeights_;
  }
  Real getFlow(const Waterbody* waterbody, const int& internalId) const;
  const Real& getDepth(const Waterbody* waterbody, const int& internalId) const;
  const Real getWidth(const Waterbody* waterbody, const int& internalId) const;

  Real getChannelArea(const Channel* channelPtr, const int& internalId) const;
  Real getChannelFlow(const Channel* channelPtr, const int& internalId) const;
  const Real& getChannelDepth(const Channel* channel,
                              const int& internalId) const;
  const Real getChannelWidth(const Channel* channel,
                             const int& internalId) const;

  Real getReservoirHeight(const int& id) const;
  Real getReservoirFlow(const Reservoir* reservoirPtr,
                              const int& internalId) const;

private:
  void clearMemory();
};

}

#endif // __Tide_h__
