/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#include <sstream>
#include "boost/foreach.hpp"
#include "boost/filesystem/path.hpp"
#include "boost/filesystem/operations.hpp"
#include "Tide.h"

namespace PTM2
{

// @todo: Upgrade the exception message
#define CHECK_ERR(a, b) if ((a) < 0) { throw std::runtime_error(b); }

Tide::Tide(const Grid& grid) :
    grid_(grid), allocated_(false)
{
  channelFlows_ = NULL;
  channelStages_ = NULL;
  channelAreas_ = NULL;
  channelAverageAreas_ = NULL;
  reservoirStages_ = NULL;
  reservoirFlows_ = NULL;
  reservoirHeights_ = NULL;
  sourceFlows_ = NULL;
  transferFlows_ = NULL;
}

Tide::~Tide()
{
  clearMemory();
}

void Tide::clearMemory()
{
  // Clear up the memory
  if (channelFlows_ != NULL) {
    delete[] channelFlows_;
  }
  if (channelStages_ != NULL) {
    delete[] channelStages_;
  }
  if (channelAreas_ != NULL) {
    delete[] channelAreas_;
  }
  if (channelAverageAreas_ != NULL) {
    delete[] channelAverageAreas_;
  }
  if (reservoirStages_ != NULL) {
    delete[] reservoirStages_;
  }
  if (reservoirFlows_ != NULL) {
    delete[] reservoirFlows_;
  }
  if (reservoirHeights_ != NULL) {
    delete[] reservoirHeights_;
  }
  if (sourceFlows_ != NULL) {
    delete[] sourceFlows_;
  }
  if (transferFlows_ != NULL) {
    delete[] transferFlows_;
  }
  allocated_ = false;
}

void Tide::allocateMemory()
{
  nChannels_ = grid_.getNumberOfChannels();
  nReservoirs_ = grid_.getNumberOfReservoirs();
  nSourceFlows_ = grid_.getNumberOfSourceFlows();
  nTransfers_ = grid_.getNumberOfTransfers();
  nReservoirConnects_ = grid_.getNumberOfReservoirConnects();
  if (nChannels_ > 0) {
    channelFlows_ = new double[nChannels_ * 2];
    channelStages_ = new double[nChannels_ * 2];
    channelAreas_ = new double[nChannels_ * 2];
    channelAverageAreas_ = new double[nChannels_];
  }
  if (nReservoirs_ > 0) {
    reservoirHeights_ = new double[nReservoirs_];
  }
  if (nReservoirConnects_ > 0) {
    reservoirFlows_ = new double[nReservoirConnects_];
  }
  if (nSourceFlows_ > 0) {
    sourceFlows_ = new double[nSourceFlows_];
  }
}

Real Tide::getFlow(const Waterbody* waterbody, const int& internalId) const
{
  assert(waterbody != NULL);
  if (typeid(*waterbody) == typeid(const Channel)) {
    const Channel* channel = static_cast<const Channel*>(waterbody);
    const int& id = channel->getId();
    assert(id < this->getNumberOfChannels());
    assert(internalId < 2);
    const Real& flow = channelFlows_[id * 2 + internalId];
    return internalId == 0 ? flow : -flow;
  }
  else if (typeid(*waterbody) == typeid(const SourceFlow)) {
    const SourceFlow* sourceFlow = static_cast<const SourceFlow*>(waterbody);
    const int& id = sourceFlow->getId();
    assert(id < this->getNumberOfSourceFlows());
    return -sourceFlows_[id];
  }
  else if (typeid(*waterbody) == typeid(const Reservoir)) {
    const Reservoir* reservoir = static_cast<const Reservoir*>(waterbody);
    const Waterbody* connectedBody = reservoir->getConnectedWaterbody(
        internalId);
    if (typeid(*connectedBody) == typeid(const Node)) {
      return -reservoirFlows_[reservoir->getInternalIdOfConnectedWaterbody(
          internalId)];
    }
    if (typeid(*connectedBody) == typeid(const SourceFlow)) {
      return -this->getFlow(connectedBody, 0);
    }
    throw std::runtime_error(
        "Tide::getFlow: Not supported waterbody connection to a reservoir.");
  }
  throw std::runtime_error("Tide::getFlow: Not supported waterbody.");
}

Real Tide::getChannelFlow(const Channel* channelPtr,
                          const int& internalId) const
{
  assert(channelPtr != NULL);
  const int& id = channelPtr->getId();
  assert(id < nChannels_);
  assert(internalId < 2);
  const Real& flow = channelFlows_[id * 2 + internalId];
  return internalId == 0 ? flow : -flow;
}

Real Tide::getReservoirFlow(const Reservoir* reservoirPtr,
                            const int& internalId) const
{
  assert(reservoirPtr != NULL);
  const Waterbody* connectedBody = reservoirPtr->getConnectedWaterbody(internalId);
  if (typeid(*connectedBody) == typeid(const Node)) {
    return -reservoirFlows_[reservoirPtr->getInternalIdOfConnectedWaterbody(
        internalId)];
  }
  if (typeid(*connectedBody) == typeid(const SourceFlow)) {
    return -this->getFlow(connectedBody, 0);
  }
  throw std::runtime_error(
      "Tide::getFlow: Not supported waterbody connection to a reservoir.");
}

Real Tide::getChannelArea(const Channel* channel, const int& internalId) const
{
  const int& id = channel->getId();
  assert(id < this->getNumberOfChannels());
  assert(internalId < 2);
  return channelAreas_[id * 2 + internalId];
}

Real Tide::getReservoirHeight(const int& id) const
{
  assert(id < this->getNumberOfReservoirs());
  return reservoirHeights_[id];
}

const Real& Tide::getDepth(const Waterbody* waterbody,
                           const int& internalId) const
{
  if (waterbody == NULL) {
    assert(waterbody != NULL);
  }
  if (typeid(*waterbody) == typeid(const Channel)) {
    const Channel* channel = static_cast<const Channel*>(waterbody);
    const int& id = channel->getId();
    assert(id < nChannels_);
    assert(internalId < 2);
    return channelStages_[id * 2 + internalId];
  }
  else {
    // @todo:
  }
  throw std::runtime_error("Tide::getDepths: Not supported waterbody.");
}

const Real& Tide::getChannelDepth(const Channel* channel,
                                  const int& internalId) const
{
  assert(channel != NULL);
  const int& id = channel->getId();
  assert(id < nChannels_);
  assert(internalId < 2);
  return channelStages_[id * 2 + internalId];
}

const Real Tide::getWidth(const Waterbody* waterbody,
                          const int& internalId) const
{
  assert(waterbody != NULL);
  if (typeid(*waterbody) == typeid(const Channel)) {
    const Channel* channel = static_cast<const Channel*>(waterbody);
    const int& id = channel->getId();
    assert(id < nChannels_);
    assert(internalId < 2);
    return channelAreas_[id * 2 + internalId]
        / channelStages_[id * 2 + internalId];
  }
  else {
    // @todo:
  }
  throw std::runtime_error("Tide::getDepths: Not supported waterbody.");
}

const Real Tide::getChannelWidth(const Channel* channel,
                                 const int& internalId) const
{
  assert(channel != NULL);
  const int& id = channel->getId();
  assert(id < nChannels_);
  assert(internalId < 2);
  return channelAreas_[id * 2 + internalId]
      / channelStages_[id * 2 + internalId];
}

} // namespace
