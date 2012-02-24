/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#include "RandomNumberGenerator.h"
#include "TransportParticleInChannel.h"
#include <cmath>
#include <algorithm>

namespace PTM2
{

// Note:
// tXXX : time in real seconds
// timeXXXX : time in Boost time tick

TransportParticleInChannel::TransportParticleInChannel(const PTM2Parameters& parameters,
                                                       Tide*& tide,
                                                       Tide*& tideNext) :
    TransportBase(parameters, tide, tideNext)
{
  precalculateVerticalProfile();
}

void TransportParticleInChannel::precalculateVerticalProfile()
{
  // Note: Problem here is... kappa is not in control.
  const Real kappa = 0.4;
  for (int i = 0; i < NUM_VERTICAL_LAYERS; ++i) {
    const Real zNorm = Real(i) / Real(NUM_VERTICAL_LAYERS);
    Real factor = 1.0 + 0.1 / kappa * (1.0 + log(zNorm));
    if (factor < 0.001) {
      factor = 0.0001;
    }
    verticalProfile_[i] = factor;
  }
}

bool TransportParticleInChannel::operator()(Particle& particle,
                                            const Channel& channel)
{
  try {
    ////std::cout << "Advection in a channel..." << std::endl;
    channelPtr_ = &channel;
    length_ = channel.getLength();
    Real& tCurrent = particle.getCurrentTime();
    const Real& tNext = parameters_.getTimeNextSecond();
    const Real tLeft = tNext - tCurrent;

    Coord& coord = particle.getCoordinate();
    if (!coord.isReady()) {
      this->setRandomYZ(particle, channel, tCurrent);
    }

    if (particle.isNewInWaterbody()) {
      particle.setNewInWaterbody(false);
    }

    Real tSubTimeStep = 0.;
    int nSubTimeSteps = 0;
    this->getSubTimeStep(tSubTimeStep, nSubTimeSteps, particle, tLeft, channel);

    bool hitEnd = false;
    for (int step = 0; step < nSubTimeSteps; ++step) {
      // Euler forwards
      const Real tNextSub = tCurrent + tSubTimeStep;
      // Collect key values at the current location
      // because they will be used repeatedly
//      this->collectValues(particle, channel, tCurrent);
      this->collectValuesFast(particle, channel);
      this->adjustYZ(particle, channel);

      // So there is a choice in the velocity
      // Either very detail velocity at each sub-time step
      // Or velocity at the beginning of the time step.
      const Real velocity = getVelocity(coord); // Simple version
      //    const Real velocity = getVelocity(currentTime, coord);
      Real tSpent = tSubTimeStep;
      // Check if the velocity is zero
      if (velocity == 0.) {
        tCurrent = tNextSub;
      }
      else {
        const Real xNew = coord(0) + velocity * tSubTimeStep;
        if (xNew < 0.) {
          const Real tTraveled = -(coord(0) / velocity);
          if (tTraveled <= 0.) {
            tCurrent = tNextSub;
          }
          else {
            tSpent = tTraveled;
            tCurrent += tTraveled;
            particle.moveIn(channel.getConnectedWaterbody(0));
            hitEnd = true;
          }
        }
        else if (xNew > channel.getLength()) {
          const Real tTraveled = (channel.getLength() - coord(0)) / velocity;
          if (tTraveled <= 0.) {
            tCurrent = tNextSub;
          }
          else {
            tSpent = tTraveled;
            tCurrent += tTraveled;
            particle.moveIn(channel.getConnectedWaterbody(1));
            hitEnd = true;
          }
        }
        else {
          coord(0) = xNew;
          tCurrent = tNextSub;
        }
      }
      particle.setOldDepth(depth_);
      particle.setOldWidth(width_);
      this->addDispersion(particle, tSpent);
      if (hitEnd)
        break;
    }
    if (!hitEnd) {
      tCurrent = tNext;
    }
    return !hitEnd;
  } // End of try
  catch (const std::exception &e) {
    std::cerr << "Exception: " << e.what() << "\n";
    std::cerr << "Time: " << particle.getCurrentTime() << "\n";
    throw e;
  }
}

// @todo: more coding to switch on or off certain dispersion
void TransportParticleInChannel::getSubTimeStep(Real& tSubTimeStep,
                                                int& nSubTimeSteps,
                                                Particle& particle,
                                                const Real& tLeft,
                                                const Channel& channel)
{
  assert(tLeft > 0.);
  Real& tCurrent = particle.getCurrentTime();
  this->collectValuesFast(particle, channel);
  // The sub-time is decided based on the width and depth.
  const Real dzMax = 0.1 * depth_;
  // Note: I do not know why we take square of dzMax
  const Real dtz = dzMax * dzMax / localVertDispCoeff_;
  const Real dyMax = 0.1 * width_;
  const Real dty = dyMax * dyMax / localHoriDispCoeff_;
  tSubTimeStep = std::min(dtz, dty);
  nSubTimeSteps = int(tLeft / tSubTimeStep);
  if (nSubTimeSteps == 0)
    nSubTimeSteps = 1;
  tSubTimeStep = tLeft / nSubTimeSteps;
  assert(tSubTimeStep > 0.);
}

void TransportParticleInChannel::addDispersion(Particle& particle,
                                               const Real& tSpent)
{
  NormalRandomNumberGenerator& rng = NormalRandomNumberGenerator::instance();
  Coord& coord = particle.getCoordinate();
  Real& y = coord(1);
  Real& z = coord(2);
  // Horizontal
  const Real dispHorizontal = sqrt(2.0 * localHoriDispCoeff_ * tSpent) * rng();
  // Vertical
  const Real dispVertical = sqrt(2.0 * localVertDispCoeff_ * tSpent) * rng();
  // Add Horizontal dispersion
  y += dispHorizontal;
  // Add Vertical dispersion
  z += dispVertical;
  this->bounce(y, z);
}

void TransportParticleInChannel::collectValues(const Particle& particle,
                                               const Channel& channel,
                                               const Real& tCurrent)
{
  const Coord& coord = particle.getCoordinate();
  width_ = this->getWidth(tCurrent, coord);
  depth_ = this->getDepth(tCurrent, coord);
  area_ = width_ * depth_;
  assert(area_ > 0.);
  averageVel_ = this->getAverageVelocity(coord);

  const Real vertDispCoeff = 0.0067;
  localVertDispCoeff_ = fabs(vertDispCoeff * depth_ * averageVel_);
  const Real horiDispCoeff = 0.6;
  localHoriDispCoeff_ = fabs(horiDispCoeff * depth_ * averageVel_);
}

void TransportParticleInChannel::collectValuesFast(const Particle& particle,
                                                   const Channel& channel)
{
  const Coord& coord = particle.getCoordinate();
  width_ = this->getWidthFast(coord);
  depth_ = this->getDepthFast(coord);
  area_ = width_ * depth_;
  assert(area_ > 0.);
  averageVel_ = this->getAverageVelocity(coord);

  const Real vertDispCoeff = 0.0067;
  localVertDispCoeff_ = fabs(vertDispCoeff * depth_ * averageVel_);
  const Real horiDispCoeff = 0.6;
  localHoriDispCoeff_ = fabs(horiDispCoeff * depth_ * averageVel_);
}

Real TransportParticleInChannel::getVelocity(const Real& tCurrent,
                                             const Coord& coord)
{
  const Real& timePrev = tide_->getTimeCursor();
  const Real& timeNext = tideNext_->getTimeCursor();
  const Real velUpPrev = tide_->getChannelFlow(channelPtr_, 0)
      / tide_->getChannelArea(channelPtr_, 0);
  const Real velUpNext = tideNext_->getChannelFlow(channelPtr_, 0)
      / tide_->getChannelArea(channelPtr_, 0);
  const Real velDownPrev = -tide_->getChannelFlow(channelPtr_, 1)
      / tide_->getChannelArea(channelPtr_, 1);
  const Real velDownNext = -tideNext_->getChannelFlow(channelPtr_, 1)
      / tide_->getChannelArea(channelPtr_, 1);
  Real velUp;
  this->interpolateLinearly(velUp, tCurrent, timePrev, timeNext, velUpPrev,
                            velUpNext);
  Real velDown;
  this->interpolateLinearly(velUp, tCurrent, timePrev, timeNext, velDownPrev,
                            velDownNext);
  const Real& x = coord(0);
  Real velOneDim;
  this->interpolateLinearly(velOneDim, x, 0., length_, velUp, velDown);
  const Real factor = this->getProfile(coord);
  return factor * velOneDim;
}

Real TransportParticleInChannel::getVelocity(const Coord& coord)
{
  const Real factor = this->getProfile(coord);
  return factor * averageVel_;
}

// This is spatial interpolation without temporal one.
Real TransportParticleInChannel::getAverageVelocity(const Coord& coord)
{
  const Real& flowUp = tide_->getChannelFlow(channelPtr_, 0);
  const Real& flowDown = -tide_->getChannelFlow(channelPtr_, 1);
  const Real& x = coord(0);
  Real flow;
  this->interpolateLinearly(flow, x, 0., length_, flowUp, flowDown);
  return flow / area_;
}

Real TransportParticleInChannel::getAverageVelocityNext(const Coord& coord)
{
  const Real& flowUp = tideNext_->getChannelFlow(channelPtr_, 0);
  const Real& flowDown = -tideNext_->getChannelFlow(channelPtr_, 1);
  const Real& x = coord(0);
  Real flow;
  this->interpolateLinearly(flow, x, 0., length_, flowUp, flowDown);
  return flow / area_;
}

Real TransportParticleInChannel::getProfile(const Coord& coord) const
{
  const Real x = coord(0);
  const Real zNorm = coord(2) / depth_;
  if (zNorm < 0. || zNorm > 1.) {
    std::cerr << "z: " << coord(2) << ", depth: " << depth_ << "\n";
    std::runtime_error(
        "TransportParticleInChannel::getProfile: z in out of bound.");
  }
  //std::cout << "zNorm: " << zNorm << "\n";
  const Real yNorm = 2.0 * coord(1) / width_;
//  std::cout << "y: " << coord(1) << ", " << width_ << "\n";
  if (yNorm < -1.0 || yNorm > 1.0) {
    std::cerr << "y: " << coord(1) << ", " << width_ << "\n";
    std::runtime_error(
        "TransportParticleInChannel::getProfile: y in out of bound.");
  }
  const Real yProfile = getHorizontalProfile(yNorm);
  // Fast version here
  const Real zProfile = getVerticalProfileFast(zNorm);
  return yProfile * zProfile;
}

Real TransportParticleInChannel::getVerticalProfile(const Real& zNorm) const
{
  // Use an equation for now.
  // It could be faster using pre-calculated values as in PTM1.
  // The equation is 1 + 1 / k *( 1 + log(z / d))
  // which is derived from the von Karman profile (u = u* /k * ln (z / z0))
  if (zNorm <= 0.) {
    return 0.;
  }
  if (zNorm > 1.) {
    throw std::runtime_error("zNorm > 1.");
  }assert(zNorm > 0.);
  assert(zNorm <= 1.);
  // @todo: Make this a user option
  const Real kappa = 0.4;
  Real factor = 1.0 + 0.1 / kappa * (1.0 + log(zNorm));
  if (factor < 0.001) {
    factor = 0.0001;
  }
  return factor;
}

Real TransportParticleInChannel::getVerticalProfileFast(const Real& zNorm) const
{
  // Use an equation for now.
  // It could be faster using pre-calculated values as in PTM1.
  // The equation is 1 + 1 / k *( 1 + log(z / d))
  // which is derived from the von Karman profile (u = u* /k * ln (z / z0))
  if (zNorm <= 0.) {
    return 0.;
  }
  if (zNorm > 1.) {
    throw std::runtime_error("zNorm > 1.");
  }assert(zNorm > 0.);
  assert(zNorm <= 1.);

  const int index = int(zNorm * NUM_VERTICAL_LAYERS);
  assert(index < NUM_VERTICAL_LAYERS);

  // @todo: Make this a user option
  return verticalProfile_[index];
}

Real TransportParticleInChannel::getHorizontalProfile(const Real& yNorm) const
{
  Real a = 1.2;
  Real b = 0.3;
  Real c = -1.5;
  return a + b * pow(yNorm, 2) + c * pow(yNorm, 4); // quartic profile across Channel width

  return 1.0;
}

// Use the old time step information
Real TransportParticleInChannel::getDepthFast(const Coord& coord)
{
  const Real theta = 0.5;
  const Real depthUpPrev = tide_->getChannelDepth(channelPtr_, 0);
  const Real depthUpNext = tideNext_->getChannelDepth(channelPtr_, 0);
  const Real depthUp = (1. - theta) * depthUpPrev + theta * depthUpNext;
  const Real depthDownPrev = tide_->getChannelDepth(channelPtr_, 1);
  const Real depthDownNext = tideNext_->getChannelDepth(channelPtr_, 1);
  const Real depthDown = (1. - theta) * depthDownPrev + theta * depthDownNext;
  //std::cout << "depthUp, Down: " << depthUp << ", " << depthDown << "\n";
  const Real& x = coord(0);
  assert(x >= 0.);
  assert(x <= length_);
  Real depthOneDim;
  interpolateLinearly(depthOneDim, x, 0., length_, depthUp, depthDown);
  assert(depthOneDim > 0.);
  return depthOneDim;
}

Real TransportParticleInChannel::getDepth(const Real& tCurrent,
                                          const Coord& coord)
{
  const Real& tPrev = tide_->getTimeCursor();
  const Real& tNext = tideNext_->getTimeCursor();
  const Real depthUpPrev = tide_->getDepth(channelPtr_, 0);
  const Real depthUpNext = tideNext_->getDepth(channelPtr_, 0);
  const Real depthDownPrev = tide_->getDepth(channelPtr_, 1);
  const Real depthDownNext = tideNext_->getDepth(channelPtr_, 1);
  Real depthUp;
  interpolateLinearly(depthUp, tCurrent, tPrev, tNext, depthUpPrev,
                      depthUpNext);
  Real depthDown;
  interpolateLinearly(depthDown, tCurrent, tPrev, tNext, depthDownPrev,
                      depthDownNext);
  //std::cout << "depthUp, Down: " << depthUp << ", " << depthDown << "\n";
  const Real& x = coord(0);
  assert(x >= 0.);
  assert(x <= length_);
  Real depthOneDim;
  interpolateLinearly(depthOneDim, x, 0., length_, depthUp, depthDown);
  assert(depthOneDim > 0.);
  return depthOneDim;
}

Real TransportParticleInChannel::getWidth(const Real& tCurrent,
                                          const Coord& coord)
{
  const Real& tPrev = tide_->getTimeCursor();
  const Real& tNext = tideNext_->getTimeCursor();
  const Real widthUpPrev = tide_->getChannelWidth(channelPtr_, 0);
  const Real widthUpNext = tideNext_->getChannelWidth(channelPtr_, 0);
  const Real widthDownPrev = tide_->getChannelWidth(channelPtr_, 1);
  const Real widthDownNext = tideNext_->getChannelWidth(channelPtr_, 1);
  Real widthUp;
  interpolateLinearly(widthUp, tCurrent, tPrev, tNext, widthUpPrev,
                      widthUpNext);
  Real widthDown;
  interpolateLinearly(widthDown, tCurrent, tPrev, tNext, widthDownPrev,
                      widthDownNext);
  const Real& x = coord(0);
  assert(x >= 0.);
  assert(x <= length_);
  Real widthOneDim;
  interpolateLinearly(widthOneDim, x, 0., length_, widthUp, widthDown);
  assert(widthOneDim > 0.);
  return widthOneDim;
}

Real TransportParticleInChannel::getWidthFast(const Coord& coord)
{
  const Real theta = 0.5;
  const Real widthUpPrev = tide_->getChannelWidth(channelPtr_, 0);
  const Real widthUpNext = tideNext_->getChannelWidth(channelPtr_, 0);
  const Real widthUp = (1. - theta) * widthUpPrev + theta * widthUpNext;
  const Real widthDownPrev = tide_->getChannelWidth(channelPtr_, 1);
  const Real widthDownNext = tideNext_->getChannelWidth(channelPtr_, 1);
  const Real widthDown = (1. - theta) * widthDownPrev + theta * widthDownNext;
  const Real& x = coord(0);
  assert(x >= 0.);
  assert(x <= length_);
  Real widthOneDim;
  interpolateLinearly(widthOneDim, x, 0., length_, widthUp, widthDown);
  assert(widthOneDim > 0.);
  return widthOneDim;
}

void TransportParticleInChannel::setRandomYZ(Particle& particle,
                                             const Channel& channel,
                                             const Real& tCurrent)
{
  Coord& coord = particle.getCoordinate();
  Real& y = coord(1);
  Real& z = coord(2);
  depth_ = getDepthFast(coord);
  width_ = getWidthFast(coord);
  assert(depth_ > 0.);
  assert(width_ > 0.);
  RandomNumberGenerator& rng = RandomNumberGenerator::instance();
  y = width_ * (rng() - 0.5);
  z = depth_ * rng();

  this->bounce(y, z);

  coord.setReady();
  particle.setNewInWaterbody(false);
  particle.setOldWidth(width_);
  particle.setOldDepth(depth_);
}

void TransportParticleInChannel::adjustYZ(Particle& particle,
                                          const Channel& channel)
{
  Coord& coord = particle.getCoordinate();
  Real& y = coord(1);
  Real& z = coord(2);
  const Real& oldWidth = particle.getOldWidth();
  const Real& oldDepth = particle.getOldDepth();
  Real newY = y * width_ / oldWidth;
  Real newZ = z * depth_ / oldDepth;
  y = newY;
  z = newZ;
}

void TransportParticleInChannel::bounce(Real& y, Real& z)
{
  const Real halfWidth = width_ * 0.5;
  while (y < -halfWidth || y > halfWidth) {
    if (y < -halfWidth) {
      y = -width_ - y;
    }
    else if (y > halfWidth) {
      y = width_ - y;
    }
  }
  // Add Vertical dispersion
  while (z < 0. || z > depth_) {
    if (z < 0.) {
      z = -z;
    }
    else if (z > depth_) {
      z = 2. * depth_ - z;
    }
  }
}

}
