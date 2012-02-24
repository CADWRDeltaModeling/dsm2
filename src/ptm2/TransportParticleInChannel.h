/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _TransportParticleInChannel_h_
#define _TransportParticleInChannel_h_

#include "Particle.h"
#include "Tide.h"
#include "Channel.h"
#include "PTM2Parameters.h"
#include "TransportBase.h"

namespace PTM2
{

class TransportParticleInChannel : public TransportBase
{
private:
  enum Constants {NUM_VERTICAL_LAYERS = 1000};

public:
  // Temporary variables
  Real width_;
  Real depth_;
  Real area_;
  Real averageVel_;
  Real length_;
  Real localVertDispCoeff_;
  Real localHoriDispCoeff_;
  const Channel* channelPtr_;

protected:
  Real verticalProfile_[NUM_VERTICAL_LAYERS];

  // Methods
public:
  TransportParticleInChannel(const PTM2Parameters& parameters, Tide*& tide, Tide*& tideNext);
  bool operator()(Particle& p, const Channel& wb);

protected:
  void precalculateVerticalProfile();
  // Get velocity with time interpolation
  Real getVelocity(const Real& currentTime, const Coord& coord);
  // Get velocity from average velocity
  Real getVelocity(const Coord& coord);
  Real getProfile(const Coord& coord) const;
  Real getVerticalProfile(const Real& z) const;
  Real getVerticalProfileFast(const Real& z) const;
  Real getHorizontalProfile(const Real& z) const;
  Real getDepth(const Real& currentTime, const Coord& coord);
  Real getDepthFast(const Coord& coord);
  Real getWidth(const Real& currentTime, const Coord& coord);
  Real getWidthFast(const Coord& coord);
  void setRandomYZ(Particle& particle, const Channel& channel,
    const Real& tCurrent);
  void adjustYZ(Particle& particle,
                const Channel& channel);
  // This collects variables at the current location of the particle
  // with time interpolation.
  void collectValues(const Particle& particle,
                     const Channel& channel,
                     const Real& tCurrent);
  // This collects variables at the current location of the particle
  // without time interpolation.
  // It is supposed to be faster than the full version.
  void collectValuesFast(const Particle& particle,
                     const Channel& channel);
  void addDispersion(Particle& particle,
                     const Real& tSpent);
  Real getAverageVelocity(const Coord& coord);
  Real getAverageVelocityNext(const Coord& coord);
  void getSubTimeStep(Real& tSubTimeStep,
                      int& nSubTimeSteps,
                      Particle& particle,
                      const Real& tLeft,
                      const Channel& channel);
  void bounce(Real& y, Real& z);
};

}

#endif /* __TransportParticleInChannel_h__ */
