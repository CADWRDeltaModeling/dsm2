/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _Particle_h_
#define _Particle_h_

#include "PTM2.h"
#include "Coord.h"

namespace PTM2
{

/**
 * @brief Base particle class without any behavior
 **/
class Particle
{
protected:
  enum ParticleStatus {NOTUSED, ACTIVE, EXITING, INACTIVE};

  // Attributes
protected:
  int id_; // Integer in MSVC is 4 byte, which should cover up to 3 billions.
  Coord coord_;  
  Real insertionTime_;
  Real currentTime_;
  ParticleStatus status_;
  bool newInWaterbody_;

  const Waterbody* prevWb_;  // Additional info for tracing

  Real oldWidth_;
  Real oldDepth_;

  static int count_;

  // Methods
public:
  /// @brief Constructor
  Particle(const Coord& coord, const Real& insertionTime)
    : id_(count_), coord_(coord), insertionTime_(insertionTime), status_(NOTUSED),
      prevWb_(NULL), newInWaterbody_(true)
  {
    ++count_;
  }
  virtual ~Particle() {}
  
  bool isNotUsed() const { return status_ == NOTUSED; }
  bool isActive() const { return status_ == ACTIVE; }
  void setActive() { status_ = ACTIVE;  currentTime_ = insertionTime_; }
  void setExiting() { status_ = EXITING; }
  bool isExiting() const { return status_ == EXITING; }
  bool isInactive() const { return status_ == INACTIVE; }
  void setInactive() { status_ = INACTIVE; }
  bool isNewInWaterbody() const { return newInWaterbody_; }
  void setNewInWaterbody(const bool f = true) { newInWaterbody_ = f; }
  void setOldWidth(const Real& w) { oldWidth_ = w; }
  const Real& getOldWidth() const { return oldWidth_; }
  void setOldDepth(const Real& d) { oldDepth_ = d; }
  const Real& getOldDepth() const { return oldDepth_; }

  int getId() const { return id_; } 
  Real& getCurrentTime() { return currentTime_; }
  const Real& getInsertionTime() const { return insertionTime_; }
  Coord& getCoordinate() { return coord_; }
  const Coord& getCoordinate() const { return coord_; }
  const Real& getCurrentTime() const { return currentTime_; }
  void setCurrentTime(const Real& t) { currentTime_ = t; }

  virtual void moveIn(const Waterbody* Waterbody, const int internalId = 0);
  const Waterbody* getPreviousWaterbody() const { return prevWb_; }
};

}

#endif
