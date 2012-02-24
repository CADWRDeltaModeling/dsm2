/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef __insertion_h__
#define __insertion_h__

#include "PTM2.h"

namespace PTM2
{
class Insertion
{
  // Properties
private:
  int insertionNodeId_;
  unsigned nParticles_;
  Real tDelay_;
  Real tDuration_;
  //Real t_period_;
  //unsigned int n_left_;
  //Real t_end_;
  //bool done_;

    // Methods
public:
  /**
   * @brief Constructor
   */
  Insertion(const int& insertionNodeId,
                const unsigned int& nParticles,
                const Real& tDelay, 
                const Real& tDuration) :
              insertionNodeId_(insertionNodeId),
              nParticles_(nParticles),
              tDelay_(tDelay),
              tDuration_(tDuration)
  {  }

  /**
   * @brief Copy constructor
   */
  Insertion(const Insertion& r)
  {
    *this = r;
  }

  // Overloaded assignment operator
  Insertion& operator=(const Insertion& r)
  {
    insertionNodeId_ = r.insertionNodeId_;
    nParticles_ = r.nParticles_;
    tDelay_ = r.tDelay_;
    tDuration_ = r.tDuration_;
    return *this;
  }

    // Accessors
  const int& getInsertionNodeId() const { return insertionNodeId_; }
  const unsigned int& getNumberOfInsertedParticles() const { return nParticles_; }
  const Real& getStartDelay() const { return tDelay_; }
  const Real& getDuration() const { return tDuration_; }
};

}

#endif
