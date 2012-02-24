/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _Reservoir_h_
#define _Reservoir_h_

#include "PTM2.h"
#include "Waterbody.h"
#include "Node.h"
#include <string>

namespace PTM2
{

/**
 * @brief Reservoir waterbody
 **/
class Reservoir : public Waterbody
{
protected:
  Real area_;
  Real bottomElevation_;
  int nReservoirConnects_;

public:
  Reservoir(const int& id,
    const std::string& externalId,
    const Real& area,
    const Real& bottomElevation)
    : Waterbody(id, externalId),
    area_(area * 1.e6),
    bottomElevation_(bottomElevation),
    nReservoirConnects_(0)
  {
  }
  ~Reservoir() { Waterbody::~Waterbody(); }

  const Real& getArea() const { return area_; }
  const Real& getBottomElevation() const { return bottomElevation_; }
  const int& getNumberOfReservoirConnects() const { return nReservoirConnects_; }
  int& getNumberOfReservoirConnects() { return nReservoirConnects_; }

  /**
   * @brief Add a connection to a reservoir
   * @param waterbody a pointer to a connected waterbody
   * @param internalId an internal Id of the connection in the connected waterbody
   **/
  void addConnectedWaterbody(const Waterbody* waterbody, const int& internalId)
  {
    Waterbody::addConnectedWaterbody(waterbody, internalId);
    ++nReservoirConnects_;
  }

  virtual int getType() const { return RESERVOIR; }
};

}

#endif