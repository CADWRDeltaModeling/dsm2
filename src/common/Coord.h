/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _Coord_h_
#define _Coord_h_

#include "PTM2.h"
#include "TypeVector.h"
#include "Waterbody.h"

namespace PTM2
{

/**
 * @brief Coordinate class for PTM2
 *
 * Coordinate of DSM2 native coordinate is always associated with a waterbody
 **/
class Coord : public TypeVector<Real>
{
protected:
  const Waterbody* waterbody_;
  bool ready_;

public:
  Coord(const Waterbody* waterbody)
    : waterbody_(waterbody), ready_(false)
  {
  }
	/**
	 * @brief Constructor
	 */
	Coord(const Waterbody* waterbody, const Real& x, const Real& y, const Real& z)
		: TypeVector<Real>(x,y,z), waterbody_(waterbody), ready_(true) {}

	/**
	 * @brief Copy constructor
	 */
	Coord(const Coord& r)
    : TypeVector<Real>(r), waterbody_(r.waterbody_), ready_(r.ready_) {}

	/**
	 * @brief Copy constructor
	 */
	Coord(const TypeVector<Real>& r) : TypeVector<Real>(r), waterbody_(NULL), ready_(false) {}

	// Operator overloading
	/**
	 * @brief Assignment operator
	 */
	Coord& operator=(const Coord & r)
	{
    waterbody_ = r.waterbody_;
    coords_[0] = r(0);
    coords_[1] = r(1);
    coords_[2] = r(2);
    return *this;
	}

  const Waterbody* getWaterbody() const { return waterbody_; }
  void setWaterbody(const Waterbody* waterbody) { waterbody_ = waterbody; }

  friend std::ostream& operator<<(std::ostream& os, const Coord& r)
  {
    os << r.getWaterbody()->getExternalId() << ": " << r.coords_[0] << ", " << r.coords_[1] << ", " << r.coords_[2];
    return os;
  }
  bool isReady() const { return ready_; }
  void setReady(bool ready = true) { ready_ = ready; }
};

}

#endif // __Coord_h__
