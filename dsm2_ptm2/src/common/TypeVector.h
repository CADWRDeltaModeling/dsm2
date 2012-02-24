/**
*  Author: Kijin Nam, knam@water.ca.gov
*/

#ifndef _TypeVector_h_
#define _TypeVector_h_

#include <iostream>
#include <cassert>

namespace PTM2
{

/**
* @brief Base class of all coordinate and 3D vectors
*/
template<typename T>
class TypeVector
{
  // Properties
protected:
  T coords_[3];

  // Methods
public:
  /**
  * @brief Constructor
  * Constructor with three numbers
  */
  TypeVector(const T& x=0., const T& y=0., const T& z=0.);
  /// @brief Copy constructor
  TypeVector(const TypeVector<T>& r);

  /// @brief Destructor
  virtual ~TypeVector() {}

  /**
  * @brief Accessor
  */
  T& operator()(const int&);
  /**
  * @brief Constant accessor
  */
  const T& operator()(const int&) const;

  /**
  * @brief Add operator
  * @param r Add r to this vector
  */
  virtual void add(const TypeVector<T>& r);
  /**
  * @brief Add operator
  */
  virtual TypeVector<T> operator+(const TypeVector<T>& r) const;
  /**
  * @brief Scalar multiplication operator
  * @param r Scalar to mulitply by
  */
  virtual TypeVector<T> operator*(const T scalar) const;
  /**
  * @brief Scale the vector
  */
  virtual void scale(const T scalar);

  /**
  * @brief Print this vector to iostream
  * @param os output stream to send the vector
  */
  virtual void print(std::ostream& os) const;
  /**
  * @brief Stream operator
  * @param os output stream
  * @param r a vector to send to the stream
  */
  friend std::ostream& operator<<(std::ostream& os, const TypeVector<T>& r)
  {
    os << r.coords_[0] << ", " << r.coords_[1] << ", " << r.coords_[2];
    return os;
  }
};




template<typename T>
inline
  TypeVector<T>::TypeVector(const T& x, const T& y, const T& z)
{
  coords_[0] = x;
  coords_[1] = y;
  coords_[2] = z;
}



template<typename T>
inline
  TypeVector<T>::TypeVector(const TypeVector<T>& r)
{
  for ( unsigned int i=0; i<3; ++i ) {
    this->coords_[i] = r.coords_[i];
  }
}



template<typename T>
inline
  void
  TypeVector<T>::add(const TypeVector<T>& r)
{
  for ( unsigned int i=0; i<3; ++i ) {
    coords_[i] += r(i);
  }
}



template<typename T>
inline
  T&
  TypeVector<T>::operator()(const int& i)
{
  assert(i<3);
  return coords_[i];
}



template<typename T>
inline
  const T&
  TypeVector<T>::operator()(const int& i) const
{
  assert(i<3);
  return coords_[i];
}



template<typename T>
inline
  void
  TypeVector<T>::print(std::ostream& os) const
{
  os << coords_[0] << ", " << coords_[1] << ", " << coords_[2] << "\n";
}



template<typename T>
inline
  TypeVector<T>
  TypeVector<T>::operator*(const T scalar) const
{
  return TypeVector<T>(coords_[0] * scalar,
    coords_[1] * scalar,
    coords_[2] * scalar
    );
}


template<typename T>
inline
  void
  TypeVector<T>::scale(const T scalar)
{
  coords_[0] *= scalar;
  coords_[1] *= scalar;
  coords_[2] *= scalar;
}



template<typename T>
inline
  TypeVector<T>
  TypeVector<T>::operator+(const TypeVector<T>& r) const
{
  return TypeVector<T>(coords_[0] + r.coords_[0],
    coords_[1] + r.coords_[1],
    coords_[2] + r.coords_[2]);
}

} // namespace PTM2

#endif // __TypeVector_h__
