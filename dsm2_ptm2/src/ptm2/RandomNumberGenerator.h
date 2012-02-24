/**
 *  Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _RandomNumberGenerator_h_
#define _RandomNumberGenerator_h_

#include "boost/random.hpp"

namespace PTM2
{

/**
 * @brief Random number generator
 *
 * This is a random number generator. Only one RNG can exist globally.
 */
class RandomNumberGenerator
{
public:
  // typedef boost::minstd_rand base_generator_type;
  typedef boost::mt19937 base_generator_type;

  // Properties
private:
  base_generator_type generator_;
  boost::uniform_real<> uni_dist_;
  boost::variate_generator<base_generator_type&, boost::uniform_real<> > uni_;

  // Methods
public:
  static RandomNumberGenerator& instance()
  {
    static RandomNumberGenerator theRng;
    return theRng;
  }

private:
  RandomNumberGenerator() :
      uni_dist_(0., 1.), uni_(generator_, uni_dist_)
  {
  }

public:
  void setSeed(unsigned long seed)
  {
    generator_.seed(seed);
  }

  double operator()()
  {
    return uni_();
  }
};

class NormalRandomNumberGenerator
{
  // Properties
private:
  typedef boost::mt19937 BaseGeneratorT;
  typedef boost::normal_distribution<double> DistributionT;

  // Base Random number generator
  BaseGeneratorT baseGenerator_;
  // Actual random number generator
  boost::variate_generator<BaseGeneratorT&, DistributionT> normal_;

  // Static pointer for the singleton
  static NormalRandomNumberGenerator* s_instance;

  // Methods
public:
  static NormalRandomNumberGenerator& instance()
  {
    static NormalRandomNumberGenerator theRng;
    return theRng;
  }

private:
  // Constructor. It is private because the class is create only by itself.
  NormalRandomNumberGenerator() :
      normal_(baseGenerator_, DistributionT())
  {
  }

public:
  /**
   * @brief Create a one random number that follows the standard normal dist.
   */
  double operator()()
  {
    return normal_();
  }
  void setSeed(unsigned long a_seed)
  {
    baseGenerator_.seed(a_seed);
  }
};

}

#endif /* __RandomNumberGenerator_h__ */
