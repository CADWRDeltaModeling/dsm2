/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _Model_h_
#define _Model_h_

#include "PTM2Time.h"
#include "Grid.h"
#include "Tide.h"
#include "H5TideReader.h"
#include "Insertion.h"
#include "Particle.h"
#include "Transport.h"
#include "PTM2Parameters.h"
#include "H5Output.h"
#include "TraceOutput.h"
#include <vector>

namespace PTM2
{

/**
 * @brief Model 
 **/
class Model
{
// Properties
protected:
  Grid grid_;
  Tide* tide_;
  Tide* tideNext_;
  PTM2Parameters parameters_;

  H5TideReader tideProvider_;

  // Model variables
  std::vector<Insertion> insertions_;

  // Particles
  std::vector<Particle*> particles_;

  // Transport algorithm
  Transport transport_;

  H5Output h5Writer_;
  TraceOutput traceWriter_;

// Methods
public:
  /// @brief Constructor
  Model();
  /// @brief Destructor
  ~Model();

  // Accessors
  Parameters& getParameters() { return parameters_; }
  const Grid& getGrid() const { return grid_; }
  Grid& getGrid() { return grid_; }

  void initialize();
  void run();

  // Input related function
  void addTideFile(const std::string& fname,
    const int& julminStart,
    const int& julminEnd);
  void addInsertion(const int& insertionNodeId,
    const unsigned int& nParticles,
    const Real& tStart, 
    const Real& tDuration);

private:
  void createParticles();
  void swapTides();
};

}

#endif
