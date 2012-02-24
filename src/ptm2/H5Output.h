/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _H5Output_h_
#define _H5Output_h_

#include "PTM2.h"
#include "PTM2Time.h"
#include "Particle.h"
#include "Grid.h"

#include "hdf5.h"
#include <vector>
#include <string>

namespace PTM2
{

/**
 * @brief Class to manage HDF5 outputs of particles
 **/
class H5Output
{
private:
  static hid_t fileId_;
  hid_t currentTimeGroupId_;
  int nSteps_;

  std::vector<int> intCache_;
  std::vector<double> doubleCache_;

public:
  ~H5Output();
  void open(const std::string& fname);
  void close();
  void writeGrid(const Grid& grid);
  void createTimeStep(const int& julmin);
  void closeTimeStep();
  void writeInserting(const Particle& particle);
  void writeExiting(const Particle& particle);
  void writeParticles(const std::vector<Particle*>& particles);
  static hid_t getH5FileHandle() { return fileId_; }

private:
  template<typename T>
  void writeData(std::string& name, hid_t hid, const int rank, const hsize_t* dims,const T* data);
};

}

#endif
