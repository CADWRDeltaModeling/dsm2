/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#include "Model.h"
#include "RandomNumberGenerator.h"
#include "boost/foreach.hpp"
#include <omp.h>

namespace PTM2
{

Model::Model() :
    transport_(parameters_, tide_, tideNext_)
{
  tide_ = new Tide(grid_);
  tideNext_ = new Tide(grid_);
}

Model::~Model()
{
  herr_t err = H5close();
  if (err < 0) {
    std::runtime_error("Model::H5close: Cannot close H5 library.");
  }

  if (tide_ != NULL) {
    delete tide_;
  }
  if (tideNext_ != NULL) {
    delete tideNext_;
  }

  // Clean up particles
  BOOST_FOREACH(Particle* particle, particles_)
      {
        assert(particle != NULL);
        delete particle;
      }
}

void Model::addTideFile(const std::string& fname,
                        const int& julminStart,
                        const int& julminEnd)
{
  tideProvider_.addTideFile(fname, julminStart, julminEnd);
}

void Model::addInsertion(const int& insertionNodeId,
                         const unsigned int& nParticles,
                         const Real& tStart,
                         const Real& tDuration)
{
  insertions_.push_back(
      Insertion(insertionNodeId, nParticles, tStart, tDuration));
}

void Model::initialize()
{
  herr_t err = H5open();
  if (err < 0) {
    std::runtime_error("Model::initialize: Cannot initialize H5 library.");
  }

  tideProvider_.retrieveTimeCoverageOfTideFiles();
  createParticles();
  h5Writer_.open(parameters_.getHdf5OutputFilename());
  h5Writer_.writeGrid(grid_);

  parameters_.setTraceWriterPointer(&traceWriter_);
  // Set seeds
  NormalRandomNumberGenerator& normal = NormalRandomNumberGenerator::instance();
  unsigned int seed = 1;
  normal.setSeed(seed);
}

void Model::run()
{
  initialize();
  int& julminCurrent = parameters_.getTimeCurrent();
  int& julminNext = parameters_.getTimeNext();
  const int& dt = parameters_.getDt();
  julminCurrent = parameters_.getTimeStart();
  julminNext = julminCurrent + dt;
  tideProvider_.updateTideAtOrBefore(julminCurrent, tide_);
  tideProvider_.updateTideAfter(julminCurrent, tideNext_);
  int stride = 4;

  //omp_set_dynamic(0);
  //omp_set_nested(1);
  //const int procs = omp_get_num_procs();
  //omp_set_num_threads(procs);
  const PTM2Time& ptm2time = *PTM2Time::instance();

  for (int i = 0; julminCurrent < parameters_.getTimeEnd();
      julminCurrent += dt, ++i) {
    if (julminCurrent >= tideNext_->getTimeCursorJulian()) {
      swapTides();
      tideProvider_.updateTideAfter(julminCurrent, tideNext_);
    }
    h5Writer_.createTimeStep(julminNext);
    if (i % stride == 0)
      std::cout << "Time:" << ptm2time.julmin2Time(julminCurrent) << std::endl;
    parameters_.setTimeNext(julminCurrent + dt);
    const Real tCurrent = Real(julminCurrent) * 60.;
    const Real tNext = Real(julminNext) * 60.;
    //#pragma omp parallel
    //{
    BOOST_FOREACH(Particle* particle, particles_)
        {
          //#pragma omp single nowait
          //{
          if (!particle->isInactive()) {
            if (particle->isNotUsed()) {
              // Note that all the time units are now Julian seconds
              const Real& insertionReal = particle->getInsertionTime();
              if (tCurrent <= insertionReal && insertionReal < tNext) {
                particle->setActive();
                h5Writer_.writeInserting(*particle);
              }
            }
            if (particle->isActive()) {
              transport_(*particle);
            }
            if (particle->isExiting()) {
              h5Writer_.writeExiting(*particle);
              particle->setInactive();
            }
            //}
          }
          //std::cout << particle->getCoordinate() << " " << particle->isActive() << std::endl;
          //}
        }
    h5Writer_.writeParticles(particles_);
    traceWriter_.flush();
    h5Writer_.closeTimeStep();
  }
  traceWriter_.close();
  h5Writer_.close();
}

void Model::swapTides()
{
  Tide* tmpTide;
  tmpTide = tide_;
  tide_ = tideNext_;
  tideNext_ = tmpTide;
}

void Model::createParticles()
{
  // Note that the time unit here is Julian second.
  BOOST_FOREACH(const Insertion& insertion, insertions_)
      {
        const int& nParticles = insertion.getNumberOfInsertedParticles();
        const int& nodeId = insertion.getInsertionNodeId();
        const Real& delay = insertion.getStartDelay();
        const Real& duration = insertion.getDuration();
        const Node* pNode = grid_.findNodeByName(
            boost::lexical_cast<std::string>(nodeId));
        assert(pNode != NULL);
        const Real dt = duration / nParticles;
        const Real insertionStartTime = Real(parameters_.getTimeStart()) * 60.
            + delay; // Dump below second
        Coord insertionCoord(pNode);
        for (int i = 0; i < nParticles; ++i) {
          const Real offset = (Real) (i) * dt;
          Real insertionTime = insertionStartTime + offset;
          // @todo: One type of particle are created.
          // It will be modified to create different types of particle.
          Particle* pParticle = new Particle(insertionCoord, insertionTime);
          particles_.push_back(pParticle);
        }
      }
}

}
