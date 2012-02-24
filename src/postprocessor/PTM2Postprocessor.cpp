/**
 * Author: Kijin Nam, knam@water.ca.gov
 */

#include "PTM2Postprocessor.h"
#include "PTM2Time.h"
#include "heclib_c.h"
#include "hdf5.h"
#include "boost/foreach.hpp"
#include "boost/format.hpp"
#include <iostream>
#include <stdio.h>

namespace PTM2
{

void PTM2Postprocessor::run()
{
  calculateFluxes();
  processFluxes();
  processGroups();
  writeFluxes();
  writeGroups();
}

void PTM2Postprocessor::calculateFluxes()
{
  const int nFluxes = parameters_.getFluxOutputs().size();

  const PTM2Time& ptm2time = *PTM2Time::instance();
  processedFluxes_.resize(nFluxes);
  const int tStart = parameters_.getTimeStart();
  const int tEnd = parameters_.getTimeEnd();
  int iFlux = 0;
  BOOST_FOREACH(const PTM2FluxOutput& fluxOutput, parameters_.getFluxOutputs()) {
    const int interval = fluxOutput.getInterval();
    const int nBins = (tEnd - tStart) / interval + 1;
    processedFluxes_[iFlux].resize(nBins, 0.);
    ++iFlux;
  }
  int iGroup = 0;
  const int nGroups = parameters_.getGroupOutputs().size();
  groupCounts_.resize(nGroups);
  BOOST_FOREACH(const PTM2GroupOutput& groupOutput, parameters_.getGroupOutputs()) {
    const int interval = groupOutput.getInterval();
    const int nBins = (tEnd - tStart) / interval + 1;
    groupCounts_[iGroup].resize(nBins, 0.);
    ++iGroup;
  }
  readTracesAndCalculate();
}

void PTM2Postprocessor::readTracesAndCalculate()
{
  const std::string& fname = parameters_.getHdf5OutputFilename();
  H5open();
  hid_t fh = H5Fopen(fname.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);

  const std::string tracesGname("traces");
  hid_t tracesGroup = H5Gopen(fh, tracesGname.c_str(), H5P_DEFAULT);
  const std::string numOfChunksName("num_trace_groups");
  hid_t numOfChunksAttr = H5Aopen(tracesGroup, numOfChunksName.c_str(), H5P_DEFAULT);
  int numOfChunks;
  H5Aread(numOfChunksAttr, H5T_NATIVE_INT, &numOfChunks);
  H5Aclose(numOfChunksAttr);

  const int tStart = parameters_.getTimeStart();
  const int stride = 10;
  for (int iChunk = 0; iChunk < numOfChunks; ++iChunk) {
    if (iChunk % stride == 0)
      std::cout << "Progress: " << float(iChunk * 100) / float(numOfChunks) << " %...\r" << std::flush;
    std::string traceChunkName(boost::str(boost::format("%d") % iChunk));
    hid_t traceChunkGroup = H5Gopen(tracesGroup, traceChunkName.c_str(), H5P_DEFAULT);
    const std::string numOfRecordsName("num_traces");
    hid_t numOfRecordsAttr = H5Aopen(traceChunkGroup, numOfRecordsName.c_str(), H5P_DEFAULT);
    int nRecords;
    H5Aread(numOfRecordsAttr, H5T_NATIVE_INT, &nRecords);
    H5Aclose(numOfRecordsAttr);
    // Read traces
    pIds_.resize(nRecords);
    timestamp_.resize(nRecords);
    fromWbT_.resize(nRecords);
    fromWb_.resize(nRecords);
    toWbT_.resize(nRecords);
    toWb_.resize(nRecords);
    hid_t ds;
    ds = H5Dopen(traceChunkGroup, "particle_id", H5P_DEFAULT);
    H5Dread(ds, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &pIds_[0]);
    H5Dclose(ds);
    ds = H5Dopen(traceChunkGroup, "timestamp", H5P_DEFAULT);
    H5Dread(ds, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &timestamp_[0]);
    H5Dclose(ds);
    ds = H5Dopen(traceChunkGroup, "wb_from", H5P_DEFAULT);
    H5Dread(ds, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &fromWb_[0]);
    H5Dclose(ds);
    ds = H5Dopen(traceChunkGroup, "wb_type_from", H5P_DEFAULT);
    H5Dread(ds, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &fromWbT_[0]);
    H5Dclose(ds);
    ds = H5Dopen(traceChunkGroup, "wb_to", H5P_DEFAULT);
    H5Dread(ds, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &toWb_[0]);
    H5Dclose(ds);
    ds = H5Dopen(traceChunkGroup, "wb_type_to", H5P_DEFAULT);
    H5Dread(ds, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &toWbT_[0]);
    H5Dclose(ds);

    for (int iTrace = 0; iTrace < nRecords; ++iTrace) {
      const int& id = pIds_[iTrace];
      const int& ts = timestamp_[iTrace];
      const int& fromWb = fromWb_[iTrace];
      const int& fromWbT = fromWbT_[iTrace];
      const int& toWb = toWb_[iTrace];
      const int& toWbT = toWbT_[iTrace];
      // Loop through fluxes
      int iFlux = 0;
      const std::list<PTM2FluxOutput>& fluxOutputs = parameters_.getFluxOutputs();
      BOOST_FOREACH(const PTM2FluxOutput& fluxOutput, fluxOutputs) {
        std::vector<float>& flux = processedFluxes_[iFlux];
        const int matched = fluxOutput.match(fromWb, fromWbT, toWb, toWbT);
        if (matched != 0) {
          const int interval = fluxOutput.getInterval();
          const int bin = (ts - tStart) / interval + 1;
          if (bin < flux.size()) {
            flux[bin] += float(matched);
          } 
        }
        ++iFlux;
      }
      // Group
      int iGroup = 0;
      BOOST_FOREACH(const PTM2GroupOutput& groupOutput, parameters_.getGroupOutputs()) {
        std::vector<float>& counts = groupCounts_[iGroup];
        const int matched = groupOutput.match(fromWb, fromWbT, toWb, toWbT);
        assert(matched > -2 && matched < 2);
        if (matched != 0) {
          const int interval = groupOutput.getInterval();
          const int bin = (ts -tStart) / interval + 1;
          if (bin < counts.size()) {
            counts[bin] += float(matched);
          }
        }
        ++iGroup;
      }
    }
    H5Gclose(traceChunkGroup);
  }
  H5Gclose(tracesGroup);
  H5Fclose(fh);
  H5close();
}

void PTM2Postprocessor::processFluxes()
{
  const bool isAccumulated = parameters_.getAccumulated();
  if (isAccumulated) {
    int iFlux = 0;
    BOOST_FOREACH(const PTM2FluxOutput& fluxOutput, parameters_.getFluxOutputs()) {
      std::vector<float>& flux = processedFluxes_[iFlux];
      const int nBins = flux.size();
      for (int i = 1; i < nBins; ++i) {
        flux[i] += flux[i-1];
      }
      ++iFlux;
    }
  }
  const bool isPercent = parameters_.getFluxPercentaged();
  if (isPercent) {
    int iFlux = 0;
    const float factor = 100. / float(parameters_.getTotalNParticles());
    BOOST_FOREACH(const PTM2FluxOutput& fluxOutput, parameters_.getFluxOutputs()) {
      std::vector<float>& flux = processedFluxes_[iFlux];
      const int nBins = flux.size();
      for (int i = 1; i < nBins; ++i) {
        flux[i] *= factor;
      }
      ++iFlux;
    }
  }
}

void PTM2Postprocessor::writeFluxes()
{
  int iFlux = 0;
  const std::string dsm2modifier(parameters_.getModifier());
  const int tStart = parameters_.getTimeStart();
  const int julday = tStart / (24 * 60);
  char cdate[10];
  cdate[9] = '\0';
  int cdate_len = 9;
  int cstyle = 104;
  juldat(&julday, &cstyle, cdate, &cdate_len, cdate_len);
  cdate_len = strlen(cdate);
  const int hour = tStart - julday * (24 * 60);
  const int julhour = hour / 60;
  const int minute = hour - julhour * 60;
  const std::string ctime(boost::str(boost::format("%02d%02d") % julhour % minute));
  const int ctime_len = ctime.size();
  const bool isPercent = parameters_.getFluxPercentaged();
  BOOST_FOREACH(const PTM2FluxOutput& fluxOutput, parameters_.getFluxOutputs()) {
    const std::string& fname = fluxOutput.getFname();
    int ifltab[601];
    char fnamePtr[256];
    sprintf(fnamePtr, "%s", fname.c_str());
    int filenameLength = fname.size();
    int status;
    zopen(ifltab, fnamePtr, &status, filenameLength);
    if (status != 0) {
      throw std::runtime_error("Failed to open a dss file.");
    }

    std::vector<float>& flux = processedFluxes_[iFlux];
    const std::string& name = fluxOutput.getName();
    const int interval = fluxOutput.getInterval();
    const std::string intervalStr(convertIntervalToString(interval));

    std::string cpath(boost::str(boost::format("/PTM2/%s/FLUX//%s/%s/")
      % name % intervalStr % dsm2modifier));
    const int cpath_len = cpath.size();
    const int nvals = flux.size();
    const float* values = &flux[0];
    std::string cunit("N_PART");
    if (isPercent) {
      cunit = "%";
    }
    const int cunit_len = cunit.size();
    std::string ctype("INST-VAL");
    const int ctype_len = ctype.size();
    int iplan = 0;
    int istat;
    zsrts(ifltab, cpath.c_str(), cdate, ctime.c_str(),
      &nvals, values, cunit.c_str(), ctype.c_str(), &iplan, &istat,
      cpath_len, cdate_len, ctime_len, cunit_len, ctype_len);
    zclose(ifltab);
    ++iFlux;
  }
}

void PTM2Postprocessor::processGroups()
{
  int iGroup = 0;
  BOOST_FOREACH(const PTM2GroupOutput& fluxOutput, parameters_.getGroupOutputs()) {
    std::vector<float>& count = groupCounts_[iGroup];
    const int nBins = count.size();
    for (int i = 1; i < nBins; ++i) {
      count[i] += count[i-1];
    }
    ++iGroup;
  }
  const bool isPercent = parameters_.getGroupPercentaged();
  if (isPercent) {
    iGroup = 0;
    const float factor = 100. / float(parameters_.getTotalNParticles());
    BOOST_FOREACH(const PTM2GroupOutput& fluxOutput, parameters_.getGroupOutputs()) {
      std::vector<float>& count = groupCounts_[iGroup];
      const int nBins = count.size();
      for (int i = 1; i < nBins; ++i) {
        count[i] *= factor;
      }
      ++iGroup;
    }
  }
}

void PTM2Postprocessor::writeGroups()
{
  int iGroup = 0;
  const std::string dsm2modifier(parameters_.getModifier());
  const int tStart = parameters_.getTimeStart();
  const int julday = tStart / (24 * 60);
  char cdate[10];
  cdate[9] = '\0';
  int cdate_len = 9;
  int cstyle = 104;
  juldat(&julday, &cstyle, cdate, &cdate_len, cdate_len);
  cdate_len = strlen(cdate);
  const int hour = tStart - julday * (24 * 60);
  const int julhour = hour / 60;
  const int minute = hour - julhour * 60;
  const std::string ctime(boost::str(boost::format("%02d%02d") % julhour % minute));
  const int ctime_len = ctime.size();
  const bool isPercent = parameters_.getGroupPercentaged();
  BOOST_FOREACH(const PTM2GroupOutput& groupOutput, parameters_.getGroupOutputs()) {
    const std::string& fname = groupOutput.getFname();
    int ifltab[601];
    char fnamePtr[256];
    sprintf(fnamePtr, "%s", fname.c_str());
    int filenameLength = fname.size();
    int status;
    zopen(ifltab, fnamePtr, &status, filenameLength);
    if (status != 0) {
      throw std::runtime_error("Failed to open a dss file.");
    }

    std::vector<float>& count = groupCounts_[iGroup];
    const std::string& name = groupOutput.getName();
    const int interval = groupOutput.getInterval();
    const std::string intervalStr(convertIntervalToString(interval));

    std::string cpath(boost::str(boost::format("/PTM2/%s/PTM_GROUP//%s/%s/")
      % name % intervalStr % dsm2modifier));
    const int cpath_len = cpath.size();
    const int nvals = count.size();
    const float* values = &count[0];
    std::string cunit("N_PART");
    if (isPercent)
      cunit = "%";
    const int cunit_len = cunit.size();
    std::string ctype("INST-VAL");
    const int ctype_len = ctype.size();
    int iplan = 0;
    int istat;
    zsrts(ifltab, cpath.c_str(), cdate, ctime.c_str(),
      &nvals, values, cunit.c_str(), ctype.c_str(), &iplan, &istat,
      cpath_len, cdate_len, ctime_len, cunit_len, ctype_len);
    zclose(ifltab);
    ++iGroup;
  }
}

std::string PTM2Postprocessor::convertIntervalToString(const int interval) const
{
  int val = interval;
  std::string result;
  if (interval >= 60) {
    val /= 60;
    if (val >= 24) {
      val /= 24;
      if (val > 7) {
        val /= 7;
        result = boost::str(boost::format("%dWEEK") % val);
      }
      else {
        result = boost::str(boost::format("%dDAY") % val);
      }
    }
    else {
      result = boost::str(boost::format("%dHOUR") % val);
    }
  }
  else {
    result = boost::str(boost::format("%dMIN") % val);
  }
  return result;
}

}
