/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _TideReader_h_
#define _TideReader_h_

#include "Tide.h"
#include "TideProvider.h"
#include "H5TideFile.h"
#include "hdf5.h"
#include <string>
#include <vector>

namespace PTM2
{

/**
 * @brief Tide Data provider from Tide Files
 *
 * Julmin is julian minute based on yera 1900. This convention is
 * used in DSS file, and the way is picked for convenience.
 **/
class H5TideReader: public TideProvider
{
// Attributes
protected:
  std::vector<H5TideFile> tideFiles_;

private:
  // private member variables for private functions
  hid_t fid_;
  hid_t hydroGid_;
  hid_t hydrodataGid_;
  hid_t h5Err_;
  int julminStart_;
  int minInterval_;
  int nRecords_;

// Methods
public:
  // Empty destructor
  virtual ~H5TideReader()
  {
  }

  /**
   * @brief Add a tide file name into the list of tide files
   */
  void addTideFile(const std::string& filename,
                   const int& julminStartTime,
                   const int& julminEndTime);

  /**
   * @brief Update tide information from a tide file before the given time
   */
  void updateTideAtOrBefore(const int& julminToRead, Tide* tide);
  /**
   * @brief Update tide information from a tide file after the given time
   */
  void updateTideAfter(const int& julminToRead, Tide* tide);
  // Not used at the moment
  void retrieveTimeCoverageOfTideFiles();

private:
  void openFile(const std::string& filename);
  void closeFile();
  // This fuction gets an index of a tide time step at or before
  // the given Julian minutes.
  void calculateH5Index(long& index, const int& julminToRead);
  void readTide(const long& index, Tide* tide);
  void readChannelData(const long& index, Tide* tide);
  void readChannelStageData(const long& index, Tide* tide);
  void readChannelFlowData(const long& index, Tide* tide);
  void readChannelAreaData(const long& index, Tide* tide);
  void readChannelAverageAreaData(const long& index, Tide* tide);
  void readReservoirData(const long& index, Tide* tide);
  void readReservoirHeightData(const long& index, Tide* tide);
  void readReservoirFlowData(const long& index, Tide* tide);
  void readSourceFlowData(const long& index, Tide* tide);

  void getH5TimeStart();
  void getH5NumberOfRecords();
  void getH5TimeInterval();
  const H5TideFile& findFile(const int& t);
  bool compareTime(const H5TideFile& tidefile, const int& julmin);
};

}
#endif
