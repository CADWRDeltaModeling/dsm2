/**
 * Author: Kijin Nam, knam@water.ca.gov
 */

#ifndef _TraceOutput_h_
#define _TraceOutput_h_

#include "PTM2.h"
#include "PTM2Time.h"
#include "Particle.h"
#include "hdf5.h"
#include "TraceOutput.h"
#include <vector>

namespace PTM2
{

/**
 * @brief Trace output class
 */
class TraceOutput
{
  // Properties
private:
	enum IntConstants {
		ID, TS, WB_T_FROM, WB_FROM, WB_T_TO, WB_TO, INT_NUM_FIELDS,
    FLUSH_CHUNK = 1000, FLUSH_LIMIT = 10000
	};
//	enum RealConstants {
//	  TS, REAL_NUM_FIELDS
//	};
	hid_t fileId_;
	int nRecords_;
  // Cache
  std::vector<std::vector<int> > intCache_;
  std::vector<std::string> intFieldNames_;
//  std::vector<std::vector<Real> > realCache_;
//  std::vector<std::string> realFieldNames_;
  const PTM2Time& ptm2time_;

  // Methods
public:
  TraceOutput();
  ~TraceOutput();
  void setH5FileHandle(const hid_t fileid) { fileId_ = fileid; }
  void open(const std::string& fname);
  void close();
  void writeHeader();
  void writeRecord(const Particle& particle, 
                   const Waterbody* wbFrom,
                   const Waterbody* wbTo);
  void flush(const bool flag = false);

private:
  void clearUp();
};

}

#endif
