/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#ifndef _DSM2PTMInput_h_
#define _DSM2PTMInput_h_

#include "DSM2Input.h"
#include "Model.h"

namespace PTM2
{

class DSM2PTM2Input : public DSM2Input
{
  // Properties
public:
  Model& model_;

  // Methods
public:
  DSM2PTM2Input(Model& model)
    : DSM2Input(model.getGrid(), model.getParameters()), model_(model) {}
  void readInputFile(const std::string& fname);

private:
  void registerInsertions();
  void registerTidefiles();
};

}

#endif // _DSM2PTM2Input_h_
