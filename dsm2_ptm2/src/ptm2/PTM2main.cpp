/**
 * Author: Kijin Nam, knam@water.ca.gov
 **/

#include "Model.h"
#include "DSM2PTM2Input.h"
#include "hdf5.h"
#include <iostream>

using namespace PTM2;

int main( int argc, char* argv[] )
{
  try
  {
    if ( argc < 2 ) {
      std::cout << "Usage: " << argv[0]
      << " input_file_name" << std::endl;
      throw std::runtime_error("A user did not provided an input file.");
    }
    std::string inputFileName;
    inputFileName = argv[1];

    H5open();
    Model model;
    DSM2PTM2Input dsm2input(model);
    dsm2input.readInputFile(inputFileName);
    model.run();
    H5close();
  }
  catch (std::exception& e)
  {
    std::cerr << "Exception: " << e.what() << std::endl;
    return -1;
  }
  catch (...)
  {
    std::cerr << "Unhandled unknown exception: " << std::endl;
    return -1;
  }

  return 0;
}
