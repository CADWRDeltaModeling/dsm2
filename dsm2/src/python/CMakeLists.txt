cmake_minimum_required(VERSION 3.17)

# Safety net
if(PROJECT_SOURCE_DIR STREQUAL PROJECT_BINARY_DIR)
  message(
    FATAL_ERROR
      "In-source builds not allowed. Please make a new directory (called a build directory) and run CMake from there.\n"
  )
endif()

# Grab Python, 3.8  or newer
find_package(Python 3.8 REQUIRED COMPONENTS Interpreter Development NumPy)

# Grab the variables from a local Python installation F2PY headers
execute_process(
  COMMAND "${Python_EXECUTABLE}" -c
          "import numpy.f2py; print(numpy.f2py.get_include())"
  OUTPUT_VARIABLE F2PY_INCLUDE_DIR
  OUTPUT_STRIP_TRAILING_WHITESPACE)

include_directories(BEFORE ${Python_INCLUDE_DIRS} ${Python_NumPy_INCLUDE_DIRS}
                    ${F2PY_INCLUDE_DIR})

# Print out the discovered paths
include(CMakePrintHelpers)
cmake_print_variables(Python_INCLUDE_DIRS)
cmake_print_variables(Python_LIBRARY_DIRS)
cmake_print_variables(CMAKE_Fortran_MODULE_DIRECTORY)
cmake_print_variables(F2PY_INCLUDE_DIR)
cmake_print_variables(Python_NumPy_INCLUDE_DIRS)

# Common variables for hydro
set(f2py_hydro_module_name "pydsm2hydro")
set(f2py_hydro_module_c "${f2py_hydro_module_name}module.c")
set(f2py_hydro_module_def "${CMAKE_CURRENT_SOURCE_DIR}/pydsm2hydro.pyf")
set(f2py_hydro_module_wrapper ${f2py_hydro_module_name}-f2pywrappers2.f90)

# Generate
add_custom_target(hydrogenpyf DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${f2py_hydro_module_c}")

add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${f2py_hydro_module_c}"
  COMMAND
    ${Python_EXECUTABLE} ARGS -m "numpy.f2py" "${f2py_hydro_module_def}"
    -m ${f2py_hydro_module_name} --lower
    ${CMAKE_CURRENT_SOURCE_DIR}/pydsm2hydro.f90
  DEPENDS "${f2py_hydro_module_def}" # f2py definitions. See ../SourceCode/Export
                                     # generate_with_intent.py to regenerate the
                                     # definitions
)

message("Fortran Module Definition file ${f2py_hydro_module_def}")

# Setup module generation target
python_add_library(
  ${f2py_hydro_module_name} MODULE WITH_SOABI
  "${CMAKE_CURRENT_BINARY_DIR}/${f2py_hydro_module_c}" # Generated
  "${F2PY_INCLUDE_DIR}/fortranobject.c" # From NumPy
)
target_sources(${f2py_hydro_module_name}
               PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/${f2py_hydro_module_wrapper}
               ${CMAKE_CURRENT_SOURCE_DIR}/pydsm2utilities.f90
               ${CMAKE_CURRENT_SOURCE_DIR}/pydsm2hydro.f90
)

target_include_directories(${f2py_hydro_module_name}
                           PRIVATE ${CMAKE_BINARY_DIR}/modules)
# target_link_directories(${f2py_hydro_module_name} PRIVATE ${Python_LIBRARY_DIRS})
target_link_options(${f2py_hydro_module_name} PRIVATE -qmkl=sequential -static-intel)
target_link_libraries(
  ${f2py_hydro_module_name}
  PRIVATE -Wl,--start-group
          fourpt
          input_storage_cpp
          input_storage
          hdf_tidefile_fortran
          hdf_tidefile_cpp
          hydrolib
          fixed
          gtm_core
          common
          oprule_interface_fortran
          oprule_interface_cpp
          klu_fortran
          sparse
          pattern_match_cpp
          ${HDF5}
          ${HDF5_FORTRAN}
          ${HDF5_F90CSTUB}
          ${HDF5_HL}
          ${HDF5_HL_FORTRAN}
          ${HDF5_HL_F90CSTUB}
          ${SZLIB}
          ${ZLIB}
          -Wl,--end-group
          debug
          ${BOOST_FILESYSTEM_DEBUG}
          optimized
          ${BOOST_FILESYSTEM_RELEASE}
          debug
          ${BOOST_REGEX_DEBUG}
          optimized
          ${BOOST_REGEX_RELEASE}
          debug
          ${BOOST_SYSTEM_DEBUG}
          optimized
          ${BOOST_SYSTEM_RELEASE}
          debug
          ${HECLIB_DEBUG}
          optimized
          ${HECLIB_RELEASE}
          debug
          ${OPRULE_PARSER_DEBUG}
          optimized
          ${OPRULE_PARSER_RELEASE}
          debug
          ${OPRULE_DEBUG}
          optimized
          ${OPRULE_RELEASE}
          debug
          ${INPUT_STORAGE_DEBUG}
          optimized
          ${INPUT_STORAGE_RELEASE}
          debug
          ${KLULIB_DEBUG}
          optimized
          ${KLULIB_RELEASE})

add_dependencies(${f2py_hydro_module_name} fourpt hydrogenpyf)

set_target_properties(
  ${f2py_hydro_module_name} PROPERTIES PREFIX "" OUTPUT_NAME "${f2py_hydro_module_name}"
                                 LINKER_LANGUAGE C)
