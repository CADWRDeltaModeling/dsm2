cmake_minimum_required(VERSION 3.21)

# Safety net
if(PROJECT_SOURCE_DIR STREQUAL PROJECT_BINARY_DIR)
  message(
    FATAL_ERROR
    "In-source builds not allowed. Please make a new directory (called a build directory) and run CMake from there.\n"
  )
endif()

# Grab Python, 3.8 or newer
find_package(Python 3.8 REQUIRED COMPONENTS Interpreter Development NumPy)

function(add_cython_module module_name)
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${module_name}.c
    COMMAND ${Python_EXECUTABLE} -m cython -3 ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.pyx
    -o ${CMAKE_CURRENT_BINARY_DIR}/${module_name}.c
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.pyx  ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.pxd
    ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.h
    COMMENT "Cythonizing ${module_name}.pyx to ${module_name}.c"
  )
  python_add_library(${module_name} MODULE WITH_SOABI ${CMAKE_CURRENT_BINARY_DIR}/${module_name}.c)
  target_include_directories(${module_name}
    PRIVATE ${Python_INCLUDE_DIRS} ${Python_NumPy_INCLUDE_DIRS} ${CMAKE_CURRENT_SOURCE_DIR})
endfunction()

# Define a macro to safely add debug/optimized library pairs
macro(add_library_pair TARGET DEBUG_LIB RELEASE_LIB)
if(DEFINED ${DEBUG_LIB} AND NOT "${${DEBUG_LIB}}" STREQUAL "")
target_link_libraries(${TARGET} PRIVATE debug ${${DEBUG_LIB}})
endif()
if(DEFINED ${RELEASE_LIB} AND NOT "${${RELEASE_LIB}}" STREQUAL "")
target_link_libraries(${TARGET} PRIVATE optimized ${${RELEASE_LIB}})
endif()
endmacro()

# Common variables for hydro
set(HYDRO_PYMODULE_NAME "pydsm2hydro")
add_cython_module(${HYDRO_PYMODULE_NAME} "${HYDRO_PYMODULE_NAME}.pyx")

# Link with the common libraries first
target_link_libraries(
  ${HYDRO_PYMODULE_NAME}
  PRIVATE
  fourpt
  hydrolib
  fixed
  hdf_tidefile
  hdf_tidefile_cpp
  oprule_interface_cpp
  oprule_interface
  gtm_main
  process_io
  transport
  gtm_core
  dsm2_input_storage
  dsm2_input_storage_cpp
  klu_fortran
  common
  pattern_match
  input_storage
  oprule_parser
  oprule
  ${Boost_LIBRARIES}
  ${HDF5_HL_LIBRARIES}
  ${HDF5_LIBRARIES}
  ${KLU_LIBRARIES}
  ${HEClib_LIBRARIES}
)
target_link_libraries(${HYDRO_PYMODULE_NAME} PRIVATE "$<LINK_ONLY:MKL::MKL>")
