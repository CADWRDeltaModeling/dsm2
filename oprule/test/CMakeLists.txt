# Tests for the oprule library and the DSM2 binding. See ../doc/OPRULE_TESTS_OVERVIEW.md for the coverage map.
#   oprule_smoke_tests : quick sanity checks (parser + runtime, mock model)
#   oprule_core_tests  : grammar, lexer, expression nodes, activation/transition/conflict logic (mock model)
#   oprule_dsm2_tests  : the REAL DSM2 C++ binding linked against a mock of the Fortran model, driven by the
#                        rules from the study oprule_*.inp files (needs Loki headers, found under deps/)

add_library(oprule_test_support STATIC support/MockModel.cpp support/InpReader.cpp)
target_include_directories(oprule_test_support PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/support)
target_link_libraries(oprule_test_support PUBLIC oprule_parser oprule)

add_executable(oprule_smoke_tests smoke/SmokeTests.cpp)
target_link_libraries(oprule_smoke_tests PRIVATE oprule_test_support)
add_test(NAME oprule_smoke COMMAND oprule_smoke_tests)

add_executable(oprule_core_tests core/CoreTests.cpp)
target_link_libraries(oprule_core_tests PRIVATE oprule_test_support)
add_test(NAME oprule_core COMMAND oprule_core_tests)

# ---- the HDF5 sink of the rule log (needs the HDF5 C library)
if(OPRULE_WITH_HDF5)
  add_executable(oprule_hdf5_tests hdf5/Hdf5LogTests.cpp)
  target_link_libraries(oprule_hdf5_tests PRIVATE oprule_test_support)
  add_test(NAME oprule_hdf5 COMMAND oprule_hdf5_tests)
endif()

# ---- DSM2 binding against the mock Fortran layer
set(OPRULE_LOKI_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../deps/loki-0.1.7/include/loki"
    CACHE PATH "Directory containing Loki's MultiMethods.h")
set(OPRULE_DSM2_IF_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../dsm2/src/oprule_interface")
if(EXISTS "${OPRULE_LOKI_DIR}/MultiMethods.h" AND EXISTS "${OPRULE_DSM2_IF_DIR}/dsm2_oprule_management.cpp")
  file(GLOB OPRULE_DSM2_IF_SOURCES ${OPRULE_DSM2_IF_DIR}/*.cpp)
  add_executable(oprule_dsm2_tests
    dsm2/Dsm2BindingTests.cpp
    support/Dsm2FortranMock.cpp
    support/Dsm2Harness.cpp
    ${OPRULE_DSM2_IF_SOURCES})
  target_include_directories(oprule_dsm2_tests PRIVATE ${OPRULE_DSM2_IF_DIR} ${OPRULE_LOKI_DIR})
  target_compile_definitions(oprule_dsm2_tests PRIVATE
    OPRULE_TEST_DATA_DIR="${CMAKE_CURRENT_SOURCE_DIR}/data")
  target_link_libraries(oprule_dsm2_tests PRIVATE oprule_test_support)
  add_test(NAME oprule_dsm2 COMMAND oprule_dsm2_tests)
else()
  message(STATUS "oprule: Loki or the DSM2 oprule interface sources not found; skipping oprule_dsm2_tests")
endif()
