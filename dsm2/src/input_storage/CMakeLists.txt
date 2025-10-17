cmake_minimum_required(VERSION 3.21)
enable_language(Fortran CXX)

set(dsm2_input_storage_cpp_sources
  buffer_actions.cpp
  input_state_map.cpp
  input_storage_boundary_flow.cpp
  input_storage_boundary_stage.cpp
  input_storage_channel_ic.cpp
  input_storage_channel.cpp
  input_storage_envvar.cpp
  input_storage_gate_pipe_device.cpp
  input_storage_gate_weir_device.cpp
  input_storage_gate.cpp
  input_storage_group_member.cpp
  input_storage_group_variable_sed.cpp
  input_storage_group_variable.cpp
  input_storage_group.cpp
  input_storage_input_climate.cpp
  input_storage_input_gate.cpp
  input_storage_input_time_series.cpp
  input_storage_input_transfer_flow.cpp
  input_storage_io_file.cpp
  input_storage_node_concentration.cpp
  input_storage_operating_rule.cpp
  input_storage_oprule_expression.cpp
  input_storage_oprule_time_series.cpp
  input_storage_output_channel_source_track.cpp
  input_storage_output_channel.cpp
  input_storage_output_gate.cpp
  input_storage_output_reservoir_source_track.cpp
  input_storage_output_reservoir.cpp
  input_storage_particle_filter.cpp
  input_storage_particle_flux_output.cpp
  input_storage_particle_group_output.cpp
  input_storage_particle_insertion.cpp
  input_storage_particle_res_filter.cpp
  input_storage_rate_coefficient.cpp
  input_storage_reservoir_concentration.cpp
  input_storage_reservoir_connection.cpp
  input_storage_reservoir_ic.cpp
  input_storage_reservoir_vol.cpp
  input_storage_reservoir.cpp
  input_storage_scalar.cpp
  input_storage_source_flow_reservoir.cpp
  input_storage_source_flow.cpp
  input_storage_suspended_sediment_boundary.cpp
  input_storage_suspended_sediment_type.cpp
  input_storage_tidefile.cpp
  input_storage_transfer.cpp
  input_storage_xsect_layer.cpp
  input_storage_xsect.cpp
  text_parser.cpp)
set(dsm2_input_storage_sources input_storage_fortran.f90)

add_custom_command(
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/generate.py
  OUTPUT ${dsm2_input_storage_cpp_sources} ${dsm2_input_storage_sources}
  DEPENDS
  generate.py
  ${CMAKE_CURRENT_SOURCE_DIR}/../../../input_storage/src/template/fortran_binding_template.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/../../../input_storage/src/template/input_storage_fortran_template.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/../../../input_storage/src/generate_input_storage.py
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

add_library(dsm2_input_storage_cpp ${dsm2_input_storage_cpp_sources})
add_dependencies(dsm2_input_storage_cpp input_storage)
target_include_directories(
  dsm2_input_storage_cpp
  PUBLIC ${Boost_INCLUDE_DIRS}
  ${CMAKE_CURRENT_SOURCE_DIR}/../../../input_storage/src/
  ${HDF5_INCLUDE_DIRS})

add_library(dsm2_input_storage ${dsm2_input_storage_sources})
target_include_directories(dsm2_input_storage PRIVATE ${HDF5_Fortran_INCLUDE_DIRS})
target_link_libraries(dsm2_input_storage PUBLIC ${HDF5_LIBRARIES})
add_dependencies(dsm2_input_storage dsm2_input_storage_cpp)
