if(NOT TARGET "test-drive::test-drive")
  find_package("test-drive" REQUIRED)
endif()

macro(ADDTEST name)
  add_executable(test_${name} test_${name}.f90)
  target_link_libraries(test_${name} PRIVATE "test-drive::test-drive")
  add_test(
    NAME ${name}
    COMMAND $<TARGET_FILE:test_${name}> ${CMAKE_CURRENT_BINARY_DIR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
  if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
    set_target_properties(test_${name} PROPERTIES LINKER_LANGUAGE Fortran)
  endif()
endmacro(ADDTEST)

add_subdirectory(hydro)
