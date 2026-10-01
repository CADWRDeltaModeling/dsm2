# Smoke tests: parser and rule runtime against mock model objects (header-only Boost.Test).
add_executable(oprule_smoke_tests smoke/SmokeTests.cpp)
target_include_directories(oprule_smoke_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/parser)
target_link_libraries(oprule_smoke_tests PRIVATE oprule_parser oprule)
add_test(NAME oprule_smoke COMMAND oprule_smoke_tests)
