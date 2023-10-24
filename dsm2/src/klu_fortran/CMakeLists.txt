cmake_minimum_required(VERSION 3.21)
enable_language(C)

add_library(klu_fortran klu_fortran.c)
target_include_directories(klu_fortran PUBLIC ${KLU_INCLUDE_DIRS})
