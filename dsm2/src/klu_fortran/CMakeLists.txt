cmake_minimum_required(VERSION 3.17)
project(klu_fortran LANGUAGES C)

add_library(klu_fortran klu_fortran.c)
target_include_directories(klu_fortran PUBLIC ${KLU_INCLUDE_DIRS})
