## $Id$
## this is a simple CMakeLists.txt file for managing the building of DFSIM
## with help from
## http://stackoverflow.com/questions/2186110/cmake-tutorial

cmake_minimum_required (VERSION 2.6) 
project (dfsim CXX Fortran) 

# The version number.
SET(APP_NAME dfsim)
set (${APP_NAME}_VERSION_MAJOR 1)
set (${APP_NAME}_VERSION_MINOR 4)

##enable_language (Fortran)
add_executable (${APP_NAME} econ.for functions.for main.for subroutines.for)

SET_TARGET_PROPERTIES(${APP_NAME} PROPERTIES LINKER_LANGUAGE CXX)



