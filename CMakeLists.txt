## $Id$
## this is a simple CMakeLists.txt file for managing the building of DFSIM
## with help from
## http://stackoverflow.com/questions/2186110/cmake-tutorial

################################################################################
cmake_minimum_required (VERSION 2.6) 

## there's no C or C++ code in the project, but there might be in the future.
project (dfsim C CXX Fortran) 

# The version number.
SET(APP_NAME dfsim)
set (${APP_NAME}_VERSION_MAJOR 1)
set (${APP_NAME}_VERSION_MINOR 4)

##enable_language (Fortran)
add_executable (${APP_NAME} econ.for functions.for main.for subroutines.for)

SET_TARGET_PROPERTIES(${APP_NAME} PROPERTIES LINKER_LANGUAGE CXX)

add_executable (simin2 simin.for)


################################################################################
## for using with external libs.
# should we use our own math functions
## option(USE_MYMATH "Use tutorial provided math implementation" ON)
## who does this get turned on?

################################################################################
# add the install targets
install (TARGETS dfsim DESTINATION bin)

## if you have include files that need to be installed, put them into
## the lib and include dirs
##install (FILES MathFunctions.h DESTINATION include)

##install (FILES "${PROJECT_BINARY_DIR}/TutorialConfig.h"        
##         DESTINATION include)


################################################################################
## try a test with the basic TAPE3
ENABLE_TESTING()

## including dart does what again???
##INCLUDE(Dart)

## format( test-name exepath
##ADD_TEST(dfsim ${EXECUTABLE_OUTPUT_PATH}/dfsim)

## add a test called Simpletest
##ADD_TEST(SimpleTest ${CMAKE_CURRENT_BINARY_DIR}/SimpleTest Hello)

## this will create a test called builtrighttest
## test_prog is the called to generate the test outputs
## -P runs the cmake script file builtrighttest.cmake
add_test(NAME builtrighttest
    COMMAND ${CMAKE_COMMAND}
    -DTEST_PROG=$<TARGET_FILE:dfsim>
    -DSOURCEDIR=${CMAKE_CURRENT_SOURCE_DIR}
    -P ${CMAKE_CURRENT_SOURCE_DIR}/builtrighttest.cmake)




################################################################################
# build a CPack driven installer package
#include (InstallRequiredSystemLibraries)
set (CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/License.txt")
set (CPACK_PACKAGE_VERSION_MAJOR "${${APP_NAME}_VERSION_MAJOR}")
set (CPACK_PACKAGE_VERSION_MINOR "${${APP_NAME}_VERSION_MINOR}")
set (CPACK_PACKAGE_CONTACT       "jeff.hamann@forestinformatics.com")
include (CPack)


################################################################################
## create a test and upload to the public dash board.
