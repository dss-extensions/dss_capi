cmake_minimum_required(VERSION 3.5 FATAL_ERROR)
SET(CMAKE_DEBUG_POSTFIX "d")
project(klusolve)

find_path(SUITESPARSE_INCLUDE_DIR NAMES klu.h HINTS /usr/include /usr/include/suitesparse /usr/local/include /usr/local/include/suitesparse)

if (NOT SUITESPARSE_INCLUDE_DIR)
    message(FATAL_ERROR "KLU header not found. Please ensure SuiteSparse development package is properly installed.")
endif ()

find_library(SUITESPARSE_KLU_LIB NAMES klu)
find_library(SUITESPARSE_AMD_LIB NAMES amd)
find_library(SUITESPARSE_BTF_LIB NAMES btf)
find_library(SUITESPARSE_COLAMD_LIB NAMES colamd)
find_library(SUITESPARSE_CSPARSE_LIB NAMES csparse)
find_library(SUITESPARSE_CXSPARSE_LIB NAMES cxsparse)
set(KLU_LIBRARIES ${SUITESPARSE_KLU_LIB} ${SUITESPARSE_AMD_LIB} ${SUITESPARSE_BTF_LIB} ${SUITESPARSE_COLAMD_LIB} ${SUITESPARSE_CSPARSE_LIB} ${SUITESPARSE_CXSPARSE_LIB})

if (NOT SUITESPARSE_KLU_LIB)
    message(FATAL_ERROR "KLU header not found. Please ensure SuiteSparse development package is properly installed.")
endif ()

set(CMAKE_SHARED_LINKER_FLAGS "-static-libgcc -static-libstdc++ -fPIC")
add_library(shared SHARED KLUSolve.cpp KLUSystem.cpp csz_stuff.c)
add_definitions(-DNCOMPLEX -fPIC)
set_target_properties(shared PROPERTIES OUTPUT_NAME klusolve)
set_target_properties(shared PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../lib)
target_link_libraries(shared ${KLU_LIBRARIES})
include_directories(${SUITESPARSE_INCLUDE_DIR})
