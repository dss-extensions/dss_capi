cmake_minimum_required(VERSION 3.15 FATAL_ERROR)
project(CppIndMach012)
set(CMAKE_DEBUG_POSTFIX "d")
set(CMAKE_RELEASE_PREFIX "")
SET(USE_SYSTEM_EIGEN3 ON CACHE BOOL "Use system Eigen3; set to OFF to download using CMake's FetchContent.")
SET(CMAKE_CXX_STANDARD 17)

# Handle Eigen3
# Eigen3 is a header-only library, no building necessary
if(USE_SYSTEM_EIGEN3)
    find_package(Eigen3 REQUIRED)
else()
    include(FetchContent)
    set(TARGET_EIGEN_VERSION "3.4.0")
    if(CMAKE_VERSION VERSION_GREATER_EQUAL "3.24.0")
        cmake_policy(SET CMP0135 NEW)
    endif()    
    FetchContent_Declare(Eigen3 URL "https://gitlab.com/libeigen/eigen/-/archive/${TARGET_EIGEN_VERSION}/eigen-${TARGET_EIGEN_VERSION}.tar.gz")
    FetchContent_Populate(Eigen3)
    SET(EIGEN3_INCLUDE_DIR "${eigen3_SOURCE_DIR}")
endif()

include_directories(${EIGEN3_INCLUDE_DIR})
include_directories("${CMAKE_CURRENT_SOURCE_DIR}/../../include")
add_library(CppIndMach012_AltDSS SHARED CppIndMach012.cpp)
add_library(CppIndMach012_OpenDSSv7 SHARED CppIndMach012.cpp)
add_library(CppIndMach012_OpenDSSv8v9 SHARED CppIndMach012.cpp)
add_library(CppIndMach012_OpenDSSv10 SHARED CppIndMach012.cpp)

if(MSVC)
    # We don't need .EXP files, clutter...
    set(CMAKE_SHARED_LINKER_FLAGS "/NOEXP")
endif()

add_definitions(-DALTDSS_CAPI_DLL=) # Silence warnings, we won't use AltDSS/DSS C-API functions directly anyway.

target_compile_definitions(CppIndMach012_AltDSS PUBLIC ALTDSS_USERMODEL)
target_compile_definitions(CppIndMach012_OpenDSSv7 PUBLIC OPENDSS_USERMODEL_V7)
target_compile_definitions(CppIndMach012_OpenDSSv8v9 PUBLIC)
target_compile_definitions(CppIndMach012_OpenDSSv10 PUBLIC OPENDSS_USERMODEL_V10)

