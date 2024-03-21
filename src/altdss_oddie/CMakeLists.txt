cmake_minimum_required(VERSION 3.20)

project(altdss_oddie)

SET(CMAKE_DEBUG_POSTFIX "d")

string(FIND ${CMAKE_SYSTEM_PROCESSOR} "arm" IS_ARM_PROCESSOR)

# Select output dir to match the DSS C-API current infra. This will be updated/removed
# when DSS C-API is fully ported to C++.

if((CMAKE_SYSTEM_PROCESSOR STREQUAL "aarch64") OR (CMAKE_SYSTEM_PROCESSOR STREQUAL "arm64"))
    SET(ALTDSS_ODDIE_OUT_DIR_SUFFIX "arm64")
elseif(NOT ${IS_ARM_PROCESSOR} EQUAL -1)
    SET(ALTDSS_ODDIE_OUT_DIR_SUFFIX "arm32")
elseif ("${CMAKE_SIZEOF_VOID_P}" EQUAL "8")
    SET(ALTDSS_ODDIE_OUT_DIR_SUFFIX "x64")
else ()
    SET(ALTDSS_ODDIE_OUT_DIR_SUFFIX "x86")
endif ()

if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    SET(ALTDSS_ODDIE_OUT_DIR_PREFIX "win")
elseif ("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    MESSAGE(WARNING "The platform is currently unsupported, use it at your own risk: ${CMAKE_SYSTEM_NAME}" )
    set(CMAKE_CXX_STANDARD 11)
    SET(ALTDSS_ODDIE_OUT_DIR_PREFIX "darwin")
elseif ("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    MESSAGE(WARNING "The platform is currently unsupported, use it at your own risk: ${CMAKE_SYSTEM_NAME}" )
    SET(ALTDSS_ODDIE_OUT_DIR_PREFIX "linux")
else ()
    MESSAGE(FATAL_ERROR "Unsupported system? ${CMAKE_SYSTEM_NAME}" )
endif ()

SET(ALTDSS_ODDIE_OUT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../lib/${ALTDSS_ODDIE_OUT_DIR_PREFIX}_${ALTDSS_ODDIE_OUT_DIR_SUFFIX}")

add_library(altdss_oddie_capi SHARED altdss_oddie.c "${CMAKE_CURRENT_SOURCE_DIR}/../../include/altdss/altdss_oddie.h" altdss_oddie_private.h)

include_directories("${CMAKE_CURRENT_SOURCE_DIR}/../../include/altdss/")

set_target_properties(altdss_oddie_capi PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${ALTDSS_ODDIE_OUT_DIR})
set_target_properties(altdss_oddie_capi PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${ALTDSS_ODDIE_OUT_DIR})
set_target_properties(altdss_oddie_capi PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${ALTDSS_ODDIE_OUT_DIR})
set_target_properties(altdss_oddie_capi PROPERTIES ARCHIVE_OUTPUT_DIRECTORY $<1:${ALTDSS_ODDIE_OUT_DIR}>)
set_target_properties(altdss_oddie_capi PROPERTIES LIBRARY_OUTPUT_DIRECTORY $<1:${ALTDSS_ODDIE_OUT_DIR}>)
set_target_properties(altdss_oddie_capi PROPERTIES RUNTIME_OUTPUT_DIRECTORY $<1:${ALTDSS_ODDIE_OUT_DIR}>)


install(TARGETS altdss_oddie_capi DESTINATION bin)
install(FILES ../../include/altdss/altdss_oddie.h DESTINATION include/altdss)
