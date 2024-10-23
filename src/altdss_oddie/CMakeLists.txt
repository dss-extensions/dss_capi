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
    # MESSAGE(WARNING "The platform is currently unsupported, use it at your own risk: ${CMAKE_SYSTEM_NAME}" )
    SET(ALTDSS_ODDIE_OUT_DIR_PREFIX "linux")
else ()
    MESSAGE(FATAL_ERROR "Unsupported system? ${CMAKE_SYSTEM_NAME}" )
endif ()

SET(ALTDSS_ODDIE_OUT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../lib/${ALTDSS_ODDIE_OUT_DIR_PREFIX}_${ALTDSS_ODDIE_OUT_DIR_SUFFIX}")

option(ALTDSS_ODDIE_LINK_ODD "OpenDSS(Direct API) library to link at build time, if any" "")
set_property(CACHE ALTDSS_ODDIE_LINK_ODD PROPERTY STRINGS "" "OpenDSSC" "OpenDSSDirect") 

add_library(altdss_oddie_capi SHARED altdss_oddie.c "${CMAKE_CURRENT_SOURCE_DIR}/../../include/altdss/capi/oddie.h" altdss_oddie_private.h)
if (NOT "${ALTDSS_ODDIE_LINK_ODD}" STREQUAL "")
    message("Linking ${ALTDSS_ODDIE_LINK_ODD}")
    find_library(ODD_LIBRARY NAMES lib${ALTDSS_ODDIE_LINK_ODD}.a ${ALTDSS_ODDIE_LINK_ODD}.lib ${ALTDSS_ODDIE_LINK_ODD} HINTS "${CMAKE_PREFIX_PATH}")
    target_compile_definitions(altdss_oddie_capi PRIVATE ALTDSS_ODDIE_LINK_OPENDSSDIRECT_API)
    target_link_libraries(altdss_oddie_capi ${ODD_LIBRARY})
    string(TOLOWER ${ALTDSS_ODDIE_LINK_ODD} ODDIE_LIB_SUFFIX)
    set(ODDIE_LIB_SUFFIX "-${ODDIE_LIB_SUFFIX}")
else()
    set(ODDIE_LIB_SUFFIX "")
endif ()
set_target_properties(altdss_oddie_capi PROPERTIES OUTPUT_NAME "altdss_oddie_capi${ODDIE_LIB_SUFFIX}")

include_directories("${CMAKE_CURRENT_SOURCE_DIR}/../../include/")

set_target_properties(altdss_oddie_capi PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${ALTDSS_ODDIE_OUT_DIR})
set_target_properties(altdss_oddie_capi PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${ALTDSS_ODDIE_OUT_DIR})
set_target_properties(altdss_oddie_capi PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${ALTDSS_ODDIE_OUT_DIR})
set_target_properties(altdss_oddie_capi PROPERTIES ARCHIVE_OUTPUT_DIRECTORY $<1:${ALTDSS_ODDIE_OUT_DIR}>)
set_target_properties(altdss_oddie_capi PROPERTIES LIBRARY_OUTPUT_DIRECTORY $<1:${ALTDSS_ODDIE_OUT_DIR}>)
set_target_properties(altdss_oddie_capi PROPERTIES RUNTIME_OUTPUT_DIRECTORY $<1:${ALTDSS_ODDIE_OUT_DIR}>)


install(TARGETS altdss_oddie_capi DESTINATION bin)
install(FILES ../../include/altdss/altdss_oddie.h DESTINATION include/altdss)
