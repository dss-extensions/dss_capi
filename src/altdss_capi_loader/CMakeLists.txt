cmake_minimum_required(VERSION 3.20)

project(altdss_capi_loader)

SET(DSS_EXTENSIONS OFF CACHE BOOL "If building for distribution on DSS-Extensions, enable this. It tweaks the output folders.")

SET(CMAKE_DEBUG_POSTFIX "d")

string(FIND ${CMAKE_SYSTEM_PROCESSOR} "arm" IS_ARM_PROCESSOR)

# Select output dir to match the DSS C-API current infra. This will be updated/removed
# when DSS C-API is fully ported to C++.

if((CMAKE_SYSTEM_PROCESSOR STREQUAL "aarch64") OR (CMAKE_SYSTEM_PROCESSOR STREQUAL "arm64"))
    SET(ALTDSS_CAPI_LOADER_OUT_DIR_SUFFIX "arm64")
elseif(NOT ${IS_ARM_PROCESSOR} EQUAL -1)
    SET(ALTDSS_CAPI_LOADER_OUT_DIR_SUFFIX "arm32")
elseif ("${CMAKE_SIZEOF_VOID_P}" EQUAL "8")
    SET(ALTDSS_CAPI_LOADER_OUT_DIR_SUFFIX "x64")
else ()
    SET(ALTDSS_CAPI_LOADER_OUT_DIR_SUFFIX "x86")
endif ()

if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    SET(ALTDSS_CAPI_LOADER_OUT_DIR_PREFIX "win")
elseif ("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    MESSAGE(WARNING "The platform is currently unsupported, use it at your own risk: ${CMAKE_SYSTEM_NAME}" )
    set(CMAKE_CXX_STANDARD 11)
    SET(ALTDSS_CAPI_LOADER_OUT_DIR_PREFIX "darwin")
elseif ("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    # MESSAGE(WARNING "The platform is currently unsupported, use it at your own risk: ${CMAKE_SYSTEM_NAME}" )
    SET(ALTDSS_CAPI_LOADER_OUT_DIR_PREFIX "linux")
else ()
    MESSAGE(FATAL_ERROR "Unsupported system? ${CMAKE_SYSTEM_NAME}" )
endif ()

SET(ALTDSS_CAPI_LOADER_OUT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../lib/${ALTDSS_CAPI_LOADER_OUT_DIR_PREFIX}_${ALTDSS_CAPI_LOADER_OUT_DIR_SUFFIX}")

add_library(altdss_capi_loader altdss_capi_loader.c "${CMAKE_CURRENT_SOURCE_DIR}/../../include/altdss/capi/capi.h" "${CMAKE_CURRENT_SOURCE_DIR}/../../include/altdss/capi/common.h" "${CMAKE_CURRENT_SOURCE_DIR}/../../include/altdss/capi/enums.h")

target_compile_options(altdss_capi_loader PRIVATE "-DHAS_ALTDSS_LOADER_FUNC")
include_directories("${CMAKE_CURRENT_SOURCE_DIR}/../../include/")

set_target_properties(altdss_capi_loader PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${ALTDSS_CAPI_LOADER_OUT_DIR})
set_target_properties(altdss_capi_loader PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${ALTDSS_CAPI_LOADER_OUT_DIR})
set_target_properties(altdss_capi_loader PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${ALTDSS_CAPI_LOADER_OUT_DIR})
set_target_properties(altdss_capi_loader PROPERTIES ARCHIVE_OUTPUT_DIRECTORY $<1:${ALTDSS_CAPI_LOADER_OUT_DIR}>)
set_target_properties(altdss_capi_loader PROPERTIES LIBRARY_OUTPUT_DIRECTORY $<1:${ALTDSS_CAPI_LOADER_OUT_DIR}>)
set_target_properties(altdss_capi_loader PROPERTIES RUNTIME_OUTPUT_DIRECTORY $<1:${ALTDSS_CAPI_LOADER_OUT_DIR}>)

install(TARGETS altdss_capi_loader DESTINATION bin)
