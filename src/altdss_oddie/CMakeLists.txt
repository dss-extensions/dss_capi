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

set(ALTDSS_ODDIE_LINK_ODD "" CACHE STRING "OpenDSS(Direct API) library to link at build time, if any. Keep as an empty string otherwise.")
set_property(CACHE ALTDSS_ODDIE_LINK_ODD PROPERTY STRINGS "" "OpenDSSC" "OpenDSSDirect") 

#TODO: Use FetchContent to integrate the build process

SET(EXPORT_DEF altdss_oddie_capi_EXPORTS) # For the time being, always use dllexport on Windows, even when build static libs.
add_library(altdss_oddie_capi altdss_oddie.c altdss_oddie_init.c "${CMAKE_CURRENT_SOURCE_DIR}/../../include/altdss/capi/oddie.h" altdss_oddie_private.h)
if (NOT "${ALTDSS_ODDIE_LINK_ODD}" STREQUAL "")
    message("Linking Oddie to \"${ALTDSS_ODDIE_LINK_ODD}\"")
    find_library(ODD_LIBRARY NAMES lib${ALTDSS_ODDIE_LINK_ODD}.a ${ALTDSS_ODDIE_LINK_ODD}.lib ${ALTDSS_ODDIE_LINK_ODD} HINTS "${CMAKE_PREFIX_PATH}")
    target_compile_definitions(altdss_oddie_capi PRIVATE ALTDSS_ODDIE_LINK_OPENDSSDIRECT_API ${EXPORT_DEF})
    target_link_libraries(altdss_oddie_capi "${ODD_LIBRARY}")
    if ("${ALTDSS_ODDIE_LINK_ODD}-" STREQUAL "OpenDSSC-")
        if (NOT WIN32)
            find_library(ODD_KLUSOLVE_LIBRARY NAMES klusolvex klusolve libklusolvex.a libklusolve.a libklusolvex libklusolve HINTS "${CMAKE_PREFIX_PATH}" REQUIRED)
            find_library(UUID_LIBRARY NAMES uuid REQUIRED)
            set(CMAKE_SHARED_LINKER_FLAGS "-static-libgcc -static-libstdc++")
            # find_library(ICONV_LIBRARY NAMES iconv libiconv libiconv.so.2 HINTS "${CMAKE_PREFIX_PATH}")
            message("LIBS: ${ODD_LIBRARY} ${UUID_LIBRARY} ${ODD_KLUSOLVE_LIBRARY}")
            target_link_libraries(altdss_oddie_capi c stdc++ ${UUID_LIBRARY} ${ODD_KLUSOLVE_LIBRARY})
            #TODO: linker script / hide most symbols
        endif()
    endif()
    string(TOLOWER ${ALTDSS_ODDIE_LINK_ODD} ODDIE_LIB_SUFFIX)
    set(ODDIE_LIB_SUFFIX "-${ODDIE_LIB_SUFFIX}")
else()
    set(ODDIE_LIB_SUFFIX "")
    target_compile_definitions(altdss_oddie_capi PRIVATE ${EXPORT_DEF})
endif ()
set_target_properties(altdss_oddie_capi PROPERTIES OUTPUT_NAME "altdss_oddie_capi${ODDIE_LIB_SUFFIX}")

if(MSVC)
    # We don't need .EXP files, clutter...
    set(CMAKE_SHARED_LINKER_FLAGS "/NOEXP")
endif()

include_directories("${CMAKE_CURRENT_SOURCE_DIR}/../../include/")

set_target_properties(altdss_oddie_capi PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${ALTDSS_ODDIE_OUT_DIR})
set_target_properties(altdss_oddie_capi PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${ALTDSS_ODDIE_OUT_DIR})
set_target_properties(altdss_oddie_capi PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${ALTDSS_ODDIE_OUT_DIR})
set_target_properties(altdss_oddie_capi PROPERTIES ARCHIVE_OUTPUT_DIRECTORY $<1:${ALTDSS_ODDIE_OUT_DIR}>)
set_target_properties(altdss_oddie_capi PROPERTIES LIBRARY_OUTPUT_DIRECTORY $<1:${ALTDSS_ODDIE_OUT_DIR}>)
set_target_properties(altdss_oddie_capi PROPERTIES RUNTIME_OUTPUT_DIRECTORY $<1:${ALTDSS_ODDIE_OUT_DIR}>)


install(TARGETS altdss_oddie_capi DESTINATION bin)
install(FILES ../../include/altdss/altdss_oddie.h DESTINATION include/altdss)
