# Copyright (C) 2012-2018 Paulo Meira
# This augmented KLUSolve CMakeLists.txt is based on a personal script to
# build SuiteSparse on Windows

cmake_minimum_required(VERSION 3.5 FATAL_ERROR)
project(klusolve)
SET(CMAKE_DEBUG_POSTFIX "d")
SET(CMAKE_RELEASE_PREFIX "")

SET(USE_SYSTEM_SUITESPARSE ON CACHE BOOL "Use system SuiteSparse.")

if ("${CMAKE_SIZEOF_VOID_P}" EQUAL "8")
    SET(KLUSOLVE_OUT_DIR_SUFFIX "x64")
else () 
    SET(KLUSOLVE_OUT_DIR_SUFFIX "x86")
endif ()

if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    SET(KLUSOLVE_OUT_DIR_PREFIX "win")
elseif ("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    SET(KLUSOLVE_OUT_DIR_PREFIX "darwin")
elseif ("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    SET(KLUSOLVE_OUT_DIR_PREFIX "linux")
else ()
    MESSAGE(FATAL_ERROR "Unsupported system? ${CMAKE_SYSTEM_NAME}" )
endif ()


SET(KLUSOLVE_OUT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../lib/${KLUSOLVE_OUT_DIR_PREFIX}_${KLUSOLVE_OUT_DIR_SUFFIX}")

if (USE_SYSTEM_SUITESPARSE)
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

    if (NOT MSVC)
        if (APPLE) 
            # MacOS doesn't support linking statically with system libraries
            set(CMAKE_SHARED_LINKER_FLAGS "-fPIC")
        else()
            set(CMAKE_SHARED_LINKER_FLAGS "-static-libgcc -static-libstdc++ -fPIC -Wl,--version-script,\"${CMAKE_CURRENT_SOURCE_DIR}/klusolve.map\"")
        endif()
        add_definitions(-DNCOMPLEX -fPIC)
    else()
        add_definitions(-DNCOMPLEX)
    endif()
    
    add_library(shared SHARED KLUSolve.cpp KLUSystem.cpp csz_stuff.c klusolve.def)
    target_link_libraries(shared ${KLU_LIBRARIES})
    include_directories(${SUITESPARSE_INCLUDE_DIR})

else ()
    IF (EXISTS "$ENV{SUITESPARSE_SRC}/SuiteSparse_config/SuiteSparse_config.h")
        SET(SUITESPARSE_DIR $ENV{SUITESPARSE_SRC})
        if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
            STRING(REGEX REPLACE "\\\\" "/" SUITESPARSE_DIR ${SUITESPARSE_DIR})
        endif()
    ELSE()
        IF(NOT EXISTS "${CMAKE_BINARY_DIR}/SuiteSparse/SuiteSparse_config/SuiteSparse_config.h")
            message(STATUS "Downloading SuiteSparse source code...")
            file(DOWNLOAD "http://faculty.cse.tamu.edu/davis/SuiteSparse/SuiteSparse-5.3.0.tar.gz" "${CMAKE_BINARY_DIR}/suitesparse.tar.gz")
            message(STATUS "Unpacking SuiteSparse...")
            execute_process(COMMAND ${CMAKE_COMMAND} -E tar zxf "suitesparse.tar.gz" WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
        ELSE()
            message(STATUS "SuiteSparse already exists, skipping download.")
        ENDIF()
        SET(SUITESPARSE_DIR "${CMAKE_BINARY_DIR}/SuiteSparse")
    ENDIF()
    
    
    include_directories(
        "${SUITESPARSE_DIR}/AMD/Include/"
        "${SUITESPARSE_DIR}/COLAMD/Include/"
        "${SUITESPARSE_DIR}/BTF/Include/"
        "${SUITESPARSE_DIR}/CSparse/Include/"
        "${SUITESPARSE_DIR}/KLU/Include/"
        "${SUITESPARSE_DIR}/SuiteSparse_config/"
        "${CMAKE_CURRENT_SOURCE_DIR}"
    #    "${SUITESPARSE_DIR}/CHOLMOD/Include/"
    )

    #SET(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)
    add_definitions("-DSUITESPARSE_DLLEXPORT=__declspec(dllexport)")
    set(CMAKE_DEBUG_POSTFIX "d")

    # AMD
    file(GLOB AMD_TEMPLATE_SRC "${SUITESPARSE_DIR}/AMD/Source/*.c")

    foreach(libsrc ${AMD_TEMPLATE_SRC})
        GET_FILENAME_COMPONENT(SRC "${libsrc}" NAME_WE)
        string(COMPARE NOTEQUAL "${SRC}" "amd_global" PROCESS_THIS_FILE)
        if(PROCESS_THIS_FILE)
            set(DST_I "${CMAKE_CURRENT_BINARY_DIR}/${SRC}_int.c")
            set(DST_L "${CMAKE_CURRENT_BINARY_DIR}/${SRC}_long.c")
            configure_file("${libsrc}" "${DST_I}" COPYONLY)
            configure_file("${libsrc}" "${DST_L}" COPYONLY)
            LIST(APPEND AMD_I "${DST_I}")
            LIST(APPEND AMD_L "${DST_L}")
        endif(PROCESS_THIS_FILE)
    endforeach(libsrc)

    set_source_files_properties(${AMD_I} PROPERTIES COMPILE_FLAGS "-DDINT")
    set_source_files_properties(${AMD_L} PROPERTIES COMPILE_FLAGS "-DDLONG")
    set(AMD_GLOBAL "${SUITESPARSE_DIR}/AMD/Source/amd_global.c")

    # COLAMD
    file(GLOB COLAMD_TEMPLATE_SRC "${SUITESPARSE_DIR}/COLAMD/Source/*.c")
    #list(REMOVE_ITEM COLAMD_TEMPLATE_SRC "COLAMD/Source/colamd_global.c")

    foreach(libsrc ${COLAMD_TEMPLATE_SRC})
        GET_FILENAME_COMPONENT(SRC "${libsrc}" NAME_WE)
        #string(COMPARE NOTEQUAL "${SRC}" "colamd_global" PROCESS_THIS_FILE)
        if(PROCESS_THIS_FILE)
            set(DST_I "${CMAKE_CURRENT_BINARY_DIR}/${SRC}_int.c")
            set(DST_L "${CMAKE_CURRENT_BINARY_DIR}/${SRC}_long.c")
            configure_file("${libsrc}" "${DST_I}" COPYONLY)
            configure_file("${libsrc}" "${DST_L}" COPYONLY)
            LIST(APPEND COLAMD_I "${DST_I}")
            LIST(APPEND COLAMD_L "${DST_L}")
        endif(PROCESS_THIS_FILE)
    endforeach(libsrc)

    #set_source_files_properties(${COLAMD_I} PROPERTIES COMPILE_FLAGS "-DDINT")
    set_source_files_properties(${COLAMD_L} PROPERTIES COMPILE_FLAGS "-DDLONG")
    #set(COLAMD_GLOBAL "COLAMD/Source/colamd_global.c")

    # BTF
    file(GLOB BTF_TEMPLATE_SRC "${SUITESPARSE_DIR}/BTF/Source/*.c")

    foreach(libsrc ${BTF_TEMPLATE_SRC})
        GET_FILENAME_COMPONENT(SRC "${libsrc}" NAME_WE)
        set(DST_I "${CMAKE_CURRENT_BINARY_DIR}/${SRC}_int.c")
        set(DST_L "${CMAKE_CURRENT_BINARY_DIR}/${SRC}_long.c")
        configure_file("${libsrc}" "${DST_I}" COPYONLY)
        configure_file("${libsrc}" "${DST_L}" COPYONLY)
        LIST(APPEND BTF_I "${DST_I}")
        LIST(APPEND BTF_L "${DST_L}")
    endforeach(libsrc)

    #set_source_files_properties(${BTF_I} PROPERTIES COMPILE_FLAGS "-DDINT")
    set_source_files_properties(${BTF_L} PROPERTIES COMPILE_FLAGS "-DDLONG")

    # CSparse
    file(GLOB CSPARSE_SRC "${SUITESPARSE_DIR}/CSparse/Source/*.c")
    add_definitions("-Dcsi=int")
    
    #KLU
    set(KLU_SRC "${SUITESPARSE_DIR}/KLU/Source")

    set(KLU_TEMPLATE_SRC 
        "${KLU_SRC}/klu.c"
        "${KLU_SRC}/klu_diagnostics.c"
        "${KLU_SRC}/klu_dump.c"
        "${KLU_SRC}/klu_extract.c"
        "${KLU_SRC}/klu_factor.c"
        "${KLU_SRC}/klu_kernel.c"
        "${KLU_SRC}/klu_refactor.c"
        "${KLU_SRC}/klu_scale.c"
        "${KLU_SRC}/klu_solve.c"
        "${KLU_SRC}/klu_sort.c"
        "${KLU_SRC}/klu_tsolve.c"
        "${KLU_SRC}/klu_free_numeric.c")

    foreach(libsrc ${KLU_TEMPLATE_SRC})
        GET_FILENAME_COMPONENT(SRC "${libsrc}" NAME_WE)
        # set(DST_D "${CMAKE_CURRENT_BINARY_DIR}/${SRC}_doubleint.c")
        # set(DST_DL "${CMAKE_CURRENT_BINARY_DIR}/${SRC}_doublelong.c")
        set(DST_Z "${CMAKE_CURRENT_BINARY_DIR}/${SRC}_complex.c")
        set(DST_ZL "${CMAKE_CURRENT_BINARY_DIR}/${SRC}_complexlong.c")
        # configure_file("${libsrc}" "${DST_D}" COPYONLY)
        # configure_file("${libsrc}" "${DST_DL}" COPYONLY)
        configure_file("${libsrc}" "${DST_Z}" COPYONLY)
        configure_file("${libsrc}" "${DST_ZL}" COPYONLY)
        # LIST(APPEND KLU_D "${DST_D}")
        # LIST(APPEND KLU_DL "${DST_DL}")
        LIST(APPEND KLU_Z "${DST_Z}")
        LIST(APPEND KLU_ZL "${DST_ZL}")
    endforeach(libsrc)

    set_source_files_properties(${KLU_D} PROPERTIES COMPILE_FLAGS "-DDINT")
    set_source_files_properties(${KLU_Z} PROPERTIES COMPILE_FLAGS "-DCOMPLEX -DDINT")
    set_source_files_properties(${KLU_ZL} PROPERTIES COMPILE_FLAGS "-DCOMPLEX -DDLONG")
    set_source_files_properties(${KLU_DL} PROPERTIES COMPILE_FLAGS "-DDLONG")
        
    set(KLU_COMMON_SRC 
        "${KLU_SRC}/klu_analyze.c"
        "${KLU_SRC}/klu_analyze_given.c"
        "${KLU_SRC}/klu_defaults.c"
        "${KLU_SRC}/klu_free_symbolic.c"
        "${KLU_SRC}/klu_memory.c")
        
        
    foreach(libsrc ${KLU_COMMON_SRC})
        GET_FILENAME_COMPONENT(SRC "${libsrc}" NAME_WE)
        set(DST_I "${CMAKE_CURRENT_BINARY_DIR}/${SRC}_int.c")
        set(DST_L "${CMAKE_CURRENT_BINARY_DIR}/${SRC}_long.c")
        configure_file("${libsrc}" "${DST_I}" COPYONLY)
        configure_file("${libsrc}" "${DST_L}" COPYONLY)
        LIST(APPEND KLU_I "${DST_I}")
        LIST(APPEND KLU_L "${DST_L}")
    endforeach(libsrc)

    set_source_files_properties(${KLU_L} PROPERTIES COMPILE_FLAGS "-DDLONG")
    
    if (NOT MSVC)
        if (APPLE) 
            # MacOS doesn't support linking statically with system libraries
            set(CMAKE_SHARED_LINKER_FLAGS "-fPIC")
        else()
            set(CMAKE_SHARED_LINKER_FLAGS "-static-libgcc -static-libstdc++ -fPIC -Wl,--version-script,\"${CMAKE_CURRENT_SOURCE_DIR}/klusolve.map\"")
        endif()
        add_definitions(-DNCOMPLEX -fPIC)
    else()
        add_definitions(-DNCOMPLEX)
    endif()
    
    SET(SUITESPARSE_SRC "${SUITESPARSE_DIR}/SuiteSparse_config/SuiteSparse_config.c" ${KLU_I} ${KLU_L} ${KLU_D} ${KLU_DL} ${KLU_Z} ${KLU_ZL} ${BTF_I} ${BTF_L} ${COLAMD_I} ${COLAMD_L} ${COLAMD_GLOBAL} ${AMD_I} ${AMD_L} ${AMD_GLOBAL} ${CSPARSE_SRC})
    SET(KLUSOLVE_SRC KLUSolve.cpp KLUSystem.cpp csz_stuff.c)
    add_library(shared SHARED ${KLUSOLVE_SRC} ${SUITESPARSE_SRC} klusolve.def)
    target_link_libraries(shared ${KLU_LIBRARIES})
    include_directories(
        "${SUITESPARSE_DIR}/AMD/Include"
        "${SUITESPARSE_DIR}/COLAMD/Include"
        "${SUITESPARSE_DIR}/KLU/Include"
        "${SUITESPARSE_DIR}/SuiteSparse_config/Include"
    )
endif (USE_SYSTEM_SUITESPARSE)

if (MSVC)
    set_target_properties(shared PROPERTIES OUTPUT_NAME libklusolve)
else ()
    set_target_properties(shared PROPERTIES OUTPUT_NAME klusolve)
endif ()

set_target_properties(shared PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${KLUSOLVE_OUT_DIR})
set_target_properties(shared PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${KLUSOLVE_OUT_DIR})
set_target_properties(shared PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${KLUSOLVE_OUT_DIR})

if (MSVC)
    # Force the Release output to be in the expected folder
    set_target_properties(shared PROPERTIES LIBRARY_OUTPUT_DIRECTORY_RELEASE ${KLUSOLVE_OUT_DIR})
    set_target_properties(shared PROPERTIES RUNTIME_OUTPUT_DIRECTORY_RELEASE ${KLUSOLVE_OUT_DIR})
    set_target_properties(shared PROPERTIES ARCHIVE_OUTPUT_DIRECTORY_RELEASE ${KLUSOLVE_OUT_DIR})
endif ()

