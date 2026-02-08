cmake_minimum_required(VERSION 3.20)

project(altdss_capi_tools)

SET(DSS_EXTENSIONS ON CACHE BOOL "If NOT building for distribution on DSS-Extensions, disable this. It tweaks the output folders, on by default from the root CMakeLists.txt")
SET(BUILD_SHARED_LIBS ON CACHE BOOL "Build shared libs by default. Set to OFF to build static libs, where supported.")
SET(DSS_EXTENSIONS_EXAMPLES ON CACHE BOOL "Build examples from DSS-Extensions. Output to the bin folder.")
SET(BUILD_KLUSOLVEX OFF CACHE BOOL "Build KLUSolveX (otherwise, the binaries must be provided by the user).")

message(WARNING "Reminder: this currently does not build the AltDSS engine. It will be integrated in 2026.")

add_subdirectory(src/altdss_capi_loader)
add_subdirectory(src/altdss_oddie)

if (DSS_EXTENSIONS_EXAMPLES)
    add_subdirectory(examples/UserModels)
    #TODO: rest of the examples
endif()

if (BUILD_KLUSOLVEX)
	# If BUILD_KLUSOLVEX is ON and we have a local copy, use it.
	# Search order:
	# 1. Inside the source dir.
	# 2. In the parent of the source dir.
	# 3. Inside the binary dir.
	if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/klusolvex")
		# First check inside the current directory
		add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/klusolvex" "${CMAKE_CURRENT_BINARY_DIR}/klusolvex-build")
	elseif (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../klusolvex")
		# Then check in the parent directory
		add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/../klusolvex" "${CMAKE_CURRENT_BINARY_DIR}/klusolvex-build")
	elseif (EXISTS "${CMAKE_CURRENT_BINARY_DIR}/klusolvex")
		# Then check in the parent directory
		add_subdirectory("${CMAKE_CURRENT_BINARY_DIR}/klusolvex" "${CMAKE_CURRENT_BINARY_DIR}/klusolvex-build")
	else() 
		# Otherwise, grab it from GitHub
		# FetchContent is used to download the project from a specific tag
		include(FetchContent)

		FetchContent_Declare(
			klusolvex
			GIT_REPOSITORY git@github.com:dss-extensions/klusolve.git
			GIT_TAG f4723ab0d2524bda4f76f69f5f61dcf1d49bfb87
		)

		FetchContent_MakeAvailable(klusolvex)
	endif()
endif()