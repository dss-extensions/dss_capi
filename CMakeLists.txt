cmake_minimum_required(VERSION 3.20)

project(altdss_capi_tools)

SET(DSS_EXTENSIONS ON CACHE BOOL "If NOT building for distribution on DSS-Extensions, disable this. It tweaks the output folders, on by default from the root CMakeLists.txt")
SET(BUILD_SHARED_LIBS ON CACHE BOOL "Build shared libs by default. Set to OFF to build static libs, where supported.")

message(WARNING "Reminder: this currently does not build the AltDSS engine. It will be integrated in 2025.")

add_subdirectory(src/altdss_capi_loader)
add_subdirectory(src/altdss_oddie)

