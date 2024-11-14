cmake_minimum_required(VERSION 3.15)
project(asyncIO Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

# Find required packages
find_package(ESMF REQUIRED)
find_package(NetCDF COMPONENTS Fortran REQUIRED)

# Set source files with proper paths
# Assuming source files are in a 'src' subdirectory
set(SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/asyncIOApp.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/asyncIODriver.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/model.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/io.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/error_handling.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/IO_TypeMod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/io_utils.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/qfyaml_mod.F90
    ${CMAKE_CURRENT_SOURCE_DIR}/charpack_mod.F90
)

# Create library from the modules
add_library(asyncio_lib ${SOURCES})

# Set dependencies for the library
target_link_libraries(asyncio_lib PUBLIC
    ESMF
    NetCDF::NetCDF_Fortran
)

# Add the main executable
add_executable(asyncIOApp ${CMAKE_CURRENT_SOURCE_DIR}/asyncIOApp.F90)
target_link_libraries(asyncIOApp PRIVATE asyncio_lib)

# Add custom targets for cleaning
add_custom_target(dust
    COMMAND ${CMAKE_COMMAND} -E remove_directory PET*.ESMF_LogFile *.nc *.stdout
)

# Add custom target for running
add_custom_target(run
    COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 $<TARGET_FILE:asyncIOApp>
    DEPENDS asyncIOApp
)