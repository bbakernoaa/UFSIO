cmake_minimum_required(VERSION 3.15)
project(asyncIO Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

# Find required packages
find_package(ESMF REQUIRED)
find_package(NetCDF COMPONENTS Fortran REQUIRED)

# Add MPI
find_package(MPI REQUIRED)  # TODO: only if ESMF is built with MPI

# Add library and main executable
add_subdirectory(src)

# Add custom target for cleaning
add_custom_target(dust
    COMMAND ${CMAKE_COMMAND} -E remove_directory PET*.ESMF_LogFile *.nc *.stdout
)

# Add custom target for running
add_custom_target(run
    COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 $<TARGET_FILE:asyncIOApp>
    DEPENDS asyncIOApp
)
