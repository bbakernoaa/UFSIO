cmake_minimum_required(VERSION 3.15)
project(locstream_to_grid_local_pet Fortran)

# Find ESMF and NCEPLIBS-ncio
find_package(ESMF REQUIRED)
find_package(NCEPLIBS-ncio REQUIRED)

# Add the executable
add_executable(driver_program driver_program.f90 grid_locstream_module.f90)

# Link the libraries
target_link_libraries(driver_program PRIVATE ESMF NCEPLIBS::ncio)
