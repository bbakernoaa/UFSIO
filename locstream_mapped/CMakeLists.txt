cmake_minimum_required(VERSION 3.15)
project(locstream_to_grid_local_pet Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

# Find ESMF and NCEPLIBS-ncio
find_package(ESMF REQUIRED)
find_package(NCEPLIBS-ncio REQUIRED)

# Add the executable
add_executable(driver_program driver_program.f90 grid_locstream_module.f90)

# Link the libraries
target_link_libraries(driver_program PRIVATE ESMF::ESMF NCEPLIBS::ncio)

# Enable testing
enable_testing()

# Add test executables
add_executable(test_grid_locstream test_grid_locstream.F90)
add_executable(test_driver test_driver.F90)

# Link the tests with the required modules/libraries
target_link_libraries(test_grid_locstream grid_locstream_module PUBLIC ESMF::ESMF NetCDF::NetCDF_Fortran)
target_link_libraries(test_driver grid_locstream_module driver_program PUBLIC ESMF::ESMF NetCDF::NetCDF_Fortran)

# Add tests to CTest
add_test(NAME test_grid_locstream
         COMMAND test_grid_locstream)
add_test(NAME test_driver
         COMMAND test_driver)

# Optional: Set properties for the tests
set_tests_properties(test_grid_locstream PROPERTIES
    PASS_REGULAR_EXPRESSION "Test Summary:"
    FAIL_REGULAR_EXPRESSION "FAILED:")

set_tests_properties(test_driver PROPERTIES
    PASS_REGULAR_EXPRESSION "Test Summary:"
    FAIL_REGULAR_EXPRESSION "FAILED:")