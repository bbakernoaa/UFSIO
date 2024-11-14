cmake_minimum_required(VERSION 3.15)
project(locstream_to_grid_local_pet Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

# Set source files
set(SOURCES driver_program.F90 grid_locstream_module.F90 test_grid_locstream.F90 test_driver.F90)

# Find ESMF and NetCDF
find_package(ESMF REQUIRED)
find_package(NetCDF COMPONENTS Fortran REQUIRED)

# Create a library from the module
add_library(grid_locstream_lib ${SOURCES})

# Link the library with required components
target_link_libraries(grid_locstream_lib PUBLIC
    ESMF
    NetCDF::NetCDF_Fortran
)

# Add executable targets
add_executable(driver_program driver_program.F90)
target_link_libraries(driver_program PRIVATE grid_locstream_lib)

add_executable(test_grid_locstream test_grid_locstream.F90)
target_link_libraries(test_grid_locstream PRIVATE grid_locstream_lib)

add_executable(test_driver test_driver.F90)
target_link_libraries(test_driver PRIVATE grid_locstream_lib)

# Enable testing
enable_testing()

# Add tests to CTest
add_test(NAME test_grid_locstream COMMAND test_grid_locstream)
add_test(NAME test_driver COMMAND test_driver)

# Optional: Set properties for the tests
set_tests_properties(test_grid_locstream PROPERTIES
    PASS_REGULAR_EXPRESSION "Test Summary:"
    FAIL_REGULAR_EXPRESSION "FAILED:")

set_tests_properties(test_driver PROPERTIES
    PASS_REGULAR_EXPRESSION "Test Summary:"
    FAIL_REGULAR_EXPRESSION "FAILED:")
