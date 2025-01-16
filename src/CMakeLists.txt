set(_sources
    asyncIODriver.F90
    model.F90
    io.F90
    error_handling.F90
    IO_TypeMod.F90
    io_utils.F90
    qfyaml_mod.F90
    charpak_mod.F90
)

# Create library from the modules
add_library(asyncio_lib ${_sources})

# Set dependencies for the library
target_link_libraries(asyncio_lib PUBLIC
    ESMF
    NetCDF::NetCDF_Fortran
)

# Add the main executable
add_executable(asyncIOApp asyncIOApp.F90)
target_link_libraries(asyncIOApp PRIVATE asyncio_lib)
