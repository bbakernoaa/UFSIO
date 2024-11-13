
program driver_program
  use ESMF
  use grid_locstream_module
  implicit none

  type(ESMF_Grid) :: grid
  type(ESMF_LocStream) :: locstream
  integer :: rc, nx, ny, locCount
  integer, dimension(:), allocatable :: i_coords, j_coords
  real(ESMF_KIND_R8), dimension(:), allocatable :: lats, lons
  real(ESMF_KIND_R8), dimension(:,:), allocatable :: coords

  ! Initialize ESMF
  call ESMF_Initialize(petCount=ESMF_PET_COUNT_ANY, rc=rc)
  if (rc /= ESMF_SUCCESS) stop 'ESMF_Initialize failed'

  ! Define grid dimensions (example: global grid with 1-degree spacing)
  nx = 360
  ny = 180
  locCount = 10

  ! Create the grid
  call create_grid(grid, nx, ny, lats, lons, rc)
  if (rc /= ESMF_SUCCESS) stop 'create_grid failed'

  ! Create the LocStream
  call create_locstream(locstream, locCount, coords, rc)
  if (rc /= ESMF_SUCCESS) stop 'create_locstream failed'

  ! Map LocStream to grid and get i, j coordinates
  allocate(i_coords(locCount))
  allocate(j_coords(locCount))
  call map_locstream_to_grid(grid, locstream, i_coords, j_coords, rc)
  if (rc /= ESMF_SUCCESS) stop 'map_locstream_to_grid failed'

  ! Write the grid to a netCDF file
  call write_grid_to_netcdf('grid.nc', grid, lats, lons, rc)
  if (rc /= ESMF_SUCCESS) stop 'write_grid_to_netcdf failed'

  ! Write the LocStream to a separate netCDF file
  call write_locstream_to_netcdf('locstream.nc', locstream, i_coords, j_coords, rc)
  if (rc /= ESMF_SUCCESS) stop 'write_locstream_to_netcdf failed'

  ! Finalize ESMF
  call ESMF_Finalize(rc=rc)
  if (rc /= ESMF_SUCCESS) stop 'ESMF_Finalize failed'

end program driver_program
