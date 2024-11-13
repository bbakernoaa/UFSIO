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
  call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN, &
                      defaultLogFileName="esmf_driver.log", &
                      logkindflag=ESMF_LOGKIND_MULTI, &
                      rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Define grid dimensions (example: global grid with 1-degree spacing)
  nx = 360
  ny = 180
  locCount = 10

  ! Create the grid
  call create_grid(grid, nx, ny, lats, lons, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create the LocStream
  call create_locstream(locstream, locCount, coords, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Allocate arrays before mapping
  allocate(i_coords(locCount), j_coords(locCount), stat=rc)
  if (rc /= 0) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Map LocStream to grid and get i, j coordinates
  call map_locstream_to_grid(grid, locstream, i_coords, j_coords, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Write the grid to a netCDF file
  call write_grid_to_netcdf('grid.nc', grid, lats, lons, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Write the LocStream to a separate netCDF file
  call write_locstream_to_netcdf('locstream.nc', locstream, i_coords, j_coords, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Clean up allocated memory
  if (allocated(lats)) deallocate(lats)
  if (allocated(lons)) deallocate(lons)
  if (allocated(coords)) deallocate(coords)
  if (allocated(i_coords)) deallocate(i_coords)
  if (allocated(j_coords)) deallocate(j_coords)

  ! Destroy ESMF objects
  call ESMF_GridDestroy(grid, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LocStreamDestroy(locstream, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Finalize ESMF
  call ESMF_Finalize(rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

end program driver_program