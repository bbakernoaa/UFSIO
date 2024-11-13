module grid_locstream_module
  use ESMF
  use netcdf
  implicit none
contains
  subroutine create_grid(grid, nx, ny, lats, lons, rc)
    type(ESMF_Grid), intent(out) :: grid
    integer, intent(in) :: nx, ny
    real(ESMF_KIND_R8), dimension(:), intent(out) :: lats, lons
    integer, intent(out) :: rc
    integer :: i, j

    ! Create a target grid with 1-degree spacing
    call ESMF_GridCreate1PeriDim(grid, coordSys=ESMF_COORDSYS_SPH_DEG, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ! Allocate arrays for latitude and longitude
    allocate(lats(ny), lons(nx))
    do i = 1, ny
      lats(i) = -90.0 + (i - 1)
    end do
    do j = 1, nx
      lons(j) = -180.0 + (j - 1)
    end do

    ! Set grid coordinates
    call ESMF_GridAddCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, coordVals=lons, rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_GridSetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, coordVals=lats, rc=rc)
    if (rc /= ESMF_SUCCESS) return
  end subroutine create_grid

  subroutine create_locstream(locstream, locCount, coords, rc)
    type(ESMF_LocStream), intent(out) :: locstream
    integer, intent(in) :: locCount
    real(ESMF_KIND_R8), dimension(:,:), intent(out) :: coords
    integer, intent(out) :: rc
    integer :: i

    ! Create a LocStream with some example locations
    call ESMF_LocStreamCreate(locCount, coordSys=ESMF_COORDSYS_SPH_DEG, locstream=locstream, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ! Set example coordinates (latitude and longitude)
    allocate(coords(2, locCount))
    do i = 1, locCount
      coords(1, i) = 30.0 + i
      coords(2, i) = -90.0 + i
    end do
    call ESMF_LocStreamSet(locstream, keyword=ESMF_KEYWORD_COORD, coordDim=1, coordVal=coords(1,:), rc=rc)
    if (rc /= ESMF_SUCCESS) return
    call ESMF_LocStreamSet(locstream, keyword=ESMF_KEYWORD_COORD, coordDim=2, coordVal=coords(2,:), rc=rc)
    if (rc /= ESMF_SUCCESS) return
  end subroutine create_locstream

  subroutine map_locstream_to_grid(grid, locstream, i_coords, j_coords, rc)
    type(ESMF_Grid), intent(in) :: grid
    type(ESMF_LocStream), intent(inout) :: locstream
    integer, dimension(:), intent(out) :: i_coords, j_coords
    integer, intent(out) :: rc
    integer :: i, locCount
    real(ESMF_KIND_R8), dimension(:,:), allocatable :: coords

    ! Get the number of locations
    call ESMF_LocStreamGet(locstream, keyword=ESMF_KEYWORD_COUNT, count=locCount, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ! Allocate arrays for coordinates
    allocate(coords(2, locCount))
    allocate(i_coords(locCount))
    allocate(j_coords(locCount))

    ! Loop over all points in the LocStream and map to local PET's i, j coordinates
    do i = 1, locCount
      call ESMF_LocStreamGet(locstream, keyword=ESMF_KEYWORD_COORD, coordDim=1, coordVal=coords(1,i), rc=rc)
      if (rc /= ESMF_SUCCESS) return
      call ESMF_LocStreamGet(locstream, keyword=ESMF_KEYWORD_COORD, coordDim=2, coordVal=coords(2,i), rc=rc)
      if (rc /= ESMF_SUCCESS) return
      call ESMF_GridGetCoord(grid, gridItem=ESMF_GRIDITEM_COORD, coordDim=1, coordVal=i_coords(i), rc=rc)
      if (rc /= ESMF_SUCCESS) return
      call ESMF_GridGetCoord(grid, gridItem=ESMF_GRIDITEM_COORD, coordDim=2, coordVal=j_coords(i), rc=rc)
      if (rc /= ESMF_SUCCESS) return

      ! Store the i, j coordinates in the LocStream
      call ESMF_LocStreamSet(locstream, keyword='i_coord', coordDim=1, coordVal=i_coords(i), rc=rc)
      if (rc /= ESMF_SUCCESS) return
      call ESMF_LocStreamSet(locstream, keyword='j_coord', coordDim=2, coordVal=j_coords(i), rc=rc)
      if (rc /= ESMF_SUCCESS) return
    end do
  end subroutine map_locstream_to_grid

  subroutine write_grid_to_netcdf(filename, grid, lats, lons, rc)
    use netcdf
    character(len=*), intent(in) :: filename
    type(ESMF_Grid), intent(in) :: grid
    real(ESMF_KIND_R8), dimension(:), intent(in) :: lats, lons
    integer, intent(out) :: rc

    integer :: ncid, lat_dimid, lon_dimid
    integer :: lat_varid, lon_varid
    integer :: nx, ny, status

    ! Get grid dimensions
    nx = size(lons)
    ny = size(lats)

    ! Create netCDF file
    status = nf90_create(filename, NF90_CLOBBER, ncid)
    if (status /= nf90_noerr) then
      rc = ESMF_FAILURE
      return
    endif

    ! Define dimensions
    status = nf90_def_dim(ncid, "lat", ny, lat_dimid)
    if (status /= nf90_noerr) goto 10
    status = nf90_def_dim(ncid, "lon", nx, lon_dimid)
    if (status /= nf90_noerr) goto 10

    ! Define variables
    status = nf90_def_var(ncid, "latitude", NF90_DOUBLE, (/lat_dimid/), lat_varid)
    if (status /= nf90_noerr) goto 10
    status = nf90_def_var(ncid, "longitude", NF90_DOUBLE, (/lon_dimid/), lon_varid)
    if (status /= nf90_noerr) goto 10

    ! Add attributes
    status = nf90_put_att(ncid, lat_varid, "units", "degrees_north")
    if (status /= nf90_noerr) goto 10
    status = nf90_put_att(ncid, lat_varid, "long_name", "latitude")
    if (status /= nf90_noerr) goto 10
    status = nf90_put_att(ncid, lat_varid, "standard_name", "latitude")
    if (status /= nf90_noerr) goto 10

    status = nf90_put_att(ncid, lon_varid, "units", "degrees_east")
    if (status /= nf90_noerr) goto 10
    status = nf90_put_att(ncid, lon_varid, "long_name", "longitude")
    if (status /= nf90_noerr) goto 10
    status = nf90_put_att(ncid, lon_varid, "standard_name", "longitude")
    if (status /= nf90_noerr) goto 10

    ! End define mode
    status = nf90_enddef(ncid)
    if (status /= nf90_noerr) goto 10

    ! Write data
    status = nf90_put_var(ncid, lat_varid, lats)
    if (status /= nf90_noerr) goto 10
    status = nf90_put_var(ncid, lon_varid, lons)
    if (status /= nf90_noerr) goto 10

    ! Close file
    status = nf90_close(ncid)
    if (status /= nf90_noerr) goto 10

    rc = ESMF_SUCCESS
    return

  10 continue
    rc = ESMF_FAILURE
    return
  end subroutine write_grid_to_netcdf

  subroutine write_locstream_to_netcdf(filename, locstream, i_coords, j_coords, rc)
    use netcdf
    character(len=*), intent(in) :: filename
    type(ESMF_LocStream), intent(in) :: locstream
    integer, dimension(:), intent(in) :: i_coords, j_coords
    integer, intent(out) :: rc

    integer :: ncid, loc_dimid
    integer :: i_varid, j_varid
    integer :: locCount, status

    ! Get the number of locations
    call ESMF_LocStreamGet(locstream, keyword=ESMF_KEYWORD_COUNT, count=locCount, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ! Create netCDF file
    status = nf90_create(filename, NF90_CLOBBER, ncid)
    if (status /= nf90_noerr) then
      rc = ESMF_FAILURE
      return
    endif

    ! Define dimensions
    status = nf90_def_dim(ncid, "location", locCount, loc_dimid)
    if (status /= nf90_noerr) goto 10

    ! Define variables
    status = nf90_def_var(ncid, "i_coord", NF90_INT, (/loc_dimid/), i_varid)
    if (status /= nf90_noerr) goto 10
    status = nf90_def_var(ncid, "j_coord", NF90_INT, (/loc_dimid/), j_varid)
    if (status /= nf90_noerr) goto 10

    ! End define mode
    status = nf90_enddef(ncid)
    if (status /= nf90_noerr) goto 10

    ! Write data
    status = nf90_put_var(ncid, i_varid, i_coords)
    if (status /= nf90_noerr) goto 10
    status = nf90_put_var(ncid, j_varid, j_coords)
    if (status /= nf90_noerr) goto 10

    ! Close file
    status = nf90_close(ncid)
    if (status /= nf90_noerr) goto 10

    rc = ESMF_SUCCESS
    return

  10 continue
    rc = ESMF_FAILURE
    return
  end subroutine write_locstream_to_netcdf
end module grid_locstream_module
