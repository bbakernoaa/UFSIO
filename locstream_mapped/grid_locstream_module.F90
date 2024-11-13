module grid_locstream_module
  use ESMF
  use netcdf
  implicit none
  private
  public :: create_grid, create_locstream, map_locstream_to_grid
  public :: write_grid_to_netcdf, write_locstream_to_netcdf

contains
  subroutine create_grid(grid, nx, ny, lats, lons, rc)
    type(ESMF_GridType), intent(out) :: grid
    integer, intent(in) :: nx, ny
    real(ESMF_KIND_R8), dimension(:), allocatable, intent(out) :: lats, lons
    integer, intent(out) :: rc
    integer :: i, j, stat
    integer, dimension(2) :: minIndex, maxIndex
    real(ESMF_KIND_R8), pointer :: coordX(:,:), coordY(:,:)

    ! Set the index ranges
    minIndex = (/1, 1/)
    maxIndex = (/nx, ny/)

    ! Create a regular lat-lon grid
    grid = ESMF_GridCreateNoPeriDim(minIndex=minIndex, &
                                   maxIndex=maxIndex, &
                                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                                   indexflag=ESMF_INDEX_GLOBAL, &
                                   rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ! Allocate arrays for latitude and longitude
    allocate(lats(ny), lons(nx), stat=stat)
    if (stat /= 0) then
        rc = ESMF_FAILURE
        return
    endif

    ! Initialize lat/lon values
    do i = 1, ny
        lats(i) = -90.0 + (i - 1)
    end do
    do j = 1, nx
        lons(j) = -180.0 + (j - 1)
    end do

    ! Add coordinates to the Grid
    call ESMF_GridAddCoord(grid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ! Get coordinate arrays
    call ESMF_GridGetCoord(grid, coordDim=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
                          farrayPtr=coordX, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    call ESMF_GridGetCoord(grid, coordDim=2, staggerloc=ESMF_STAGGERLOC_CENTER, &
                          farrayPtr=coordY, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ! Set coordinate values
    do i = 1, ny
        do j = 1, nx
            coordX(j,i) = lons(j)
            coordY(j,i) = lats(i)
        end do
    end do

  end subroutine create_grid

  subroutine create_locstream(locstream, locCount, coords, rc)
    type(ESMF_LocStreamType), intent(out) :: locstream
    integer, intent(in) :: locCount
    real(ESMF_KIND_R8), dimension(:,:), allocatable, intent(out) :: coords
    integer, intent(out) :: rc
    integer :: i, stat

    ! Allocate and set coordinates
    allocate(coords(2, locCount), stat=stat)
    if (stat /= 0) then
        rc = ESMF_FAILURE
        return
    endif

    ! Set coordinate values
    do i = 1, locCount
        coords(1, i) = 30.0 + i  ! longitude
        coords(2, i) = -90.0 + i ! latitude
    end do

    ! Create a LocStream
    locstream = ESMF_LocStreamCreate(minIndex=1, maxIndex=locCount, &
                                   coordSys=ESMF_COORDSYS_SPH_DEG, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ! Add coordinates to the LocStream
    call ESMF_LocStreamAddCoord(locstream)

    ! Set coordinates in the LocStream
    call ESMF_LocStreamSetCoord(locstream, coord1=coords(1,:), coord2=coords(2,:), rc=rc)
    if (rc /= ESMF_SUCCESS) return

  end subroutine create_locstream

subroutine map_locstream_to_grid(grid, locstream, i_coords, j_coords, rc)
    type(ESMF_GridType), intent(in) :: grid
    type(ESMF_LocStreamType), intent(inout) :: locstream
    integer, dimension(:), allocatable, intent(out) :: i_coords, j_coords
    integer, intent(out) :: rc
    integer :: locCount, stat
    real(ESMF_KIND_R8), dimension(:), allocatable :: lon, lat

    ! Get the number of locations
    call ESMF_LocStreamGet(locstream, localDe=0, elementCount=locCount, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ! Allocate arrays
    allocate(lon(locCount), lat(locCount), &
             i_coords(locCount), j_coords(locCount), stat=stat)
    if (stat /= 0) then
        rc = ESMF_FAILURE
        return
    endif

    ! Get coordinates from LocStream
    call ESMF_LocStreamGetCoord(locstream, coord1=lon, coord2=lat, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ! Use ESMF_GridLocate to find the grid indices
    call ESMF_GridLocate(grid, lon=lon, lat=lat, &
                        i_indx=i_coords, j_indx=j_coords, rc=rc)
    if (rc /= ESMF_SUCCESS) return

    ! Clean up temporary arrays
    deallocate(lon, lat, stat=stat)
    if (stat /= 0) then
        rc = ESMF_FAILURE
        return
    endif

end subroutine map_locstream_to_grid

  subroutine write_grid_to_netcdf(filename, grid, lats, lons, rc)
    use netcdf
    character(len=*), intent(in) :: filename
    type(ESMF_GridType), intent(in) :: grid
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
    type(ESMF_LocStreamType), intent(in) :: locstream
    integer, dimension(:), intent(in) :: i_coords, j_coords
    integer, intent(out) :: rc

    integer :: ncid, loc_dimid
    integer :: i_varid, j_varid
    integer :: locCount, status

    ! Get the number of locations
    call ESMF_LocStreamGet(locstream, count=locCount, rc=rc)  ! Remove keyword and change to count
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
