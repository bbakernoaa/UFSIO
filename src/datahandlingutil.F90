subroutine readGridFromFile(grid, ncid, esmf_clock)
  type(ESMF_Grid), intent(out) :: grid
  integer, intent(in) :: ncid
  type(ESMF_Clock), intent(in) :: esmf_clock
  character(len=256) :: fileName
  integer :: i, rc
  character(len=256) :: lat_name, lon_name, units
  logical :: area_defined, corner_stagger_defined

  do i = 1, size(collections)
    ! Replace template with current model time
    fileName = replaceTemplate(collections(i)%file_template, collections(i)%time_format, esmf_clock)

    ! Open netCDF file (kept open)
    if (ncid == -1) then
      call ESMF_NCFileOpen(fileName, ncid, rc=rc)
      if (rc /= ESMF_SUCCESS) then
        print *, 'Error opening netCDF file: ', fileName
        stop
      end if
    end if

    ! Identify latitude and longitude variable names using netCDF API
    call identify_lat_lon_vars_netcdf(ncid, lat_name, lon_name, rc)
    if (rc /= ESMF_SUCCESS) then
      print *, 'Error identifying lat/lon variables in file: ', fileName
      stop
    end if

    ! Create grid directly from file if COARDS convention is followed
    call ESMF_GridCreateFromFile(grid, fileName, staggerloc=ESMF_STAGGERLOC_CENTER, varnames=(/lat_name, lon_name/), rc=rc)
    if (rc /= ESMF_SUCCESS) then
      print *, 'Error creating grid from file using ESMF functions'
      stop
    end if

    ! Check if area is defined
    call check_area_defined(grid, area_defined, rc)
    if (rc /= ESMF_SUCCESS) then
      print *, 'Error checking if area is defined'
      stop
    end if

    ! Add user area if not defined
    if (.not. area_defined) then
      call ESMF_GridItemAdd(grid, item=ESMF_GRIDITEM_AREA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
      if (rc /= ESMF_SUCCESS) then
        print *, 'Error adding user area to grid'
        stop
      end if
    end if

    ! Check if corner stagger locations are defined
    call check_corner_stagger_defined(grid, corner_stagger_defined, rc)
    if (rc /= ESMF_SUCCESS) then
      print *, 'Error checking if corner stagger locations are defined'
      stop
    end if

    ! Add corner stagger locations if not defined
    if (.not. corner_stagger_defined) then
      call ESMF_GridStaggerLocGridCreate(grid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=rc)
      if (rc /= ESMF_SUCCESS) then
        print *, 'Error creating corner stagger locations in grid'
        stop
      end if
    end if
  end do

contains

  subroutine identify_lat_lon_vars_netcdf(ncid, lat_name, lon_name, rc)
    integer, intent(in) :: ncid
    character(len=256), intent(out) :: lat_name, lon_name
    integer, intent(out) :: rc
    character(len=256) :: attr_val
    integer :: varid

    ! Check for latitude and longitude variables
    rc = nf90_inq_varid(ncid, 'lat', varid)
    if (rc == nf90_noerr) then
      lat_name = 'lat'
    else
      rc = nf90_inq_varid(ncid, 'latitude', varid)
      if (rc == nf90_noerr) lat_name = 'latitude'
    end if

    rc = nf90_inq_varid(ncid, 'lon', varid)
    if (rc == nf90_noerr) then
      lon_name = 'lon'
    else
      rc = nf90_inq_varid(ncid, 'longitude', varid)
      if (rc == nf90_noerr) lon_name = 'longitude'
    end if

    ! Check for units attribute to confirm COARDS convention
    call nf90_get_att_text(ncid, varid, 'units', attr_val, rc)
    if (trim(attr_val) /= 'degrees_north' .and. trim(attr_val) /= 'degrees_east') then
      rc = -1
      return
    end if

    rc = nf90_noerr
  end subroutine identify_lat_lon_vars_netcdf

  subroutine check_area_defined(grid, area_defined, rc)
    type(ESMF_Grid), intent(in) :: grid
    logical, intent(out) :: area_defined
    integer, intent(out) :: rc

    ! Check if area is defined
    call ESMF_GridItemInquire(grid, item=ESMF_GRIDITEM_AREA, staggerloc=ESMF_STAGGERLOC_CENTER, exists=area_defined, rc=rc)
    if (rc /= ESMF_SUCCESS) then
      area_defined = .false.
      rc = ESMF_ERR_ARG
    else
      rc = ESMF_SUCCESS
    end if
  end subroutine check_area_defined

  subroutine check_corner_stagger_defined(grid, corner_stagger_defined, rc)
    type(ESMF_Grid), intent(in) :: grid
    logical, intent(out) :: corner_stagger_defined
    integer, intent(out) :: rc

    ! Check if corner stagger locations are defined
    call ESMF_GridStaggerLocInquire(grid, staggerloc=ESMF_STAGGERLOC_CORNER, exists=corner_stagger_defined, rc=rc)
    if (rc /= ESMF_SUCCESS) then
      corner_stagger_defined = .false.
      rc = ESMF_ERR_ARG
    else
      rc = ESMF_SUCCESS
    end if
  end subroutine check_corner_stagger_defined

end subroutine readGridFromFile
