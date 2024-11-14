module io_utils
  use ESMF
  use netcdf
  use error_handling, only: Check_ESMFLogErr

  implicit none

  public readGridFromFile
  ! other public functions and variables

  contains

  subroutine calc_doy(year, month, day, doy)
    use io_utils, only: check_date
    implicit none

    ! Arguments
    integer, intent(in)  :: year, month, day
    integer, intent(out) :: doy

    ! Local variables
    logical :: is_leap
    integer :: days_before_month(12) = &
        [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]

    ! Calculate if it's a leap year
    is_leap = (mod(year,4) == 0 .and. mod(year,100) /= 0) .or. &
              (mod(year,400) == 0)

    ! Calculate day of year
    doy = days_before_month(month) + day

    ! Add one day after February if it's a leap year
    if (is_leap .and. month > 2) then
        doy = doy + 1
    endif

  end subroutine calc_doy

  subroutine check_date(year, month, day, rc)
    use ESMF
    implicit none

    ! Arguments
    integer, intent(in)  :: year, month, day
    integer, intent(out) :: rc

    ! Local variables
    logical :: is_leap
    integer :: days_in_month(12) = &
        [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    rc = ESMF_SUCCESS

    ! Check year range (allowing for reasonable climate data)
    if (year < 1500 .or. year > 2500) then
        rc = ESMF_FAILURE
        return
    endif

    ! Check month range
    if (month < 1 .or. month > 12) then
        rc = ESMF_FAILURE
        return
    endif

    ! Calculate if it's a leap year
    is_leap = (mod(year,4) == 0 .and. mod(year,100) /= 0) .or. &
              (mod(year,400) == 0)

    ! Adjust February for leap year
    if (is_leap) then
        days_in_month(2) = 29
    endif

    ! Check day range for the specific month
    if (day < 1 .or. day > days_in_month(month)) then
        rc = ESMF_FAILURE
        return
    endif

  end subroutine check_date

  !==============================================================================


  function replaceTemplate(template, time_format, esmf_clock) result(output)
    character(len=*), intent(in) :: template, time_format
    type(ESMF_Clock), intent(in) :: esmf_clock
    character(len=256) :: output
    character(len=4) :: year
    character(len=2) :: month, day
    integer :: rc

    call ESMF_ClockGet(ESMF_ClockGetStartTime(esmf_clock), year=year, month=month, day=day, rc=rc)
    if (rc /= ESMF_SUCCESS) then
      print *, 'Error getting date from ESMF_Clock'
      stop
    end if

    ! Replace placeholders in the time format with actual values
    output = template
    output = replace(output, "{yyyy}", year)
    output = replace(output, "{mm}", month)
    output = replace(output, "{dd}", day)
  end function replaceTemplate

end module io_utils