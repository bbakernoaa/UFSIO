module io_utils
  use ESMF
  use error_handling, only: Check_ESMFLogErr  ! FIXME: unused

  implicit none

contains

  subroutine calc_doy(year, month, day, doy)
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

  function replace_time_template(template, clock) result(output)
    use Charpak_Mod, only: replace => StrRepl

    character(len=*), intent(in) :: template
    type(ESMF_Clock), intent(in) :: clock
    character(len=256) :: output

    type(ESMF_Time) :: time
    integer :: year, month, day
    character(len=4) :: s_year
    character(len=2) :: s_month, s_day
    integer :: rc

    call ESMF_ClockGet(clock, startTime=time, rc=rc)
    if (rc /= ESMF_SUCCESS) then
      print *, 'Error getting start time from ESMF_Clock'
      return
    end if

    call ESMF_TimeGet(time, yy=year, m=month, d=day, rc=rc)
    if (rc /= ESMF_SUCCESS) then
      print *, 'Error getting time info from ESMF_Time'
      return
    end if
    write (s_year, '(I4)') year
    write (s_month, '(I2)') month
    write (s_day, '(I2)') day

    ! Replace placeholders in the time format with actual values
    output = template
    call replace(output, "{yyyy}", s_year)
    call replace(output, "{mm}", s_month)
    call replace(output, "{dd}", s_day)
  end function replace_time_template

end module io_utils