!> @module error_handling
!> @brief Module for ESMF error handling and logging functionality
module error_handling
  use ESMF

  implicit none

contains

  !> @brief Checks ESMF error status and logs error information if found
  !> @details This subroutine checks the return code from ESMF operations and logs
  !>          error information using ESMF's logging system if an error is detected.
  !> @param[in] rc   Return code from ESMF operation
  !> @param[in] line Line number where the error occurred
  !> @param[in] file Source file name where the error occurred
  subroutine Check_ESMFLogErr(rc, line, file)
    integer, intent(in) :: rc
    integer, intent(in) :: line
    character(len=*), intent(in) :: file

    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=line, file=file)) then
      return
    end if
  end subroutine Check_ESMFLogErr

  !> @brief Checks ESMF error status and terminates execution if error is found
  !> @details This subroutine checks the return code from ESMF operations and if an
  !>          error is detected, logs the error information and terminates the
  !>          program execution using ESMF_Finalize with abort flag.
  !> @param[in] rc   Return code from ESMF operation
  !> @param[in] line Line number where the error occurred
  !> @param[in] file Source file name where the error occurred
  subroutine Check_ESMFLogErr_Final(rc, line, file)
    integer, intent(in) :: rc
    integer, intent(in) :: line
    character(len=*), intent(in) :: file

    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=line, file=file)) then
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    end if
  end subroutine Check_ESMFLogErr_Final

end module error_handling