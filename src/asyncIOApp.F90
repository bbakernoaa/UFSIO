!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

!> \brief Main program for the asynchronous I/O application
!> \details This is the main driver program that initializes and runs the
!> Earth System Model (ESM) with asynchronous I/O capabilities. It handles:
!> - ESMF initialization and finalization
!> - Component creation and destruction
!> - Component lifecycle management (Initialize, Run, Finalize)
!> \author ESMF Development Team
!> \date 2024
program asyncIOApp

    !-----------------------------------------------------------------------------
    ! Generic ESM application driver
    !-----------------------------------------------------------------------------

    use ESMF
    use NUOPC
    use asyncIODriver, only: driverSS => SetServices
    use error_handling, only: Check_ESMFLogErr_Final

    implicit none

    !> Return code for error handling
    integer                 :: rc, urc
    !> Main driver component
    type(ESMF_GridComp)     :: driver

    ! Initialize ESMF
    call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    call Check_ESMFLogErr_Final(rc, __LINE__, __FILE__)

    call ESMF_LogWrite("asyncIOApp STARTING", ESMF_LOGMSG_INFO, rc=rc)
    call Check_ESMFLogErr_Final(rc, __LINE__, __FILE__)

    ! Create the driver Component
    driver = ESMF_GridCompCreate(name="asyncIODriver", rc=rc)
    call Check_ESMFLogErr_Final(rc, __LINE__, __FILE__)

    ! SetServices for the earth system Component
    call ESMF_GridCompSetServices(driver, driverSS, userRc=urc, rc=rc)
    call Check_ESMFLogErr_Final(rc, __LINE__, __FILE__)
    call Check_ESMFLogErr_Final(urc, __LINE__, __FILE__)

    ! Call Initialize for the earth system Component
    call ESMF_GridCompInitialize(driver, userRc=urc, rc=rc)
    call Check_ESMFLogErr_Final(rc, __LINE__, __FILE__)
    call Check_ESMFLogErr_Final(urc, __LINE__, __FILE__)

    ! Call Run  for earth the system Component
    call ESMF_GridCompRun(driver, userRc=urc, rc=rc)
    call Check_ESMFLogErr_Final(rc, __LINE__, __FILE__)
    call Check_ESMFLogErr_Final(urc, __LINE__, __FILE__)

    ! Call Finalize for the earth system Component
    call ESMF_GridCompFinalize(driver, userRc=urc, rc=rc)
    call Check_ESMFLogErr_Final(rc, __LINE__, __FILE__)
    call Check_ESMFLogErr_Final(urc, __LINE__, __FILE__)

    ! Destroy the earth system Component
    call ESMF_GridCompDestroy(driver, rc=rc)
    call Check_ESMFLogErr_Final(rc, __LINE__, __FILE__)

    call ESMF_LogWrite("asyncIOApp FINISHED", ESMF_LOGMSG_INFO, rc=rc)
    call Check_ESMFLogErr_Final(rc, __LINE__, __FILE__)

    ! Finalize ESMF
    call ESMF_Finalize()

end program