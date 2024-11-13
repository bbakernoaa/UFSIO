!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

!> \brief Module that implements an asynchronous I/O driver component
!> \details This module specializes generic ESM Component code for handling
!> asynchronous I/O operations using ESMF and NUOPC frameworks
module asyncIODriver

    !-----------------------------------------------------------------------------
    ! Code that specializes generic ESM Component code.
    !-----------------------------------------------------------------------------

    use ESMF
    use NUOPC
    use NUOPC_Driver, &
      driverSS             => SetServices

    use ModelComp, only: modelSS => SetServices
    use IOComp, only: ioSS => SetServices

    use NUOPC_Connector, only: cplSS => SetServices

    use error_handling

    implicit none

    private

    public SetServices

    !-----------------------------------------------------------------------------
    contains
    !-----------------------------------------------------------------------------
    !> \brief Initializes the driver component services
    !> \param[in]     driver  The ESMF Grid Component
    !> \param[out]    rc      Return code
    subroutine SetServices(driver, rc)
      type(ESMF_GridComp)  :: driver
      integer, intent(out) :: rc

      rc = ESMF_SUCCESS

      ! derive from NUOPC_Driver
      call NUOPC_CompDerive(driver, driverSS, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

      ! attach specializing method(s)
      call NUOPC_CompSpecialize(driver, specLabel=label_SetModelServices, &
        specRoutine=SetModelServices, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)
      call NUOPC_CompSpecialize(driver, specLabel=label_ModifyCplLists, &
        specRoutine=ModifyCplLists, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

      ! set driver verbosity
      call NUOPC_CompAttributeSet(driver, name="Verbosity", value="high", rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    end subroutine

    !-----------------------------------------------------------------------------
    !> \brief Sets up the model services and configuration
    !> \details Configures the model components, I/O components, and their connections.
    !> Also sets up the model clock and time parameters.
    !> \param[in]     driver  The ESMF Grid Component
    !> \param[out]    rc      Return code
    subroutine SetModelServices(driver, rc)
      type(ESMF_GridComp)  :: driver
      integer, intent(out) :: rc

      ! local variables
      character(len=80)             :: name
      type(ESMF_Grid)               :: grid
      type(ESMF_Field)              :: field
      type(ESMF_Time)               :: startTime
      type(ESMF_Time)               :: stopTime
      type(ESMF_TimeInterval)       :: timeStep
      type(ESMF_Clock)              :: internalClock
      type(ESMF_GridComp)           :: child
      type(ESMF_CplComp)            :: connector
      integer                       :: petCount, i
      integer, allocatable          :: petList(:)
      integer                       :: petListBounds(2)
      type(ESMF_Config)             :: config

      rc = ESMF_SUCCESS

      ! get the petCount and name
      call ESMF_GridCompGet(driver, petCount=petCount, name=name, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

      ! create and open the config
      config = ESMF_ConfigCreate(rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)
      call ESMF_ConfigLoadFile(config, "test.config", rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

      ! determine the ModelComp petList bounds
      call ESMF_ConfigGetAttribute(config, petListBounds, &
        label="model_petlist_bounds:", default=-1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      if (petListBounds(1)==-1 .or. petListBounds(2)==-1) then
        petListBounds(1) = 0
        petListBounds(2) = petCount - 1
      endif

      ! add the ModelComp
      allocate(petList(petListBounds(2)-petListBounds(1)+1))
      do i=petListBounds(1), petListBounds(2)
        petList(i-petListBounds(1)+1) = i ! PETs are 0 based
      enddo
      call NUOPC_DriverAddComp(driver, "Model", modelSS, &
        petList=petList, comp=child, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)
      call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)
      deallocate(petList)

      ! determine the IOComp petList bounds
      call ESMF_ConfigGetAttribute(config, petListBounds, &
        label="io_petlist_bounds:", default=-1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      if (petListBounds(1)==-1 .or. petListBounds(2)==-1) then
        petListBounds(1) = 0
        petListBounds(2) = petCount - 1
      endif

      ! add the IOComp
      allocate(petList(petListBounds(2)-petListBounds(1)+1))
      do i=petListBounds(1), petListBounds(2)
        petList(i-petListBounds(1)+1) = i ! PETs are 0 based
      enddo
      call NUOPC_DriverAddComp(driver, "IO", ioSS, &
        petList=petList, comp=child, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)
      call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)
      deallocate(petList)

      ! Add Connector for model2io
      call NUOPC_DriverAddComp(driver, srcCompLabel="Model", dstCompLabel="IO", &
        compSetServicesRoutine=cplSS, comp=connector, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)
      call NUOPC_CompAttributeSet(connector, name="Verbosity", value="high", &
        rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

      ! set the model clock
      call ESMF_TimeIntervalSet(timeStep, m=15, rc=rc) ! 15 minute steps
      call Check_ESMFLogErr_Final(rc, __LINE__, __FILE__)

      call ESMF_TimeSet(startTime, yy=2010, mm=6, dd=1, h=0, m=0, &
        calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
      call Check_ESMFLogErr_Final(rc, __LINE__, __FILE__)

      call ESMF_TimeSet(stopTime, yy=2010, mm=6, dd=1, h=8, m=0, &
        calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
      call Check_ESMFLogErr_Final(rc, __LINE__, __FILE__)

      internalClock = ESMF_ClockCreate(name="Application Clock", &
        timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
      call Check_ESMFLogErr_Final(rc, __LINE__, __FILE__)

      call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    end subroutine

    !-----------------------------------------------------------------------------
    !> \brief Modifies the coupling lists for component connections
    !> \details Updates the coupling specifications between components by adding
    !> remapping methods and other coupling parameters
    !> \param[in]     driver  The ESMF Grid Component
    !> \param[out]    rc      Return code
    subroutine ModifyCplLists(driver, rc)
      type(ESMF_GridComp)  :: driver
      integer, intent(out) :: rc

      ! local variables
      character(len=160)              :: msg
      type(ESMF_CplComp), pointer     :: connectorList(:)
      integer                         :: i, j, cplListSize
      character(len=160), allocatable :: cplList(:)
      character(len=160)              :: tempString

      rc = ESMF_SUCCESS

      call ESMF_LogWrite("Driver is in ModifyCplLists()", ESMF_LOGMSG_INFO, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

      nullify(connectorList)
      call NUOPC_DriverGetComp(driver, compList=connectorList, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

      write (msg,*) "Found ", size(connectorList), " Connectors."// &
        " Modifying CplList Attribute...."
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

      do i=1, size(connectorList)
        ! query the cplList for connector i
        call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
          itemCount=cplListSize, rc=rc)
        call Check_ESMFLogErr(rc, __LINE__, __FILE__)
        if (cplListSize>0) then
          allocate(cplList(cplListSize))
          call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
            valueList=cplList, rc=rc)
          call Check_ESMFLogErr(rc, __LINE__, __FILE__)
          ! go through all of the entries in the cplList and add options
          do j=1, cplListSize
            tempString = trim(cplList(j))//":REMAPMETHOD=redist"
            cplList(j) = trim(tempString)
          enddo
          ! store the modified cplList in CplList attribute of connector i
          call NUOPC_CompAttributeSet(connectorList(i), &
            name="CplList", valueList=cplList, rc=rc)
          call Check_ESMFLogErr(rc, __LINE__, __FILE__)
          deallocate(cplList)
        endif
      enddo

      deallocate(connectorList)

    end subroutine

    !-----------------------------------------------------------------------------

  end module