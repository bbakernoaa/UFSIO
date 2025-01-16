!==============================================================================
! Earth System Modeling Framework
! Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

!> \module IOComp
!> \brief IO Component module implementing NUOPC Model component
!> \details This module provides an implementation of a NUOPC Model component
!>          that handles IO operations within the ESMF framework.
module IOComp

  !-----------------------------------------------------------------------------
  ! IO Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS    => SetServices
  use error_handling, only: Check_ESMFLogErr
  use io_utils
  use IO_TypeMod

  implicit none

  private

  public SetServices

  type(IOConfig), save :: io_config

contains

  !> \brief Initialize and set up the IO component services
  !> \details Sets up the component by deriving from NUOPC_Model and specializing
  !>          it with specific routines for advertising, realizing, and advancing
  !> \param[inout] model The ESMF_GridComp object to be set up
  !> \param[out] rc Return code; equals ESMF_SUCCESS if there are no errors
  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Model
    call NUOPC_CompDerive(model, modelSS, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    ! specialize model
    call NUOPC_CompSpecialize(model, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, &
      specRoutine=RealizeProvided, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    call NUOPC_CompSpecialize(model, specLabel=label_AcceptTransfer, &
      specRoutine=AcceptTransfer, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    call NUOPC_CompSpecialize(model, specLabel=label_RealizeAccepted, &
      specRoutine=RealizeAccepted, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=Advance, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

  end subroutine

  !> \brief Advertise fields that can be imported/exported  by the IO component
  !> \details Advertises the fields that this component can import,
  !>          specifically the air_pressure_at_sea_level field
  !> \param[inout] model The ESMF_GridComp object
  !> \param[out] rc Return code; equals ESMF_SUCCESS if there are no errors
  subroutine Advertise(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)     :: importState    !< State containing fields to be imported from other components
    type(ESMF_State)     :: exportState    !< State containing fields to be exported to other components
    integer              :: i, j
    character(len=160)   :: msgString      !< Message string for logging


    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    ! Loop through collections and advertise all variables in exportState
    if (allocated(io_config%collections)) then
      do i = 1, size(io_config%collections)
        if (allocated(io_config%collections(i)%vars)) then
          do j = 1, size(io_config%collections(i)%vars)
            ! Advertise each variable in the export state
            call NUOPC_Advertise(exportState, &
              StandardName=io_config%collections(i)%vars(j)%name, &
              name=io_config%collections(i)%vars(j)%export_name, &
              TransferOfferGeomObject="will provide", rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) then
                write(msgString,*) "Error advertising field: ", &
                    trim(io_config%collections(i)%vars(j)%name)
                call ESMF_LogWrite(msgString, ESMF_LOGMSG_ERROR)
                return
            endif
          end do
        endif
      end do
    endif

    ! importable field: air_pressure_at_sea_level
    call NUOPC_Advertise(importState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", &
#define WITH_GRID_TRANSFER
#ifdef WITH_GRID_TRANSFER
      TransferOfferGeomObject="cannot provide", rc=rc)
#else
      TransferOfferGeomObject="will provide", rc=rc)
#endif
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

  end subroutine

  !-----------------------------------------------------------------------------
  !> \brief Realize fields that this component provides
  !> \details Creates and sets up fields that this component is responsible for providing,
  !>          including their grid objects and initial conditions
  !> \param[inout] model The ESMF_GridComp object
  !> \param[out] rc Return code; equals ESMF_SUCCESS if there are no errors
  subroutine RealizeProvided(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)     :: importState    !< State containing fields to be imported from other components
    type(ESMF_State)     :: exportState    !< State containing fields to be exported to other components

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    call checkConnectedFlagProvide(importState, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    call checkConnectedFlagProvide(exportState, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine checkConnectedFlagProvide(state, rc)
      ! Look at all of the fields in state, including in nested states. Error
      ! out if a connected field is found for which geom object must be
      ! provided here. Remove all not connected fields.

      type(ESMF_State)  :: state              ! ESMF state to be processed
      integer, optional :: rc                 ! Return code - error handling
      ! local variables
      integer                                 :: itemCount     ! Number of items in state
      integer                                 :: item         ! Loop counter for items
      character(len=80)                       :: stateName    ! Name of the ESMF state
      type(ESMF_Field)                        :: field        ! ESMF field for data handling
      character(len=80)                       :: connectedValue    ! Connection status value
      character(len=80)                       :: transferAction    ! Data transfer action
      character(len=80), allocatable          :: itemNameList(:)  ! List of item names in state
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)  ! List of item types in state
      type(ESMF_Config)                       :: config       ! ESMF configuration
      integer                                 :: gridDims(2)  ! Grid dimensions
      type(ESMF_Grid)                         :: gridIn       ! Input grid
      type(ESMF_StateIntent_Flag)             :: stateIntent  ! Intent of the state
      character(len=80)                       :: transferActionAttr ! Transfer action attribute

      if (present(rc)) rc = ESMF_SUCCESS

      call ESMF_StateGet(state, stateIntent=stateIntent, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

      if (stateIntent==ESMF_STATEINTENT_EXPORT) then
        transferActionAttr="ProducerTransferAction"
      elseif (stateIntent==ESMF_STATEINTENT_IMPORT) then
        transferActionAttr="ConsumerTransferAction"
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="The stateIntent must either be IMPORT or EXPORT here.", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return  ! bail out
      endif

      call ESMF_StateGet(state, name=stateName, nestedFlag=.true., &
        itemCount=itemCount, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

      allocate(itemNameList(itemCount), itemTypeList(itemCount))

      call ESMF_StateGet(state, nestedFlag=.true., &
        itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

      do item=1, itemCount
        if (itemTypeList(item)==ESMF_STATEITEM_FIELD) then
          ! this is a field -> get more info
          call ESMF_StateGet(state, field=field, itemName=itemNameList(item), &
            rc=rc)
          call Check_ESMFLogErr(rc, __LINE__, __FILE__)
          call NUOPC_GetAttribute(field, name="Connected", &
            value=connectedValue, rc=rc)
          call Check_ESMFLogErr(rc, __LINE__, __FILE__)
          if (connectedValue=="false") then
            ! remove the field from the state
            call ESMF_StateRemove(state, itemNameList(item), rc=rc)
            call Check_ESMFLogErr(rc, __LINE__, __FILE__)
          else
            call NUOPC_GetAttribute(field, name=transferActionAttr, &
              value=transferAction, rc=rc)
            call Check_ESMFLogErr(rc, __LINE__, __FILE__)
            if (trim(transferAction)=="provide") then
              ! the Connector instructed the model to provide geom object

              ! create and open the config
              config = ESMF_ConfigCreate(rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              call ESMF_ConfigLoadFile(config, "test.config", rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)

              ! determine the grid size
              call ESMF_ConfigGetAttribute(config, gridDims, &
                label="grid_dims:", default=-1, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              if (gridDims(1)==-1 .or. gridDims(2)==-1) then
                ! default tiny grid
                gridDims(1) = 10
                gridDims(2) = 20
              endif

              ! create a Grid object for Fields
              gridIn = ESMF_GridCreate1PeriDimUfrm(maxIndex=gridDims, &
                minCornerCoord=(/0._ESMF_KIND_R8, -60._ESMF_KIND_R8/), &
                maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
                staggerLocList=(/ESMF_STAGGERLOC_CENTER/), rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)

              ! importable field: air_pressure_at_sea_level
              field = ESMF_FieldCreate(name="pmsl", grid=gridIn, &
                typekind=ESMF_TYPEKIND_R8, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              call NUOPC_Realize(importState, field=field, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)

            endif
          endif
        endif
      enddo

      deallocate(itemNameList, itemTypeList)

    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------
  !> \brief Accept and process field transfers
  !> \details Handles the acceptance of field transfers from other components,
  !>          adjusting geometric objects as needed
  !> \param[inout] model The ESMF_GridComp object
  !> \param[out] rc Return code; equals ESMF_SUCCESS if there are no errors
  subroutine AcceptTransfer(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)     :: importState    !< State containing fields to be imported from other components
    type(ESMF_State)     :: exportState    !< State containing fields to be exported to other components

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    call adjustAcceptedGeom(importState, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    call adjustAcceptedGeom(exportState, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine adjustAcceptedGeom(state, rc)
      ! Look at all of the fields in state, including in nested states. Adjust
      ! the distribution of the accepted geom object to a 1 DE/PET distribution.
      type(ESMF_State)  :: state              !< ESMF State object for data exchange
      integer, optional :: rc                 !< Return code for error handling
      ! local variables
      integer                                 :: itemCount          !< Number of items in state
      integer                                 :: item               !< Iterator for state items
      type(ESMF_Field)                        :: field              !< ESMF Field for data storage
      character(len=80)                       :: transferAction     !< Action for data transfer
      character(len=80), allocatable          :: itemNameList(:)    !< Names of items in state
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)    !< Types of items in state
      type(ESMF_GeomType_Flag)                :: geomtype           !< Geometry type flag
      type(ESMF_Grid)                         :: grid               !< ESMF Grid object
      type(ESMF_Mesh)                         :: mesh               !< ESMF Mesh object
      character(160)                          :: msgString          !< Message for logging
      type(ESMF_DistGrid)                     :: distgrid           !< Distributed grid object
      integer                                 :: dimCount           !< Number of dimensions
      integer                                 :: tileCount          !< Number of tiles
      integer, allocatable                    :: minIndexPTile(:,:) !< Minimum indices per tile
      integer, allocatable                    :: maxIndexPTile(:,:) !< Maximum indices per tile
      type(ESMF_StateIntent_Flag)             :: stateIntent        !< Intent of the state
      character(len=80)                       :: transferActionAttr !< Transfer action attribute

      if (present(rc)) rc = ESMF_SUCCESS

      call ESMF_StateGet(state, nestedFlag=.true., itemCount=itemCount, &
        stateIntent=stateIntent, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

      if (stateIntent==ESMF_STATEINTENT_EXPORT) then
        transferActionAttr="ProducerTransferAction"
      elseif (stateIntent==ESMF_STATEINTENT_IMPORT) then
        transferActionAttr="ConsumerTransferAction"
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="The stateIntent must either be IMPORT or EXPORT here.", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return  ! bail out
      endif

      allocate(itemNameList(itemCount), itemTypeList(itemCount))

      call ESMF_StateGet(state, nestedFlag=.true., &
        itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

      do item=1, itemCount
        if (itemTypeList(item)==ESMF_STATEITEM_FIELD) then
          ! this is a field -> get more info
          call ESMF_StateGet(state, field=field, itemName=itemNameList(item), &
            rc=rc)
          call Check_ESMFLogErr(rc, __LINE__, __FILE__)
          call NUOPC_GetAttribute(field, name=transferActionAttr, &
            value=transferAction, rc=rc)
          call Check_ESMFLogErr(rc, __LINE__, __FILE__)
          if (trim(transferAction)=="accept") then
            ! the Connector instructed the model to accept geom object
            ! -> find out which type geom object the field holds
            call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
            call Check_ESMFLogErr(rc, __LINE__, __FILE__)
            if (geomtype==ESMF_GEOMTYPE_GRID) then
              ! empty field holds a Grid with DistGrid
              call ESMF_FieldGet(field, grid=grid, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              ! access the DistGrid
              call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              ! Create a custom DistGrid, based on the minIndex, maxIndex of the
              ! accepted DistGrid, but with a default regDecomp for the current VM
              ! that leads to 1DE/PET.
              ! get dimCount and tileCount
              call ESMF_DistGridGet(distgrid, dimCount=dimCount, &
                tileCount=tileCount, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              ! allocate minIndexPTile and maxIndexPTile accord. to dimCount
              ! and tileCount
              allocate(minIndexPTile(dimCount, tileCount), &
                maxIndexPTile(dimCount, tileCount))
              ! get minIndex and maxIndex arrays
              call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              ! create the new DistGrid with the same minIndexPTile and
              ! maxIndexPTile, but with a default regDecompPTile
              distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              ! Create a new Grid on the new DistGrid and swap it in the Field
              grid = ESMF_GridCreate(distgrid, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              call ESMF_FieldEmptySet(field, grid=grid, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              ! local clean-up
              deallocate(minIndexPTile, maxIndexPTile)
            elseif (geomtype==ESMF_GEOMTYPE_MESH) then
              ! empty field holds a Mesh with DistGrid
              call ESMF_FieldGet(field, mesh=mesh, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              ! access the DistGrid
              call ESMF_MeshGet(mesh, elementDistgrid=distgrid, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              ! Create a custom DistGrid, based on the minIndex, maxIndex of the
              ! accepted DistGrid, but with a default regDecomp for the current
              ! VM ! that leads to 1DE/PET.
              ! get dimCount and tileCount
              call ESMF_DistGridGet(distgrid, dimCount=dimCount, &
                tileCount=tileCount, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              ! allocate minIndexPTile and maxIndexPTile according
              ! to dimCount and tileCount
              allocate(minIndexPTile(dimCount, tileCount), &
                maxIndexPTile(dimCount, tileCount))
              ! get minIndex and maxIndex arrays
              call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              ! create the new DistGrid with the same minIndexPTile and
              ! maxIndexPTile, but with a default regDecompPTile
              distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              ! Create a new Grid on the new DistGrid and swap it in the Field
              mesh = ESMF_MeshCreate(distgrid, distgrid, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              call ESMF_FieldEmptySet(field, mesh=mesh, rc=rc)
              call Check_ESMFLogErr(rc, __LINE__, __FILE__)
              ! local clean-up
              deallocate(minIndexPTile, maxIndexPTile)
            else
              call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
                msg="Unsupported geom object found in "// &
                trim(itemNameList(item)), &
                line=__LINE__, &
                file=__FILE__, &
                rcToReturn=rc)
              return ! bail out
            endif
          endif
        endif
      enddo

      deallocate(itemNameList, itemTypeList)

    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------
  !> \brief Realize accepted fields
  !> \details Completes the realization of fields that were accepted from
  !>          other components during the transfer phase
  !> \param[inout] model The ESMF_GridComp object
  !> \param[out] rc Return code; equals ESMF_SUCCESS if there are no errors
  subroutine RealizeAccepted(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState    !< NUOPC import state containing fields from other components
    type(ESMF_State)        :: exportState    !< NUOPC export state containing fields for other components

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    call realizeWithAcceptedGeom(importState, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    call realizeWithAcceptedGeom(exportState, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine realizeWithAcceptedGeom(state, rc)
      ! Look at all of the fields in state, including in nested states. Realize
      ! with the accepted and adjusted geom object.
      type(ESMF_State)  :: state              !< NUOPC state object for data exchange
      integer, optional :: rc                 !< Return code for error handling
      ! local variables
      integer                                 :: itemCount     !< Total number of items in state
      integer                                 :: item         !< Iterator for processing state items
      character(len=80), allocatable          :: itemNameList(:) !< Array of item names in state
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:) !< Array of item types in state

      if (present(rc)) rc = ESMF_SUCCESS

      ! query info about the items in the state
      call ESMF_StateGet(state, nestedFlag=.true., itemCount=itemCount, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)
      allocate(itemNameList(itemCount), itemTypeList(itemCount))
      call ESMF_StateGet(state, nestedFlag=.true., &
        itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

      ! realize all the fields in the state (geoms have been transferred)
      do item=1, itemCount
        if (itemTypeList(item)==ESMF_STATEITEM_FIELD) then
          ! realize this field
          call NUOPC_Realize(state, fieldName=itemNameList(item), rc=rc)
          call Check_ESMFLogErr(rc, __LINE__, __FILE__)
        endif
      enddo

      deallocate(itemNameList, itemTypeList)

    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------
  !> \brief Advance the model state
  !> \details Advances the model state by one time step, writing out fields
  !>          in the importState to files
  !> \param[inout] model The ESMF_GridComp object
  !> \param[out] rc Return code; equals ESMF_SUCCESS if there are no errors
  subroutine Advance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)             :: clock         ! ESMF clock for time management
    type(ESMF_State)             :: importState   ! Import state containing input fields
    type(ESMF_State)             :: exportState   ! Export state containing output fields
    type(ESMF_Time)              :: currTime      ! Current model time
    type(ESMF_TimeInterval)      :: timeStep      ! Time step interval
    type(ESMF_Field)             :: field         ! ESMF field for data storage
    type(ESMF_StateItem_Flag)    :: itemType      ! Type of item in ESMF state
    type(ESMF_RegridMethod_Flag) :: regrid_method ! Method used for regridding
    real(ESMF_KIND_R8)           :: hours_elapsed ! Hours since reference time
    character(len=ESMF_MAXSTR)   :: filename      ! Data file name
    logical                      :: file_exists   ! Flag for file existence check
    real(ESMF_KIND_R8), pointer  :: dataPtr(:,:) ! Pointer to field data array
    integer                      :: i,j           ! Loop indices
    character(len=160)           :: msgString     ! Message string for logging
    integer, save                :: slice=1       ! Time slice counter (persistent)
    character(len=5)             :: sChar         ! String for number conversion

    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState
    call ESMF_GridCompGet(model, clock=clock, importState=importState, &
      rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------->Advancing IO from: ", unit=msgString, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="---------------------> to: ", unit=msgString, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    ! Process each collection
    collection_loop: do i = 1, size(io_config%collections)
      ! Generate filename for current time
      call get_filename_for_time(io_config%collections(i)%filename_template, &
        currTime, filename, rc)
      call Check_ESMFLogErr(rc, __LINE__, __FILE__)

      ! Check if file exists
      inquire(file=trim(filename), exist=file_exists)
      if (.not. file_exists) then
        write(msgString,*) "Data file not found: ", trim(filename)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_WARNING, rc=rc)
        cycle collection_loop
      endif

      ! Create grid from file if not already created
      if (.not. associated(io_config%collections(i)%grid)) then
        ! Convert config regrid method to ESMF type
        select case(trim(io_config%collections(i)%regrid_method))
        case("bilinear")
          regrid_method = ESMF_REGRIDMETHOD_BILINEAR
        case("conserve")
          regrid_method = ESMF_REGRIDMETHOD_CONSERVE
        case("nearest")
          regrid_method = ESMF_REGRIDMETHOD_NEAREST_STOD
        case("patch")
          regrid_method = ESMF_REGRIDMETHOD_PATCH
        case default
          write(msgString,*) "Unknown regrid method: ", &
            trim(io_config%collections(i)%regrid_method), &
            ". Using bilinear."
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_WARNING, rc=rc)
          regrid_method = ESMF_REGRIDMETHOD_BILINEAR
        end select

        ! Create grid from file
        io_config%collections(i)%grid = ESMF_GridCreate( &
          filename=trim(filename), &
          fileformat=ESMF_FILEFORMAT_GRIDSPEC, &
          ! regridMethod=regrid_method, &
          rc=rc)
        call Check_ESMFLogErr(rc, __LINE__, __FILE__)

        write(msgString,*) "Created grid from file for collection: ", &
          trim(io_config%collections(i)%name), &
          " using regrid method: ", trim(io_config%collections(i)%regrid_method)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        call Check_ESMFLogErr(rc, __LINE__, __FILE__)
      endif ! grid creation

      ! Process each variable in the collection
      var_loop: do j = 1, size(io_config%collections(i)%vars)
        ! Get elapsed time in hours
        call ESMF_TimeIntervalGet(timeStep, h_r8=hours_elapsed, rc=rc)
        call Check_ESMFLogErr(rc, __LINE__, __FILE__)

        ! Check if it's time to refresh this variable
        if (mod(int(hours_elapsed), io_config%collections(i)%vars(j)%refresh_interval) == 0) then
          ! Read the field data
          call read_field_data(filename, &
            io_config%collections(i)%vars(j), &
            io_config%collections(i)%grid, &
            currTime, field, rc)
          call Check_ESMFLogErr(rc, __LINE__, __FILE__)

          ! Apply scale factor if needed
          if (io_config%collections(i)%vars(j)%scale_factor /= 1.0_ESMF_KIND_R8) then
            call apply_scale_factor(field, &
              io_config%collections(i)%vars(j)%scale_factor, rc)
            call Check_ESMFLogErr(rc, __LINE__, __FILE__)
          endif

          ! Update field in export state
          call ESMF_StateGet(exportState, &
            io_config%collections(i)%vars(j)%export_name, &
            field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) then
            ! Field doesn't exist, add it
            call ESMF_StateAdd(exportState, (/field/), rc=rc)
            call Check_ESMFLogErr(rc, __LINE__, __FILE__)
          else
            ! Field exists, update it
            call ESMF_FieldCopy(field, field, rc=rc)
            call Check_ESMFLogErr(rc, __LINE__, __FILE__)
          endif
        endif
      end do var_loop
    end do collection_loop

    ! write out the Fields in the importState
#ifdef SINGLEFILE
    call NUOPC_Write(importState, fileNamePrefix="field_", &
      timeslice=slice, overwrite=.true., relaxedFlag=.true., rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)
#else
    write (sChar,"(i5.5)") slice
    call NUOPC_Write(importState, fileNamePrefix="field_"//trim(sChar)//"_", &
      overwrite=.true., relaxedFlag=.true., rc=rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)
#endif
    ! advance the time slice counter
    slice = slice + 1

  end subroutine Advance

  subroutine get_filename_for_time(template, curr_time, filename, rc)
    character(len=*), intent(in) :: template
    type(ESMF_Time), intent(in) :: curr_time
    character(len=*), intent(out) :: filename
    integer, intent(out) :: rc

    integer :: yy, mm, dd, h, m

    call ESMF_TimeGet(curr_time, yy=yy, mm=mm, dd=dd, h=h, m=m, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU)) return

    ! Replace time tokens in template
    filename = template
    ! Add your specific filename template processing here
    ! This will depend on your filename convention
  end subroutine

  !==============================================================================
  !==============================================================================
subroutine read_field_data(filename_template, var_config, grid, curr_time, field, rc)
    use ESMF
    use NUOPC
    use netcdf
    use error_handling, only: Check_ESMFLogErr
    implicit none

    ! Arguments
    character(len=*), intent(in) :: filename_template
    type(VarConfig), intent(in) :: var_config
    type(ESMF_Grid), intent(in) :: grid
    type(ESMF_Time), intent(in) :: curr_time
    type(ESMF_Field), intent(out) :: field
    integer, intent(out) :: rc

    ! Local variables
    character(ESMF_MAXSTR) :: filename1, filename2
    type(ESMF_Field) :: field1, field2
    real(ESMF_KIND_R8) :: w1, w2
    integer :: status
    type(ESMF_Time) :: time1, time2
    real(ESMF_KIND_R8), pointer :: fptr(:,:,:,:), f1ptr(:,:,:,:), f2ptr(:,:,:,:)
    character(ESMF_MAXSTR) :: timeString1, timeString2
    integer :: ncid, varid
    logical :: file_exists
    character(len=160) :: msgString !< Message string for logging

    rc = ESMF_SUCCESS

    ! Create the output field
    field = ESMF_FieldCreate(grid, typekind=var_config%data_type, &
                            name=var_config%name, &
                            gridToFieldMap=var_config%grid_to_field_map, &
                            rc=status)
    call Check_ESMFLogErr(status, __LINE__, __FILE__)

    ! Get bracketing times and filenames based on data type
    if (var_config%isClimatology) then
        call get_clim_bracketing_times(curr_time, time1, time2, w1, w2, rc)
        filename1 = filename_template
        filename2 = filename_template
    else
        call get_data_bracketing_times(curr_time, filename_template, var_config, &
                                     time1, time2, w1, w2, filename1, filename2, rc)
    endif
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    ! Check if first file exists
    inquire(file=trim(filename1), exist=file_exists)
    if (.not. file_exists) then
        write(msgString,*) "Data file not found: ", trim(filename1)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
    endif

    ! Create temporary field for first time slice
    field1 = ESMF_FieldCreate(grid, typekind=var_config%data_type, &
                             gridToFieldMap=var_config%grid_to_field_map, &
                             rc=status)
    call Check_ESMFLogErr(status, __LINE__, __FILE__)

    ! Read first time slice
    call ESMF_TimeGet(time1, timeStringISOFrac=timeString1, rc=status)
    call Check_ESMFLogErr(status, __LINE__, __FILE__)

    ! Open first file
    status = nf90_open(filename1, NF90_NOWRITE, ncid)
    if (status /= NF90_NOERR) then
        call ESMF_LogWrite("Error opening file: "//trim(filename1), &
                          ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
    endif

    ! Check if variable exists
    status = nf90_inq_varid(ncid, trim(var_config%name), varid)
    if (status /= NF90_NOERR) then
        call ESMF_LogWrite("Variable not found in file: "// &
                          trim(var_config%name), ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
    endif

    ! Read data from first file
    call ESMF_FieldRead(field1, filename1, variableName=var_config%name, &
                       timeslice=1, rc=status)
    call Check_ESMFLogErr(status, __LINE__, __FILE__)

    if (var_config%time_interp) then
        ! Create and read second field if interpolation needed
        field2 = ESMF_FieldCreate(grid, typekind=var_config%data_type, &
                                 gridToFieldMap=var_config%grid_to_field_map, &
                                 rc=status)
        call Check_ESMFLogErr(status, __LINE__, __FILE__)

        call ESMF_TimeGet(time2, timeStringISOFrac=timeString2, rc=status)
        call Check_ESMFLogErr(status, __LINE__, __FILE__)

        ! Check if second file exists (if different from first)
        if (filename2 /= filename1) then
            inquire(file=trim(filename2), exist=file_exists)
            if (.not. file_exists) then
                write(msgString,*) "Data file not found: ", trim(filename2)
                call ESMF_LogWrite(msgString, ESMF_LOGMSG_ERROR)
                rc = ESMF_FAILURE
                return
            endif
        endif

        ! Read data from second file
        call ESMF_FieldRead(field2, filename2, variableName=var_config%name, &
                           timeslice=1, rc=status)
        call Check_ESMFLogErr(status, __LINE__, __FILE__)

        ! Get pointers for interpolation
        call ESMF_FieldGet(field, farrayPtr=fptr, rc=status)
        call Check_ESMFLogErr(status, __LINE__, __FILE__)

        call ESMF_FieldGet(field1, farrayPtr=f1ptr, rc=status)
        call Check_ESMFLogErr(status, __LINE__, __FILE__)

        call ESMF_FieldGet(field2, farrayPtr=f2ptr, rc=status)
        call Check_ESMFLogErr(status, __LINE__, __FILE__)

        ! Perform time interpolation
        fptr = w1 * f1ptr + w2 * f2ptr

        ! Clean up second field
        call ESMF_FieldDestroy(field2, rc=status)
        call Check_ESMFLogErr(status, __LINE__, __FILE__)
    else
        ! No interpolation needed, just copy the data
        call ESMF_FieldCopy(field1, field, rc=status)
        call Check_ESMFLogErr(status, __LINE__, __FILE__)
    endif

    ! Clean up first field
    call ESMF_FieldDestroy(field1, rc=status)
    call Check_ESMFLogErr(status, __LINE__, __FILE__)

    ! Close NetCDF file
    status = nf90_close(ncid)
    if (status /= NF90_NOERR) then
        rc = ESMF_FAILURE
        return
    endif

end subroutine read_field_data

  subroutine apply_scale_factor(field, scale_factor, rc)
    type(ESMF_Field), intent(inout) :: field
    real(ESMF_KIND_R8), intent(in) :: scale_factor
    integer, intent(out) :: rc

    ! Local variables
    real(ESMF_KIND_R8), pointer :: dataPtr2d(:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr3d(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr4d(:,:,:,:)
    integer :: rank
    integer :: dims(4)
    type(ESMF_TypeKind_Flag) :: typekind

    rc = ESMF_SUCCESS

    ! Get the rank of the field
    call ESMF_FieldGet(field, rank=rank, typekind=typekind, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU)) return

    ! Apply scale factor based on rank
    select case(rank)
      case(2)
        call ESMF_FieldGet(field, farrayPtr=dataPtr2d, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU)) return
        dataPtr2d = dataPtr2d * scale_factor

      case(3)
        call ESMF_FieldGet(field, farrayPtr=dataPtr3d, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU)) return
        dataPtr3d = dataPtr3d * scale_factor

      case(4)
        call ESMF_FieldGet(field, farrayPtr=dataPtr4d, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU)) return
        dataPtr4d = dataPtr4d * scale_factor

      case default
        call ESMF_LogWrite("Unsupported field rank", ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
    end select

  end subroutine apply_scale_factor

  subroutine get_data_bracketing_times(curr_time, filename_template, var_config, &
                                    time1, time2, w1, w2, filename1, filename2, rc)
    use ESMF
    use NUOPC
    use netcdf
    use error_handling, only: Check_ESMFLogErr
    implicit none

    ! Arguments
    type(ESMF_Time), intent(in)  :: curr_time
    character(len=*), intent(in) :: filename_template
    type(VarConfig), intent(in)  :: var_config
    type(ESMF_Time), intent(out) :: time1, time2
    real(ESMF_KIND_R8), intent(out) :: w1, w2
    character(len=*), intent(out) :: filename1, filename2
    integer, intent(out) :: rc

    ! Local variables
    integer :: ncid, timeid, status, numTimes
    integer :: hours1, hours2
    real(ESMF_KIND_R8) :: curr_hours
    type(ESMF_Time) :: refTime, fileTime, nextFileTime
    type(ESMF_TimeInterval) :: timeOffset
    character(len=256) :: timeUnits
    integer :: yy, mm, dd, hh, min, sec
    logical :: file_exists
    character(len=ESMF_MAXSTR) :: curr_filename

    rc = ESMF_SUCCESS

    ! Generate filename for current time
    call get_filename_for_time(filename_template, curr_time, curr_filename, rc)
    call Check_ESMFLogErr(rc, __LINE__, __FILE__)

    ! Check if current file exists
    inquire(file=trim(curr_filename), exist=file_exists)
    if (.not. file_exists) then
        ! Try previous file
        call ESMF_TimeIntervalSet(timeOffset, h=-var_config%file_interval, rc=status)
        call Check_ESMFLogErr(status, __LINE__, __FILE__)
        fileTime = curr_time + timeOffset
        call get_filename_for_time(filename_template, fileTime, filename1, rc)
        call Check_ESMFLogErr(rc, __LINE__, __FILE__)

        ! Try next file
        call ESMF_TimeIntervalSet(timeOffset, h=var_config%file_interval, rc=status)
        call Check_ESMFLogErr(status, __LINE__, __FILE__)
        nextFileTime = curr_time + timeOffset
        call get_filename_for_time(filename_template, nextFileTime, filename2, rc)
        call Check_ESMFLogErr(rc, __LINE__, __FILE__)

        ! Check if either file exists
        inquire(file=trim(filename1), exist=file_exists)
        if (.not. file_exists) then
            inquire(file=trim(filename2), exist=file_exists)
            if (.not. file_exists) then
                rc = ESMF_FAILURE
                return
            endif
        endif
    else
        filename1 = curr_filename
        filename2 = curr_filename
    endif

    ! Open first file and get time information
    status = nf90_open(filename1, NF90_NOWRITE, ncid)
    if (status /= NF90_NOERR) then
        rc = ESMF_FAILURE
        return
    endif

    ! Get time information from file
    call get_file_time_info(ncid, var_config%time_name, refTime, numTimes, rc)
    if (rc /= ESMF_SUCCESS) return

    ! Calculate hours since reference for current time
    call ESMF_TimeIntervalGet(curr_time - refTime, h_r8=curr_hours, rc=status)
    call Check_ESMFLogErr(status, __LINE__, __FILE__)

    ! Find bracketing times
    hours1 = floor(curr_hours)
    hours2 = hours1 + 1

    ! Check if second time is in next file
    if (hours2 >= numTimes) then
        status = nf90_close(ncid)
        if (status /= NF90_NOERR) then
            rc = ESMF_FAILURE
            return
        endif

        ! Open next file if different
        if (filename1 /= filename2) then
            status = nf90_open(filename2, NF90_NOWRITE, ncid)
            if (status /= NF90_NOERR) then
                rc = ESMF_FAILURE
                return
            endif
            ! Update reference time for second file
            call get_file_time_info(ncid, var_config%time_name, refTime, numTimes, rc)
            if (rc /= ESMF_SUCCESS) return
            hours2 = 0  ! First time in next file
        endif
    endif

    ! Set time1
    call ESMF_TimeIntervalSet(timeOffset, h=hours1, rc=status)
    call Check_ESMFLogErr(status, __LINE__, __FILE__)
    time1 = refTime + timeOffset

    ! Set time2
    call ESMF_TimeIntervalSet(timeOffset, h=hours2, rc=status)
    call Check_ESMFLogErr(status, __LINE__, __FILE__)
    time2 = refTime + timeOffset

    ! Calculate weights
    w1 = 1.0_ESMF_KIND_R8 - (curr_hours - real(hours1, ESMF_KIND_R8))
    w2 = curr_hours - real(hours1, ESMF_KIND_R8)

    ! Close NetCDF file
    status = nf90_close(ncid)
    if (status /= NF90_NOERR) then
        rc = ESMF_FAILURE
        return
    endif

end subroutine get_data_bracketing_times

subroutine get_clim_bracketing_times(curr_time, time1, time2, w1, w2, rc)
  use ESMF
  use error_handling, only: Check_ESMFLogErr
  implicit none

  ! Arguments
  type(ESMF_Time), intent(in)  :: curr_time
  type(ESMF_Time), intent(out) :: time1, time2
  real(ESMF_KIND_R8), intent(out) :: w1, w2
  integer, intent(out) :: rc

  ! Local variables
  integer :: yy, mm, dd, curr_doy
  integer :: time1_doy, time2_doy
  type(ESMF_Time) :: base_time
  type(ESMF_TimeInterval) :: time_offset
  real(ESMF_KIND_R8) :: curr_doy_real
  integer :: status

  rc = ESMF_SUCCESS

  ! Get current date components
  call ESMF_TimeGet(curr_time, yy=yy, mm=mm, dd=dd, rc=status)
  call Check_ESMFLogErr(status, __LINE__, __FILE__)

  ! Calculate day of year for current time
  call calc_doy(yy, mm, dd, curr_doy)
  curr_doy_real = real(curr_doy, ESMF_KIND_R8)

  ! Set base time to January 1st of current year
  call ESMF_TimeSet(base_time, yy=yy, mm=1, dd=1, rc=status)
  call Check_ESMFLogErr(status, __LINE__, __FILE__)

  ! Find bracketing climatological times
  if (curr_doy == 1) then
      ! Special case for January 1st
      time1_doy = 365  ! Use December 31st of climatology
      time2_doy = 1    ! Use January 1st of climatology
      w1 = 0.0_ESMF_KIND_R8
      w2 = 1.0_ESMF_KIND_R8
  else if (curr_doy == 365 .or. curr_doy == 366) then
      ! Special case for December 31st
      time1_doy = 365  ! Use December 31st of climatology
      time2_doy = 1    ! Use January 1st of climatology
      w1 = 1.0_ESMF_KIND_R8
      w2 = 0.0_ESMF_KIND_R8
  else
      ! Normal case
      time1_doy = curr_doy
      time2_doy = curr_doy + 1
      ! Calculate weights for linear interpolation
      w1 = 1.0_ESMF_KIND_R8
      w2 = 0.0_ESMF_KIND_R8
  endif

  ! Set time1
  call ESMF_TimeIntervalSet(time_offset, d=time1_doy-1, rc=status)
  call Check_ESMFLogErr(status, __LINE__, __FILE__)

  time1 = base_time + time_offset

  ! Set time2
  if (time2_doy == 1) then
      ! Handle wrap around to January 1st
      time2 = base_time
  else
      call ESMF_TimeIntervalSet(time_offset, d=time2_doy-1, rc=status)
      call Check_ESMFLogErr(status, __LINE__, __FILE__)

      time2 = base_time + time_offset
  endif

end subroutine get_clim_bracketing_times

!==============================================================================
subroutine get_file_time_info(ncid, time_name, refTime, numTimes, rc)
    use ESMF
    use netcdf
    use error_handling, only: Check_ESMFLogErr
    implicit none

    ! Arguments
    integer, intent(in) :: ncid                  ! NetCDF file ID
    character(len=*), intent(in) :: time_name    ! Name of time variable
    type(ESMF_Time), intent(out) :: refTime      ! Reference time from units
    integer, intent(out) :: numTimes             ! Number of time steps
    integer, intent(out) :: rc                   ! Return code

    ! Local variables
    integer :: timeid, status
    character(len=256) :: timeUnits
    integer :: yy, mm, dd, hh, min, sec
    character(len=2) :: hrstr
    integer :: unitLen, idx
    character(len=:), allocatable :: units_type

    rc = ESMF_SUCCESS

    ! Get time variable ID
    status = nf90_inq_varid(ncid, trim(time_name), timeid)
    if (status /= NF90_NOERR) then
        call ESMF_LogWrite("Could not find time variable: "//trim(time_name), &
                          ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
    endif

    ! Get number of time steps
    status = nf90_inquire_dimension(ncid, timeid, len=numTimes)
    if (status /= NF90_NOERR) then
        call ESMF_LogWrite("Error getting time dimension length", &
                          ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
    endif

    ! Get time units attribute
    status = nf90_get_att(ncid, timeid, "units", timeUnits)
    if (status /= NF90_NOERR) then
        call ESMF_LogWrite("Could not get time units attribute", &
                          ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
    endif

    ! Parse units type (hours, days, etc.)
    idx = index(timeUnits, " since")
    if (idx <= 1) then
        call ESMF_LogWrite("Invalid time units format", ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
    endif

    units_type = trim(timeUnits(1:idx-1))
    if (units_type /= "hours") then
        call ESMF_LogWrite("Unsupported time units: "//units_type, &
                          ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
    endif

    ! Parse reference time from units (format: "hours since YYYY-MM-DD HH:MM:SS")
    idx = index(timeUnits, "since") + 6

    ! Initialize optional time components
    hh = 0
    min = 0
    sec = 0

    if (index(timeUnits(idx:), ":") > 0) then
        ! Time includes HH:MM:SS
        read(timeUnits(idx:), *, iostat=status) yy, mm, dd, hrstr
        if (status == 0) then
            read(hrstr, *, iostat=status) hh
            if (status == 0 .and. index(timeUnits(idx:), ":") > 0) then
                read(timeUnits(idx:), *, iostat=status) yy, mm, dd, hh, min, sec
            endif
        endif
    else
        ! Only date provided
        read(timeUnits(idx:), *, iostat=status) yy, mm, dd
    endif

    if (status /= 0) then
        call ESMF_LogWrite("Error parsing reference time from units", &
                          ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
    endif

    ! Validate date components
    if (yy < 1500 .or. yy > 2500 .or. &
        mm < 1 .or. mm > 12 .or. &
        dd < 1 .or. dd > 31 .or. &
        hh < 0 .or. hh > 23 .or. &
        min < 0 .or. min > 59 .or. &
        sec < 0 .or. sec > 59) then
        call ESMF_LogWrite("Invalid reference date/time components", &
                          ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
    endif

    ! Create reference time
    call ESMF_TimeSet(refTime, yy=yy, mm=mm, dd=dd, h=hh, m=min, s=sec, rc=status)
    call Check_ESMFLogErr(status, __LINE__, __FILE__)

end subroutine get_file_time_info

end module IOComp