module IO_TypeMod
  use ESMF
  use charpak_mod
  use QFYAML_Mod

  implicit none

  character(len=ESMF_MAXSTR) :: msgString

  ! Add new types for configuration
  !> \brief Configuration for a single variable
  type VarConfig
        character(len=ESMF_MAXSTR) :: name           ! Variable name in file
        character(len=ESMF_MAXSTR) :: export_name    ! Name in export state
        character(len=ESMF_MAXSTR) :: time_name      ! Name of time dimension
        type(ESMF_TypeKind_Flag) :: data_type        ! Data type (R4, R8, etc)
        integer, allocatable :: grid_to_field_map(:)  ! Grid to field mapping
        logical :: time_interp                        ! Whether to do time interpolation
        logical :: isClimatology                      ! Whether this is climatological data
        integer :: file_interval                      ! Hours between files
        real(ESMF_KIND_R8) :: scale_factor           ! Scale factor to apply
        integer :: refresh_interval                   ! Hours between refreshes
  end type VarConfig


  type Collection
    character(len=ESMF_MAXSTR) :: name              ! Collection name
    character(len=ESMF_MAXSTR) :: filename_template ! Template for filenames
    character(len=ESMF_MAXSTR) :: regrid_method     ! Regridding method
    type(ESMF_Grid), pointer :: grid => null()      ! Grid for this collection
    type(VarConfig), allocatable :: vars(:)         ! Variables in collection
    end type Collection

  !> \brief Top level configuration container
  type :: IOConfig
    type(InputCollection), allocatable :: collections(:)
  contains
    procedure :: init => init_config
  end type

contains

    !> \brief Read and parse YAML configuration file
    !>
    !> This subroutine reads a YAML configuration file and parses it into an IOConfig object.
    !>
    !> \param[in] config_file Path to the YAML configuration file
    !> \param[out] io_config IOConfig object to store the parsed configuration
    !> \param[out] rc Return code indicating success or failure
    subroutine read_io_config(config_file, io_config, rc)
      character(len=*), intent(in) :: config_file
      type(IOConfig), intent(out) :: io_config
      integer, intent(out) :: rc

      type(qf_yaml) :: yaml
      character(len=ESMF_MAXSTR) :: msg

      ! Initialize return code
      rc = ESMF_SUCCESS

      ! Check if file exists
      inquire(file=config_file, exist=file_exists)
      if (.not. file_exists) then
          write(msg, *) "Configuration file not found: ", trim(config_file)
          call ESMF_LogWrite(msg, ESMF_LOGMSG_ERROR)
          rc = ESMF_FAILURE
          return
      endif

      ! Initialize YAML parser
      call yaml%load_file(config_file)
      if (yaml%error() /= 0) then
          write(msg, *) "Failed to load YAML file: ", trim(config_file)
          call ESMF_LogWrite(msg, ESMF_LOGMSG_ERROR)
          rc = ESMF_FAILURE
          return
      endif

      ! Parse configuration
      call io_config%init(yaml, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU)) then
          call ESMF_LogWrite("Failed to initialize IO configuration", &
                            ESMF_LOGMSG_ERROR)
          return
      endif

      ! Cleanup
      call yaml%destroy()

    end subroutine read_io_config

    !> \brief Initialize configuration from YAML data
    !>
    !> This subroutine initializes an IOConfig object from a YAML object.
    !>
    !> \param[inout] this IOConfig object to initialize
    !> \param[in] yaml YAML object containing configuration data
    !> \param[out] rc Return code indicating success or failure
    subroutine init_config(this, yaml, rc)
      class(IOConfig), intent(inout) :: this
      type(qf_yaml), intent(in) :: yaml
      integer, intent(out) :: rc

      type(qf_yaml) :: collections, coll_item, vars, var_item
      integer :: i, j, ncoll, nvars, localrc
      character(len=:), allocatable :: temp_str
      character(len=ESMF_MAXSTR) :: msg

      rc = ESMF_SUCCESS

      ! Get collections section
      if (.not. yaml%has("inputcollections")) then
          call ESMF_LogWrite("Missing required 'inputcollections' section", &
                            ESMF_LOGMSG_ERROR)
          rc = ESMF_FAILURE
          return
      endif

      collections = yaml%get("inputcollections")
      ncoll = collections%count()

      if (ncoll < 1) then
          call ESMF_LogWrite("No input collections found in configuration", &
                            ESMF_LOGMSG_ERROR)
          rc = ESMF_FAILURE
          return
      endif

      ! Allocate collections array
      allocate(this%collections(ncoll), stat=localrc)
      if (localrc /= 0) then
          call ESMF_LogWrite("Failed to allocate collections array", &
                            ESMF_LOGMSG_ERROR)
          rc = ESMF_FAILURE
          return
      endif

      ! Process each collection
      collection_loop: do i = 1, ncoll
          coll_item = collections%get(i)

          ! Verify required collection fields
          if (.not. coll_item%has("name")) then
              write(msg, *) "Missing required 'name' for collection ", i
              call ESMF_LogWrite(msg, ESMF_LOGMSG_ERROR)
              rc = ESMF_FAILURE
              return
          endif

          if (.not. coll_item%has("file_template")) then
              write(msg, *) "Missing required 'file_template' for collection ", i
              call ESMF_LogWrite(msg, ESMF_LOGMSG_ERROR)
              rc = ESMF_FAILURE
              return
          endif

          ! Get collection metadata
          temp_str = coll_item%get("name")%to_str()
          call this%collections(i)%name%set(temp_str)

          temp_str = coll_item%get("file_template")%to_str()
          call this%collections(i)%filename_template%set(temp_str)

          ! Process variables section
          if (.not. coll_item%has("vars_to_read")) then
              write(msg, *) "Missing required 'vars_to_read' for collection ", i
              call ESMF_LogWrite(msg, ESMF_LOGMSG_ERROR)
              rc = ESMF_FAILURE
              return
          endif

          vars = coll_item%get("vars_to_read")
          nvars = vars%count()

          if (nvars < 1) then
              write(msg, *) "No variables specified for collection ", i
              call ESMF_LogWrite(msg, ESMF_LOGMSG_ERROR)
              rc = ESMF_FAILURE
              return
          endif

          ! Allocate variables array
          allocate(this%collections(i)%vars(nvars), stat=localrc)
          if (localrc /= 0) then
              write(msg, *) "Failed to allocate variables array for collection ", i
              call ESMF_LogWrite(msg, ESMF_LOGMSG_ERROR)
              rc = ESMF_FAILURE
              return
          endif

          ! Process each variable
          variable_loop: do j = 1, nvars
              var_item = vars%get(j)

              ! Verify required variable fields
              if (.not. var_item%has("name")) then
                  write(msg, *) "Missing required 'name' for variable ", j, &
                               " in collection ", i
                  call ESMF_LogWrite(msg, ESMF_LOGMSG_ERROR)
                  rc = ESMF_FAILURE
                  return
              endif

              if (.not. var_item%has("regridding_method")) then
                  write(msg, *) "Missing required 'regridding_method' for variable ", &
                               j, " in collection ", i
                  call ESMF_LogWrite(msg, ESMF_LOGMSG_ERROR)
                  rc = ESMF_FAILURE
                  return
              endif

              ! Fill variable config
              temp_str = var_item%get("name")%to_str()
              call this%collections(i)%vars(j)%name%set(temp_str)

              temp_str = var_item%get("regridding_method")%to_str()
              call this%collections(i)%vars(j)%regridding_method%set(temp_str)

              ! Required refresh_interval
              if (.not. var_item%has("refresh_interval")) then
                  write(msg, *) "Missing required 'refresh_interval' for variable ", &
                               j, " in collection ", i
                  call ESMF_LogWrite(msg, ESMF_LOGMSG_ERROR)
                  rc = ESMF_FAILURE
                  return
              endif

              this%collections(i)%vars(j)%refresh_interval = &
                  var_item%get("refresh_interval")%to_real()

              ! Optional fields with default values
              if (var_item%has("export_name")) then
                  temp_str = var_item%get("export_name")%to_str()
                  call this%collections(i)%vars(j)%export_name%set(temp_str)
              else
                  call this%collections(i)%vars(j)%export_name%set( &
                      this%collections(i)%vars(j)%name%str())
              endif

              if (var_item%has("scale_factor")) then
                  this%collections(i)%vars(j)%scale_factor = &
                      var_item%get("scale_factor")%to_real()
              else
                  this%collections(i)%vars(j)%scale_factor = 1.0_ESMF_KIND_R8
              endif

              if (var_item%has("isClimatology")) then
                  this%collections(i)%vars(j)%isClimatology = &
                      var_item%get("isClimatology")%to_logical()
              else
                  this%collections(i)%vars(j)%isClimatology = .false.
              endif

              if (var_item%has("time_interp")) then
                  this%collections(i)%vars(j)%time_interp = &
                      var_item%get("time_interp")%to_logical()
              else
                  this%collections(i)%vars(j)%time_interp = .true.
              endif

          end do variable_loop
      end do collection_loop

  end subroutine init_config

end module IO_TypeMod