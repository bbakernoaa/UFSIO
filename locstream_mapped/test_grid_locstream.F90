program test_grid_locstream
    use grid_locstream_module
    implicit none

    ! Test variables
    type(grid_type) :: grid
    integer :: i, j, ierr
    real(kind=8), allocatable :: lats(:), lons(:)
    logical :: test_passed
    integer :: num_tests = 0, num_failed = 0

    print *, "Starting tests for grid_locstream_module"
    print *, "======================================="

    ! Test 1: Grid initialization
    call test_grid_init()

    ! Test 2: Grid coordinate generation
    call test_grid_coords()

    ! Test 3: Boundary conditions
    call test_boundaries()

    ! Print summary
    print *, "======================================="
    print *, "Test Summary:"
    print *, "Total tests: ", num_tests
    print *, "Failed tests: ", num_failed

contains

    subroutine test_grid_init()
        num_tests = num_tests + 1

        ! Test grid initialization
        call init_grid(grid, 180, 90, ierr)

        if (ierr /= 0) then
            print *, "FAILED: Grid initialization"
            print *, "Expected ierr = 0, got ", ierr
            num_failed = num_failed + 1
        else
            print *, "PASSED: Grid initialization"
        endif

        ! Verify grid dimensions
        if (grid%nx /= 180 .or. grid%ny /= 90) then
            print *, "FAILED: Grid dimensions"
            print *, "Expected 180x90, got ", grid%nx, "x", grid%ny
            num_failed = num_failed + 1
        endif
    end subroutine test_grid_init

    subroutine test_grid_coords()
        num_tests = num_tests + 1

        ! Allocate test arrays
        allocate(lats(grid%ny), lons(grid%nx))

        ! Generate coordinates
        call generate_grid_coords(grid, lats, lons, ierr)

        ! Test coordinate ranges
        test_passed = .true.
        do i = 1, grid%nx
            if (lons(i) < -180.0d0 .or. lons(i) > 180.0d0) then
                test_passed = .false.
                exit
            endif
        enddo

        do j = 1, grid%ny
            if (lats(j) < -90.0d0 .or. lats(j) > 90.0d0) then
                test_passed = .false.
                exit
            endif
        enddo

        if (.not. test_passed) then
            print *, "FAILED: Coordinate ranges"
            num_failed = num_failed + 1
        else
            print *, "PASSED: Coordinate ranges"
        endif

        deallocate(lats, lons)
    end subroutine test_grid_coords

    subroutine test_boundaries()
        num_tests = num_tests + 1
        real(kind=8) :: test_lat, test_lon

        ! Test boundary conditions
        test_lat = 90.0d0
        test_lon = 180.0d0

        call check_boundaries(grid, test_lat, test_lon, ierr)

        if (ierr /= 0) then
            print *, "FAILED: Boundary check"
            num_failed = num_failed + 1
        else
            print *, "PASSED: Boundary check"
        endif
    end subroutine test_boundaries

end program test_grid_locstream