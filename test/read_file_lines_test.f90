module read_file_lines_test
    implicit none
    private

    public :: test_read_file_lines
contains
    function test_read_file_lines() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "gets the contents from the file", check_read_file_lines)
        individual_tests(2) = it( &
                "is faster than an alternative implementation", check_speed)
        tests = describe("read_file_lines", individual_tests)
    end function

    function check_read_file_lines() result(result_)
        use iso_varying_string, only: varying_string, put
        use strff, only: read_file_lines, NEWLINE
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        character(len=*), parameter :: FIRST_LINE = "First Line"
        character(len=*), parameter :: SECOND_LINE = "2nd Line"
        character(len=*), parameter :: THIRD_LINE = "Third Line"
        character(len=*), parameter :: FILE_CONTENTS = &
                FIRST_LINE // NEWLINE // SECOND_LINE // NEWLINE // THIRD_LINE
        character(len=*), parameter :: TEMP_FILE_NAME = "read_file_lines_tmp.txt"
        type(varying_string), allocatable :: lines(:)
        integer :: file_unit

        allocate(lines(0)) ! TODO: remove once bug in gfortran has been fixed

        open(newunit = file_unit, file = TEMP_FILE_NAME, action = "WRITE", status = "REPLACE")
        call put(file_unit, FILE_CONTENTS)
        close(file_unit)

        lines = read_file_lines(TEMP_FILE_NAME)

        open(newunit = file_unit, file = TEMP_FILE_NAME)
        close(file_unit, status = "DELETE")

        result_ = &
                assert_equals(FIRST_LINE, lines(1)) &
                .and.assert_equals(SECOND_LINE, lines(2)) &
                .and.assert_equals(THIRD_LINE, lines(3))
    end function

    function check_speed() result(result_)
        use iso_varying_string, only: varying_string, put
        use text_m, only: TEST_TEXT
        use vegetables, only: result_t, assert_faster_than

        type(result_t) :: result_

        character(len=*), parameter :: TEMP_FILE_NAME = "read_file_lines_speed_tmp.txt"
        type(varying_string), allocatable :: lines(:)
        integer :: file_unit

        open(newunit = file_unit, file = TEMP_FILE_NAME, action = "WRITE", status = "REPLACE")
        call put(file_unit, TEST_TEXT)
        close(file_unit)

        result_ = assert_faster_than(do_alt_read, do_fast_read, 50)

        open(newunit = file_unit, file = TEMP_FILE_NAME)
        close(file_unit, status = "DELETE")
    contains
        subroutine do_fast_read
            use strff, only: read_file_lines

            lines = read_file_lines(TEMP_FILE_NAME)
        end subroutine

        subroutine do_alt_read
            lines = alt_read_file_lines(TEMP_FILE_NAME)
        end subroutine
    end function

    function alt_read_file_lines(filename) result(lines)
        use iso_fortran_env, only: IOSTAT_END
        use iso_varying_string, only: varying_string, operator(//), get
        use strff, only: split_at, NEWLINE

        character(len=*), intent(in) :: filename
        type(varying_string), allocatable :: lines(:)

        type(varying_string) :: contents
        integer :: file_unit
        integer :: stat
        type(varying_string) :: tmp

        open(newunit = file_unit, file = filename, action = "READ", status = "OLD")
        call get(file_unit, contents, iostat = stat)
        if (stat == IOSTAT_END) return
        do
            call get(file_unit, tmp, iostat = stat)
            if (stat == IOSTAT_END) exit
            contents = contents // NEWLINE // tmp
        end do
        close(file_unit)

        lines = split_at(contents, NEWLINE)
    end function
end module
