module readfilelines_test
    use iso_fortran_env, only: iostat_end
    use iso_varying_string, only: VARYING_STRING, operator(//), get, put
    use strff, only: readFileLines, splitAt, NEWLINE
    use text_m, only: TEST_TEXT
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, assertFasterThan, describe, it

    implicit none
    private

    public :: test_readfilelines
contains
    function test_readfilelines() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "gets the contents from the file", checkReadFileLines)
        individual_tests(2) = it( &
                "is faster than an alternative implementation", checkSpeed)
        tests = describe("readFileLines", individual_tests)
    end function test_readfilelines

    function checkReadFileLines() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: FIRST_LINE = "First Line"
        character(len=*), parameter :: SECOND_LINE = "2nd Line"
        character(len=*), parameter :: THIRD_LINE = "Third Line"
        character(len=*), parameter :: FILE_CONTENTS = &
                FIRST_LINE // NEWLINE // SECOND_LINE // NEWLINE // THIRD_LINE
        character(len=*), parameter :: TEMP_FILE_NAME = "readfilelines_tmp.txt"
        type(VARYING_STRING), allocatable :: lines(:)
        integer :: file_unit

        open(newunit = file_unit, file = TEMP_FILE_NAME, action = "WRITE", status = "REPLACE")
        call put(file_unit, FILE_CONTENTS)
        close(file_unit)

        lines = readFileLines(TEMP_FILE_NAME)

        open(newunit = file_unit, file = TEMP_FILE_NAME)
        close(file_unit, status = "DELETE")

        result_ = &
                assertEquals(FIRST_LINE, lines(1)) &
                .and.assertEquals(SECOND_LINE, lines(2)) &
                .and.assertEquals(THIRD_LINE, lines(3))
    end function checkReadFileLines

    function checkSpeed() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: TEMP_FILE_NAME = "readfilelines_speed_tmp.txt"
        type(VARYING_STRING), allocatable :: lines(:)
        integer :: file_unit

        open(newunit = file_unit, file = TEMP_FILE_NAME, action = "WRITE", status = "REPLACE")
        call put(file_unit, TEST_TEXT)
        close(file_unit)

        result_ = assertFasterThan(doAltRead, doFastRead, 50)

        open(newunit = file_unit, file = TEMP_FILE_NAME)
        close(file_unit, status = "DELETE")
    contains
        subroutine doFastRead
            lines = readFileLines(TEMP_FILE_NAME)
        end subroutine doFastRead

        subroutine doAltRead
            lines = altReadFileLines(TEMP_FILE_NAME)
        end subroutine doAltRead
    end function checkSpeed

    function altReadFileLines(filename) result(lines)
        character(len=*), intent(in) :: filename
        type(VARYING_STRING), allocatable :: lines(:)

        type(VARYING_STRING) :: contents
        integer :: file_unit
        integer :: stat
        type(VARYING_STRING) :: tmp

        open(newunit = file_unit, file = filename, action = "READ", status = "OLD")
        call get(file_unit, contents, iostat = stat)
        if (stat == iostat_end) return
        do
            call get(file_unit, tmp, iostat = stat)
            if (stat == iostat_end) exit
            contents = contents // NEWLINE // tmp
        end do
        close(file_unit)

        lines = splitAt(contents, NEWLINE)
    end function altReadFileLines
end module readfilelines_test
