module readfilelines_test
    use iso_varying_string, only: VARYING_STRING, put
    use strff, only: readFileLines, NEWLINE
    use Vegetables_m, only: Result_t, TestItem_t, assertEquals, describe, it

    implicit none
    private

    public :: test_readfilelines
contains
    function test_readfilelines() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it("gets the contents from the file", checkReadFileLines)
        tests = describe("readFileLines", individual_tests)
    end function test_readfilelines

    function checkReadFileLines() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: FIRST_LINE = "First Line"
        character(len=*), parameter :: SECOND_LINE = "2nd Line"
        character(len=*), parameter :: THIRD_LINE = "Third Line"
        character(len=*), parameter :: FILE_CONTENTS = &
                FIRST_LINE // NEWLINE // SECOND_LINE // NEWLINE // THIRD_LINE
        character(len=*), parameter :: TEMP_FILE_NAME = "temp_file.txt"
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
end module readfilelines_test
