module readfile_test
    use iso_fortran_env, only: iostat_end
    use iso_varying_string, only: VARYING_STRING, get, put
    use strff, only: join, readFile, NEWLINE
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, assertFasterThan, describe, it

    implicit none
    private

    public :: test_readfile
contains
    function test_readfile() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it("gets the contents from the file", checkReadFile)
        individual_tests(2) = it("is faster than a naive implementation", checkSpeed)
        tests = describe("readFile", individual_tests)
    end function test_readfile

    function checkReadFile() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: FILE_CONTENTS = &
                "Just" // NEWLINE &
                // "Some" // NEWLINE &
                // "Contents"
        character(len=*), parameter :: TEMP_FILE_NAME = "temp_file.txt"
        type(VARYING_STRING) :: contents
        integer :: file_unit

        open(newunit = file_unit, file = TEMP_FILE_NAME, action = "WRITE", status = "REPLACE")
        call put(file_unit, FILE_CONTENTS)
        close(file_unit)

        contents = readFile(TEMP_FILE_NAME)

        open(newunit = file_unit, file = TEMP_FILE_NAME)
        close(file_unit, status = "DELETE")

        result_ = assertEquals(FILE_CONTENTS, contents)
    end function checkReadFile

    function checkSpeed() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: FILE_CONTENTS = &
                "Just" // NEWLINE &
                // "Some" // NEWLINE &
                // "Contents" // NEWLINE &
                // "For" // NEWLINE &
                // "Testing" // NEWLINE &
                // "Read" // NEWLINE &
                // "Speed"
        character(len=*), parameter :: TEMP_FILE_NAME = "speed_check_file.txt"
        type(VARYING_STRING) :: contents
        integer :: file_unit

        open(newunit = file_unit, file = TEMP_FILE_NAME, action = "WRITE", status = "REPLACE")
        call put(file_unit, FILE_CONTENTS)
        close(file_unit)

        result_ = assertFasterThan(doNaiveRead, doFastRead, 50)

        open(newunit = file_unit, file = TEMP_FILE_NAME)
        close(file_unit, status = "DELETE")
    contains
        subroutine doFastRead
            contents = readFile(TEMP_FILE_NAME)
        end subroutine doFastRead

        subroutine doNaiveRead
            contents = naiveReadFile(TEMP_FILE_NAME)
        end subroutine doNaiveRead
    end function checkSpeed

    function naiveReadFile(filename) result(contents)
        character(len=*), intent(in) :: filename
        type(VARYING_STRING) :: contents

        integer :: file_unit
        integer :: i
        type(VARYING_STRING), allocatable :: lines(:)
        integer :: num_lines
        integer :: stat
        type(VARYING_STRING) :: tmp

        open(newunit = file_unit, file = filename, action = "READ", status = "OLD")
        num_lines = 0
        do
            call get(file_unit, tmp, iostat = stat)
            if (stat == iostat_end) exit
            num_lines = num_lines + 1
        end do
        rewind(file_unit)

        allocate(lines(num_lines))
        do i = 1, num_lines
            call get(file_unit, lines(i))
        end do
        close(file_unit)

        contents = join(lines, NEWLINE)
    end function naiveReadFile
end module readfile_test
