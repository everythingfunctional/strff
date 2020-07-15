module readfile_test
    use iso_fortran_env, only: iostat_end
    use iso_varying_string, only: VARYING_STRING, operator(//), get, put
    use strff, only: join, readFile, NEWLINE
    use text_m, only: TEST_TEXT
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
        individual_tests(2) = it( &
                "is faster than an alternative implementation", checkSpeed)
        tests = describe("readFile", individual_tests)
    end function test_readfile

    function checkReadFile() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: FILE_CONTENTS = &
                "Just" // NEWLINE &
                // "Some" // NEWLINE &
                // "Contents"
        character(len=*), parameter :: TEMP_FILE_NAME = "readfile_tmp.txt"
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

        character(len=*), parameter :: TEMP_FILE_NAME = "readfile_speed_tmp.txt"
        type(VARYING_STRING) :: contents
        integer :: file_unit

        open(newunit = file_unit, file = TEMP_FILE_NAME, action = "WRITE", status = "REPLACE")
        call put(file_unit, TEST_TEXT)
        close(file_unit)

        result_ = assertFasterThan(doAltRead, doFastRead, 50)

        open(newunit = file_unit, file = TEMP_FILE_NAME)
        close(file_unit, status = "DELETE")
    contains
        subroutine doFastRead
            contents = readFile(TEMP_FILE_NAME)
        end subroutine doFastRead

        subroutine doAltRead
            contents = altReadFile(TEMP_FILE_NAME)
        end subroutine doAltRead
    end function checkSpeed

    function altReadFile(filename) result(contents)
        character(len=*), intent(in) :: filename
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
    end function altReadFile
end module readfile_test
