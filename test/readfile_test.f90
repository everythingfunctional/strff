module readfile_test
    use iso_varying_string, only: VARYING_STRING, put
    use strff, only: readFile, NEWLINE
    use Vegetables_m, only: Result_t, TestItem_t, assertEquals, describe, it

    implicit none
    private

    public :: test_readfile
contains
    function test_readfile() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it("gets the contents from the file", checkReadFile)
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
end module readfile_test
