module hangingindent_test
    implicit none
    private

    public :: test_hangingIndent
contains
    function test_hangingIndent() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it("does nothing to a single line", checkSingleLine)
        individual_tests(2) = it("indents all but the first line", checkIndentsCorrectly)
        tests = describe("hangingIndent", individual_tests)
    end function test_hangingIndent

    pure function checkSingleLine() result(result_)
        use strff, only: hangingIndent
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals("Test", hangingIndent("Test", 1))
    end function checkSingleLine

    pure function checkIndentsCorrectly() result(result_)
        use strff, only: hangingIndent, NEWLINE
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: input = &
                "First Line" // NEWLINE &
                // "Second Line" // NEWLINE &
                // "Third Line"
        character(len=*), parameter :: expected = &
                "First Line" // NEWLINE &
                // "    Second Line" // NEWLINE &
                // "    Third Line"

        result_ = assertEquals(expected, hangingIndent(input, 4))
    end function checkIndentsCorrectly
end module hangingindent_test
