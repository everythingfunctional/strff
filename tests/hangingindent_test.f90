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

    function checkSingleLine() result(result_)
        use iso_varying_string, only: VARYING_STRING
        use strff, only: hangingIndent
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: indented

        indented = hangingIndent("Test", 1)

        result_ = assertEquals("Test", indented)
    end function checkSingleLine

    function checkIndentsCorrectly() result(result_)
        use iso_varying_string, only: VARYING_STRING
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
        type(VARYING_STRING) :: indented

        indented = hangingIndent(input, 4)

        result_ = assertEquals(expected, indented)
    end function checkIndentsCorrectly
end module hangingindent_test
