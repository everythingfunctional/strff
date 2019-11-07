module indent_test
    use iso_varying_string, only: VARYING_STRING
    use strff, only: indent, NEWLINE
    use Vegetables_m, only: Result_t, TestItem_t, assertEquals, describe, it

    implicit none
    private

    public :: test_indent
contains
    function test_indent() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it("indents a single line", checkSingleLine)
        individual_tests(2) = it("indents multiple lines", checkindentsCorrectly)
        tests = describe("indent", individual_tests)
    end function test_indent

    function checkSingleLine() result(result_)
        type(Result_t) :: result_

        type(VARYING_STRING) :: indented

        indented = indent("Test", 4)

        result_ = assertEquals("    Test", indented)
    end function checkSingleLine

    function checkindentsCorrectly() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: input = &
                "First Line" // NEWLINE &
                // "Second Line" // NEWLINE &
                // "Third Line"
        character(len=*), parameter :: expected = &
                "    First Line" // NEWLINE &
                // "    Second Line" // NEWLINE &
                // "    Third Line"
        type(VARYING_STRING) :: indented

        indented = indent(input, 4)

        result_ = assertEquals(expected, indented)
    end function checkindentsCorrectly
end module indent_test
