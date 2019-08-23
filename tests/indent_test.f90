module indent_test
    implicit none
    private

    public :: test_indent
contains
    function test_indent() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it("indents a single line", checkSingleLine)
        individual_tests(2) = it("indents multiple lines", checkindentsCorrectly)
        tests = describe("indent", individual_tests)
    end function test_indent

    pure function checkSingleLine() result(result_)
        use strff, only: indent
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = assertEquals("    Test", indent("Test", 4))
    end function checkSingleLine

    pure function checkindentsCorrectly() result(result_)
        use strff, only: indent, NEWLINE
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: input = &
                "First Line" // NEWLINE &
                // "Second Line" // NEWLINE &
                // "Third Line"
        character(len=*), parameter :: expected = &
                "    First Line" // NEWLINE &
                // "    Second Line" // NEWLINE &
                // "    Third Line"

        result_ = assertEquals(expected, indent(input, 4))
    end function checkindentsCorrectly
end module indent_test
