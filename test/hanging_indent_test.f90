module hanging_indent_test
    implicit none
    private

    public :: test_hanging_indent
contains
    function test_hanging_indent() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(2)

        individual_tests(1) = it("does nothing to a single line", check_single_line)
        individual_tests(2) = it("indents all but the first line", check_indents_correctly)
        tests = describe("hanging_indent", individual_tests)
    end function test_hanging_indent

    pure function check_single_line() result(result_)
        use strff, only: hanging_indent
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals("Test", hanging_indent("Test", 1))
    end function

    pure function check_indents_correctly() result(result_)
        use strff, only: hanging_indent, NEWLINE
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        character(len=*), parameter :: input = &
                "First Line" // NEWLINE &
                // "Second Line" // NEWLINE &
                // "Third Line"
        character(len=*), parameter :: expected = &
                "First Line" // NEWLINE &
                // "    Second Line" // NEWLINE &
                // "    Third Line"

        result_ = assert_equals(expected, hanging_indent(input, 4))
    end function
end module
