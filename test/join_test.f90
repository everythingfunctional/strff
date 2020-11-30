module join_test
    implicit none
    private

    public :: test_join
contains
    function test_join() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "for only one string returns that string", check_join_one)
        individual_tests(2) = it( &
                "puts multiple strings together separated by the given string", &
                check_join_multiple)
        tests = describe("join", individual_tests)
    end function test_join

    pure function check_join_one() result(result_)
        use iso_varying_string, only: var_str
        use strff, only: join
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        character(len=*), parameter :: EXAMPLE = "Example"

        result_ = assert_equals(EXAMPLE, join([var_str(EXAMPLE)], "anything"))
    end function

    pure function check_join_multiple() result(result_)
        use iso_varying_string, only: var_str
        use strff, only: join
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        result_ = assert_equals( &
                "Hello, again, world", &
                join([var_str("Hello"), var_str("again"), var_str("world")], ", "))
    end function
end module
