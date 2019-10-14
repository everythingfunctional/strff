module join_test
    implicit none
    private

    public :: test_join
contains
    function test_join() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "for only one string returns that string", checkJoinOne)
        individual_tests(2) = it( &
                "puts multiple strings together separated by the given string", &
                checkJoinMultiple)
        tests = describe("join", individual_tests)
    end function test_join

    function checkJoinOne() result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)
        use strff, only: join
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: EXAMPLE = "Example"
        type(VARYING_STRING) :: strings(1)

        strings(1) = EXAMPLE

        result_ = assertEquals(EXAMPLE, join(strings, "anything"))
    end function checkJoinOne

    function checkJoinMultiple() result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)
        use strff, only: join
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING) :: strings(3)

        strings(1) = "Hello"
        strings(2) = "again"
        strings(3) = "world"

        result_ = assertEquals("Hello, again, world", join(strings, ", "))
    end function checkJoinMultiple
end module join_test
