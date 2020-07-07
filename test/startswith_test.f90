module startswith_test
    use iso_varying_string, only: VARYING_STRING
    use strff, only: operator(.startsWith.)
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertThat, assertNot, describe, it

    implicit none
    private

    public :: test_startswith
contains
    function test_startswith() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "is true if the first string starts with the second", &
                checkTrue)
        individual_tests(2) = it( &
                "is false if the first string doesn't start with the second", &
                checkFalse)
        tests = describe(".startsWith.", individual_tests)
    end function test_startswith

    pure function checkTrue() result(result_)
        type(Result_t) :: result_

        result_ = assertThat("Hello, World!".startsWith."Hello")
    end function checkTrue

    pure function checkFalse() result(result_)
        type(Result_t) :: result_

        result_ = assertNot("Hello, World!".startsWith."World!")
    end function checkFalse
end module startswith_test
