module tostring_test
    implicit none
    private

    public :: test_toString_for_integers
contains
    function test_toString_for_integers() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it("works", checkToStringForIntegers)
        tests = describe("toString for integers", individual_tests)
    end function test_toString_for_integers

    pure function checkToStringForIntegers() result(result_)
        use strff, only: toString
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = &
                assertEquals("1", toString(1)) &
                .and.assertEquals("12", toString(12)) &
                .and.assertEquals("-1", toString(-1))
    end function checkToStringForIntegers
end module tostring_test
