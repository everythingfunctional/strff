module tostring_test
    implicit none
    private

    public :: test_toString_for_doubles, test_toString_for_integers
contains
    function test_toString_for_doubles() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(6)

        individual_tests(1) = it( &
                "includes zero after the decimal", &
                checkIncludesZeroAfterDecimal)
        individual_tests(2) = it( &
                "only keeps the specified number of digits", &
                checkOnlyKeepsSixDigits)
        individual_tests(3) = it( &
                "handles zero correctly", &
                checkHandlesZero)
        individual_tests(4) = it( &
                "handles extreme numbers correctly", &
                checkHandlesExtremeNumbers)
        individual_tests(5) = it( &
                "can do negative numbers", &
                checkNegativeNumbers)
        individual_tests(6) = it( &
                "shortens round numbers with scientific notation", &
                checkRoundNumbers)
        tests = describe("toString for doubles", individual_tests)
    end function test_toString_for_doubles

    function test_toString_for_integers() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it("works", checkToStringForIntegers)
        tests = describe("toString for integers", individual_tests)
    end function test_toString_for_integers

    pure function checkIncludesZeroAfterDecimal() result(result_)
        use strff, only: toString
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = &
                assertEquals("1.0", toString(1.0D0)) &
                .and.assertEquals("10.0", toString(1.0D1))
    end function checkIncludesZeroAfterDecimal

    pure function checkOnlyKeepsSixDigits() result(result_)
        use strff, only: toString
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = &
            assertEquals("123457.0", toString(123456.7D0, 6)) &
            .and.assertEquals("123456.0", toString(123456.1D0, 6)) &
            .and.assertEquals("1.23457e6", toString(1234567.0D0, 6)) &
            .and.assertEquals("1.23456e6", toString(1234561.0D0, 6)) &
            .and.assertEquals("0.123457", toString(0.1234567D0, 6)) &
            .and.assertEquals("0.123456", toString(0.1234561D0, 6)) &
            .and.assertEquals("1.23457e-2", toString(0.01234567D0, 6)) &
            .and.assertEquals("1.23456e-2", toString(0.01234561D0, 6))
    end function checkOnlyKeepsSixDigits

    pure function checkHandlesZero() result(result_)
        use strff, only: toString
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        double precision, parameter :: MACHINE_TINY = TINY(0.0D0)

        result_ = &
                assertEquals("0.0", toString(0.0D0)) &
                .and.assertEquals("0.0", toString(MACHINE_TINY))
    end function checkHandlesZero

    pure function checkHandlesExtremeNumbers() result(result_)
        use strff, only: toString
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = &
                assertEquals("1.23456e11", toString(1.23456D11, 6)) &
                .and.assertEquals("1.23457e11", toString(1.234567D11, 6)) &
                .and.assertEquals("1.23456e11", toString(1.234561D11, 6)) &
                .and.assertEquals("1.23456e111", toString(1.23456D111, 6)) &
                .and.assertEquals("1.23457e111", toString(1.234567D111, 6)) &
                .and.assertEquals("1.23456e111", toString(1.234561D111, 6)) &
                .and.assertEquals("1.23456e-11", toString(1.23456D-11, 6)) &
                .and.assertEquals("1.23457e-11", toString(1.234567D-11, 6)) &
                .and.assertEquals("1.23456e-11", toString(1.234561D-11, 6)) &
                .and.assertEquals("1.23456e-111", toString(1.23456D-111, 6)) &
                .and.assertEquals("1.23457e-111", toString(1.234567D-111, 6)) &
                .and.assertEquals("1.23456e-111", toString(1.234561D-111, 6))
    end function checkHandlesExtremeNumbers

    pure function checkNegativeNumbers() result(result_)
        use strff, only: toString
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = &
                assertEquals("-1.0", toString(-1.0D0, 6)) &
                .and.assertEquals("-123457.0", toString(-123456.7D0, 6)) &
                .and.assertEquals("-0.123457", toString(-0.1234567D0, 6)) &
                .and.assertEquals("-1.23457e-2", toString(-0.01234567D0, 6)) &
                .and.assertEquals("-1.23457e111", toString(-1.234567D111, 6))
    end function checkNegativeNumbers

    pure function checkRoundNumbers() result(result_)
        use strff, only: toString
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        result_ = &
                assertEquals("1.0e6", toString(1.0D6))
    end function checkRoundNumbers

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
