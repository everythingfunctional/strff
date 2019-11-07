module tostring_test
    use iso_varying_string, only: VARYING_STRING
    use strff, only: toString
    use Vegetables_m, only: Result_t, TestItem_t, assertEquals, describe, it

    implicit none
    private

    public :: test_toString_for_doubles, test_toString_for_integers
contains
    function test_toString_for_doubles() result(tests)
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
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it("works", checkToStringForIntegers)
        tests = describe("toString for integers", individual_tests)
    end function test_toString_for_integers

    pure function checkIncludesZeroAfterDecimal() result(result_)
        type(Result_t) :: result_

        type(VARYING_STRING) :: strings(2)

        strings(1) = toString(1.0D0)
        strings(2) = toString(1.0D1)

        result_ = &
                assertEquals("1.0", strings(1)) &
                .and.assertEquals("10.0", strings(2))
    end function checkIncludesZeroAfterDecimal

    pure function checkOnlyKeepsSixDigits() result(result_)
        type(Result_t) :: result_

        type(VARYING_STRING) :: strings(8)

        strings(1) = toString(123456.7D0, 6)
        strings(2) = toString(123456.1D0, 6)
        strings(3) = toString(1234567.0D0, 6)
        strings(4) = toString(1234561.0D0, 6)
        strings(5) = toString(0.1234567D0, 6)
        strings(6) = toString(0.1234561D0, 6)
        strings(7) = toString(0.01234567D0, 6)
        strings(8) = toString(0.01234561D0, 6)

        result_ = &
            assertEquals("123457.0", strings(1)) &
            .and.assertEquals("123456.0", strings(2)) &
            .and.assertEquals("1.23457e6", strings(3)) &
            .and.assertEquals("1.23456e6", strings(4)) &
            .and.assertEquals("0.123457", strings(5)) &
            .and.assertEquals("0.123456", strings(6)) &
            .and.assertEquals("1.23457e-2", strings(7)) &
            .and.assertEquals("1.23456e-2", strings(8))
    end function checkOnlyKeepsSixDigits

    pure function checkHandlesZero() result(result_)
        type(Result_t) :: result_

        double precision, parameter :: MACHINE_TINY = TINY(0.0D0)
        type(VARYING_STRING) :: strings(2)

        strings(1) = toString(0.0D0)
        strings(2) = toString(MACHINE_TINY)

        result_ = &
                assertEquals("0.0", strings(1)) &
                .and.assertEquals("0.0", strings(2))
    end function checkHandlesZero

    pure function checkHandlesExtremeNumbers() result(result_)
        type(Result_t) :: result_

        type(VARYING_STRING) :: strings(12)

        strings(1) = toString(1.23456D11, 6)
        strings(2) = toString(1.234567D11, 6)
        strings(3) = toString(1.234561D11, 6)
        strings(4) = toString(1.23456D111, 6)
        strings(5) = toString(1.234567D111, 6)
        strings(6) = toString(1.234561D111, 6)
        strings(7) = toString(1.23456D-11, 6)
        strings(8) = toString(1.234567D-11, 6)
        strings(9) = toString(1.234561D-11, 6)
        strings(10) = toString(1.23456D-111, 6)
        strings(11) = toString(1.234567D-111, 6)
        strings(12) = toString(1.234561D-111, 6)

        result_ = &
                assertEquals("1.23456e11", strings(1)) &
                .and.assertEquals("1.23457e11", strings(2)) &
                .and.assertEquals("1.23456e11", strings(3)) &
                .and.assertEquals("1.23456e111", strings(4)) &
                .and.assertEquals("1.23457e111", strings(5)) &
                .and.assertEquals("1.23456e111", strings(6)) &
                .and.assertEquals("1.23456e-11", strings(7)) &
                .and.assertEquals("1.23457e-11", strings(8)) &
                .and.assertEquals("1.23456e-11", strings(9)) &
                .and.assertEquals("1.23456e-111", strings(10)) &
                .and.assertEquals("1.23457e-111", strings(11)) &
                .and.assertEquals("1.23456e-111", strings(12))
    end function checkHandlesExtremeNumbers

    pure function checkNegativeNumbers() result(result_)
        type(Result_t) :: result_

        type(VARYING_STRING) :: strings(5)

        strings(1) = toString(-1.0D0, 6)
        strings(2) = toString(-123456.7D0, 6)
        strings(3) = toString(-0.1234567D0, 6)
        strings(4) = toString(-0.01234567D0, 6)
        strings(5) = toString(-1.234567D111, 6)

        result_ = &
                assertEquals("-1.0", strings(1)) &
                .and.assertEquals("-123457.0", strings(2)) &
                .and.assertEquals("-0.123457", strings(3)) &
                .and.assertEquals("-1.23457e-2", strings(4)) &
                .and.assertEquals("-1.23457e111", strings(5))
    end function checkNegativeNumbers

    pure function checkRoundNumbers() result(result_)
        type(Result_t) :: result_

        type(VARYING_STRING) :: the_string

        the_string = toString(1.0D6)

        result_ = &
                assertEquals("1.0e6", the_string)
    end function checkRoundNumbers

    pure function checkToStringForIntegers() result(result_)
        type(Result_t) :: result_

        type(VARYING_STRING) :: strings(3)

        strings(1) = toString(1)
        strings(2) = toString(12)
        strings(3) = toString(-1)

        result_ = &
                assertEquals("1", strings(1)) &
                .and.assertEquals("12", strings(2)) &
                .and.assertEquals("-1", strings(3))
    end function checkToStringForIntegers
end module tostring_test
