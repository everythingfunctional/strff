module is_negative_test
    use ieee_arithmetic, only: &
            ieee_value, &
            ieee_negative_inf, &
            ieee_negative_zero, &
            ieee_positive_inf, &
            ieee_positive_zero, &
            ieee_quiet_nan
    use strff, only: is_negative
    use veggies, only: &
            result_t, test_item_t, assert_not, assert_that, describe, it

    implicit none
    private
    public :: test_is_negative
contains
    function test_is_negative() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "is_negative", &
                [ it("is false for +0.0", check_pos_zero) &
                , it("is true for -0.0", check_neg_zero) &
                , it("is false for a normal positive number", check_pos_norm) &
                , it("is true for a normal negative number", check_neg_norm) &
                , it("is false for +huge", check_pos_huge) &
                , it("is true for -huge", check_neg_huge) &
                , it("is false for +tiny", check_pos_tiny) &
                , it("is true for -tiny", check_neg_tiny) &
                , it("is false for +inf", check_pos_inf) &
                , it("is true for -inf", check_neg_inf) &
                , it("is false for numbers larger than +huge", check_pos_big) &
                , it("is true for numbers smaller than -huge", check_neg_big) &
                , it("is false for numbers smaller than +tiny", check_pos_small) &
                , it("is true for numbers larger than -tiny", check_neg_small) &
                , it("is false for nan", check_nan) &
                ])
    end function

    function check_pos_zero() result(result_)
        type(result_t) :: result_

        result_ = assert_not(is_negative(ieee_value(0.0, ieee_positive_zero)))
    end function

    function check_neg_zero() result(result_)
        type(result_t) :: result_

        result_ = assert_that(is_negative(ieee_value(0.0, ieee_negative_zero)))
    end function

    function check_pos_norm() result(result_)
        type(result_t) :: result_

        result_ = assert_not(is_negative(1.0))
    end function

    function check_neg_norm() result(result_)
        type(result_t) :: result_

        result_ = assert_that(is_negative(-1.0))
    end function

    function check_pos_huge() result(result_)
        type(result_t) :: result_

        result_ = assert_not(is_negative(huge(1.0)))
    end function

    function check_neg_huge() result(result_)
        type(result_t) :: result_

        result_ = assert_that(is_negative(-huge(1.0)))
    end function

    function check_pos_tiny() result(result_)
        type(result_t) :: result_

        result_ = assert_not(is_negative(tiny(1.0)))
    end function

    function check_neg_tiny() result(result_)
        type(result_t) :: result_

        result_ = assert_that(is_negative(-tiny(1.0)))
    end function

    function check_pos_inf() result(result_)
        type(result_t) :: result_

        result_ = assert_not(is_negative(ieee_value(0.0, ieee_positive_inf)))
    end function

    function check_neg_inf() result(result_)
        type(result_t) :: result_

        result_ = assert_that(is_negative(ieee_value(0.0, ieee_negative_inf)))
    end function

    function check_pos_big() result(result_)
        type(result_t) :: result_

        real :: val

        val = huge(1.0)
        val = val * 2
        result_ = assert_not(is_negative(val))
    end function

    function check_neg_big() result(result_)
        type(result_t) :: result_

        real :: val

        val = -huge(1.0)
        val = val * 2
        result_ = assert_that(is_negative(val))
    end function

    function check_pos_small() result(result_)
        type(result_t) :: result_

        real :: val

        val = tiny(1.0)
        val = val / 2
        result_ = assert_not(is_negative(val))
    end function

    function check_neg_small() result(result_)
        type(result_t) :: result_

        real :: val

        val = -tiny(1.0)
        val = val / 2
        result_ = assert_that(is_negative(val))
    end function

    function check_nan() result(result_)
        type(result_t) :: result_

        result_ = assert_not(is_negative(ieee_value(0.0, ieee_quiet_nan)))
    end function
end module