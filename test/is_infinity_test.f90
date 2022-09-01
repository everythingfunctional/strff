module is_infinity_test
    use ieee_arithmetic, only: &
            ieee_value, ieee_negative_inf, ieee_positive_inf, ieee_quiet_nan
    use strff, only: is_infinity
    use veggies, only: &
            result_t, test_item_t, assert_not, assert_that, describe, it

    implicit none
    private
    public :: test_is_infinity
contains
    function test_is_infinity() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "is_infinity", &
                [ it("is false for normal numbers", check_normal) &
                , it("is false for huge", check_huge) &
                , it("is false for tiny", check_tiny) &
                , it("is true for +inf", check_pos_inf) &
                , it("is true for -inf", check_neg_inf) &
                ! , it("is false for numbers larger than huge", check_big) &
                , it("is false for numbers smaller than tiny", check_small) &
                , it("is false for nan", check_nan) &
                ])
    end function

    function check_normal() result(result_)
        type(result_t) :: result_

        result_ = assert_not(is_infinity(1.0))
    end function

    function check_huge() result(result_)
        type(result_t) :: result_

        result_ = assert_not(is_infinity(huge(1.0)))
    end function

    function check_tiny() result(result_)
        type(result_t) :: result_

        result_ = assert_not(is_infinity(tiny(1.0)))
    end function

    function check_pos_inf() result(result_)
        type(result_t) :: result_

        result_ = assert_that(is_infinity(ieee_value(1.0, ieee_positive_inf)))
    end function

    function check_neg_inf() result(result_)
        type(result_t) :: result_

        result_ = assert_that(is_infinity(ieee_value(1.0, ieee_negative_inf)))
    end function

    ! Can't test this, because on most systems anything greater than huge overflows and actually does produce infinity
    ! function check_big() result(result_)
    !     type(result_t) :: result_

    !     real :: val

    !     val = huge(1.0)
    !     val = val * 2
    !     result_ = assert_not(is_infinity(val))
    ! end function

    function check_small() result(result_)
        type(result_t) :: result_

        real :: val

        val = tiny(1.0)
        val = val / 2
        result_ = assert_not(is_infinity(val))
    end function

    function check_nan() result(result_)
        type(result_t) :: result_

        result_ = assert_not(is_infinity(ieee_value(1.0, ieee_quiet_nan)))
    end function
end module