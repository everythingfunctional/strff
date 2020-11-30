module split_at_test
    implicit none
    private

    public :: test_split_at
contains
    function test_split_at() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(8)

        individual_tests(1) = it( &
                "returns the same string when split on a character it doesn't contain", &
                check_split_doesnt_contain)
        individual_tests(2) = it( &
                "can split strings at something", check_split_at_something)
        individual_tests(3) = it( &
                "doesn't include an empty string at the end", &
                check_no_empty_end)
        individual_tests(4) = it( &
                "doesn't include an empty string at the beginning", &
                check_no_empty_begin)
        individual_tests(5) = it( &
                "returns the same string when given no split characters", &
                check_no_split_characters)
        individual_tests(6) = it( &
                "doesn't include an empty string between split characters", &
                check_no_empty_between)
        individual_tests(7) = it( &
                "returns an empty array when given an empty string", &
                check_for_empty_string)
        individual_tests(8) = it( &
                "returns an empty array when given a string that only contains split characters", &
                check_for_only_split_characters)
        tests = describe("split_at", individual_tests)
    end function

    pure function check_split_doesnt_contain() result(result_)
        use iso_varying_string, only: varying_string
        use strff, only: split_at
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at("Hello World", ",")
        result_ = &
                assert_equals(1, size(strings)) &
                .and.assert_equals("Hello World", strings(1))
    end function

    pure function check_split_at_something() result(result_)
        use iso_varying_string, only: varying_string
        use strff, only: split_at
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at("Hello,World", ",")
        result_ = &
                assert_equals(2, size(strings)) &
                .and.assert_equals("Hello", strings(1)) &
                .and.assert_equals("World", strings(2))
    end function

    pure function check_no_empty_end() result(result_)
        use iso_varying_string, only: varying_string
        use strff, only: split_at
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at("Hello,World,", ",")
        result_ = &
                assert_equals(2, size(strings)) &
                .and.assert_equals("Hello", strings(1)) &
                .and.assert_equals("World", strings(2))
    end function

    pure function check_no_empty_begin() result(result_)
        use iso_varying_string, only: varying_string
        use strff, only: split_at
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at(",Hello,World", ",")
        result_ = &
                assert_equals(2, size(strings)) &
                .and.assert_equals("Hello", strings(1)) &
                .and.assert_equals("World", strings(2))
    end function

    pure function check_no_split_characters() result(result_)
        use iso_varying_string, only: varying_string
        use strff, only: split_at
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at("Hello,World", "")
        result_ = &
                assert_equals(1, size(strings)) &
                .and.assert_equals("Hello,World", strings(1))
    end function

    pure function check_no_empty_between() result(result_)
        use iso_varying_string, only: varying_string
        use strff, only: split_at
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at("Hello, World", " ,")
        result_ = &
                assert_equals(2, size(strings)) &
                .and.assert_equals("Hello", strings(1)) &
                .and.assert_equals("World", strings(2))
    end function

    pure function check_for_empty_string() result(result_)
        use iso_varying_string, only: varying_string
        use strff, only: split_at
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at("", " ,")
        result_ = assert_equals(0, size(strings))
    end function

    pure function check_for_only_split_characters() result(result_)
        use iso_varying_string, only: varying_string
        use strff, only: split_at
        use vegetables, only: result_t, assert_equals

        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at(", ", " ,")
        result_ = assert_equals(0, size(strings))
    end function
end module
