module split_at_test
    use iso_varying_string, only: varying_string
    use strff, only: split_at
    use vegetables, only: test_item_t, result_t, assert_equals, describe, it

    implicit none
    private

    public :: test_split_at
contains
    function test_split_at() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "split_at", &
                [ it( &
                        "returns the same string when split on a character it doesn't contain", &
                        check_split_doesnt_contain) &
                , it( &
                        "can split strings at something", check_split_at_something) &
                , it( &
                        "doesn't include an empty string at the end", &
                        check_no_empty_end) &
                , it( &
                        "doesn't include an empty string at the beginning", &
                        check_no_empty_begin) &
                , it( &
                        "returns the same string when given no split characters", &
                        check_no_split_characters) &
                , it( &
                        "doesn't include an empty string between split characters", &
                        check_no_empty_between) &
                , it( &
                        "returns an empty array when given an empty string", &
                        check_for_empty_string) &
                , it( &
                        "returns an empty array when given a string that only contains split characters", &
                        check_for_only_split_characters) &
                ])
    end function

    pure function check_split_doesnt_contain() result(result_)
        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at("Hello World", ",")
        result_ = &
                assert_equals(1, size(strings)) &
                .and.assert_equals("Hello World", strings(1))
    end function

    pure function check_split_at_something() result(result_)
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
        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at("Hello,World", "")
        result_ = &
                assert_equals(1, size(strings)) &
                .and.assert_equals("Hello,World", strings(1))
    end function

    pure function check_no_empty_between() result(result_)
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
        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at("", " ,")
        result_ = assert_equals(0, size(strings))
    end function

    pure function check_for_only_split_characters() result(result_)
        type(result_t) :: result_

        type(varying_string), allocatable :: strings(:)

        allocate(strings(0)) ! TODO: remove once bug in gfortran has been fixed

        strings = split_at(", ", " ,")
        result_ = assert_equals(0, size(strings))
    end function
end module
