module splitat_test
    use ISO_VARYING_STRING, only: VARYING_STRING
    use strff, only: splitAt
    use Vegetables_m, only: Result_t, TestItem_t, assertEquals, describe, it

    implicit none
    private

    public :: test_splitAt
contains
    function test_splitAt() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(8)

        individual_tests(1) = it( &
                "returns the same string when split on a character it doesn't contain", &
                checkSplitDoesntContain)
        individual_tests(2) = it( &
                "can split strings at something", checkSplitAtSomething)
        individual_tests(3) = it( &
                "doesn't include an empty string at the end", &
                checkNoEmptyEnd)
        individual_tests(4) = it( &
                "doesn't include an empty string at the beginning", &
                checkNoEmptyBegin)
        individual_tests(5) = it( &
                "returns the same string when given no split characters", &
                checkNoSplitCharacters)
        individual_tests(6) = it( &
                "doesn't include an empty string between split characters", &
                checkNoEmptyBetween)
        individual_tests(7) = it( &
                "returns an empty array when given an empty string", &
                checkForEmptyString)
        individual_tests(8) = it( &
                "returns an empty array when given a string that only contains split characters", &
                checkForOnlySplitCharacters)
        tests = describe("splitAt", individual_tests)
    end function test_splitAt

    function checkSplitDoesntContain() result(result_)
        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt("Hello World", ","))

        result_ = &
                assertEquals(1, size(strings)) &
                .and.assertEquals("Hello World", strings(1))
    end function checkSplitDoesntContain

    function checkSplitAtSomething() result(result_)
        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt("Hello,World", ","))
        result_ = &
                assertEquals(2, size(strings)) &
                .and.assertEquals("Hello", strings(1)) &
                .and.assertEquals("World", strings(2))
    end function checkSplitAtSomething

    function checkNoEmptyEnd() result(result_)
        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt("Hello,World,", ","))
        result_ = &
                assertEquals(2, size(strings)) &
                .and.assertEquals("Hello", strings(1)) &
                .and.assertEquals("World", strings(2))
    end function checkNoEmptyEnd

    function checkNoEmptyBegin() result(result_)
        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt(",Hello,World", ","))
        result_ = &
                assertEquals(2, size(strings)) &
                .and.assertEquals("Hello", strings(1)) &
                .and.assertEquals("World", strings(2))
    end function checkNoEmptyBegin

    function checkNoSplitCharacters() result(result_)
        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt("Hello,World", ""))
        result_ = &
                assertEquals(1, size(strings)) &
                .and.assertEquals("Hello,World", strings(1))
    end function checkNoSplitCharacters

    function checkNoEmptyBetween() result(result_)
        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt("Hello, World", " ,"))
        result_ = &
                assertEquals(2, size(strings)) &
                .and.assertEquals("Hello", strings(1)) &
                .and.assertEquals("World", strings(2))
    end function checkNoEmptyBetween

    function checkForEmptyString() result(result_)
        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt("", " ,"))
        result_ = assertEquals(0, size(strings))
    end function checkForEmptyString

    function checkForOnlySplitCharacters() result(result_)
        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt(", ", " ,"))
        result_ = assertEquals(0, size(strings))
    end function checkForOnlySplitCharacters
end module splitat_test
