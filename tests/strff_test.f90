module strff_test
    implicit none
    private

    public :: test_join, test_splitAt
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

    function test_splitAt() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

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
                "returns an empty string when given an empty string", &
                checkForEmptyString)
        individual_tests(8) = it( &
                "returns an empty string when given a string that only contains split characters", &
                checkForOnlySplitCharacters)
        tests = describe("splitAt", individual_tests)
    end function test_splitAt

    pure function checkJoinOne() result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)
        use strff, only: join
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        character(len=*), parameter :: EXAMPLE = "Example"
        type(VARYING_STRING) :: strings(1)

        strings(1) = EXAMPLE

        result_ = assertEquals(EXAMPLE, join(strings, "anything"))
    end function checkJoinOne

    pure function checkJoinMultiple() result(result_)
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

    pure function checkSplitDoesntContain() result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING
        use strff, only: splitAt
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt("Hello World", ","))

        result_ = &
                assertEquals(1, size(strings)) &
                .and.assertEquals("Hello World", strings(1))
    end function checkSplitDoesntContain

    pure function checkSplitAtSomething() result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING
        use strff, only: splitAt
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt("Hello,World", ","))
        result_ = &
                assertEquals(2, size(strings)) &
                .and.assertEquals("Hello", strings(1)) &
                .and.assertEquals("World", strings(2))
    end function checkSplitAtSomething

    pure function checkNoEmptyEnd() result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING
        use strff, only: splitAt
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt("Hello,World,", ","))
        result_ = &
                assertEquals(2, size(strings)) &
                .and.assertEquals("Hello", strings(1)) &
                .and.assertEquals("World", strings(2))
    end function checkNoEmptyEnd

    pure function checkNoEmptyBegin() result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING
        use strff, only: splitAt
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt(",Hello,World", ","))
        result_ = &
                assertEquals(2, size(strings)) &
                .and.assertEquals("Hello", strings(1)) &
                .and.assertEquals("World", strings(2))
    end function checkNoEmptyBegin

    pure function checkNoSplitCharacters() result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING
        use strff, only: splitAt
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt("Hello,World", ""))
        result_ = &
                assertEquals(1, size(strings)) &
                .and.assertEquals("Hello,World", strings(1))
    end function checkNoSplitCharacters

    pure function checkNoEmptyBetween() result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING
        use strff, only: splitAt
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt("Hello, World", " ,"))
        result_ = &
                assertEquals(2, size(strings)) &
                .and.assertEquals("Hello", strings(1)) &
                .and.assertEquals("World", strings(2))
    end function checkNoEmptyBetween

    pure function checkForEmptyString() result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING
        use strff, only: splitAt
        use Vegetables_m, only: Result_t, assertEmpty, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt("", " ,"))
        result_ = &
                assertEquals(1, size(strings)) &
                .and.assertEmpty(strings(1))
    end function checkForEmptyString

    pure function checkForOnlySplitCharacters() result(result_)
        use ISO_VARYING_STRING, only: VARYING_STRING
        use strff, only: splitAt
        use Vegetables_m, only: Result_t, assertEmpty, assertEquals

        type(Result_t) :: result_

        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt(", ", " ,"))
        result_ = &
                assertEquals(1, size(strings)) &
                .and.assertEmpty(strings(1))
    end function checkForOnlySplitCharacters
end module strff_test
