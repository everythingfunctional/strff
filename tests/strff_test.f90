module strff_test
    implicit none
    private

    public :: test_splitAt
contains
    function test_splitAt() result(tests)
        use Vegetables_m, only: TestItem_t, describe, it

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it( &
                "returns the same string when split on a character it doesn't contain", &
                checkSplitDoesntContain)
        tests = describe("splitAt", individual_tests)
    end function test_splitAt

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
end module strff_test
