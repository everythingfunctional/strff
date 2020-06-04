program test
    implicit none

    call run()
contains
    subroutine run()
        use hangingindent_test, only: &
            hangingindent_hangingindent => test_hangingindent
        use indent_test, only: &
            indent_indent => test_indent
        use join_test, only: &
            join_join => test_join
        use splitat_test, only: &
            splitat_splitat => test_splitat
        use tostring_test, only: &
            tostring_tostring_for_integers => test_tostring_for_integers, &
            tostring_tostring_for_doubles => test_tostring_for_doubles
        use iso_varying_string
        use Vegetables_m, only: TestItem_t, testThat, runTests

        type(TestItem_t) :: tests
        type(TestItem_t) :: individual_tests(6)

        individual_tests(1) = hangingindent_hangingindent()
        individual_tests(2) = indent_indent()
        individual_tests(3) = join_join()
        individual_tests(4) = splitat_splitat()
        individual_tests(5) = tostring_tostring_for_integers()
        individual_tests(6) = tostring_tostring_for_doubles()
        tests = testThat(individual_tests)

        call runTests(tests)
    end subroutine run
end program test
