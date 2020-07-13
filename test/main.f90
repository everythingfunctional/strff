program main
    use hangingindent_test, only: &
            hangingindent_hangingIndent => test_hangingIndent
    use indent_test, only: &
            indent_indent => test_indent
    use join_test, only: &
            join_join => test_join
    use readfilelines_test, only: &
            readfilelines_readfilelines => test_readfilelines
    use readfile_test, only: &
            readfile_readfile => test_readfile
    use splitat_test, only: &
            splitat_splitAt => test_splitAt
    use startswith_test, only: &
            startswith_startswith => test_startswith
    use tostring_test, only: &
            tostring_toString_for_doubles => test_toString_for_doubles, &
            tostring_toString_for_integers => test_toString_for_integers
    use Vegetables_m, only: TestItem_t, testThat, runTests

    implicit none

    call run()
contains
    subroutine run()
        type(TestItem_t) :: tests
        type(TestItem_t) :: individual_tests(9)

        individual_tests(1) = hangingindent_hangingIndent()
        individual_tests(2) = indent_indent()
        individual_tests(3) = join_join()
        individual_tests(4) = readfilelines_readfilelines()
        individual_tests(5) = readfile_readfile()
        individual_tests(6) = splitat_splitAt()
        individual_tests(7) = startswith_startswith()
        individual_tests(8) = tostring_toString_for_doubles()
        individual_tests(9) = tostring_toString_for_integers()
        tests = testThat(individual_tests)

        call runTests(tests)
    end subroutine run
end program
