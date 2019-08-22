module strff
    implicit none
    private

    public :: splitAt
contains
    pure function splitAt(string, split_characters) result(strings)
        use ISO_VARYING_STRING, only: VARYING_STRING, VAR_STR

        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: split_characters
        type(VARYING_STRING), allocatable :: strings(:)

        associate(a => split_characters); end associate
        allocate(strings, source = [VAR_STR(string)])
    end function splitAt
end module strff
