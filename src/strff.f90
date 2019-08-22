module strff
    implicit none
    private

    interface hangingIndent
        module procedure hangingIndentC
        module procedure hangingIndentS
    end interface hangingIndent

    interface join
        module procedure joinC
        module procedure joinS
    end interface join

    interface splitAt
        module procedure splitAtCC
        module procedure splitAtCS
        module procedure splitAtSC
        module procedure splitAtSS
    end interface splitAt

    interface toString
        module procedure toStringInteger
    end interface toString

    character(len=*), parameter, public :: NEWLINE = NEW_LINE('A')

    public :: hangingIndent, join, splitAt, toString
contains
    pure function hangingIndentC(string, spaces) result(indented)
        use ISO_VARYING_STRING, only: VARYING_STRING, VAR_STR

        character(len=*), intent(in) :: string
        integer, intent(in) :: spaces
        type(VARYING_STRING) :: indented

        indented = hangingIndent(VAR_STR(string), spaces)
    end function hangingIndentC

    pure function hangingIndentS(string, spaces) result(indented)
        use ISO_VARYING_STRING, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: spaces
        type(VARYING_STRING) :: indented

        type(VARYING_STRING), allocatable :: lines(:)

        allocate(lines, source = splitAt(string, NEWLINE))
        indented = join(lines, NEWLINE // repeat(" ", spaces))
    end function hangingIndentS

    pure function joinC(strings, separator) result(string)
        use ISO_VARYING_STRING, only: VARYING_STRING, VAR_STR

        type(VARYING_STRING), intent(in) :: strings(:)
        character(len=*), intent(in) :: separator
        type(VARYING_STRING) :: string

        string = join(strings, VAR_STR(separator))
    end function joinC

    pure function joinS(strings, separator) result(string)
        use ISO_VARYING_STRING, only: VARYING_STRING, operator(//)

        type(VARYING_STRING), intent(in) :: strings(:)
        type(VARYING_STRING), intent(in) :: separator
        type(VARYING_STRING) :: string

        integer :: i

        string = strings(1)
        do i = 2, size(strings)
            string = string // separator // strings(i)
        end do
    end function joinS

    pure recursive function splitAtCC(string, split_characters) result(strings)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)

        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: split_characters
        type(VARYING_STRING), allocatable :: strings(:)

        integer :: string_length

        if (len(split_characters) > 0) then
            string_length = len(string)
            if (string_length > 0) then
                if (index(split_characters, string(1:1)) > 0) then
                    allocate(strings, source = splitAt(string(2:), split_characters))
                else if (index(split_characters, string(string_length:string_length)) > 0) then
                    allocate(strings, source = splitAt(string(1:string_length - 1), split_characters))
                else
                    allocate(strings, source = doSplit(string, split_characters))
                end if
            else
                allocate(strings(1))
                strings(1) = ""
            end if
        else
            allocate(strings(1))
            strings(1) = string
        end if
    contains
        pure function doSplit(string_, split_characters_) result(strings_)
            character(len=*), intent(in) :: string_
            character(len=*), intent(in) :: split_characters_
            type(VARYING_STRING), allocatable :: strings_(:)

            integer :: i
            type(VARYING_STRING), allocatable :: rest(:)
            integer :: string_length_
            type(VARYING_STRING) :: this_string

            string_length_ = len(string_)
            do i = 2, string_length_
                if (index(split_characters_, string_(i:i)) > 0) exit
            end do
            if (i < string_length_) then
                this_string = string_(1:i - 1)
                allocate(rest, source = splitAt(string_(i + 1:), split_characters_))
                allocate(strings_(size(rest) + 1))
                strings_(1) = this_string
                strings_(2:) = rest(:)
            else
                allocate(strings_(1))
                strings_(1) = string_
            end if
        end function doSplit
    end function splitAtCC

    pure function splitAtCS(string, split_characters) result(strings)
        use ISO_VARYING_STRING, only: VARYING_STRING, char

        character(len=*), intent(in) :: string
        type(VARYING_STRING), intent(in) :: split_characters
        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt(string, char(split_characters)))
    end function splitAtCS

    pure function splitAtSC(string, split_characters) result(strings)
        use ISO_VARYING_STRING, only: VARYING_STRING, char

        type(VARYING_STRING), intent(in) :: string
        character(len=*), intent(in) :: split_characters
        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt(char(string), split_characters))
    end function splitAtSC

    pure function splitAtSS(string, split_characters) result(strings)
        use ISO_VARYING_STRING, only: VARYING_STRING, char

        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING), intent(in) :: split_characters
        type(VARYING_STRING), allocatable :: strings(:)

        allocate(strings, source = splitAt(char(string), char(split_characters)))
    end function splitAtSS

    pure function toStringInteger(number) result(string)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)

        integer, intent(in) :: number
        type(VARYING_STRING) :: string

        character(len=32) :: temp

        write(temp, '(I0)') number
        string = trim(temp)
    end function toStringInteger
end module strff
