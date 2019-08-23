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

    interface lastCharacter
        module procedure lastCharacterC
        module procedure lastCharacterS
    end interface lastCharacter

    interface splitAt
        module procedure splitAtCC
        module procedure splitAtCS
        module procedure splitAtSC
        module procedure splitAtSS
    end interface splitAt

    interface toString
        module procedure toStringDouble
        module procedure toStringInteger
        module procedure toStringWithSignificantDigits
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

    pure recursive function joinS(strings, separator) result(string)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=), operator(//)

        type(VARYING_STRING), intent(in) :: strings(:)
        type(VARYING_STRING), intent(in) :: separator
        type(VARYING_STRING) :: string

        integer :: num_strings

        num_strings = size(strings)
        if (num_strings == 1) then
            string = strings(1)
        else if (num_strings == 0) then
            string = ""
        else
            string = strings(1) // separator // join(strings(2:), separator)
        end if
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
                allocate(strings(0))
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

    pure function toStringDouble(number) result(string)
        use ISO_VARYING_STRING, only: VARYING_STRING

        double precision, intent(in) :: number
        type(VARYING_STRING) :: string

        string = toString(number, 16)
    end function toStringDouble

    pure function toStringInteger(number) result(string)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)

        integer, intent(in) :: number
        type(VARYING_STRING) :: string

        character(len=32) :: temp

        write(temp, '(I0)') number
        string = trim(temp)
    end function toStringInteger

    pure function toStringWithSignificantDigits(number, significant_digits) result(string_)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), operator(//), len

        double precision, intent(in) :: number
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string_

        integer, parameter :: C_LEN = 32
        double precision, parameter :: MACHINE_TINY = TINY(0.0D0)
        double precision :: abs_num
        character(len=C_LEN) :: exponent_part
        character(len=C_LEN) :: floating_part
        character(len=7) :: format_string
        type(VARYING_STRING) :: intermediate
        type(VARYING_STRING) :: intermediate_basic
        type(VARYING_STRING) :: intermediate_scientific
        integer :: scale_

        abs_num = abs(number)
        if (abs_num <= MACHINE_TINY) then
            string_ = "0.0"
            return
        end if
        scale_ = floor(log10(abs_num))
        if (scale_ <= -2) then
            write(format_string, '(A,I0,A)') "(f0.", significant_digits-1, ")"
            write(floating_part, format_string) abs_num * 1.0D1**(-scale_)
            write(exponent_part, '(A,I0)') 'e', scale_
            intermediate = coverEmptyDecimal(removeTrailingZeros(floating_part)) // trim(exponent_part)
        else
            write(format_string, '(A,I0,A)') "(f0.", significant_digits-1, ")"
            write(floating_part, format_string) abs_num / 1.0D1**scale_
            write(exponent_part, '(A,I0)') 'e', scale_
            intermediate_scientific = coverEmptyDecimal(removeTrailingZeros(floating_part)) // trim(exponent_part)

            if (scale_ < significant_digits) then
                write(format_string, '(A,I0,A)') "(f0.", significant_digits-scale_-1, ")"
                write(floating_part, format_string) abs_num
                intermediate_basic = coverEmptyDecimal(removeTrailingZeros(floating_part))

                if (len(intermediate_scientific) < len(intermediate_basic)) then
                    intermediate = intermediate_scientific
                else
                    intermediate = intermediate_basic
                end if
            else
                intermediate = intermediate_scientific
            end if
        end if
        if (number < 0.0D0) then
            string_ = "-" // intermediate
        else
            string_ = intermediate
        end if
    end function toStringWithSignificantDigits

    pure function removeTrailingZeros(number) result(trimmed)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, &
                assignment(=), &
                extract, &
                len

        character(len=*), intent(in) :: number
        type(VARYING_STRING) :: trimmed

        trimmed = trim(number)
        do while (lastCharacter(trimmed) == "0")
            trimmed = extract(trimmed, 1, len(trimmed) - 1)
        end do
    end function removeTrailingZeros

    pure function lastCharacterC(string) result(char_)
        character(len=*), intent(in) :: string
        character(len=1) :: char_

        integer :: length

        length = len(trim(string))
        char_ = string(length:length)
    end function lastCharacterC

    pure function lastCharacterS(string) result(char_)
        use ISO_VARYING_STRING, only: VARYING_STRING, char

        type(VARYING_STRING), intent(in) :: string
        character(len=1) :: char_

        char_ = lastCharacter(char(string))
    end function lastCharacterS

    pure function coverEmptyDecimal(number) result(fixed)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, &
                assignment(=), &
                operator(==), &
                operator(//), &
                extract

        type(VARYING_STRING), intent(in) :: number
        type(VARYING_STRING) :: fixed

        if (lastCharacter(number) == ".") then
            fixed = number // "0"
        else if (extract(number, 1, 1) == ".") then
            fixed = "0" // number
        else
            fixed = number
        end if
    end function coverEmptyDecimal
end module strff
