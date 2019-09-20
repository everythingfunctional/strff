module strff
    implicit none
    private

    interface operator(.includes.)
        module procedure includesCC
        module procedure includesCS
        module procedure includesSC
        module procedure includesSS
    end interface operator(.includes.)

    interface coverEmptyDecimal
        module procedure coverEmptyDecimalC
        module procedure coverEmptyDecimalS
    end interface coverEmptyDecimal

    interface firstCharacter
        module procedure firstCharacterC
        module procedure firstCharacterS
    end interface firstCharacter

    interface hangingIndent
        module procedure hangingIndentC
        module procedure hangingIndentS
    end interface hangingIndent

    interface includes
        module procedure includesCC
        module procedure includesCS
        module procedure includesSC
        module procedure includesSS
    end interface includes

    interface indent
        module procedure indentC
        module procedure indentS
    end interface indent

    interface join
        module procedure joinC
        module procedure joinS
    end interface join

    interface lastCharacter
        module procedure lastCharacterC
        module procedure lastCharacterS
    end interface lastCharacter

    interface removeTrailingZeros
        module procedure removeTrailingZerosC
        module procedure removeTrailingZerosS
    end interface removeTrailingZeros

    interface splitAt
        module procedure splitAtCC
        module procedure splitAtCS
        module procedure splitAtSC
        module procedure splitAtSS
    end interface splitAt

    interface toString
        module procedure toStringInt8
        module procedure toStringInt16
        module procedure toStringInt32
        module procedure toStringInt64
        module procedure toStringReal32
        module procedure toStringReal64
        ! module procedure toStringReal128
        module procedure toStringWithSignificantDigitsReal32
        module procedure toStringWithSignificantDigitsReal64
        ! module procedure toStringWithSignificantDigitsReal128
    end interface toString

    interface withoutFirstCharacter
        module procedure withoutFirstCharacterC
        module procedure withoutFirstCharacterS
    end interface withoutFirstCharacter

    interface withoutLastCharacter
        module procedure withoutLastCharacterC
        module procedure withoutLastCharacterS
    end interface withoutLastCharacter

    character(len=*), parameter, public :: NEWLINE = NEW_LINE('A')

    public :: &
            operator(.includes.), &
            coverEmptyDecimal, &
            firstCharacter, &
            hangingIndent, &
            includes, &
            indent, &
            join, &
            lastCharacter, &
            removeTrailingZeros, &
            splitAt, &
            toString, &
            withoutFirstCharacter, &
            withoutLastCharacter
contains
    pure function coverEmptyDecimalC(number) result(fixed)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)

        character(len=*), intent(in) :: number
        type(VARYING_STRING) :: fixed

        if (lastCharacter(number) == ".") then
            fixed = number // "0"
        else if (firstCharacter(number) == ".") then
            fixed = "0" // number
        else
            fixed = number
        end if
    end function coverEmptyDecimalC

    pure function coverEmptyDecimalS(number) result(fixed)
        use ISO_VARYING_STRING, only: VARYING_STRING, char

        type(VARYING_STRING), intent(in) :: number
        type(VARYING_STRING) :: fixed

        fixed = coverEmptyDecimal(char(number))
    end function coverEmptyDecimalS

    pure function firstCharacterC(string) result(char_)
        character(len=*), intent(in) :: string
        character(len=1) :: char_

        char_ = string(1:1)
    end function firstCharacterC

    pure function firstCharacterS(string) result(char_)
        use ISO_VARYING_STRING, only: VARYING_STRING, char

        type(VARYING_STRING), intent(in) :: string
        character(len=1) :: char_

        char_ = firstCharacter(char(string))
    end function firstCharacterS

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

    pure function includesCC(within, search_for)
        character(len=*), intent(in) :: within
        character(len=*), intent(in) :: search_for
        logical :: includesCC

        includesCC = index(within, search_for) > 0
    end function includesCC

    pure function includesCS(within, search_for)
        use ISO_VARYING_STRING, only: VARYING_STRING, char

        character(len=*), intent(in) :: within
        type(VARYING_STRING), intent(in) :: search_for
        logical :: includesCS

        includesCS = within.includes.char(search_for)
    end function includesCS

    pure function includesSC(within, search_for)
        use ISO_VARYING_STRING, only: VARYING_STRING, char

        type(VARYING_STRING), intent(in) :: within
        character(len=*), intent(in) :: search_for
        logical :: includesSC

        includesSC = char(within).includes.search_for
    end function includesSC

    pure function includesSS(within, search_for)
        use ISO_VARYING_STRING, only: VARYING_STRING, char

        type(VARYING_STRING), intent(in) :: within
        type(VARYING_STRING), intent(in) :: search_for
        logical :: includesSS

        includesSS = char(within).includes.char(search_for)
    end function includesSS

    pure function indentC(string, spaces) result(indented)
        use ISO_VARYING_STRING, only: VARYING_STRING, VAR_STR

        character(len=*), intent(in) :: string
        integer, intent(in) :: spaces
        type(VARYING_STRING) :: indented

        indented = indent(VAR_STR(string), spaces)
    end function indentC

    pure function indentS(string, spaces) result(indented)
        use ISO_VARYING_STRING, only: VARYING_STRING, operator(//)

        type(VARYING_STRING), intent(in) :: string
        integer, intent(in) :: spaces
        type(VARYING_STRING) :: indented

        indented = repeat(" ", spaces) // hangingIndent(string, spaces)
    end function indentS

    pure function joinC(strings, separator) result(string)
        use ISO_VARYING_STRING, only: VARYING_STRING, VAR_STR

        type(VARYING_STRING), intent(in) :: strings(:)
        character(len=*), intent(in) :: separator
        type(VARYING_STRING) :: string

        string = join(strings, VAR_STR(separator))
    end function joinC

    pure recursive function joinS(strings, separator) result(string)
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), operator(//)

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
            string = &
                    strings(1) &
                    // separator &
                    // join(strings(2:), separator)
        end if
    end function joinS

    pure function lastCharacterC(string) result(char_)
        character(len=*), intent(in) :: string
        character(len=1) :: char_

        integer :: length

        length = len(string)
        char_ = string(length:length)
    end function lastCharacterC

    pure function lastCharacterS(string) result(char_)
        use ISO_VARYING_STRING, only: VARYING_STRING, char

        type(VARYING_STRING), intent(in) :: string
        character(len=1) :: char_

        char_ = lastCharacter(char(string))
    end function lastCharacterS

    pure function removeTrailingZerosC(number) result(trimmed)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)

        character(len=*), intent(in) :: number
        type(VARYING_STRING) :: trimmed

        if (lastCharacter(number) == "0") then
            trimmed = removeTrailingZeros(withoutLastCharacter(number))
        else
            trimmed = number
        end if
    end function removeTrailingZerosC

    pure function removeTrailingZerosS(number) result(trimmed)
        use ISO_VARYING_STRING, only: VARYING_STRING, char

        type(VARYING_STRING), intent(in) :: number
        type(VARYING_STRING) :: trimmed

        trimmed = removeTrailingZeros(char(number))
    end function removeTrailingZerosS

    pure recursive function splitAtCC( &
            string, split_characters) result(strings)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)

        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: split_characters
        type(VARYING_STRING), allocatable :: strings(:)

        if (len(split_characters) > 0) then
            if (len(string) > 0) then
                if (split_characters.includes.firstCharacter(string)) then
                    allocate(strings, source = splitAt( &
                            withoutFirstCharacter(string), &
                            split_characters))
                else if (split_characters.includes.lastCharacter(string)) then
                    allocate(strings, source = splitAt( &
                            withoutLastCharacter(string), &
                            split_characters))
                else
                    allocate(strings, source = &
                        doSplit(string, split_characters))
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
                if (split_characters_.includes.string_(i:i)) exit
            end do
            if (i < string_length_) then
                this_string = string_(1:i - 1)
                allocate(rest, source = &
                        splitAt(string_(i + 1:), split_characters_))
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

    pure function toStringInt8(number) result(string)
        use ISO_FORTRAN_ENV, only: INT8
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)

        integer(INT8), intent(in) :: number
        type(VARYING_STRING) :: string

        character(len=4) :: temp

        write(temp, '(I0)') number
        string = trim(temp)
    end function toStringInt8

    pure function toStringInt16(number) result(string)
        use ISO_FORTRAN_ENV, only: INT16
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)

        integer(INT16), intent(in) :: number
        type(VARYING_STRING) :: string

        character(len=6) :: temp

        write(temp, '(I0)') number
        string = trim(temp)
    end function toStringInt16

    pure function toStringInt32(number) result(string)
        use ISO_FORTRAN_ENV, only: INT32
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)

        integer(INT32), intent(in) :: number
        type(VARYING_STRING) :: string

        character(len=11) :: temp

        write(temp, '(I0)') number
        string = trim(temp)
    end function toStringInt32

    pure function toStringInt64(number) result(string)
        use ISO_FORTRAN_ENV, only: INT64
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)

        integer(INT64), intent(in) :: number
        type(VARYING_STRING) :: string

        character(len=20) :: temp

        write(temp, '(I0)') number
        string = trim(temp)
    end function toStringInt64

    pure function toStringReal32(number) result(string)
        use ISO_FORTRAN_ENV, only: REAL32
        use ISO_VARYING_STRING, only: VARYING_STRING

        real(REAL32), intent(in) :: number
        type(VARYING_STRING) :: string

        string = toString(number, 9)
    end function toStringReal32

    pure function toStringReal64(number) result(string)
        use ISO_FORTRAN_ENV, only: REAL64
        use ISO_VARYING_STRING, only: VARYING_STRING

        real(REAL64), intent(in) :: number
        type(VARYING_STRING) :: string

        string = toString(number, 17)
    end function toStringReal64

    ! pure function toStringReal128(number) result(string)
    !     use ISO_FORTRAN_ENV, only: REAL128
    !     use ISO_VARYING_STRING, only: VARYING_STRING
    !
    !     real(REAL128), intent(in) :: number
    !     type(VARYING_STRING) :: string
    !
    !     string = toString(number, 36)
    ! end function toStringReal128

    pure function toStringWithSignificantDigitsReal32( &
            number, significant_digits) result(string_)
        use ISO_FORTRAN_ENV, only: REAL32
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), operator(//), len

        real(REAL32), intent(in) :: number
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string_

        integer, parameter :: C_LEN = 18
        real(REAL32), parameter :: MACHINE_TINY = TINY(real(0.0, kind=REAL32))
        real(REAL32) :: abs_num
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
            write(format_string, '(A,I0,A)') &
                    "(f0.", significant_digits-1, ")"
            write(floating_part, format_string) &
                    abs_num * 1.0D1**(-scale_)
            write(exponent_part, '(A,I0)') 'e', scale_
            intermediate = &
                    coverEmptyDecimal( &
                            removeTrailingZeros(trim(floating_part))) &
                    // trim(exponent_part)
        else
            write(format_string, '(A,I0,A)') &
                    "(f0.", significant_digits-1, ")"
            write(floating_part, format_string) abs_num / 1.0D1**scale_
            write(exponent_part, '(A,I0)') 'e', scale_
            intermediate_scientific = &
                    coverEmptyDecimal( &
                            removeTrailingZeros(trim(floating_part))) &
                    // trim(exponent_part)

            if (scale_ < significant_digits) then
                write(format_string, '(A,I0,A)') &
                        "(f0.", significant_digits-scale_-1, ")"
                write(floating_part, format_string) abs_num
                intermediate_basic = coverEmptyDecimal( &
                        removeTrailingZeros(trim(floating_part)))

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
    end function toStringWithSignificantDigitsReal32

    pure function toStringWithSignificantDigitsReal64( &
            number, significant_digits) result(string_)
        use ISO_FORTRAN_ENV, only: REAL64
        use ISO_VARYING_STRING, only: &
                VARYING_STRING, assignment(=), operator(//), len

        real(REAL64), intent(in) :: number
        integer, intent(in) :: significant_digits
        type(VARYING_STRING) :: string_

        integer, parameter :: C_LEN = 34
        real(REAL64), parameter :: MACHINE_TINY = TINY(real(0.0, kind=REAL64))
        real(REAL64) :: abs_num
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
            write(format_string, '(A,I0,A)') &
                    "(f0.", significant_digits-1, ")"
            write(floating_part, format_string) &
                    abs_num * 1.0D1**(-scale_)
            write(exponent_part, '(A,I0)') 'e', scale_
            intermediate = &
                    coverEmptyDecimal( &
                            removeTrailingZeros(trim(floating_part))) &
                    // trim(exponent_part)
        else
            write(format_string, '(A,I0,A)') &
                    "(f0.", significant_digits-1, ")"
            write(floating_part, format_string) abs_num / 1.0D1**scale_
            write(exponent_part, '(A,I0)') 'e', scale_
            intermediate_scientific = &
                    coverEmptyDecimal( &
                            removeTrailingZeros(trim(floating_part))) &
                    // trim(exponent_part)

            if (scale_ < significant_digits) then
                write(format_string, '(A,I0,A)') &
                        "(f0.", significant_digits-scale_-1, ")"
                write(floating_part, format_string) abs_num
                intermediate_basic = coverEmptyDecimal( &
                        removeTrailingZeros(trim(floating_part)))

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
    end function toStringWithSignificantDigitsReal64

    ! pure function toStringWithSignificantDigitsReal128( &
    !         number, significant_digits) result(string_)
    !     use ISO_FORTRAN_ENV, only: REAL128
    !     use ISO_VARYING_STRING, only: &
    !             VARYING_STRING, assignment(=), operator(//), len
    !
    !     real(REAL128), intent(in) :: number
    !     integer, intent(in) :: significant_digits
    !     type(VARYING_STRING) :: string_
    !
    !     integer, parameter :: C_LEN = 72
    !     real(REAL128), parameter :: MACHINE_TINY = TINY(real(0.0, kind=REAL128))
    !     real(REAL128) :: abs_num
    !     character(len=C_LEN) :: exponent_part
    !     character(len=C_LEN) :: floating_part
    !     character(len=7) :: format_string
    !     type(VARYING_STRING) :: intermediate
    !     type(VARYING_STRING) :: intermediate_basic
    !     type(VARYING_STRING) :: intermediate_scientific
    !     integer :: scale_
    !
    !     abs_num = abs(number)
    !     if (abs_num <= MACHINE_TINY) then
    !         string_ = "0.0"
    !         return
    !     end if
    !     scale_ = floor(log10(abs_num))
    !     if (scale_ <= -2) then
    !         write(format_string, '(A,I0,A)') &
    !                 "(f0.", significant_digits-1, ")"
    !         write(floating_part, format_string) &
    !                 abs_num * 1.0D1**(-scale_)
    !         write(exponent_part, '(A,I0)') 'e', scale_
    !         intermediate = &
    !                 coverEmptyDecimal( &
    !                         removeTrailingZeros(trim(floating_part))) &
    !                 // trim(exponent_part)
    !     else
    !         write(format_string, '(A,I0,A)') &
    !                 "(f0.", significant_digits-1, ")"
    !         write(floating_part, format_string) abs_num / 1.0D1**scale_
    !         write(exponent_part, '(A,I0)') 'e', scale_
    !         intermediate_scientific = &
    !                 coverEmptyDecimal( &
    !                         removeTrailingZeros(trim(floating_part))) &
    !                 // trim(exponent_part)
    !
    !         if (scale_ < significant_digits) then
    !             write(format_string, '(A,I0,A)') &
    !                     "(f0.", significant_digits-scale_-1, ")"
    !             write(floating_part, format_string) abs_num
    !             intermediate_basic = coverEmptyDecimal( &
    !                     removeTrailingZeros(trim(floating_part)))
    !
    !             if (len(intermediate_scientific) < len(intermediate_basic)) then
    !                 intermediate = intermediate_scientific
    !             else
    !                 intermediate = intermediate_basic
    !             end if
    !         else
    !             intermediate = intermediate_scientific
    !         end if
    !     end if
    !     if (number < 0.0D0) then
    !         string_ = "-" // intermediate
    !     else
    !         string_ = intermediate
    !     end if
    ! end function toStringWithSignificantDigitsReal128

    pure function withoutFirstCharacterC(string) result(trimmed)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)

        character(len=*), intent(in) :: string
        type(VARYING_STRING) :: trimmed

        trimmed = string(2:)
    end function withoutFirstCharacterC

    pure function withoutFirstCharacterS(string) result(trimmed)
        use ISO_VARYING_STRING, only: VARYING_STRING, char

        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: trimmed

        trimmed = withoutFirstCharacter(char(string))
    end function withoutFirstCharacterS

    pure function withoutLastCharacterC(string) result(trimmed)
        use ISO_VARYING_STRING, only: VARYING_STRING, assignment(=)

        character(len=*), intent(in) :: string
        type(VARYING_STRING) :: trimmed

        trimmed = string(1:len(string) - 1)
    end function withoutLastCharacterC

    pure function withoutLastCharacterS(string) result(trimmed)
        use ISO_VARYING_STRING, only: VARYING_STRING, char

        type(VARYING_STRING), intent(in) :: string
        type(VARYING_STRING) :: trimmed

        trimmed = withoutLastCharacter(char(string))
    end function withoutLastCharacterS
end module strff
