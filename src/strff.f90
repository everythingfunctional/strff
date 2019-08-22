module strff
    implicit none
    private

    public :: splitAt
contains
    pure recursive function splitAt(string, split_characters) result(strings)
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
    end function splitAt
end module strff
