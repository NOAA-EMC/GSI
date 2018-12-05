! Copyright (c) 2012 Joseph A. Levin
!
! Permission is hereby granted, free of charge, to any person obtaining a copy of this
! software and associated documentation files (the "Software"), to deal in the Software
! without restriction, including without limitation the rights to use, copy, modify, merge,
! publish, distribute, sublicense, and/or sell copies of the Software, and to permit 
! persons to whom the Software is furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all copies or 
! substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
! INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
! PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE 
! LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT
! OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
! DEALINGS IN THE SOFTWARE.


! FSON MODULE 
!
! File:   nc_diag_fson.f95
! Author: Joseph A. Levin
!
! Created on March 6, 2012, 7:48 PM
!

module nc_diag_fson
    use ncdf_value_m, ncdf_print => ncdf_value_print, ncdf_destroy => ncdf_value_destroy
    use ncdf_string_m
    use ncdf_path_m, ncdf_get => ncdf_path_get

    implicit none

    private

    public :: ncdf_parse, ncdf_value, ncdf_get, ncdf_print, ncdf_destroy

    ! FILE IOSTAT CODES
    integer, parameter :: end_of_file = -1
    integer, parameter :: end_of_record = -2

    ! PARSING STATES
    integer, parameter :: STATE_LOOKING_FOR_VALUE = 1
    integer, parameter :: STATE_IN_OBJECT = 2
    
    integer, parameter :: STATE_IN_PAIR_NAME = 3
    integer, parameter :: STATE_IN_PAIR_VALUE = 4

    ! POP/PUSH CHARACTER
    integer :: pushed_index = 0
    character (len = 10) :: pushed_char

contains

    !
    ! FSON PARSE
    !
    function ncdf_parse(file, unit, str) result(p)
        type(ncdf_value), pointer :: p
        integer, optional, intent(inout) :: unit
        character(len = *), optional, intent(in) :: file
        character(len = *), optional, intent(in) :: str
        character(len=:),allocatable :: strBuffer
        logical :: unit_available
        integer :: u
        ! init the pointer to null
        nullify(p)

        ! select the file unit to use
        if (present(unit) .and. present(file)) then
            u = unit
        elseif (present(file)) then
            ! find the first available unit
            unit_available = .false.
            u = 20

            do while (.not.unit_available)
                inquire(unit = u, exist = unit_available)
                u = u + 1
            end do
        elseif (present(str)) then
            strBuffer = str
            u = 0
        else 
            print *, "ERROR: Need a file or a string"
            call exit (1)
        end if

        ! open the file
        if (present(file)) then
            open (unit = u, file = file, status = "old", action = "read", form = "formatted", position = "rewind")
        end if

        ! create the value and associate the pointer        
        p => ncdf_value_create()

        ! parse as a value
        call ncdf_parse_value(unit = u, value = p, str = strBuffer)

        ! close the file
        if( .not. present(unit)) then
            close (u)
        end if

        if(allocated(strBuffer)) deallocate(strBuffer)

    end function ncdf_parse

    !
    ! PARSE_VALUE
    !
    recursive subroutine ncdf_parse_value(unit, str, value)
        integer, intent(inout) :: unit
        character(*), intent(inout) :: str
        type(ncdf_value), pointer :: value
        logical :: eof
        character :: c

        ! pop the next non whitespace character off the file
        c = ncdf_pop_char(unit, str, eof = eof, skip_ws = .true.)

        if (eof) then
            return
        else
            select case (c)
            case ("{")
                ! start object                
                value % value_type = TYPE_OBJECT
                call ncdf_parse_object(unit, str, value)
            case ("[")
                ! start array
                value % value_type = TYPE_ARRAY
                call ncdf_parse_array(unit, str, value)
            case ("]")
                ! end an empty array
               call ncdf_push_char(c)
               nullify(value)
            case ('"')
                ! string                                      
                value % value_type = TYPE_STRING
                value % value_string => ncdf_parse_string(unit, str)
            case ("t")
                !true
                value % value_type = TYPE_LOGICAL
                call ncdf_parse_for_chars(unit, str, "rue")
                value % value_logical = .true.
            case ("f")
                !false
                value % value_type = TYPE_LOGICAL
                value % value_logical = .false.
                call ncdf_parse_for_chars(unit, str, "alse")
            case ("n")
                value % value_type = TYPE_NULL
                call ncdf_parse_for_chars(unit, str, "ull")
            case("-", "0": "9")
                call ncdf_push_char(c)
                call ncdf_parse_number(unit, str, value)
            case default
                print *, "ERROR: Unexpected character while parsing value. '", c, "' ASCII=", iachar(c)
                call exit (1)
            end select
        end if

    end subroutine ncdf_parse_value

    !
    ! PARSE OBJECT
    !    
    recursive subroutine ncdf_parse_object(unit, str, parent)
        integer, intent(inout) :: unit
        character(*), intent(inout) :: str
        type(ncdf_value), pointer :: parent, pair


        logical :: eof
        character :: c

        ! pair name
        c = ncdf_pop_char(unit, str, eof = eof, skip_ws = .true.)
        if (eof) then
            print *, "ERROR: Unexpected end of file while parsing start of object."
            call exit (1)
        else if ("}" == c) then
            ! end of an empty object
            return
        else if ('"' == c) then
            pair => ncdf_value_create()
            pair % name => ncdf_parse_string(unit, str)
        else
            print *, "ERROR: Expecting string: '", c, "'"
            call exit (1)
        end if

        ! pair value
        c = ncdf_pop_char(unit, str, eof = eof, skip_ws = .true.)
        if (eof) then
            print *, "ERROR: Unexpected end of file while parsing object member. 1"
            call exit (1)
        else if (":" == c) then
            ! parse the value                       
            call ncdf_parse_value(unit, str, pair)
            call ncdf_value_add(parent, pair)
        else
            print *, "ERROR: Expecting : and then a value. ", c
            call exit (1)
        end if

        ! another possible pair
        c = ncdf_pop_char(unit, str, eof = eof, skip_ws = .true.)
        if (eof) then
            return
        else if ("," == c) then
            ! read the next member            
            call ncdf_parse_object(unit = unit, str=str, parent = parent)
        else if ("}" == c) then
            return
        else
            print *, "ERROR: Expecting end of object.", c
            call exit (1)
        end if

    end subroutine ncdf_parse_object

    !
    ! PARSE ARRAY
    !    
    recursive subroutine ncdf_parse_array(unit, str, array)

      implicit none
      integer, intent(inout) :: unit
      character(*), intent(inout) :: str
      type(ncdf_value), pointer :: array, element

      logical :: eof, finished
      character :: c

      finished = .false.
      do while (.not. finished)

         ! try to parse an element value
         element => ncdf_value_create()
         call ncdf_parse_value(unit, str, element)

         ! parse value will disassociate an empty array value
         if (associated(element)) then
            call ncdf_value_add(array, element)
         end if

         ! pop the next character
         c = ncdf_pop_char(unit, str, eof = eof, skip_ws = .true.)

         if (eof) then
            finished = .true.
         else if ("]" == c) then
            ! end of array
            finished = .true.
         end if

      end do

    end subroutine ncdf_parse_array

    !
    ! PARSE STRING
    !
    function ncdf_parse_string(unit, str) result(string)
        integer, intent(inout) :: unit
        character(*), intent(inout) :: str
        type(ncdf_string), pointer :: string

        logical :: eof, escape
        character :: c

        string => ncdf_string_create()
        escape = .false.

        do
            c = ncdf_pop_char(unit, str, eof = eof, skip_ws = .false.)
            if (eof) then
               print *, "Expecting end of string"
               call exit(1)
            else if (escape) then
              call ncdf_string_append(string,c)
              escape = .false.
            else
               if (c == '\\') then
                  escape = .true.
               else if (c == '\"') then
                  exit
               else
                  call ncdf_string_append(string,c)
               end if
            end if
        end do
    end function ncdf_parse_string

    !
    ! PARSE FOR CHARACTERS
    !
    subroutine ncdf_parse_for_chars(unit, str, chars)
        integer, intent(in) :: unit
        character(*), intent(inout) :: str
        character(len = *), intent(in) :: chars
        integer :: i, length
        logical :: eof
        character :: c

        length = len_trim(chars)

        do i = 1, length
            c = ncdf_pop_char(unit, str, eof = eof, skip_ws = .true.)
            if (eof) then
                print *, "ERROR: Unexpected end of file while parsing array."
                call exit (1)
            else if (c .ne. chars(i:i)) then
                print *, "ERROR: Unexpected character.'", c,"'", chars(i:i)
                call exit (1)
            end if
        end do

    end subroutine ncdf_parse_for_chars

    !
    ! PARSE NUMBER
    !
    subroutine ncdf_parse_number(unit, str, value)
        integer, intent(inout) :: unit
        character(*), intent(inout) :: str
        type(ncdf_value), pointer :: value
        logical :: eof, negative, decimal, scientific
        character :: c
        integer :: integral, exp, digit_count
        double precision :: frac


        ! first character is either - or a digit        
        c = ncdf_pop_char(unit, str, eof = eof, skip_ws = .true.)
        if (eof) then
            print *, "ERROR: Unexpected end of file while parsing number."
            call exit (1)
        else if ("-" == c) then
            negative = .true.
        else
            negative = .false.
            call ncdf_push_char(c)
        end if


        ! parse the integral
        integral = ncdf_parse_integer(unit, str)

        decimal = .false.
        scientific = .false.

        do
            ! first character is either - or a digit        
            c = ncdf_pop_char(unit, str, eof = eof, skip_ws = .true.)
            if (eof) then
                print *, "ERROR: Unexpected end of file while parsing number."
                call exit (1)
            else
                select case (c)
                case (".")
                    ! this is already fractional number
                    if (decimal) then
                        ! already found a decimal place
                        print *, "ERROR: Unexpected second decimal place while parsing number."
                        call exit(1)
                    end if
                    decimal = .true.
                    frac = ncdf_parse_integer(unit, str, digit_count)
                    frac = frac / (10.0d0 ** digit_count)
                case ("e", "E")
                    ! this is already an exponent number
                    if (scientific) then
                        ! already found a e place
                        print *, "ERROR: Unexpected second exponent while parsing number."
                        call exit(1)
                    end if
                    scientific = .true.
                    ! this number has an exponent
                    exp = ncdf_parse_integer(unit, str)
                    if (exp < 0) then
                       decimal = .true.
                    end if

                case default
                    ! this is a integer
                    if (decimal) then

                        ! add the integral
                        frac = frac + integral

                        if (scientific) then
                            ! apply exponent
                            frac = frac * (10.0d0 ** exp)
                        end if

                        ! apply negative
                        if (negative) then
                            frac = frac * (-1)
                        end if

                        value % value_type = TYPE_REAL
                        value % value_real = frac
                        value % value_double = frac

                    else
                        if (scientific) then
                        ! apply exponent
                        integral = integral * (10.0d0 ** exp)
                        end if

                        ! apply negative
                        if (negative) then
                        integral = integral * (-1)
                        end if

                        value % value_type = TYPE_INTEGER
                        value % value_integer = integral
                    end if
                    call ncdf_push_char(c)
                    exit
                end select
            end if
        end do



    end subroutine

    !
    ! PARSE INTEGER    
    !
    integer(kind=8) function ncdf_parse_integer(unit, str, digit_count) result(integral)
        integer, intent(in) :: unit
        character(*), intent(inout) :: str
        integer, optional, intent(inout) :: digit_count
        logical :: eof, found_sign, found_digit
        character :: c
        integer :: tmp, icount, isign
        integer, parameter :: max_integer_length = 18


        icount = 0
        integral = 0
        isign = 1
        found_sign = .false.
        found_digit = .false.
        do
            c = ncdf_pop_char(unit, str, eof = eof, skip_ws = .true.)
            if (eof) then
                print *, "ERROR: Unexpected end of file while parsing digit."
                call exit (1)
            else
                select case(c)
                case ("+")
                    if (found_sign.or.found_digit) then
                        print *, "ERROR: Miss formatted number."
                        call exit(1)
                    end if
                    found_sign = .true.
                case ("-")
                    if (found_sign.or.found_digit) then
                        print *, "ERROR: Miss formatted number."
                        call exit(1)
                    end if
                    found_sign = .true.
                    isign = -1
                case ("0":"9")
                    found_sign = .true.
                    if (icount > max_integer_length) then
                        print *, "ERROR: Too many digits for an integer."
                        call exit(1)
                    end if
                    ! digit        
                    read (c, '(i1)') tmp
                    ! shift
                    if (icount > 0) then
                        integral = integral * 10
                    end if
                    ! add
                    integral = integral + tmp

                    ! increase the icount
                    icount = icount + 1
                case default
                    if (present(digit_count)) then
                        digit_count = icount
                    end if
                    call ncdf_push_char(c)
                    integral = isign * integral
                    return
                end select
            end if
        end do

    end function ncdf_parse_integer

    !
    ! POP CHAR
    !
    recursive character function ncdf_pop_char(unit, str, eof, skip_ws) result(popped)
        integer, intent(in) :: unit
        character(*), intent(inout) :: str
        logical, intent(out) :: eof
        logical, intent(in), optional :: skip_ws

        integer :: ios
        character :: c
        logical :: ignore

        eof = .false.
        if (.not.present(skip_ws)) then
            ignore = .false.
        else
            ignore = skip_ws
        end if

        do
            if (pushed_index > 0) then
                ! there is a character pushed back on, most likely from the number parsing                
                c = pushed_char(pushed_index:pushed_index)
                pushed_index = pushed_index - 1
                ios = 0
            else
                if (unit .gt. 0) then
                    read (unit = unit, fmt = "(a)", advance = "no", iostat = ios) c
                else
                    read (unit = str, fmt = "(a)", iostat = ios) c
                    str = str(2:)
                endif
            end if
            if (ios == end_of_record) then
                cycle            
            else if (ios == end_of_file) then
                eof = .true.
                exit
            else if (iachar(c) <= 31) then
                ! non printing ascii characters
                cycle
            else if (ignore .and. c == " ") then
                cycle
            else
                popped = c
                exit
            end if
        end do

    end function ncdf_pop_char

    !
    ! PUSH CHAR
    !
    subroutine ncdf_push_char(c)
        character, intent(inout) :: c
        pushed_index = pushed_index + 1
        pushed_char(pushed_index:pushed_index) = c

    end subroutine ncdf_push_char

end module nc_diag_fson
