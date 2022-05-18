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

!     
! File:   string.f95
! Author: josephalevin
!
! Created on March 7, 2012, 7:40 PM
!

module ncdf_string_m

    private

    public :: ncdf_string, ncdf_string_create, ncdf_string_destroy, ncdf_ncdf_string_length, ncdf_string_append,&
              ncdf_string_clear
    public :: ncdf_string_equals, ncdf_string_copy

    integer, parameter :: BLOCK_SIZE = 32

    type ncdf_string
        character (len = BLOCK_SIZE) :: chars
        integer :: index = 0
        type(ncdf_string), pointer :: next => null()
    end type ncdf_string

    interface ncdf_string_append
        module procedure ncdf_append_chars, ncdf_append_string
    end interface ncdf_string_append

    interface ncdf_string_copy
        module procedure ncdf_copy_chars
    end interface ncdf_string_copy

    interface ncdf_string_equals
        module procedure ncdf_equals_string
    end interface ncdf_string_equals
    
    interface ncdf_ncdf_string_length
        module procedure ncdf_string_length
    end interface ncdf_ncdf_string_length

contains

    !
    ! FSON STRING CREATE
    !
    function ncdf_string_create(chars) result(new)
        character(len=*), optional :: chars
        type(ncdf_string), pointer :: new

        nullify(new)
        allocate(new)
        
        ! append chars if available
        if(present(chars)) then
            call ncdf_append_chars(new, chars)
        end if

    end function ncdf_string_create
    
    !
    ! FSON STRING CREATE
    !
    recursive subroutine ncdf_string_destroy(this)

      implicit none
      type(ncdf_string), pointer :: this

      if (associated(this)) then

         if(associated(this % next)) then
            call ncdf_string_destroy(this % next)
         end if

         deallocate(this)
         nullify (this)

      end if

    end subroutine ncdf_string_destroy

    !
    ! ALLOCATE BLOCK
    !
    subroutine ncdf_allocate_block(this)

      implicit none
      type(ncdf_string), pointer :: this
      type(ncdf_string), pointer :: new

      if (.not.associated(this % next)) then
         nullify(new)
         allocate(new)
         this % next => new
      end if

    end subroutine ncdf_allocate_block


    !
    ! APPEND_STRING
    !
    subroutine ncdf_append_string(str1, str2)
        type(ncdf_string), pointer :: str1, str2
        integer length, i

        length = ncdf_string_length(str2)

        do i = 1, length
            call ncdf_append_char(str1, ncdf_get_char_at(str2, i))
        end do


    end subroutine ncdf_append_string

    !
    ! APPEND_CHARS
    !
    subroutine ncdf_append_chars(str, c)
        type(ncdf_string), pointer :: str
        character (len = *), intent(in) :: c
        integer length, i

        length = len(c)

        do i = 1, length
            call ncdf_append_char(str, c(i:i))
        end do


    end subroutine ncdf_append_chars

    !
    ! APPEND_CHAR
    !
    recursive subroutine ncdf_append_char(str, c)
        type(ncdf_string), pointer :: str
        character, intent(in) :: c

        if (str % index .GE. BLOCK_SIZE) then
            !set down the chain
            call ncdf_allocate_block(str)
            call ncdf_append_char(str % next, c)

        else
            ! set local
            str % index = str % index + 1
            str % chars(str % index:str % index) = c
        end if

    end subroutine ncdf_append_char

    !
    ! COPY CHARS
    !
    subroutine ncdf_copy_chars(this, to)
        type(ncdf_string), pointer :: this
        character(len = *), intent(inout) :: to
        integer :: i, length

        length = min(ncdf_string_length(this), len(to))

        do i = 1, length
            to(i:i) = ncdf_get_char_at(this, i)
        end do

        ! pad with nothing
        do i = length + 1, len(to)
            to(i:i) = ""
        end do


    end subroutine ncdf_copy_chars



    !
    ! CLEAR
    !
    recursive subroutine ncdf_string_clear(this)
        type(ncdf_string), pointer :: this

        if (associated(this % next)) then
            call ncdf_string_clear(this % next)
            deallocate(this % next)
            nullify (this % next)
        end if

        this % index = 0

    end subroutine ncdf_string_clear

    !
    ! SIZE    
    !
    recursive integer function ncdf_string_length(str) result(count)
        type(ncdf_string), pointer :: str

        count = str % index

        if (str % index == BLOCK_SIZE .AND. associated(str % next)) then
            count = count + ncdf_string_length(str % next)
        end if

    end function ncdf_string_length


    !
    ! GET CHAR AT
    !
    recursive character function ncdf_get_char_at(this, i) result(c)
        type(ncdf_string), pointer :: this
        integer, intent(in) :: i

        if (i .LE. this % index) then
            c = this % chars(i:i)
        else
            c = ncdf_get_char_at(this % next, i - this % index)
        end if

    end function ncdf_get_char_at

    !
    ! EQUALS STRING
    !
    logical function ncdf_equals_string(this, other) result(equals)
        type(ncdf_string), pointer :: this, other
        integer :: i
        equals = .false.
        
        if(ncdf_ncdf_string_length(this) .ne. ncdf_ncdf_string_length(other)) then
            equals = .false.
            return
        else if(ncdf_ncdf_string_length(this) == 0) then
            equals = .true.
            return
        end if
        
        do i=1, ncdf_string_length(this)
            if(ncdf_get_char_at(this, i) .ne. ncdf_get_char_at(other, i)) then
                equals = .false.
                return
            end if
        end do
        
        equals = .true.
        
    end function ncdf_equals_string

end module ncdf_string_m
