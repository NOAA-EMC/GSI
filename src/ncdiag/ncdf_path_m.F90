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
! File:   ncdf_path_m.f95
! Author: Joseph A. Levin
!
! Created on March 10, 2012, 11:01 PM
!

module ncdf_path_m
    
    use ncdf_value_m 
    use ncdf_string_m

    private
    
    public :: ncdf_path_get
    
    interface ncdf_path_get
        module procedure ncdf_get_by_path
        module procedure ncdf_get_integer
        module procedure ncdf_get_real
        module procedure ncdf_get_double
        module procedure ncdf_get_logical
        module procedure ncdf_get_chars
        module procedure ncdf_get_array_1d_integer
        module procedure ncdf_get_array_2d_integer
        module procedure ncdf_get_array_1d_real
        module procedure ncdf_get_array_2d_real
        module procedure ncdf_get_array_1d_double
        module procedure ncdf_get_array_2d_double
        module procedure ncdf_get_array_1d_logical
        module procedure ncdf_get_array_2d_logical
    end interface ncdf_path_get

    abstract interface

       subroutine ncdf_array_callback_1d(element, i, count)
         use ncdf_value_m
         implicit none
         type(ncdf_value), pointer,intent(in) :: element
         integer, intent(in) :: i        ! index
         integer, intent(in) :: count    ! size of array
       end subroutine ncdf_array_callback_1d

       subroutine ncdf_array_callback_2d(element, i1, i2, count1, count2)
         use ncdf_value_m
         implicit none
         type(ncdf_value), pointer,intent(in) :: element
         integer, intent(in) :: i1, i2
         integer, intent(in) :: count1, count2
       end subroutine ncdf_array_callback_2d

    end interface

contains
    !
    ! GET BY PATH
    !
    ! $     = root 
    ! @     = this
    ! .     = child object member
    ! []    = child array element
    !
    recursive subroutine ncdf_get_by_path(this, path, p)
        type(ncdf_value), pointer :: this, p        
        character(len=*) :: path
        integer :: i, length, child_i
        character :: c
        logical :: array        
                
        ! default to assuming relative to this
        p => this
        
        child_i = 1          
        
        array = .false.
        
        length = len_trim(path)
        
        do i=1, length
            c = path(i:i)    
            select case (c)
                case ("$")
                    ! root
                    do while (associated (p % parent))
                        p => p % parent
                    end do
                    child_i = i + 1
                case ("@")
                    ! this                    
                    p => this
                    child_i = i + 1
                case (".", "[")                    
                    ! get child member from p                          
                    if (child_i < i) then                          
                        p => ncdf_value_get(p, path(child_i:i-1))
                    else
                        child_i = i + 1
                        cycle
                    end if
                    
                    if(.not.associated(p)) then
                        return                                        
                    end if
                    
                    child_i = i+1
                    
                    ! check if this is an array
                    ! if so set the array flag
                    if (c == "[") then
                        ! start looking for the array element index
                        array = .true.
                    end if
                case ("]")
                    if (.not.array) then
                        print *, "ERROR: Unexpected ], not missing preceding ["
                        call exit(1)
                    end if
                    array = .false.
                    child_i = parse_integer(path(child_i:i-1))                                                
                    p => ncdf_value_get(p, child_i)                                                                                                                    
                    
                    child_i= i + 1                                     
            end select            
        end do
                
        ! grab the last child if present in the path
        if (child_i <= length) then            
            p => ncdf_value_get(p, path(child_i:i-1))                    
            if(.not.associated(p)) then
                return
            else                
            end if
        end if
                
        
    end subroutine ncdf_get_by_path
    
    !
    ! PARSE INTEGER
    !
    integer function parse_integer(chars) result(integral)
        character(len=*) :: chars
        character :: c
        integer :: tmp, i
                
        integral = 0        
        do i=1, len_trim(chars)
            c = chars(i:i)            
            select case(c)
                case ("0":"9")
                    ! digit        
                    read (c, '(i1)') tmp                                               
                    
                    ! shift
                    if(i > 1) then
                        integral = integral * 10
                    end if
                    ! add
                    integral = integral + tmp
                                                    
                case default                          
                    return
            end select            
        end do
    
    end function parse_integer    
    
    !
    ! GET INTEGER
    !
    subroutine ncdf_get_integer(this, path, value)
        type(ncdf_value), pointer :: this, p
        character(len=*), optional :: path
        integer :: value        
        
        
        nullify(p)                
        if(present(path)) then
            call ncdf_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
        
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            return
        end if
                
        
        if(p % value_type == TYPE_INTEGER) then            
            value = p % value_integer
        else if (p % value_type == TYPE_REAL) then
            value = p % value_real
        else if (p % value_type == TYPE_LOGICAL) then
            if (p % value_logical) then
                value = 1
            else
                value = 0
            end if
        else
            print *, "Unable to resolve value to integer: ", path
            call exit(1)
        end if
        
    end subroutine ncdf_get_integer
    
    !
    ! GET REAL
    !
    subroutine ncdf_get_real(this, path, value)
        type(ncdf_value), pointer :: this, p
        character(len=*), optional :: path
        real :: value        
        
        
        nullify(p)                
        
        if(present(path)) then
            call ncdf_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
        
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            return
        end if
                
        
        if(p % value_type == TYPE_INTEGER) then            
            value = p % value_integer
        else if (p % value_type == TYPE_REAL) then
            value = p % value_real
        else if (p % value_type == TYPE_LOGICAL) then
            if (p % value_logical) then
                value = 1
            else
                value = 0
            end if
        else
            print *, "Unable to resolve value to real: ", path
            call exit(1)
        end if
        
    end subroutine ncdf_get_real
    
    !
    ! GET DOUBLE
    !
    subroutine ncdf_get_double(this, path, value)
        type(ncdf_value), pointer :: this, p
        character(len=*), optional :: path
        double precision :: value        
        
        
        nullify(p)                
        
        if(present(path)) then
            call ncdf_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
        
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            return
        end if
                
        
        if(p % value_type == TYPE_INTEGER) then            
            value = p % value_integer
        else if (p % value_type == TYPE_REAL) then
            value = p % value_double
        else if (p % value_type == TYPE_LOGICAL) then
            if (p % value_logical) then
                value = 1
            else
                value = 0
            end if
        else
            print *, "Unable to resolve value to double: ", path
            call exit(1)
        end if
        
    end subroutine ncdf_get_double
    
    
    !
    ! GET LOGICAL
    !
    subroutine ncdf_get_logical(this, path, value)
        type(ncdf_value), pointer :: this, p
        character(len=*), optional :: path
        logical :: value        
        
        
        nullify(p)                
        
        if(present(path)) then
            call ncdf_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
        
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            return
        end if
                
        
        if(p % value_type == TYPE_INTEGER) then            
            value = (p % value_integer > 0)       
        else if (p % value_type == TYPE_LOGICAL) then
            value = p % value_logical
        else
            print *, "Unable to resolve value to real: ", path
            call exit(1)
        end if
        
    end subroutine ncdf_get_logical
    
    !
    ! GET CHARS
    !
    subroutine ncdf_get_chars(this, path, value)
        type(ncdf_value), pointer :: this, p
        character(len=*), optional :: path
        character(len=*) :: value  
        
        nullify(p)                
        
        if(present(path)) then
            call ncdf_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
        
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            return
        end if
                
        
        if(p % value_type == TYPE_STRING) then            
            call ncdf_string_copy(p % value_string, value)          
        else
            print *, "Unable to resolve value to characters: ", path
            call exit(1)
        end if
        
    end subroutine ncdf_get_chars
    
    !
    ! GET ARRAY 1D
    !
    
    subroutine ncdf_get_array_1d(this, path, array_callback)
        type(ncdf_value), pointer :: this
        character(len = *), optional :: path
        procedure(ncdf_array_callback_1d) :: array_callback

        type(ncdf_value), pointer :: p, element
        integer :: index, count
                
        nullify(p)                
        
        ! resolve the path to the value
        if(present(path)) then
            call ncdf_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
            
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            return
        end if
        
        if(p % value_type == TYPE_ARRAY) then            
            count = ncdf_value_count(p)
            element => p % children
            do index = 1, count
                call array_callback(element, index, count)
                element => element % next
            end do
        else
            print *, "Resolved value is not an array. ", path
            call exit(1)
        end if

        if (associated(p)) nullify(p)

      end subroutine ncdf_get_array_1d

!
! GET ARRAY INTEGER 1D
!
    subroutine ncdf_get_array_1d_integer(this, path, arr)

      implicit none
      type(ncdf_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      integer, allocatable, intent(out) :: arr(:)

      if (allocated(arr)) deallocate(arr)
#ifdef OLDPGI
#else
        call ncdf_get_array_1d(this, path, ncdf_array_callback_1d_integer)
#endif

    contains

      subroutine ncdf_array_callback_1d_integer(element, i, count)
        implicit none
        type(ncdf_value), pointer, intent(in) :: element
        integer, intent(in) :: i, count
        if (.not. allocated(arr)) allocate(arr(count))
        call ncdf_path_get(element, "", arr(i))
      end subroutine ncdf_array_callback_1d_integer

    end subroutine ncdf_get_array_1d_integer

!
! GET ARRAY REAL 1D
!
    subroutine ncdf_get_array_1d_real(this, path, arr)

      implicit none
      type(ncdf_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      real, allocatable, intent(out) :: arr(:)

      if (allocated(arr)) deallocate(arr)
#ifdef OLDPGI
#else
        call ncdf_get_array_1d(this, path, ncdf_array_callback_1d_real)
#endif

    contains

      subroutine ncdf_array_callback_1d_real(element, i, count)
        implicit none
        type(ncdf_value), pointer, intent(in) :: element
        integer, intent(in) :: i, count
        if (.not. allocated(arr)) allocate(arr(count))
        call ncdf_path_get(element, "", arr(i))
      end subroutine ncdf_array_callback_1d_real

    end subroutine ncdf_get_array_1d_real

!
! GET ARRAY DOUBLE 1D
!
    subroutine ncdf_get_array_1d_double(this, path, arr)

      implicit none
      type(ncdf_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      double precision, allocatable, intent(out) :: arr(:)

      if (allocated(arr)) deallocate(arr)
#ifdef OLDPGI
#else
        call ncdf_get_array_1d(this, path, ncdf_array_callback_1d_double)
#endif

    contains

      subroutine ncdf_array_callback_1d_double(element, i, count)
        implicit none
        type(ncdf_value), pointer, intent(in) :: element
        integer, intent(in) :: i, count
        if (.not. allocated(arr)) allocate(arr(count))
        call ncdf_path_get(element, "", arr(i))
      end subroutine ncdf_array_callback_1d_double

    end subroutine ncdf_get_array_1d_double

!
! GET ARRAY LOGICAL 1D
!
    subroutine ncdf_get_array_1d_logical(this, path, arr)

      implicit none
      type(ncdf_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      logical, allocatable, intent(out) :: arr(:)

      if (allocated(arr)) deallocate(arr)
#ifdef OLDPGI
#else
        call ncdf_get_array_1d(this, path, ncdf_array_callback_1d_logical)
#endif

    contains

      subroutine ncdf_array_callback_1d_logical(element, i, count)
        implicit none
        type(ncdf_value), pointer, intent(in) :: element
        integer, intent(in) :: i, count
        if (.not. allocated(arr)) allocate(arr(count))
        call ncdf_path_get(element, "", arr(i))
      end subroutine ncdf_array_callback_1d_logical

    end subroutine ncdf_get_array_1d_logical

    !
    ! GET ARRAY 2D
    !
    
    subroutine ncdf_get_array_2d(this, path, array_callback)
        type(ncdf_value), pointer :: this
        character(len = *), optional :: path
        procedure(ncdf_array_callback_2d) :: array_callback

        type(ncdf_value), pointer :: p, element, item
        integer :: i1, i2, count1, count2, c
                
        nullify(p)                
        
        ! resolve the path to the value
        if(present(path)) then
            call ncdf_get_by_path(this=this, path=path, p=p)
        else
            p => this
        end if
            
        if(.not.associated(p)) then
            print *, "Unable to resolve path: ", path
            return
        end if
        
        if(p % value_type == TYPE_ARRAY) then            
            count1 = ncdf_value_count(p)
            element => p % children
            do i1 = 1, count1
               if (element % value_type == TYPE_ARRAY) then
                  c = ncdf_value_count(element)
                  if (i1 == 1) then
                     count2 = c
                  else if (c /= count2) then
                     print *, "Resolved value has the wrong number of elements. ", &
                          path, "[", i1, "]"
                     call exit(1)
                  end if
                  item => element % children
                  do i2 = 1, count2
                     call array_callback(item, i1, i2, count1, count2)
                     item => item % next
                  end do
                  element => element % next
               else
                  print *, "Resolved value is not an array. ", path, "[", i1, "]"
                  call exit(1)
               end if
            end do
        else
            print *, "Resolved value is not an array. ", path
            call exit(1)
        end if

        if (associated(p)) nullify(p)

      end subroutine ncdf_get_array_2d

!
! GET ARRAY INTEGER 2D
!
    subroutine ncdf_get_array_2d_integer(this, path, arr)

      implicit none
      type(ncdf_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      integer, allocatable, intent(out) :: arr(:, :)

      if (allocated(arr)) deallocate(arr)
#ifdef OLDPGI
#else
        call ncdf_get_array_2d(this, path, ncdf_array_callback_2d_integer)
#endif

    contains

      subroutine ncdf_array_callback_2d_integer(element, i1, i2, count1, count2)
        implicit none
        type(ncdf_value), pointer, intent(in) :: element
        integer, intent(in) :: i1, i2, count1, count2
        if (.not. allocated(arr)) allocate(arr(count1, count2))
        call ncdf_path_get(element, "", arr(i1, i2))
      end subroutine ncdf_array_callback_2d_integer

    end subroutine ncdf_get_array_2d_integer

!
! GET ARRAY REAL 2D
!
    subroutine ncdf_get_array_2d_real(this, path, arr)

      implicit none
      type(ncdf_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      real, allocatable, intent(out) :: arr(:, :)

      if (allocated(arr)) deallocate(arr)
#ifdef OLDPGI
#else
        call ncdf_get_array_2d(this, path, ncdf_array_callback_2d_real)
#endif

    contains

      subroutine ncdf_array_callback_2d_real(element, i1, i2, count1, count2)
        implicit none
        type(ncdf_value), pointer, intent(in) :: element
        integer, intent(in) :: i1, i2, count1, count2
        if (.not. allocated(arr)) allocate(arr(count1, count2))
        call ncdf_path_get(element, "", arr(i1, i2))
      end subroutine ncdf_array_callback_2d_real

    end subroutine ncdf_get_array_2d_real

!
! GET ARRAY DOUBLE 2D
!
    subroutine ncdf_get_array_2d_double(this, path, arr)

      implicit none
      type(ncdf_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      double precision, allocatable, intent(out) :: arr(:, :)

      if (allocated(arr)) deallocate(arr)
#ifdef OLDPGI
#else
        call ncdf_get_array_2d(this, path, ncdf_array_callback_2d_double)
#endif

    contains

      subroutine ncdf_array_callback_2d_double(element, i1, i2, count1, count2)
        implicit none
        type(ncdf_value), pointer, intent(in) :: element
        integer, intent(in) :: i1, i2, count1, count2
        if (.not. allocated(arr)) allocate(arr(count1, count2))
        call ncdf_path_get(element, "", arr(i1, i2))
      end subroutine ncdf_array_callback_2d_double

    end subroutine ncdf_get_array_2d_double

!
! GET ARRAY LOGICAL 2D
!
    subroutine ncdf_get_array_2d_logical(this, path, arr)

      implicit none
      type(ncdf_value), pointer, intent(in) :: this
      character(len=*), intent(in), optional :: path   
      logical, allocatable, intent(out) :: arr(:, :)

      if (allocated(arr)) deallocate(arr)
#ifdef OLDPGI
#else
        call ncdf_get_array_2d(this, path, ncdf_array_callback_2d_logical)
#endif

    contains

      subroutine ncdf_array_callback_2d_logical(element, i1, i2, count1, count2)
        implicit none
        type(ncdf_value), pointer, intent(in) :: element
        integer, intent(in) :: i1, i2, count1, count2
        if (.not. allocated(arr)) allocate(arr(count1, count2))
        call ncdf_path_get(element, "", arr(i1, i2))
      end subroutine ncdf_array_callback_2d_logical

    end subroutine ncdf_get_array_2d_logical


end module ncdf_path_m
