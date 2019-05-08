! utils.f90
! general utilities for Fortran programs
! Author: Albert Huang for SSAI/NASA GSFC GMAO

module ncdw_strarrutils
    implicit none
    
    contains
        function lentrim(s)
            character(len=*) :: s
            integer lentrim

            do lentrim = len(s), 1, -1
              if (s(lentrim:lentrim) .ne. ' ') return
            end do
        end function lentrim
        
        function string_count_substr(s, substr) result(sub_count)
            character(len=*), intent(in)      :: s
            character(len=*), intent(in)      :: substr
            integer                           :: sub_count
            
            integer                           :: substr_len, i, jump
            substr_len = len(substr)
            sub_count = 0
            jump = 1
            i = 1
            
            do while (i <= len(s) - len(substr))
                if (s(i:i+len(substr)-1) == substr) then
                    sub_count = sub_count + 1
                    jump = len(substr)
                else
                    jump = 1
                end if
                
                i = i + jump
            end do
        end function string_count_substr
        
        function string_get_max_split(s, substr) result(max_len)
            character(len=*), intent(in)      :: s
            character(len=*), intent(in)      :: substr
            integer                           :: sub_count
            
            integer                           :: substr_len, i, jump
            integer                           :: max_len, tmp_len
            
            substr_len = len(substr)
            sub_count = 0
            jump = 1
            i = 1
            
            tmp_len = 0
            max_len = 0
            
            do while (i <= len_trim(s) - len(substr) + 1)
                if (s(i:i+len(substr)-1) == substr) then
                    sub_count = sub_count + 1
                    if (tmp_len > max_len) max_len = tmp_len
                    tmp_len = 0
                    jump = len(substr)
                else
                    jump = 1
                    tmp_len = tmp_len + 1
                end if
                
                i = i + jump
            end do
            
            ! Do one more check to ensure we get the end!
            if ((tmp_len + len(substr) - 1) > max_len) max_len = tmp_len + len(substr) - 1
        end function string_get_max_split
        
        function string_split_index(s, delimiter) result(split_strings)
            character(len=*)      :: s
            character(len=*)      :: delimiter
            
            integer                           :: substr_len, i, jump
            integer                           :: tmp_idx, start_idx, total
            integer                           :: split_length, item_length
            
            character(len=:), allocatable     :: split_strings(:)
            character(len=:), allocatable     :: tmp_str
            
            ! Get lengths
            split_length = string_count_substr(s, delimiter) + 1
            item_length = string_get_max_split(s, delimiter)
            
            allocate(character(item_length) :: split_strings(split_length))
            allocate(character(item_length) :: tmp_str)
            
            substr_len = len(delimiter)
            jump = 1
            i = 1
            
            tmp_idx = 1
            start_idx = 1
            total = 1
            
            do while (i <= len_trim(s) - len(delimiter) + 1)
                if (s(i:i+len(delimiter)-1) == delimiter) then
                    if (start_idx /= tmp_idx) then
                        split_strings(total) = s(start_idx:tmp_idx - 1)
                    else
                        split_strings(total) = ""
                    end if
                    
                    tmp_idx = tmp_idx + len(delimiter)
                    start_idx = tmp_idx
                    
                    total = total + 1
                    
                    jump = len(delimiter)
                else
                    jump = 1
                    tmp_idx = tmp_idx + 1
                end if
                
                i = i + jump
            end do
            
            ! Do one more check to ensure we get the end!
            split_strings(total) = s(start_idx:tmp_idx - 1)
        end function string_split_index
        
        ! asl = assumed shape length
        subroutine string_array_dump(strings)
            character(len=:), allocatable     :: strings(:)
            integer i
            
            write (*, "(A, I0)") "Length of strings array: ", size(strings(:))
            print *, " -> String array dump:"
            
            do i = 1, size(strings(:))
                if (strings(i) == "") then
                    write (*, "(A, I0, A, I0, A, I0, A)") "  --> Position ", i, ": (empty) [Trim length = ", len_trim(strings(i)), ", Full length = ", len(strings(i)), "]"
                else
                    write (*, "(A, I0, A, A, A, I0, A, I0, A)") "  --> Position ", i, ": '", trim(strings(i)), "' [Trim length = ", len_trim(strings(i)), ", Full length = ", len(strings(i)), "]"
                end if
            end do
        end subroutine string_array_dump
        
        function max_len_string_array(str_arr, arr_length) result(max_len)
            character(len=*), intent(in) :: str_arr(:)
            integer         , intent(in) :: arr_length
            
            integer :: i, max_len
            
            max_len = -1
            
#ifdef _DEBUG_MEM_
            write (*, "(A, I0)") " ** max_len_string_array: size(str_arr) is ", size(str_arr)
#endif
            
            do i = 1, arr_length
                if (len_trim(str_arr(i)) > max_len) max_len = len_trim(str_arr(i))
#ifdef _DEBUG_MEM_
                write (*, "(A, I0, A, I0)") "max_len_string_array: str_arr(", i, ") is " // trim(str_arr(i)) // ", size is ", len_trim(str_arr(i))
                write (*, "(A, I0)") "max_len_string_array: max_len is ", max_len
#endif
            end do
        end function max_len_string_array
        
        function max_len_notrim_string_array(str_arr, arr_length) result(max_len)
            character(len=*), intent(in) :: str_arr(:)
            integer         , intent(in) :: arr_length
            
            integer :: i, max_len
            
            max_len = -1
            
#ifdef _DEBUG_MEM_
            write (*, "(A, I0)") " ** max_len_notrim_string_array: size(str_arr) is ", size(str_arr)
#endif
            
            do i = 1, arr_length
                if (len(str_arr(i)) > max_len) max_len = len(str_arr(i))
#ifdef _DEBUG_MEM_
                write (*, "(A, I0, A, I0)") "max_len_notrim_string_array: str_arr(", i, ") is " // trim(str_arr(i)) // ", size is ", len_trim(str_arr(i))
                write (*, "(A, I0)") "max_len_notrim_string_array: max_len is ", max_len
#endif
            end do
        end function max_len_notrim_string_array
        
        subroutine string_before_delimiter(s, delimiter, string_part)
            character(len=*), intent(in)      :: s
            character(len=*), intent(in)      :: delimiter
            character(len=:), intent(inout), allocatable :: string_part
            
            integer                             :: substr_len, i, jump
            integer                             :: tmp_idx, start_idx, total
            
            logical found
            found = .FALSE.
            
            ! Get lengths
            substr_len = len(delimiter)
            jump = 1
            i = 1
            
            tmp_idx = 1
            start_idx = 1
            total = 1
            
            do while (i <= len_trim(s) - len(delimiter) + 1)
                if (s(i:i+len(delimiter)-1) == delimiter) then
                    found = .TRUE.
                    exit
                else
                    jump = 1
                    tmp_idx = tmp_idx + 1
                end if
                
                i = i + jump
            end do
            
            ! Do one more check to ensure we get the end!
            if (found) then
                if (start_idx == tmp_idx) then
                    allocate(character(0) :: string_part)
                    string_part = ""
                else
                    allocate(character(tmp_idx - start_idx + 1) :: string_part)
                    string_part = s(start_idx:tmp_idx - 1)
                end if
            else
                allocate(character(len(s)) :: string_part)
                string_part = s
            end if
        end subroutine string_before_delimiter
end module ncdw_strarrutils
