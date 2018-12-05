module ncdw_realloc
    use ncd_kinds, only: i_byte, i_short, i_long, i_llong, r_single, &
        r_double
    use ncdw_climsg, only: nclayer_error
#ifdef _DEBUG_MEM_
    use ncdw_climsg, only: nclayer_debug
#endif
    
    implicit none
    
    ! This file provides the interface wrapper for the array
    ! reallocation subroutines. This is so that others can simply
    ! call nc_diag_realloc with the necessary arguments, instead of
    ! having to call the specific nc_diag_realloc_* subroutines.
    
    interface nc_diag_realloc
        module procedure nc_diag_realloc_byte, &
            nc_diag_realloc_short, nc_diag_realloc_long, &
            nc_diag_realloc_llong, nc_diag_realloc_rsingle, &
            nc_diag_realloc_rdouble, nc_diag_realloc_string, &
            nc_diag_realloc_logical
    end interface nc_diag_realloc
    
    contains
        ! nc_diag_realloc_byte(arr, addl_num_entries)
        !   input:
        !     integer(i_byte), dimension(:)  :: arr
        !         array to reallocate
        !     integer(i_long), intent(in)    :: addl_num_entries
        !         additional number of elements to allocate to the
        !         specified array
        subroutine nc_diag_realloc_byte(arr, addl_num_entries)
            integer(i_byte), dimension(:), allocatable, intent(inout) :: arr
            integer(i_llong),intent(in) :: addl_num_entries
            
            integer(i_byte), dimension(:), allocatable   :: tmp
            integer(i_llong)                             :: new_size
            
            integer(i_byte)                              :: alloc_err
            character(len=100)                           :: err_msg
            
            new_size = size(arr) + addl_num_entries
            
            allocate(tmp(new_size), STAT=alloc_err)
            if (alloc_err /= 0) then
                write(err_msg, "(A, I0)") "Reallocator was unable to reallocate memory! Error code: ", alloc_err
                call nclayer_error(trim(err_msg))
            end if
            tmp(1:size(arr)) = arr
            deallocate(arr)
            allocate(arr(new_size))
            arr = tmp
        end subroutine nc_diag_realloc_byte
        
        ! nc_diag_realloc_short(arr, addl_num_entries)
        !   input:
        !     integer(i_short), dimension(:) :: arr
        !         array to reallocate
        !     integer(i_long), intent(in)    :: addl_num_entries
        !         additional number of elements to allocate to the
        !         specified array
        subroutine nc_diag_realloc_short(arr, addl_num_entries)
            integer(i_short), dimension(:), allocatable, intent(inout) :: arr
            integer(i_llong),intent(in) :: addl_num_entries
            
            integer(i_short), dimension(:), allocatable   :: tmp
            integer(i_llong)                              :: new_size
            
            integer(i_byte)                               :: alloc_err
            character(len=100)                            :: err_msg
            
            new_size = size(arr) + addl_num_entries
            
            allocate(tmp(new_size), STAT=alloc_err)
            if (alloc_err /= 0) then
                write(err_msg, "(A, I0)") "Reallocator was unable to reallocate memory! Error code: ", alloc_err
                call nclayer_error(trim(err_msg))
            end if
            tmp(1:size(arr)) = arr
            deallocate(arr)
            allocate(arr(new_size))
            arr = tmp
        end subroutine nc_diag_realloc_short
        
        ! nc_diag_realloc_long(arr, addl_num_entries)
        !   input:
        !     integer(i_long), dimension(:)  :: arr
        !         array to reallocate
        !     integer(i_long), intent(in)    :: addl_num_entries
        !         additional number of elements to allocate to the
        !         specified array
        subroutine nc_diag_realloc_long(arr, addl_num_entries)
            integer(i_long), dimension(:), allocatable, intent(inout) :: arr
            integer(i_llong),intent(in) :: addl_num_entries
            
            integer(i_long), dimension(:), allocatable   :: tmp
            integer(i_llong)                             :: new_size
            
            integer(i_byte)                              :: alloc_err
            character(len=100)                           :: err_msg
            
#ifdef _DEBUG_MEM_
            call nclayer_debug("Reallocating long array...")
#endif
            
            new_size = size(arr) + addl_num_entries
            
#ifdef _DEBUG_MEM_
            print *, "REALLOCATOR: new_size is ", new_size
#endif
            
            allocate(tmp(new_size), STAT=alloc_err)
            if (alloc_err /= 0) then
                write(err_msg, "(A, I0)") "Reallocator was unable to reallocate memory! Error code: ", alloc_err
                call nclayer_error(trim(err_msg))
            end if
            
            tmp(1:size(arr)) = arr
            deallocate(arr)
            allocate(arr(new_size))
            arr = tmp
            
#ifdef _DEBUG_MEM_
            print *, "REALLOCATOR: final actual size is ", size(arr)
            call nclayer_debug("Realloc finished for long")
#endif
        end subroutine nc_diag_realloc_long
        
        ! nc_diag_realloc_llong(arr, addl_num_entries)
        !   input:
        !     integer(i_llong), dimension(:)  :: arr
        !         array to reallocate
        !     integer(i_llong), intent(in)    :: addl_num_entries
        !         additional number of elements to allocate to the
        !         specified array
        subroutine nc_diag_realloc_llong(arr, addl_num_entries)
            integer(i_llong), dimension(:), allocatable, intent(inout) :: arr
            integer(i_llong),intent(in) :: addl_num_entries
            
            integer(i_llong), dimension(:), allocatable   :: tmp
            integer(i_llong)                             :: new_size
            
            integer(i_byte)                              :: alloc_err
            character(len=100)                           :: err_msg
            
#ifdef _DEBUG_MEM_
            call nclayer_debug("Reallocating long array...")
#endif
            
            new_size = size(arr) + addl_num_entries
            
            allocate(tmp(new_size), STAT=alloc_err)
            if (alloc_err /= 0) then
                write(err_msg, "(A, I0)") "Reallocator was unable to reallocate memory! Error code: ", alloc_err
                call nclayer_error(trim(err_msg))
            end if
            
            tmp(1:size(arr)) = arr
            deallocate(arr)
            allocate(arr(new_size))
            arr = tmp
            
#ifdef _DEBUG_MEM_
            call nclayer_debug("Realloc finished for long")
#endif
        end subroutine nc_diag_realloc_llong
        
        ! nc_diag_realloc_rsingle(arr, addl_num_entries)
        !   input:
        !     real(r_single), dimension(:)   :: arr
        !         array to reallocate
        !     integer(i_long), intent(in)    :: addl_num_entries
        !         additional number of elements to allocate to the
        !         specified array
        subroutine nc_diag_realloc_rsingle(arr, addl_num_entries)
            real(r_single), dimension(:), allocatable, intent(inout) :: arr
            integer(i_llong),intent(in) :: addl_num_entries
            
            real(r_single), dimension(:), allocatable    :: tmp
            integer(i_llong)                             :: new_size
            
            integer(i_byte)                              :: alloc_err
            character(len=100)                           :: err_msg
            
            new_size = size(arr) + addl_num_entries
            
            allocate(tmp(new_size), STAT=alloc_err)
            if (alloc_err /= 0) then
                write(err_msg, "(A, I0)") "Reallocator was unable to reallocate memory! Error code: ", alloc_err
                call nclayer_error(trim(err_msg))
            end if
            tmp(1:size(arr)) = arr
            deallocate(arr)
            allocate(arr(new_size))
            arr = tmp
        end subroutine nc_diag_realloc_rsingle
        
        ! nc_diag_realloc_rdouble(arr, addl_num_entries)
        !   input:
        !     real(r_double), dimension(:)   :: arr
        !         array to reallocate
        !     integer(i_long), intent(in)    :: addl_num_entries
        !         additional number of elements to allocate to the
        !         specified array
        subroutine nc_diag_realloc_rdouble(arr, addl_num_entries)
            real(r_double), dimension(:), allocatable, intent(inout) :: arr
            integer(i_llong),intent(in) :: addl_num_entries
            
            real(r_double), dimension(:), allocatable    :: tmp
            integer(i_llong)                             :: new_size
            
            integer(i_byte)                              :: alloc_err
            character(len=100)                           :: err_msg
            
            new_size = size(arr) + addl_num_entries
            
            allocate(tmp(new_size), STAT=alloc_err)
            if (alloc_err /= 0) then
                write(err_msg, "(A, I0)") "Reallocator was unable to reallocate memory! Error code: ", alloc_err
                call nclayer_error(trim(err_msg))
            end if
            tmp(1:size(arr)) = arr
            deallocate(arr)
            allocate(arr(new_size))
            arr = tmp
        end subroutine nc_diag_realloc_rdouble
        
        ! nc_diag_realloc_string(arr, addl_num_entries)
        !   input:
        !     character(len=*), dimension(:) :: arr
        !         array to reallocate
        !     integer(i_long), intent(in)    :: addl_num_entries
        !         additional number of elements to allocate to the
        !         specified array
        subroutine nc_diag_realloc_string(arr, addl_num_entries)
            character(len=*), dimension(:), allocatable, intent(inout) :: arr
            integer(i_llong),intent(in) :: addl_num_entries
            
            character(len=len(arr(1))), dimension(:), allocatable   :: tmp
            integer(i_llong)            :: new_size
            
            integer(i_byte)                              :: alloc_err
            character(len=100)                           :: err_msg
            
#ifdef _DEBUG_MEM_
            integer :: string_len, string_arr_size
            
            string_len = len(arr(1))
            string_arr_size = size(arr)
            
            call nclayer_debug("[string] Length of string to allocate to:")
            print *, string_len
            
            call nclayer_debug("[string] Allocating from...")
            print *, string_arr_size
            
            call nclayer_debug("[string] ...to size...")
            print *, (string_arr_size + addl_num_entries)
#endif
            
            new_size = size(arr) + addl_num_entries
            
            allocate(tmp(new_size), STAT=alloc_err)
            if (alloc_err /= 0) then
                write(err_msg, "(A, I0)") "Reallocator was unable to reallocate memory! Error code: ", alloc_err
                call nclayer_error(trim(err_msg))
            end if
            tmp(1:size(arr)) = arr
            deallocate(arr)
            allocate(arr(new_size))
            arr = tmp
        end subroutine nc_diag_realloc_string
        
        ! nc_diag_realloc_logical(arr, addl_num_entries)
        !   input:
        !     logical, dimension(:)          :: arr
        !         array to reallocate
        !     integer(i_long), intent(in)    :: addl_num_entries
        !         additional number of elements to allocate to the
        !         specified array
        subroutine nc_diag_realloc_logical(arr, addl_num_entries)
            logical, dimension(:), allocatable, intent(inout) :: arr
            integer(i_llong),intent(in) :: addl_num_entries
            
            logical, dimension(:), allocatable   :: tmp
            integer(i_llong)                     :: new_size
            
            integer(i_llong) :: logical_arr_size
            logical_arr_size = size(arr)
            
            new_size = logical_arr_size + addl_num_entries
            
#ifdef _DEBUG_MEM_
            call nclayer_debug("[logical] Allocating from...")
            print *, logical_arr_size
            
            call nclayer_debug("[logical] ...to size...")
            print *, (logical_arr_size + addl_num_entries)
#endif
            
            allocate(tmp(new_size))
            tmp(1:logical_arr_size) = arr
            deallocate(arr)
            allocate(arr(new_size))
            arr = tmp
#ifdef _DEBUG_MEM_
            call nclayer_debug("[logical] Final size:")
            print *, size(arr)
#endif
        end subroutine nc_diag_realloc_logical
end module ncdw_realloc
