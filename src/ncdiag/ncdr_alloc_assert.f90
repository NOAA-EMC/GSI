module ncdr_alloc_assert
    ! Allocate if things aren't allocated, or assert that things are
    ! all good to go.
    ! 
    ! Other parts include just assertion functions (e.g. asserting
    ! that a variable exists).
    use ncd_kinds, only: i_byte, i_short, i_long, r_single, r_double
    use ncdr_state, only: ncdr_files, current_ncdr_id
    use ncdr_climsg, only: ncdr_error
    use ncdr_check, only: ncdr_nc_check, ncdr_check_ncdr_id, &
        ncdr_check_current_ncdr_id
    use netcdf, only: nf90_inquire_attribute, NF90_GLOBAL, NF90_BYTE, &
        NF90_SHORT, NF90_INT, NF90_FLOAT, NF90_DOUBLE, NF90_CHAR
    
    implicit none
    
    interface nc_diag_read_assert_var
        module procedure nc_diag_read_id_assert_var, &
            nc_diag_read_noid_assert_var
    end interface nc_diag_read_assert_var
    
    interface nc_diag_read_assert_attr
        module procedure nc_diag_read_id_assert_attr, &
            nc_diag_read_noid_assert_attr
    end interface nc_diag_read_assert_attr
    
    interface nc_diag_read_assert_global_attr
        module procedure nc_diag_read_id_assert_global_attr, &
            nc_diag_read_noid_assert_global_attr
    end interface nc_diag_read_assert_global_attr
    
    interface nc_diag_read_assert_dims
        ! Note that nc_diag_read_assert_dims_alloc_string is seperate
        ! since it is rare and conflicts with the non-alloc def.
        module procedure &
            nc_diag_read_assert_dims_single_byte, &
            nc_diag_read_assert_dims_single_short, &
            nc_diag_read_assert_dims_single_long, &
            nc_diag_read_assert_dims_single_float, &
            nc_diag_read_assert_dims_single_double, &
            nc_diag_read_assert_dims_string, &
            nc_diag_read_assert_dims_1d_byte, &
            nc_diag_read_assert_dims_1d_short, &
            nc_diag_read_assert_dims_1d_long, &
            nc_diag_read_assert_dims_1d_float, &
            nc_diag_read_assert_dims_1d_double, &
            nc_diag_read_assert_dims_1d_string, &
            nc_diag_read_assert_dims_2d_byte, &
            nc_diag_read_assert_dims_2d_short, &
            nc_diag_read_assert_dims_2d_long, &
            nc_diag_read_assert_dims_2d_float, &
            nc_diag_read_assert_dims_2d_double, &
            nc_diag_read_assert_dims_2d_string
    end interface nc_diag_read_assert_dims
    
    contains
        function nc_diag_read_id_assert_var(file_ncdr_id, var_name) result(var_index)
            integer(i_long), intent(in)    :: file_ncdr_id
            character(len=*), intent(in)   :: var_name
            
            integer(i_long)                :: var_index
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            do var_index = 1, ncdr_files(file_ncdr_id)%nvars
                if (ncdr_files(file_ncdr_id)%vars(var_index)%var_name == var_name) &
                    return
            end do
            
            ! If we didn't find anything, show an error!
            call ncdr_error("The specified variable '" // var_name // "' does not exist!")
        end function nc_diag_read_id_assert_var
        
        function nc_diag_read_noid_assert_var(var_name) result(var_index)
            character(len=*), intent(in)   :: var_name
            
            integer(i_long)                :: var_index
            
            call ncdr_check_current_ncdr_id
            
            var_index = nc_diag_read_id_assert_var(current_ncdr_id, var_name)
        end function nc_diag_read_noid_assert_var
        
        subroutine nc_diag_read_id_assert_attr(file_ncdr_id, var_name, attr_name, attr_type, attr_len)
            integer(i_long), intent(in)    :: file_ncdr_id
            character(len=*), intent(in)   :: var_name
            character(len=*), intent(in)   :: attr_name
            integer(i_long), intent(out)   :: attr_type
            integer(i_long), intent(out)   :: attr_len
            
            integer(i_long)                :: var_id
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            var_id = ncdr_files(file_ncdr_id)%vars( &
                nc_diag_read_assert_var(file_ncdr_id, var_name) )%var_id
            
            call ncdr_nc_check(nf90_inquire_attribute(ncdr_files(file_ncdr_id)%ncid, &
                var_id, &
                attr_name, attr_type, attr_len))
        end subroutine nc_diag_read_id_assert_attr
        
        subroutine nc_diag_read_noid_assert_attr(var_name, attr_name, attr_type, attr_len)
            character(*), intent(in)   :: var_name
            character(len=*), intent(in)   :: attr_name
            integer(i_long), intent(out)   :: attr_type
            integer(i_long), intent(out)   :: attr_len
            
            call ncdr_check_current_ncdr_id
            
            call nc_diag_read_id_assert_attr(current_ncdr_id, var_name, attr_name, attr_type, attr_len)
        end subroutine nc_diag_read_noid_assert_attr
        
        subroutine nc_diag_read_id_assert_global_attr(file_ncdr_id, attr_name, attr_type, attr_len)
            integer(i_long), intent(in)    :: file_ncdr_id
            character(len=*), intent(in)   :: attr_name
            integer(i_long), intent(out)   :: attr_type
            integer(i_long), intent(out)   :: attr_len
            
            call ncdr_check_ncdr_id(file_ncdr_id)
            
            call ncdr_nc_check(nf90_inquire_attribute(ncdr_files(file_ncdr_id)%ncid, &
                NF90_GLOBAL, &
                attr_name, attr_type, attr_len))
        end subroutine nc_diag_read_id_assert_global_attr
        
        subroutine nc_diag_read_noid_assert_global_attr(attr_name, attr_type, attr_len)
            character(len=*), intent(in)   :: attr_name
            integer(i_long), intent(out)   :: attr_type
            integer(i_long), intent(out)   :: attr_len
            
            call ncdr_check_current_ncdr_id
            
            call nc_diag_read_id_assert_global_attr(current_ncdr_id, attr_name, attr_type, attr_len)
        end subroutine nc_diag_read_noid_assert_global_attr
        
        subroutine nc_diag_read_assert_var_type(var_type, correct_var_type)
            integer(i_long)                            :: var_type
            integer(i_long)                            :: correct_var_type
            
            if (var_type /= correct_var_type) &
                call ncdr_error("Mismatched type for variable! Got " // &
                    nc_diag_read_get_type_str(var_type) // &
                    " when " // &
                    nc_diag_read_get_type_str(correct_var_type) // &
                    " was expected for the variable!")
        end subroutine nc_diag_read_assert_var_type
        
        subroutine nc_diag_read_assert_attr_type(attr_type, correct_attr_type)
            integer(i_long)                            :: attr_type
            integer(i_long)                            :: correct_attr_type
            
            if (attr_type /= correct_attr_type) &
                call ncdr_error("Mismatched type for attribute! Got " // &
                    nc_diag_read_get_type_str(attr_type) // &
                    " when " // &
                    nc_diag_read_get_type_str(correct_attr_type) // &
                    " was expected for the attribute!")
        end subroutine nc_diag_read_assert_attr_type
        
        subroutine nc_diag_read_assert_global_attr_type(attr_type, correct_attr_type)
            integer(i_long)                            :: attr_type
            integer(i_long)                            :: correct_attr_type
            
            if (attr_type /= correct_attr_type) &
                call ncdr_error("Mismatched type for global attribute! Got " // &
                    nc_diag_read_get_type_str(attr_type) // &
                    " when " // &
                    nc_diag_read_get_type_str(correct_attr_type) // &
                    " was expected for the global attribute!")
        end subroutine nc_diag_read_assert_global_attr_type
        
        function nc_diag_read_get_type_str(var_type) result(type_str)
            integer(i_long)                            :: var_type
            character(len=:), allocatable              :: type_str
            
            if (var_type == NF90_BYTE) then
                type_str = "NF90_BYTE"
            else if (var_type == NF90_SHORT) then
                type_str = "NF90_SHORT"
            else if (var_type == NF90_INT) then
                type_str = "NF90_INT"
            else if (var_type == NF90_FLOAT) then
                type_str = "NF90_FLOAT"
            else if (var_type == NF90_DOUBLE) then
                type_str = "NF90_DOUBLE"
            else if (var_type == NF90_CHAR) then
                type_str = "NF90_CHAR"
            else if (var_type == 12) then
                type_str = "NF90_STRING (not supported)"
            else
                type_str = "(unknown type)"
            end if
        end function nc_diag_read_get_type_str
        
        subroutine nc_diag_read_assert_var_ndims(var_ndims, correct_var_ndims)
            integer(i_long)                            :: var_ndims
            integer(i_long)                            :: correct_var_ndims
            
            if (var_ndims /= correct_var_ndims) &
                call ncdr_error("Mismatched dimensions for variable!")
        end subroutine nc_diag_read_assert_var_ndims
        
        !-------------------------------------------------------------
        ! Variable allocation and assertion subroutines
        !-------------------------------------------------------------
        subroutine nc_diag_read_assert_dims_string(var_stor, correct_dims)
            character(len=*), intent(in)               :: var_stor
            integer(i_long), dimension(:), intent(in)  :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            if (size(correct_dims) /= correct_ndims) &
                call ncdr_error("Invalid number of dimensions for variable!")
            if (len(var_stor) < correct_dims(1)) &
                call ncdr_error("Mismatched dimensions for variable storage!")
        end subroutine nc_diag_read_assert_dims_string
        
        subroutine nc_diag_read_assert_dims_single_byte(var_stor, correct_dims)
            integer(i_byte), intent(in)                             :: var_stor
            integer(i_long), dimension(:), intent(in)               :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            if (size(correct_dims) /= correct_ndims) &
                call ncdr_error("Invalid number of dimensions for variable!")
            if (correct_dims(1) /= 1) &
                call ncdr_error("Mismatched dimensions for variable storage!")
        end subroutine nc_diag_read_assert_dims_single_byte
        
        subroutine nc_diag_read_assert_dims_single_short(var_stor, correct_dims)
            integer(i_short), intent(in)                            :: var_stor
            integer(i_long), dimension(:), intent(in)               :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            if (size(correct_dims) /= correct_ndims) &
                call ncdr_error("Invalid number of dimensions for variable!")
            if (correct_dims(1) /= 1) &
                call ncdr_error("Mismatched dimensions for variable storage!")
        end subroutine nc_diag_read_assert_dims_single_short
        
        subroutine nc_diag_read_assert_dims_single_long(var_stor, correct_dims)
            integer(i_long), intent(in)                             :: var_stor
            integer(i_long), dimension(:), intent(in)               :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            if (size(correct_dims) /= correct_ndims) &
                call ncdr_error("Invalid number of dimensions for variable!")
            if (correct_dims(1) /= 1) &
                call ncdr_error("Mismatched dimensions for variable storage!")
        end subroutine nc_diag_read_assert_dims_single_long
        
        subroutine nc_diag_read_assert_dims_single_float(var_stor, correct_dims)
            real(r_single) , intent(in)                             :: var_stor
            integer(i_long), dimension(:), intent(in)               :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            if (size(correct_dims) /= correct_ndims) &
                call ncdr_error("Invalid number of dimensions for variable!")
            if (correct_dims(1) /= 1) &
                call ncdr_error("Mismatched dimensions for variable storage!")
        end subroutine nc_diag_read_assert_dims_single_float
        
        subroutine nc_diag_read_assert_dims_single_double(var_stor, correct_dims)
            real(r_double) , intent(in)                             :: var_stor
            integer(i_long), dimension(:), intent(in)               :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            if (size(correct_dims) /= correct_ndims) &
                call ncdr_error("Invalid number of dimensions for variable!")
            if (correct_dims(1) /= 1) &
                call ncdr_error("Mismatched dimensions for variable storage!")
        end subroutine nc_diag_read_assert_dims_single_double
        
        subroutine nc_diag_read_assert_dims_alloc_string(var_stor, correct_dims)
            character(len=:),allocatable,intent(inout) :: var_stor
            integer(i_long), dimension(:), intent(in)  :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call ncdr_error("Invalid number of dimensions for variable!")
                if (len(var_stor) /= correct_dims(1)) &
                    call ncdr_error("Mismatched dimensions for variable storage!")
            else
                allocate(character(len=correct_dims(1)) :: var_stor)
            end if
        end subroutine nc_diag_read_assert_dims_alloc_string
        
        subroutine nc_diag_read_assert_dims_1d_byte(var_stor, correct_dims)
            integer(i_byte),dimension(:),allocatable,intent(inout) :: var_stor
            integer(i_long), dimension(:), intent(in)              :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call ncdr_error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call ncdr_error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1)))
            end if
        end subroutine nc_diag_read_assert_dims_1d_byte
        
        subroutine nc_diag_read_assert_dims_1d_short(var_stor, correct_dims)
            integer(i_short),dimension(:),allocatable,intent(inout) :: var_stor
            integer(i_long), dimension(:), intent(in)               :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call ncdr_error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call ncdr_error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1)))
            end if
        end subroutine nc_diag_read_assert_dims_1d_short
        
        subroutine nc_diag_read_assert_dims_1d_long(var_stor, correct_dims)
            integer(i_long),dimension(:),allocatable,intent(inout) :: var_stor
            integer(i_long), dimension(:), intent(in)              :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call ncdr_error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call ncdr_error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1)))
            end if
        end subroutine nc_diag_read_assert_dims_1d_long
        
        subroutine nc_diag_read_assert_dims_1d_float(var_stor, correct_dims)
            real(r_single),dimension(:),allocatable,intent(inout) :: var_stor
            integer(i_long), dimension(:), intent(in)             :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call ncdr_error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call ncdr_error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1)))
            end if
        end subroutine nc_diag_read_assert_dims_1d_float
        
        subroutine nc_diag_read_assert_dims_1d_double(var_stor, correct_dims)
            real(r_double),dimension(:),allocatable,intent(inout) :: var_stor
            integer(i_long), dimension(:), intent(in)             :: correct_dims
            integer(i_long), parameter :: correct_ndims = 1
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call ncdr_error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call ncdr_error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1)))
            end if
        end subroutine nc_diag_read_assert_dims_1d_double
        
        subroutine nc_diag_read_assert_dims_1d_string(var_stor, correct_dims)
            character(len=:),dimension(:),allocatable,intent(inout) :: var_stor
            integer(i_long), dimension(:), intent(in)               :: correct_dims
            integer(i_long), parameter :: correct_ndims = 2
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call ncdr_error("Invalid number of dimensions for variable!")
                if (len(var_stor) /= correct_dims(1)) &
                    call ncdr_error("Mismatched dimensions for variable storage!")
                if (size(var_stor) /= correct_dims(2)) &
                    call ncdr_error("Mismatched dimensions for variable storage!")
            else
                allocate(character(len=correct_dims(1)) :: var_stor(correct_dims(2)))
            end if
        end subroutine nc_diag_read_assert_dims_1d_string
        
        subroutine nc_diag_read_assert_dims_2d_byte(var_stor, correct_dims)
            integer(i_byte),dimension(:,:),allocatable,intent(inout):: var_stor
            integer(i_long), dimension(:), intent(in)               :: correct_dims
            integer(i_long), parameter :: correct_ndims = 2
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call ncdr_error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call ncdr_error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1), correct_dims(2)))
            end if
        end subroutine nc_diag_read_assert_dims_2d_byte
        
        subroutine nc_diag_read_assert_dims_2d_short(var_stor, correct_dims)
            integer(i_short),dimension(:,:),allocatable,intent(inout):: var_stor
            integer(i_long), dimension(:), intent(in)                :: correct_dims
            integer(i_long), parameter :: correct_ndims = 2
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call ncdr_error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call ncdr_error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1), correct_dims(2)))
            end if
        end subroutine nc_diag_read_assert_dims_2d_short
        
        subroutine nc_diag_read_assert_dims_2d_long(var_stor, correct_dims)
            integer(i_long),dimension(:,:),allocatable,intent(inout):: var_stor
            integer(i_long), dimension(:), intent(in)               :: correct_dims
            integer(i_long), parameter :: correct_ndims = 2
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call ncdr_error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call ncdr_error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1), correct_dims(2)))
            end if
        end subroutine nc_diag_read_assert_dims_2d_long
        
        subroutine nc_diag_read_assert_dims_2d_float(var_stor, correct_dims)
            real(r_single),dimension(:,:),allocatable,intent(inout):: var_stor
            integer(i_long), dimension(:), intent(in)              :: correct_dims
            integer(i_long), parameter :: correct_ndims = 2
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call ncdr_error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call ncdr_error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1), correct_dims(2)))
            end if
        end subroutine nc_diag_read_assert_dims_2d_float
        
        subroutine nc_diag_read_assert_dims_2d_double(var_stor, correct_dims)
            real(r_double),dimension(:,:),allocatable,intent(inout):: var_stor
            integer(i_long), dimension(:), intent(in)              :: correct_dims
            integer(i_long), parameter :: correct_ndims = 2
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call ncdr_error("Invalid number of dimensions for variable!")
                if (any(shape(var_stor) /= correct_dims)) &
                    call ncdr_error("Mismatched dimensions for variable storage!")
            else
                allocate(var_stor(correct_dims(1), correct_dims(2)))
            end if
        end subroutine nc_diag_read_assert_dims_2d_double
        
        subroutine nc_diag_read_assert_dims_2d_string(var_stor, correct_dims)
            character(len=:),dimension(:,:),allocatable,intent(inout):: var_stor
            integer(i_long), dimension(:), intent(in)                :: correct_dims
            integer(i_long), parameter :: correct_ndims = 3
            
            ! If allocated, make sure the dimensions are correct.
            ! If not, go ahead and allocate it ourselves.
            if (allocated(var_stor)) then
                if (size(correct_dims) /= correct_ndims) &
                    call ncdr_error("Invalid number of dimensions for variable!")
                if (len(var_stor) /= correct_dims(1)) &
                    call ncdr_error("Mismatched dimensions for variable storage!")
                if (any(shape(var_stor) /= correct_dims(2:3))) &
                    call ncdr_error("Mismatched dimensions for variable storage!")
            else
                allocate(character(len=correct_dims(1)) :: var_stor(correct_dims(2), correct_dims(3)))
            end if
        end subroutine nc_diag_read_assert_dims_2d_string
end module ncdr_alloc_assert
