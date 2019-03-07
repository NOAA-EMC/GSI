module ncdr_types
    use ncd_kinds, only: i_long
    
    implicit none
    
    ! Dimensions type - type for storing all of the dimensions in the
    !                   file
    ! Allocates to the number of dimensions in the file
    type ncdr_dim
        ! Dimension names
        character(len=:), allocatable :: dim_name
        ! Dimension IDs
        integer(i_long)               :: dim_id
        ! Dimension sizes
        integer(i_long)               :: dim_size
        ! Boolean whether the dimension is unlimited or not!
        logical                       :: dim_unlim
    end type ncdr_dim
    
    ! Dimension indicies type - type for storing all of the dimension
    !                           indicies for a single variable
    ! Allocates to the number of indicies within each variable
    
    ! Variables type - type for storing all variables in the file
    ! Allocates to the number of variables in the file
    type ncdr_var
        ! Variable name
        character(len=:),   allocatable            :: var_name
        ! Variable ID
        integer(i_long)                            :: var_id
        ! Variable type
        integer(i_long)                            :: var_type
        ! Variable number of dimensions
        integer(i_long)                            :: var_ndims
        ! Dimension indexes - all of the dimension indicies for a
        ! single variable
        integer(i_long), dimension(:), allocatable :: var_dim_inds
        ! Actual dimensions
        integer(i_long), dimension(:), allocatable :: var_dim_sizes
    end type ncdr_var
    
    ! File type - type for storing a single file's metadata
    ! Allocates to the number of files
    type ncdr_file
        character(:), allocatable                 :: filename
        integer(i_long)                           :: ncid
        integer(i_long)                           :: ndims
        type(ncdr_dim), dimension(:), allocatable :: dims
        integer(i_long)                           :: nvars
        type(ncdr_var), dimension(:), allocatable :: vars
        logical                                   :: file_open = .TRUE.
    end type ncdr_file
end module ncdr_types
