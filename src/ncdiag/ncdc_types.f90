module ncdc_types
    use ncd_kinds, only: i_byte, i_short, i_long, r_single, r_double
    
    implicit none
    
    integer(i_long), parameter         :: NC_DIAG_CAT_GZIP_COMPRESS = 6
    integer(i_long), parameter         :: NC_DIAG_CAT_CHUNK_SIZE = 16384
    
    ! Variable dimensions storage
    type nc_diag_cat_dim_names
        character(len=100), dimension(:), allocatable :: dim_names
        integer(i_long),    dimension(:), allocatable :: output_dim_ids
        integer(i_long)                               :: num_names = 0
    end type nc_diag_cat_dim_names
    
    type data_blob
        integer(i_byte),    dimension(:), allocatable :: byte_buffer
        integer(i_short),   dimension(:), allocatable :: short_buffer
        integer(i_long),    dimension(:), allocatable :: long_buffer
        
        real(r_single),     dimension(:), allocatable :: rsingle_buffer
        real(r_double),     dimension(:), allocatable :: rdouble_buffer
        
        character(1)     ,dimension(:,:), allocatable :: string_buffer
        
        integer(i_byte),  dimension(:,:), allocatable :: byte_2d_buffer
        integer(i_short), dimension(:,:), allocatable :: short_2d_buffer
        integer(i_long),  dimension(:,:), allocatable :: long_2d_buffer
        
        real(r_single),   dimension(:,:), allocatable :: rsingle_2d_buffer
        real(r_double),   dimension(:,:), allocatable :: rdouble_2d_buffer
        
        character(1),   dimension(:,:,:), allocatable :: string_2d_buffer
        
        integer(i_long)                               :: cur_pos = 1
        integer(i_long),  dimension(3)                :: alloc_size
    end type data_blob
end module ncdc_types
