! polyfill for nc_inq_unlimdims
! (Polyfill = code that provides API support when API support is
! missing!)
! Needed to supplement Fortran API, since the NetCDF devs were a bit
! lazy with the Fortran side of things...

module netcdf_unlimdims
    use iso_c_binding
    implicit none
    
    interface
        integer (C_INT) function nc_inq_unlimdims(ncid, nunlimdimsp, unlimdimidsp) bind(c)
            use iso_c_binding
            integer(c_int), value :: ncid
            type(c_ptr),    value :: nunlimdimsp
            type(c_ptr),    value :: unlimdimidsp
        end function
    end interface 
    
    contains
        ! pf = polyfill
        integer(c_int) function pf_nf90_inq_unlimdims(ncid, num_unlim_dims, unlim_dims)
            integer(c_int), intent(in)            :: ncid
            integer(c_int), target, intent(inout) :: num_unlim_dims
            integer(c_int), target, intent(out), optional :: unlim_dims(:)
            
            integer :: i
            
            if (present(unlim_dims)) then
                ! Assume num_unlim_dims is set!
                pf_nf90_inq_unlimdims = nc_inq_unlimdims(ncid, c_loc(num_unlim_dims), c_loc(unlim_dims))
                
                do i = 1, num_unlim_dims
                    unlim_dims(i) = unlim_dims(i) + 1
                end do
            else
                pf_nf90_inq_unlimdims = nc_inq_unlimdims(ncid, c_loc(num_unlim_dims), c_null_ptr)
            end if
        end function pf_nf90_inq_unlimdims
        
end module netcdf_unlimdims
