    type(Dataset), intent(inout) :: dset
    character(len=*), intent(in) :: varname
    integer, intent(in), optional :: nslice
    integer ncerr, nvar, ncount, n1
    logical is_slice
    if (present(nslice)) then
       ncount = nslice
       is_slice = .true.
    else
       is_slice = .false.
    endif
    nvar = get_nvar(dset,varname)
    if (is_slice) then
        n1 = dset%variables(nvar)%dimlens(1)
        ncerr = nf90_put_var(dset%ncid, dset%variables(nvar)%varid,values, &
                start=(/1,ncount/),count=(/n1,1/))
        call nccheck(ncerr)
    else
        ncerr = nf90_put_var(dset%ncid, dset%variables(nvar)%varid, values)
        call nccheck(ncerr)
    endif
    ! reset unlim dim size for all variables
    if (dset%variables(nvar)%hasunlim) call set_varunlimdimlens_(dset)
