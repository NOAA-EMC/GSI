    type(Dataset), intent(inout) :: dset
    character(len=*), intent(in) :: varname
    integer, intent(in), optional :: nslice
    integer ncerr, nvar
    if (present(nslice)) then
       print *,'cannot write slice to 1d var'
       stop "stopped"
    endif
    nvar = get_nvar(dset,varname)
    ncerr = nf90_put_var(dset%ncid, dset%variables(nvar)%varid, values)
    call nccheck(ncerr)
    ! reset unlim dim size for all variables
    if (dset%variables(nvar)%hasunlim) call set_varunlimdimlens_(dset)
