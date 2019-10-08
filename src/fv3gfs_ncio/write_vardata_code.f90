    type(Dataset), intent(inout) :: dset
    character(len=*), intent(in) :: varname
    integer, intent(in), optional :: nslice
    integer, intent(out), optional :: errcode
    integer ncerr, nvar, ncount, n1,n2,n3
    logical is_slice
    logical return_errcode
    if(present(errcode)) then
       return_errcode=.true.
       errcode = 0
    else
       return_errcode=.false.
    endif
    if (present(nslice)) then
       ncount = nslice
       is_slice = .true.
    else
       is_slice = .false.
    endif
    nvar = get_nvar(dset,varname)
    if (is_slice) then
        if (dset%variables(nvar)%ndims == 4) then
           n1 = dset%variables(nvar)%dimlens(1)
           n2 = dset%variables(nvar)%dimlens(2)
           n3 = dset%variables(nvar)%dimlens(3)
           ncerr = nf90_put_var(dset%ncid, dset%variables(nvar)%varid,values, &
                   start=(/1,1,1,ncount/),count=(/n1,n2,n3,1/))
        else if (dset%variables(nvar)%ndims == 3) then
           n1 = dset%variables(nvar)%dimlens(1)
           n2 = dset%variables(nvar)%dimlens(2)
           ncerr = nf90_put_var(dset%ncid, dset%variables(nvar)%varid,values, &
                   start=(/1,1,ncount/),count=(/n1,n2,1/))
        else if (dset%variables(nvar)%ndims == 2) then
           n1 = dset%variables(nvar)%dimlens(1)
           ncerr = nf90_put_var(dset%ncid, dset%variables(nvar)%varid,values, &
                   start=(/1,ncount/),count=(/n1,1/))
        else if (dset%variables(nvar)%ndims == 1) then
           if (return_errcode) then
              errcode = -1
              return
           else
              print *,'cannot write a slice to a 1d variable'
              stop "stopped"
           endif
        else if (dset%variables(nvar)%ndims > 4) then
           if (return_errcode) then
              errcode = -1
              return
           else
              print *,'only variables up to 4d supported'
              stop "stopped"
           endif
        endif
    else
        ncerr = nf90_put_var(dset%ncid, dset%variables(nvar)%varid, values)
    endif
    if (return_errcode) then
       call nccheck(ncerr,halt=.false.)
       errcode=ncerr
       if (ncerr /= 0) return
    else
       call nccheck(ncerr)
    endif
    ! reset unlim dim size for all variables
    if (dset%variables(nvar)%hasunlim) then
        if (return_errcode) then
           call set_varunlimdimlens_(dset,errcode)
        else
           call set_varunlimdimlens_(dset)
        endif
    endif 
