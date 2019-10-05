    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    integer, intent(in), optional :: nslice
    integer, intent(out), optional :: errcode
    integer ncerr, nvar, n1,n2, ncount
    logical return_errcode
    if(present(errcode)) then
       return_errcode=.true.
       errcode = 0
    else
       return_errcode=.false.
    endif
    if (present(nslice)) then
       ncount = nslice
    else
       ncount = 1
    endif
    nvar = get_nvar(dset,varname)
    if (dset%variables(nvar)%ndims /= 1 .and. dset%variables(nvar)%ndims /= 2) then
       if (return_errcode) then
          call nccheck(ncerr,halt=.false.)
          errcode=nf90_ebaddim
          return
       else
          print *,'rank of data array != variable ndims (or ndims-1)'
          stop "stopped"
       endif
    endif
    n1 = dset%variables(nvar)%dimlens(1)
    if (dset%variables(nvar)%ndims == 2) n2 = dset%variables(nvar)%dimlens(2)
    if (allocated(values)) deallocate(values)
    allocate(values(n1))
    if (dset%variables(nvar)%ndims == 2 .and. n2 == 1) then
       ! return slice along last dimension
       ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values,&
               start=(/1,ncount/), count=(/n1,1/))
    else
       ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values)
    endif
    if (return_errcode) then
       call nccheck(ncerr,halt=.false.)
       errcode=ncerr
    else
       call nccheck(ncerr)
    endif
