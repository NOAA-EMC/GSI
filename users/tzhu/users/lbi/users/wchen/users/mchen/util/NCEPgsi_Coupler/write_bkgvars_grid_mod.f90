subroutine write_bkgvars_grid_mod(a,b,c,d,ntime,mype,cname)
!$$$  subroutine documentation block
!
! subprogram:    write_bkgvars_grid_mod
!
!   prgrmmr:
!
! abstract:  modified routine to write out files to compare spectral computation
!            of horizontal derivatives with the derivatives that are being
!            carried around for the dynamical balance constraint
!
! program history log:
!   2008-03-27  safford -- add subprogram doc block, rm unused vars and uses
!   2010-03-10  rancic - modifications to monitore run of nonlinear model for 4dvar
!
!   input argument list:
!     mype     - mpi task id
!     a        - 3d array
!     b        - 3d array
!     c        - 3d array
!     d        - 2d array
!     ntime    - number of time steps
!     cname    - title
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use constants, only: izero
  use gridmod, only: nlat,nlon,nsig,lat2,lon2
  implicit none

  integer(i_kind)                       ,intent(in   ) :: ntime,mype
  character(7)                          ,intent(in   ) :: cname
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: a,b,c
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: d

  character(4)  :: ctime
  character(255):: grdfile

  real(r_kind),dimension(nlat,nlon,nsig):: ag,bg,cg
  real(r_kind),dimension(nlat,nlon):: dg

  real(r_single),dimension(nlon,nlat,nsig):: a4,b4,c4
  real(r_single),dimension(nlon,nlat):: d4

  integer(i_kind) ncfggg,iret,i,j,k

! gather stuff to processor 0
  do k=1,nsig
     call gather_stuff2(a(1,1,k),ag(1,1,k),mype,izero)
     call gather_stuff2(b(1,1,k),bg(1,1,k),mype,izero)
     call gather_stuff2(c(1,1,k),cg(1,1,k),mype,izero)
  end do
  call gather_stuff2(d,dg,mype,izero)

  if (mype==izero) then
     write(6,*) 'WRITE OUT UPDATED FIELDS'
! load single precision arrays
     do k=1,nsig
        do j=1,nlon
           do i=1,nlat
              a4(j,i,k)=ag(i,j,k)
              b4(j,i,k)=bg(i,j,k)
              c4(j,i,k)=cg(i,j,k)
           end do
        end do
     end do
     do j=1,nlon
        do i=1,nlat
           d4(j,i)=dg(i,j)
        end do
     end do

! Create byte-addressable binary file for grads
!!     grdfile='bkgvar_rewgt.grd'
    write(ctime,'(i4.4)') ntime
    grdfile=cname//ctime//'.grd'

     ncfggg=len_trim(grdfile)
     call baopenwt(22_i_kind,grdfile(1:ncfggg),iret)
     call wryte(22_i_kind,4*nlat*nlon*nsig,a4)
     call wryte(22_i_kind,4*nlat*nlon*nsig,b4)
     call wryte(22_i_kind,4*nlat*nlon*nsig,c4)
     call wryte(22_i_kind,4*nlat*nlon,d4)
     call baclose(22_i_kind,iret)
  end if
   
  return
end subroutine write_bkgvars_grid_mod


