subroutine write_bkgvars_grid(a,b,c,d,mype)
!$$$  subroutine documentation block
!
! subprogram:    write_bkgvars_grid
!
!   prgrmmr:
!
! abstract:  modified routine to write out files to compare spectral computation
!            of horizontal derivatives with the derivatives that are being
!            carried around for the dynamical balance constraint
!
! program history log:
!   2008-03-27  safford -- add subprogram doc block, rm unused vars and uses
!
!   input argument list:
!     mype     - mpi task id
!     a        -
!     b        -
!     c        -
!     d        -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use gridmod, only: nlat,nlon,nsig,lat2,lon2
  implicit none

  integer(i_kind),intent(in):: mype
  character(255):: grdfile

  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: a,b,c
  real(r_kind),dimension(lat2,lon2),intent(in):: d

  real(r_kind),dimension(nlat,nlon,nsig):: ag,bg,cg
  real(r_kind),dimension(nlat,nlon):: dg

  real(r_single),dimension(nlon,nlat,nsig):: a4,b4,c4
  real(r_single),dimension(nlon,nlat):: d4

  integer(i_kind) ncfggg,iret,i,j,k

! gather stuff to processor 0
  do k=1,nsig
    call gather_stuff2(a(1,1,k),ag(1,1,k),mype,0)
    call gather_stuff2(b(1,1,k),bg(1,1,k),mype,0)
    call gather_stuff2(c(1,1,k),cg(1,1,k),mype,0)
  end do
  call gather_stuff2(d,dg,mype,0)

  if (mype==0) then
    write(6,*) 'WRITE OUT NEW VARIANCES'
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
    grdfile='bkgvar_rewgt.grd'
    ncfggg=len_trim(grdfile)
    call baopenwt(22,grdfile(1:ncfggg),iret)
    call wryte(22,4*nlat*nlon*nsig,a4)
    call wryte(22,4*nlat*nlon*nsig,b4)
    call wryte(22,4*nlat*nlon*nsig,c4)
    call wryte(22,4*nlat*nlon,d4)
    call baclose(22,iret)
  end if
   
  return
end subroutine write_bkgvars_grid

subroutine load_grid2(grid_in,grid_out)
!$$$  subroutine documentation block
!
! subprogram:    load_grid2
!
!   prgrmmr:
!
! abstract: 
!
! program history log:
!   2008-03-27  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     grid_in  - input grid 
!
!   output argument list:
!     grid_out - output grid
!
! attributes:
!   language:  f90
!   machine:
!
!$$$
  use kinds, only: i_kind,r_kind
  use gridmod, only:  nlat,nlon
  implicit none

  real(r_kind),dimension(nlat,nlon),intent(in):: grid_in        ! input grid
  real(r_kind),dimension(nlon,nlat-2),intent(out):: grid_out    ! output grid

  integer(i_kind) i,j,nlatm1,jj,j2

! Transfer contents of local array to output array.
  nlatm1=nlat-1
  do j=2,nlatm1
    jj=j-1
    j2=nlat-j+1
    do i=1,nlon
      grid_out(i,jj)=grid_in(j2,i)
    end do
  end do

  return
end subroutine load_grid2

subroutine fill_ns2(grid_in,grid_out)
!$$$  subroutine documentation block
!
! subprogram:    fill_ns2
!
!   prgrmmr:
!
! abstract: 
!
! program history log:
!   2008-03-27  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     grid_in  - input grid 
!
!   output argument list:
!     grid_out - output grid
!
! attributes:
!   language:  f90
!   machine:
!
!$$$
  use kinds, only: i_kind,r_kind
  use gridmod, only: nlat,nlon
  implicit none

  real(r_kind),dimension(nlon,nlat-2),intent(in):: grid_in  ! input grid
  real(r_kind),dimension(nlat,nlon),intent(out):: grid_out  ! output grid
!  Declare local variables
  integer(i_kind) i,j,jj,nlatm2
  real(r_kind) rnlon,sumn,sums

!  Transfer contents of input grid to local work array
!  Reverse ordering in j direction from n-->s to s-->n
  do j=2,nlat-1
    jj=nlat-j
    do i=1,nlon
      grid_out(j,i)=grid_in(i,jj)
    end do
  end do

!  Compute mean along southern and northern latitudes
  sumn=0.
  sums=0.
  nlatm2=nlat-2
  do i=1,nlon
    sumn=sumn+grid_in(i,1)
    sums=sums+grid_in(i,nlatm2)
  end do
  rnlon=1./float(nlon)
  sumn=sumn*rnlon
  sums=sums*rnlon

!  Load means into local work array
  do i=1,nlon
    grid_out(1,i)   =sums
    grid_out(nlat,i)=sumn
  end do

  return
end subroutine fill_ns2

