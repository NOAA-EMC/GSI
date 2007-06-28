subroutine deter_subdomain(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_subdomain          perform domain decomposition
!   prgmmr: weiyu yang       org: np20                date: 1998-05-14
!
! abstract: Given an array of the observation computation load and
!           the number of available mpi tasks (npe), this routine 
!           decomposes the total analysis grid into npe subdomains
!
! program history log:
!   1998-05-14  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-01  treadon - simplify algorithm
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2005-10-17  derber - rewrite routine using simpler algorithm
!   2005-10-26  treadon - correct error in 100 format text
!
!   input argument list:
!     mype      - mpi task number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: periodic,periodic_s,lon1,lat1,nlon,nlat,&
       ilat1,istart,jlon1,jstart
  use mpimod, only: npe
  use constants, only: zero,one
  use satthin, only: indexx
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local variables
  integer(i_kind) npts,nrnc,iinum,iileft,jrows,jleft,k,i,jjnum
  integer(i_kind) j,mm1,iicnt,ipts,jjleft
  integer(i_kind),dimension(npe+1):: iiend,jjend,iistart
  real(r_kind):: anperpe

!************************************************************************
! Compute number of points on full grid and target number of
! point per mpi task (pe)
  npts=nlat*nlon
  anperpe=float(npts)/float(npe)

! Start with square subdomains
  nrnc=sqrt(anperpe)
  iinum=nlon/nrnc
  iicnt=nlon/iinum
  iileft=nlon-iicnt*iinum
  jrows=npe/iinum
  jleft=npe-jrows*iinum

! Adjust subdomain boundaries 
  k=0
  istart=1
  jstart=1
  iistart(1)=1
  do i=1,iinum
     ipts = iicnt
     if(i <= iileft)ipts=ipts+1
     iiend(i)=iistart(i)+ipts-1
     iistart(i+1)=iiend(i)+1
     jjnum=jrows
     if(i <= jleft)jjnum=jrows+1
     do j=1,jjnum
        k=k+1
        jlon1(k)=ipts
        jstart(k)= iistart(i)
        ilat1(k)=nlat/jjnum
        jjleft=nlat-ilat1(k)*jjnum
        if(j <= jjleft)ilat1(k)=ilat1(k)+1
        if(j > 1)istart(k)=jjend(j-1)+1
        jjend(j)=istart(k)+ilat1(k)-1

        if (jlon1(k)==nlon) then
           periodic=.true.
           periodic_s(k)=.true.
        endif
        if(mype == 0) &
             write(6,100) k,istart(k),jstart(k),ilat1(k),jlon1(k)
     end do
  end do
100 format('DETER_SUBDOMAIN:  task,istart,jstart,ilat1,jlon1=',6(i6,1x))
  
         
! Set number of latitude and longitude for given subdomain
  mm1=mype+1
  lat1=ilat1(mm1)
  lon1=jlon1(mm1)

  return
end subroutine deter_subdomain
