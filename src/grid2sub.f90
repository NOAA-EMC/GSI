subroutine grid2sub(workout,t,p,q,oz,sst,slndt,sicet,cwmr,st,vp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grid2sub   converts from full horizontal grid to subdomains
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: converst from horizontal grid to subdomains
!
! program history log:
!   2004-02-03  kleist, new mpi strategy
!   2004-05-06  derber
!   2004-07-15  treadon - handle periodic subdomains
!   2004-07-28  treadon - add only on use declarations; add intent in/out
!   2004-10-26  kleist - u,v removed; periodicity accounted for only in 
!               sub2grid routine if necessary
!
!   input argument list:
!     work1    - input grid values on full grid after horizontal part of cov.
!
!   output argument list:
!     t        - t grid values 
!     p        - p surface grid values 
!     q        - q grid values 
!     oz       - ozone grid values
!     sst      - sea surface skin temperature grid values
!     slndt    - land surface skin temperature grid values
!     sicet    - ice surface skin temperature grid values
!     cwmr     - cloud water mixing ratio grid values
!     st       - streamfunction grid values 
!     vp       - velocity potential grid values 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: irdsp_s,ircnt_s,iscnt_s,isdsp_s,ierror,&
       mpi_comm_world,mpi_rtype,reorder2,vectosub
  use gridmod, only: itotsub,nsig,ltosj_s,ltosi_s,lat2,lon2,nlat,nlon,nsig1o,nnnn1o,latlon1n,latlon11
  use jfunc, only: nsst2,noz2,nslt2,ncw2,nsit2,nvp2,nst2,np2,nq2,nt2
  implicit none

! Declare passed variables
  real(r_kind),dimension(nlat,nlon,nnnn1o),intent(in   ) :: workout
  real(r_kind),dimension(lat2,lon2)       ,intent(  out) :: p,sst,slndt,sicet
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(  out) :: t,q,cwmr,oz,st,vp

! Declare local variables
  integer(i_kind) k,l,ni1,ni2
  real(r_kind),dimension(itotsub,nsig1o):: work1
  real(r_kind),dimension(lat2*lon2*(nsig*6+4_i_kind)):: xtmp


! Transfer input array to local work array
  do k=1,nnnn1o
     do l=1,itotsub
        ni1=ltosi_s(l); ni2=ltosj_s(l)
        work1(l,k)=workout(ni1,ni2,k)
     end do
  end do

! reoder work1 global slab array for communications
  call reorder2(work1,nsig1o,nnnn1o)

! send global slabs to subdomains
  call mpi_alltoallv(work1(1,1),iscnt_s,isdsp_s,&
       mpi_rtype,xtmp(1),ircnt_s,irdsp_s,&
       mpi_rtype,mpi_comm_world,ierror)

! load the received subdomain vector
  call vectosub(xtmp(nst2),latlon1n,st)
  call vectosub(xtmp(nvp2),latlon1n,vp)
  call vectosub(xtmp(np2),latlon11,p)
  call vectosub(xtmp(nt2),latlon1n,t)
  call vectosub(xtmp(nq2),latlon1n,q)
  call vectosub(xtmp(noz2),latlon1n,oz)
  call vectosub(xtmp(nsst2),latlon11,sst)
  call vectosub(xtmp(nslt2),latlon11,slndt)
  call vectosub(xtmp(nsit2),latlon11,sicet)
  call vectosub(xtmp(ncw2),latlon1n,cwmr)

  return
end subroutine grid2sub

subroutine grid2sub2(workout,st,vp,pri,t)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grid2sub2  converts from full horizontal grid to subdomains
!   prgmmr: derber          org: np22                date: 1990-10-06
!
! abstract: converst from horizontal grid to subdomains for balance variable
!
! program history log:
!   2008-05-21  derber, new mpi strategy
!
!   input argument list:
!     workout  - input grid values on full grid after horizontal part of cov.
!
!   output argument list:
!     st       - streamfunction grid values 
!     vp       - velocity potential grid values 
!     pri      - pressure grid values 
!     t        - t grid values 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: ione
  use mpimod, only: irdbal_s,ircbal_s,iscbal_s,isdbal_s,ierror,&
       mpi_comm_world,mpi_rtype,reorder2,vectosub,nlevsbal,nnnvsbal, &
       ku_gs,kv_gs,kt_gs,kp_gs
  use gridmod, only: itotsub,nsig,ltosj_s,ltosi_s,lat2,lon2,nlat,nlon,latlon11
  implicit none

! Declare passed variables
  real(r_kind),dimension(nlat,nlon,nnnvsbal) ,intent(in   ) :: workout
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(  out) :: pri
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(  out) :: t,st,vp

! Declare local variables
  integer(i_kind) k,l,ni1,ni2,ioff
  real(r_kind),dimension(itotsub,nlevsbal):: work1
  real(r_kind),dimension(lat2*lon2*(nsig*4+ione)):: xtmp

! Transfer input array to local work array
  do k=1,nnnvsbal
     do l=1,itotsub
        ni1=ltosi_s(l); ni2=ltosj_s(l)
        work1(l,k)=workout(ni1,ni2,k)
     end do
  end do

! reorder work1 global slab array for communications
  call reorder2(work1,nlevsbal,nnnvsbal)

! send global slabs to subdomains
  call mpi_alltoallv(work1(1,1),iscbal_s,isdbal_s,&
       mpi_rtype,xtmp(1),ircbal_s,irdbal_s,&
       mpi_rtype,mpi_comm_world,ierror)


! load the received subdomain vector
  do k=1,nsig
     ioff=ku_gs(k)*latlon11+ione
     call vectosub(xtmp(ioff),latlon11,st(1,1,k))
     ioff=kv_gs(k)*latlon11+ione
     call vectosub(xtmp(ioff),latlon11,vp(1,1,k))
     ioff=kt_gs(k)*latlon11+ione
     call vectosub(xtmp(ioff),latlon11,t(1,1,k))
  end do
  do k=1,nsig+ione
     ioff=kp_gs(k)*latlon11+ione
     call vectosub(xtmp(ioff),latlon11,pri(1,1,k))
  end do


  return
end subroutine grid2sub2

