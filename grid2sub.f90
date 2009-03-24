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
  use gridmod, only: itotsub,nsig,ltosj_s,ltosi_s,lat2,lon2,nlat,nlon,nsig1o
  use jfunc, only: nsst2,noz2,nslt2,ncw2,nsit2,nvp2,nst2,np2,nq2,nt2
  use constants, only:  zero
  implicit none

! Declare passed variables
  real(r_kind),dimension(nlat,nlon,nsig1o),intent(in):: workout
  real(r_kind),dimension(lat2,lon2),intent(out):: p,sst,slndt,sicet
  real(r_kind),dimension(lat2,lon2,nsig),intent(out):: t,q,cwmr,oz,st,vp

! Declare local variables
  integer(i_kind) k,l,ni1,ni2
  real(r_kind),dimension(itotsub,nsig1o):: work1
  real(r_kind),dimension(lat2*lon2*(nsig*6+4)):: xtmp


! Transfer input array to local work array
  do k=1,nsig1o
     do l=1,itotsub
        ni1=ltosi_s(l); ni2=ltosj_s(l)
        work1(l,k)=workout(ni1,ni2,k)
     end do
  end do

! reoder work1 global slab array for communications
  call reorder2(work1,nsig1o)

! zero xtmp work array
! do k=1,lat2*lon2*(nsig*6+4)
!    xtmp(k)=zero
! end do

! send global slabs to subdomains
  call mpi_alltoallv(work1(1,1),iscnt_s,isdsp_s,&
       mpi_rtype,xtmp(1),ircnt_s,irdsp_s,&
       mpi_rtype,mpi_comm_world,ierror)

! load the received subdomain vector
     call vectosub(xtmp(nst2),nsig,st)
     call vectosub(xtmp(nvp2),nsig,vp)
     call vectosub(xtmp(np2),1,p)
     call vectosub(xtmp(nt2),nsig,t)
     call vectosub(xtmp(nq2),nsig,q)
     call vectosub(xtmp(noz2),nsig,oz)
     call vectosub(xtmp(nsst2),1,sst)
     call vectosub(xtmp(nslt2),1,slndt)
     call vectosub(xtmp(nsit2),1,sicet)
     call vectosub(xtmp(ncw2),nsig,cwmr)

 return
end subroutine grid2sub
