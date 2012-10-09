subroutine grid2sub(workout,cstate,sst,slndt,sicet)
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
!   2010-04-01  treadon - move reorder2 and vectosub to gridmod
!   2010-04-28  todling - update to use gsi_bundle
!   2010-05-22  todling - replace individual pointers to xtmp w/ global pointer nsubwhalo
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
       mpi_comm_world,mpi_rtype
  use gridmod, only: itotsub,nsig,ltosj_s,ltosi_s,lat2,lon2,nlat,nlon,nsig1o,nnnn1o, &
       latlon1n,latlon11,vlevs,reorder2,vectosub
  use control_vectors, only: nrf,nrf_var,nrf_3d
  use control_vectors, only: mvars  ! need to pass motley to avoid passing this via common
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  real(r_kind),dimension(nlat,nlon,nnnn1o),intent(in   ) :: workout
  real(r_kind),dimension(lat2,lon2)       ,intent(  out) :: sst,slndt,sicet
  type(gsi_bundle)                        ,intent(inout) :: cstate

! Declare local variables
  character(len=*), parameter :: myname='grid2sub'
  integer(i_kind) ii,jj,i,k,l,ni1,ni2,n,istatus
  integer(i_kind),allocatable,dimension(:):: nsubwhalo ! indexes to sub-domain with halo
  real(r_kind),dimension(itotsub,nsig1o):: work1
  real(r_kind),dimension(lat2*lon2*vlevs):: xtmp
  real(r_kind),pointer:: rank2(:,:)
  real(r_kind),pointer:: rank3(:,:,:)


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

! do some checking
  if (cstate%grid%im*cstate%grid%jm/=latlon11) then
      write(6,*) trim(myname),': inconsistent 2d dims'
      call stop2(999)
  endif
  if (cstate%grid%im*cstate%grid%jm*cstate%grid%km/=latlon1n) then
      write(6,*) trim(myname),': inconsistent 3d dims'
      call stop2(999)
  endif

  allocate(nsubwhalo(nrf+mvars))

  ii=0
  do i=1,nrf
     if(nrf_3d(i)) then
        n=latlon1n
     else
        n=latlon11
     endif
     nsubwhalo(i)=ii+1
     ii=ii+n
  enddo
  do i=nrf+1,nrf+mvars
     nsubwhalo(i)=ii+1
     ii=ii+latlon11
  enddo

  ii=0
  do i=1,nrf
     if(nrf_3d(i)) then
        call gsi_bundlegetpointer (cstate,trim(nrf_var(i)),rank3,istatus)
        if(istatus==0) then
           n=size(rank3)
           call vectosub(xtmp(nsubwhalo(i)),n,rank3)
        else
           write(6,*) myname,': cannot find 3d pointer'
           call stop2(999)
        endif
     else
        call gsi_bundlegetpointer (cstate,trim(nrf_var(i)),rank2,istatus)
        if(istatus==0) then
           if(trim(nrf_var(i))=='sst') then ! _RT better handled via motley
              n=size(sst)
              call vectosub(xtmp(nsubwhalo(i)),n,sst)
           else
              n=size(rank2)
              call vectosub(xtmp(nsubwhalo(i)),n,rank2)
           endif
        else
           write(6,*) myname,': cannot find 2d pointer'
           call stop2(999)
        endif
     endif
  enddo
  if(mvars>0) then  ! _RT should be done via motley
     ii=nrf+1
     n=size(slndt)
     call vectosub(xtmp(nsubwhalo(ii)),n,slndt)
     ii=ii+1
     n=size(sicet)
     call vectosub(xtmp(nsubwhalo(ii)),n,sicet)
  endif

  deallocate(nsubwhalo)

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
!   2010-04-01  treadon - move reorder2 and vectosub to gridmod
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
  use mpimod, only: irdbal_s,ircbal_s,iscbal_s,isdbal_s,ierror,&
       mpi_comm_world,mpi_rtype,nlevsbal,nnnvsbal, &
       ku_gs,kv_gs,kt_gs,kp_gs
  use gridmod, only: itotsub,nsig,ltosj_s,ltosi_s,lat2,lon2,nlat,nlon,latlon11,&
       reorder2,vectosub
  implicit none

! Declare passed variables
  real(r_kind),dimension(nlat,nlon,nnnvsbal),intent(in   ) :: workout
  real(r_kind),dimension(lat2,lon2,nsig+1)  ,intent(  out) :: pri
  real(r_kind),dimension(lat2,lon2,nsig)    ,intent(  out) :: t,st,vp

! Declare local variables
  integer(i_kind) k,l,ni1,ni2,ioff
  real(r_kind),dimension(itotsub,nlevsbal):: work1
  real(r_kind),dimension(lat2*lon2*(nsig*4+1)):: xtmp

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
!$omp parallel do  schedule(dynamic,1) private(k,ioff)
  do k=1,nsig+1
    if(k <= nsig)then
       ioff=ku_gs(k)*latlon11+1
       call vectosub(xtmp(ioff),latlon11,st(1,1,k))
       ioff=kv_gs(k)*latlon11+1
       call vectosub(xtmp(ioff),latlon11,vp(1,1,k))
       ioff=kt_gs(k)*latlon11+1
       call vectosub(xtmp(ioff),latlon11,t(1,1,k))
    end if
    ioff=kp_gs(k)*latlon11+1
    call vectosub(xtmp(ioff),latlon11,pri(1,1,k))
  end do


  return
end subroutine grid2sub2

