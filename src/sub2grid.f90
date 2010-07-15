subroutine sub2grid(workin,cstate,sst,slndt,sicet,iflg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sub2grid adjoint of grid2sub
!   prgmmr: parrish          org: np22                date: 1994-02-12
!
! abstract: adjoint of horizontal grid to subdomain
!
! program history log:
!   2004-02-03  kleist, new mpi strategy
!   2004-05-06  derber 
!   2004-07-15  treadon - handle periodic subdomains
!   2004-07-28  treadon - add only on use declarations; add intent in/out
!   2004-10-26  kleist - remove u,v; do periodic update on st,vp
!   2004-03-30  treaon - change work1 dimension to max(iglobal,itotsub)
!   2008-04-03  safford - rm unused vars and uses                       
!   2010-04-01  treadon - move strip,strip_periodic,reorder to gridmod
!   2010-04-28  todling - update to use gsi_bundle
!   2010-05-22  todling - replace individual pointers to xtmp w/ global pointer nsubnhalo
!
!   input argument list:
!     t        - t grid values                    
!     p        - p surface grid values                   
!     q        - q grid values                     
!     oz       - ozone grid values                            
!     sst      - sea surface temperature grid  
!     slndt    - land surface temperature 
!     sicet    - ice surface temperature                        
!     cwmr     - cloud water mixing ratio grid values                     
!     st       - streamfunction grid values                     
!     vp       - velocity potential grid values                     
!     iflg     = 1=not periodic, 2=periodic
!
!   output argument list:
!     workin   - output grid values on full grid after vertical operations
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: irdsp_g,ircnt_g,iscnt_g,isdsp_g,&
       ierror,mpi_comm_world,mpi_rtype
  use gridmod, only: itotsub,lat1,lon1,lat2,lon2,iglobal,&
       nlat,nlon,nsig,ltosi,ltosj,nsig1o,nnnn1o,vlevs,strip,strip_periodic,reorder
  use control_vectors, only: nrf,nrf_var,nrf_3d
  use control_vectors, only: mvars  ! need to pass motley to avoid passing this via common
  use constants, only: zero
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  integer(i_kind)                         ,intent(in   ) :: iflg
  real(r_kind),dimension(lat2,lon2)       ,intent(in   ) :: sst,slndt,sicet
  real(r_kind),dimension(nlat,nlon,nnnn1o),intent(  out) :: workin
  type(gsi_bundle)                        ,intent(inout) :: cstate

! Declare local variables
  character(len=*),parameter:: myname='sub2grid'
  integer(i_kind) ii,i,j,k,l,n,ni1,ni2,istatus
  integer(i_kind),allocatable,dimension(:):: nsubnhalo         ! indexes to sub-domain w/o halo
  real(r_kind),dimension(lat1*lon1*vlevs):: xhatsm
  real(r_kind),dimension(max(iglobal,itotsub),nsig1o):: work1  ! contain nsig1o slab of any variables
  real(r_kind),pointer::rank2(:,:)
  real(r_kind),pointer::rank3(:,:,:)

! Initialize variables
  xhatsm=zero
  workin=zero

  allocate(nsubnhalo(nrf+mvars))

  ii=0
  do i=1,nrf
     if(nrf_3d(i)) then
        n=lat1*lon1*nsig
     else
        n=lat1*lon1
     endif
     nsubnhalo(i)=ii+1
     ii=ii+n
  enddo
  do i=nrf+1,nrf+mvars
     nsubnhalo(i)=ii+1
     ii=ii+lat1*lon1
  enddo


! strip off boundary points and load vector for communication
  if (iflg==1) then

     do i=1,nrf
        if (nrf_3d(i)) then
            call gsi_bundlegetpointer (cstate,trim(nrf_var(i)),rank3,istatus)
            if(istatus==0) then 
               call strip(rank3,xhatsm(nsubnhalo(i)),nsig)
            else
               write(6,*) myname,': cannot find 3d pointer'
               call stop2(999)
            endif
        else
            call gsi_bundlegetpointer (cstate,trim(nrf_var(i)),rank2,istatus)
            if(istatus==0) then 
               if(trim(nrf_var(i))=='sst') then ! _RT shouuld be done via motley
                  call strip(sst  ,xhatsm(nsubnhalo(i)),1)
               else
                  call strip(rank2,xhatsm(nsubnhalo(i)),1)
               endif
            else
               write(6,*) myname,': cannot find 2d pointer'
               call stop2(999)
            endif
        endif
        if(mvars>0) then  ! _RT should be done via motley
           ii=nrf+1
           call strip(slndt,xhatsm(nsubnhalo(ii)),1)
           ii=ii+1
           call strip(sicet,xhatsm(nsubnhalo(ii)),1)
        endif
     enddo

  elseif (iflg==2) then

     do i=1,nrf
        if (nrf_3d(i)) then
            call gsi_bundlegetpointer (cstate,trim(nrf_var(i)),rank3,istatus)
            if(istatus==0) then 
               call strip_periodic(rank3,xhatsm(i),nsig)
            else
               write(6,*) myname,': cannot find 3d pointer'
               call stop2(999)
            endif
        else
            call gsi_bundlegetpointer (cstate,trim(nrf_var(i)),rank2,istatus)
            if(istatus==0) then 
               if(trim(nrf_var(i))=='sst') then ! _RT shouuld be done via motley
                  call strip_periodic(sst  ,xhatsm(i),1)
               else
                  call strip_periodic(rank2,xhatsm(i),1)
               endif
            else
               write(6,*) myname,': cannot find 2d pointer'
               call stop2(999)
            endif
        endif
        if(mvars>0) then  ! _RT should be done via motley
           ii=nrf+1
           call strip_periodic(slndt,xhatsm(ii),1)
           ii=ii+lat1*lon1+1
           call strip_periodic(sicet,xhatsm(ii),1)
        endif
     enddo

  else
     write(6,*)'SUB2GRID:  ***ERROR*** iflg=',iflg,' is an illegal value'
     call stop2(999)
  endif


! zero out work arrays
  do k=1,nsig1o
     do j=1,itotsub
        work1(j,k)=zero
     end do
  end do
! send subdomain vector to global slabs
  call mpi_alltoallv(xhatsm(1),iscnt_g,isdsp_g,&
       mpi_rtype,work1(1,1),ircnt_g,irdsp_g,mpi_rtype,&
       mpi_comm_world,ierror)

! reorder work1 array post communication
  call reorder(work1,nsig1o,nnnn1o)
  do k=1,nnnn1o
     do l=1,iglobal
        ni1=ltosi(l); ni2=ltosj(l)
        workin(ni1,ni2,k)=work1(l,k)
     end do
  end do

  deallocate(nsubnhalo)

  return
end subroutine sub2grid

subroutine sub2grid2(workin,st,vp,pri,t,iflg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sub2grid2 adjoint of grid2sub for bal
!   prgmmr: parrish          org: np22                date: 1994-02-12
!
! abstract: adjoint of horizontal grid to subdomain for balance variables
!
! program history log:
!   2008-04-21  derber, new mpi strategy
!
!   input argument list:
!     st       - streamfunction grid values                     
!     vp       - velocity potential grid values                     
!     pri      - p surface grid values                   
!     t        - t grid values                    
!     iflg     = 1=not periodic, 2=periodic
!
!   output argument list:
!     workin   - output grid values on full grid after vertical operations
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: irdbal_g,ircbal_g,iscbal_g,isdbal_g,&
       ierror,mpi_comm_world,mpi_rtype,&
       nnnvsbal,nlevsbal,ku_gs,kv_gs, &
       kp_gs,kt_gs
  use gridmod, only: itotsub,lat1,lon1,lat2,lon2,iglobal,&
       nlat,nlon,nsig,ltosi,ltosj,&
       strip,strip_periodic,reorder
  use constants, only: zero
  implicit none

! Declare passed variables
  integer(i_kind)                            ,intent(in   ) :: iflg
  real(r_kind),dimension(lat2,lon2,nsig+1)   ,intent(in   ) :: pri
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: t,vp,st
  real(r_kind),dimension(nlat,nlon,nnnvsbal) ,intent(  out) :: workin

! Declare local variables
  integer(i_kind) j,k,l,ni1,ni2,ioff
  real(r_kind),dimension(lat1*lon1*(nsig*4+1)):: xhatsm
  real(r_kind),dimension(max(iglobal,itotsub),nlevsbal):: work1  !  contain nsig1o slab of any variables


! strip off boundary points and load vector for communication
  if (iflg==1) then
     do k=1,nsig
        ioff=ku_gs(k)*lat1*lon1+1
        call strip(st (1,1,k),xhatsm(ioff),1)
        ioff=kv_gs(k)*lat1*lon1+1
        call strip(vp (1,1,k),xhatsm(ioff),1)
        ioff=kt_gs(k)*lat1*lon1+1
        call strip(t  (1,1,k),xhatsm(ioff),1)
     end do
     do k=1,nsig+1
        ioff=kp_gs(k)*lat1*lon1+1
        call strip(pri(1,1,k),xhatsm(ioff),1)
     end do
  elseif (iflg==2) then
     do k=1,nsig
        ioff=ku_gs(k)*lat1*lon1+1
        call strip_periodic(st (1,1,k),xhatsm(ioff),1)
        ioff=kv_gs(k)*lat1*lon1+1
        call strip_periodic(vp (1,1,k),xhatsm(ioff),1)
        ioff=kt_gs(k)*lat1*lon1+1
        call strip_periodic(t  (1,1,k),xhatsm(ioff),1)
     end do
     do k=1,nsig+1
        ioff=kp_gs(k)*lat1*lon1+1
        call strip_periodic(pri(1,1,k),xhatsm(ioff),1)
     end do
  else
     write(6,*)'SUB2GRID:  ***ERROR*** iflg=',iflg,' is an illegal value'
     call stop2(999)
  endif


! zero out work arrays
  do k=1,nlevsbal
     do j=1,itotsub
        work1(j,k)=zero
     end do
  end do
! send subdomain vector to global slabs
  call mpi_alltoallv(xhatsm(1),iscbal_g,isdbal_g,&
       mpi_rtype,work1(1,1),ircbal_g,irdbal_g,mpi_rtype,&
       mpi_comm_world,ierror)

! reorder work1 array post communication
  call reorder(work1,nlevsbal,nnnvsbal)
  do k=1,nnnvsbal
     do l=1,iglobal
        ni1=ltosi(l); ni2=ltosj(l)
        workin(ni1,ni2,k)=work1(l,k)
     end do
  end do

  return
end subroutine sub2grid2

