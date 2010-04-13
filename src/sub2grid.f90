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
  use jfunc, only: nsstsm,nozsm,nsltsm,ncwsm,nsitsm,nvpsm,nstsm,&
       npsm,nqsm,ntsm
  use constants, only: ione,izero,zero
  use control_vectors
  implicit none

! Declare passed variables
  integer(i_kind)                         ,intent(in   ) :: iflg
  real(r_kind),dimension(lat2,lon2)       ,intent(in   ) :: sst,slndt,sicet
  real(r_kind),dimension(nlat,nlon,nnnn1o),intent(  out) :: workin
  type(control_state)                     ,intent(in   ) :: cstate

! Declare local variables
  integer(i_kind) j,k,l,ni1,ni2
  real(r_kind),dimension(lat1*lon1*vlevs):: xhatsm
  real(r_kind),dimension(max(iglobal,itotsub),nsig1o):: work1  !  contain nsig1o slab of any variables

! Initialize variables
! do k=1,nsig1o
!    do j=1,nlon
!       do i=1,nlat
!          workin(i,j,k)=zero
!       end do
!    end do
! end do
! do k=1,lat1*lon1*(nsig*6+4_i_kind)
!    xhatsm(k)=zero
! end do
  xhatsm=zero
  workin=zero

! strip off boundary points and load vector for communication
  if (iflg==ione) then
     call strip(cstate%st   ,xhatsm(nstsm) ,nsig)
     call strip(cstate%vp   ,xhatsm(nvpsm) ,nsig)
     call strip(cstate%p    ,xhatsm(npsm)  ,ione)
     call strip(cstate%t    ,xhatsm(ntsm)  ,nsig)
     call strip(cstate%rh   ,xhatsm(nqsm)  ,nsig)
     if (nrf3_oz>izero) call strip(cstate%oz,xhatsm(nozsm),nsig)
     if (nrf2_sst>izero) then
        call strip(sst  ,xhatsm(nsstsm),ione)
        call strip(slndt,xhatsm(nsltsm),ione)
        call strip(sicet,xhatsm(nsitsm),ione)
     end if
     if (nrf3_cw>izero) call strip(cstate%cw ,xhatsm(ncwsm) ,nsig)
  elseif (iflg==2_i_kind) then
     call strip_periodic(cstate%st   ,xhatsm(nstsm) ,nsig)
     call strip_periodic(cstate%vp   ,xhatsm(nvpsm) ,nsig)
     call strip_periodic(cstate%p    ,xhatsm(npsm)  ,ione)
     call strip_periodic(cstate%t    ,xhatsm(ntsm)  ,nsig)
     call strip_periodic(cstate%rh   ,xhatsm(nqsm)  ,nsig)
     if (nrf3_oz>izero) call strip_periodic(cstate%oz,xhatsm(nozsm),nsig)
     if (nrf2_sst>izero) then
        call strip_periodic(sst  ,xhatsm(nsstsm),ione)
        call strip_periodic(slndt,xhatsm(nsltsm),ione)
        call strip_periodic(sicet,xhatsm(nsitsm),ione)
     end if
     if (nrf3_cw>izero) call strip_periodic(cstate%cw ,xhatsm(ncwsm) ,nsig)
  else
     write(6,*)'SUB2GRID:  ***ERROR*** iflg=',iflg,' is an illegal value'
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
  use constants, only: ione,zero
  implicit none

! Declare passed variables
  integer(i_kind)                            ,intent(in   ) :: iflg
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(in   ) :: pri
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: t,vp,st
  real(r_kind),dimension(nlat,nlon,nnnvsbal) ,intent(  out) :: workin

! Declare local variables
  integer(i_kind) j,k,l,ni1,ni2,ioff
  real(r_kind),dimension(lat1*lon1*(nsig*4+ione)):: xhatsm
  real(r_kind),dimension(max(iglobal,itotsub),nlevsbal):: work1  !  contain nsig1o slab of any variables


! strip off boundary points and load vector for communication
  if (iflg==ione) then
     do k=1,nsig
        ioff=ku_gs(k)*lat1*lon1+ione
        call strip(st (1,1,k),xhatsm(ioff),ione)
        ioff=kv_gs(k)*lat1*lon1+ione
        call strip(vp (1,1,k),xhatsm(ioff),ione)
        ioff=kt_gs(k)*lat1*lon1+ione
        call strip(t  (1,1,k),xhatsm(ioff),ione)
     end do
     do k=1,nsig+ione
        ioff=kp_gs(k)*lat1*lon1+ione
        call strip(pri(1,1,k),xhatsm(ioff),ione)
     end do
  elseif (iflg==2_i_kind) then
     do k=1,nsig
        ioff=ku_gs(k)*lat1*lon1+ione
        call strip_periodic(st (1,1,k),xhatsm(ioff),ione)
        ioff=kv_gs(k)*lat1*lon1+ione
        call strip_periodic(vp (1,1,k),xhatsm(ioff),ione)
        ioff=kt_gs(k)*lat1*lon1+ione
        call strip_periodic(t  (1,1,k),xhatsm(ioff),ione)
     end do
     do k=1,nsig+ione
        ioff=kp_gs(k)*lat1*lon1+ione
        call strip_periodic(pri(1,1,k),xhatsm(ioff),ione)
     end do
  else
     write(6,*)'SUB2GRID:  ***ERROR*** iflg=',iflg,' is an illegal value'
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

