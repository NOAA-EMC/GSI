subroutine getuv(u,v,st,vp,iflg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    getuv     
!
!   prgmmr: kleist           org: np22                date: 2004-10-15
!
! abstract: performs setup and calls routine to get conversion from 
!           streamfunction and velocity potential to u,v
!
! program history log:
!   2004-10-15  kleist - initial routine
!   2005-01-22  parrish - add "use compact_diffs"
!   2008-06-04  safford - rm unused var i
!   2009-04-21  derber - rewrite for improved communication - combine with adjoint
!   2010-04-01  treadon - move strip,reorder,reorder2,vectosub to gridmod
!
!   input argument list:
!     st        - stream function grid values 
!     vp        - velocity potential grid values 
!     iflg      = 0 forward model
!               = 1 adjoint model
!
!   output argument list:
!     u         - u grid values 
!     v         - v grid values 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: regional,lat2,nsig,iglobal,lon1,itotsub,lon2,lat1, &
        nlat,nlon,latlon11,ltosj_s,ltosi_s,ltosi,ltosj,&
        strip,reorder,reorder2,vectosub
  use mpimod, only: iscvec_s,ierror,mpi_comm_world,irdvec_s,ircvec_s,&
       isdvec_s,isdvec_g,iscvec_g,nnnvsuv,nlevsuv,mpi_rtype,&
       lu_gs,lv_gs,ircvec_g,irdvec_g
  use compact_diffs, only: stvp2uv,tstvp2uv
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: st,vp
  integer(i_kind)                       ,intent(in   ) :: iflg

  real(r_kind),dimension(lat2,lon2,nsig),intent(inout):: u,v

! Declare local variables
  integer(i_kind) i,j,k,l,ioff,ni1,ni2

  real(r_kind),dimension(lat2*lon2*2*nsig):: uvsm
  real(r_kind),dimension(itotsub,nlevsuv):: work1
  real(r_kind),dimension(nlat,nlon,nlevsuv)::workin
  real(r_kind),dimension(nlat,nlon)::stx,vpx
  real(r_kind),dimension(lat2,lon2)::ux,vx


  if(iflg == 0)then
!$omp parallel do  schedule(dynamic,1) private(k,ioff)
     do k=1,nsig
        ioff=lu_gs(k)*lat1*lon1+1
        call strip(st(1,1,k),uvsm(ioff),1)
        ioff=lv_gs(k)*lat1*lon1+1
        call strip(vp(1,1,k),uvsm(ioff),1)
     end do
  else
!$omp parallel do  schedule(dynamic,1) private(k,ioff)
     do k=1,nsig
        ioff=lu_gs(k)*lat1*lon1+1
        call strip(u(1,1,k),uvsm(ioff),1)
        ioff=lv_gs(k)*lat1*lon1+1
        call strip(v(1,1,k),uvsm(ioff),1)
     end do
  end if
   

! zero out work arrays
! do k=1,nlevsuv
!    do j=1,itotsub
!       work1(j,k)=zero
!    end do
! end do

!  subdomain vector to global slabs
  call mpi_alltoallv(uvsm(1),iscvec_g,isdvec_g,&
       mpi_rtype,work1(1,1),ircvec_g,irdvec_g,mpi_rtype,&
       mpi_comm_world,ierror)

! reorder work1 array post communication
  call reorder(work1,nlevsuv,nnnvsuv)
  if(regional)then
     do k=1,nnnvsuv,2
        do l=1,iglobal
           ni1=ltosi(l); ni2=ltosj(l)
           stx(ni1,ni2)=work1(l,k)
           vpx(ni1,ni2)=work1(l,k+1)
        end do
        if(iflg == 0)then
           call psichi2uv_reg(stx,vpx,workin(1,1,k),workin(1,1,k+1))
        else
           call psichi2uvt_reg(stx,vpx,workin(1,1,k),workin(1,1,k+1))
        end if
     end do
  else
     do k=1,nnnvsuv,2
        do l=1,iglobal
           ni1=ltosi(l); ni2=ltosj(l)
           workin(ni1,ni2,k)=work1(l,k)
           workin(ni1,ni2,k+1)=work1(l,k+1)
        end do
        if(iflg == 0)then
           call stvp2uv(workin(1,1,k),workin(1,1,k+1))
        else
           call tstvp2uv(workin(1,1,k),workin(1,1,k+1))
        end if
     end do
  end if

! Transfer input array to local work array
!$omp parallel do  schedule(dynamic,1) private(k,l,ni1,ni2)
  do k=1,nnnvsuv
     do l=1,itotsub
        ni1=ltosi_s(l); ni2=ltosj_s(l)
        work1(l,k)=workin(ni1,ni2,k)
     end do
  end do

! reorder work1 global slab array for communications
  call reorder2(work1,nlevsuv,nnnvsuv)

! send global slabs to subdomains
  call mpi_alltoallv(work1(1,1),iscvec_s,isdvec_s,&
       mpi_rtype,uvsm(1),ircvec_s,irdvec_s,&
       mpi_rtype,mpi_comm_world,ierror)

  if(iflg == 0)then
!$omp parallel do  schedule(dynamic,1) private(k,ioff)
     do k=1,nsig
        ioff=lu_gs(k)*latlon11+1
        call vectosub(uvsm(ioff),latlon11,u(1,1,k))
        ioff=lv_gs(k)*latlon11+1
        call vectosub(uvsm(ioff),latlon11,v(1,1,k))
     end do
  else
!$omp parallel do  schedule(dynamic,1) private(k,ioff,j,i,ux,vx)
     do k=1,nsig
        ioff=lu_gs(k)*latlon11+1
        call vectosub(uvsm(ioff),latlon11,ux)
        ioff=lv_gs(k)*latlon11+1
        call vectosub(uvsm(ioff),latlon11,vx)
        do j=1,lon2
           do i=1,lat2
              st(i,j,k)=st(i,j,k)+ux(i,j)
              vp(i,j,k)=vp(i,j,k)+vx(i,j)
           end do
        end do
     end do
  end if

  return
end subroutine getuv
