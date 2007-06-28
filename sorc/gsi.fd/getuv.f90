subroutine getuv(u,v,st,vp)
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
!
!   input argument list:
!     st        - stream function grid values 
!     vp        - velocity potential grid values 
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
  use gridmod, only: regional,lat2,nsig,iglobal,lon1,itotsub,lon2,lat1
  use mpimod, only: iscuv_s,ierror,mpi_comm_world,irduv_s,ircuv_s,&
       isduv_s,isduv_g,iscuv_g,nuvlevs,irduv_g,ircuv_g,mpi_rtype,&
       strip,reorder,reorder2
  use compact_diffs, only: stvp2uv
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: st,vp

  real(r_kind),dimension(lat2,lon2,nsig),intent(out):: u,v

! Declare local variables
  integer(i_kind) i,j,k,isize

  real(r_kind),dimension(lat1,lon1,nsig):: usm,vsm
  real(r_kind),dimension(itotsub,nuvlevs):: work3,work4

! Initialize variables
  isize=max(iglobal,itotsub)

! zero out work arrays
  do k=1,nuvlevs
    do j=1,isize
      work3(j,k)=zero
      work4(j,k)=zero
    end do
  end do

!   begin strmfctn / vp to u/v section
!   strip off endpoints of st/vp arrays on subdomains
!   note that right now, place in usm,vsm
    call strip(st,usm,nsig)
    call strip(vp,vsm,nsig)

!   put st/vp on global slabs
    call mpi_alltoallv(usm(1,1,1),iscuv_g,isduv_g,&
         mpi_rtype,work3(1,1),ircuv_g,irduv_g,mpi_rtype,&
         mpi_comm_world,ierror)
    call mpi_alltoallv(vsm(1,1,1),iscuv_g,isduv_g,&
         mpi_rtype,work4(1,1),ircuv_g,irduv_g,mpi_rtype,&
         mpi_comm_world,ierror)

!   reorder work arrays before calling of 
!   st,vp --> u,v routine
    call reorder(work3,nuvlevs)
    call reorder(work4,nuvlevs)

!   call st,vp --> u,v routine 
    do k=1,nuvlevs
      if(regional) then
        call stvp2uv_reg(work3(1,k),work4(1,k))
      else
        call stvp2uv(work3(1,k),work4(1,k))
      end if
    end do

!   reorder the work array for the mpi communication
    call reorder2(work3,nuvlevs)
    call reorder2(work4,nuvlevs)

!   get u,v back on subdomains
    call mpi_alltoallv(work3(1,1),iscuv_s,isduv_s,&
         mpi_rtype,u(1,1,1),ircuv_s,irduv_s,mpi_rtype,&
         mpi_comm_world,ierror)
    call mpi_alltoallv(work4(1,1),iscuv_s,isduv_s,&
         mpi_rtype,v(1,1,1),ircuv_s,irduv_s,mpi_rtype,&
         mpi_comm_world,ierror)
    
  return
end subroutine getuv
