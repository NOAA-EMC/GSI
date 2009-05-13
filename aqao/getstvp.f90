subroutine getstvp(u,v,st,vp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    getstvp
!   prgmmr: kleist           org: np22                date: 2004-10-16
!
! abstract: performs setup and calls routine which is adjoint of
!           conversion from streamfunction and velocity potential to 
!           u,v
!
! program history log:
!   2004-10-16  kleist - initial routine
!   2005-01-22  parrish - add "use compact_diffs"
!   2008-10-10  derber  - correct error when used as adjoint
!
!   input argument list:
!     u        - u grid values from int routines 
!     v        - v grid values from int routines 
!
!   output argument list:
!     st       - int st grid values
!     vp       - int vp grid values
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,one
  use gridmod, only: itotsub,lon1,lat1,regional,iglobal,lon2,lat2,nsig
  use mpimod, only: iscuv_s,ierror,mpi_comm_world,irduv_s,ircuv_s,&
       isduv_s,isduv_g,iscuv_g,nnnuvlevs,nuvlevs,irduv_g,ircuv_g,mpi_rtype,&
       strip,reorder,reorder2
  use compact_diffs, only: tstvp2uv
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in):: u,v
  real(r_kind),dimension(lat2,lon2,nsig),intent(out):: st,vp  

! Declare local variables
  integer(i_kind) i,j,k,igrid,isize

  real(r_kind),dimension(lat1,lon1,nsig):: usm,vsm
  real(r_kind),dimension(itotsub,nuvlevs):: work3,work4  ! for st,vp <--> u,v
  real(r_kind),dimension(lat2,lon2,nsig):: st1,vp1  

! Initialize variables
  igrid=lat2*lon2
  isize=max(iglobal,itotsub)

  do k=1,nuvlevs
    do j=1,isize
      work3(j,k)=zero
      work4(j,k)=zero
    end do
  end do

! strip off end points of u,v on subdomains
  call strip(u,usm,nsig)
  call strip(v,vsm,nsig)

!   begin adjoint of st/vp to u/v section

!   put u/v on global slabs
    call mpi_alltoallv(usm(1,1,1),iscuv_g,isduv_g,&
         mpi_rtype,work3(1,1),ircuv_g,irduv_g,mpi_rtype,&
         mpi_comm_world,ierror)
    call mpi_alltoallv(vsm(1,1,1),iscuv_g,isduv_g,&
         mpi_rtype,work4(1,1),ircuv_g,irduv_g,mpi_rtype,&
         mpi_comm_world,ierror)

!   reorder work arrays before calling adjoint of 
!   st,vp --> u,v routine
    call reorder(work3,nuvlevs,nnnuvlevs)
    call reorder(work4,nuvlevs,nnnuvlevs)

!   call transpose of st,vp --> u,v routine
!$omp parallel do private(k)
    do k=1,nnnuvlevs
      if(regional) then
        call tstvp2uv_reg(work3(1,k),work4(1,k))
      else
        call tstvp2uv(work3(1,k),work4(1,k))
      end if
    end do
!$omp end parallel do

!   reorder work3/work4 arrays for mpi communication
    call reorder2(work3,nuvlevs,nnnuvlevs)
    call reorder2(work4,nuvlevs,nnnuvlevs)

!   get st/vp back on subdomains
    call mpi_alltoallv(work3(1,1),iscuv_s,isduv_s,&
         mpi_rtype,st1(1,1,1),ircuv_s,irduv_s,mpi_rtype,&
         mpi_comm_world,ierror)
    call mpi_alltoallv(work4(1,1),iscuv_s,isduv_s,&
         mpi_rtype,vp1(1,1,1),ircuv_s,irduv_s,mpi_rtype,&
         mpi_comm_world,ierror)

    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
          st(i,j,k)=st(i,j,k)+st1(i,j,k)
          vp(i,j,k)=vp(i,j,k)+vp1(i,j,k)
        end do
      end do
    end do
  return
end subroutine getstvp
