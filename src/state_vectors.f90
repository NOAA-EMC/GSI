module state_vectors
!$$$  module documentation block
!
! abstract: define state vectors and basic operators
!
! program history log:
!   2007-04-13  tremolet - initial code
!   2007-05-10  todling  - expanded interface to dot_product
!   2008-01-04  tremolet - improve allocate/deallocate
!   2008-04-28  guo      - add norms1 for more detailed info
!   2008-11-27  todling  - add tsen and p3d for Map-2008 update
!   2009-01-27  todling  - rename prt_norms to prevent IBM compiler confusion
!
!$$$

use kinds, only: r_kind,i_kind,r_quad
use constants, only: zero,zero_quad
use mpimod, only: mype
use mpl_allreducemod, only: mpl_allreduce,mpl_allgather

implicit none

save
private sum_mask,lat2,lon2,nsig,nval_len,latlon11,latlon1n
public state_vector, allocate_state, deallocate_state, &
     assignment(=), self_add, self_mul, prt_state_norms, setup_state_vectors, &
     dot_product, set_random, inquire_state, assign_scalar2state

! State vector definition
! Could contain model state fields plus other fields required
! by observation operators that can be saved from TL model run
! (from the physics or others)

type state_vector
  real(r_kind), pointer :: values(:) => NULL()

  real(r_kind), pointer :: u(:)   => NULL()
  real(r_kind), pointer :: v(:)   => NULL()
  real(r_kind), pointer :: t(:)   => NULL()
  real(r_kind), pointer :: tsen(:)=> NULL()
  real(r_kind), pointer :: q(:)   => NULL()
  real(r_kind), pointer :: oz(:)  => NULL()
  real(r_kind), pointer :: cw(:)  => NULL()
  real(r_kind), pointer :: p3d(:) => NULL()
  real(r_kind), pointer :: p(:)   => NULL()
  real(r_kind), pointer :: sst(:) => NULL()

  logical :: lallocated = .false.
end type state_vector

integer(i_kind) :: nval_len,latlon11,latlon1n,latlon1n1,lat2,lon2,nsig

integer(i_kind), parameter :: nvars=10
character(len=4) :: cvar(nvars)

logical :: llinit = .false.
integer(i_kind) :: m_st_alloc, max_st_alloc, m_allocs, m_deallocs

! ----------------------------------------------------------------------
INTERFACE ASSIGNMENT (=)
MODULE PROCEDURE assign_scalar2state, assign_state2state
END INTERFACE

INTERFACE PRT_STATE_NORMS
MODULE PROCEDURE prt_norms0,prt_norms1
END INTERFACE

INTERFACE SELF_ADD  ! What we really want here is ASSIGNMENT (+=)
MODULE PROCEDURE self_add_st, self_add_scal
END INTERFACE

INTERFACE DOT_PRODUCT
MODULE PROCEDURE dot_prod_st
END INTERFACE

INTERFACE SET_RANDOM
MODULE PROCEDURE set_random_st
END INTERFACE

INTERFACE SVSUM
MODULE PROCEDURE rsum,qsum
END INTERFACE
! ----------------------------------------------------------------------
contains
! ----------------------------------------------------------------------
subroutine setup_state_vectors(katlon11,katlon1n,kval_len,kat2,kon2,ksig)
  implicit none
  integer(i_kind), intent(in) :: katlon11,katlon1n,kval_len,kat2,kon2,ksig

  latlon11=katlon11
  latlon1n=katlon1n
  nval_len=kval_len
  lat2=kat2
  lon2=kon2
  nsig=ksig
  latlon1n1=latlon1n+latlon11

  llinit = .true.

  cvar( 1)='U   '
  cvar( 2)='V   '
  cvar( 3)='TV  '
  cvar( 4)='TSEN'
  cvar( 5)='Q   '
  cvar( 6)='OZ  '
  cvar( 7)='CW  '
  cvar( 8)='P   '
  cvar( 9)='PS  '
  cvar(10)='SST '

  m_st_alloc=0
  max_st_alloc=0
  m_allocs=0
  m_deallocs=0

  return
end subroutine setup_state_vectors
! ----------------------------------------------------------------------
subroutine allocate_state(yst)
  implicit none
  type(state_vector), intent(out) :: yst
  integer(i_kind) :: ii

  if (yst%lallocated) then
    write(6,*)'allocate_state: state already allocated'
    call stop2(312)
  end if

  yst%lallocated = .true.
  ALLOCATE(yst%values(nval_len))

  ii=0
  yst%u   => yst%values(ii+1:ii+latlon1n)
  ii=ii+latlon1n
  yst%v   => yst%values(ii+1:ii+latlon1n)
  ii=ii+latlon1n
  yst%t   => yst%values(ii+1:ii+latlon1n)
  ii=ii+latlon1n
  yst%tsen=> yst%values(ii+1:ii+latlon1n)
  ii=ii+latlon1n
  yst%q   => yst%values(ii+1:ii+latlon1n)
  ii=ii+latlon1n
  yst%oz  => yst%values(ii+1:ii+latlon1n)
  ii=ii+latlon1n
  yst%cw  => yst%values(ii+1:ii+latlon1n)
  ii=ii+latlon1n
  yst%p3d => yst%values(ii+1:ii+latlon1n1)
  ii=ii+latlon1n1
  yst%p   => yst%values(ii+1:ii+latlon11)
  ii=ii+latlon11
  yst%sst => yst%values(ii+1:ii+latlon11)
  ii=ii+latlon11

  if (ii/=nval_len) then
    write(6,*)'allocate_state: error length'
    call stop2(313)
  end if

  m_st_alloc=m_st_alloc+1
  if (m_st_alloc>max_st_alloc) max_st_alloc=m_st_alloc
  m_allocs=m_allocs+1

  return
end subroutine allocate_state
! ----------------------------------------------------------------------
subroutine deallocate_state(yst)
  implicit none
  type(state_vector), intent(inout) :: yst

  if (yst%lallocated) then
    NULLIFY(yst%u   )
    NULLIFY(yst%v   )
    NULLIFY(yst%t   )
    NULLIFY(yst%tsen)
    NULLIFY(yst%q   )
    NULLIFY(yst%oz  )
    NULLIFY(yst%cw  )
    NULLIFY(yst%p3d )
    NULLIFY(yst%p   )
    NULLIFY(yst%sst )
    DEALLOCATE(yst%values)
    yst%lallocated = .false.

    m_st_alloc=m_st_alloc-1
    m_deallocs=m_deallocs+1
  else
    write(6,*)'deallocate_state warning: vector not allocated'
  endif

  return
end subroutine deallocate_state
! ----------------------------------------------------------------------
subroutine assign_scalar2state(yst,pval)
  implicit none
  type(state_vector), intent(inout) :: yst
  real(r_kind), intent(in) :: pval
  integer(i_kind) :: ii

  DO ii=1,nval_len
    yst%values(ii)=pval
  ENDDO

  return
end subroutine assign_scalar2state
! ----------------------------------------------------------------------
subroutine assign_state2state(yst,xst)
  implicit none
  type(state_vector), intent(inout) :: yst
  type(state_vector), intent(in) :: xst
  integer(i_kind) :: ii

  DO ii=1,nval_len
    yst%values(ii)=xst%values(ii)
  ENDDO

  return
end subroutine assign_state2state
! ----------------------------------------------------------------------
subroutine self_add_st(yst,xst)
  implicit none
  type(state_vector), intent(inout) :: yst
  type(state_vector), intent(in) :: xst
  integer(i_kind) :: ii

  DO ii=1,nval_len
    yst%values(ii)=yst%values(ii)+xst%values(ii)
  ENDDO

  return
end subroutine self_add_st
! ----------------------------------------------------------------------
subroutine self_add_scal(yst,pa,xst)
  implicit none
  type(state_vector), intent(inout) :: yst
  real(r_kind),       intent(in) :: pa
  type(state_vector), intent(in) :: xst
  integer(i_kind) :: ii

  DO ii=1,nval_len
    yst%values(ii)=yst%values(ii)+pa*xst%values(ii)
  ENDDO

  return
end subroutine self_add_scal
! ----------------------------------------------------------------------
subroutine self_mul(yst,pa)
  implicit none
  type(state_vector), intent(inout) :: yst
  real(r_kind),       intent(in) :: pa
  integer(i_kind) :: ii

  DO ii=1,nval_len
    yst%values(ii)=pa*yst%values(ii)
  ENDDO

  return
end subroutine self_mul
! ----------------------------------------------------------------------
real(r_kind) function sum_mask(field,nlevs)
  implicit none
  integer(i_kind),intent(in):: nlevs
  real(r_kind),dimension(lat2,lon2,nlevs),intent(in) :: field

! local variables
  integer(i_kind) :: i,j,k
 
  sum_mask=zero
  do k=1,nlevs
    do j=2,lon2-1 
      do i=2,lat2-1
        sum_mask=sum_mask+field(i,j,k)
      end do
    end do
  end do
  return
end function sum_mask
subroutine norms_vars(xst,pmin,pmax,psum,pnum)
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,npe
  implicit none
  type(state_vector), intent(in) :: xst
  real(r_kind), intent(out) :: pmin(nvars),pmax(nvars),psum(nvars),pnum(nvars)

! local variables
  real(r_kind) :: zloc(3*nvars+3),zall(3*nvars+3,npe),zz
  integer(i_kind) :: ii

! Independent part of vector
  zloc(1) = sum_mask(xst%u,nsig)
  zloc(2) = sum_mask(xst%v,nsig)
  zloc(3) = sum_mask(xst%t,nsig)
  zloc(4) = sum_mask(xst%tsen,nsig)
  zloc(5) = sum_mask(xst%q,nsig)
  zloc(6) = sum_mask(xst%oz,nsig)
  zloc(7) = sum_mask(xst%cw,nsig)
  zloc(8) = sum_mask(xst%p3d,nsig+1)
  zloc(9) = sum_mask(xst%p,1)
  zloc(10)= sum_mask(xst%sst,1)
  zloc(nvars+1) = minval(xst%u(:))
  zloc(nvars+2) = minval(xst%v(:))
  zloc(nvars+3) = minval(xst%t(:))
  zloc(nvars+4) = minval(xst%tsen(:))
  zloc(nvars+5) = minval(xst%q(:))
  zloc(nvars+6) = minval(xst%oz(:))
  zloc(nvars+7) = minval(xst%cw(:))
  zloc(nvars+8) = minval(xst%p3d(:))
  zloc(nvars+9) = minval(xst%p(:))
  zloc(nvars+10)= minval(xst%sst(:))
  zloc(2*nvars+1) = maxval(xst%u(:))
  zloc(2*nvars+2) = maxval(xst%v(:))
  zloc(2*nvars+3) = maxval(xst%t(:))
  zloc(2*nvars+4) = maxval(xst%tsen(:))
  zloc(2*nvars+5) = maxval(xst%q(:))
  zloc(2*nvars+6) = maxval(xst%oz(:))
  zloc(2*nvars+7) = maxval(xst%cw(:))
  zloc(2*nvars+8) = maxval(xst%p3d(:))
  zloc(2*nvars+9) = maxval(xst%p(:))
  zloc(2*nvars+10)= maxval(xst%sst(:))
  zloc(3*nvars+1) = real((lat2-2)*(lon2-2)*nsig, r_kind)
  zloc(3*nvars+2) = real((lat2-2)*(lon2-2)*(nsig+1),r_kind)
  zloc(3*nvars+3) = real((lat2-2)*(lon2-2), r_kind)

! Gather contributions
  call mpi_allgather(zloc,3*nvars+3,mpi_rtype, &
                   & zall,3*nvars+3,mpi_rtype, mpi_comm_world,ierror)
! call mpl_allgather (3*nvars+3,npe,zloc,zall)

  zz=SUM(zall(3*nvars+1,:))
  do ii=1,7
    psum(ii)=SUM(zall(ii,:))
    pnum(ii)=zz
  enddo
  zz=SUM(zall(3*nvars+2,:))
  do ii=8,8
    psum(ii)=SUM(zall(ii,:))
    pnum(ii)=zz
  enddo
  zz=SUM(zall(3*nvars+3,:))
  do ii=9,10
    psum(ii)=SUM(zall(ii,:))
    pnum(ii)=zz
  enddo
  do ii=1,nvars
    pmin(ii)=MINVAL(zall(  nvars+ii,:))
    pmax(ii)=MAXVAL(zall(2*nvars+ii,:))
  enddo

return
end subroutine norms_vars
! ----------------------------------------------------------------------
subroutine prt_norms1(xst,sgrep)
  implicit none
  type(state_vector),dimension(:),intent(in) :: xst
  character(len=256), intent(in) :: sgrep

  character(len=8) :: bindx,bform
  character(len=len(sgrep)+len(bindx)+2) :: bgrep
  
  integer :: nx,ix

  nx=size(xst)
  ix=1;
  if(nx>9) ix=2
  if(nx>99) ix=3
  if(nx>999) ix=4
  if(nx>9999) ix=0
  write(bform,'(a,i1,a,i1,a)') '(i',ix,'.',min(ix,2),')'

  do ix=1,nx
    write(bindx,bform) ix
    bindx=adjustl(bindx)
    write(bgrep,'(4a)') trim(sgrep),'(',trim(bindx),')'
    call prt_norms0(xst(ix),trim(bgrep))
  end do
end subroutine prt_norms1
! ----------------------------------------------------------------------
subroutine prt_norms0(xst,sgrep)
  implicit none
  type(state_vector), intent(in) :: xst
  character(len=*), intent(in) :: sgrep

  real(r_kind) :: zmin(nvars),zmax(nvars),zsum(nvars),znum(nvars)
  real(r_kind) :: zavg
  integer(i_kind) :: ii

  call norms_vars(xst,zmin,zmax,zsum,znum)

  if (mype==0) then
    do ii=1,nvars
      zavg=zsum(ii)/znum(ii)
      write(6,999)sgrep,cvar(ii),zavg,zmin(ii),zmax(ii)
    enddo
  endif
999 format(A,1X,A,3(1X,ES20.12))

  return
end subroutine prt_norms0
! ----------------------------------------------------------------------
real(r_quad) function dplevs(nlevs,dx,dy)
  implicit none
  integer(i_kind),intent(in) :: nlevs
  real(r_kind),intent(in)::dx(lat2,lon2,nlevs),dy(lat2,lon2,nlevs)

  integer(i_kind) :: ii,jj,kk

  dplevs=zero_quad
  do kk=1,nlevs
     do jj=2,lon2-1
        do ii=2,lat2-1
           dplevs=dplevs+dx(ii,jj,kk)*dy(ii,jj,kk)
        end do
     end do
  end do

return
end function dplevs
! ----------------------------------------------------------------------
real(r_quad) function dot_prod_st(xst,yst,which)
  implicit none
  type(state_vector), intent(in) :: xst, yst
  character(len=*),   intent(in), optional :: which  ! in the form: "var1+var2+..."

  real(r_quad) :: zz(nvars)
  integer(i_kind) :: ii

  zz=zero_quad
  if (.not.present(which)) then

     zz(1) = dplevs(nsig,xst%u  ,yst%u)
     zz(2) = dplevs(nsig,xst%v  ,yst%v)
     zz(3) = dplevs(nsig,xst%t  ,yst%t)
     zz(4) = dplevs(nsig,xst%tsen,yst%tsen)
     zz(5) = dplevs(nsig,xst%q  ,yst%q)
     zz(6) = dplevs(nsig,xst%oz ,yst%oz)
     zz(7) = dplevs(nsig,xst%cw ,yst%cw)
     zz(8) = dplevs(nsig,xst%p3d,yst%p3d)
     zz(9) = dplevs(1   ,xst%p  ,yst%p)
     zz(10)= dplevs(1   ,xst%sst,yst%sst)

  else

     if(index(which,'u+'   )/=0) zz(1) = dplevs(nsig,xst%u   ,yst%u)
     if(index(which,'v+'   )/=0) zz(2) = dplevs(nsig,xst%v   ,yst%v)
     if(index(which,'tv+'  )/=0) zz(3) = dplevs(nsig,xst%t   ,yst%t)
     if(index(which,'tsen+')/=0) zz(4) = dplevs(nsig,xst%tsen,yst%tsen)
     if(index(which,'q+'   )/=0) zz(5) = dplevs(nsig,xst%q   ,yst%q)
     if(index(which,'oz+'  )/=0) zz(6) = dplevs(nsig,xst%oz  ,yst%oz)
     if(index(which,'cw+'  )/=0) zz(7) = dplevs(nsig,xst%cw  ,yst%cw)
     if(index(which,'p3d+' )/=0) zz(8) = dplevs(nsig,xst%p3d ,yst%p3d)
     if(index(which,'p+'   )/=0) zz(9) = dplevs(1   ,xst%p   ,yst%p)
     if(index(which,'sst+' )/=0) zz(10)= dplevs(1   ,xst%sst ,yst%sst)

  endif

  call mpl_allreduce(nvars,zz)

  dot_prod_st=zero_quad
  do ii=1,nvars
    dot_prod_st=dot_prod_st+zz(ii)
  enddo

return
end function dot_prod_st
! ----------------------------------------------------------------------
subroutine set_random_st ( xst )
  implicit none
  type(state_vector), intent(inout) :: xst
  call random ( xst%u   )
  call random ( xst%v   )
  call random ( xst%t   )
  call random ( xst%tsen )
  call random ( xst%q   )
  call random ( xst%oz  )
  call random ( xst%cw  )
  call random ( xst%p3d  )
  call random ( xst%p   )
  call random ( xst%sst )
return
end subroutine set_random_st
! ----------------------------------------------------------------------
subroutine inquire_state
real(r_kind) :: zz

if (mype==0) then
  write(6,*)'state_vectors: latlon11,latlon1n,latlon1n1,lat2,lon2,nsig=', &
                            latlon11,latlon1n,latlon1n1,lat2,lon2,nsig
  zz=real(max_st_alloc*nval_len,r_kind)*8.0_r_kind/1.048e6_r_kind
  write(6,*)'state_vectors: length=',nval_len
  write(6,*)'state_vectors: currently allocated=',m_st_alloc
  write(6,*)'state_vectors: maximum allocated=',max_st_alloc
  write(6,*)'state_vectors: number of allocates=',m_allocs
  write(6,*)'state_vectors: number of deallocates=',m_deallocs
  write(6,'(A,F8.1,A)')'state_vectors: Estimated max memory used= ',zz,' Mb'
endif

end subroutine inquire_state
! ----------------------------------------------------------------------
real(r_quad) function rsum(x)
  implicit none
  real(r_kind), intent(in) :: x(:)
  integer(i_kind) :: idim,i
  idim=size(x)
  rsum=zero_quad
!$omp parallel do
  do i=1,idim
     rsum=rsum + x(i)
  enddo
!$omp end parallel do
end function rsum
! ----------------------------------------------------------------------
real(r_quad) function qsum(x)
  implicit none
  real(r_quad), intent(in) :: x(:)
  integer(i_kind) :: idim,i
  idim=size(x)
  qsum=zero_quad
!$omp parallel do
  do i=1,idim
     qsum=qsum + x(i)
  enddo
!$omp end parallel do
end function qsum
! ----------------------------------------------------------------------
end module state_vectors
