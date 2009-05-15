module control_vectors
!$$$  module documentation block
!
! abstract: define control vectors and basic operators
!
! program history log:
!   2007-04-13  tremolet - initial code
!   2007-08-03  todling  - using get_lun for file unit definition
!   2008-01-04  tremolet - improve allocate/deallocate
!   2008-12-29  todling  - add omp to various loops
!   2009-01-27  todling  - rename prt_norms to prevent IBM compiler confusion
!
!$$$

use kinds, only: r_kind,i_kind,r_quad
use mpimod, only: mpi_comm_world,mpi_max,mpi_rtype,mype,npe,ierror
use constants, only: zero, one, two, zero_quad
use gsi_4dvar, only: iadatebgn
use file_utility, only : get_lun
use mpl_allreducemod, only: mpl_allreduce

implicit none
save
private
public control_vector, allocate_cv, deallocate_cv, assignment(=), &
     & dot_product, prt_control_norms, axpy, random_cv, setup_control_vectors, &
     & write_cv, read_cv, inquire_cv, maxval, qdot_prod_sub

type control_state
  real(r_kind), pointer :: values(:) => NULL()
  real(r_kind), pointer :: st(:)  => NULL()
  real(r_kind), pointer :: vp(:)  => NULL()
  real(r_kind), pointer :: t(:)   => NULL()
  real(r_kind), pointer :: rh(:)  => NULL()
  real(r_kind), pointer :: oz(:)  => NULL()
  real(r_kind), pointer :: cw(:)  => NULL()
  real(r_kind), pointer :: p(:)   => NULL()
  real(r_kind), pointer :: sst(:) => NULL()
end type control_state

type control_vector
  integer(i_kind) :: lencv
  real(r_kind), pointer :: values(:) => NULL()
  type(control_state), allocatable :: step(:)
  real(r_kind), pointer :: predr(:) => NULL()
  real(r_kind), pointer :: predp(:) => NULL()
  logical :: lallocated = .false.
end type control_vector

integer(i_kind) :: nclen,nclen1,nsclen,npclen,nrclen,nsubwin,nval_len
integer(i_kind) :: latlon11,latlon1n,lat2,lon2,nsig
logical :: lsqrtb

integer(i_kind) :: m_vec_alloc, max_vec_alloc, m_allocs, m_deallocs

logical :: llinit = .false.

! ----------------------------------------------------------------------
INTERFACE ASSIGNMENT (=)
MODULE PROCEDURE assign_scalar2cv, assign_array2cv, assign_cv2array, assign_cv2cv
END INTERFACE

INTERFACE DOT_PRODUCT
MODULE PROCEDURE dot_prod_cv,qdot_prod_cv
END INTERFACE

INTERFACE DOT_PROD_VARS
MODULE PROCEDURE ddot_prod_vars,qdot_prod_vars
END INTERFACE

INTERFACE MAXVAL
MODULE PROCEDURE maxval_cv
END INTERFACE

INTERFACE PRT_CONTROL_NORMS
MODULE PROCEDURE prt_norms
END INTERFACE
! ----------------------------------------------------------------------
contains
! ----------------------------------------------------------------------
subroutine setup_control_vectors(ksig,klat,klon,katlon11,katlon1n, &
                               & ksclen,kpclen,kclen,ksubwin,kval_len,ldsqrtb)
  implicit none
  integer(i_kind), intent(in) :: ksig,klat,klon,katlon11,katlon1n, &
                               & ksclen,kpclen,kclen,ksubwin,kval_len
  logical, intent(in) :: ldsqrtb

  nsig=ksig
  lat2=klat
  lon2=klon
  latlon11=katlon11
  latlon1n=katlon1n
  nsclen=ksclen
  npclen=kpclen
  nrclen=nsclen+npclen
  nclen =kclen
  nclen1=nclen-nrclen
  nsubwin=ksubwin
  nval_len=kval_len
  lsqrtb=ldsqrtb

  llinit = .true.
  m_vec_alloc=0
  max_vec_alloc=0
  m_allocs=0
  m_deallocs=0

  call inquire_cv

  return
end subroutine setup_control_vectors
! ----------------------------------------------------------------------
subroutine allocate_cv(ycv)
  implicit none
  type(control_vector), intent(out) :: ycv
  integer(i_kind) :: ii,jj

  if (ycv%lallocated) then
    write(6,*)'allocate_cv: vector already allocated'
    call stop2(108)
  end if

  ycv%lallocated=.true.
  ycv%lencv = nclen
  ALLOCATE(ycv%values(ycv%lencv))
  ALLOCATE(ycv%step(nsubwin))

  ii=0
  do jj=1,nsubwin
    ycv%step(jj)%values => ycv%values(ii+1:ii+nval_len)

    if (lsqrtb) then
      ycv%step(jj)%st  => NULL()
      ycv%step(jj)%vp  => NULL()
      ycv%step(jj)%t   => NULL()
      ycv%step(jj)%rh  => NULL()
      ycv%step(jj)%oz  => NULL()
      ycv%step(jj)%cw  => NULL()
      ycv%step(jj)%p   => NULL()
      ycv%step(jj)%sst => NULL()
      ii=ii+nval_len
    else
      ycv%step(jj)%st  => ycv%values(ii+1:ii+latlon1n)
      ii=ii+latlon1n
      ycv%step(jj)%vp  => ycv%values(ii+1:ii+latlon1n)
      ii=ii+latlon1n
      ycv%step(jj)%t   => ycv%values(ii+1:ii+latlon1n)
      ii=ii+latlon1n
      ycv%step(jj)%rh  => ycv%values(ii+1:ii+latlon1n)
      ii=ii+latlon1n
      ycv%step(jj)%oz  => ycv%values(ii+1:ii+latlon1n)
      ii=ii+latlon1n
      ycv%step(jj)%cw  => ycv%values(ii+1:ii+latlon1n)
      ii=ii+latlon1n
      ycv%step(jj)%p   => ycv%values(ii+1:ii+latlon11)
      ii=ii+latlon11
      ycv%step(jj)%sst => ycv%values(ii+1:ii+latlon11)
      ii=ii+latlon11
    endif
  enddo

  ycv%predr => ycv%values(ii+1:ii+nsclen)
  ii=ii+nsclen
  ycv%predp => ycv%values(ii+1:ii+npclen)
  ii=ii+npclen

  if (ii/=nclen) then
    write(6,*)'allocate_mods: error length',ii,nclen
    call stop2(109)
  end if

  m_allocs=m_allocs+1
  m_vec_alloc=m_vec_alloc+1
  max_vec_alloc=MAX(max_vec_alloc,m_vec_alloc)

  return
end subroutine allocate_cv
! ----------------------------------------------------------------------
subroutine deallocate_cv(ycv)
  implicit none
  type(control_vector), intent(inout) :: ycv
  integer(i_kind) :: ii

  if (ycv%lallocated) then
    do ii=1,nsubwin
      NULLIFY(ycv%step(ii)%st )
      NULLIFY(ycv%step(ii)%vp )
      NULLIFY(ycv%step(ii)%t  )
      NULLIFY(ycv%step(ii)%rh )
      NULLIFY(ycv%step(ii)%oz )
      NULLIFY(ycv%step(ii)%cw )
      NULLIFY(ycv%step(ii)%p  )
      NULLIFY(ycv%step(ii)%sst)
    end do
    NULLIFY(ycv%predr)
    NULLIFY(ycv%predp)

    DEALLOCATE(ycv%step)
    DEALLOCATE(ycv%values)

    ycv%lallocated=.false.

    m_deallocs=m_deallocs+1
    m_vec_alloc=m_vec_alloc-1
  else
    if (mype==0) write(6,*)'deallocate_cv warning: vector not allocated'
  endif

  return
end subroutine deallocate_cv
! ----------------------------------------------------------------------
subroutine assign_scalar2cv(ycv,pval)
  implicit none
  type(control_vector), intent(inout) :: ycv
  real(r_kind), intent(in) :: pval
  integer(i_kind) :: ii

  DO ii=1,ycv%lencv
    ycv%values(ii)=pval
  ENDDO

  return
end subroutine assign_scalar2cv
! ----------------------------------------------------------------------
subroutine assign_cv2cv(ycv,xcv)
  implicit none
  type(control_vector), intent(inout) :: ycv
  type(control_vector), intent(in) :: xcv
  integer(i_kind) :: ii

  if (xcv%lencv/=ycv%lencv) then
      write(6,*)'assign_cv2cv: error length',xcv%lencv,ycv%lencv
      call stop2(110)
  end if

!$omp parallel do
  DO ii=1,ycv%lencv
    ycv%values(ii)=xcv%values(ii)
  ENDDO
!$omp end parallel do

  return
end subroutine assign_cv2cv
! ----------------------------------------------------------------------
subroutine assign_array2cv(ycv,parray)
  implicit none
  type(control_vector), intent(inout) :: ycv
  real(r_kind), intent(in) :: parray(:)
  integer(i_kind) :: ii

  if (size(parray)/=ycv%lencv) then
      write(6,*)'assign_array2cv: array wrong length',size(parray),ycv%lencv
      call stop2(111)
  end if


!$omp parallel do
  DO ii=1,ycv%lencv
    ycv%values(ii)=parray(ii)
  ENDDO
!$omp end parallel do

  return
end subroutine assign_array2cv
! ----------------------------------------------------------------------
subroutine assign_cv2array(parray,ycv)
  implicit none
  real(r_kind), intent(out) :: parray(:)
  type(control_vector), intent(in) :: ycv
  integer(i_kind) :: ii

  if (size(parray)/=ycv%lencv) then
      write(6,*)'assign_cv2array: array wrong length',size(parray),ycv%lencv
      call stop2(112)
  end if

!$omp parallel do
  DO ii=1,ycv%lencv
    parray(ii)=ycv%values(ii)
  ENDDO
!$omp end parallel do

  return
end subroutine assign_cv2array
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
subroutine ddot_prod_vars(xcv,ycv,prods)
  implicit none
  type(control_vector), intent(in) :: xcv, ycv
  real(r_kind), intent(out) :: prods(nsubwin+1)

  real(r_kind) :: zz(nsubwin)
  integer(i_kind) :: ii

  prods(:)=zero
  zz(:)=zero

! Independent part of vector
  if (lsqrtb) then
!$omp parallel do
    do ii=1,nsubwin
      zz(ii)=dot_product( xcv%step(ii)%values(:) ,ycv%step(ii)%values(:) )
    end do
!$omp end parallel do
  else
    do ii=1,nsubwin
      zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%st(:) ,ycv%step(ii)%st(:))
      zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%vp(:) ,ycv%step(ii)%vp(:))
      zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%t(:)  ,ycv%step(ii)%t(:))
      zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%rh(:) ,ycv%step(ii)%rh(:))
      zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%oz(:) ,ycv%step(ii)%oz(:))
      zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%cw(:) ,ycv%step(ii)%cw(:))
      zz(ii) = zz(ii) + dplevs(1   ,xcv%step(ii)%p(:)  ,ycv%step(ii)%p(:))
      zz(ii) = zz(ii) + dplevs(1   ,xcv%step(ii)%sst(:),ycv%step(ii)%sst(:))
    end do
  end if

  call mpl_allreduce(nsubwin,zz)
  prods(1:nsubwin) = zz(1:nsubwin)

! Duplicated part of vector
  if (nsclen>0) then
    prods(nsubwin+1) = prods(nsubwin+1) + dot_product(xcv%predr(:),ycv%predr(:))
  endif
  if (npclen>0) then
    prods(nsubwin+1) = prods(nsubwin+1) + dot_product(xcv%predp(:),ycv%predp(:))
  endif

return
end subroutine ddot_prod_vars
! ----------------------------------------------------------------------
real(r_quad) function qdot_prod_sub(xcv,ycv)
  implicit none
  type(control_vector), intent(in) :: xcv, ycv
  integer(i_kind) :: ii,j


  qdot_prod_sub=zero_quad

! Independent part of vector
  if (lsqrtb) then
!$omp parallel do
    do ii=1,nsubwin
      qdot_prod_sub=qdot_prod_sub+qdot_product( xcv%step(ii)%values(:) ,ycv%step(ii)%values(:) )
    end do
!$omp end parallel do
  else
    do ii=1,nsubwin
      qdot_prod_sub = qdot_prod_sub + dplevs(nsig,xcv%step(ii)%st(:) ,ycv%step(ii)%st(:))
      qdot_prod_sub = qdot_prod_sub + dplevs(nsig,xcv%step(ii)%vp(:) ,ycv%step(ii)%vp(:))
      qdot_prod_sub = qdot_prod_sub + dplevs(nsig,xcv%step(ii)%t(:)  ,ycv%step(ii)%t(:))
      qdot_prod_sub = qdot_prod_sub + dplevs(nsig,xcv%step(ii)%rh(:) ,ycv%step(ii)%rh(:))
      qdot_prod_sub = qdot_prod_sub + dplevs(nsig,xcv%step(ii)%oz(:) ,ycv%step(ii)%oz(:))
      qdot_prod_sub = qdot_prod_sub + dplevs(nsig,xcv%step(ii)%cw(:) ,ycv%step(ii)%cw(:))
      qdot_prod_sub = qdot_prod_sub + dplevs(1   ,xcv%step(ii)%p(:)  ,ycv%step(ii)%p(:))
      qdot_prod_sub = qdot_prod_sub + dplevs(1   ,xcv%step(ii)%sst(:),ycv%step(ii)%sst(:))
    end do
  end if

! Duplicated part of vector
  if(mype == 0)then
    do j=nclen1+1,nclen
      qdot_prod_sub=qdot_prod_sub+xcv%values(j)*ycv%values(j) 
    end do
  end if

return
end function qdot_prod_sub
! ----------------------------------------------------------------------
subroutine qdot_prod_vars(xcv,ycv,prods)
  implicit none
  type(control_vector), intent(in) :: xcv, ycv
  real(r_quad), intent(out) :: prods(nsubwin+1)

  real(r_quad) :: zz(nsubwin)
  integer(i_kind) :: ii

  prods(:)=zero_quad
  zz(:)=zero_quad

! Independent part of vector
  if (lsqrtb) then
!$omp parallel do
    do ii=1,nsubwin
      zz(ii)=qdot_product( xcv%step(ii)%values(:) ,ycv%step(ii)%values(:) )
    end do
!$omp end parallel do
  else
    do ii=1,nsubwin
      zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%st(:) ,ycv%step(ii)%st(:))
      zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%vp(:) ,ycv%step(ii)%vp(:))
      zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%t(:)  ,ycv%step(ii)%t(:))
      zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%rh(:) ,ycv%step(ii)%rh(:))
      zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%oz(:) ,ycv%step(ii)%oz(:))
      zz(ii) = zz(ii) + dplevs(nsig,xcv%step(ii)%cw(:) ,ycv%step(ii)%cw(:))
      zz(ii) = zz(ii) + dplevs(1   ,xcv%step(ii)%p(:)  ,ycv%step(ii)%p(:))
      zz(ii) = zz(ii) + dplevs(1   ,xcv%step(ii)%sst(:),ycv%step(ii)%sst(:))
    end do
  end if

  call mpl_allreduce(nsubwin,zz)
  prods(1:nsubwin) = zz(1:nsubwin)

! Duplicated part of vector
  if (nsclen>0) then
    prods(nsubwin+1) = prods(nsubwin+1) + qdot_product(xcv%predr(:),ycv%predr(:))
  endif
  if (npclen>0) then
    prods(nsubwin+1) = prods(nsubwin+1) + qdot_product(xcv%predp(:),ycv%predp(:))
  endif

return
end subroutine qdot_prod_vars
! ----------------------------------------------------------------------
real(r_kind) function dot_prod_cv(xcv,ycv)
  implicit none
  type(control_vector), intent(in) :: xcv, ycv

! local variables
  real(r_kind) :: zz(nsubwin+1)
  integer(i_kind) :: ii

  if (xcv%lencv/=ycv%lencv) then
      write(6,*)'dot_prod_cv: error length',xcv%lencv,ycv%lencv
      call stop2(113)
  end if

  call dot_prod_vars(xcv,ycv,zz)

  dot_prod_cv = zero
  do ii=1,nsubwin+1
    dot_prod_cv = dot_prod_cv + zz(ii)
  enddo

return
end function dot_prod_cv
! ----------------------------------------------------------------------
real(r_quad) function qdot_prod_cv(xcv,ycv,kind)
  implicit none
  integer(i_kind),intent(in)::kind
  type(control_vector), intent(in) :: xcv, ycv

! local variables
  real(r_quad) :: zz(nsubwin+1)
  integer(i_kind) :: ii

  if (xcv%lencv/=ycv%lencv) then
      write(6,*)'qdot_prod_cv: error length',xcv%lencv,ycv%lencv
      call stop2(114)
  end if

  call qdot_prod_vars(xcv,ycv,zz)

  qdot_prod_cv = zero_quad
  do ii=1,nsubwin+1
    qdot_prod_cv = qdot_prod_cv + zz(ii)
  enddo

return
end function qdot_prod_cv
! ----------------------------------------------------------------------
subroutine prt_norms(xcv,sgrep)
  implicit none
  type(control_vector), intent(in) :: xcv
  character(len=*), intent(in) :: sgrep

  real(r_quad) :: zz(nsubwin+1),zt
  integer(i_kind) :: ii

  call dot_prod_vars(xcv,xcv,zz)
  zt = zero_quad
  do ii=1,nsubwin+1
    zt = zt + zz(ii)
  enddo
  zt=sqrt(zt)
  zz(:) = SQRT(zz(:))

  if (mype==0) then
    write(6,*)sgrep,' partial norms=',real(zz(:),r_kind)
    write(6,*)sgrep,' global  norm =',real(zt,r_kind)
  endif

!!!  call prt_norms_vars(xcv,sgrep)

  return
end subroutine prt_norms
! ----------------------------------------------------------------------
subroutine prt_norms_vars(xcv,sgrep)
  use m_stats,only : stats_sum,stats_allreduce
  implicit none
  type(control_vector), intent(in) :: xcv
  character(len=*), intent(in) :: sgrep

  real   (r_kind),dimension(8) :: vdot,vsum,vmin,vmax
  integer(i_kind),dimension(8) :: vnum
  integer :: iw,nsw,iv,nv
  real(r_kind),pointer,dimension(:) :: piv

  character(len=4),dimension(8) :: vnames = &
			(/'st  ','vp  ','t   ','rh  ','oz  ','cw  ','p   ','sst '/)

  nsw=size(xcv%step)

  do iv=1,8
    do iw=1,nsw
      piv => null()
      select case(iv)
      case(1); piv => xcv%step(iw)%st
      case(2); piv => xcv%step(iw)%vp
      case(3); piv => xcv%step(iw)%t
      case(4); piv => xcv%step(iw)%rh
      case(5); piv => xcv%step(iw)%oz
      case(6); piv => xcv%step(iw)%cw
      case(7); piv => xcv%step(iw)%p
      case(8); piv => xcv%step(iw)%sst
      end select

      call stats_sum(piv, &
      	vdot(iv),vsum(iv),vmin(iv),vmax(iv),vnum(iv),add=iw>1)
    enddo

    call stats_allreduce(vdot(iv),vsum(iv),vmin(iv),vmax(iv),	&
    	vnum(iv),MPI_comm_world)
    nv=max(vnum(iv),1)
  
    if(mype==0) then
      write(6,'(2(1x,a),4(1x,ES20.12),1x,i10)')		&
        sgrep,vnames(iv),sqrt(vdot(iv)/nv),vsum(iv)/nv,	&
		vmin(iv),vmax(iv),vnum(iv)
    endif
  end do
  piv => null()
  
end subroutine prt_norms_vars
! ----------------------------------------------------------------------
subroutine axpy(alpha,xcv,ycv)
  implicit none
  real(r_kind) :: alpha
  type(control_vector), intent(in) :: xcv
  type(control_vector), intent(inout) :: ycv
  integer(i_kind) :: ii

  if (xcv%lencv/=ycv%lencv) then
      write(6,*)'axpy: error length',xcv%lencv,ycv%lencv
      call stop2(115)
  end if

  DO ii=1,ycv%lencv
    ycv%values(ii) = ycv%values(ii) + alpha * xcv%values(ii)
  ENDDO

  return
end subroutine axpy
! ----------------------------------------------------------------------
subroutine random_cv(ycv,kseed)

implicit none
type(control_vector), intent(inout) :: ycv
integer(i_kind), optional, intent(in) :: kseed

integer(i_kind):: ii,jj,iseed
integer, allocatable :: nseed(:) ! Intentionaly default integer
real(r_kind), allocatable :: zz(:)

iseed=iadatebgn
if (present(kseed)) iseed=iseed+kseed
call random_seed(size=jj)
allocate(nseed(jj))
nseed(1:jj)=iseed
! The following because we don't want all procs to get
! exactly the same sequence (which would be repeated in
! the then not so random vector) but it makes the test
! not reproducible if the number of procs is changed.
nseed(1)=iseed+mype
call random_seed(put=nseed)
deallocate(nseed)

allocate(zz(nval_len))
do jj=1,nsubwin
  call random_number(zz)
  do ii=1,nval_len
    ycv%step(jj)%values(ii) = two*zz(ii)-one
  enddo
enddo
deallocate(zz)

if (nsclen>0) then
  allocate(zz(nsclen))
  call random_number(zz)
  do ii=1,nsclen
    ycv%predr(ii) = two*zz(ii)-one
  enddo
  deallocate(zz)
endif

if (npclen>0) then
  allocate(zz(npclen))
  call random_number(zz)
  do ii=1,npclen
    ycv%predr(ii) = two*zz(ii)-one
  enddo
  deallocate(zz)
endif

return
end subroutine random_cv
! ----------------------------------------------------------------------
subroutine write_cv(xcv,cdfile)
  implicit none
  type(control_vector), intent(in) :: xcv
  character(len=*), intent(in) :: cdfile

  character(len=100) :: clfile
  character(len=5) :: clmype
  integer(i_kind):: iunit

  iunit=get_lun()
  clmype='.YYYY'
  write(clmype(2:5),'(I4.4)')mype
  clfile=trim(cdfile)//clmype
  if (mype==0) write(6,*)'Writing control vector to file ',clfile

  open(iunit,file=trim(clfile),form='unformatted')
  write(iunit)xcv%lencv
  write(iunit)xcv%values(1:xcv%lencv)
  close(iunit)

  return
end subroutine write_cv
! ----------------------------------------------------------------------
subroutine read_cv(xcv,cdfile)
  implicit none
  type(control_vector), intent(inout) :: xcv
  character(len=*), intent(in) :: cdfile

  character(len=100) :: clfile
  character(len=5) :: clmype
  integer(i_kind):: iunit,ilen

  iunit=get_lun()
  clmype='.YYYY'
  write(clmype(2:5),'(I4.4)')mype
  clfile=trim(cdfile)//clmype
  if (mype==0) write(6,*)'Reading control vector from file ',clfile

  open(iunit,file=trim(clfile),form='unformatted')
  read(iunit)ilen
  if (ilen/=xcv%lencv) then
     write(6,*)'read_cv: wrong length',ilen,xcv%lencv
     call stop2(116)
  end if
  read(iunit)xcv%values(1:xcv%lencv)
  close(iunit)

  return
end subroutine read_cv
! ----------------------------------------------------------------------
subroutine inquire_cv
real(r_kind) :: zz

if (mype==0) then
  zz=real(max_vec_alloc*nclen,r_kind)*8.0_r_kind/1.048e6_r_kind
  write(6,*)'control_vectors: length=',nclen
  write(6,*)'control_vectors: currently allocated=',m_vec_alloc
  write(6,*)'control_vectors: maximum allocated=',max_vec_alloc
  write(6,*)'control_vectors: number of allocates=',m_allocs
  write(6,*)'control_vectors: number of deallocates=',m_deallocs
  write(6,'(A,F8.1,A)')'control_vectors: Estimated max memory used= ',zz,' Mb'
endif

end subroutine inquire_cv
! ----------------------------------------------------------------------
real(r_kind) function maxval_cv(ycv)

implicit none
type(control_vector), intent(in) :: ycv
real(r_kind) :: zloc(1),zglo(1)

zloc(1)=maxval(ycv%values(:))

call mpi_allreduce(zloc,zglo,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
if (ierror/=0) then
  write(6,*)'maxval_cv: MPI error',ierror
  call stop2(117)
end if

maxval_cv=zglo(1)

return
end function maxval_cv
! ----------------------------------------------------------------------
real(r_quad) function qdot_product(x,y)
  implicit none
  real(r_kind),intent(in)::x(:),y(:)
  real(r_quad):: zz
  integer(i_kind) :: nx,ny,i
  nx=size(x)
  ny=size(y)
  if(nx/=ny) then
    write(6,*)'qdot_product: inconsistent dims',nx,ny
    call stop2(118)
  end if
  zz=zero_quad
!$omp parallel do
  do i=1,nx
     zz = zz + x(i)*y(i)
  enddo
!$omp end parallel do
  qdot_product=zz
end function qdot_product
! ----------------------------------------------------------------------
end module control_vectors
