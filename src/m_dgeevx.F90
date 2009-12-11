!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_dgeevx - an alternative interface of LAPACK DGEEV()
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_dgeevx
      implicit none
      private   ! except

      public :: dgeevx          ! alternative DGEEV()

! !REVISION HISTORY:
!       29May08 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_dgeevx'

contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: dgeevx - an alternative interface of DGEEV
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine dgeevx(nsig,qmat,ldqmat,www,wwwd,zzz,ldzzz,zzzd,ldzzzd,info,mype)
      use kinds,only : i_kind
      use kinds,only : r_double,r_kind
      use constants,only : izero,ione,zero
      implicit none

      integer(i_kind)                        ,intent(in   ) :: nsig   ! dimension matrix A (n-by-n (n=nsig))
      integer(i_kind)                        ,intent(in   ) :: ldqmat ! leading-dimension of qmat
      real(r_kind)  ,dimension(ldqmat  ,nsig),intent(in   ) :: qmat   ! matrix A
      real(r_double),dimension(2,nsig)       ,intent(  out) :: www    ! right-eigen-values
      real(r_double),dimension(2,nsig)       ,intent(  out) :: wwwd   ! left-eigen-values
      integer(i_kind)                        ,intent(in   ) :: ldzzz  ! leading-dimension of zzz
      real(r_double),dimension(2,ldzzz ,nsig),intent(  out) :: zzz    ! right-eigen-vectors
      integer(i_kind)                        ,intent(in   ) :: ldzzzd ! leading-dimension of zzzd
      real(r_double),dimension(2,ldzzzd,nsig),intent(  out) :: zzzd   ! left-eigen-vectors
      integer(i_kind)                        ,intent(  out) :: info   ! return status
      integer(i_kind)                        ,intent(in   ) :: mype   ! PE rank for diagnostics, -1 to turnoff

! !REVISION HISTORY:
!       29May08 - Jing Guo <guo@gmao.gsfc.nasa.gov>
!               - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::dgeevx'

#ifdef ibm_sp

    integer(i_kind)  :: iopt,i,j,k
    real(r_double)   :: aaa(nsig,nsig)
    logical          :: select(nsig)    ! an argument of dgeev(), but not used.
    integer(i_kind)  :: naux
    real(r_kind)     :: aux,factor

	! Use IBM ESSL routine dgeev()

! Below is a copy of the original code in mod_vtrans.f90_1.2.
!<<<<
! next get eigenvalues, eigenvectors and compare to singular values, vectors.
! use essl routine dgeev

    iopt=ione      !  eigenvalues and eigenvectors are computed

    do j=1,nsig
      do i=1,nsig
        aaa(i,j)=qmat(i,j)
      end do
    end do
    naux=izero
    call dgeev(iopt,aaa,nsig,www,zzz,nsig,select,nsig,aux,naux)
!   sort from largest to smallest eigenvalue
    do j=1,nsig-ione
      do i=j+ione,nsig
        if(www(1,i)>www(1,j)) then
          factor=www(1,j)
          www(1,j)=www(1,i)
          www(1,i)=factor
          factor=www(2,j)
          www(2,j)=www(2,i)
          www(2,i)=factor
          do k=1,nsig
            factor=zzz(1,k,j)
            zzz(1,k,j)=zzz(1,k,i)
            zzz(1,k,i)=factor
            factor=zzz(2,k,j)
            zzz(2,k,j)=zzz(2,k,i)
            zzz(2,k,i)=factor
          end do
        end if
      end do
    end do

! checks and print out eigenvalues (removed)
!

    iopt=ione      !  eigenvalues and dual eigenvectors are computed next
    do j=1,nsig
      do i=1,nsig
        aaa(i,j)=qmat(j,i)         !  to get dual vectors, use transpose of qmat
      end do
    end do
    naux=izero
    call dgeev(iopt,aaa,nsig,wwwd,zzzd,nsig,select,nsig,aux,naux)
!   sort from largest to smallest eigenvalue
    do j=1,nsig-ione
      do i=j+ione,nsig
        if(wwwd(1,i)>wwwd(1,j)) then
          factor=wwwd(1,j)
          wwwd(1,j)=wwwd(1,i)
          wwwd(1,i)=factor
          factor=wwwd(2,j)
          wwwd(2,j)=wwwd(2,i)
          wwwd(2,i)=factor
          do k=1,nsig
            factor=zzzd(1,k,j)
            zzzd(1,k,j)=zzzd(1,k,i)
            zzzd(1,k,i)=factor
            factor=zzzd(2,k,j)
            zzzd(2,k,j)=zzzd(2,k,i)
            zzzd(2,k,i)=factor
          end do
        end if
      end do
    end do

    info=izero
  
#else
	! Use standard LAPACK routine dgeev()

!----------------------------------------
! Modification for using GSI dgeev  -RY
!----------------------------------------
    character*1,parameter:: jobvr='V'
    character*1,parameter:: jobvl='V'
!  double precision
    real(r_double)  :: aaa(nsig,nsig)
    real(r_double)  :: wr(nsig),wi(nsig)
    real(r_double)  :: vl(nsig,nsig),vr(nsig,nsig)
    real(r_double)  :: work(4*nsig+ione)
    integer(i_kind) :: lwork,i,j,k
    real(r_kind)    :: factor,factor2

! size of work(:), used by dgeev subroutine 
    lwork=size(work)

!------------------------------------
!  ryang: 
!      use SGI scslib dgeev subroutine

    do j=1,nsig
      do i=1,nsig
        aaa(i,j)=qmat(i,j)
      end do
    end do

    if (mype ==izero) write (6,*) 'in mod_vtrans_create_vtrans, before CALL DGEEV'
    call dgeev(jobvl,jobvr,nsig,aaa,nsig,wr,wi,vl,nsig,vr,nsig,work,lwork,info)


    if (mype ==izero) write (6,*) ' AFTER CALL DGEEV', 'status: info =  ', info

! use Dave's array names
    do j=1,nsig
           if (wi(j) /= zero) write (6,*) &
              'wrong eigen computation: create_vtrans'
       www(1,j)=wr(j)
       www(2,j)=wi(j)
!-----------------------------------------------------------
!ADD explanation:!!!
!
! vl is zzzd
!the following line is not generic, only hold when wi (j)=0.0
!eigenvalues of A^transpose are the same as A
!----------------------------------------------
       do k=1,nsig
          zzz (1,k,j)=vr(k,j)
          zzzd(1,k,j)=vl(k,j)
          zzz (2,k,j)=zero
          zzzd(2,k,j)=zero
        enddo
    enddo
! back to Dave's code
!sort from largest to smallest eigenvalues
!   sort from largest to smallest eigenvalue
    do j=1,nsig-ione
      do i=j+ione,nsig
        if(www(1,i)>www(1,j)) then
          factor=www(1,j)
          www(1,j)=www(1,i)
          www(1,i)=factor
          factor=www(2,j)
          www(2,j)=www(2,i)
          www(2,i)=factor
          do k=1,nsig
            factor =zzz (1,k,j)
            factor2=zzzd(1,k,j)
            zzz (1,k,j)=zzz (1,k,i)
            zzzd(1,k,j)=zzzd(1,k,i)
            zzz (1,k,i)=factor
            zzzd(1,k,i)=factor2
            factor =zzz (2,k,j)
            factor2=zzzd(2,k,j)
            zzz (2,k,j)=zzz (2,k,i)
            zzzd(2,k,j)=zzzd(2,k,i)
            zzz (2,k,i)=factor
            zzzd(2,k,i)=factor2
          end do
        end if
      end do
    end do

! checks and print out eigenvalues (removed)
!

!**********************************************************
!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! do not need to do the transpose

! check the eigenvalues

!  using the component form
     wwwd=www

    if (mype==izero) write (6,*) '****************************'
    if (mype==izero) write (6,*) 'SECOND TIME CALL DGEEV'
#endif
end subroutine dgeevx
end module m_dgeevx
