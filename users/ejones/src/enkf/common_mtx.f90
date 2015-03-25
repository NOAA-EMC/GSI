module common_mtx
!=======================================================================
!
! [PURPOSE:] Matrix Functions
!
! [CREATED:] 07/20/2004 Takemasa Miyoshi
! [UPDATED:] 10/16/2004 Takemasa Miyoshi
!            06/03/2011 Yoichiro Ota - Adapted for EnKF/GFS to meet with coding standards
!
! [PUBLIC:]
!   mtx_eigen  : eigenvalue decomposition
!   mtx_inv    : real symmetric matrix inverse
!   mtx_sqrt   : real symmetric matrix square root
!
! [REFERENCES:]
!    Core subroutines are adapted from netlib.org
!
! [HISTORY:]
!  07/20/2003 Takemasa Miyoshi  Created at University of Maryland, College Park
!
!=======================================================================
  use kinds,only: i_kind,r_kind,r_double
  use netlib,only: rs

  implicit none

  private
  public :: mtx_eigen

contains
!=======================================================================
!  Eigenvalue decomposition using subroutine rs
!    INPUT
!      integer :: imode           : mode switch (0: only eiven values)
!      integer :: n               : dimension of matrix
!      real(r_kind) :: a(n,n)     : input matrix
!    OUTPUT
!      real(r_kind) :: eival(n)   : eiven values in decending order
!                                   i.e. eival(1) is the largest
!      real(r_kind) :: eivec(n,n) : eiven vectors
!      integer :: nrank_eff       : number of positive eivenvalues
!=======================================================================
subroutine mtx_eigen(imode,n,a,eival,eivec,nrank_eff)
  implicit none

  integer(i_kind),intent(in) :: imode ! 0: calculate only eigen values
  integer(i_kind),intent(in) :: n
  real(r_kind),intent(in) :: a(1:n,1:n)
  real(r_kind),intent(out) :: eival(1:n)
  real(r_kind),intent(out) :: eivec(1:n,1:n)
  integer(i_kind),intent(out) :: nrank_eff

  real(r_double) :: a8(n,n)
  real(r_double) :: eival8(n)
  real(r_double) :: eivec8(n,n)
  real(r_double) :: wrk1(n)
  real(r_double) :: wrk2(n)
  integer(i_kind) :: ierr,i,j

  a8 = a
  eivec8 = 0._r_double
  call rs(n,n,a8,eival8,imode,eivec8,wrk1,wrk2,ierr)
  if( ierr/=0 ) then
    print *,'!!! ERROR (mtx_eigen): rs error code is ',ierr
    call stop2(2)
  end if

  nrank_eff = n
  if( eival8(n) > 0 ) then
    do i=1,n
      if( eival8(i) < abs(eival8(n))*sqrt(epsilon(eival8)) ) then
        nrank_eff = nrank_eff - 1
        eival8(i) = 0._r_double
        eivec8(:,i) = 0._r_double
      end if
    end do
  else
    print *,'!!! ERROR (mtx_eigen): All Eigenvalues are below 0'
    call stop2(2)
  end if

  if( nrank_eff<n .and. eival8(1)/=0 ) then
    j = 0
    do i=n,1,-1
      if( eival8(i) == 0 ) then
        eival8(i) = eival8(n-nrank_eff-j)
        eivec(:,i) = eivec8(:,n-nrank_eff-j)
        eival8(n-nrank_eff-j) = 0._r_double
        eivec8(:,n-nrank_eff-j) = 0._r_double
        j = j+1
      end if
    end do
  end if

  do i=1,n
    eival(i) = eival8(n+1-i)
    eivec(:,i) = eivec8(:,n+1-i)
  end do

  return
end subroutine mtx_eigen
end module common_mtx
