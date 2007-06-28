module intsstmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intsstmod    module for intsst and its tangent linear intsst_tl
!
! abstract: module for intsst and its tangent linear intsst_tl
!
! program history log:
!   2005-05-16  Yanqiu zhu - wrap intsst and its tangent linear intsst_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC intsst,intsst_tl

contains

subroutine intsst(rsst,ssst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intsst      apply nonlin qc obs operator for conv. sst
!   prgmmr: derber           org: np23                date: 2004-07-20
!
! abstract: apply observation operator and adjoint for conventional sst
!           observations with nonlinear qc operator
!
! program history log:
!   2004-07-20  derber
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-09  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intsst and intsst_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!
!   input argument list:
!     ssst    - increment in grid space
!
!   output argument list:
!     rsst    - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,two,zero,tiny_r_kind,cg_term
  use obsmod, only: ssthead,sstptr
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon11
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon11),intent(in):: ssst
  real(r_kind),dimension(latlon11),intent(inout):: rsst

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_sst,p0,grad,wnotgross,wgross

  sstptr => ssthead
  do while (associated(sstptr))
     j1=sstptr%ij(1)
     j2=sstptr%ij(2)
     j3=sstptr%ij(3)
     j4=sstptr%ij(4)
     w1=sstptr%wij(1)
     w2=sstptr%wij(2)
     w3=sstptr%wij(3)
     w4=sstptr%wij(4)

!    Forward model
     val=w1*ssst(j1)+w2*ssst(j2)&
        +w3*ssst(j3)+w4*ssst(j4)-sstptr%res

!    gradient of nonlinear operator
     if (nlnqc_iter .and. sstptr%pg > tiny_r_kind .and. &
                          sstptr%b  > tiny_r_kind) then
        cg_sst=cg_term/sstptr%b
        wnotgross= one-sstptr%pg
        wgross = sstptr%pg*cg_sst/wnotgross
        p0   = wgross/(wgross+exp(-half*sstptr%err2*val**2))
        val = val*(one-p0)
     endif

     grad     = val*sstptr%raterr2*sstptr%err2

!    Adjoint
     rsst(j1)=rsst(j1)+w1*grad
     rsst(j2)=rsst(j2)+w2*grad
     rsst(j3)=rsst(j3)+w3*grad
     rsst(j4)=rsst(j4)+w4*grad

     sstptr => sstptr%llpoint

  end do
  return
end subroutine intsst


subroutine intsst_tl(rsst,ssst,rsst_tl,ssst_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intsst_tl      the tangent linear of the operator that applies 
!                              nonlin qc obs operator for conv. sst
!   prgmmr: yanqiu           org: GMAO                date: 2005-05-16
!
! abstract: the tangent linear of the operator that applies observation operator 
!           and adjoint for conventional sst observations with nonlinear qc operator
!
! program history log:
!   2005-05-16  Yanqiu zhu - tangent linear of intsst
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!
!   input argument list:
!     ssst    - increment in grid space
!     ssst_tl - tangent linear increment in grid space
!
!   output argument list:
!     rsst    - results from observation operator (0 for no data)
!     rsst_tl - tangent linear results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,two,zero,tiny_r_kind,cg_term
  use obsmod, only: ssthead,sstptr
  use obsmod_tl, only: sstdataerr_tl
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon11
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon11),intent(in):: ssst
  real(r_kind),dimension(latlon11),intent(in):: ssst_tl
  real(r_kind),dimension(latlon11),intent(inout):: rsst
  real(r_kind),dimension(latlon11),intent(inout):: rsst_tl

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
  real(r_kind) w1,w2,w3,w4
! real(r_kind) penalty
  real(r_kind) val
  real(r_kind) val_tl
  real(r_kind) cg_sst,p0,grad,wnotgross,wgross,term
  real(r_kind) p0_tl,grad_tl,term_tl

  sstptr => ssthead
  i=0
  do while (associated(sstptr))
     i=i+1
     j1=sstptr%ij(1)
     j2=sstptr%ij(2)
     j3=sstptr%ij(3)
     j4=sstptr%ij(4)
     w1=sstptr%wij(1)
     w2=sstptr%wij(2)
     w3=sstptr%wij(3)
     w4=sstptr%wij(4)

!    Forward model
     val=w1*ssst(j1)+w2*ssst(j2)&
        +w3*ssst(j3)+w4*ssst(j4)-sstptr%res
     val_tl=w1*ssst_tl(j1)+w2*ssst_tl(j2)&
          +w3*ssst_tl(j3)+w4*ssst_tl(j4)-sstdataerr_tl(i)

!    gradient of nonlinear operator
     if (nlnqc_iter .and. sstptr%pg > tiny_r_kind) then
        cg_sst=cg_term/sstptr%b
        wnotgross= one-sstptr%pg
        wgross = sstptr%pg*cg_sst
        p0     = wnotgross*exp(-half*sstptr%err2*val**2)+wgross
        term   = (p0-wgross)/p0
        p0_tl  = -val*(p0-wgross)*val_tl*sstptr%err2
        term_tl = wgross/(p0*p0)*p0_tl
     else
        term = one
        term_tl = zero
     endif
     grad     = val*term
     grad_tl  = val_tl*term + val*term_tl
     grad     = grad*sstptr%raterr2*sstptr%err2
     grad_tl  = grad_tl*sstptr%raterr2*sstptr%err2


!    Adjoint
     rsst(j1)=rsst(j1)+w1*grad
     rsst(j2)=rsst(j2)+w2*grad
     rsst(j3)=rsst(j3)+w3*grad
     rsst(j4)=rsst(j4)+w4*grad
     rsst_tl(j1)=rsst_tl(j1)+w1*grad_tl
     rsst_tl(j2)=rsst_tl(j2)+w2*grad_tl
     rsst_tl(j3)=rsst_tl(j3)+w3*grad_tl
     rsst_tl(j4)=rsst_tl(j4)+w4*grad_tl
     
     sstptr => sstptr%llpoint

  end do
  return
end subroutine intsst_tl

end module intsstmod
