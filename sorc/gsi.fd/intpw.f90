module intpwmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intpwmod    module for intpw and its tangent linear intpw_tl
!
! abstract: module for intpw and its tangent linear intpw_tl
!
! program history log:
!   2005-05-16  Yanqiu zhu - wrap intpw and its tangent linear intpw_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC intpw,intpw_tl

contains

subroutine intpw(rq,sq)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intpw       apply nonlin qc obs operator for p.w.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for precip. water
!           with addition of nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1993-08-15  wu
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-12-23  d.kleist - routine generalized to use interpolated delta(pressure)
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intpw and intpw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-03-30  wu - add vertical index k to i1,i2,i3,i4 in adjoint (bug fix)
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!
!   input argument list:
!     sq       - increment in grid space
!
!   output argument list:
!     rq       - results from observation operator (0 for no data)
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: pwhead,pwptr
  use gridmod, only: latlon11,latlon1n,nsig
  use qcmod, only: nlnqc_iter
  use constants, only: zero,tpwcon,half,one,two,tiny_r_kind,cg_term
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: sq
  real(r_kind),dimension(latlon1n),intent(inout):: rq

! Declare local variables
  integer(i_kind) i,k
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
! real(r_kind) penalty
  real(r_kind) val,pwcon1,w1,w2,w3,w4
  real(r_kind) cg_pw,grad,p0,wnotgross,wgross

  pwptr => pwhead
  do while (associated(pwptr))
     val=zero
     w1=pwptr%wij(1)
     w2=pwptr%wij(2)
     w3=pwptr%wij(3)
     w4=pwptr%wij(4)
     i1(1)=pwptr%ij(1)
     i2(1)=pwptr%ij(2)
     i3(1)=pwptr%ij(3)
     i4(1)=pwptr%ij(4)
     do k=2,nsig
        i1(k)=i1(k-1)+latlon11
        i2(k)=i2(k-1)+latlon11
        i3(k)=i3(k-1)+latlon11
        i4(k)=i4(k-1)+latlon11
     end do
     
!    Forward model
     do k=1,nsig
        val=val+(w1*sq(i1(k))+w2*sq(i2(k))   &
               + w3*sq(i3(k))+w4*sq(i4(k)))* &
                 tpwcon*pwptr%dp(k)
     end do
     
!    Difference from observation
     val=val-pwptr%res

!    needed for gradient of nonlinear qc operator
     if (nlnqc_iter .and. pwptr%pg > tiny_r_kind .and.  &
                          pwptr%b  > tiny_r_kind) then
        cg_pw=cg_term/pwptr%b
        wnotgross= one-pwptr%pg
        wgross = pwptr%pg*cg_pw/wnotgross
        p0   = wgross/(wgross+exp(-half*pwptr%err2*val**2))
        val = val*(one-p0)
     endif

     grad     = val*pwptr%raterr2*pwptr%err2

!    Adjoint
     do k=1,nsig
        pwcon1=tpwcon*pwptr%dp(k)*grad
        rq(i1(k))=rq(i1(k))+w1*pwcon1
        rq(i2(k))=rq(i2(k))+w2*pwcon1
        rq(i3(k))=rq(i3(k))+w3*pwcon1
        rq(i4(k))=rq(i4(k))+w4*pwcon1
     end do

     pwptr => pwptr%llpoint

  end do
  return
end subroutine intpw


subroutine intpw_tl(rq,sq,rq_tl,sq_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intpw_tl       the tangent linear of the operator that applies 
!                              nonlin qc obs operator for p.w.
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-16
!
! abstract: the tangent linear of the operator that applies observation 
!           operator and adjoint for precip. water with addition of nonlinear qc.
!
! program history log:
!   2005-05-16  yanqiu zhu - tangent linear of intpw
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2006-03-30  wu - add vertical index k to i1,i2,i3,i4 in adjoint (bug fix)
!
!   input argument list:
!     sq       - increment in grid space
!     sq_tl     - tangent linear increment in grid space
!
!   output argument list:
!     rq       - results from observation operator (0 for no data)
!     rq_tl     - tangent linear results from observation operator (0 for no data)
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: pwhead,pwptr
  use obsmod_tl, only: pwdataerr_tl
  use gridmod, only: latlon11,latlon1n,nsig
  use qcmod, only: nlnqc_iter
  use constants, only: zero,tpwcon,half,one,two,tiny_r_kind,cg_term
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: sq
  real(r_kind),dimension(latlon1n),intent(in):: sq_tl
  real(r_kind),dimension(latlon1n),intent(inout):: rq
  real(r_kind),dimension(latlon1n),intent(inout):: rq_tl

! Declare local variables
  integer(i_kind) i,k
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
! real(r_kind) penalty
  real(r_kind) val,pwcon1,w1,w2,w3,w4
  real(r_kind) cg_pw,grad,p0,wnotgross,wgross,term
  real(r_kind) val_tl,pwcon1_tl
  real(r_kind) grad_tl,p0_tl,term_tl

  pwptr => pwhead
  i=0
  do while (associated(pwptr))
     i=i+1
     val_tl=zero
     val=zero
     w1=pwptr%wij(1)
     w2=pwptr%wij(2)
     w3=pwptr%wij(3)
     w4=pwptr%wij(4)
     i1(1)=pwptr%ij(1)
     i2(1)=pwptr%ij(2)
     i3(1)=pwptr%ij(3)
     i4(1)=pwptr%ij(4)
     do k=2,nsig
        i1(k)=i1(k-1)+latlon11
        i2(k)=i2(k-1)+latlon11
        i3(k)=i3(k-1)+latlon11
        i4(k)=i4(k-1)+latlon11
     end do
     
!    Forward model
     do k=1,nsig
        val_tl=val_tl+(w1*sq_tl(i1(k))+w2*sq_tl(i2(k))   &
                     + w3*sq_tl(i3(k))+w4*sq_tl(i4(k)))* &
                     tpwcon*pwptr%dp(k)
        val=val+(w1*sq(i1(k))+w2*sq(i2(k))   &
               + w3*sq(i3(k))+w4*sq(i4(k)))* &
                 tpwcon*pwptr%dp(k)
     end do
     
!    Difference from observation
     val_tl=val_tl-pwdataerr_tl(i)
     val=val-pwptr%res

!    needed for gradient of nonlinear qc operator
     if (nlnqc_iter .and. pwptr%pg > tiny_r_kind) then
        cg_pw=cg_term/pwptr%b
        wnotgross= one-pwptr%pg
        wgross = pwptr%pg*cg_pw
        p0   = wnotgross*exp(-half*pwptr%err2*val**2)+wgross
        term = (p0-wgross)/p0
        p0_tl = -val*(p0-wgross)*val_tl
        term_tl = wgross/(p0*p0)*p0_tl
     else
        term = one
        term_tl = zero
     endif
     grad     = val * term
     grad_tl   = val_tl * term + val * term_tl
     grad     = grad*pwptr%raterr2*pwptr%err2
     grad_tl   = grad_tl*pwptr%raterr2*pwptr%err2

!    Adjoint
     do k=1,nsig
        pwcon1_tl=tpwcon*pwptr%dp(k)*grad_tl
        rq_tl(i1(k))=rq_tl(i1(k))+w1*pwcon1_tl
        rq_tl(i2(k))=rq_tl(i2(k))+w2*pwcon1_tl
        rq_tl(i3(k))=rq_tl(i3(k))+w3*pwcon1_tl
        rq_tl(i4(k))=rq_tl(i4(k))+w4*pwcon1_tl
        pwcon1=tpwcon*pwptr%dp(k)*grad
        rq(i1(k))=rq(i1(k))+w1*pwcon1
        rq(i2(k))=rq(i2(k))+w2*pwcon1
        rq(i3(k))=rq(i3(k))+w3*pwcon1
        rq(i4(k))=rq(i4(k))+w4*pwcon1
     end do

     pwptr => pwptr%llpoint

  end do
  return
end subroutine intpw_tl

end module intpwmod
