module intrwmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrwmod    module for intrw and its tangent linear intrw_tl
!
! abstract: module for intrw and its tangent linear intrw_tl
!
! program history log:
!   2005-05-13  Yanqiu zhu - wrap intrw and its tangent linear intrw_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC intrw,intrw_tl


contains

subroutine intrw(ru,rv,su,sv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrw       apply nonlin qc operator for radar winds
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator for radar winds
!             with nonlinear qc operator
!
! program history log:
!   1991-02-26  derber
!   1999-11-22  yang
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intrw and intrw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!
! usage: call intw(ru,rv,su,sv)
!   input argument list:
!     su       - current solution increment in grid space
!     sv       - current solution increment in grid space
!
!   output argument list:
!     ru       - results from observation operator (0 for no data)
!     rv       - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,two,zero,tiny_r_kind,cg_term
  use obsmod, only: rwhead,rwptr
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: su,sv
  real(r_kind),dimension(latlon1n),intent(inout):: ru,rv

! Declare local varibles
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
! real(r_kind) penalty
  real(r_kind) val,valu,valv,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) cg_rw,p0,grad,wnotgross,wgross

  rwptr => rwhead
  do while (associated(rwptr))
     j1=rwptr%ij(1)
     j2=rwptr%ij(2)
     j3=rwptr%ij(3)
     j4=rwptr%ij(4)
     j5=rwptr%ij(5)
     j6=rwptr%ij(6)
     j7=rwptr%ij(7)
     j8=rwptr%ij(8)
     w1=rwptr%wij(1)
     w2=rwptr%wij(2)
     w3=rwptr%wij(3)
     w4=rwptr%wij(4)
     w5=rwptr%wij(5)
     w6=rwptr%wij(6)
     w7=rwptr%wij(7)
     w8=rwptr%wij(8)

!    Forward model
     val=(w1*su(j1)+w2*su(j2)+w3*su(j3)+w4*su(j4)+                  &
          w5*su(j5)+w6*su(j6)+w7*su(j7)+w8*su(j8))*rwptr%cosazm+ &
         (w1*sv(j1)+w2*sv(j2)+w3*sv(j3)+w4*sv(j4)+                  &
          w5*sv(j5)+w6*sv(j6)+w7*sv(j7)+w8*sv(j8))*rwptr%sinazm  &
          -rwptr%res

!    gradient of nonlinear operator
     if (nlnqc_iter .and. rwptr%pg > tiny_r_kind .and. &
                          rwptr%b  > tiny_r_kind) then
        cg_rw=cg_term/rwptr%b
        wnotgross= one-rwptr%pg
        wgross = rwptr%pg*cg_rw/wnotgross
        p0   = wgross/(wgross+exp(-half*rwptr%err2*val**2))
        val = val*(one-p0)
     endif

     grad     = val*rwptr%raterr2*rwptr%err2

!    Adjoint
     valu=rwptr%cosazm*grad
     valv=rwptr%sinazm*grad
     ru(j1)=ru(j1)+w1*valu
     ru(j2)=ru(j2)+w2*valu
     ru(j3)=ru(j3)+w3*valu
     ru(j4)=ru(j4)+w4*valu
     ru(j5)=ru(j5)+w5*valu
     ru(j6)=ru(j6)+w6*valu
     ru(j7)=ru(j7)+w7*valu
     ru(j8)=ru(j8)+w8*valu
     rv(j1)=rv(j1)+w1*valv
     rv(j2)=rv(j2)+w2*valv
     rv(j3)=rv(j3)+w3*valv
     rv(j4)=rv(j4)+w4*valv
     rv(j5)=rv(j5)+w5*valv
     rv(j6)=rv(j6)+w6*valv
     rv(j7)=rv(j7)+w7*valv
     rv(j8)=rv(j8)+w8*valv

     rwptr => rwptr%llpoint
  end do
  return
end subroutine intrw


subroutine intrw_tl(ru,rv,su,sv,ru_tl,rv_tl,su_tl,sv_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrw_tl       the tangent linear of the operator that applies 
!                              nonlin qc operator for radar winds
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-13
!
! abstract: the tangent linear of the operator that applies observation 
!           operator for radar winds with nonlinear qc operator
!
! program history log:
!   2005-05-13  yanqiu zhu - tangent linear of intrw
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!
! usage: call intrw_tl(ru,rv,su,sv,ru_tl,rv_tl,su_tl,sv_tl)
!   input argument list:
!     su       - current solution increment in grid space
!     sv       - current solution increment in grid space
!     su_tl     - current tangent linear solution increment in grid space
!     sv_tl     - current tangent linear solution increment in grid space
!
!   output argument list:
!     ru       - results from observation operator (0 for no data)
!     rv       - results from observation operator (0 for no data)
!     ru_tl     - tangent linear results from observation operator (0 for no data)
!     rv_tl     - tangent linear results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,two,zero,tiny_r_kind,cg_term
  use obsmod, only: rwhead,rwptr
  use obsmod_tl, only: rrw_tl
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: su,sv
  real(r_kind),dimension(latlon1n),intent(in):: su_tl,sv_tl
  real(r_kind),dimension(latlon1n),intent(inout):: ru,rv
  real(r_kind),dimension(latlon1n),intent(inout):: ru_tl,rv_tl

! Declare local varibles
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
! real(r_kind) penalty
  real(r_kind) val,valu,valv,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) val_tl,valu_tl,valv_tl
  real(r_kind) cg_rw,p0,grad,wnotgross,wgross,term
  real(r_kind) p0_tl,grad_tl,term_tl

  rwptr => rwhead
  i=0
  do while (associated(rwptr))
     i=i+1
     j1=rwptr%ij(1)
     j2=rwptr%ij(2)
     j3=rwptr%ij(3)
     j4=rwptr%ij(4)
     j5=rwptr%ij(5)
     j6=rwptr%ij(6)
     j7=rwptr%ij(7)
     j8=rwptr%ij(8)
     w1=rwptr%wij(1)
     w2=rwptr%wij(2)
     w3=rwptr%wij(3)
     w4=rwptr%wij(4)
     w5=rwptr%wij(5)
     w6=rwptr%wij(6)
     w7=rwptr%wij(7)
     w8=rwptr%wij(8)

!    Forward model
     val_tl=(w1*su_tl(j1)+w2*su_tl(j2)+               &
             w3*su_tl(j3)+w4*su_tl(j4)+               &
             w5*su_tl(j5)+w6*su_tl(j6)+               &
             w7*su_tl(j7)+w8*su_tl(j8))*rwptr%cosazm+ &
            (w1*sv_tl(j1)+w2*sv_tl(j2)+               &
             w3*sv_tl(j3)+w4*sv_tl(j4)+               &
             w5*sv_tl(j5)+w6*sv_tl(j6)+               &
             w7*sv_tl(j7)+w8*sv_tl(j8))*rwptr%sinazm  &
             -rrw_tl(i)
     val=(w1*su(j1)+w2*su(j2)+               &
          w3*su(j3)+w4*su(j4)+               &
          w5*su(j5)+w6*su(j6)+               &
          w7*su(j7)+w8*su(j8))*rwptr%cosazm+ &
         (w1*sv(j1)+w2*sv(j2)+               &
          w3*sv(j3)+w4*sv(j4)+               &
          w5*sv(j5)+w6*sv(j6)+               &
          w7*sv(j7)+w8*sv(j8))*rwptr%sinazm  &
          -rwptr%res

!    gradient of nonlinear operator
     if (nlnqc_iter .and. rwptr%pg > tiny_r_kind) then
        cg_rw=cg_term/rwptr%b
        wnotgross= one-rwptr%pg
        wgross = rwptr%pg*cg_rw
        p0   = wnotgross*exp(-half*rwptr%err2*val**2)+wgross
        term = (p0-wgross)/p0
        p0_tl = -val*(p0-wgross)*val_tl*rwptr%err2
        term_tl = wgross/(p0*p0)*p0_tl 
     else
        term = one
        term_tl = zero
     endif
     grad     = val * term
     grad_tl   = val_tl * term + val * term_tl
     grad     = grad*rwptr%raterr2*rwptr%err2
     grad_tl   = grad_tl*rwptr%raterr2*rwptr%err2


!    Adjoint
     valu_tl=rwptr%cosazm*grad_tl
     valv_tl=rwptr%sinazm*grad_tl
     ru_tl(j1)=ru_tl(j1)+w1*valu_tl
     ru_tl(j2)=ru_tl(j2)+w2*valu_tl
     ru_tl(j3)=ru_tl(j3)+w3*valu_tl
     ru_tl(j4)=ru_tl(j4)+w4*valu_tl
     ru_tl(j5)=ru_tl(j5)+w5*valu_tl
     ru_tl(j6)=ru_tl(j6)+w6*valu_tl
     ru_tl(j7)=ru_tl(j7)+w7*valu_tl
     ru_tl(j8)=ru_tl(j8)+w8*valu_tl
     rv_tl(j1)=rv_tl(j1)+w1*valv_tl
     rv_tl(j2)=rv_tl(j2)+w2*valv_tl
     rv_tl(j3)=rv_tl(j3)+w3*valv_tl
     rv_tl(j4)=rv_tl(j4)+w4*valv_tl
     rv_tl(j5)=rv_tl(j5)+w5*valv_tl
     rv_tl(j6)=rv_tl(j6)+w6*valv_tl
     rv_tl(j7)=rv_tl(j7)+w7*valv_tl
     rv_tl(j8)=rv_tl(j8)+w8*valv_tl
     valu=rwptr%cosazm*grad
     valv=rwptr%sinazm*grad
     ru(j1)=ru(j1)+w1*valu
     ru(j2)=ru(j2)+w2*valu
     ru(j3)=ru(j3)+w3*valu
     ru(j4)=ru(j4)+w4*valu
     ru(j5)=ru(j5)+w5*valu
     ru(j6)=ru(j6)+w6*valu
     ru(j7)=ru(j7)+w7*valu
     ru(j8)=ru(j8)+w8*valu
     rv(j1)=rv(j1)+w1*valv
     rv(j2)=rv(j2)+w2*valv
     rv(j3)=rv(j3)+w3*valv
     rv(j4)=rv(j4)+w4*valv
     rv(j5)=rv(j5)+w5*valv
     rv(j6)=rv(j6)+w6*valv
     rv(j7)=rv(j7)+w7*valv
     rv(j8)=rv(j8)+w8*valv

     rwptr => rwptr%llpoint

  end do
  return
end subroutine intrw_tl

end module intrwmod
