module stpdwmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpdwmod    module for stpdw and its tangent linear stpdw_tl
!
! abstract: module for stpdw and its tangent linear stpdw_tl
!
! program history log:
!   2005-05-18  Yanqiu zhu - wrap stpdw and its tangent linear stpdw_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC stpdw,stpdw_tl

contains

subroutine stpdw(ru,rv,su,sv,pen,b1,b3,sges1,sges2,sges3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpdw  calculate contribution to penalty and
!                stepsize from dw, with nonlinear qc added.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate contribution to penalty and stepsize from lidar winds
!
! program history log:
!   1991-02-26  derber
!   1999-11-22  yang
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpdw and stpdw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2007-07-28  derber   - modify to use new inner loop obs data structure
!                        - unify NL qc
!
!   input argument list:
!     ru   - search direction for u
!     rv   - search direction for v
!     su   - current analysis increment for u
!     sv   - current analysis increment for v
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!
!   output argument list:                                      
!     pen  - penalty contribution from lidar winds
!     b1   - contribution to numerator from lidar winds
!     b3   - contribution to denomenator from lidar winds
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: dwhead,dwptr
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),dimension(latlon1n),intent(in):: ru,rv,su,sv
  real(r_kind),intent(in):: sges1,sges2,sges3

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) valdw,facdw,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc
  real(r_kind) cg_dw,dw1,dw2,dw3,pen1,pen2,pen3,pencur,wgross,wnotgross

  pen=zero
  b1=zero; b3=zero
  alpha=one/(sges2-sges1)
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges2*ccoef

  dwptr => dwhead
  do while (associated(dwptr))
    if(dwptr%luse)then
     j1=dwptr%ij(1)
     j2=dwptr%ij(2)
     j3=dwptr%ij(3)
     j4=dwptr%ij(4)
     j5=dwptr%ij(5)
     j6=dwptr%ij(6)
     j7=dwptr%ij(7)
     j8=dwptr%ij(8)
     w1=dwptr%wij(1)
     w2=dwptr%wij(2)
     w3=dwptr%wij(3)
     w4=dwptr%wij(4)
     w5=dwptr%wij(5)
     w6=dwptr%wij(6)
     w7=dwptr%wij(7)
     w8=dwptr%wij(8)

     valdw=(w1*ru(j1)+w2*ru(j2)+w3*ru(j3)+w4*ru(j4)+&
            w5*ru(j5)+w6*ru(j6)+w7*ru(j7)+w8*ru(j8))*dwptr%sinazm+&
           (w1*rv(j1)+w2*rv(j2)+w3*rv(j3)+w4*rv(j4)+&
            w5*rv(j5)+w6*rv(j6)+w7*rv(j7)+w8*rv(j8))*dwptr%cosazm

     facdw=(w1*su(j1)+w2*su(j2)+w3*su(j3)+w4*su(j4)+&
            w5*su(j5)+w6*su(j6)+w7*su(j7)+w8*su(j8))*dwptr%sinazm+&
           (w1*sv(j1)+w2*sv(j2)+w3*sv(j3)+w4*sv(j4)+&
            w5*sv(j5)+w6*sv(j6)+w7*sv(j7)+w8*sv(j8))*dwptr%cosazm&
           -dwptr%res
     dw1=facdw+sges1*valdw
     dw2=facdw+sges2*valdw
     dw3=facdw+sges3*valdw

     pencur = facdw*facdw*dwptr%err2
     pen1   = dw1*dw1*dwptr%err2
     pen2   = dw2*dw2*dwptr%err2
     pen3   = dw3*dw3*dwptr%err2

!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. dwptr%pg > tiny_r_kind .and. dwptr%b > tiny_r_kind) then
        cg_dw=cg_term/dwptr%b
        wnotgross= one-dwptr%pg
        wgross = dwptr%pg*cg_dw/wnotgross
        pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
     endif

     pen = pen+pencur*dwptr%raterr2
     cc  = (pen1+pen3-two*pen2)*dwptr%raterr2
     b1  = b1+(pen1-pen3)*dwptr%raterr2*bcoef1+cc*bcoef2
     b3  = b3+cc*ccoef
   end if
   
   dwptr => dwptr%llpoint

  end do

  return
end subroutine stpdw

subroutine stpdw_tl(ru,rv,su,sv,pen,b1,b3,sges1,sges2,sges3, &
                   ru_tl,rv_tl,su_tl,sv_tl,pen_tl,b1_tl,b3_tl,sges1_tl,sges2_tl,sges3_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpdw_tl  the tangent linear of the operator that calculates contribution 
!                         to penalty and stepsize from dw, with nonlinear qc added.
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-18
!
! abstract: the tangent linear of the operator that calculates contribution to 
!           penalty and stepsize from lidar winds
!
! program history log:
!   2005-05-18  yanqiu zhu - tangent linear of stpdw
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!
!   input argument list:
!     ru   - search direction for u
!     rv   - search direction for v
!     su   - current analysis increment for u
!     sv   - current analysis increment for v
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!     ru_tl   - tangent linear search direction for u
!     rv_tl   - tangent linear search direction for v
!     su_tl   - current tangent linear analysis increment for u
!     sv_tl   - current tangent linear analysis increment for v
!     sges1_tl    - tangent linear estimate step size 1
!     sges2_tl    - tangent linear estimate step size 2
!     sges3_tl    - tangent linear estimate step size 3
!
!   output argument list:                                      
!     pen  - penalty contribution from lidar winds
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!     pen_tl  - tangent linear of penalty contribution from lidar winds
!     b1_tl       - pen_tl(sges1)-pen_tl(sges2)
!     b3_tl       - pen_tl(sges3)-pen_tl(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: dwhead,dwptr
  use obsmod_tl, only: rdw_tl
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),intent(out):: pen_tl,b1_tl,b3_tl
  real(r_kind),dimension(latlon1n),intent(in):: ru,rv,su,sv
  real(r_kind),dimension(latlon1n),intent(in):: ru_tl,rv_tl,su_tl,sv_tl
  real(r_kind),intent(in):: sges1,sges2,sges3
  real(r_kind),intent(in):: sges1_tl,sges2_tl,sges3_tl

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) valdw,facdw,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) valdw_tl,facdw_tl
  real(r_kind) cg_dw,dw1,dw2,dw3,pen1,pen2,pen3,pencur,wgross,wnotgross
  real(r_kind) dw1_tl,dw2_tl,dw3_tl,pen1_tl,pen2_tl,pen3_tl,pencur_tl
  real(r_kind) term,term1,term2,term3
  real(r_kind) term_tl,term1_tl,term2_tl,term3_tl
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3
  real(r_kind) exp_arg_tl,exp_arg1_tl,exp_arg2_tl,exp_arg3_tl
  real(r_kind) temp

  pen=zero
  b1=zero; b3=zero
  pen_tl=zero
  b1_tl=zero; b3_tl=zero

  dwptr => dwhead
  i=0
  do while (associated(dwptr))
    i=i+1
    if(dwptr%luse)then
     j1=dwptr%ij(1)
     j2=dwptr%ij(2)
     j3=dwptr%ij(3)
     j4=dwptr%ij(4)
     j5=dwptr%ij(5)
     j6=dwptr%ij(6)
     j7=dwptr%ij(7)
     j8=dwptr%ij(8)
     w1=dwptr%wij(1)
     w2=dwptr%wij(2)
     w3=dwptr%wij(3)
     w4=dwptr%wij(4)
     w5=dwptr%wij(5)
     w6=dwptr%wij(6)
     w7=dwptr%wij(7)
     w8=dwptr%wij(8)

     valdw=(w1*ru(j1)+w2*ru(j2)+&
            w3*ru(j3)+w4*ru(j4)+&
            w5*ru(j5)+w6*ru(j6)+&
            w7*ru(j7)+w8*ru(j8))*dwptr%sinazm+&
           (w1*rv(j1)+w2*rv(j2)+&
            w3*rv(j3)+w4*rv(j4)+&
            w5*rv(j5)+w6*rv(j6)+&
            w7*rv(j7)+w8*rv(j8))*dwptr%cosazm

     facdw=(w1*su(j1)+w2*su(j2)+&
            w3*su(j3)+w4*su(j4)+&
            w5*su(j5)+w6*su(j6)+&
            w7*su(j7)+w8*su(j8))*dwptr%sinazm+&
           (w1*sv(j1)+w2*sv(j2)+&
            w3*sv(j3)+w4*sv(j4)+&
            w5*sv(j5)+w6*sv(j6)+&
            w7*sv(j7)+w8*sv(j8))*dwptr%cosazm&
           -dwptr%res

     valdw_tl=(w1*ru_tl(j1)+w2*ru_tl(j2)+&
               w3*ru_tl(j3)+w4*ru_tl(j4)+&
               w5*ru_tl(j5)+w6*ru_tl(j6)+&
               w7*ru_tl(j7)+w8*ru_tl(j8))*dwptr%sinazm+&
              (w1*rv_tl(j1)+w2*rv_tl(j2)+&
               w3*rv_tl(j3)+w4*rv_tl(j4)+&
               w5*rv_tl(j5)+w6*rv_tl(j6)+&
               w7*rv_tl(j7)+w8*rv_tl(j8))*dwptr%cosazm

     facdw_tl=(w1*su_tl(j1)+w2*su_tl(j2)+&
               w3*su_tl(j3)+w4*su_tl(j4)+&
               w5*su_tl(j5)+w6*su_tl(j6)+&
               w7*su_tl(j7)+w8*su_tl(j8))*dwptr%sinazm+&
              (w1*sv_tl(j1)+w2*sv_tl(j2)+&
               w3*sv_tl(j3)+w4*sv_tl(j4)+&
               w5*sv_tl(j5)+w6*sv_tl(j6)+&
               w7*sv_tl(j7)+w8*sv_tl(j8))*dwptr%cosazm&
              -rdw_tl(i)

     dw1=facdw+sges1*valdw
     dw2=facdw+sges2*valdw
     dw3=facdw+sges3*valdw
     dw1_tl=facdw_tl+sges1_tl*valdw+sges1*valdw_tl
     dw2_tl=facdw_tl+sges2_tl*valdw+sges2*valdw_tl
     dw3_tl=facdw_tl+sges3_tl*valdw+sges3*valdw_tl

     exp_arg  = -half*facdw*facdw*dwptr%err2
     exp_arg1 = -half*dw1*dw1*dwptr%err2
     exp_arg2 = -half*dw2*dw2*dwptr%err2
     exp_arg3 = -half*dw3*dw3*dwptr%err2
     exp_arg_tl  = -facdw*facdw_tl*dwptr%err2
     exp_arg1_tl = -dw1*dw1_tl*dwptr%err2
     exp_arg2_tl = -dw2*dw2_tl*dwptr%err2
     exp_arg3_tl = -dw3*dw3_tl*dwptr%err2

     if (nlnqc_iter .and. dwptr%pg > tiny_r_kind) then
        cg_dw=cg_term/dwptr%b
        wnotgross= one-dwptr%pg
        wgross = dwptr%pg*cg_dw
        temp    = wnotgross*exp(exp_arg)
        term_tl  = temp/(temp+wgross)*exp_arg_tl
        temp    = wnotgross*exp(exp_arg1)
        term1_tl = temp/(temp+wgross)*exp_arg1_tl
        temp    = wnotgross*exp(exp_arg2)
        term2_tl = temp/(temp+wgross)*exp_arg2_tl
        temp    = wnotgross*exp(exp_arg3)
        term3_tl = temp/(temp+wgross)*exp_arg3_tl
        term  = log(wnotgross*exp(exp_arg)  + wgross)
        term1 = log(wnotgross*exp(exp_arg1) + wgross)
        term2 = log(wnotgross*exp(exp_arg2) + wgross)
        term3 = log(wnotgross*exp(exp_arg3) + wgross)
     else
        term_tl  = exp_arg_tl
        term1_tl = exp_arg1_tl
        term2_tl = exp_arg2_tl
        term3_tl = exp_arg3_tl
        term  = exp_arg
        term1 = exp_arg1
        term2 = exp_arg2
        term3 = exp_arg3
     endif

     pencur_tl = term_tl
     pen1_tl   = term1_tl
     pen2_tl   = term2_tl
     pen3_tl   = term3_tl
     pencur = term
     pen1   = term1
     pen2   = term2
     pen3   = term3

     pen_tl = pen_tl-two*pencur_tl*dwptr%raterr2
     b1_tl  = b1_tl-two*(pen1_tl-pen2_tl)*dwptr%raterr2
     b3_tl  = b3_tl-two*(pen3_tl-pen2_tl)*dwptr%raterr2
     pen = pen-two*pencur*dwptr%raterr2
     b1  = b1-two*(pen1-pen2)*dwptr%raterr2
     b3  = b3-two*(pen3-pen2)*dwptr%raterr2
   end if
   
   dwptr => dwptr%llpoint

  end do

  return
end subroutine stpdw_tl

end module stpdwmod
