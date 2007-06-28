module stprwmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stprwmod    module for stprw and its tangent linear stprw_tl
!
! abstract: module for stprw and its tangent linear stprw_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stprw and its tangent linear stprw_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC stprw,stprw_tl

contains

subroutine stprw(ru,rv,su,sv,pen,b1,b3,sges1,sges2,sges3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stprw       calculate penalty and contribution to
!                            stepsize with nonlinear qc added.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from radar winds
!
! program history log:
!   1991-02-26  derber
!   1999-11-22  yang
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stprw and stprw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!
!   input argument list:
!     ru       - search direction for u
!     rv       - search direction for v
!     su       - analysis increment for u
!     sv       - analysis increment for v
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!
!   output argument list     - output for step size calculation
!     pen      - penalty from radar winds
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: rwhead,rwptr
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
  real(r_kind) valrw,facrw,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) cg_rw,rw1,rw2,rw3,pen1,pen2,pen3,pencur,wgross,wnotgross
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc

  pen=zero
  b1=zero; b3=zero
  alpha=one/(sges2-sges1)
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges2*ccoef

  rwptr => rwhead
  do while (associated(rwptr))
    if(rwptr%luse)then
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
     valrw=(w1*ru(j1)+w2*ru(j2)+w3*ru(j3)+w4*ru(j4)+              &
            w5*ru(j5)+w6*ru(j6)+w7*ru(j7)+w8*ru(j8))*rwptr%cosazm+&
           (w1*rv(j1)+w2*rv(j2)+w3*rv(j3)+w4*rv(j4)+              &
            w5*rv(j5)+w6*rv(j6)+w7*rv(j7)+w8*rv(j8))*rwptr%sinazm
     facrw=(w1*su(j1)+w2*su(j2)+w3*su(j3)+w4*su(j4)+              &
            w5*su(j5)+w6*su(j6)+w7*su(j7)+w8*su(j8))*rwptr%cosazm+&
           (w1*sv(j1)+w2*sv(j2)+w3*sv(j3)+w4*sv(j4)+              &
            w5*sv(j5)+w6*sv(j6)+w7*sv(j7)+w8*sv(j8))*rwptr%sinazm &
            -rwptr%res
     rw1=facrw+sges1*valrw
     rw2=facrw+sges2*valrw
     rw3=facrw+sges3*valrw

     pencur = facrw*facrw*rwptr%err2
     pen1   = rw1*rw1*rwptr%err2
     pen2   = rw2*rw2*rwptr%err2
     pen3   = rw3*rw3*rwptr%err2

!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. rwptr%pg > tiny_r_kind .and.  &
                          rwptr%b  > tiny_r_kind) then
        cg_rw=cg_term/rwptr%b
        wnotgross= one-rwptr%pg
        wgross = rwptr%pg*cg_rw/wnotgross
        pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
     endif

     pen = pen+pencur*rwptr%raterr2
     cc  = (pen1+pen3-two*pen2)*rwptr%raterr2
     b1  = b1+(pen1-pen3)*rwptr%raterr2*bcoef1+cc*bcoef2
     b3  = b3+cc*ccoef
    end if

    rwptr => rwptr%llpoint

  end do
  return
end subroutine stprw


subroutine stprw_tl(ru,rv,su,sv,pen,b1,b3,sges1,sges2,sges3, &
                   ru_tl,rv_tl,su_tl,sv_tl,pen_tl,b1_tl,b3_tl,sges1_tl,sges2_tl,sges3_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stprw_tl      the tangent linear of the operator that calculates penalty 
!                             and contribution to stepsize with nonlinear qc added.
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-19
!
! abstract: the tangent linear of the operator that calculates penalty and contribution 
!           to stepsize from radar winds
!
! program history log:
!   2005-05-19  yanqiu zhu - tangent linear of stprw
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!
!   input argument list:
!     ru       - search direction for u
!     rv       - search direction for v
!     su       - analysis increment for u
!     sv       - analysis increment for v
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!     ru_tl       - tangent linear search direction for u
!     rv_tl       - tangent linear search direction for v
!     su_tl       - tangent linear analysis increment for u
!     sv_tl       - tangent linear analysis increment for v
!     sges1_tl    - tangent linear estimate step size 1
!     sges2_tl    - tangent linear estimate step size 2
!     sges3_tl    - tangent linear estimate step size 3
!
!   output argument list     - output for step size calculation
!     pen      - penalty from radar winds
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!     pen_tl      - tangent linear of penalty from radar winds
!     b1_tl       - pen_tl(sges1)-pen_tl(sges2)
!     b3_tl       - pen_tl(sges3)-pen_tl(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: rwhead,rwptr
  use obsmod_tl, only: rrw_tl
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
  real(r_kind) valrw,facrw,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) valrw_tl,facrw_tl
  real(r_kind) cg_rw,rw1,rw2,rw3,pen1,pen2,pen3,pencur,wgross,wnotgross
  real(r_kind) rw1_tl,rw2_tl,rw3_tl,pen1_tl,pen2_tl,pen3_tl,pencur_tl
  real(r_kind) term,term1,term2,term3
  real(r_kind) term_tl,term1_tl,term2_tl,term3_tl
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3
  real(r_kind) exp_arg_tl,exp_arg1_tl,exp_arg2_tl,exp_arg3_tl
  real(r_kind) temp


  pen=zero
  b1=zero; b3=zero
  pen_tl=zero
  b1_tl=zero; b3_tl=zero

  rwptr => rwhead
  i=0
  do while (associated(rwptr))
    i=i+1
    if(rwptr%luse)then
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
     valrw=(w1*ru(j1)+w2*ru(j2)+&
            w3*ru(j3)+w4*ru(j4)+&
            w5*ru(j5)+w6*ru(j6)+&
            w7*ru(j7)+w8*ru(j8))*rwptr%cosazm+&
           (w1*rv(j1)+w2*rv(j2)+&
            w3*rv(j3)+w4*rv(j4)+&
            w5*rv(j5)+w6*rv(j6)+&
            w7*rv(j7)+w8*rv(j8))*rwptr%sinazm
     facrw=(w1*su(j1)+w2*su(j2)+&
            w3*su(j3)+w4*su(j4)+&
            w5*su(j5)+w6*su(j6)+&
            w7*su(j7)+w8*su(j8))*rwptr%cosazm+&
           (w1*sv(j1)+w2*sv(j2)+&
            w3*sv(j3)+w4*sv(j4)+&
            w5*sv(j5)+w6*sv(j6)+&
            w7*sv(j7)+w8*sv(j8))*rwptr%sinazm&
            -rwptr%res

     valrw_tl=(w1*ru_tl(j1)+w2*ru_tl(j2)+&
               w3*ru_tl(j3)+w4*ru_tl(j4)+&
               w5*ru_tl(j5)+w6*ru_tl(j6)+&
               w7*ru_tl(j7)+w8*ru_tl(j8))*rwptr%cosazm+&
              (w1*rv_tl(j1)+w2*rv_tl(j2)+&
               w3*rv_tl(j3)+w4*rv_tl(j4)+&
               w5*rv_tl(j5)+w6*rv_tl(j6)+&
               w7*rv_tl(j7)+w8*rv_tl(j8))*rwptr%sinazm
     facrw_tl=(w1*su_tl(j1)+w2*su_tl(j2)+&
               w3*su_tl(j3)+w4*su_tl(j4)+&
               w5*su_tl(j5)+w6*su_tl(j6)+&
               w7*su_tl(j7)+w8*su_tl(j8))*rwptr%cosazm+&
              (w1*sv_tl(j1)+w2*sv_tl(j2)+&
               w3*sv_tl(j3)+w4*sv_tl(j4)+&
               w5*sv_tl(j5)+w6*sv_tl(j6)+&
               w7*sv_tl(j7)+w8*sv_tl(j8))*rwptr%sinazm&
               -rrw_tl(i)
 
     rw1=facrw+sges1*valrw
     rw2=facrw+sges2*valrw
     rw3=facrw+sges3*valrw
     rw1_tl=facrw_tl+sges1_tl*valrw+sges1*valrw_tl
     rw2_tl=facrw_tl+sges2_tl*valrw+sges2*valrw_tl
     rw3_tl=facrw_tl+sges3_tl*valrw+sges3*valrw_tl

     exp_arg  = -half*facrw*facrw*rwptr%err2
     exp_arg1 = -half*rw1*rw1*rwptr%err2
     exp_arg2 = -half*rw2*rw2*rwptr%err2
     exp_arg3 = -half*rw3*rw3*rwptr%err2
     exp_arg_tl  = -facrw*facrw_tl*rwptr%err2
     exp_arg1_tl = -rw1*rw1_tl*rwptr%err2
     exp_arg2_tl = -rw2*rw2_tl*rwptr%err2
     exp_arg3_tl = -rw3*rw3_tl*rwptr%err2

     if (nlnqc_iter .and. rwptr%pg > tiny_r_kind) then
        cg_rw=cg_term/rwptr%b
        wnotgross= one-rwptr%pg
        wgross = rwptr%pg*cg_rw
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
        term_tl = exp_arg_tl
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

     pen_tl = pen_tl-two*pencur_tl*rwptr%raterr2
     b1_tl  = b1_tl-two*(pen1_tl-pen2_tl)*rwptr%raterr2
     b3_tl  = b3_tl-two*(pen3_tl-pen2_tl)*rwptr%raterr2
     pen = pen-two*pencur*rwptr%raterr2
     b1  = b1-two*(pen1-pen2)*rwptr%raterr2
     b3  = b3-two*(pen3-pen2)*rwptr%raterr2
    end if
    
    rwptr => rwptr%llpoint

  end do
  return
end subroutine stprw_tl

end module stprwmod
