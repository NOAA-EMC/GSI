module stpwmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpwmod    module for stpw and its tangent linear stpw_tl
!
! abstract: module for stpw and its tangent linear stpw_tl
!
! program history log:
!   2005-05-20  Yanqiu zhu - wrap stpw and its tangent linear stpw_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC stpw,stpw_tl

contains

subroutine stpw(ru,rv,su,sv,pen,b1,b3,sges1,sges2,sges3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpw        calculate penalty and contribution to stepsize
!                            from winds, using nonlinear qc.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from winds,
!              using nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1998-02-03  derber
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpw and stpw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-07-28  derber  - modify output for b1 and b3
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
!   output argument list  
!     pen      - current penalty
!     b1       - contribution to numerator for winds
!     b3       - contribution to denomonator for winds
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: whead,wptr
  use qcmod, only: nlnqc_iter
  use constants, only: zero,one,half,two,tiny_r_kind,cg_term
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),dimension(latlon1n),intent(in):: ru,rv,su,sv
  real(r_kind),intent(in):: sges1,sges2,sges3

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) valu,facu,valv,facv,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) cg_w,pen1,pen2,pen3,pencur,u1,u2,u3,v1,v2,v3,wgross,wnotgross
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc

  pen=zero
  b1=zero; b3=zero
  alpha=one/(sges2-sges1)
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges2*ccoef
  
  wptr => whead
  do while (associated(wptr))
    if(wptr%luse)then
     j1=wptr%ij(1)
     j2=wptr%ij(2)
     j3=wptr%ij(3)
     j4=wptr%ij(4)
     j5=wptr%ij(5)
     j6=wptr%ij(6)
     j7=wptr%ij(7)
     j8=wptr%ij(8)
     w1=wptr%wij(1)
     w2=wptr%wij(2)
     w3=wptr%wij(3)
     w4=wptr%wij(4)
     w5=wptr%wij(5)
     w6=wptr%wij(6)
     w7=wptr%wij(7)
     w8=wptr%wij(8)

     valu=w1*ru(j1)+w2*ru(j2)+w3*ru(j3)+w4*ru(j4) &
         +w5*ru(j5)+w6*ru(j6)+w7*ru(j7)+w8*ru(j8)

     facu=w1*su(j1)+w2*su(j2)+w3*su(j3)+w4*su(j4) &
         +w5*su(j5)+w6*su(j6)+w7*su(j7)+w8*su(j8) - wptr%ures
     
     valv=w1*rv(j1)+w2*rv(j2)+w3*rv(j3)+w4*rv(j4) &
         +w5*rv(j5)+w6*rv(j6)+w7*rv(j7)+w8*rv(j8)

     facv=w1*sv(j1)+w2*sv(j2)+w3*sv(j3)+w4*sv(j4) &
         +w5*sv(j5)+w6*sv(j6)+w7*sv(j7)+w8*sv(j8) - wptr%vres
     
     u1=facu+sges1*valu
     u2=facu+sges2*valu
     u3=facu+sges3*valu
     v1=facv+sges1*valv
     v2=facv+sges2*valv
     v3=facv+sges3*valv

     pencur = (facu*facu+facv*facv)*wptr%err2
     pen1   = (u1*u1+v1*v1)*wptr%err2
     pen2   = (u2*u2+v2*v2)*wptr%err2
     pen3   = (u3*u3+v3*v3)*wptr%err2

!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. wptr%pg > tiny_r_kind .and.  &
                          wptr%b  > tiny_r_kind) then
        cg_w=cg_term/wptr%b
        wnotgross= one-wptr%pg
        wgross =wptr%pg*cg_w/wnotgross
        pencur = -two*log((exp(-half*pencur)+wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  )+wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  )+wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  )+wgross)/(one+wgross))
     endif

     pen = pen+pencur*wptr%raterr2
     cc  = (pen3+pen1-two*pen2)*wptr%raterr2
     b1  = b1+(pen1-pen3)*wptr%raterr2*bcoef1+cc*bcoef2
     b3  = b3+cc*ccoef
    end if

    wptr => wptr%llpoint

  end do
  return
end subroutine stpw

subroutine stpw_tl(ru,rv,su,sv,pen,b1,b3,sges1,sges2,sges3, &
                  ru_tl,rv_tl,su_tl,sv_tl,pen_tl,b1_tl,b3_tl,sges1_tl,sges2_tl,sges3_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpw_tl      the tangent linear of the operator that calculates 
!                            penalty and contribution to stepsize
!                            from winds, using nonlinear qc.
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-20
!
! abstract: the tangent linear of the operator that calculates penalty and 
!           contribution to stepsize from winds, using nonlinear qc.
!
! program history log:
!   2005-05-20  yanqiu zhu - tangent linear of stpw
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-10-21  su      - modify for variational qc
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
!   output argument list  
!     pen      - current penalty
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!     pen_tl      - current tangent linear of penalty
!     b1_tl       - pen_tl(sges1)-pen_tl(sges2)
!     b3_tl       - pen_tl(sges3)-pen_tl(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: whead,wptr
  use obsmod_tl, only: ures_tl,vres_tl
  use qcmod, only: nlnqc_iter
  use constants, only: zero,one,half,two,tiny_r_kind,cg_term
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
  real(r_kind) valu,facu,valv,facv,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) valu_tl,facu_tl,valv_tl,facv_tl
  real(r_kind) cg_w,pen1,pen2,pen3,pencur,u1,u2,u3,v1,v2,v3,wgross,wnotgross
  real(r_kind) pen1_tl,pen2_tl,pen3_tl,pencur_tl,u1_tl,u2_tl,u3_tl,v1_tl,v2_tl,v3_tl
  real(r_kind) term,term1,term2,term3
  real(r_kind) term_tl,term1_tl,term2_tl,term3_tl
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3
  real(r_kind) exp_arg_tl,exp_arg1_tl,exp_arg2_tl,exp_arg3_tl
  real(r_kind) temp

  pen=zero
  b1=zero; b3=zero
  pen_tl=zero
  b1_tl=zero; b3_tl=zero
  
  wptr => whead
  i=0
  do while (associated(wptr))
    i=i+1
    if(wptr%luse)then
     j1=wptr%ij(1)
     j2=wptr%ij(2)
     j3=wptr%ij(3)
     j4=wptr%ij(4)
     j5=wptr%ij(5)
     j6=wptr%ij(6)
     j7=wptr%ij(7)
     j8=wptr%ij(8)
     w1=wptr%wij(1)
     w2=wptr%wij(2)
     w3=wptr%wij(3)
     w4=wptr%wij(4)
     w5=wptr%wij(5)
     w6=wptr%wij(6)
     w7=wptr%wij(7)
     w8=wptr%wij(8)
     valu=w1*ru(j1)+w2*ru(j2)+w3*ru(j3)+w4*ru(j4) &
         +w5*ru(j5)+w6*ru(j6)+w7*ru(j7)+w8*ru(j8)
    
     facu=w1*su(j1)+w2*su(j2)+w3*su(j3)+w4*su(j4) &
         +w5*su(j5)+w6*su(j6)+w7*su(j7)+w8*su(j8) - wptr%ures
   
     valv=w1*rv(j1)+w2*rv(j2)+w3*rv(j3)+w4*rv(j4) &
         +w5*rv(j5)+w6*rv(j6)+w7*rv(j7)+w8*rv(j8)
    
     facv=w1*sv(j1)+w2*sv(j2)+w3*sv(j3)+w4*sv(j4) &
         +w5*sv(j5)+w6*sv(j6)+w7*sv(j7)+w8*sv(j8) - wptr%vres
    
     valu_tl=w1*ru_tl(j1)+w2*ru_tl(j2)+w3*ru_tl(j3)+w4*ru_tl(j4) &
            +w5*ru_tl(j5)+w6*ru_tl(j6)+w7*ru_tl(j7)+w8*ru_tl(j8)

     facu_tl=w1*su_tl(j1)+w2*su_tl(j2)+w3*su_tl(j3)+w4*su_tl(j4) &
            +w5*su_tl(j5)+w6*su_tl(j6)+w7*su_tl(j7)+w8*su_tl(j8) - ures_tl(i)

     valv_tl=w1*rv_tl(j1)+w2*rv_tl(j2)+w3*rv_tl(j3)+w4*rv_tl(j4) &
            +w5*rv_tl(j5)+w6*rv_tl(j6)+w7*rv_tl(j7)+w8*rv_tl(j8)

     facv_tl=w1*sv_tl(j1)+w2*sv_tl(j2)+w3*sv_tl(j3)+w4*sv_tl(j4) &
            +w5*sv_tl(j5)+w6*sv_tl(j6)+w7*sv_tl(j7)+w8*sv_tl(j8) - vres_tl(i)

     u1=facu+sges1*valu
     u2=facu+sges2*valu
     u3=facu+sges3*valu
     v1=facv+sges1*valv
     v2=facv+sges2*valv
     v3=facv+sges3*valv
     u1_tl=facu_tl+sges1_tl*valu+sges1*valu_tl
     u2_tl=facu_tl+sges2_tl*valu+sges2*valu_tl
     u3_tl=facu_tl+sges3_tl*valu+sges3*valu_tl
     v1_tl=facv_tl+sges1_tl*valv+sges1*valv_tl
     v2_tl=facv_tl+sges2_tl*valv+sges2*valv_tl
     v3_tl=facv_tl+sges3_tl*valv+sges3*valv_tl

     exp_arg  = -half*(facu**2+facv**2)*wptr%err2
     exp_arg1 = -half*(u1**2+v1**2)*wptr%err2
     exp_arg2 = -half*(u2**2+v2**2)*wptr%err2
     exp_arg3 = -half*(u3**2+v3**2)*wptr%err2
     exp_arg_tl  = -(facu*facu_tl+facv*facv_tl)*wptr%err2
     exp_arg1_tl = -(u1*u1_tl+v1*v1_tl)*wptr%err2
     exp_arg2_tl = -(u2*u2_tl+v2*v2_tl)*wptr%err2
     exp_arg3_tl = -(u3*u3_tl+v3*v3_tl)*wptr%err2

     if (nlnqc_iter .and. wptr%pg > tiny_r_kind .and. wptr%b >tiny_r_kind) then
        cg_w=cg_term/wptr%b
        wnotgross= one-wptr%pg
        wgross =wptr%pg*cg_w/wnotgross
        term_tl  = (exp(exp_arg )/(exp(exp_arg )+wgross)) * exp_arg_tl
        term1_tl = (exp(exp_arg1)/(exp(exp_arg1)+wgross)) * exp_arg1_tl
        term2_tl = (exp(exp_arg2)/(exp(exp_arg2)+wgross)) * exp_arg2_tl
        term3_tl = (exp(exp_arg3)/(exp(exp_arg3)+wgross)) * exp_arg3_tl
        term  = log((exp(exp_arg)+wgross)/(one+wgross))
        term1  = log((exp(exp_arg1)+wgross)/(one+wgross))
        term2  = log((exp(exp_arg2)+wgross)/(one+wgross))
        term3  = log((exp(exp_arg3)+wgross)/(one+wgross))
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

     pen_tl = pen_tl-two*pencur_tl*wptr%raterr2
     b1_tl  = b1_tl-two*(pen1_tl-pen2_tl)*wptr%raterr2
     b3_tl  = b3_tl-two*(pen3_tl-pen2_tl)*wptr%raterr2
     pen = pen-two*pencur*wptr%raterr2
     b1  = b1-two*(pen1-pen2)*wptr%raterr2
     b3  = b3-two*(pen3-pen2)*wptr%raterr2
    end if

    wptr => wptr%llpoint

  end do
  return
end subroutine stpw_tl

end module stpwmod
