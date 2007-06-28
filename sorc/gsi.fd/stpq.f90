module stpqmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpqmod    module for stpq and its tangent linear stpq_tl
!
! abstract: module for stpq and its tangent linear stpq_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stpq and its tangent linear stpq_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC stpq,stpq_tl

contains

subroutine stpq(rq,sq,pen,b1,b3,sges1,sges2,sges3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpq        calcuate penalty and stepsize from q
!                            with addition of nonlinear qc.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from q
!           using nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1993-08-25  wu
!   1998-02-03  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-05  parrish - add non-linear qc option
!   2005-04-11  treadon - merge stpq and stpq_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!
!   input argument list:
!     rq       - search direction for q
!     sq       - analysis increment for q
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!
!   output argument list:
!     pen    - contribution of penalty from q
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: qptr,qhead
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon1n
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  implicit none

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),dimension(latlon1n),intent(in):: rq,sq
  real(r_kind),intent(in):: sges1,sges2,sges3

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) cg_q,pen1,pen2,pen3,pencur,q1,q2,q3,val,val2,wgross,wnotgross
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc

  pen=zero
  b1=zero; b3=zero
  alpha=one/(sges2-sges1)
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges2*ccoef

  qptr => qhead
  do while (associated(qptr))
    if(qptr%luse)then
     j1=qptr%ij(1)
     j2=qptr%ij(2)
     j3=qptr%ij(3)
     j4=qptr%ij(4)
     j5=qptr%ij(5)
     j6=qptr%ij(6)
     j7=qptr%ij(7)
     j8=qptr%ij(8)
     w1=qptr%wij(1)
     w2=qptr%wij(2)
     w3=qptr%wij(3)
     w4=qptr%wij(4)
     w5=qptr%wij(5)
     w6=qptr%wij(6)
     w7=qptr%wij(7)
     w8=qptr%wij(8)

     val= w1*rq(j1)+w2*rq(j2)+w3*rq(j3)+w4*rq(j4)+ &
          w5*rq(j5)+w6*rq(j6)+w7*rq(j7)+w8*rq(j8)
     val2=w1*sq(j1)+w2*sq(j2)+w3*sq(j3)+w4*sq(j4)+ &
          w5*sq(j5)+w6*sq(j6)+w7*sq(j7)+w8*sq(j8)-qptr%res
     q1=val2+sges1*val
     q2=val2+sges2*val
     q3=val2+sges3*val

     pencur = val2*val2*qptr%err2
     pen1   = q1*q1*qptr%err2
     pen2   = q2*q2*qptr%err2
     pen3   = q3*q3*qptr%err2

!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. qptr%pg > tiny_r_kind .and. &
                          qptr%b  > tiny_r_kind) then
        cg_q=cg_term/qptr%b
        wnotgross= one-qptr%pg
        wgross = qptr%pg*cg_q/wnotgross
        pencur = -two*log((exp(-half*pencur)+wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  )+wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  )+wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  )+wgross)/(one+wgross))
     endif
     
     pen = pen+pencur*qptr%raterr2
     cc  = (pen1+pen3-two*pen2)*qptr%raterr2
     b1  = b1+(pen1-pen3)*qptr%raterr2*bcoef1+cc*bcoef2
     b3  = b3+cc*ccoef
    end if

    qptr => qptr%llpoint

  end do

  return
end subroutine stpq

subroutine stpq_tl(rq,sq,pen,b1,b3,sges1,sges2,sges3, &
                  rq_tl,sq_tl,pen_tl,b1_tl,b3_tl,sges1_tl,sges2_tl,sges3_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpq_tl      the tangent linear of the operator that calcuates penalty 
!                            and stepsize from q with addition of nonlinear qc.
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-19
!
! abstract: the tangent linear of the operator that calculates penalty and contribution 
!           to stepsize from q using nonlinear qc.
!
! program history log:
!   2005-05-19  yanqiu zhu - tangent linear of stpq
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-10-21  su      - modify for variational qc
!
!   input argument list:
!     rq       - search direction for q
!     sq       - analysis increment for q
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!     rq_tl       - tangent linear search direction for q
!     sq_tl       - tangent linear analysis increment for q
!     sges1_tl    - tangent linear estimate step size 1
!     sges2_tl    - tangent linear estimate step size 2
!     sges3_tl    - tangent linear estimate step size 3
!
!   output argument list:
!     pen    - contribution of penalty from q
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!     pen_tl    - tangent linear of the contribution of penalty from q
!     b1_tl       - pen_tl(sges1)-pen_tl(sges2)
!     b3_tl       - pen_tl(sges3)-pen_tl(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: qhead,qptr
  use obsmod_tl, only: qdataerr_tl
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon1n
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  implicit none

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),intent(out):: pen_tl,b1_tl,b3_tl
  real(r_kind),dimension(latlon1n),intent(in):: rq,sq
  real(r_kind),dimension(latlon1n),intent(in):: rq_tl,sq_tl
  real(r_kind),intent(in):: sges1,sges2,sges3
  real(r_kind),intent(in):: sges1_tl,sges2_tl,sges3_tl

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) cg_q,pen1,pen2,pen3,pencur,q1,q2,q3,val,val2,wgross,wnotgross
  real(r_kind) pen1_tl,pen2_tl,pen3_tl,pencur_tl,q1_tl,q2_tl,q3_tl,val_tl,val2_tl
  real(r_kind) term,term1,term2,term3
  real(r_kind) term_tl,term1_tl,term2_tl,term3_tl
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3
  real(r_kind) exp_arg_tl,exp_arg1_tl,exp_arg2_tl,exp_arg3_tl
  real(r_kind) temp,w1,w2,w3,w4,w5,w6,w7,w8

  pen=zero
  b1=zero; b3=zero
  pen_tl=zero
  b1_tl=zero; b3_tl=zero

  qptr => qhead
  i=0
  do while (associated(qptr))
    i=i+1
    if(qptr%luse)then
     j1=qptr%ij(1)
     j2=qptr%ij(2)
     j3=qptr%ij(3)
     j4=qptr%ij(4)
     j5=qptr%ij(5)
     j6=qptr%ij(6)
     j7=qptr%ij(7)
     j8=qptr%ij(8)
     w1=qptr%wij(1)
     w2=qptr%wij(2)
     w3=qptr%wij(3)
     w4=qptr%wij(4)
     w5=qptr%wij(5)
     w6=qptr%wij(6)
     w7=qptr%wij(7)
     w8=qptr%wij(8)
     val =w1*rq(j1)+w2*rq(j2)+w3*rq(j3)+w4*rq(j4)+ &
          w5*rq(j5)+w6*rq(j6)+w7*rq(j7)+w8*rq(j8)
     val2=w1*sq(j1)+w2*sq(j2)+w3*sq(j3)+w4*sq(j4)+ &
          w5*sq(j5)+w6*sq(j6)+w7*sq(j7)+w8*sq(j8)-qptr%res
     q1=val2+sges1*val
     q2=val2+sges2*val
     q3=val2+sges3*val
     val_tl=w1*rq_tl(j1)+w2*rq_tl(j2)+w3*rq_tl(j3)&
           +w4*rq_tl(j4)+w5*rq_tl(j5)+w6*rq_tl(j6)&
           +w7*rq_tl(j7)+w8*rq_tl(j8)
     val2_tl=w1*sq_tl(j1)+w2*sq_tl(j2)+w3*sq_tl(j3)&
            +w4*sq_tl(j4)+w5*sq_tl(j5)+w6*sq_tl(j6)&
            +w7*sq_tl(j7)+w8*sq_tl(j8)-qdataerr_tl(i)
     q1_tl=val2_tl+sges1_tl*val+sges1*val_tl
     q2_tl=val2_tl+sges2_tl*val+sges2*val_tl
     q3_tl=val2_tl+sges3_tl*val+sges3*val_tl

     exp_arg  = -half*val2*val2*qptr%err2
     exp_arg1 = -half*q1*q1*qptr%err2
     exp_arg2 = -half*q2*q2*qptr%err2
     exp_arg3 = -half*q3*q3*qptr%err2
     exp_arg_tl  = -val2*val2_tl*qptr%err2
     exp_arg1_tl = -q1*q1_tl*qptr%err2
     exp_arg2_tl = -q2*q2_tl*qptr%err2
     exp_arg3_tl = -q3*q3_tl*qptr%err2

     if (nlnqc_iter .and. qptr%pg > tiny_r_kind .and.  &
                          qptr%b  > tiny_r_kind) then
        cg_q=cg_term/qptr%b
        wnotgross= one-qptr%pg
        wgross =qptr%pg*cg_q/wnotgross
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

     pen_tl = pen_tl-two*pencur_tl*qptr%raterr2
     b1_tl  = b1_tl-two*(pen1_tl-pen2_tl)*qptr%raterr2
     b3_tl  = b3_tl-two*(pen3_tl-pen2_tl)*qptr%raterr2
     pen = pen-two*pencur*qptr%raterr2
     b1  = b1-two*(pen1-pen2)*qptr%raterr2
     b3  = b3-two*(pen3-pen2)*qptr%raterr2
    end if

    qptr => qptr%llpoint

  end do

  return
end subroutine stpq_tl

end module stpqmod
