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
!   2008-12-02  Todling - remove stpq_tl
!

implicit none

PRIVATE
PUBLIC stpq

contains

subroutine stpq(qhead,rq,sq,out,sges)
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
!   2007-03-19  tremolet - binning of observations
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!   2007-02-15  rancic  - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-12-03  todling - changed handling of ptr%time
!
!   input argument list:
!     rq       - search direction for q
!     sq       - analysis increment for q
!     sges     - stepsize estimates (4)
!
!   output argument list:
!     out(1)   - contribution of penalty from q sges(1)
!     out(2)   - contribution of penalty from q sges(2)
!     out(3)   - contribution of penalty from q sges(3)
!     out(4)   - contribution of penalty from q sges(4)
!     out(5)   - pen(sges1)-pen(sges2)
!     out(6)   - pen(sges3)-pen(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: q_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon1n
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use jfunc, only: iter,jiter,niter_no_qc,jiterstart,l_foto,dhat_dt,xhat_dt
  implicit none

! Declare passed variables
  type(q_ob_type),pointer,intent(in):: qhead
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(latlon1n),intent(in):: rq,sq
  real(r_kind),dimension(4),intent(in):: sges

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) cg_q,pen1,pen2,pen3,pencur,q0,q1,q2,q3,val,val2,wgross,wnotgross,q_pg
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,time_q
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc
  type(q_ob_type), pointer :: qptr

  out=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

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


     val= w1* rq(j1)+w2* rq(j2)+w3* rq(j3)+w4* rq(j4)+ &
          w5* rq(j5)+w6* rq(j6)+w7* rq(j7)+w8* rq(j8)
     val2=w1* sq(j1)+w2* sq(j2)+w3* sq(j3)+w4* sq(j4)+ &
          w5* sq(j5)+w6* sq(j6)+w7* sq(j7)+w8* sq(j8)-qptr%res
     if(l_foto)then
        time_q=qptr%time*r3600
        val =val +(w1*dhat_dt%q(j1)+w2*dhat_dt%q(j2)+ &
                   w3*dhat_dt%q(j3)+w4*dhat_dt%q(j4)+ &
                   w5*dhat_dt%q(j5)+w6*dhat_dt%q(j6)+ &
                   w7*dhat_dt%q(j7)+w8*dhat_dt%q(j8))*time_q
        val2=val2+(w1*xhat_dt%q(j1)+w2*xhat_dt%q(j2)+ &
                   w3*xhat_dt%q(j3)+w4*xhat_dt%q(j4)+ &
                   w5*xhat_dt%q(j5)+w6*xhat_dt%q(j6)+ &
                   w7*xhat_dt%q(j7)+w8*xhat_dt%q(j8))*time_q
     end if
     q0=val2+sges(1)*val
     q1=val2+sges(2)*val
     q2=val2+sges(3)*val
     q3=val2+sges(4)*val

     pencur = q0*q0*qptr%err2
     pen1   = q1*q1*qptr%err2
     pen2   = q2*q2*qptr%err2
     pen3   = q3*q3*qptr%err2

!  Modify penalty term if nonlinear QC

     if (nlnqc_iter .and. qptr%pg > tiny_r_kind .and. &
                          qptr%b  > tiny_r_kind) then
        q_pg=qptr%pg*varqc_iter
        cg_q=cg_term/qptr%b
        wnotgross= one-q_pg
        wgross = q_pg*cg_q/wnotgross
        pencur = -two*log((exp(-half*pencur)+wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  )+wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  )+wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  )+wgross)/(one+wgross))
     endif
     
     out(1) = out(1)+pencur*qptr%raterr2
     out(2) = out(2)+pen1  *qptr%raterr2
     out(3) = out(3)+pen2  *qptr%raterr2
     out(4) = out(4)+pen3  *qptr%raterr2
     cc     = (pen1+pen3-two*pen2)*qptr%raterr2
     out(5) = out(5)+(pen1-pen3)*qptr%raterr2*bcoef1+cc*bcoef2
     out(6) = out(6)+cc*ccoef
    end if

    qptr => qptr%llpoint

  end do

  return
end subroutine stpq

end module stpqmod
