module intqmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intqmod    module for intq and its tangent linear intq_tl
!   prgmmr:
!
! abstract: module for intq and its tangent linear intq_tl
!
! program history log:
!   2005-05-13  Yanqiu zhu - wrap intq and its tangent linear intq_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intq_tl; add interface back
!   2009-08-13  lueken - update documentation
!
! subroutines included:
!   sub intq_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC intq

interface intq; module procedure &
          intq_
end interface

contains

subroutine intq_(qhead,rq,sq)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intq        apply nonlin qc obs operator for q 
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for q with
!             nonlinear qc operator
!
! program history log:
!   1991-02-26  derber
!   1993-08-15  wu
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-05  parrish - add non-linear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intq and intq_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2007-02-15  rancic - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-05-31  safford - rm unused vars
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-11-28  todling  - turn FOTO optional; changed ptr%time handle
!
!   input argument list:
!     qhead    - obs type pointer to obs structure
!     sq       - q increment in grid space
!     rq
!
!   output argument list:
!     rq       - results from q observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term,r3600
  use obsmod, only: q_ob_type,lsaveobsens,l_do_adjoint
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon1n
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
  implicit none

! Declare passed variables
  type(q_ob_type),pointer,intent(in):: qhead
  real(r_kind),dimension(latlon1n),intent(in):: sq
  real(r_kind),dimension(latlon1n),intent(inout):: rq

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,time_q
! real(r_kind) penalty
  real(r_kind) cg_q,val,p0,grad,wnotgross,wgross,q_pg
  type(q_ob_type), pointer :: qptr

  qptr => qhead
  do while (associated(qptr))
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
     
!    Forward model
     val=w1* sq(j1)+w2* sq(j2)+w3* sq(j3)+w4* sq(j4)+ &
         w5* sq(j5)+w6* sq(j6)+w7* sq(j7)+w8* sq(j8)
     if ( l_foto ) then
       time_q=qptr%time*r3600
       val=val+&
        (w1*xhat_dt%q(j1)+w2*xhat_dt%q(j2)+ &
         w3*xhat_dt%q(j3)+w4*xhat_dt%q(j4)+ &
         w5*xhat_dt%q(j5)+w6*xhat_dt%q(j6)+ &
         w7*xhat_dt%q(j7)+w8*xhat_dt%q(j8))*time_q
     endif

     if (lsaveobsens) then
       qptr%diags%obssen(jiter) = val*qptr%raterr2*qptr%err2
     else
       if (qptr%luse) qptr%diags%tldepart(jiter)=val
     endif

    if (l_do_adjoint) then
      if (lsaveobsens) then
        grad = qptr%diags%obssen(jiter)
  
      else
        val=val-qptr%res

!       gradient of nonlinear operator

        if (nlnqc_iter .and. qptr%pg > tiny_r_kind .and.  &
                             qptr%b  > tiny_r_kind) then
          q_pg=qptr%pg*varqc_iter
          cg_q=cg_term/qptr%b
          wnotgross= one-q_pg
          wgross =q_pg*cg_q/wnotgross              ! wgross is gama in the reference by Enderson
          p0=wgross/(wgross+exp(-half*qptr%err2*val**2))  ! p0 is P in the reference by Enderson
          val=val*(one-p0)                         ! term is Wqc in the referenc by Enderson
        endif

        grad     = val*qptr%raterr2*qptr%err2
      endif

!     Adjoint
      rq(j1)=rq(j1)+w1*grad
      rq(j2)=rq(j2)+w2*grad
      rq(j3)=rq(j3)+w3*grad
      rq(j4)=rq(j4)+w4*grad
      rq(j5)=rq(j5)+w5*grad
      rq(j6)=rq(j6)+w6*grad
      rq(j7)=rq(j7)+w7*grad
      rq(j8)=rq(j8)+w8*grad

      if ( l_foto ) then
        grad=grad*time_q
        dhat_dt%q(j1)=dhat_dt%q(j1)+w1*grad
        dhat_dt%q(j2)=dhat_dt%q(j2)+w2*grad
        dhat_dt%q(j3)=dhat_dt%q(j3)+w3*grad
        dhat_dt%q(j4)=dhat_dt%q(j4)+w4*grad
        dhat_dt%q(j5)=dhat_dt%q(j5)+w5*grad
        dhat_dt%q(j6)=dhat_dt%q(j6)+w6*grad
        dhat_dt%q(j7)=dhat_dt%q(j7)+w7*grad
        dhat_dt%q(j8)=dhat_dt%q(j8)+w8*grad
      endif
    endif

    qptr => qptr%llpoint

  end do
  return
end subroutine intq_

end module intqmod
