module intqmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intqmod    module for intq and its tangent linear intq_tl
!
! abstract: module for intq and its tangent linear intq_tl
!
! program history log:
!   2005-05-13  Yanqiu zhu - wrap intq and its tangent linear intq_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC intq,intq_tl


contains

subroutine intq(rq,sq)
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
!
!   input argument list:
!     sq       - increment in grid space
!
!   output argument list:
!     rq       - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,two,zero,tiny_r_kind,cg_term
  use obsmod, only: qhead,qptr
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: sq
  real(r_kind),dimension(latlon1n),intent(inout):: rq

! Declare local variables  
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
! real(r_kind) penalty
  real(r_kind) cg_q,val,p0,grad,wnotgross,wgross,term

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
     val=w1*sq(j1)+w2*sq(j2)+w3*sq(j3)+w4*sq(j4)+ &
         w5*sq(j5)+w6*sq(j6)+w7*sq(j7)+w8*sq(j8)-qptr%res

!    gradient of nonlinear operator
     if (nlnqc_iter .and. qptr%pg > tiny_r_kind .and.  &
                          qptr%b  > tiny_r_kind) then
        cg_q=cg_term/qptr%b
        wnotgross= one-qptr%pg
        wgross =qptr%pg*cg_q/wnotgross              ! wgross is gama in the reference by Enderson
        p0=wgross/(wgross+exp(-half*qptr%err2*val**2))        ! p0 is P in the reference by Enderson
        val=val*(one-p0)                            ! term is Wqc in the referenc by Enderson
     endif

     grad     = val*qptr%raterr2*qptr%err2

!    Adjoint
     rq(j1)=rq(j1)+w1*grad
     rq(j2)=rq(j2)+w2*grad
     rq(j3)=rq(j3)+w3*grad
     rq(j4)=rq(j4)+w4*grad
     rq(j5)=rq(j5)+w5*grad
     rq(j6)=rq(j6)+w6*grad
     rq(j7)=rq(j7)+w7*grad
     rq(j8)=rq(j8)+w8*grad

     qptr => qptr%llpoint

  end do
  return
end subroutine intq


subroutine intq_tl(rq,sq,rq_tl,sq_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intq_tl        the tangent linear of the operator that applies 
!                              nonlin qc obs operator for q 
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-13
!
! abstract: the tangent linear of the operator that applies observation operator 
!           and adjoint for q with nonlinear qc operator
!
! program history log:
!   2005-05-13  yanqiu zhu - tangent linear of intq
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-10-21  su  - modify for variational qc
!
!   input argument list:
!     sq       - increment in grid space
!     sq_tl     - tangent linear increment in grid space
!
!   output argument list:
!     rq       - results from observation operator (0 for no data)
!     rq_tl     - tangent linear results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,two,zero,tiny_r_kind,cg_term
  use obsmod, only: qhead,qptr
  use obsmod_tl, only: qdataerr_tl
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: sq
  real(r_kind),dimension(latlon1n),intent(in):: sq_tl
  real(r_kind),dimension(latlon1n),intent(inout):: rq
  real(r_kind),dimension(latlon1n),intent(inout):: rq_tl

! Declare local variables  
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) cg_q,val,p0,grad,wnotgross,wgross,term
  real(r_kind) val_tl,p0_tl,grad_tl,term_tl

  qptr => qhead
  i=0
  do while (associated(qptr))
     i=i+1
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
     val_tl=w1*sq_tl(j1)+w2*sq_tl(j2)+w3*sq_tl(j3)+w4*sq_tl(j4)+ &
            w5*sq_tl(j5)+w6*sq_tl(j6)+w7*sq_tl(j7)+w8*sq_tl(j8)-qdataerr_tl(i)
     val=w1*sq(j1)+w2*sq(j2)+w3*sq(j3)+w4*sq(j4)+ &
         w5*sq(j5)+w6*sq(j6)+w7*sq(j7)+w8*sq(j8)-qptr%res

!    gradient of nonlinear operator
     if (nlnqc_iter .and. qptr%pg > tiny_r_kind .and. qptr%b >tiny_r_kind) then
        cg_q=cg_term/qptr%b
        wnotgross= one-qptr%pg
        wgross =qptr%pg*cg_q/wnotgross              ! wgross is gama in the reference by Enderson
        p0=wgross/(wgross+exp(-half*qptr%err2*val**2))        ! p0 is P in the reference by Enderson
        term=one-p0                                 ! term is Wqc in the referenc by Enderson
        p0_tl = (p0*val*exp(-half*qptr%err2*val**2)*val_tl)/(wgross+exp(-half*qptr%err2*val**2))
        term_tl = -p0_tl
     else
        term = one
        term_tl = zero
     endif
     grad     = val*term
     grad_tl   = val_tl*term + val*term_tl
     grad     = grad*qptr%raterr2*qptr%err2
     grad_tl   = grad_tl*qptr%raterr2*qptr%err2

!    Adjoint
     rq_tl(j1)=rq_tl(j1)+w1*grad_tl
     rq_tl(j2)=rq_tl(j2)+w2*grad_tl
     rq_tl(j3)=rq_tl(j3)+w3*grad_tl
     rq_tl(j4)=rq_tl(j4)+w4*grad_tl
     rq_tl(j5)=rq_tl(j5)+w5*grad_tl
     rq_tl(j6)=rq_tl(j6)+w6*grad_tl
     rq_tl(j7)=rq_tl(j7)+w7*grad_tl
     rq_tl(j8)=rq_tl(j8)+w8*grad_tl
     rq(j1)=rq(j1)+w1*grad
     rq(j2)=rq(j2)+w2*grad
     rq(j3)=rq(j3)+w3*grad
     rq(j4)=rq(j4)+w4*grad
     rq(j5)=rq(j5)+w5*grad
     rq(j6)=rq(j6)+w6*grad
     rq(j7)=rq(j7)+w7*grad
     rq(j8)=rq(j8)+w8*grad

     qptr => qptr%llpoint

  end do
  return
end subroutine intq_tl

end module intqmod
