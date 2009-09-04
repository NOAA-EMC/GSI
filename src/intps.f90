module intpsmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intpsmod    module for intps and its tangent linear intps_tl
!  prgmmr:
!
! abstract: module for intps and its tangent linear intps_tl
!
! program history log:
!   2005-05-12  Yanqiu zhu - wrap intps and its tangent linear intps_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intps_tl; add interface back
!   2009-08-13  lueken - update documentation
!
! subroutines included:
!   sub intps_
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
PUBLIC intps

interface intps; module procedure &
          intps_
end interface

contains

subroutine intps_(pshead,rp,sp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intps       apply nonlin qc obs operator for ps
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for ps observations
!           with nonlinear qc operator
!
! program history log:
!   1991-02-26  derber
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intps and intps_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2007-02-15  rancic - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-06-02  safford - rm unused vars
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-11-28  todling  - turn FOTO optional; changed ptr%time handle
!
!   input argument list:
!     pshead  - obs type pointer to obs structure
!     sp      - ps increment in grid space
!     rp
!
!   output argument list:
!     rp      - ps results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term,r3600
  use obsmod, only: ps_ob_type,lsaveobsens,l_do_adjoint
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon1n1
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
  implicit none

! Declare passed variables
  type(ps_ob_type),pointer,intent(in):: pshead
  real(r_kind),dimension(latlon1n1),intent(in):: sp
  real(r_kind),dimension(latlon1n1),intent(inout):: rp

! Declare local variables
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) cg_ps,val,p0,grad,wnotgross,wgross,ps_pg
  real(r_kind) w1,w2,w3,w4,time_ps
  type(ps_ob_type), pointer :: psptr
  psptr => pshead
  do while (associated(psptr))
     j1=psptr%ij(1)
     j2=psptr%ij(2)
     j3=psptr%ij(3)
     j4=psptr%ij(4)
     w1=psptr%wij(1)
     w2=psptr%wij(2)
     w3=psptr%wij(3)
     w4=psptr%wij(4)
     

!    Forward model
     val=w1* sp(j1)+w2* sp(j2)+w3* sp(j3)+w4* sp(j4)
     if (l_foto) then
       time_ps=psptr%time*r3600
       val=val+&
        (w1*xhat_dt%p3d(j1)+w2*xhat_dt%p3d(j2)+ &
         w3*xhat_dt%p3d(j3)+w4*xhat_dt%p3d(j4))*time_ps
     endif

     if (lsaveobsens) then
       psptr%diags%obssen(jiter) = val*psptr%raterr2*psptr%err2
     else
       if (psptr%luse) psptr%diags%tldepart(jiter)=val
     endif
  
     if (l_do_adjoint) then
       if (lsaveobsens) then
         grad = psptr%diags%obssen(jiter)
  
       else
         val=val-psptr%res

!        gradient of nonlinear operator
         if (nlnqc_iter .and. psptr%pg > tiny_r_kind .and.  &
                              psptr%b  > tiny_r_kind) then
            ps_pg=psptr%pg*varqc_iter
            cg_ps=cg_term/psptr%b                           ! b is d in Enderson
            wnotgross= one-ps_pg                            ! pg is A in Enderson
            wgross =ps_pg*cg_ps/wnotgross                   ! wgross is gama in Enderson
            p0=wgross/(wgross+exp(-half*psptr%err2*val**2)) ! p0 is P in Enderson
            val=val*(one-p0)                                ! term is Wqc in Enderson
         endif

         grad = val*psptr%raterr2*psptr%err2
       endif

!      Adjoint
       rp(j1)=rp(j1)+w1*grad
       rp(j2)=rp(j2)+w2*grad
       rp(j3)=rp(j3)+w3*grad
       rp(j4)=rp(j4)+w4*grad


       if (l_foto) then
         grad=grad*time_ps
         dhat_dt%p3d(j1)=dhat_dt%p3d(j1)+w1*grad
         dhat_dt%p3d(j2)=dhat_dt%p3d(j2)+w2*grad
         dhat_dt%p3d(j3)=dhat_dt%p3d(j3)+w3*grad
         dhat_dt%p3d(j4)=dhat_dt%p3d(j4)+w4*grad
       endif

     endif

     psptr => psptr%llpoint
  end do
  return
end subroutine intps_

end module intpsmod
