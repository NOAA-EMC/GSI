module intpm2_5mod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intpm2_5mod    module for intq and its tangent linear intq_tl
!   prgmmr:
!
! abstract: module for intq and its tangent linear intq_tl
!
! program history log:
!   2005-05-13  Yanqiu zhu - wrap intq and its tangent linear intq_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intq_tl; add interface back
!   2009-08-13  lueken - update documentation
!   2010-10-15  pagowski - use for in-situ pm2_5
!
! subroutines included:
!   sub intpm2_5_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  
  private
  public intpm2_5
  
  interface intpm2_5; module procedure &
       intpm2_5_
  end interface
  
contains
  
  subroutine intpm2_5_(pm2_5head,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intpm2_5        apply nonlin qc obs operator for q 
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
!   2010-05-13  todling  - update to use gsi_bundle; update interface 
!   2010-10-15  pagowski  - convert for in-situ pm2_5
!
!   input argument list:
!     pm2_5head    - obs type pointer to obs structure
!     spm2_5       - pm2_5 increment in grid space
!     rpm2_5
!
!   output argument list:
!     rpm2_5       - results from pm2_5 observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind
    use constants, only: half,one,tiny_r_kind,cg_term
    use obsmod, only: pm2_5_ob_type,lsaveobsens,l_do_adjoint
    use qcmod, only: nlnqc_iter,varqc_iter
    use jfunc, only: jiter
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    implicit none
    
! declare passed variables
    type(pm2_5_ob_type),pointer,intent(in   ) :: pm2_5head
    type(gsi_bundle)       ,intent(in   ) :: sval
    type(gsi_bundle)       ,intent(inout) :: rval
    
! declare local variables  
    integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,ier,istatus
    real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
! real(r_kind) penalty
    real(r_kind) cg_pm2_5,val,p0,grad,wnotgross,wgross,pm2_5_pg
    real(r_kind),pointer,dimension(:) :: spm2_5
    real(r_kind),pointer,dimension(:) :: rpm2_5
    type(pm2_5_ob_type), pointer :: pm2_5ptr
    
!   If no pm2_5 obs return
    if(.not. associated(pm2_5head))return
! retrieve pointers
! simply return if any pointer not found
    ier=0
    call gsi_bundlegetpointer(sval,'pm2_5',spm2_5,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'pm2_5',rpm2_5,istatus);ier=istatus+ier

    if(ier/=0) return
    
    pm2_5ptr => pm2_5head
    do while (associated(pm2_5ptr))
       j1=pm2_5ptr%ij(1)
       j2=pm2_5ptr%ij(2)
       j3=pm2_5ptr%ij(3)
       j4=pm2_5ptr%ij(4)
       j5=pm2_5ptr%ij(5)
       j6=pm2_5ptr%ij(6)
       j7=pm2_5ptr%ij(7)
       j8=pm2_5ptr%ij(8)
       w1=pm2_5ptr%wij(1)
       w2=pm2_5ptr%wij(2)
       w3=pm2_5ptr%wij(3)
       w4=pm2_5ptr%wij(4)
       w5=pm2_5ptr%wij(5)
       w6=pm2_5ptr%wij(6)
       w7=pm2_5ptr%wij(7)
       w8=pm2_5ptr%wij(8)

!    forward model
       val=w1* spm2_5(j1)+w2* spm2_5(j2)+w3* spm2_5(j3)+w4* spm2_5(j4)+ &
            w5* spm2_5(j5)+w6* spm2_5(j6)+w7* spm2_5(j7)+w8* spm2_5(j8)
       
       if (lsaveobsens) then
          pm2_5ptr%diags%obssen(jiter) = val*pm2_5ptr%raterr2*pm2_5ptr%err2
       else
          if (pm2_5ptr%luse) pm2_5ptr%diags%tldepart(jiter)=val
       endif
       
       if (l_do_adjoint) then
          if (lsaveobsens) then
             grad = pm2_5ptr%diags%obssen(jiter)
             
          else
             val=val-pm2_5ptr%res
             
!          gradient of nonlinear operator
             
             if (nlnqc_iter .and. pm2_5ptr%pg > tiny_r_kind .and.  &
                  pm2_5ptr%b  > tiny_r_kind) then
                pm2_5_pg=pm2_5ptr%pg*varqc_iter
                cg_pm2_5=cg_term/pm2_5ptr%b
                wnotgross= one-pm2_5_pg
                wgross =pm2_5_pg*cg_pm2_5/wnotgross              ! wgross is gama in the reference by enderson
                p0=wgross/(wgross+exp(-half*pm2_5ptr%err2*val**2))  ! p0 is p in the reference by enderson
                val=val*(one-p0)                         ! term is wqc in the referenc by enderson
             endif
             
             grad     = val*pm2_5ptr%raterr2*pm2_5ptr%err2
          endif
          
!       adjoint
          rpm2_5(j1)=rpm2_5(j1)+w1*grad
          rpm2_5(j2)=rpm2_5(j2)+w2*grad
          rpm2_5(j3)=rpm2_5(j3)+w3*grad
          rpm2_5(j4)=rpm2_5(j4)+w4*grad
          rpm2_5(j5)=rpm2_5(j5)+w5*grad
          rpm2_5(j6)=rpm2_5(j6)+w6*grad
          rpm2_5(j7)=rpm2_5(j7)+w7*grad
          rpm2_5(j8)=rpm2_5(j8)+w8*grad
          
       endif
       
       pm2_5ptr => pm2_5ptr%llpoint
       
    end do
    return
  end subroutine intpm2_5_
  
end module intpm2_5mod
