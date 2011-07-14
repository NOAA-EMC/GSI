module stppm2_5mod
  
!$$$ module documentation block
!           .      .    .                                       .
! module:   stppm2_5mod    module for stppm2_5 and its tangent linear stppm2_5_tl
!  pgrmmr:
!
! abstract: module for stppm2_5 and its tangent linear stppm2_5_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stpq and its tangent linear stpq_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stpq_tl
!   2009-08-12  lueken - update documentation
!   2010-05-13  todling - uniform interface across stp routines
!   2010-10-15  pagowski - convert for pm2_5
!
! subroutines included:
!   sub stppm2_5
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  private
  public stppm2_5
  
contains
  
  subroutine stppm2_5(pm2_5head,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppm2_5        calcuate penalty and stepsize from q
!                            with addition of nonlinear qc.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from pm2_5
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
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-05-13  todling - udpate to use gsi_bundle
!   2010-10-15  pagowski - udpate for pm2_5
!
!   input argument list:
!     pm2_5head
!     rpm2_5       - search direction for pm2_5
!     spm2_5       - analysis increment for pm2_5
!     sges     - stepsize estimates (nstep)
!     nstep    - number of stepsize estimates (== 0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep)   - contribution of penalty from pm2_5 sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind,r_quad
    use obsmod, only: pm2_5_ob_type
    use qcmod, only: nlnqc_iter,varqc_iter
    use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    implicit none
    
! declare passed variables
    type(pm2_5_ob_type),pointer             ,intent(in   ) :: pm2_5head
    integer(i_kind)                     ,intent(in   ) :: nstep
    real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
    type(gsi_bundle)                    ,intent(in   ) :: rval,sval
    real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
    
! declare local variables
    integer(i_kind) ier,istatus
    integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,kk
    real(r_kind) cg_pm2_5,val,val2,wgross,wnotgross,pm2_5_pg
    real(r_kind),dimension(max(1,nstep))::pen
    real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,qq
    real(r_kind),pointer,dimension(:):: rpm2_5,spm2_5
    type(pm2_5_ob_type), pointer :: pm2_5ptr
    
    out=zero_quad

!   If no pm2_5 data return
    if(.not. associated(pm2_5head))return
    
    ier=0
    call gsi_bundlegetpointer(sval,'pm2_5',spm2_5,istatus);ier=ier+istatus
    call gsi_bundlegetpointer(rval,'pm2_5',rpm2_5,istatus);ier=ier+istatus

    if(ier/=0) return
    
    pm2_5ptr => pm2_5head
    do while (associated(pm2_5ptr))
       if(pm2_5ptr%luse)then
          if(nstep > 0)then
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
             
             
             val= w1* rpm2_5(j1)+w2* rpm2_5(j2)+w3* rpm2_5(j3)+w4* rpm2_5(j4)+ &
                  w5* rpm2_5(j5)+w6* rpm2_5(j6)+w7* rpm2_5(j7)+w8* rpm2_5(j8)
             val2=w1* spm2_5(j1)+w2* spm2_5(j2)+w3* spm2_5(j3)+w4* spm2_5(j4)+ &
                  w5* spm2_5(j5)+w6* spm2_5(j6)+w7* spm2_5(j7)+w8* spm2_5(j8)-pm2_5ptr%res

             do kk=1,nstep
                qq=val2+sges(kk)*val
                pen(kk)=qq*qq*pm2_5ptr%err2
             end do
          else
             pen(1)=pm2_5ptr%res*pm2_5ptr%res*pm2_5ptr%err2
          end if
          
!  modify penalty term if nonlinear qc
          
          if (nlnqc_iter .and. pm2_5ptr%pg > tiny_r_kind .and. &
               pm2_5ptr%b  > tiny_r_kind) then
             pm2_5_pg=pm2_5ptr%pg*varqc_iter
             cg_pm2_5=cg_term/pm2_5ptr%b
             wnotgross= one-pm2_5_pg
             wgross = pm2_5_pg*cg_pm2_5/wnotgross
             do kk=1,max(1,nstep)
                pen(kk)= -two*log((exp(-half*pen(kk))+wgross)/(one+wgross))
             end do
          endif
          
          out(1) = out(1)+pen(1)*pm2_5ptr%raterr2
          do kk=2,nstep
             out(kk) = out(kk)+(pen(kk)-pen(1))*pm2_5ptr%raterr2
          end do
       end if
       
       pm2_5ptr => pm2_5ptr%llpoint
       
    end do
    
    return
  end subroutine stppm2_5
  
end module stppm2_5mod
