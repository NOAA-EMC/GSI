module stppsmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppsmod    module for stpps and its tangent linear stpps_tl
!
! abstract: module for stpps and its tangent linear stpps_tl
!
! program history log:
!   2005-05-18  Yanqiu zhu - wrap stpps and its tangent linear stpps_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC stpps,stpps_tl

contains

subroutine stpps(rp,sp,pen,b1,b3,sges1,sges2,sges3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpps       calculate penalty and contribution to
!                             stepsize for sfcp, using nonlinear qc
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize for
!           surface pressure with nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1997-12-14  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpps and stpps_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2006-09-18  derber  - modify to output of b1 and b3
!
!   input argument list:
!     rp       - search direction for ps
!     sp       - analysis increment for ps
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!                                         
!   output argument list:         
!     pen      - contribution to penalty for surface pressure
!     b1       - contribution to numerator for surface pressure
!     b3       - contribution to denomonator for surface pressure
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: psptr,pshead
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use gridmod, only: latlon11
  implicit none

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),dimension(latlon11),intent(in):: rp,sp
  real(r_kind),intent(in):: sges1,sges2,sges3

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
  real(r_kind) val,val2,w1,w2,w3,w4
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc
  real(r_kind) cg_ps,pen1,pen2,pen3,pencur,ps1,ps2,ps3,wgross,wnotgross

  pen=zero
  b1=zero; b3=zero
  alpha=one/(sges2-sges1)
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges2*ccoef

  psptr => pshead
  do while (associated(psptr))
    if(psptr%luse)then
     j1 = psptr%ij(1)
     j2 = psptr%ij(2)
     j3 = psptr%ij(3)
     j4 = psptr%ij(4)
     w1 = psptr%wij(1)
     w2 = psptr%wij(2)
     w3 = psptr%wij(3)
     w4 = psptr%wij(4)
     val =w1*rp(j1)+w2*rp(j2)+w3*rp(j3)+w4*rp(j4)
     val2=w1*sp(j1)+w2*sp(j2)+w3*sp(j3)+w4*sp(j4)-psptr%res

     ps1=val2+sges1*val
     ps2=val2+sges2*val
     ps3=val2+sges3*val

     pencur = val2*val2*psptr%err2
     pen1   = ps1*ps1*psptr%err2
     pen2   = ps2*ps2*psptr%err2
     pen3   = ps3*ps3*psptr%err2

!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. psptr%pg > tiny_r_kind .and.  &
                          psptr%b  > tiny_r_kind) then
        cg_ps=cg_term/psptr%b
        wnotgross= one-psptr%pg
        wgross =psptr%pg*cg_ps/wnotgross
        pencur = -two*log((exp(-half*pencur)+wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  )+wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  )+wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  )+wgross)/(one+wgross))
     endif
     
     pen = pen+pencur*psptr%raterr2
     cc  = (pen1+pen3-two*pen2)*psptr%raterr2
     b1  = b1+(pen1-pen3)*psptr%raterr2*bcoef1+cc*bcoef2
     b3  = b3+cc*ccoef
    end if

    psptr => psptr%llpoint
  end do
  
  return
end subroutine stpps


subroutine stpps_tl(rp,sp,pen,b1,b3,sges1,sges2,sges3, &
                   rp_tl,sp_tl,pen_tl,b1_tl,b3_tl,sges1_tl,sges2_tl,sges3_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpps_tl      the tangent linear of the operator that calculates 
!                             penalty and contribution to stepsize for sfcp, using 
!                             nonlinear qc
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-18
!
! abstract: the tangent linear of the operator that calculates penalty and contribution 
!           to stepsize for surface pressure with nonlinear qc.
!
! program history log:
!   2005-05-18  yanqiu zhu - tangent linear of stpps
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-10-21  su      - modify for variational qc
!
!   input argument list:
!     rp       - search direction for ps
!     sp       - analysis increment for ps
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!     rp_tl       - tangent linear search direction for ps
!     sp_tl       - tangent linear analysis increment for ps
!     sges1_tl    - tangent linear estimate step size 1
!     sges2_tl    - tangent linear estimate step size 2
!     sges3_tl    - tangent linear estimate step size 3
!                                         
!   output argument list:         
!     pen      - contribution to penalty for surface pressure
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!     pen_tl      - tangent linear  of the contribution to penalty for surface pressure
!     b1_tl       - pen_tl(sges1)-pen_tl(sges2)
!     b3_tl       - pen_tl(sges3)-pen_tl(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: psptr,pshead
  use obsmod_tl, only: presier_tl
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use gridmod, only: latlon11
  implicit none

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),intent(out):: pen_tl,b1_tl,b3_tl
  real(r_kind),dimension(latlon11),intent(in):: rp,sp
  real(r_kind),dimension(latlon11),intent(in):: rp_tl,sp_tl
  real(r_kind),intent(in):: sges1,sges2,sges3
  real(r_kind),intent(in):: sges1_tl,sges2_tl,sges3_tl

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
  real(r_kind) val,val2
  real(r_kind) val_tl,val2_tl
  real(r_kind) cg_ps,pen1,pen2,pen3,pencur,ps1,ps2,ps3,wgross,wnotgross
  real(r_kind) pen1_tl,pen2_tl,pen3_tl,pencur_tl,ps1_tl,ps2_tl,ps3_tl
  real(r_kind) term,term1,term2,term3
  real(r_kind) term_tl,term1_tl,term2_tl,term3_tl
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3
  real(r_kind) exp_arg_tl,exp_arg1_tl,exp_arg2_tl,exp_arg3_tl
  real(r_kind) temp,w1,w2,w3,w4

  pen=zero
  b1=zero; b3=zero
  pen_tl=zero
  b1_tl=zero; b3_tl=zero

  psptr => pshead
  i=0
  do while (associated(psptr))
    i=i+1
    if(psptr%luse)then
     j1 = psptr%ij(1)
     j2 = psptr%ij(2)
     j3 = psptr%ij(3)
     j4 = psptr%ij(4)
     w1 = psptr%wij(1)
     w2 = psptr%wij(2)
     w3 = psptr%wij(3)
     w4 = psptr%wij(4)
     val =w1*rp(j1)+w2*rp(j2)+w3*rp(j3)+w4*rp(j4)
     val2=w1*sp(j1)+w2*sp(j2)+w3*sp(j3)+w4*sp(j4)-psptr%res

     val_tl =w1*rp_tl(j1)+w2*rp_tl(j2)+w3*rp_tl(j3)+w4*rp_tl(j4)
     val2_tl=w1*sp_tl(j1)+w2*sp_tl(j2)+w3*sp_tl(j3)+w4*sp_tl(j4)-presier_tl(i)

     ps1_tl=val2_tl+sges1_tl*val+sges1*val_tl
     ps2_tl=val2_tl+sges2_tl*val+sges2*val_tl
     ps3_tl=val2_tl+sges3_tl*val+sges3*val_tl
     ps1=val2+sges1*val
     ps2=val2+sges2*val
     ps3=val2+sges3*val

     exp_arg  = -half*val2*val2*psptr%err2
     exp_arg1 = -half*ps1*ps1*psptr%err2
     exp_arg2 = -half*ps2*ps2*psptr%err2
     exp_arg3 = -half*ps3*ps3*psptr%err2
     exp_arg_tl  = -val2*val2_tl*psptr%err2
     exp_arg1_tl = -ps1*ps1_tl*psptr%err2
     exp_arg2_tl = -ps2*ps2_tl*psptr%err2
     exp_arg3_tl = -ps3*ps3_tl*psptr%err2

     if (nlnqc_iter .and. psptr%pg > tiny_r_kind .and.  &
                          psptr%b  > tiny_r_kind) then
        cg_ps=cg_term/psptr%b
        wnotgross= one-psptr%pg
        wgross =psptr%pg*cg_ps/wnotgross
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

     pen_tl = pen_tl-two*pencur_tl*psptr%raterr2
     b1_tl  = b1_tl-two*(pen1_tl-pen2_tl)*psptr%raterr2
     b3_tl  = b3_tl-two*(pen3_tl-pen2_tl)*psptr%raterr2
     pen = pen-two*pencur*psptr%raterr2
     b1  = b1-two*(pen1-pen2)*psptr%raterr2
     b3  = b3-two*(pen3-pen2)*psptr%raterr2
    end if

    psptr => psptr%llpoint
  end do
  
  return
end subroutine stpps_tl

end module stppsmod
