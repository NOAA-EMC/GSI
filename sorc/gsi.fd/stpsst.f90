module stpsstmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpsstmod    module for stpsst and its tangent linear stpsst_tl
!
! abstract: module for stpsst and its tangent linear stpsst_tl
!
! program history log:
!   2005-05-20  Yanqiu zhu - wrap stpsst and its tangent linear stpsst_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC stpsst,stpsst_tl

contains

subroutine stpsst(rsst,ssst,pen,b1,b3,sges1,sges2,sges3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpsst      calculate penalty and contribution to stepsize
!   prgmmr: derber           org: np23                date: 2004-07-20
!
! abstract: calculate penalty and contribution to stepsize for surface pressure
!            with addition of nonlinear qc
!
! program history log:
!   2004-07-20  derber
!   2004-07-30  treadon - add only to module use, add intent in/out
!   2004-10-09  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpsst and stpsst_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-18  derber  - modify output for b1 and b3
!   2006-12-11  li      - correct bug in alpha and add cc
!
!   input argument list:
!     rsst     - search direction for sst
!     ssst     - analysis increment for sst
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!                                         
!   output argument list:         
!     pen      - contribution to penalty for conventional sst
!     b1       - contribution to numerator for conventional sst
!     b3       - contribution to denomenator for conventional sst
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: ssthead,sstptr
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use gridmod, only: latlon11
  implicit none

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),dimension(latlon11),intent(in):: rsst,ssst
  real(r_kind),intent(in):: sges1,sges2,sges3

! Declare local variables  
  integer(i_kind) i,j1,j2,j3,j4
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) cg_sst,pen1,pen2,pen3,pencur,sst1,sst2,sst3,wgross,wnotgross
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc

  pen=zero
  b1=zero; b3=zero
  alpha=one/(sges2-sges1)
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges2*ccoef

  sstptr => ssthead
  do while (associated(sstptr))
    if(sstptr%luse)then
     j1=sstptr%ij(1)
     j2=sstptr%ij(2)
     j3=sstptr%ij(3)
     j4=sstptr%ij(4)
     w1=sstptr%wij(1)
     w2=sstptr%wij(2)
     w3=sstptr%wij(3)
     w4=sstptr%wij(4)

     val =w1*rsst(j1)+w2*rsst(j2)+w3*rsst(j3)+w4*rsst(j4)
     val2=w1*ssst(j1)+w2*ssst(j2)+w3*ssst(j3)+w4*ssst(j4)-sstptr%res

     sst1=val2+sges1*val
     sst2=val2+sges2*val
     sst3=val2+sges3*val

     pencur = val2*val2*sstptr%err2
     pen1   = sst1*sst1*sstptr%err2
     pen2   = sst2*sst2*sstptr%err2
     pen3   = sst3*sst3*sstptr%err2

!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. sstptr%pg > tiny_r_kind .and.  &
                          sstptr%b  > tiny_r_kind) then
        cg_sst=cg_term/sstptr%b
        wnotgross= one-sstptr%pg
        wgross = sstptr%pg*cg_sst/wnotgross
        pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
     endif

     pen = pen+pencur*sstptr%raterr2
     cc  = (pen1+pen3-two*pen2)*sstptr%raterr2
     b1  = b1+(pen1-pen3)*sstptr%raterr2*bcoef1+bcoef2*cc
     b3  = b3+cc*ccoef
    end if

    sstptr => sstptr%llpoint

  end do
  
  return
end subroutine stpsst

subroutine stpsst_tl(rsst,ssst,pen,b1,b3,sges1,sges2,sges3, &
                    rsst_tl,ssst_tl,pen_tl,b1_tl,b3_tl,sges1_tl,sges2_tl,sges3_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpsst_tl     the tangent linear of the operator that calculates 
!                             penalty and contribution to stepsize
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-20
!
! abstract: the tangent linear of the operator that calculates penalty and contribution 
!           to stepsize for surface pressure with addition of nonlinear qc
!
! program history log:
!   2005-05-20  yanqiu zhu - tangent linear of stpsst
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!
!   input argument list:
!     rsst     - search direction for sst
!     ssst     - analysis increment for sst
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!     rsst_tl     - tangent linear search direction for sst
!     ssst_tl     - tangent linear analysis increment for sst
!     sges1_tl    - tangent linear estimate step size 1
!     sges2_tl    - tangent linear estimate step size 2
!     sges3_tl    - tangent linear estimate step size 3
!                                         
!   output argument list:         
!     pen      - contribution to penalty for conventional sst
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!     pen_tl      - tangent linear of the contribution to penalty for conventional sst
!     b1_tl       - pen_tl(sges1)-pen_tl(sges2)
!     b3_tl       - pen_tl(sges3)-pen_tl(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: ssthead,sstptr
  use obsmod_tl, only: sstdataerr_tl
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use gridmod, only: latlon11
  implicit none

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),intent(out):: pen_tl,b1_tl,b3_tl
  real(r_kind),dimension(latlon11),intent(in):: rsst,ssst
  real(r_kind),dimension(latlon11),intent(in):: rsst_tl,ssst_tl
  real(r_kind),intent(in):: sges1,sges2,sges3
  real(r_kind),intent(in):: sges1_tl,sges2_tl,sges3_tl

! Declare local variables  
  integer(i_kind) i,j1,j2,j3,j4
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) val_tl,val2_tl
  real(r_kind) cg_sst,pen1,pen2,pen3,pencur,sst1,sst2,sst3,wgross,wnotgross
  real(r_kind) pen1_tl,pen2_tl,pen3_tl,pencur_tl,sst1_tl,sst2_tl,sst3_tl
  real(r_kind) term,term1,term2,term3
  real(r_kind) term_tl,term1_tl,term2_tl,term3_tl
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3
  real(r_kind) exp_arg_tl,exp_arg1_tl,exp_arg2_tl,exp_arg3_tl
  real(r_kind) temp

  
  pen=zero
  b1=zero; b3=zero
  pen_tl=zero
  b1_tl=zero; b3_tl=zero

  sstptr => ssthead
  i=0
  do while (associated(sstptr))
    i=i+1
    if(sstptr%luse)then
     j1=sstptr%ij(1)
     j2=sstptr%ij(2)
     j3=sstptr%ij(3)
     j4=sstptr%ij(4)
     w1=sstptr%wij(1)
     w2=sstptr%wij(2)
     w3=sstptr%wij(3)
     w4=sstptr%wij(4)
     val =w1*rsst(j1)+w2*rsst(j2)+w3*rsst(j3)+w4*rsst(j4)
     val2=w1*ssst(j1)+w2*ssst(j2)+w3*ssst(j3)+w4*ssst(j4)-sstptr%res
     val_tl =w1*rsst_tl(j1)+w2*rsst_tl(j2)&
            +w3*rsst_tl(j3)+w4*rsst_tl(j4)
     val2_tl=w1*ssst_tl(j1)+w2*ssst_tl(j2)&
            +w3*ssst_tl(j3)+w4*ssst_tl(j4)-sstdataerr_tl(i)

     sst1=val2+sges1*val
     sst2=val2+sges2*val
     sst3=val2+sges3*val
     sst1_tl=val2_tl+sges1_tl*val+sges1*val_tl
     sst2_tl=val2_tl+sges2_tl*val+sges2*val_tl
     sst3_tl=val2_tl+sges3_tl*val+sges3*val_tl

     exp_arg  = -half*val2*val2*sstptr%err2
     exp_arg1 = -half*sst1*sst1*sstptr%err2
     exp_arg2 = -half*sst2*sst2*sstptr%err2
     exp_arg3 = -half*sst3*sst3*sstptr%err2
     exp_arg_tl  = -val2*val2_tl*sstptr%err2
     exp_arg1_tl = -sst1*sst1_tl*sstptr%err2
     exp_arg2_tl = -sst2*sst2_tl*sstptr%err2
     exp_arg3_tl = -sst3*sst3_tl*sstptr%err2

     if (nlnqc_iter .and. sstptr%pg > tiny_r_kind) then
        cg_sst=cg_term/sstptr%b
        wnotgross= one-sstptr%pg
        wgross = sstptr%pg*cg_sst
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
        term_tl  = exp_arg_tl
        term1_tl = exp_arg1_tl
        term2_tl = exp_arg2_tl
        term3_tl = exp_arg3_tl
        term  = exp_arg
        term1 = exp_arg1
        term2 = exp_arg2
        term3 = exp_arg3
     endif

     pencur = term
     pen1   = term1
     pen2   = term2
     pen3   = term3
     pencur_tl = term_tl
     pen1_tl   = term1_tl
     pen2_tl   = term2_tl
     pen3_tl   = term3_tl

     pen = pen-two*pencur*sstptr%raterr2
     b1  = b1-two*(pen1-pen2)*sstptr%raterr2
     b3  = b3-two*(pen3-pen2)*sstptr%raterr2
     pen_tl = pen_tl-two*pencur_tl*sstptr%raterr2
     b1_tl  = b1_tl-two*(pen1_tl-pen2_tl)*sstptr%raterr2
     b3_tl  = b3_tl-two*(pen3_tl-pen2_tl)*sstptr%raterr2
    end if

    sstptr => sstptr%llpoint

  end do
  
  return
end subroutine stpsst_tl

end module stpsstmod
