module stpsrwmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpsrwmod    module for stpsrw and its tangent linear stpsrw_tl
!
! abstract: module for stpsrw and its tangent linear stpsrw_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stpsrw and its tangent linear stpsrw_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC stpsrw,stpsrw_tl

contains

subroutine stpsrw(ru,rv,su,sv,pen,b1,b3,sges1,sges2,sges3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpsrw      apply nonlin qc op for radar superob wind
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: apply operator for radar superob wind and calculation of 
!             step size using nonlinear qc.
!
! program history log:
!   2004-06-22  parrish, document
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-09  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpsrw and stpsrw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-18  derber  - modify output values of b1 and b3
!
!   input argument list:
!     ru       - search direction for u
!     su       - analysis increment for u
!     rv       - search direction for v
!     sv       - analysis increment for v
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!
!   output argument list  
!     pen      - penalty for srw obs
!     b1       - contribution to numerator for srw obs
!     b3       - contribution to denomenator for srw obs
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: srwhead,srwptr
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),dimension(latlon1n),intent(in):: ru,rv,su,sv
  real(r_kind),intent(in):: sges1,sges2,sges3

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) valu,facu,valv,facv,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) bigu11,bigu12,bigu21,bigu22,facsrw1,facsrw2,valsrw1,valsrw2
  real(r_kind) cg_srw,pen1,pen2,pen3,pencur,u1,u2,u3,v1,v2,v3,wgross,wnotgross
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc

  pen=zero
  b1=zero; b3=zero
  alpha=one/(sges2-sges1)
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges2*ccoef

  srwptr => srwhead
  do while (associated(srwptr))
    if(srwptr%luse)then
     j1=srwptr%ij(1)
     j2=srwptr%ij(2)
     j3=srwptr%ij(3)
     j4=srwptr%ij(4)
     j5=srwptr%ij(5)
     j6=srwptr%ij(6)
     j7=srwptr%ij(7)
     j8=srwptr%ij(8)
     w1=srwptr%wij(1)
     w2=srwptr%wij(2)
     w3=srwptr%wij(3)
     w4=srwptr%wij(4)
     w5=srwptr%wij(5)
     w6=srwptr%wij(6)
     w7=srwptr%wij(7)
     w8=srwptr%wij(8)

     bigu11=srwptr%rsrw(1)
     bigu21=srwptr%rsrw(2)
     bigu12=srwptr%rsrw(3)
     bigu22=srwptr%rsrw(4)
     valu=w1*ru(j1)+w2*ru(j2)+w3*ru(j3)+w4*ru(j4) &
         +w5*ru(j5)+w6*ru(j6)+w7*ru(j7)+w8*ru(j8)
     
     valv=w1*rv(j1)+w2*rv(j2)+w3*rv(j3)+w4*rv(j4) &
         +w5*rv(j5)+w6*rv(j6)+w7*rv(j7)+w8*rv(j8)
     
     valsrw1=bigu11*valu+bigu12*valv
     valsrw2=bigu21*valu+bigu22*valv
    
     facu=w1*su(j1)+w2*su(j2)+w3*su(j3)+w4*su(j4) &
         +w5*su(j5)+w6*su(j6)+w7*su(j7)+w8*su(j8)
    
     facv=w1*sv(j1)+w2*sv(j2)+w3*sv(j3)+w4*sv(j4) &
         +w5*sv(j5)+w6*sv(j6)+w7*sv(j7)+w8*sv(j8)
     
     facsrw1=bigu11*facu+bigu12*facv-srwptr%res1
     facsrw2=bigu21*facu+bigu22*facv-srwptr%res2
     
     u1=facsrw1+sges1*valsrw1
     u2=facsrw1+sges2*valsrw1
     u3=facsrw1+sges3*valsrw1
     v1=facsrw2+sges1*valsrw2
     v2=facsrw2+sges2*valsrw2
     v3=facsrw2+sges3*valsrw2

     pencur = (facsrw1**2+facsrw2**2)*srwptr%err2
     pen1   = (u1*u1+v1*v1)*srwptr%err2
     pen2   = (u2*u2+v2*v2)*srwptr%err2
     pen3   = (u3*u3+v3*v3)*srwptr%err2

!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. srwptr%pg > tiny_r_kind .and.  &
                          srwptr%b  > tiny_r_kind) then
        cg_srw=cg_term/srwptr%b
        wnotgross= one-srwptr%pg
        wgross = srwptr%pg*cg_srw/wnotgross
        pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
     endif

     pen = pen+pencur*srwptr%raterr2
     cc  = (pen1+pen3-two*pen2)*srwptr%raterr2
     b1  = b1+(pen1-pen3)*srwptr%raterr2*bcoef1+cc*bcoef2
     b3  = b3+cc*ccoef
    end if

    srwptr => srwptr%llpoint

  end do
 return
end subroutine stpsrw


subroutine stpsrw_tl(ru,rv,su,sv,pen,b1,b3,sges1,sges2,sges3, &
                    ru_tl,rv_tl,su_tl,sv_tl,pen_tl,b1_tl,b3_tl,sges1_tl,sges2_tl,sges3_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpsrw_tl     the tangent linear of the operator that applies 
!                             nonlin qc op for radar superob wind
!   prgmmr: yanqiu zhu          org: GMAO                date: 2005-05-19
!
! abstract: the tangent linear of the operator that applies operator for radar superob 
!           wind and calculation of step size using nonlinear qc.
!
! program history log:
!   2005-05-19  yanqiu zhu - tangent linear of stpsrw
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!
!   input argument list:
!     ru       - search direction for u
!     su       - analysis increment for u
!     rv       - search direction for v
!     sv       - analysis increment for v
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!     ru_tl       - tangent linear search direction for u
!     su_tl       - tangent linear analysis increment for u
!     rv_tl       - tangent linear search direction for v
!     sv_tl       - tangent linear analysis increment for v
!     sges1_tl    - tangent linear estimate step size 1
!     sges2_tl    - tangent linear estimate step size 2
!     sges3_tl    - tangent linear estimate step size 3
!
!   output argument list  
!     pen      - penalty for srw obs
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!     pen_tl      - tangent linear  of penalty for srw obs
!     b1_tl       - pen_tl(sges1)-pen_tl(sges2)
!     b3_tl       - pen_tl(sges3)-pen_tl(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: srwhead,srwptr
  use obsmod_tl, only: srw1res_tl,srw2res_tl
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
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
  real(r_kind) bigu11,bigu12,bigu21,bigu22,facsrw1,facsrw2,valsrw1,valsrw2
  real(r_kind) facsrw1_tl,facsrw2_tl,valsrw1_tl,valsrw2_tl
  real(r_kind) cg_srw,pen1,pen2,pen3,pencur,u1,u2,u3,v1,v2,v3,wgross,wnotgross
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

  srwptr => srwhead
  i=0
  do while (associated(srwptr))
    i=i+1
    if(srwptr%luse)then
     j1=srwptr%ij(1)
     j2=srwptr%ij(2)
     j3=srwptr%ij(3)
     j4=srwptr%ij(4)
     j5=srwptr%ij(5)
     j6=srwptr%ij(6)
     j7=srwptr%ij(7)
     j8=srwptr%ij(8)
     w1=srwptr%wij(1)
     w2=srwptr%wij(2)
     w3=srwptr%wij(3)
     w4=srwptr%wij(4)
     w5=srwptr%wij(5)
     w6=srwptr%wij(6)
     w7=srwptr%wij(7)
     w8=srwptr%wij(8)
     bigu11=srwptr%rsrw(1)
     bigu21=srwptr%rsrw(2)
     bigu12=srwptr%rsrw(3)
     bigu22=srwptr%rsrw(4)
     valu=w1*ru(j1)+w2*ru(j2)+w3*ru(j3)+w4*ru(j4) &
         +w5*ru(j5)+w6*ru(j6)+w7*ru(j7)+w8*ru(j8)
     
     valv=w1*rv(j1)+w2*rv(j2)+w3*rv(j3)+w4*rv(j4) &
         +w5*rv(j5)+w6*rv(j6)+w7*rv(j7)+w8*rv(j8)
     
     valsrw1=bigu11*valu+bigu12*valv
     valsrw2=bigu21*valu+bigu22*valv

     valu_tl=w1*ru_tl(j1)+w2*ru_tl(j2)+w3*ru_tl(j3)+w4*ru_tl(j4) &
            +w5*ru_tl(j5)+w6*ru_tl(j6)+w7*ru_tl(j7)+w8*ru_tl(j8)

     valv_tl=w1*rv_tl(j1)+w2*rv_tl(j2)+w3*rv_tl(j3)+w4*rv_tl(j4) &
            +w5*rv_tl(j5)+w6*rv_tl(j6)+w7*rv_tl(j7)+w8*rv_tl(j8)

     valsrw1_tl=bigu11*valu_tl+bigu12*valv_tl
     valsrw2_tl=bigu21*valu_tl+bigu22*valv_tl
     
     facu=w1*su(j1)+w2*su(j2)+w3*su(j3)+w4*su(j4) &
         +w5*su(j5)+w6*su(j6)+w7*su(j7)+w8*su(j8)
     
     facv=w1*sv(j1)+w2*sv(j2)+w3*sv(j3)+w4*sv(j4) &
         +w5*sv(j5)+w6*sv(j6)+w7*sv(j7)+w8*sv(j8)
     
     facsrw1=bigu11*facu+bigu12*facv-srwptr%res1
     facsrw2=bigu21*facu+bigu22*facv-srwptr%res2
     
     facu_tl=w1*su_tl(j1)+w2*su_tl(j2)+w3*su_tl(j3)+w4*su_tl(j4) &
            +w5*su_tl(j5)+w6*su_tl(j6)+w7*su_tl(j7)+w8*su_tl(j8)

     facv_tl=w1*sv_tl(j1)+w2*sv_tl(j2)+w3*sv_tl(j3)+w4*sv_tl(j4) &
            +w5*sv_tl(j5)+w6*sv_tl(j6)+w7*sv_tl(j7)+w8*sv_tl(j8)

     facsrw1_tl=bigu11*facu_tl+bigu12*facv_tl-srw1res_tl(i)
     facsrw2_tl=bigu21*facu_tl+bigu22*facv_tl-srw2res_tl(i)

     u1=facsrw1+sges1*valsrw1
     u2=facsrw1+sges2*valsrw1
     u3=facsrw1+sges3*valsrw1
     v1=facsrw2+sges1*valsrw2
     v2=facsrw2+sges2*valsrw2
     v3=facsrw2+sges3*valsrw2
     u1_tl=facsrw1_tl+sges1_tl*valsrw1+sges1*valsrw1_tl
     u2_tl=facsrw1_tl+sges2_tl*valsrw1+sges2*valsrw1_tl
     u3_tl=facsrw1_tl+sges3_tl*valsrw1+sges3*valsrw1_tl
     v1_tl=facsrw2_tl+sges1_tl*valsrw2+sges1*valsrw2_tl
     v2_tl=facsrw2_tl+sges2_tl*valsrw2+sges2*valsrw2_tl
     v3_tl=facsrw2_tl+sges3_tl*valsrw2+sges3*valsrw2_tl

     exp_arg  = -half*(facsrw1**2+facsrw2**2)*srwptr%err2
     exp_arg1 = -half*(u1**2+v1**2)*srwptr%err2
     exp_arg2 = -half*(u2**2+v2**2)*srwptr%err2
     exp_arg3 = -half*(u3**2+v3**2)*srwptr%err2
     exp_arg_tl  = -(facsrw1*facsrw1_tl+facsrw2*facsrw2_tl)*srwptr%err2
     exp_arg1_tl = -(u1*u1_tl+v1*v1_tl)*srwptr%err2
     exp_arg2_tl = -(u2*u2_tl+v2*v2_tl)*srwptr%err2
     exp_arg3_tl = -(u3*u3_tl+v3*v3_tl)*srwptr%err2

     if (nlnqc_iter .and. srwptr%pg > tiny_r_kind) then
        cg_srw=cg_term/srwptr%b
        wnotgross= one-srwptr%pg
        wgross = srwptr%pg*cg_srw
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

     pencur_tl = term_tl
     pen1_tl   = term1_tl
     pen2_tl   = term2_tl
     pen3_tl   = term3_tl
     pencur = term
     pen1   = term1
     pen2   = term2
     pen3   = term3

     pen_tl = pen_tl-two*pencur_tl*srwptr%raterr2
     b1_tl  = b1_tl-two*(pen1_tl-pen2_tl)*srwptr%raterr2
     b3_tl  = b3_tl-two*(pen3_tl-pen2_tl)*srwptr%raterr2
     pen = pen-two*pencur*srwptr%raterr2
     b1  = b1-two*(pen1-pen2)*srwptr%raterr2
     b3  = b3-two*(pen3-pen2)*srwptr%raterr2
    end if

    srwptr => srwptr%llpoint

  end do
 return
end subroutine stpsrw_tl

end module stpsrwmod
