module stpspdmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpspdmod    module for stpspd and its tangent linear stpspd_tl
!
! abstract: module for stpspd and its tangent linear stpspd_tl
!
! program history log:
!   2005-05-20  Yanqiu zhu - wrap stpspd and its tangent linear stpspd_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC stpspd,stpspd_tl

contains

subroutine stpspd(ru,rv,su,sv,pen,b1,b3,sges1,sges2,sges3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpspd  calculate penalty and stepsize terms
!                for wind speed, with nonlinear qc.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and stepsize terms for wind speed
!
! program history log:
!   1991-02-26  derber
!   1998-02-03  derber
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpspd and stpspd_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-18  derber  - modify output b1 and b3
!
!   input argument list:
!     ru       - search direction for u
!     rv       - search direction for v
!     su       - analysis increment for u
!     sv       - analysis increment for v
!     sges1    - estimate step size 1 
!     sges2    - estimate step size 2 
!     sges3    - estimate step size 3 
!
!   output argument list 
!     pen      - contribution to penalty from wind speed
!     b1       - contribution to numerator from wind speed
!     b3       - contribution to denomonator from wind speed
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: spdhead,spdptr
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),intent(in):: sges1,sges2,sges3
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),dimension(latlon1n),intent(in):: ru,rv,su,sv

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
  real(r_kind) w1,w2,w3,w4
  real(r_kind) spd1,spd2,spd3,valu,valv,ucur,vcur, &
       u1,u2,u3,v1,v2,v3,spd
  real(r_kind) cg_spd,pen1,pen2,pen3,pencur,wgross,wnotgross
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc

  pen=zero
  b1=zero
  b3=zero
  alpha=one/(sges2-sges1)
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges2*ccoef

  spdptr => spdhead
  do while (associated(spdptr))

    if(spdptr%luse)then
     j1 = spdptr%ij(1)
     j2 = spdptr%ij(2)
     j3 = spdptr%ij(3)
     j4 = spdptr%ij(4)
     w1 = spdptr%wij(1)
     w2 = spdptr%wij(2)
     w3 = spdptr%wij(3)
     w4 = spdptr%wij(4)

     valu=w1*ru(j1)+w2*ru(j2)+w3*ru(j3)+w4*ru(j4)
     valv=w1*rv(j1)+w2*rv(j2)+w3*rv(j3)+w4*rv(j4)
     ucur=w1*su(j1)+w2*su(j2)+w3*su(j3)+w4*su(j4)+spdptr%uges
     vcur=w1*sv(j1)+w2*sv(j2)+w3*sv(j3)+w4*sv(j4)+spdptr%vges
     u1=ucur+sges1*valu; v1=vcur+sges1*valv
     u2=ucur+sges2*valu; v2=vcur+sges2*valv
     u3=ucur+sges3*valu; v3=vcur+sges3*valv
     spd =sqrt(ucur*ucur+vcur*vcur)-spdptr%res
     spd1=sqrt(u1*u1+v1*v1)-spdptr%res
     spd2=sqrt(u2*u2+v2*v2)-spdptr%res
     spd3=sqrt(u3*u3+v3*v3)-spdptr%res

     pencur = spd*spd*spdptr%err2
     pen1   = spd1*spd1*spdptr%err2
     pen2   = spd2*spd2*spdptr%err2
     pen3   = spd3*spd3*spdptr%err2

!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. spdptr%pg > tiny_r_kind .and. &
                          spdptr%b  > tiny_r_kind) then
        cg_spd=cg_term/spdptr%b
        wnotgross= one-spdptr%pg
        wgross = spdptr%pg*cg_spd/wnotgross
        pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
     endif

     pen = pen+pencur*spdptr%raterr2
     cc  = (pen1+pen3-two*pen2)*spdptr%raterr2
     b1  = b1+(pen1-pen3)*spdptr%raterr2*bcoef1+cc*bcoef2
     b3  = b3+cc*ccoef
    end if
    
    spdptr => spdptr%llpoint

  end do
  return
end subroutine stpspd

subroutine stpspd_tl(ru,rv,su,sv,pen,b1,b3,sges1,sges2,sges3, &
              ru_tl,rv_tl,su_tl,sv_tl,pen_tl,b1_tl,b3_tl,sges1_tl,sges2_tl,sges3_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpspd_tl  the tangent linear of the operator that calculates penalty 
!                          and stepsize terms for wind speed, with nonlinear qc.
!   prgmmr: yanqiu zhu          org: GMAO                date: 2005-05-20
!
! abstract: the tangent linear of the operator that calculates penalty and 
!           stepsize terms for wind speed
!
! program history log:
!   2005-05-20  yanqiu zhu - tangent linear of stpspd
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!
!   input argument list:
!     ru       - search direction for u
!     rv       - search direction for v
!     su       - analysis increment for u
!     sv       - analysis increment for v
!     sges1    - estimate step size 1 
!     sges2    - estimate step size 2 
!     sges3    - estimate step size 3 
!     ru_tl       - tangent linear search direction for u
!     rv_tl       - tangent linear search direction for v
!     su_tl       - tangent linear analysis increment for u
!     sv_tl       - tangent linear analysis increment for v
!     sges1_tl    - tangent linear estimate step size 1
!     sges2_tl    - tangent linear estimate step size 2
!     sges3_tl    - tangent linear estimate step size 3
!
!   output argument list 
!     pen      - contribution to penalty from wind speed
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!     pen_tl   - tangent linear of the contribution to penalty from wind speed
!     b1_tl    - pen_tl(sges1)-pen_tl(sges2)
!     b3_tl    - pen_tl(sges3)-pen_tl(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: spdhead,spdptr
  use obsmod_tl, only: spdres_tl,vsges_tl,usges_tl
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),intent(in):: sges1,sges2,sges3
  real(r_kind),intent(in):: sges1_tl,sges2_tl,sges3_tl
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),intent(out):: pen_tl,b1_tl,b3_tl
  real(r_kind),dimension(latlon1n),intent(in):: ru,rv,su,sv
  real(r_kind),dimension(latlon1n),intent(in):: ru_tl,rv_tl,su_tl,sv_tl

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4
  real(r_kind) w1,w2,w3,w4
  real(r_kind) spd1,spd2,spd3,valu,valv,ucur,vcur, &
       u1,u2,u3,v1,v2,v3,spd
  real(r_kind) spd1_tl,spd2_tl,spd3_tl,valu_tl,valv_tl,ucur_tl,vcur_tl, &
       u1_tl,u2_tl,u3_tl,v1_tl,v2_tl,v3_tl,spd_tl
  real(r_kind) cg_spd,pen1,pen2,pen3,pencur,wgross,wnotgross
  real(r_kind) pen1_tl,pen2_tl,pen3_tl,pencur_tl
  real(r_kind) term,term1,term2,term3
  real(r_kind) term_tl,term1_tl,term2_tl,term3_tl
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3
  real(r_kind) exp_arg_tl,exp_arg1_tl,exp_arg2_tl,exp_arg3_tl
  real(r_kind) temp

  pen=zero
  b1=zero
  b3=zero
  pen_tl=zero
  b1_tl=zero
  b3_tl=zero

  spdptr => spdhead
  i=0
  do while (associated(spdptr))

    i=i+1
    if(spdptr%luse)then
     j1 = spdptr%ij(1)
     j2 = spdptr%ij(2)
     j3 = spdptr%ij(3)
     j4 = spdptr%ij(4)
     w1 = spdptr%wij(1)
     w2 = spdptr%wij(2)
     w3 = spdptr%wij(3)
     w4 = spdptr%wij(4)
     valu=w1*ru(j1)+w2*ru(j2)+w3*ru(j3)+w4*ru(j4)
     valv=w1*rv(j1)+w2*rv(j2)+w3*rv(j3)+w4*rv(j4)
     ucur=w1*su(j1)+w2*su(j2)+w3*su(j3)+w4*su(j4)+spdptr%uges
     vcur=w1*sv(j1)+w2*sv(j2)+w3*sv(j3)+w4*sv(j4)+spdptr%vges
     u1=ucur+sges1*valu; v1=vcur+sges1*valv
     u2=ucur+sges2*valu; v2=vcur+sges2*valv
     u3=ucur+sges3*valu; v3=vcur+sges3*valv
     valu_tl=w1*ru_tl(j1)+w2*ru_tl(j2)+w3*ru_tl(j3)+w4*ru_tl(j4)
     valv_tl=w1*rv_tl(j1)+w2*rv_tl(j2)+w3*rv_tl(j3)+w4*rv_tl(j4)
     ucur_tl=w1*su_tl(j1)+w2*su_tl(j2)+w3*su_tl(j3)+w4*su_tl(j4)+usges_tl(i)
     vcur_tl=w1*sv_tl(j1)+w2*sv_tl(j2)+w3*sv_tl(j3)+w4*sv_tl(j4)+vsges_tl(i)
     u1_tl=ucur_tl+sges1_tl*valu+sges1*valu_tl
     v1_tl=vcur_tl+sges1_tl*valv+sges1*valv_tl
     u2_tl=ucur_tl+sges2_tl*valu+sges2*valu_tl
     v2_tl=vcur_tl+sges2_tl*valv+sges2*valv_tl
     u3_tl=ucur_tl+sges3_tl*valu+sges3*valu_tl
     v3_tl=vcur_tl+sges3_tl*valv+sges3*valv_tl

     spd =sqrt(ucur*ucur+vcur*vcur)-spdptr%res
     spd1=sqrt(u1*u1+v1*v1)-spdptr%res
     spd2=sqrt(u2*u2+v2*v2)-spdptr%res
     spd3=sqrt(u3*u3+v3*v3)-spdptr%res
     spd_tl =(ucur*ucur_tl+vcur*vcur_tl)/sqrt(ucur*ucur+vcur*vcur)-spdres_tl(i)
     spd1_tl=(u1*u1_tl+v1*v1_tl)/sqrt(u1*u1+v1*v1)-spdres_tl(i)
     spd2_tl=(u2*u2_tl+v2*v2_tl)/sqrt(u2*u2+v2*v2)-spdres_tl(i)
     spd3_tl=(u3*u3_tl+v3*v3_tl)/sqrt(u3*u3+v3*v3)-spdres_tl(i)

     exp_arg  = -half*spd*spd*spdptr%err2
     exp_arg1 = -half*spd1*spd1*spdptr%err2
     exp_arg2 = -half*spd2*spd2*spdptr%err2
     exp_arg3 = -half*spd3*spd3*spdptr%err2
     exp_arg_tl  = -spd*spd_tl*spdptr%err2
     exp_arg1_tl = -spd1*spd1_tl*spdptr%err2
     exp_arg2_tl = -spd2*spd2_tl*spdptr%err2
     exp_arg3_tl = -spd3*spd3_tl*spdptr%err2

     if (nlnqc_iter .and. spdptr%pg > tiny_r_kind) then
        cg_spd=cg_term/spdptr%b
        wnotgross= one-spdptr%pg
        wgross = spdptr%pg*cg_spd
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

     pen_tl = pen_tl-two*pencur_tl*spdptr%raterr2
     b1_tl  = b1_tl-two*(pen1_tl-pen2_tl)*spdptr%raterr2
     b3_tl  = b3_tl-two*(pen3_tl-pen2_tl)*spdptr%raterr2
     pen = pen-two*pencur*spdptr%raterr2
     b1  = b1-two*(pen1-pen2)*spdptr%raterr2
     b3  = b3-two*(pen3-pen2)*spdptr%raterr2
    end if

    spdptr => spdptr%llpoint

  end do
  return
end subroutine stpspd_tl

end module stpspdmod
