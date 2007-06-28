module stptmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stptmod    module for stpt and its tangent linear stpt_tl
!
! abstract: module for stpt and its tangent linear stpt_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stpt and its tangent linear stpt_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC stpt,stpt_tl

contains

subroutine stpt(rt,st,rtv,stv,rq,sq,ru,su,rv,sv,rp,sp,rsst,ssst, &
                pen,b1,b3,sges1,sges2,sges3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpt        calculate penalty and contribution to stepsize
!                            from temperatures, using non-linear qc
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from temperatures,
!              using nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1998-02-03  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-05  parrish - add non-linear qc option
!   2005-04-11  treadon - merge stpt and stpt_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2005-12-20  parrish - add code to enable boundary layer forward model option
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-07-28  derber  - modify output for b1 and b3 and add sensible temperature
!
!   input argument list:
!     rt       - search direction for sensible t
!     st       - analysis increment for sensible t
!     rtv      - search direction for virtual t
!     stv      - analysis increment for virtual t
!     rq       - search direction for q
!     sq       - analysis increment for q
!     ru       - search direction for u
!     su       - analysis increment for u
!     rv       - search direction for v
!     sv       - analysis increment for v
!     rp       - search direction for p
!     sp       - analysis increment for p
!     rsst     - search direction for sst
!     ssst     - analysis increment for sst
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!                                         
!   output argument list:         
!     pen      - penalty from temperature observations
!     b1       - contribution to numerator for temperature data
!     b3       - contribution to denomonator for temperature data
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: thead,tptr
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use gridmod, only: latlon1n,latlon11
  implicit none

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),dimension(latlon1n),intent(in):: rt,st,rtv,stv,rq,sq,ru,su,rv,sv
  real(r_kind),dimension(latlon11),intent(in):: rsst,ssst,rp,sp
  real(r_kind),intent(in):: sges1,sges2,sges3

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) cg_t,pen1,pen2,pen3,pencur,t1,t2,t3,val,val2,wgross,wnotgross
  real(r_kind) tg_prime0,tg_prime1,tg_prime2,tg_prime3
  real(r_kind) ts_prime0,ts_prime1,ts_prime2,ts_prime3
  real(r_kind) qs_prime0,qs_prime1,qs_prime2,qs_prime3
  real(r_kind) us_prime0,us_prime1,us_prime2,us_prime3
  real(r_kind) vs_prime0,vs_prime1,vs_prime2,vs_prime3
  real(r_kind) psfc_prime0,psfc_prime1,psfc_prime2,psfc_prime3
  real(r_kind) t0
  real(r_kind) t2_prime0,t2_prime1,t2_prime2,t2_prime3
  real(r_kind) q2_prime0,q2_prime1,q2_prime2,q2_prime3
  real(r_kind) u10_prime0,u10_prime1,u10_prime2,u10_prime3
  real(r_kind) v10_prime0,v10_prime1,v10_prime2,v10_prime3
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc

  pen=zero
  b1=zero; b3=zero
  alpha=one/(sges2-sges1)
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges2*ccoef
  tptr => thead
  do while (associated(tptr))

    if(tptr%luse)then
      j1=tptr%ij(1)
      j2=tptr%ij(2)
      j3=tptr%ij(3)
      j4=tptr%ij(4)
      j5=tptr%ij(5)
      j6=tptr%ij(6)
      j7=tptr%ij(7)
      j8=tptr%ij(8)
      w1=tptr%wij(1)
      w2=tptr%wij(2)
      w3=tptr%wij(3)
      w4=tptr%wij(4)
      w5=tptr%wij(5)
      w6=tptr%wij(6)
      w7=tptr%wij(7)
      w8=tptr%wij(8)

      if(tptr%tv_ob)then
        val= w1*rtv(j1)+w2*rtv(j2)+w3*rtv(j3)+w4*rtv(j4)+ &
             w5*rtv(j5)+w6*rtv(j6)+w7*rtv(j7)+w8*rtv(j8)
        val2=w1*stv(j1)+w2*stv(j2)+w3*stv(j3)+w4*stv(j4)+ &
             w5*stv(j5)+w6*stv(j6)+w7*stv(j7)+w8*stv(j8)
      else
        val= w1*rt(j1)+w2*rt(j2)+w3*rt(j3)+w4*rt(j4)+ &
             w5*rt(j5)+w6*rt(j6)+w7*rt(j7)+w8*rt(j8)
        val2=w1*st(j1)+w2*st(j2)+w3*st(j3)+w4*st(j4)+ &
             w5*st(j5)+w6*st(j6)+w7*st(j7)+w8*st(j8)
      end if
      t0=val2
      t1=val2+sges1*val
      t2=val2+sges2*val
      t3=val2+sges3*val

      if(tptr%use_sfc_model) then

        ts_prime0=t0
        ts_prime1=t1
        ts_prime2=t2
        ts_prime3=t3

        val =w1*rsst(j1)+w2*rsst(j2)+w3*rsst(j3)+w4*rsst(j4)
        val2=w1*ssst(j1)+w2*ssst(j2)+w3*ssst(j3)+w4*ssst(j4)
        tg_prime0=val2
        tg_prime1=val2+sges1*val
        tg_prime2=val2+sges2*val
        tg_prime3=val2+sges3*val

        val =w1*rq(j1)+w2*rq(j2)+w3*rq(j3)+w4*rq(j4)
        val2=w1*sq(j1)+w2*sq(j2)+w3*sq(j3)+w4*sq(j4)
        qs_prime0=val2
        qs_prime1=val2+sges1*val
        qs_prime2=val2+sges2*val
        qs_prime3=val2+sges3*val

        val =w1*ru(j1)+w2*ru(j2)+w3*ru(j3)+w4*ru(j4)
        val2=w1*su(j1)+w2*su(j2)+w3*su(j3)+w4*su(j4)
        us_prime0=val2
        us_prime1=val2+sges1*val
        us_prime2=val2+sges2*val
        us_prime3=val2+sges3*val

        val =w1*rv(j1)+w2*rv(j2)+w3*rv(j3)+w4*rv(j4)
        val2=w1*sv(j1)+w2*sv(j2)+w3*sv(j3)+w4*sv(j4)
        vs_prime0=val2
        vs_prime1=val2+sges1*val
        vs_prime2=val2+sges2*val
        vs_prime3=val2+sges3*val

        val =w1*rp(j1)+w2*rp(j2)+w3*rp(j3)+w4*rp(j4)
        val2=w1*sp(j1)+w2*sp(j2)+w3*sp(j3)+w4*sp(j4)
        psfc_prime0=val2
        psfc_prime1=val2+sges1*val
        psfc_prime2=val2+sges2*val
        psfc_prime3=val2+sges3*val

        t0=psfc_prime0*tptr%tlm_tsfc(1) + tg_prime0*tptr%tlm_tsfc(2) + &
           ts_prime0  *tptr%tlm_tsfc(3) + qs_prime0*tptr%tlm_tsfc(4) + &
           us_prime0  *tptr%tlm_tsfc(5) + vs_prime0*tptr%tlm_tsfc(6)
        t1=psfc_prime1*tptr%tlm_tsfc(1) + tg_prime1*tptr%tlm_tsfc(2) + &
           ts_prime1  *tptr%tlm_tsfc(3) + qs_prime1*tptr%tlm_tsfc(4) + &
           us_prime1  *tptr%tlm_tsfc(5) + vs_prime1*tptr%tlm_tsfc(6)
        t2=psfc_prime2*tptr%tlm_tsfc(1) + tg_prime2*tptr%tlm_tsfc(2) + &
           ts_prime2  *tptr%tlm_tsfc(3) + qs_prime2*tptr%tlm_tsfc(4) + &
           us_prime2  *tptr%tlm_tsfc(5) + vs_prime2*tptr%tlm_tsfc(6)
        t3=psfc_prime3*tptr%tlm_tsfc(1) + tg_prime3*tptr%tlm_tsfc(2) + &
           ts_prime3  *tptr%tlm_tsfc(3) + qs_prime3*tptr%tlm_tsfc(4) + &
           us_prime3  *tptr%tlm_tsfc(5) + vs_prime3*tptr%tlm_tsfc(6)

      end if

      t0=t0 - tptr%res
      t1=t1 - tptr%res
      t2=t2 - tptr%res
      t3=t3 - tptr%res

      pencur = t0*t0*tptr%err2
      pen1   = t1*t1*tptr%err2
      pen2   = t2*t2*tptr%err2
      pen3   = t3*t3*tptr%err2

!  Modify penalty term if nonlinear QC
      if (nlnqc_iter .and. tptr%pg > tiny_r_kind .and. tptr%b >tiny_r_kind) then
        cg_t=cg_term/tptr%b
        wnotgross= one-tptr%pg
        wgross =tptr%pg*cg_t/wnotgross
        pencur = -two*log((exp(-half*pencur)+wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  )+wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  )+wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  )+wgross)/(one+wgross))
      endif
!  note:  if wgross=0 (no gross error, then wnotgross=1 and this all reduces to the linear case (no qc)

      pen = pen+pencur*tptr%raterr2
      cc  = (pen1+pen3-two*pen2)*tptr%raterr2
      b1  = b1+(pen1-pen3)*tptr%raterr2*bcoef1+cc*bcoef2
      b3  = b3+cc*ccoef
    end if

    tptr => tptr%llpoint

  end do
  return
end subroutine stpt

subroutine stpt_tl(rt,st,pen,b1,b3,sges1,sges2,sges3, &
                  rt_tl,st_tl,pen_tl,b1_tl,b3_tl,sges1_tl,sges2_tl,sges3_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpt_tl      the tangent linear of the operator that calculates penalty 
!                            and contribution to stepsize from temperatures, using non-linear qc
!   prgmmr: yanqiu zhu           org: GMAO                date: 2005-05-19
!
! abstract:  the tangent linear of the operator that calculates penalty and contribution 
!            to stepsize from temperatures, using nonlinear qc.
!
! program history log:
!   2005-05-19  yanqiu zhu - tangent linear of stpt
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-10-21  su      - modify for variational qc
!
!   input argument list:
!     rt       - search direction for t
!     st       - analysis increment for t
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!     rt_tl       - tangent linear search direction for t
!     st_tl       - tangent linear analysis increment for t
!     sges1_tl    - tangent linear estimate step size 1
!     sges2_tl    - tangent linear estimate step size 2
!     sges3_tl    - tangent linear estimate step size 3
!                                         
!   output argument list:         
!     pen      - penalty from temperature observations
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!     pen_tl      - tangent linear of penalty from temperature observations
!     b1_tl       - pen_tl(sges1)-pen_tl(sges2)
!     b3_tl       - pen_tl(sges3)-pen_tl(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: thead,tptr
  use obsmod_tl, only: tdataerr_tl
  use qcmod, only: nlnqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),intent(out):: pen_tl,b1_tl,b3_tl
  real(r_kind),dimension(latlon1n),intent(in):: rt,st
  real(r_kind),dimension(latlon1n),intent(in):: rt_tl,st_tl
  real(r_kind),intent(in):: sges1,sges2,sges3
  real(r_kind),intent(in):: sges1_tl,sges2_tl,sges3_tl

! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) cg_t,pen1,pen2,pen3,pencur,t1,t2,t3,val,val2,wgross,wnotgross
  real(r_kind) pen1_tl,pen2_tl,pen3_tl,pencur_tl,t1_tl,t2_tl,t3_tl,val_tl,val2_tl
  real(r_kind) term,term1,term2,term3
  real(r_kind) term_tl,term1_tl,term2_tl,term3_tl
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3
  real(r_kind) exp_arg_tl,exp_arg1_tl,exp_arg2_tl,exp_arg3_tl
  real(r_kind) temp

  pen=zero
  b1=zero; b3=zero
  pen_tl=zero
  b1_tl=zero; b3_tl=zero


  tptr => thead
  do while (associated(tptr))
    if(tptr%luse)then
     j1=tptr%ij(1)
     j2=tptr%ij(2)
     j3=tptr%ij(3)
     j4=tptr%ij(4)
     j5=tptr%ij(5)
     j6=tptr%ij(6)
     j7=tptr%ij(7)
     j8=tptr%ij(8)
     w1=tptr%wij(1)
     w2=tptr%wij(2)
     w3=tptr%wij(3)
     w4=tptr%wij(4)
     w5=tptr%wij(5)
     w6=tptr%wij(6)
     w7=tptr%wij(7)
     w8=tptr%wij(8)

     val=w1*rt(j1)+w2*rt(j2)+w3*rt(j3)+w4*rt(j4)+ &
         w5*rt(j5)+w6*rt(j6)+w7*rt(j7)+w8*rt(j8)
     val2=w1*st(j1)+w2*st(j2)+w3*st(j3)+w4*st(j4)+ &
          w5*st(j5)+w6*st(j6)+w7*st(j7)+w8*st(j8) -tptr%res
     t1=val2+sges1*val
     t2=val2+sges2*val
     t3=val2+sges3*val
     val_tl=w1*rt_tl(j1)+w2*rt_tl(j2)+w3*rt_tl(j3)&
           +w4*rt_tl(j4)+w5*rt_tl(j5)+w6*rt_tl(j6)&
           +w7*rt_tl(j7)+w8*rt_tl(j8)
     val2_tl=w1*st_tl(j1)+w2*st_tl(j2)+w3*st_tl(j3)&
            +w4*st_tl(j4)+w5*st_tl(j5)+w6*st_tl(j6)&
            +w7*st_tl(j7)+w8*st_tl(j8) -tdataerr_tl(i)
     t1_tl=val2_tl+sges1_tl*val+sges1*val_tl
     t2_tl=val2_tl+sges2_tl*val+sges2*val_tl
     t3_tl=val2_tl+sges3_tl*val+sges3*val_tl

     exp_arg  = -half*val2*val2*tptr%err2
     exp_arg1 = -half*t1*t1*tptr%err2
     exp_arg2 = -half*t2*t2*tptr%err2
     exp_arg3 = -half*t3*t3*tptr%err2
     exp_arg_tl  = -val2*val2_tl*tptr%err2
     exp_arg1_tl = -t1*t1_tl*tptr%err2
     exp_arg2_tl = -t2*t2_tl*tptr%err2
     exp_arg3_tl = -t3*t3_tl*tptr%err2

     if (nlnqc_iter .and. tptr%pg > tiny_r_kind .and. tptr%b >tiny_r_kind) then
        cg_t=cg_term/tptr%b
        wnotgross= one-tptr%pg
        wgross =tptr%pg*cg_t/wnotgross
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
     
     pen_tl = pen_tl-two*pencur_tl*tptr%raterr2
     b1_tl  = b1_tl-two*(pen1_tl-pen2_tl)*tptr%raterr2
     b3_tl  = b3_tl-two*(pen3_tl-pen2_tl)*tptr%raterr2
     pen = pen-two*pencur*tptr%raterr2
     b1  = b1-two*(pen1-pen2)*tptr%raterr2
     b3  = b3-two*(pen3-pen2)*tptr%raterr2
!  note:  if wgross=0 (no gross error, then wnotgross=1 and this all reduces to the linear case (no qc)

    end if

    tptr => tptr%llpoint

  end do
  return
end subroutine stpt_tl

end module stptmod
