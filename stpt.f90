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
!   2008-12-02  Todling - remove stpt_tl
!

implicit none

PRIVATE
PUBLIC stpt

contains

subroutine stpt(thead,rt,st,rtv,stv,rq,sq,ru,su,rv,sv,rp,sp,rsst,ssst,out,sges)
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
!   2007-03-19  tremolet - binning of observations
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-07-28  derber  - modify output for b1 and b3 and add sensible temperature
!   2007-02-15  rancic  - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-06-02  safford - rm unused var and uses
!   2008-12-03  todling - changed handling of ptr%time
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
!     sges     - step size estimates (4)
!                                         
!   output argument list:         
!     out(1)   - penalty from temperature observations sges(1)
!     out(2)   - penalty from temperature observations sges(2)
!     out(3)   - penalty from temperature observations sges(3)
!     out(4)   - penalty from temperature observations sges(4)
!     out(5)   - contribution to numerator for temperature data
!     out(6)   - contribution to denomonator for temperature data
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: t_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use gridmod, only: latlon1n,latlon11,latlon1n1
  use jfunc, only: iter,jiter,niter_no_qc,jiterstart,l_foto,xhat_dt,dhat_dt
  implicit none

! Declare passed variables
  type(t_ob_type),pointer,intent(in):: thead
  real(r_quad),dimension(6),intent(out):: out
  real(r_kind),dimension(latlon1n),intent(in):: rt,st,rtv,stv,rq,sq,ru,su,rv,sv
  real(r_kind),dimension(latlon11),intent(in):: rsst,ssst
  real(r_kind),dimension(latlon1n1),intent(in):: rp,sp
  real(r_kind),dimension(4),intent(in):: sges

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) cg_t,pen1,pen2,pen3,pencur,t1,t2,t3,val,val2,wgross,wnotgross,t_pg
  real(r_kind) tg_prime0,tg_prime1,tg_prime2,tg_prime3
  real(r_kind) ts_prime0,ts_prime1,ts_prime2,ts_prime3
  real(r_kind) qs_prime0,qs_prime1,qs_prime2,qs_prime3
  real(r_kind) us_prime0,us_prime1,us_prime2,us_prime3
  real(r_kind) vs_prime0,vs_prime1,vs_prime2,vs_prime3
  real(r_kind) psfc_prime0,psfc_prime1,psfc_prime2,psfc_prime3
  real(r_kind) t0,time_t
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc
  type(t_ob_type), pointer :: tptr

  out=zero_quad
  alpha=one/(sges(3)-sges(2))
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges(3)*ccoef

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
!  Note time derivative stuff not consistent for virtual temperature

      if(tptr%tv_ob)then
        val= w1*rtv(j1)+w2*rtv(j2)+w3*rtv(j3)+w4*rtv(j4)+ &
             w5*rtv(j5)+w6*rtv(j6)+w7*rtv(j7)+w8*rtv(j8)

        val2=w1*stv(j1)+w2*stv(j2)+w3*stv(j3)+w4*stv(j4)+ &
             w5*stv(j5)+w6*stv(j6)+w7*stv(j7)+w8*stv(j8)
        if(l_foto)then
          time_t=tptr%time*r3600
          val =val + (w1*dhat_dt%t(j1)+w2*dhat_dt%t(j2)+ &
                      w3*dhat_dt%t(j3)+w4*dhat_dt%t(j4)+ &
                      w5*dhat_dt%t(j5)+w6*dhat_dt%t(j6)+ &
                      w7*dhat_dt%t(j7)+w8*dhat_dt%t(j8))*time_t
          val2=val2+ (w1*xhat_dt%t(j1)+w2*xhat_dt%t(j2)+ &
                      w3*xhat_dt%t(j3)+w4*xhat_dt%t(j4)+ &
                      w5*xhat_dt%t(j5)+w6*xhat_dt%t(j6)+ &
                      w7*xhat_dt%t(j7)+w8*xhat_dt%t(j8))*time_t
        end if
      else
        val= w1*    rt(j1)+w2*    rt(j2)+w3*    rt(j3)+w4*    rt(j4)+ &
             w5*    rt(j5)+w6*    rt(j6)+w7*    rt(j7)+w8*    rt(j8)
        val2=w1*    st(j1)+w2*    st(j2)+w3*    st(j3)+w4*    st(j4)+ &
             w5*    st(j5)+w6*    st(j6)+w7*    st(j7)+w8*    st(j8)
        if(l_foto)then
          val =val + (w1*dhat_dt%tsen(j1)+w2*dhat_dt%tsen(j2)+ &
                      w3*dhat_dt%tsen(j3)+w4*dhat_dt%tsen(j4)+ &
                      w5*dhat_dt%tsen(j5)+w6*dhat_dt%tsen(j6)+ &
                      w7*dhat_dt%tsen(j7)+w8*dhat_dt%tsen(j8))*time_t
          val2=val2+ (w1*xhat_dt%tsen(j1)+w2*xhat_dt%tsen(j2)+ &
                      w3*xhat_dt%tsen(j3)+w4*xhat_dt%tsen(j4)+ &
                      w5*xhat_dt%tsen(j5)+w6*xhat_dt%tsen(j6)+ &
                      w7*xhat_dt%tsen(j7)+w8*xhat_dt%tsen(j8))*time_t
        end if
      end if

      t0=val2+sges(1)*val
      t1=val2+sges(2)*val
      t2=val2+sges(3)*val
      t3=val2+sges(4)*val

      if(tptr%use_sfc_model) then

        ts_prime0=t0
        ts_prime1=t1
        ts_prime2=t2
        ts_prime3=t3

        val =w1*rsst(j1)+w2*rsst(j2)+w3*rsst(j3)+w4*rsst(j4)
        val2=w1*ssst(j1)+w2*ssst(j2)+w3*ssst(j3)+w4*ssst(j4)
        tg_prime0=val2+sges(1)*val
        tg_prime1=val2+sges(2)*val
        tg_prime2=val2+sges(3)*val
        tg_prime3=val2+sges(4)*val

        val =w1* rq(j1)+w2* rq(j2)+w3* rq(j3)+w4* rq(j4)
        val2=w1* sq(j1)+w2* sq(j2)+w3* sq(j3)+w4* sq(j4)
        if(l_foto)then
          val =val +(w1*dhat_dt%q(j1)+w2*dhat_dt%q(j2)+ &
                     w3*dhat_dt%q(j3)+w4*dhat_dt%q(j4))*time_t
          val2=val2+(w1*xhat_dt%q(j1)+w2*xhat_dt%q(j2)+ &
                     w3*xhat_dt%q(j3)+w4*xhat_dt%q(j4))*time_t
        end if
        qs_prime0=val2+sges(1)*val
        qs_prime1=val2+sges(2)*val
        qs_prime2=val2+sges(3)*val
        qs_prime3=val2+sges(4)*val

        val =w1* ru(j1)+w2* ru(j2)+w3* ru(j3)+w4* ru(j4)
        val2=w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4)
        if(l_foto)then
          val =val +(w1*dhat_dt%u(j1)+w2*dhat_dt%u(j2)+ &
                     w3*dhat_dt%u(j3)+w4*dhat_dt%u(j4))*time_t
          val2=val2+(w1*xhat_dt%u(j1)+w2*xhat_dt%u(j2)+ &
                     w3*xhat_dt%u(j3)+w4*xhat_dt%u(j4))*time_t
        end if
        us_prime0=val2+sges(1)*val
        us_prime1=val2+sges(2)*val
        us_prime2=val2+sges(3)*val
        us_prime3=val2+sges(4)*val

        val =w1* rv(j1)+w2* rv(j2)+w3* rv(j3)+w4* rv(j4)
        val2=w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4)
        if(l_foto)then
          val =val +(w1*dhat_dt%v(j1)+w2*dhat_dt%v(j2)+ &
                     w3*dhat_dt%v(j3)+w4*dhat_dt%v(j4))*time_t
          val2=val2+(w1*xhat_dt%v(j1)+w2*xhat_dt%v(j2)+ &
                     w3*xhat_dt%v(j3)+w4*xhat_dt%v(j4))*time_t
        end if
        vs_prime0=val2+sges(1)*val
        vs_prime1=val2+sges(2)*val
        vs_prime2=val2+sges(3)*val
        vs_prime3=val2+sges(4)*val

        val =w1* rp(j1)+w2* rp(j2)+w3* rp(j3)+w4* rp(j4)
        val2=w1* sp(j1)+w2* sp(j2)+w3* sp(j3)+w4* sp(j4)
        if(l_foto)then
          val =val +(w1*dhat_dt%p3d(j1)+w2*dhat_dt%p3d(j2)+ &
                     w3*dhat_dt%p3d(j3)+w4*dhat_dt%p3d(j4))*time_t
          val2=val2+(w1*xhat_dt%p3d(j1)+w2*xhat_dt%p3d(j2)+ &
                     w3*xhat_dt%p3d(j3)+w4*xhat_dt%p3d(j4))*time_t
        end if
        psfc_prime0=val2+sges(1)*val
        psfc_prime1=val2+sges(2)*val
        psfc_prime2=val2+sges(3)*val
        psfc_prime3=val2+sges(4)*val

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
        t_pg=tptr%pg*varqc_iter
        cg_t=cg_term/tptr%b
        wnotgross= one-t_pg
        wgross =t_pg*cg_t/wnotgross
        pencur = -two*log((exp(-half*pencur)+wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  )+wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  )+wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  )+wgross)/(one+wgross))
      endif

!     Note:  if wgross=0 (no gross error, then wnotgross=1 and this all 
!            reduces to the linear case (no qc)

      out(1) = out(1)+pencur*tptr%raterr2
      out(2) = out(2)+pen1*tptr%raterr2
      out(3) = out(3)+pen2*tptr%raterr2
      out(4) = out(4)+pen3*tptr%raterr2
      cc     = (pen1+pen3-two*pen2)*tptr%raterr2
      out(5) = out(5)+(pen1-pen3)*tptr%raterr2*bcoef1+cc*bcoef2
      out(6) = out(6)+cc*ccoef
    end if

    tptr => tptr%llpoint

  end do
  return
end subroutine stpt

end module stptmod
