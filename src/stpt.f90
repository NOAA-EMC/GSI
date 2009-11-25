module stptmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stptmod    module for stpt and its tangent linear stpt_tl
!  prgmmr:
!
! abstract: module for stpt and its tangent linear stpt_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stpt and its tangent linear stpt_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stpt_tl
!   2009-08-12  lueken - update documentation
!
! subroutines included:
!   sub stpt
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none

PRIVATE
PUBLIC stpt

contains

subroutine stpt(thead,rt,st,rtv,stv,rq,sq,ru,su,rv,sv,rp,sp,rsst,ssst,out,sges,nstep)
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
!     thead
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
!     sges     - step size estimates (nstep)
!     nstep    - number of stepsizes (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - penalty from temperature observations sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: t_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use gridmod, only: latlon1n,latlon11,latlon1n1
  use jfunc, only: l_foto,xhat_dt,dhat_dt
  implicit none

! Declare passed variables
  type(t_ob_type),pointer,intent(in):: thead
  integer(i_kind),intent(in)::nstep
  real(r_quad),dimension(max(1,nstep)),intent(out):: out
  real(r_kind),dimension(latlon1n),intent(in):: rt,st,rtv,stv,rq,sq,ru,su,rv,sv
  real(r_kind),dimension(latlon11),intent(in):: rsst,ssst
  real(r_kind),dimension(latlon1n1),intent(in):: rp,sp
  real(r_kind),dimension(max(1,nstep)),intent(in):: sges

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,kk
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) cg_t,val,val2,wgross,wnotgross,t_pg
  real(r_kind),dimension(max(1,nstep))::pen,tt
  real(r_kind) tg_prime,valq,valq2,valp,valp2,valu,valu2
  real(r_kind) ts_prime,valv,valv2,valsst,valsst2
  real(r_kind) qs_prime
  real(r_kind) us_prime
  real(r_kind) vs_prime
  real(r_kind) psfc_prime
  real(r_kind) time_t
  type(t_ob_type), pointer :: tptr

  out=zero_quad

  tptr => thead
  do while (associated(tptr))

    if(tptr%luse)then
      if(nstep > 0)then
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
!    Note time derivative stuff not consistent for virtual temperature

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

        do kk=1,nstep
          tt(kk)=val2+sges(kk)*val
        end do

        if(tptr%use_sfc_model) then

            valsst =w1*rsst(j1)+w2*rsst(j2)+w3*rsst(j3)+w4*rsst(j4)
            valsst2=w1*ssst(j1)+w2*ssst(j2)+w3*ssst(j3)+w4*ssst(j4)
            valq =w1* rq(j1)+w2* rq(j2)+w3* rq(j3)+w4* rq(j4)
            valq2=w1* sq(j1)+w2* sq(j2)+w3* sq(j3)+w4* sq(j4)
            valu =w1* ru(j1)+w2* ru(j2)+w3* ru(j3)+w4* ru(j4)
            valu2=w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4)
            valv =w1* rv(j1)+w2* rv(j2)+w3* rv(j3)+w4* rv(j4)
            valv2=w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4)
            valp =w1* rp(j1)+w2* rp(j2)+w3* rp(j3)+w4* rp(j4)
            valp2=w1* sp(j1)+w2* sp(j2)+w3* sp(j3)+w4* sp(j4)
            if(l_foto)then
              valq =valq +(w1*dhat_dt%q(j1)+w2*dhat_dt%q(j2)+ &
                           w3*dhat_dt%q(j3)+w4*dhat_dt%q(j4))*time_t
              valq2=valq2+(w1*xhat_dt%q(j1)+w2*xhat_dt%q(j2)+ &
                           w3*xhat_dt%q(j3)+w4*xhat_dt%q(j4))*time_t
              valu =valu +(w1*dhat_dt%u(j1)+w2*dhat_dt%u(j2)+ &
                           w3*dhat_dt%u(j3)+w4*dhat_dt%u(j4))*time_t
              valu2=valu2+(w1*xhat_dt%u(j1)+w2*xhat_dt%u(j2)+ &
                           w3*xhat_dt%u(j3)+w4*xhat_dt%u(j4))*time_t
              valv =valv +(w1*dhat_dt%v(j1)+w2*dhat_dt%v(j2)+ &
                           w3*dhat_dt%v(j3)+w4*dhat_dt%v(j4))*time_t
              valv2=valv2+(w1*xhat_dt%v(j1)+w2*xhat_dt%v(j2)+ &
                           w3*xhat_dt%v(j3)+w4*xhat_dt%v(j4))*time_t
              valp =valp +(w1*dhat_dt%p3d(j1)+w2*dhat_dt%p3d(j2)+ &
                           w3*dhat_dt%p3d(j3)+w4*dhat_dt%p3d(j4))*time_t
              valp2=valp2+(w1*xhat_dt%p3d(j1)+w2*xhat_dt%p3d(j2)+ &
                           w3*xhat_dt%p3d(j3)+w4*xhat_dt%p3d(j4))*time_t
            end if
            do kk=1,nstep
              ts_prime=tt(kk)
              tg_prime=valsst2+sges(kk)*valsst
              qs_prime=valq2+sges(kk)*valq
              us_prime=valu2+sges(kk)*val
              vs_prime=valv2+sges(kk)*val
              psfc_prime=val2+sges(1)*val

              tt(kk)=psfc_prime*tptr%tlm_tsfc(1) + tg_prime*tptr%tlm_tsfc(2) + &
                     ts_prime  *tptr%tlm_tsfc(3) + qs_prime*tptr%tlm_tsfc(4) + &
                     us_prime  *tptr%tlm_tsfc(5) + vs_prime*tptr%tlm_tsfc(6)
           end do

        end if

        do kk=1,nstep
          tt(kk)=tt(kk)-tptr%res
        end do
      else
        tt(1)=tptr%res
      end if

      do kk=1,max(1,nstep)
        pen(kk) = tt(kk)*tt(kk)*tptr%err2
      end do

!  Modify penalty term if nonlinear QC

      if (nlnqc_iter .and. tptr%pg > tiny_r_kind .and. tptr%b >tiny_r_kind) then
        t_pg=tptr%pg*varqc_iter
        cg_t=cg_term/tptr%b
        wnotgross= one-t_pg
        wgross =t_pg*cg_t/wnotgross
        do kk=1,max(1,nstep)
          pen(kk) = -two*log((exp(-half*pen(kk))+wgross)/(one+wgross))
        end do
      endif

!     Note:  if wgross=0 (no gross error, then wnotgross=1 and this all 
!            reduces to the linear case (no qc)

      out(1) = out(1)+pen(1)*tptr%raterr2
      do kk=2,nstep
        out(kk) = out(kk)+(pen(kk)-pen(1))*tptr%raterr2
      end do
    end if

    tptr => tptr%llpoint

  end do
  return
end subroutine stpt

end module stptmod
