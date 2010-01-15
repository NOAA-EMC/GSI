module inttmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   inttmod    module for intt and its tangent linear intt_tl
!   prgmmr:
!
! abstract: module for intt and its tangent linear intt_tl
!
! program history log:
!   2005-05-12  Yanqiu zhu - wrap intt and its tangent linear intt_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intt_tl; add interface back
!   2009-08-13  lueken - update documentation
!
! subroutines included:
!   sub intt_
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
PUBLIC intt

interface intt; module procedure &
          intt_
end interface

contains

subroutine intt_(thead,rt,st,rtv,stv,rq,sq,ru,su,rv,sv,rp,sp,rsst,ssst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intt        apply nonlin qc observation operator for temps
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for temperatures with
!             nonlinear qc operator
!
!
! program history log:
!   1991-02-26  derber
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-05  parrish - add non-linear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intt and intt_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su  - modify for variational qc
!   2005-12-20  parrish - add option for boundary layer tlm
!   2006-03-30  park - correct indexing error for surface temp adjoint interpolation
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-20  derber - add sensible temperature for conventional temperatures
!   2006-10-20  rancic - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-11-26  Todling - turned FOTO optional; changed ptr%time handle
!
!   input argument list:
!     thead    - obs type pointer to obs structure
!     st       - sensible temperature increment in grid space
!     stv      - virtual temperature increment in grid space
!     sq       - moisture increment in grid space
!     su       - u increment in grid space
!     sv       - v increment in grid space
!     sp       - surface pressure increment in grid space
!     ssst     - sst increment in grid space
!     rt
!     rtv
!     rq
!     ru
!     rv
!     rp
!     rsst
!
!   output argument list:
!     rt       - sensible temperature results from observation operator
!     rtv      - virtual temperature results from observation operator
!     rq       - moisture results from observation operator
!     ru       - u results from observation operator
!     rv       - v results from observation operator
!     rp       - surface pressure results from observation operator
!     rsst     - sst results from observation operator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,zero,tiny_r_kind,cg_term,r3600
  use obsmod, only: t_ob_type,lsaveobsens,l_do_adjoint
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon1n,latlon11,latlon1n1
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
  implicit none
  

! Declare passed variables
  type(t_ob_type),pointer          ,intent(in   ) :: thead
  real(r_kind),dimension(latlon1n) ,intent(in   ) :: st,stv,sq,su,sv
  real(r_kind),dimension(latlon11) ,intent(in   ) :: ssst
  real(r_kind),dimension(latlon1n1),intent(in   ) :: sp
  real(r_kind),dimension(latlon1n) ,intent(inout) :: rt,rtv,rq,ru,rv
  real(r_kind),dimension(latlon11) ,intent(inout) :: rsst
  real(r_kind),dimension(latlon1n1),intent(inout) :: rp

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,time_t
! real(r_kind) penalty
  real(r_kind) cg_t,val,p0,grad,wnotgross,wgross,t_pg
  real(r_kind) psfc_grad,tg_grad
  real(r_kind) ts_grad,us_grad,vs_grad,qs_grad
  real(r_kind) qs_prime0,tg_prime0,ts_prime0,psfc_prime0
  real(r_kind) us_prime0,vs_prime0
  type(t_ob_type), pointer :: tptr

  time_t=zero
  tptr => thead
  do while (associated(tptr))

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

     if(l_foto) time_t=tptr%time*r3600
     if(tptr%use_sfc_model) then

!----------use surface model----------------------

        if(tptr%tv_ob)then
           ts_prime0=w1*stv(j1)+w2*stv(j2)+w3*stv(j3)+w4*stv(j4)
        else
           ts_prime0=w1*st(j1)+w2*st(j2)+w3*st(j3)+w4*st(j4)
        end if 
        tg_prime0=w1* ssst(j1)+w2*ssst(j2)+w3*ssst(j3)+w4*ssst(j4)
        qs_prime0=w1*   sq(j1)+w2*  sq(j2)+w3*  sq(j3)+w4*  sq(j4)
        us_prime0=w1*   su(j1)+w2*  su(j2)+w3*  su(j3)+w4*  su(j4)
        vs_prime0=w1*   sv(j1)+w2*  sv(j2)+w3*  sv(j3)+w4*  sv(j4)
        psfc_prime0=w1* sp(j1)+w2*  sp(j2)+w3*  sp(j3)+w4*  sp(j4)
        if ( l_foto ) then
           ts_prime0=ts_prime0+ &
                    (w1*xhat_dt%tsen(j1)+w2*xhat_dt%tsen(j2)+ &
                     w3*xhat_dt%tsen(j3)+w4*xhat_dt%tsen(j4))*time_t
           qs_prime0=qs_prime0+ &
                    (w1*xhat_dt%q(j1)+w2*xhat_dt%q(j2)+ &
                     w3*xhat_dt%q(j3)+w4*xhat_dt%q(j4))*time_t
           us_prime0=us_prime0+&
                    (w1*xhat_dt%u(j1)+w2*xhat_dt%u(j2)+ &
                     w3*xhat_dt%u(j3)+w4*xhat_dt%u(j4))*time_t
           vs_prime0=vs_prime0+&
                    (w1*xhat_dt%v(j1)+w2*xhat_dt%v(j2)+ &
                     w3*xhat_dt%v(j3)+w4*xhat_dt%v(j4))*time_t
           psfc_prime0=psfc_prime0+ &
                    (w1*xhat_dt%p3d(j1)+w2*xhat_dt%p3d(j2)+ &
                     w3*xhat_dt%p3d(j3)+w4*xhat_dt%p3d(j4))*time_t
        endif

        val=psfc_prime0*tptr%tlm_tsfc(1) + tg_prime0*tptr%tlm_tsfc(2) + &
            ts_prime0  *tptr%tlm_tsfc(3) + qs_prime0*tptr%tlm_tsfc(4) + &
            us_prime0  *tptr%tlm_tsfc(5) + vs_prime0*tptr%tlm_tsfc(6)
 
     else

!       Forward model (for interpolation)
        if(tptr%tv_ob)then
           val=w1*stv(j1)+w2*stv(j2)+w3*stv(j3)+w4*stv(j4)&
              +w5*stv(j5)+w6*stv(j6)+w7*stv(j7)+w8*stv(j8)
           if ( l_foto ) then
              val=val&
              +(w1*xhat_dt%t(j1)+w2*xhat_dt%t(j2)+ &
                w3*xhat_dt%t(j3)+w4*xhat_dt%t(j4)+ &
                w5*xhat_dt%t(j5)+w6*xhat_dt%t(j6)+ &
                w7*xhat_dt%t(j7)+w8*xhat_dt%t(j8))*time_t
           endif
        else
           val=w1*    st(j1)+w2*    st(j2)+w3*    st(j3)+w4*    st(j4)&
              +w5*    st(j5)+w6*    st(j6)+w7*    st(j7)+w8*    st(j8)
           if ( l_foto ) then
              val=val&
              +(w1*xhat_dt%tsen(j1)+w2*xhat_dt%tsen(j2)+ &
                w3*xhat_dt%tsen(j3)+w4*xhat_dt%tsen(j4)+ &
                w5*xhat_dt%tsen(j5)+w6*xhat_dt%tsen(j6)+ &
                w7*xhat_dt%tsen(j7)+w8*xhat_dt%tsen(j8))*time_t
           endif
        end if

     end if

     if (lsaveobsens) then
        tptr%diags%obssen(jiter) = val*tptr%raterr2*tptr%err2
     else
        if (tptr%luse) tptr%diags%tldepart(jiter)=val
     endif

!    Do adjoint
     if (l_do_adjoint) then
        if (lsaveobsens) then
           grad=tptr%diags%obssen(jiter)

        else
           val=val-tptr%res
 
!          gradient of nonlinear operator

           if (nlnqc_iter .and. tptr%pg > tiny_r_kind .and.  &
                                tptr%b  > tiny_r_kind) then
              t_pg=tptr%pg*varqc_iter
              cg_t=cg_term/tptr%b
              wnotgross= one-t_pg
              wgross =t_pg*cg_t/wnotgross
              p0=wgross/(wgross+exp(-half*tptr%err2*val**2))
              val=val*(one-p0)                  
           endif

           grad = val*tptr%raterr2*tptr%err2
        endif

!       Adjoint of interpolation
        if(tptr%use_sfc_model) then

!          Surface model
 
           psfc_grad=tptr%tlm_tsfc(1)*grad
           rp(j1)=rp(j1)+w1*psfc_grad
           rp(j2)=rp(j2)+w2*psfc_grad
           rp(j3)=rp(j3)+w3*psfc_grad
           rp(j4)=rp(j4)+w4*psfc_grad
           vs_grad  =tptr%tlm_tsfc(6)*grad
           rv(j1)=rv(j1)+w1*vs_grad
           rv(j2)=rv(j2)+w2*vs_grad
           rv(j3)=rv(j3)+w3*vs_grad
           rv(j4)=rv(j4)+w4*vs_grad
           us_grad  =tptr%tlm_tsfc(5)*grad
           ru(j1)=ru(j1)+w1*us_grad
           ru(j2)=ru(j2)+w2*us_grad
           ru(j3)=ru(j3)+w3*us_grad
           ru(j4)=ru(j4)+w4*us_grad
           qs_grad  =tptr%tlm_tsfc(4)*grad
           rq(j1)=rq(j1)+w1*qs_grad
           rq(j2)=rq(j2)+w2*qs_grad
           rq(j3)=rq(j3)+w3*qs_grad
           rq(j4)=rq(j4)+w4*qs_grad
           tg_grad  =tptr%tlm_tsfc(2)*grad
           rsst(j1)=rsst(j1)+w1*tg_grad
           rsst(j2)=rsst(j2)+w2*tg_grad
           rsst(j3)=rsst(j3)+w3*tg_grad
           rsst(j4)=rsst(j4)+w4*tg_grad
           if (l_foto) then
              dhat_dt%p3d(j1)=dhat_dt%p3d(j1)+w1*psfc_grad*time_t
              dhat_dt%p3d(j2)=dhat_dt%p3d(j2)+w2*psfc_grad*time_t
              dhat_dt%p3d(j3)=dhat_dt%p3d(j3)+w3*psfc_grad*time_t
              dhat_dt%p3d(j4)=dhat_dt%p3d(j4)+w4*psfc_grad*time_t
              dhat_dt%v(j1)=dhat_dt%v(j1)+w1*vs_grad*time_t
              dhat_dt%v(j2)=dhat_dt%v(j2)+w2*vs_grad*time_t
              dhat_dt%v(j3)=dhat_dt%v(j3)+w3*vs_grad*time_t
              dhat_dt%v(j4)=dhat_dt%v(j4)+w4*vs_grad*time_t
              dhat_dt%u(j1)=dhat_dt%u(j1)+w1*us_grad*time_t
              dhat_dt%u(j2)=dhat_dt%u(j2)+w2*us_grad*time_t
              dhat_dt%u(j3)=dhat_dt%u(j3)+w3*us_grad*time_t
              dhat_dt%u(j4)=dhat_dt%u(j4)+w4*us_grad*time_t
              dhat_dt%q(j1)=dhat_dt%q(j1)+w1*qs_grad*time_t
              dhat_dt%q(j2)=dhat_dt%q(j2)+w2*qs_grad*time_t
              dhat_dt%q(j3)=dhat_dt%q(j3)+w3*qs_grad*time_t
              dhat_dt%q(j4)=dhat_dt%q(j4)+w4*qs_grad*time_t
           endif


           ts_grad  =tptr%tlm_tsfc(3)*grad
           if(tptr%tv_ob)then
              rtv(j1)=rtv(j1)+w1*ts_grad
              rtv(j2)=rtv(j2)+w2*ts_grad
              rtv(j3)=rtv(j3)+w3*ts_grad
              rtv(j4)=rtv(j4)+w4*ts_grad

              if (l_foto) then
                 dhat_dt%t(j1)=dhat_dt%t(j1)+w1*ts_grad*time_t
                 dhat_dt%t(j2)=dhat_dt%t(j2)+w2*ts_grad*time_t
                 dhat_dt%t(j3)=dhat_dt%t(j3)+w3*ts_grad*time_t
                 dhat_dt%t(j4)=dhat_dt%t(j4)+w4*ts_grad*time_t
              endif

           else
              rt(j1)=rt(j1)+w1*ts_grad
              rt(j2)=rt(j2)+w2*ts_grad
              rt(j3)=rt(j3)+w3*ts_grad
              rt(j4)=rt(j4)+w4*ts_grad
 
              if (l_foto) then
                 dhat_dt%tsen(j1)=dhat_dt%tsen(j1)+w1*ts_grad*time_t
                 dhat_dt%tsen(j2)=dhat_dt%tsen(j2)+w2*ts_grad*time_t
                 dhat_dt%tsen(j3)=dhat_dt%tsen(j3)+w3*ts_grad*time_t
                 dhat_dt%tsen(j4)=dhat_dt%tsen(j4)+w4*ts_grad*time_t
              endif

           end if

        else

!------bypass surface model--------------------------

           if(tptr%tv_ob)then
              rtv(j1)=rtv(j1)+w1*grad
              rtv(j2)=rtv(j2)+w2*grad
              rtv(j3)=rtv(j3)+w3*grad
              rtv(j4)=rtv(j4)+w4*grad
              rtv(j5)=rtv(j5)+w5*grad
              rtv(j6)=rtv(j6)+w6*grad
              rtv(j7)=rtv(j7)+w7*grad
              rtv(j8)=rtv(j8)+w8*grad
 
              if ( l_foto ) then
                 dhat_dt%t(j1)=dhat_dt%t(j1)+w1*grad*time_t
                 dhat_dt%t(j2)=dhat_dt%t(j2)+w2*grad*time_t
                 dhat_dt%t(j3)=dhat_dt%t(j3)+w3*grad*time_t
                 dhat_dt%t(j4)=dhat_dt%t(j4)+w4*grad*time_t
                 dhat_dt%t(j5)=dhat_dt%t(j5)+w5*grad*time_t
                 dhat_dt%t(j6)=dhat_dt%t(j6)+w6*grad*time_t
                 dhat_dt%t(j7)=dhat_dt%t(j7)+w7*grad*time_t
                 dhat_dt%t(j8)=dhat_dt%t(j8)+w8*grad*time_t
              endif

           else
              rt(j1)=rt(j1)+w1*grad
              rt(j2)=rt(j2)+w2*grad
              rt(j3)=rt(j3)+w3*grad
              rt(j4)=rt(j4)+w4*grad
              rt(j5)=rt(j5)+w5*grad
              rt(j6)=rt(j6)+w6*grad
              rt(j7)=rt(j7)+w7*grad
              rt(j8)=rt(j8)+w8*grad
 
              if ( l_foto ) then
                 dhat_dt%tsen(j1)=dhat_dt%tsen(j1)+w1*grad*time_t
                 dhat_dt%tsen(j2)=dhat_dt%tsen(j2)+w2*grad*time_t
                 dhat_dt%tsen(j3)=dhat_dt%tsen(j3)+w3*grad*time_t
                 dhat_dt%tsen(j4)=dhat_dt%tsen(j4)+w4*grad*time_t
                 dhat_dt%tsen(j5)=dhat_dt%tsen(j5)+w5*grad*time_t
                 dhat_dt%tsen(j6)=dhat_dt%tsen(j6)+w6*grad*time_t
                 dhat_dt%tsen(j7)=dhat_dt%tsen(j7)+w7*grad*time_t
                 dhat_dt%tsen(j8)=dhat_dt%tsen(j8)+w8*grad*time_t
              endif

           end if

        end if

     end if

     tptr => tptr%llpoint
  end do
  return
end subroutine intt_

end module inttmod
