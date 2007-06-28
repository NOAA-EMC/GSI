module stppcpmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppcpmod    module for stppcp and its tangent linear stppcp_tl
!
! abstract: module for stppcp and its tangent linear stppcp_tl
!
! program history log:
!   2005-05-17  Yanqiu zhu - wrap stppcp and its tangent linear stppcp_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE 
PUBLIC  stppcp,stppcp_tl

contains

subroutine stppcp(rt,rq,ru,rv,rcwm,st,sq,su,sv,scwm, &
     rpred,spred,pen,b1,b3,sges1,sges2,sges3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppcp     compute contribution to penalty and
!                           stepsize from pcp, with nonlinear qc
!   prgmmr: treadon          org:  np23               date: 2003-09-13
!
! abstract: compute contribution to penalty and stepsize from precipitation
!           observations
!
! program history log:
!   2003-12-18 treadon - initial routine
!   2004-06-15 treadon - update documentation
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-07 parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stppcp and stppcp_qc into single routine
!   2005-09-28  derber  - modify var qc and change location and weight arrays
!   2006-07-28  derber   - modify to use new inner loop obs data structure
!                        - unify NL qc
!   2006-09-18  derber   - modify output b1 and b3 
!   2007-01-19  derber   - limit pcp_ges* > zero
!
!   input argument list:
!     rt       - search direction for temperature
!     rq       - search direction for moisture 
!     ru       - search direction for zonal wind
!     rv       - search direction for meridional wind
!     rcwm     - search direction for cloud condensate mixing ratio
!     st       - input temperature correction field
!     sq       - input q correction field
!     su       - input u correction field
!     sv       - input v correction field
!     scwm     - input cwm correction field
!     rpred    - search direction for bias correction predictors
!     spred    - input precipitation bias correction values
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!
!   output argument list:
!     pen      - contribution to penalty from precipitation rate
!     b1       - contribution to numerator from precipitation rate
!     b3       - contribution to denomonator from precipitation rate
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use pcpinfo, only: jtype,npredp,b_pcp,pg_pcp
  use obsmod, only: pcpptr,pcphead
  use constants, only: zero,one,half,two,tiny_r_kind,cg_term
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon11,nsig,latlon1n
  implicit none

! Declare passed variables
  real(r_kind),intent(in):: sges1,sges2,sges3
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),dimension(latlon1n),intent(in):: rt,st,rq,sq,ru,su,&
       rv,sv,rcwm,scwm
  real(r_kind),dimension(jtype,npredp),intent(in):: rpred,spred

! Declare local variables
  integer(i_kind) i,n,ncwm,nq,nt,nu,nv,kx
  integer(i_kind) j1,j2,j3,j4
  real(r_kind) alpha,ccoef,bcoef1,bcoef2,cc
  real(r_kind) dt,dt0,dt1,dt2,dt3,w1,w2,w3,w4,w5
  real(r_kind) dq,dq0,dq1,dq2,dq3
  real(r_kind) du,du0,du1,du2,du3
  real(r_kind) dv,dv0,dv1,dv2,dv3
  real(r_kind) dcwm,dcwm0,dcwm1,dcwm2,dcwm3
  real(r_kind) pcp_ges,pcp_ges1,pcp_ges2,pcp_ges3
  real(r_kind) obsges,obsges1,obsges2,obsges3
  real(r_kind) termges,termges1,termges2,termges3
  real(r_kind) cg_pcp,pen1,pen2,pen3,pencur,wgross,wnotgross

! Initialize penalty, b1, and b3 to zero  
  pen=zero
  b1=zero; b3=zero
  alpha=one/(sges2-sges1)
  ccoef=half*alpha*alpha
  bcoef1=half*half*alpha
  bcoef2=sges2*ccoef


! Loop over number of observations.
  pcpptr => pcphead
  do while(associated(pcpptr))
    if(pcpptr%luse)then
     j1=pcpptr%ij(1)
     j2=pcpptr%ij(2)
     j3=pcpptr%ij(3)
     j4=pcpptr%ij(4)
     w1=pcpptr%wij(1)
     w2=pcpptr%wij(2)
     w3=pcpptr%wij(3)
     w4=pcpptr%wij(4)
     pcp_ges = pcpptr%ges
     pcp_ges1 = pcpptr%ges
     pcp_ges2 = pcpptr%ges
     pcp_ges3 = pcpptr%ges

!    Compute four updates to simulated precipitation
     do n=1,nsig
        dt  =w1*st(j1)+w2*st(j2)+ w3*st(j3)+w4*st(j4)
        dq  =w1*sq(j1)+w2*sq(j2)+ w3*sq(j3)+w4*sq(j4)
        du  =w1*su(j1)+w2*su(j2)+ w3*su(j3)+w4*su(j4)
        dv  =w1*sv(j1)+w2*sv(j2)+ w3*sv(j3)+w4*sv(j4)
        dcwm=w1*scwm(j1)+w2*scwm(j2)+ w3*scwm(j3)+w4*scwm(j4)
        
        dt0  =w1*rt(j1)+w2*rt(j2)+ w3*rt(j3)+w4*rt(j4)
        dq0  =w1*rq(j1)+w2*rq(j2)+ w3*rq(j3)+w4*rq(j4)
        du0  =w1*ru(j1)+w2*ru(j2)+ w3*ru(j3)+w4*ru(j4)
        dv0  =w1*rv(j1)+w2*rv(j2)+ w3*rv(j3)+w4*rv(j4)
        dcwm0=w1*rcwm(j1)+w2*rcwm(j2)+ w3*rcwm(j3)+w4*rcwm(j4)
        
        dt1=dt+sges1*dt0;       dt2=dt+sges2*dt0;       dt3=dt+sges3*dt0
        dq1=dq+sges1*dq0;       dq2=dq+sges2*dq0;       dq3=dq+sges3*dq0
        du1=du+sges1*du0;       du2=du+sges2*du0;       du3=du+sges3*du0
        dv1=dv+sges1*dv0;       dv2=dv+sges2*dv0;       dv3=dv+sges3*dv0
        dcwm1=dcwm+sges1*dcwm0; dcwm2=dcwm+sges2*dcwm0; dcwm3=dcwm+sges3*dcwm0
        
        nt=n; nq=nt+nsig; nu=nq+nsig; nv=nu+nsig; ncwm=nv+nsig
        pcp_ges  = pcp_ges  + pcpptr%dpcp_dvar(nt)*dt + &
                              pcpptr%dpcp_dvar(nq)*dq + &
                              pcpptr%dpcp_dvar(nu)*du + &
                              pcpptr%dpcp_dvar(nv)*dv + &
                              pcpptr%dpcp_dvar(ncwm)*dcwm
        pcp_ges1 = pcp_ges1 + pcpptr%dpcp_dvar(nt)*dt1 + &
                              pcpptr%dpcp_dvar(nq)*dq1 + &
                              pcpptr%dpcp_dvar(nu)*du1 + &
                              pcpptr%dpcp_dvar(nv)*dv1 + &
                              pcpptr%dpcp_dvar(ncwm)*dcwm1
        pcp_ges2 = pcp_ges2 + pcpptr%dpcp_dvar(nt)*dt2 + &
                              pcpptr%dpcp_dvar(nq)*dq2 + &
                              pcpptr%dpcp_dvar(nu)*du2 + &
                              pcpptr%dpcp_dvar(nv)*dv2 + &
                              pcpptr%dpcp_dvar(ncwm)*dcwm2
        pcp_ges3 = pcp_ges3 + pcpptr%dpcp_dvar(nt)*dt3 + &
                              pcpptr%dpcp_dvar(nq)*dq3 + &
                              pcpptr%dpcp_dvar(nu)*du3 + &
                              pcpptr%dpcp_dvar(nv)*dv3 + &
                              pcpptr%dpcp_dvar(ncwm)*dcwm3
        
        j1=j1+latlon11
        j2=j2+latlon11
        j3=j3+latlon11
        j4=j4+latlon11
        
     end do
     
!    Logrithmic formulation.  Ensure pcp_ges > zero
     pcp_ges  = max(pcp_ges ,zero)
     pcp_ges1 = max(pcp_ges1,zero)
     pcp_ges2 = max(pcp_ges2,zero)
     pcp_ges3 = max(pcp_ges3,zero)

     termges  = log(one+pcp_ges)
     termges1 = log(one+pcp_ges1)
     termges2 = log(one+pcp_ges2)
     termges3 = log(one+pcp_ges3)

!   Compute obs-ges (innovation)
     obsges = pcpptr%obs - termges
     obsges1= pcpptr%obs - termges1
     obsges2= pcpptr%obs - termges2
     obsges3= pcpptr%obs - termges3

     pencur = pcpptr%err2*obsges*obsges
     pen1   = pcpptr%err2*obsges1*obsges1
     pen2   = pcpptr%err2*obsges2*obsges2
     pen3   = pcpptr%err2*obsges3*obsges3

     kx=pcpptr%icxp
!  Modify penalty term if nonlinear QC
     if (nlnqc_iter .and. pg_pcp(kx) > tiny_r_kind .and.  &
                          b_pcp(kx)  > tiny_r_kind) then
        cg_pcp=cg_term/b_pcp(kx)
        wnotgross= one-pg_pcp(kx)
        wgross = pg_pcp(kx)*cg_pcp/wnotgross
        pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
        pen1   = -two*log((exp(-half*pen1  ) + wgross)/(one+wgross))
        pen2   = -two*log((exp(-half*pen2  ) + wgross)/(one+wgross))
        pen3   = -two*log((exp(-half*pen3  ) + wgross)/(one+wgross))
     endif

!    Accumulate stepsize terms
     pen=pen + pencur * pcpptr%raterr2
     cc = (pen1+pen3-two*pen2)*pcpptr%raterr2
     b1 = b1 + (pen1-pen3)*pcpptr%raterr2*bcoef1+cc*bcoef2
     b3 = b3 + cc*ccoef
    end if
     
    pcpptr => pcpptr%llpoint
  end do
  
  return
end subroutine stppcp


subroutine stppcp_tl(rt,rq,ru,rv,rcwm,st,sq,su,sv,scwm, &
     rpred,spred,pen,b1,b3,sges1,sges2,sges3, &
     rt_tl,rq_tl,ru_tl,rv_tl,rcwm_tl,st_tl,sq_tl,su_tl,sv_tl,scwm_tl, &
     rpred_tl,spred_tl,pen_tl,b1_tl,b3_tl,sges1_tl,sges2_tl,sges3_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppcp_tl    the tangent linear of the operator that computes 
!                            contribution to penalty and stepsize from pcp, with 
!                            nonlinear qc
!   prgmmr: yanqiu zhu          org:  GMAO               date: 2005-05-17
!
! abstract: the tangent linear of the operator that computes contribution to penalty 
!           and stepsize from precipitation observations
!
! program history log:
!   2005-05-17  yanqiu zhu - tangent linear of stppcp
!
!   input argument list:
!     rt       - search direction for temperature
!     rq       - search direction for moisture 
!     ru       - search direction for zonal wind
!     rv       - search direction for meridional wind
!     rcwm     - search direction for cloud condensate mixing ratio
!     st       - input temperature correction field
!     sq       - input q correction field
!     su       - input u correction field
!     sv       - input v correction field
!     scwm     - input cwm correction field
!     rpred    - search direction for bias correction predictors
!     spred    - input precipitation bias correction values
!     sges1    - estimate step size 1
!     sges2    - estimate step size 2
!     sges3    - estimate step size 3
!     rt_tl       - tangent linear search direction for temperature
!     rq_tl       - tangent linear search direction for moisture
!     ru_tl       - tangent linear search direction for zonal wind
!     rv_tl       - tangent linear search direction for meridional wind
!     rcwm_tl     - tangent linear search direction for cloud condensate mixing ratio
!     st_tl       - input tangent linear temperature correction field
!     sq_tl       - input tangent linear q correction field
!     su_tl       - input tangent linear u correction field
!     sv_tl       - input tangent linear v correction field
!     scwm_tl     - input tangent linear cwm correction field
!     rpred_tl    - tangent linear search direction for bias correction predictors
!     spred_tl    - input tangent linear precipitation bias correction values
!     sges1_tl    - tangent linear estimate step size 1
!     sges2_tl    - tangent linear estimate step size 2
!     sges3_tl    - tangent linear estimate step size 3
!
!   output argument list:
!     pen      - contribution to penalty from precipitation rate
!     b1       - pen(sges1)-pen(sges2)
!     b3       - pen(sges3)-pen(sges2)
!     pen_tl      - tangent linear of the contribution to penalty from precipitation rate
!     b1_tl       - pen_tl(sges1)-pen_tl(sges2)
!     b3_tl       - pen_tl(sges3)-pen_tl(sges2)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use pcpinfo, only: jtype,npredp,b_pcp,pg_pcp
  use obsmod, only: pcpptr,pcphead
  use obsmod_tl, only: pcpobs_tl, pcpges_tl
  use constants, only: zero,one,half,two,tiny_r_kind,cg_term
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon11,nsig,latlon1n
  implicit none

! Declare passed variables
  real(r_kind),intent(in):: sges1,sges2,sges3
  real(r_kind),intent(in):: sges1_tl,sges2_tl,sges3_tl
  real(r_kind),intent(out):: pen,b1,b3
  real(r_kind),intent(out):: pen_tl,b1_tl,b3_tl
  real(r_kind),dimension(latlon1n),intent(in):: rt,st,rq,sq,ru,su,&
       rv,sv,rcwm,scwm
  real(r_kind),dimension(latlon1n),intent(in):: rt_tl,st_tl,rq_tl,sq_tl,ru_tl,su_tl,&
       rv_tl,sv_tl,rcwm_tl,scwm_tl
  real(r_kind),dimension(jtype,npredp),intent(in):: rpred,spred
  real(r_kind),dimension(jtype,npredp),intent(in):: rpred_tl,spred_tl

! Declare local variables
  integer(i_kind) i,n,ncwm,nq,nt,nu,nv,kx
  integer(i_kind) j1,j2,j3,j4
  real(r_kind) dt,dt0,dt1,dt2,dt3,w1,w2,w3,w4,w5
  real(r_kind) dt_tl,dt0_tl,dt1_tl,dt2_tl,dt3_tl
  real(r_kind) dq,dq0,dq1,dq2,dq3
  real(r_kind) dq_tl,dq0_tl,dq1_tl,dq2_tl,dq3_tl
  real(r_kind) du,du0,du1,du2,du3
  real(r_kind) du_tl,du0_tl,du1_tl,du2_tl,du3_tl
  real(r_kind) dv,dv0,dv1,dv2,dv3
  real(r_kind) dv_tl,dv0_tl,dv1_tl,dv2_tl,dv3_tl
  real(r_kind) dcwm,dcwm0,dcwm1,dcwm2,dcwm3
  real(r_kind) dcwm_tl,dcwm0_tl,dcwm1_tl,dcwm2_tl,dcwm3_tl
  real(r_kind) pcp_ges,pcp_ges1,pcp_ges2,pcp_ges3,pcp_obs
  real(r_kind) pcp_ges_tl,pcp_ges1_tl,pcp_ges2_tl,pcp_ges3_tl
  real(r_kind) obsges,obsges1,obsges2,obsges3
  real(r_kind) obsges_tl,obsges1_tl,obsges2_tl,obsges3_tl
  real(r_kind) termges,termges1,termges2,termges3
  real(r_kind) termges_tl,termges1_tl,termges2_tl,termges3_tl
  real(r_kind) termobs
  real(r_kind) termobs_tl
  real(r_kind) cg_pcp,pen1,pen2,pen3,pencur,wgross,wnotgross
  real(r_kind) pen1_tl,pen2_tl,pen3_tl,pencur_tl
  real(r_kind) term,term1,term2,term3,halfvar_pcp
  real(r_kind) term_tl,term1_tl,term2_tl,term3_tl
  real(r_kind) exp_arg,exp_arg1,exp_arg2,exp_arg3
  real(r_kind) exp_arg_tl,exp_arg1_tl,exp_arg2_tl,exp_arg3_tl
  real(r_kind) temp

! Initialize penalty, b1, and b3 to zero  
  pen=zero
  b1=zero; b3=zero
  pen_tl=zero
  b1_tl=zero; b3_tl=zero

! Loop over number of observations.
! Loop over number of observations.
  pcpptr => pcphead
  i=0
  do while(associated(pcpptr))
    i=i+1
    if(pcpptr%luse)then
     j1=pcpptr%ij(1)
     j2=pcpptr%ij(2)
     j3=pcpptr%ij(3)
     j4=pcpptr%ij(4)
     w1=pcpptr%wij(1)
     w2=pcpptr%wij(2)
     w3=pcpptr%wij(3)
     w4=pcpptr%wij(4)
     pcp_ges = pcpptr%ges
     pcp_ges1 = pcpptr%ges
     pcp_ges2 = pcpptr%ges
     pcp_ges3 = pcpptr%ges
     pcp_ges1_tl=pcpges_tl(i)
     pcp_ges2_tl=pcpges_tl(i)
     pcp_ges3_tl=pcpges_tl(i)

!    Compute four updates to simulated precipitation
     do n=1,nsig
        dt  =w1*st(j1)+w2*st(j2)+ w3*st(j3)+w4*st(j4)
        dq  =w1*sq(j1)+w2*sq(j2)+ w3*sq(j3)+w4*sq(j4)
        du  =w1*su(j1)+w2*su(j2)+ w3*su(j3)+w4*su(j4)
        dv  =w1*sv(j1)+w2*sv(j2)+ w3*sv(j3)+w4*sv(j4)
        dcwm=w1*scwm(j1)+w2*scwm(j2)+ w3*scwm(j3)+w4*scwm(j4)
        dt_tl =w1*st_tl(j1)+w2*st_tl(j2)+ w3*st_tl(j3)+w4*st_tl(j4)
        dq_tl =w1*sq_tl(j1)+w2*sq_tl(j2)+ w3*sq_tl(j3)+w4*sq_tl(j4)
        du_tl =w1*su_tl(j1)+w2*su_tl(j2)+ w3*su_tl(j3)+w4*su_tl(j4)
        dv_tl =w1*sv_tl(j1)+w2*sv_tl(j2)+ w3*sv_tl(j3)+w4*sv_tl(j4)
        dcwm_tl=w1*scwm_tl(j1)+w2*scwm_tl(j2)+ w3*scwm_tl(j3)+w4*scwm_tl(j4)
        
        dt0  =w1*rt(j1)+w2*rt(j2)+ w3*rt(j3)+w4*rt(j4)
        dq0  =w1*rq(j1)+w2*rq(j2)+ w3*rq(j3)+w4*rq(j4)
        du0  =w1*ru(j1)+w2*ru(j2)+ w3*ru(j3)+w4*ru(j4)
        dv0  =w1*rv(j1)+w2*rv(j2)+ w3*rv(j3)+w4*rv(j4)
        dcwm0=w1*rcwm(j1)+w2*rcwm(j2)+ w3*rcwm(j3)+w4*rcwm(j4)
        dt0_tl  =w1*rt_tl(j1)+w2*rt_tl(j2)+ &
                 w3*rt_tl(j3)+w4*rt_tl(j4)
        dq0_tl  =w1*rq_tl(j1)+w2*rq_tl(j2)+ &
                 w3*rq_tl(j3)+w4*rq_tl(j4)
        du0_tl  =w1*ru_tl(j1)+w2*ru_tl(j2)+ &
                 w3*ru_tl(j3)+w4*ru_tl(j4)
        dv0_tl  =w1*rv_tl(j1)+w2*rv_tl(j2)+ &
                 w3*rv_tl(j3)+w4*rv_tl(j4)
        dcwm0_tl=w1*rcwm_tl(j1)+w2*rcwm_tl(j2)+ &
                 w3*rcwm_tl(j3)+w4*rcwm_tl(j4)
        
        dt1=dt+sges1*dt0;       dt2=dt+sges2*dt0;       dt3=dt+sges3*dt0
        dq1=dq+sges1*dq0;       dq2=dq+sges2*dq0;       dq3=dq+sges3*dq0
        du1=du+sges1*du0;       du2=du+sges2*du0;       du3=du+sges3*du0
        dv1=dv+sges1*dv0;       dv2=dv+sges2*dv0;       dv3=dv+sges3*dv0
        dcwm1=dcwm+sges1*dcwm0; dcwm2=dcwm+sges2*dcwm0; dcwm3=dcwm+sges3*dcwm0
        dt1_tl=dt_tl+sges1_tl*dt0+sges1*dt0_tl
        dt2_tl=dt_tl+sges2_tl*dt0+sges2*dt0_tl
        dt3_tl=dt_tl+sges3_tl*dt0+sges3*dt0_tl
        dq1_tl=dq_tl+sges1_tl*dq0+sges1*dq0_tl
        dq2_tl=dq_tl+sges2_tl*dq0+sges2*dq0_tl
        dq3_tl=dq_tl+sges3_tl*dq0+sges3*dq0_tl
        du1_tl=du_tl+sges1_tl*du0+sges1*du0_tl
        du2_tl=du_tl+sges2_tl*du0+sges2*du0_tl
        du3_tl=du_tl+sges3_tl*du0+sges3*du0_tl
        dv1_tl=dv_tl+sges1_tl*dv0+sges1*dv0_tl
        dv2_tl=dv_tl+sges2_tl*dv0+sges2*dv0_tl
        dv3_tl=dv_tl+sges3_tl*dv0+sges3*dv0_tl
        dcwm1_tl=dcwm_tl+sges1_tl*dcwm0+sges1*dcwm0_tl
        dcwm2_tl=dcwm_tl+sges2_tl*dcwm0+sges2*dcwm0_tl
        dcwm3_tl=dcwm_tl+sges3_tl*dcwm0+sges3*dcwm0_tl
        
        nt=n; nq=nt+nsig; nu=nq+nsig; nv=nu+nsig; ncwm=nv+nsig
        pcp_ges  = pcp_ges  + pcpptr%dpcp_dvar(nt)*dt + &
                              pcpptr%dpcp_dvar(nq)*dq + &
                              pcpptr%dpcp_dvar(nu)*du + &
                              pcpptr%dpcp_dvar(nv)*dv + &
                              pcpptr%dpcp_dvar(ncwm)*dcwm
        pcp_ges1 = pcp_ges1 + pcpptr%dpcp_dvar(nt)*dt1 + &
                              pcpptr%dpcp_dvar(nq)*dq1 + &
                              pcpptr%dpcp_dvar(nu)*du1 + &
                              pcpptr%dpcp_dvar(nv)*dv1 + &
                              pcpptr%dpcp_dvar(ncwm)*dcwm1
        pcp_ges2 = pcp_ges2 + pcpptr%dpcp_dvar(nt)*dt2 + &
                              pcpptr%dpcp_dvar(nq)*dq2 + &
                              pcpptr%dpcp_dvar(nu)*du2 + &
                              pcpptr%dpcp_dvar(nv)*dv2 + &
                              pcpptr%dpcp_dvar(ncwm)*dcwm2
        pcp_ges3 = pcp_ges3 + pcpptr%dpcp_dvar(nt)*dt3 + &
                              pcpptr%dpcp_dvar(nq)*dq3 + &
                              pcpptr%dpcp_dvar(nu)*du3 + &
                              pcpptr%dpcp_dvar(nv)*dv3 + &
                              pcpptr%dpcp_dvar(ncwm)*dcwm3
        pcp_ges_tl = pcp_ges_tl  + pcpptr%dpcp_dvar(nt)*dt_tl + &
                                    pcpptr%dpcp_dvar(nq)*dq_tl + &
                                    pcpptr%dpcp_dvar(nu)*du_tl + &
                                    pcpptr%dpcp_dvar(nv)*dv_tl + &
                                    pcpptr%dpcp_dvar(ncwm)*dcwm_tl
        pcp_ges1_tl = pcp_ges1_tl + pcpptr%dpcp_dvar(nt)*dt1_tl + &
                                    pcpptr%dpcp_dvar(nq)*dq1_tl + &
                                    pcpptr%dpcp_dvar(nu)*du1_tl + &
                                    pcpptr%dpcp_dvar(nv)*dv1_tl + &
                                    pcpptr%dpcp_dvar(ncwm)*dcwm1_tl
        pcp_ges2_tl = pcp_ges2_tl + pcpptr%dpcp_dvar(nt)*dt2_tl + &
                                    pcpptr%dpcp_dvar(nq)*dq2_tl + &
                                    pcpptr%dpcp_dvar(nu)*du2_tl + &
                                    pcpptr%dpcp_dvar(nv)*dv2_tl + &
                                    pcpptr%dpcp_dvar(ncwm)*dcwm2_tl
        pcp_ges3_tl = pcp_ges3_tl + pcpptr%dpcp_dvar(nt)*dt3_tl + &
                                    pcpptr%dpcp_dvar(nq)*dq3_tl + &
                                    pcpptr%dpcp_dvar(nu)*du3_tl + &
                                    pcpptr%dpcp_dvar(nv)*dv3_tl + &
                                    pcpptr%dpcp_dvar(ncwm)*dcwm3_tl
        j1=j1+latlon11
        j2=j2+latlon11
        j3=j3+latlon11
        j4=j4+latlon11
        
     end do
     
!    Logrithmic formulation.  Ensure pcp_ges > zero
     termobs = pcpptr%obs
     pcp_obs = exp(pcpptr%obs)-one
     termobs_tl = pcpobs_tl(i)/(one+pcp_obs)

     pcp_ges  = max(pcp_ges ,zero)
     pcp_ges1 = max(pcp_ges1,zero)
     pcp_ges2 = max(pcp_ges2,zero)
     pcp_ges3 = max(pcp_ges3,zero)

     termges  = log(one+pcp_ges)
     termges1 = log(one+pcp_ges1)
     termges2 = log(one+pcp_ges2)
     termges3 = log(one+pcp_ges3)
     
     pcp_ges_tl  = max(pcp_ges_tl ,zero)
     pcp_ges1_tl = max(pcp_ges1_tl,zero)
     pcp_ges2_tl = max(pcp_ges2_tl,zero)
     pcp_ges3_tl = max(pcp_ges3_tl,zero)

     termges_tl  = pcp_ges_tl/(one+pcp_ges)
     termges1_tl = pcp_ges1_tl/(one+pcp_ges1)
     termges2_tl = pcp_ges2_tl/(one+pcp_ges2)
     termges3_tl = pcp_ges3_tl/(one+pcp_ges3)

!   Compute obs-ges (innovation)
     obsges = termobs - termges
     obsges1= termobs - termges1
     obsges2= termobs - termges2
     obsges3= termobs - termges3
     obsges_tl = termobs_tl - termges_tl
     obsges1_tl= termobs_tl - termges1_tl
     obsges2_tl= termobs_tl - termges2_tl
     obsges3_tl= termobs_tl - termges3_tl

     halfvar_pcp = -half*pcpptr%err2
     exp_arg  = halfvar_pcp*obsges*obsges
     exp_arg1 = halfvar_pcp*obsges1*obsges1
     exp_arg2 = halfvar_pcp*obsges2*obsges2
     exp_arg3 = halfvar_pcp*obsges3*obsges3
     exp_arg_tl  = -pcpptr%err2*obsges*obsges_tl
     exp_arg1_tl = -pcpptr%err2*obsges1*obsges1_tl
     exp_arg2_tl = -pcpptr%err2*obsges2*obsges2_tl
     exp_arg3_tl = -pcpptr%err2*obsges3*obsges3_tl

     kx=pcpptr%icxp
     if (nlnqc_iter .and. pg_pcp(kx) > tiny_r_kind) then
        cg_pcp=cg_term/b_pcp(kx)
        wnotgross= one-pg_pcp(kx)
        wgross = pg_pcp(kx)*cg_pcp
        temp    = wnotgross*exp(exp_arg)
        term_tl = temp/(temp+wgross)*exp_arg_tl
        temp    = wnotgross*exp(exp_arg1)
        term1_tl= temp/(temp+wgross)*exp_arg1_tl
        temp    = wnotgross*exp(exp_arg2)
        term2_tl = temp/(temp+wgross)*exp_arg2_tl
        temp    = wnotgross*exp(exp_arg3)
        term3_tl= temp/(temp+wgross)*exp_arg3_tl
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
     

!    Accumulate stepsize terms
     pen_tl=pen_tl - pencur_tl *two*pcpptr%raterr2 
     b1_tl = b1_tl - (pen1_tl-pen2_tl)*two*pcpptr%raterr2 
     b3_tl = b3_tl - (pen3_tl-pen2_tl)*two*pcpptr%raterr2 
     pen=pen - pencur *two*pcpptr%raterr2 
     b1 = b1 - (pen1-pen2)*two*pcpptr%raterr2 
     b3 = b3 - (pen3-pen2)*two*pcpptr%raterr2 
    end if
     
    pcpptr => pcpptr%llpoint
  end do
  
  return
end subroutine stppcp_tl

end module stppcpmod
