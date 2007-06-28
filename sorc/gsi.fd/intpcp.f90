module intpcpmod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intpcpmod    module for intpcp and its tangent linear intpcp_tl
!
! abstract: module for intpcp and its tangent linear intpcp_tl
!
! program history log:
!   2005-05-16  Yanqiu zhu - wrap intpcp and its tangent linear intpcp_tl into one module
!   2005-11-16  Derber - remove interfaces
!

implicit none

PRIVATE
PUBLIC intpcp,intpcp_tl


contains

subroutine intpcp(rt,rq,ru,rv,rcwm,st,sq,su,sv,scwm,rpredp,spredp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intpcp      precip rate nonlin qc obs operator
!   prgmmr: treadon          org: np23                date: 2003-09-13
!
! abstract: apply precipitation rate operator and adjoint with
!            addition of nonlinear qc operator
!
! program history log:
!   2003-12-18 treadon - initial routine
!   2004-06-15 treadon - reformat documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-07 parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intpcp and intpcp_qc into single routine
!   2005-09-28  derber  - modify var qc and change location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-01-19  derber   - limit pcp_ges* > zero
!
!   input argument list:
!     st       - input temperature correction field
!     sq       - input q correction field
!     su       - input zonal wind correction field
!     sv       - input meridional wind correction field
!     scwm     - input cloud condensate mixing ratio correction field
!     spredp   - input bias correction predictor values
!
!   output argument list:
!     rt       - output vector after inclusion of pcp. info.
!     rq       - output q vector after inclusion of pcp. info.
!     ru       - output u vector after inclusion of pcp. info.
!     rv       - output v vector after inclusion of pcp. info.
!     rcwm     - output cwm vector after inclusion of pcp. info.
!     rpredp   - output bias correction predictor vector after inclusion of pcp. info.
!     rst      - output skin temp. vector after inclusion of pcp. info.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: pcpptr,pcphead
  use qcmod, only: nlnqc_iter
  use pcpinfo, only: jtype,npredp,b_pcp,pg_pcp
  use constants, only: zero,one,half,two,tiny_r_kind,cg_term
  use gridmod, only: nsig,latlon11,latlon1n,nsig5
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: st,sq,su,sv,scwm
  real(r_kind),dimension(jtype,npredp),intent(in):: spredp
  real(r_kind),dimension(latlon1n),intent(inout):: rt,rq,ru,rv,rcwm
  real(r_kind),dimension(jtype,npredp),intent(inout):: rpredp
  
! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,nq,nu,nv,ncwm,n,nt,kx
  real(r_kind) dt,dq,du,dv,dcwm,dcwm_ad,termges_ad,w1,w2,w3,w4
  real(r_kind) pcp_ges_ad,dq_ad,dt_ad,dv_ad,du_ad,pcp_ges
! real(r_kind) pen,penalty
  real(r_kind) obsges,termges
  real(r_kind) cg_pcp,p0,wnotgross,wgross

  pcpptr => pcphead
  do while(associated(pcpptr))
     j1=pcpptr%ij(1)
     j2=pcpptr%ij(2)
     j3=pcpptr%ij(3)
     j4=pcpptr%ij(4)
     w1=pcpptr%wij(1)
     w2=pcpptr%wij(2)
     w3=pcpptr%wij(3)
     w4=pcpptr%wij(4)
     pcp_ges = pcpptr%ges 

!    Compute updated simulated rain rate based on changes in t,q,u,v,cwm
     do n = 1,nsig
        dt = w1*st(j1)+w2*st(j2)+ w3*st(j3)+w4*st(j4)
        dq = w1*sq(j1)+w2*sq(j2)+ w3*sq(j3)+w4*sq(j4)
        du = w1*su(j1)+w2*su(j2)+ w3*su(j3)+w4*su(j4)
        dv = w1*sv(j1)+w2*sv(j2)+ w3*sv(j3)+w4*sv(j4)
        dcwm=w1*scwm(j1)+w2*scwm(j2)+ w3*scwm(j3)+w4*scwm(j4)
        
        nt=n; nq=nt+nsig; nu=nq+nsig; nv=nu+nsig; ncwm=nv+nsig
        pcp_ges = pcp_ges + pcpptr%dpcp_dvar(nt)*dt + &
                            pcpptr%dpcp_dvar(nq)*dq + &
                            pcpptr%dpcp_dvar(nu)*du + &
                            pcpptr%dpcp_dvar(nv)*dv + &
                            pcpptr%dpcp_dvar(ncwm)*dcwm
        
        j1=j1+latlon11
        j2=j2+latlon11
        j3=j3+latlon11
        j4=j4+latlon11
        
     end do


!   Logrithmic obs - ges.  plus 1 added for zero rain rate.

!   Ensure rain rate is greater than a small zero
    pcp_ges = max(pcp_ges,zero)
    termges = log(one+pcp_ges)

!   Compute o-g and penalty (penalty not used)
    obsges = termges - pcpptr%obs


!   Adjoint model
    kx=pcpptr%icxp
    if (nlnqc_iter .and. pg_pcp(kx) > tiny_r_kind .and.  &
                         b_pcp(kx)  > tiny_r_kind) then
       cg_pcp=cg_term/b_pcp(kx)
       wnotgross= one-pg_pcp(kx)
       wgross = pg_pcp(kx)*cg_pcp/wnotgross
       p0   = wgross/(wgross+exp(-half*pcpptr%err2*obsges**2))
       obsges = obsges*(one-p0)
    endif

    termges_ad  = obsges*pcpptr%err2*pcpptr%raterr2

!   Adjoint for logrithmic forumulation
    pcp_ges_ad = termges_ad/(one+pcp_ges)

!   Adjoint of pcp_ges update

    j1=pcpptr%ij(1)
    j2=pcpptr%ij(2)
    j3=pcpptr%ij(3)
    j4=pcpptr%ij(4)
    do n=1,nsig
       nt=n; nq=nt+nsig; nu=nq+nsig; nv=nu+nsig; ncwm=nv+nsig

       dcwm_ad = pcpptr%dpcp_dvar(ncwm)*pcp_ges_ad
       dv_ad   = pcpptr%dpcp_dvar(nv)*pcp_ges_ad
       du_ad   = pcpptr%dpcp_dvar(nu)*pcp_ges_ad
       dq_ad   = pcpptr%dpcp_dvar(nq)*pcp_ges_ad
       dt_ad   = pcpptr%dpcp_dvar(nt)*pcp_ges_ad

       rcwm(j4) = rcwm(j4) + w4*dcwm_ad
       rcwm(j3) = rcwm(j3) + w3*dcwm_ad
       rcwm(j2) = rcwm(j2) + w2*dcwm_ad
       rcwm(j1) = rcwm(j1) + w1*dcwm_ad

       rv(j4) = rv(j4) + w4*dv_ad
       rv(j3) = rv(j3) + w3*dv_ad
       rv(j2) = rv(j2) + w2*dv_ad
       rv(j1) = rv(j1) + w1*dv_ad

       ru(j4) = ru(j4) + w4*du_ad
       ru(j3) = ru(j3) + w3*du_ad
       ru(j2) = ru(j2) + w2*du_ad
       ru(j1) = ru(j1) + w1*du_ad

       rq(j4) = rq(j4) + w4*dq_ad
       rq(j3) = rq(j3) + w3*dq_ad
       rq(j2) = rq(j2) + w2*dq_ad
       rq(j1) = rq(j1) + w1*dq_ad

       rt(j4) = rt(j4) + w4*dt_ad
       rt(j3) = rt(j3) + w3*dt_ad
       rt(j2) = rt(j2) + w2*dt_ad
       rt(j1) = rt(j1) + w1*dt_ad

       j1=j1+latlon11
       j2=j2+latlon11
       j3=j3+latlon11
       j4=j4+latlon11
            
    end do

    pcpptr => pcpptr%llpoint 
 end do

 return
end subroutine intpcp


subroutine intpcp_tl(rt,rq,ru,rv,rcwm,&
     st,sq,su,sv,scwm,rpredp,spredp, &
     rt_tl,rq_tl,ru_tl,rv_tl,rcwm_tl,&
     st_tl,sq_tl,su_tl,sv_tl,scwm_tl,rpredp_tl,spredp_tl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intpcp_tl      the tangent linear of precip rate nonlin qc obs operator
!   prgmmr: yanqiu zhu          org: GMAO                date: 2005-05-16
!
! abstract: the tangent linear of the operator that applies precipitation rate 
!           operator and adjoint with addition of nonlinear qc operator
!
! program history log:
!   2005-05-16  yanqiu zhu - tangent linear of intpcp
!
!   input argument list:
!     st       - input temperature correction field
!     sq       - input q correction field
!     su       - input zonal wind correction field
!     sv       - input meridional wind correction field
!     scwm     - input cloud condensate mixing ratio correction field
!     spredp   - input bias correction predictor values
!     st_tl     - input tangent linear temperature correction field
!     sq_tl     - input tangent linear q correction field
!     su_tl     - input tangent linear zonal wind correction field
!     sv_tl     - input tangent linear meridional wind correction field
!     scwm_tl   - input tangent linear cloud condensate mixing ratio correction field
!     spredp_tl - input tangent linear bias correction predictor values
!
!   output argument list:
!     rt       - output vector after inclusion of pcp. info.
!     rq       - output q vector after inclusion of pcp. info.
!     ru       - output u vector after inclusion of pcp. info.
!     rv       - output v vector after inclusion of pcp. info.
!     rcwm     - output cwm vector after inclusion of pcp. info.
!     rpredp   - output bias correction predictor vector after inclusion of pcp. info.
!     rst      - output skin temp. vector after inclusion of pcp. info.
!     rt_tl     - output tangent linear vector after inclusion of pcp. info.
!     rq_tl     - output tangent linear q vector after inclusion of pcp. info.
!     ru_tl     - output tangent linear u vector after inclusion of pcp. info.
!     rv_tl     - output tangent linear v vector after inclusion of pcp. info.
!     rcwm_tl   - output tangent linear cwm vector after inclusion of pcp. info.
!     rpredp_tl - output tangent linear bias correction predictor vector after inclusion of pcp. info.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: pcpptr,pcphead
  use obsmod_tl, only: pcpobs_tl, pcpges_tl
  use qcmod, only: nlnqc_iter
  use pcpinfo, only: jtype,npredp,b_pcp,pg_pcp
  use constants, only: zero,one,half,two,tiny_r_kind,cg_term
  use gridmod, only: nsig,latlon11,latlon1n,nsig5
  implicit none

! Declare passed variables
  real(r_kind),dimension(latlon1n),intent(in):: st,sq,su,sv,scwm
  real(r_kind),dimension(latlon1n),intent(in):: st_tl,sq_tl,su_tl,sv_tl,scwm_tl
  real(r_kind),dimension(jtype,npredp),intent(in):: spredp
  real(r_kind),dimension(jtype,npredp),intent(in):: spredp_tl
  real(r_kind),dimension(latlon1n),intent(inout):: rt,rq,ru,rv,rcwm
  real(r_kind),dimension(latlon1n),intent(inout):: rt_tl,rq_tl,ru_tl,rv_tl,rcwm_tl
  real(r_kind),dimension(jtype,npredp),intent(inout):: rpredp
  real(r_kind),dimension(jtype,npredp),intent(inout):: rpredp_tl
  
! Declare local variables
  integer(i_kind) i,j1,j2,j3,j4,nq,nu,nv,ncwm,n,nt,kx
  real(r_kind) dt,dq,du,dv,dcwm,dcwm_ad,termges_ad,w1,w2,w3,w4
  real(r_kind) dt_tl,dq_tl,du_tl,dv_tl,dcwm_tl,dcwm_ad_tl,termges_ad_tl
  real(r_kind) pcp_ges_ad,dq_ad,dt_ad,dv_ad,du_ad,pcp_obs,pcp_ges
  real(r_kind) pcp_ges_ad_tl,dq_ad_tl,dt_ad_tl,dv_ad_tl,du_ad_tl,pcp_obs_tl,pcp_ges_tl
! real(r_kind) pen,penalty
  real(r_kind) obsges,termobs,termges
  real(r_kind) obsges_tl,termobs_tl,termges_tl
  real(r_kind) cg_pcp,p0,wnotgross,wgross,term
  real(r_kind) p0_tl,term_tl

! If no observations, return to calling routine

  pcpptr => pcphead
  i=0
  do while(associated(pcpptr))
     i=i+1
     j1=pcpptr%ij(1)
     j2=pcpptr%ij(2)
     j3=pcpptr%ij(3)
     j4=pcpptr%ij(4)
     w1=pcpptr%wij(1)
     w2=pcpptr%wij(2)
     w3=pcpptr%wij(3)
     w4=pcpptr%wij(4)
     pcp_ges = pcpptr%ges 
     pcp_obs_tl = pcpobs_tl(i)
     pcp_ges_tl = pcpges_tl(i)

!    Compute updated simulated rain rate based on changes in t,q,u,v,cwm
     do n = 1,nsig
        dt = w1*st(j1)+w2*st(j2)+ w3*st(j3)+w4*st(j4)
        dq = w1*sq(j1)+w2*sq(j2)+ w3*sq(j3)+w4*sq(j4)
        du = w1*su(j1)+w2*su(j2)+ w3*su(j3)+w4*su(j4)
        dv = w1*sv(j1)+w2*sv(j2)+ w3*sv(j3)+w4*sv(j4)
        dcwm=w1*scwm(j1)+w2*scwm(j2)+ w3*scwm(j3)+w4*scwm(j4)
        dt_tl = w1*st_tl(j1)+w2*st_tl(j2)+ w3*st_tl(j3)+w4*st_tl(j4)
        dq_tl = w1*sq_tl(j1)+w2*sq_tl(j2)+ w3*sq_tl(j3)+w4*sq_tl(j4)
        du_tl = w1*su_tl(j1)+w2*su_tl(j2)+ w3*su_tl(j3)+w4*su_tl(j4)
        dv_tl = w1*sv_tl(j1)+w2*sv_tl(j2)+ w3*sv_tl(j3)+w4*sv_tl(j4)
        dcwm_tl=w1*scwm_tl(j1)+w2*scwm_tl(j2)+ w3*scwm_tl(j3)+w4*scwm_tl(j4)
        
        nt=n; nq=nt+nsig; nu=nq+nsig; nv=nu+nsig; ncwm=nv+nsig
        pcp_ges = pcp_ges + pcpptr%dpcp_dvar(nt)*dt + &
                            pcpptr%dpcp_dvar(nq)*dq + &
                            pcpptr%dpcp_dvar(nu)*du + &
                            pcpptr%dpcp_dvar(nv)*dv + &
                            pcpptr%dpcp_dvar(ncwm)*dcwm
        pcp_ges_tl = pcp_ges_tl + pcpptr%dpcp_dvar(nt)*dt_tl + &
                                  pcpptr%dpcp_dvar(nq)*dq_tl + &
                                  pcpptr%dpcp_dvar(nu)*du_tl + &
                                  pcpptr%dpcp_dvar(nv)*dv_tl + &
                                  pcpptr%dpcp_dvar(ncwm)*dcwm_tl
        
        j1=j1+latlon11
        j2=j2+latlon11
        j3=j3+latlon11
        j4=j4+latlon11
        
     end do


!   Logrithmic obs - ges.  plus 1 added for zero rain rate.
    termobs = pcpptr%obs
    pcp_obs = exp(pcpptr%obs)-one
    termobs_tl = pcp_obs_tl/(one+pcp_obs)

!   Ensure (one + updated model rain rate) is greater than a small
!   positive number.  If not, use constant.  The adjoint will have
!   no forcing (ie, contribution) in this case.
    pcp_ges = max(pcp_ges,zero)
    termges = log(one+pcp_ges)
    pcp_ges_tl = max(pcp_ges_tl,zero)
    termges_tl = log(one+pcp_ges_tl)

!   Compute o-g and penalty (penalty not used)
    obsges = termges - termobs 
    obsges_tl = termges_tl - termobs_tl


!   Adjoint model
    kx=pcpptr%icxp
    if (nlnqc_iter .and. pg_pcp(kx) > tiny_r_kind) then
       cg_pcp=cg_term/b_pcp(kx)
       wnotgross= one-pg_pcp(kx)
       wgross = pg_pcp(kx)*cg_pcp
       p0   = wnotgross*exp(-half*pcpptr%err2*obsges**2)+wgross
       term = (p0-wgross)/p0
       p0_tl = -pcpptr%err2*obsges*(p0-wgross)*obsges_tl
       term_tl = wgross/(p0*p0)*p0_tl
    else
       term = one
       term_tl = zero
    endif
    termges_ad  = obsges*pcpptr%err2 * term   
    termges_ad_tl  = obsges_tl*pcpptr%err2*term + obsges*pcpptr%err2*term_tl  

    termges_ad  = termges_ad*pcpptr%raterr2
    termges_ad_tl  = termges_ad_tl*pcpptr%raterr2

!   Adjoint for logrithmic forumulation
    pcp_ges_ad = termges_ad*(one/(one+pcp_ges))
    pcp_ges_ad_tl = termges_ad_tl*(one/(one+pcp_ges)) &
                 - termges_ad/((one+pcp_ges)*(one+pcp_ges))*pcp_ges_tl

!   Adjoint of pcp_ges update

    j1=pcpptr%ij(1)
    j2=pcpptr%ij(2)
    j3=pcpptr%ij(3)
    j4=pcpptr%ij(4)
    do n=1,nsig
       nt=n; nq=nt+nsig; nu=nq+nsig; nv=nu+nsig; ncwm=nv+nsig

       dcwm_ad = pcpptr%dpcp_dvar(ncwm)*pcp_ges_ad
       dv_ad   = pcpptr%dpcp_dvar(nv)*pcp_ges_ad
       du_ad   = pcpptr%dpcp_dvar(nu)*pcp_ges_ad
       dq_ad   = pcpptr%dpcp_dvar(nq)*pcp_ges_ad
       dt_ad   = pcpptr%dpcp_dvar(nt)*pcp_ges_ad
       dcwm_ad_tl = pcpptr%dpcp_dvar(ncwm)*pcp_ges_ad_tl
       dv_ad_tl   = pcpptr%dpcp_dvar(nv)*pcp_ges_ad_tl
       du_ad_tl   = pcpptr%dpcp_dvar(nu)*pcp_ges_ad_tl
       dq_ad_tl   = pcpptr%dpcp_dvar(nq)*pcp_ges_ad_tl
       dt_ad_tl   = pcpptr%dpcp_dvar(nt)*pcp_ges_ad_tl

       rcwm(j4) = rcwm(j4) + w4*dcwm_ad
       rcwm(j3) = rcwm(j3) + w3*dcwm_ad
       rcwm(j2) = rcwm(j2) + w2*dcwm_ad
       rcwm(j1) = rcwm(j1) + w1*dcwm_ad

       rv(j4) = rv(j4) + w4*dv_ad
       rv(j3) = rv(j3) + w3*dv_ad
       rv(j2) = rv(j2) + w2*dv_ad
       rv(j1) = rv(j1) + w1*dv_ad

       ru(j4) = ru(j4) + w4*du_ad
       ru(j3) = ru(j3) + w3*du_ad
       ru(j2) = ru(j2) + w2*du_ad
       ru(j1) = ru(j1) + w1*du_ad

       rq(j4) = rq(j4) + w4*dq_ad
       rq(j3) = rq(j3) + w3*dq_ad
       rq(j2) = rq(j2) + w2*dq_ad
       rq(j1) = rq(j1) + w1*dq_ad

       rt(j4) = rt(j4) + w4*dt_ad
       rt(j3) = rt(j3) + w3*dt_ad
       rt(j2) = rt(j2) + w2*dt_ad
       rt(j1) = rt(j1) + w1*dt_ad

       rcwm_tl(j4) = rcwm_tl(j4) + w4*dcwm_ad_tl
       rcwm_tl(j3) = rcwm_tl(j3) + w3*dcwm_ad_tl
       rcwm_tl(j2) = rcwm_tl(j2) + w2*dcwm_ad_tl
       rcwm_tl(j1) = rcwm_tl(j1) + w1*dcwm_ad_tl

       rv_tl(j4) = rv_tl(j4) + w4*dv_ad_tl
       rv_tl(j3) = rv_tl(j3) + w3*dv_ad_tl
       rv_tl(j2) = rv_tl(j2) + w2*dv_ad_tl
       rv_tl(j1) = rv_tl(j1) + w1*dv_ad_tl

       ru_tl(j4) = ru_tl(j4) + w4*du_ad_tl
       ru_tl(j3) = ru_tl(j3) + w3*du_ad_tl
       ru_tl(j2) = ru_tl(j2) + w2*du_ad_tl
       ru_tl(j1) = ru_tl(j1) + w1*du_ad_tl

       rq_tl(j4) = rq_tl(j4) + w4*dq_ad_tl
       rq_tl(j3) = rq_tl(j3) + w3*dq_ad_tl
       rq_tl(j2) = rq_tl(j2) + w2*dq_ad_tl
       rq_tl(j1) = rq_tl(j1) + w1*dq_ad_tl

       rt_tl(j4) = rt_tl(j4) + w4*dt_ad_tl
       rt_tl(j3) = rt_tl(j3) + w3*dt_ad_tl
       rt_tl(j2) = rt_tl(j2) + w2*dt_ad_tl
       rt_tl(j1) = rt_tl(j1) + w1*dt_ad_tl

       j1=j1+latlon11
       j2=j2+latlon11
       j3=j3+latlon11
       j4=j4+latlon11
            
    end do

    pcpptr => pcpptr%llpoint
 end do

 return
end subroutine intpcp_tl

end module intpcpmod
