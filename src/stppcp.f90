module stppcpmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stppcpmod    module for stppcp and its tangent linear stppcp_tl
!  prgmmr:
!
! abstract: module for stppcp and its tangent linear stppcp_tl
!
! program history log:
!   2005-05-17  Yanqiu zhu - wrap stppcp and its tangent linear stppcp_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stppcp_tl
!   2009-08-12  lueken - update documentation
!
! subroutines included:
!   sub stppcp
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documnetation block

implicit none

PRIVATE 
PUBLIC  stppcp

contains

subroutine stppcp(pcphead,dval,xval,out,sges,nstep)
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
!   2007-02-15  rancic   - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-05-10  tremolet - add opt to run as linear procedure
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-06-02  safford - rm unused var and uses
!   2008-12-03  todling - changed handling of ptr%time
!   2009-01-26  todling - re-implement Tremolet's linearization for q1fy10
!   2010-03-25  zhu     - use state_vector in the interface;
!                       - add handlings of cw case; add pointer_state
!
!   input argument list:
!     pcphead
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
!     sges     - step size estimates (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep)   - contribution to penalty from precipitation rate - sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use pcpinfo, only: b_pcp,pg_pcp,tinym1_obs
  use obsmod, only: pcp_ob_type
  use constants, only: izero,ione,zero,one,half,two,tiny_r_kind,cg_term,zero_quad,r3600
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon11,nsig,latlon1n
  use gsi_4dvar, only: ltlint
  use jfunc, only: l_foto,xhat_dt,dhat_dt,pointer_state
  use control_vectors, only: nrf3_cw
  use state_vectors
  implicit none

! Declare passed variables
  type(pcp_ob_type),pointer              ,intent(in   ) :: pcphead
  integer(i_kind)                        ,intent(in   ) :: nstep
  real(r_kind),dimension(max(ione,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(ione,nstep)),intent(  out) :: out
  type(state_vector),intent(in) :: dval
  type(state_vector),intent(in) :: xval

! Declare local variables
  integer(i_kind) n,ncwm,nq,nt,nu,nv,kx
  integer(i_kind) i,j1,j2,j3,j4,kk
  real(r_kind) dt,dt0,w1,w2,w3,w4,time_pcp
  real(r_kind) dq,dq0
  real(r_kind) du,du0
  real(r_kind) dv,dv0
  real(r_kind) dcwm,dcwm0
  real(r_kind) pcp_gest,pcp_ges0,pcp_ges,obsges,termges,termgtl,obsgtl
  real(r_kind),dimension(max(ione,nstep)):: pen
  real(r_kind) cg_pcp,wgross,wnotgross
  type(pcp_ob_type), pointer :: pcpptr
  real(r_kind),pointer,dimension(:):: rt,st,rq,sq,ru,su,rv,sv,rcwm,scwm

! Initialize penalty, b1, and b3 to zero  
  out=zero_quad

! prepare pointers
  call pointer_state(xval,tsen=st,q=sq,u=su,v=sv,cw=scwm)
  call pointer_state(dval,tsen=rt,q=rq,u=ru,v=rv,cw=rcwm)

! Loop over number of observations.
  pcpptr => pcphead
  do while(associated(pcpptr))
     if(pcpptr%luse)then
        pcp_ges0 = pcpptr%ges
        if(nstep > izero)then
           j1=pcpptr%ij(1)
           j2=pcpptr%ij(2)
           j3=pcpptr%ij(3)
           j4=pcpptr%ij(4)
           w1=pcpptr%wij(1)
           w2=pcpptr%wij(2)
           w3=pcpptr%wij(3)
           w4=pcpptr%wij(4)
           pcp_gest = zero


!          Compute updates to simulated precipitation
           do n=1,nsig
              dt  =w1*   st(j1)+w2*   st(j2)+ w3*   st(j3)+w4*   st(j4)
              dq  =w1*   sq(j1)+w2*   sq(j2)+ w3*   sq(j3)+w4*   sq(j4)
              du  =w1*   su(j1)+w2*   su(j2)+ w3*   su(j3)+w4*   su(j4)
              dv  =w1*   sv(j1)+w2*   sv(j2)+ w3*   sv(j3)+w4*   sv(j4)
              if (nrf3_cw>izero) then
                 dcwm=w1* scwm(j1)+w2* scwm(j2)+ w3* scwm(j3)+w4* scwm(j4)
              else
                 dcwm=zero
              end if

              dt0  =w1*   rt(j1)+w2*   rt(j2)+ w3*   rt(j3)+w4*   rt(j4)
              dq0  =w1*   rq(j1)+w2*   rq(j2)+ w3*   rq(j3)+w4*   rq(j4)
              du0  =w1*   ru(j1)+w2*   ru(j2)+ w3*   ru(j3)+w4*   ru(j4)
              dv0  =w1*   rv(j1)+w2*   rv(j2)+ w3*   rv(j3)+w4*   rv(j4)
              if (nrf3_cw>izero) then
                 dcwm0=w1* rcwm(j1)+w2* rcwm(j2)+ w3* rcwm(j3)+w4* rcwm(j4)
              else
                 dcwm0=zero
              end if
              if(l_foto) then
                 time_pcp=pcpptr%time*r3600
                 dt=dt+(w1*  xhat_dt%tsen(j1)+w2*  xhat_dt%tsen(j2)+ &
                        w3*  xhat_dt%tsen(j3)+w4*  xhat_dt%tsen(j4))*time_pcp
                 dq=dq+(w1*  xhat_dt%q(j1)+w2*  xhat_dt%q(j2)+ &
                        w3*  xhat_dt%q(j3)+w4*  xhat_dt%q(j4))*time_pcp
                 du=du+(w1*  xhat_dt%u(j1)+w2*  xhat_dt%u(j2)+ &
                        w3*  xhat_dt%u(j3)+w4*  xhat_dt%u(j4))*time_pcp
                 dv=dv+(w1*  xhat_dt%v(j1)+w2*  xhat_dt%v(j2)+ &
                        w3*  xhat_dt%v(j3)+w4*  xhat_dt%v(j4))*time_pcp
                 if (nrf3_cw>izero) &
                 dcwm=dcwm+(w1*xhat_dt%cw(j1)+w2*xhat_dt%cw(j2)+ &
                            w3*xhat_dt%cw(j3)+w4*xhat_dt%cw(j4))*time_pcp
                 dt0=dt0+(w1*  dhat_dt%tsen(j1)+w2*  dhat_dt%tsen(j2)+  &
                          w3*  dhat_dt%tsen(j3)+w4*  dhat_dt%tsen(j4))*time_pcp
                 dq0=dq0+(w1*  dhat_dt%q(j1)+w2*  dhat_dt%q(j2)+  &
                          w3*  dhat_dt%q(j3)+w4*  dhat_dt%q(j4))*time_pcp
                 du0=du0+(w1*  dhat_dt%u(j1)+w2*  dhat_dt%u(j2)+  &
                          w3*  dhat_dt%u(j3)+w4*  dhat_dt%u(j4))*time_pcp
                 dv0=dv0+(w1*  dhat_dt%v(j1)+w2*  dhat_dt%v(j2)+  &
                          w3*  dhat_dt%v(j3)+w4*  dhat_dt%v(j4))*time_pcp
                 if (nrf3_cw>izero) &
                 dcwm0=dcwm0+(w1*dhat_dt%cw(j1)+w2*dhat_dt%cw(j2)+ &
                              w3*dhat_dt%cw(j3)+w4*dhat_dt%cw(j4))*time_pcp
              end if
        
              nt=n; nq=nt+nsig; nu=nq+nsig; nv=nu+nsig; ncwm=nv+nsig
              pcp_ges0 = pcp_ges0 +  pcpptr%dpcp_dvar(nt)  *dt + &
                                     pcpptr%dpcp_dvar(nq)  *dq + &
                                     pcpptr%dpcp_dvar(nu)  *du + &
                                     pcpptr%dpcp_dvar(nv)  *dv + &
                                     pcpptr%dpcp_dvar(ncwm)*dcwm
              pcp_gest = pcp_gest +  pcpptr%dpcp_dvar(nt)  *dt0+ &
                                     pcpptr%dpcp_dvar(nq)  *dq0+ &
                                     pcpptr%dpcp_dvar(nu)  *du0+ &
                                     pcpptr%dpcp_dvar(nv)  *dv0+ &
                                     pcpptr%dpcp_dvar(ncwm)*dcwm0

              j1=j1+latlon11
              j2=j2+latlon11
              j3=j3+latlon11
              j4=j4+latlon11

           end do
           do kk=1,nstep
              pcp_ges = pcp_ges0 + sges(kk)*pcp_gest
!             Logrithmic formulation.  Ensure pcp_ges > zero
              pcp_ges = max(pcp_ges,zero)
              termges = log(one+pcp_ges)
              obsges= pcpptr%obs - termges
              pen(kk) = pcpptr%err2*obsges*obsges
              if(ltlint)then
                 if (pcp_ges>tinym1_obs) then
                    termgtl = pcp_gest/(one+pcp_ges)
                 else
                    termgtl = zero
                 endif
                 obsgtl= - termgtl
                 pen(kk) =   pen(kk)+pcpptr%err2*obsges*obsgtl
              end if
           enddo

        else
           pen(1)=pcpptr%err2*pcp_ges0*pcp_ges0
        end if

        kx=pcpptr%icxp
!       Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. pg_pcp(kx) > tiny_r_kind .and.  &
                             b_pcp(kx)  > tiny_r_kind) then
           cg_pcp=cg_term/b_pcp(kx)
           wnotgross= one-pg_pcp(kx)*varqc_iter
           wgross = varqc_iter*pg_pcp(kx)*cg_pcp/wnotgross
           do kk=1,max(ione,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

!       Accumulate stepsize terms
        out(1) = out(1)+ pen(1) * pcpptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+ (pen(kk)-pen(1)) * pcpptr%raterr2
        end do

     end if ! <luse>
     
     pcpptr => pcpptr%llpoint
  end do
  
  return
end subroutine stppcp

end module stppcpmod
