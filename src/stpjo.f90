subroutine stpjo(yobs,dval,dbias,xval,xbias,sges,pbcjo,nstep)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpjo     calculate penalty and stepsize
!   prgmmr: derber           org: np23                date: 2003-12-18
!
! abstract: calculate observation term to penalty and estimate stepsize
!               (nonlinear qc version)
!
!    A description of nonlinear qc follows:
!
!    The observation penalty Jo is defined as
!
!          Jo =  - (sum over obs) 2*log(Po)
!
!      where,
!
!          Po = Wnotgross*exp(-.5*(Hn(x+xb) - yo)**2 ) + Wgross
!            with
!                Hn = the forward model (possibly non-linear) normalized by 
!                     observation error
!                x  = the current estimate of the analysis increment
!                xb = the background state
!                yo = the observation normalized by observation error
!
!            Note:  The factor 2 in definition of Jo is present because the 
!                   penalty Jo as used in this code is 2*(usual definition 
!                   of penalty)
!
!          Wgross = Pgross*cg
!
!          Wnotgross = 1 - Wgross
!
!          Pgross = probability of gross error for observation (assumed
!                   here to have uniform distribution over the possible
!                   range of values)
!
!          cg = sqrt(2*pi)/2b
!
!          b = possible range of variable for gross errors, normalized by 
!              observation error
!
!    The values for the above parameters that Bill Collins used in the
!    eta 3dvar are:
!
!          cg = cg_term/b, where cg_term = sqrt(2*pi)/2 
!
!          b = 10.        ! range for gross errors, normalized by obs error
!
!          pg_q=.002      ! probability of gross error for specific humidity
!          pg_pw=.002     ! probability of gross error for precipitable water
!          pg_p=.002      ! probability of gross error for pressure
!          pg_w=.005      ! probability of gross error for wind
!          pg_t=.007      ! probability of gross error for temperature
!          pg_rad=.002    ! probability of gross error for radiances
!
!
!    Given the above Jo, the gradient of Jo is as follows:
!
!                                             T
!        gradx(Jo) = - (sum over observations) 2*H (Hn(x+xb)-yo)*(Po - Wgross)/Po
!
!      where, 
!
!          H = tangent linear model of Hn about x+xb
!
! 
!    Note that if Pgross = 0.0, then Wnotgross=1.0 and Wgross=0.0.  That is,
!    the code runs as though nonlinear quality control were not present
!    (which is indeed the case since the gross error probability is 0).  
!
!    As a result the same stp* routines may be used for use with or without
!    nonlinear quality control.
!    
!    Please note, however, that using the nonlinear qc routines makes the
!    stp* and int* operators nonlinear.  Hence, the need to evaluate the
!    step size operators twice for each observation type, give the current
!    step size algorithm coded below. 
!
!
! program history log:
!   2003-12-18  derber,j.
!   2004-07-23  derber  - modify to include conventional sst
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - add nonlinear qc option
!   2004-10-06  kleist  - separate control vector for u,v, get search
!                         direction for u,v from dir for st,vp
!   2004-11-30  treadon - add brightness temperatures to nonlinear
!                         quality control
!   2005-01-20  okamoto - add u,v to stprad_qc
!   2005-01-26  cucurull- implement local GPS RO linear operator
!   2005-02-10  treadon - add u,v to stprad_qc (okamoto change not present)
!   2005-02-23  wu      - add call to normal_rh_to_q to convert normalized 
!                         RH to q
!   2005-04-11  treadon - rename stpcalc_qc as stpcalc
!   2005-05-21  yanqiu zhu - add 'use stp*mod', and modify call interfaces for using these modules
!   2005-05-27  derber - remove linear stepsize estimate
!   2005-06-03  parrish - add horizontal derivatives
!   2005-07-10  kleist  - add dynamic constraint term (linear)
!   2005-09-29  kleist  - expand Jc term, include time derivatives vector
!   2005-11-21  kleist  - separate tendencies from Jc term, add call to calctends tlm
!   2005-12-01  cucurull - add code for GPS local bending angle, add use obsmod for ref_obs
!   2005-12-20  parrish - add arguments to call to stpt to enable boundary layer forward
!                         model option.
!   2006-04-18  derber - add explicit iteration over stepsize (rather than 
!                        repeated calls) - clean up and simplify
!   2006-04-24  kleist - include both Jc formulations
!   2006-05-26  derber - modify to improve convergence checking
!   2006-07-26  parrish - correct inconsistency in computation of space and time derivatives of q
!                          currently, if derivatives computed, for q it is normalized q, but
!                          should be mixing ratio.
!   2006-08-04  parrish - add strong constraint initialization option
!   2006-09-18  derber - modify output from nonlinear operators to make same as linear operators
!   2006-09-20  derber - add sensible temperatures for conventional obs.
!   2006-10-12  treadon - replace virtual temperature with sensible in stppcp
!   2007-02-15  rancic  - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-04-13  tremolet - split jo from other components of stpcalc
!   2007-04-16  kleist  - modified calls to tendency and constraint routines
!   2007-06-04  derber  - use quad precision to get reproduceability over number of processors
!   2007-07-26  cucurull - update gps code to generalized vertical coordinate;
!                          get current solution for 3d pressure (xhat_3dp);
!                          move getprs_tl out of calctends_tl; add dirx3dp
!                          and remove ps in calctends_tl argument list;
!                          use getprs_tl 
!   2007-08-08  derber - optimize, ensure that only necessary time derivatives are calculated
!   2008-12-02  todling - revisited split of stpcalc in light of 4dvar merge with May08 version
!   2009-01-08  todling - remove reference to ozohead
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-03-25  zhu     - change the interfaces of stprad,stpt,stppcp;add nrf* conditions 
!   2010-05-13  todling - harmonized all stp interfaces to use state vector; gsi_bundle use
!   2010-06-14  todling - add stpco call 
!   2010-07-10  todling - somebody reordered calls to stpw, stpq, and stpoz - any reason?
!   2010-10-15  pagowski - add stppm2_5 call 

!
!   input argument list:
!     yobs
!     dval     - current solution
!     dbias    - 
!     xval     -
!     xbias    -
!     sges
!     nstep    - number of steps
!
!   output argument list:
!     pbcjo
!
!
! remarks:
!  1. The part of xhat and dirx containing temps and psfc are values before strong initialization,
!     xhatt, xhatp and dirxt, dirxp contain temps and psfc after strong initialization.
!     If strong initialization is turned off, then xhatt, etc are equal to the corresponding 
!     fields in xhat, dirx.
!     xhatuv, xhat_t and dirxuv, dirx_t are all after
!     strong initialization if it is turned on.
!  2. Notice that now (2010-05-13) stp routines handle non-essential variables
!     internally; also, when pointers non-existent, stp routines simply return.
!     
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind,r_kind,r_quad
  use obsmod, only: obs_handle, &
                  & i_ps_ob_type, i_t_ob_type, i_w_ob_type, i_q_ob_type, &
                  & i_spd_ob_type, i_srw_ob_type, i_rw_ob_type, i_dw_ob_type, &
                  & i_sst_ob_type, i_pw_ob_type, i_oz_ob_type, i_colvk_ob_type, &
                  & i_gps_ob_type, i_rad_ob_type, i_pcp_ob_type,i_tcp_ob_type, &
                  &i_pm2_5_ob_type, &
                    nobs_type
  use stptmod, only: stpt
  use stpwmod, only: stpw
  use stppsmod, only: stpps
  use stppwmod, only: stppw
  use stpqmod, only: stpq
! use stpcldmod, only: stpcld
  use stpradmod, only: stprad
  use stpgpsmod, only: stpgps
  use stprwmod, only: stprw
  use stpspdmod, only: stpspd
  use stpsrwmod, only: stpsrw
  use stpsstmod, only: stpsst
  use stptcpmod, only: stptcp
  use stpdwmod, only: stpdw
  use stppcpmod, only: stppcp
  use stpozmod, only: stpoz
  use stpcomod, only: stpco
  use stppm2_5mod, only: stppm2_5
  use bias_predictors, only: predictors
  use gsi_bundlemod, only: gsi_bundle
  implicit none

! Declare passed variables
  type(obs_handle)                    ,intent(in   ) :: yobs
  type(gsi_bundle)                    ,intent(in   ) :: dval
  type(predictors)                    ,intent(in   ) :: dbias
  type(gsi_bundle)                    ,intent(in   ) :: xval
  type(predictors)                    ,intent(in   ) :: xbias
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(4,nobs_type)    ,intent(inout) :: pbcjo

! Declare local variables

!************************************************************************************  
!$omp parallel sections

!$omp section
!   penalty, b, and c for radiances
    call stprad(yobs%rad,dval,xval,dbias%predr,xbias%predr,&
                pbcjo(1,i_rad_ob_type),sges,nstep)

!$omp section
!   penalty, b, and c for temperature
    call stpt(yobs%t,dval,xval,pbcjo(1,i_t_ob_type),sges,nstep) 

!$omp section
!   penalty, b, and c for winds
    call stpw(yobs%w,dval,xval,pbcjo(1,i_w_ob_type),sges,nstep)

!$omp section
!   penalty, b, and c for precipitable water
    call stppw(yobs%pw,dval,xval,pbcjo(1,i_pw_ob_type),sges,nstep)

!$omp section
!   penalty, b, and c for ozone
    call stpco(yobs%colvk,dval,xval,pbcjo(1,i_colvk_ob_type),sges,nstep)

!$omp section
!   penalty, b, and c for ozone
    call stppm2_5(yobs%pm2_5,dval,xval,pbcjo(1,i_pm2_5_ob_type),sges,nstep)

!$omp section
!   penalty, b, and c for wind lidar
    call stpdw(yobs%dw,dval,xval,pbcjo(1,i_dw_ob_type),sges,nstep) 

!$omp section
!   penalty, b, and c for radar
    call stprw(yobs%rw,dval,xval,pbcjo(1,i_rw_ob_type),sges,nstep) 

!$omp section
!   penalty, b, and c for moisture
    call stpq(yobs%q,dval,xval,pbcjo(1,i_q_ob_type),sges,nstep)

!$omp section
!   penalty, b, and c for ozone
    call stpoz(yobs%oz,yobs%o3l,dval,xval,pbcjo(1,i_oz_ob_type),sges,nstep)

!$omp section
!   penalty, b, and c for radar superob wind
    call stpsrw(yobs%srw,dval,xval,pbcjo(1,i_srw_ob_type),sges,nstep)

!$omp section
!   penalty, b, and c for GPS local observation
    call stpgps(yobs%gps,dval,xval,pbcjo(1,i_gps_ob_type),sges,nstep) 

!$omp section
!   penalty, b, and c for conventional sst
    call stpsst(yobs%sst,dval,xval,pbcjo(1,i_sst_ob_type),sges,nstep)

!$omp section
!   penalty, b, and c for wind speed
    call stpspd(yobs%spd,dval,xval,pbcjo(1,i_spd_ob_type),sges,nstep) 

!$omp section
!   penalty, b, and c for precipitation
    call stppcp(yobs%pcp,dval,xval,pbcjo(1,i_pcp_ob_type),sges,nstep)

!$omp section
!   penalty, b, and c for surface pressure
    call stpps(yobs%ps,dval,xval,pbcjo(1,i_ps_ob_type),sges,nstep)

!$omp section
!   penalty, b, and c for MSLP TC obs
    call stptcp(yobs%tcp,dval,xval,pbcjo(1,i_tcp_ob_type),sges,nstep)


!_$omp section
!   penalty, b, and c for clouds
!   call stpcld(dval,xhat,pbcjo(:,i_cld_ob_type),sges,nstep)

!$omp end parallel sections

  return
end subroutine stpjo
