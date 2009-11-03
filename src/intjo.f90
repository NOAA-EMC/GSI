module intjomod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intjo    module for intjo
!   prgmmr:
!
! abstract: module for H'R^{-1}H
!
! program history log:
!   2008-12-01  Todling - wrap in module
!   2009-08-13  lueken - update documentation
!
! subroutines included:
!   sub intjo_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use mpl_allreducemod, only: mpl_allreduce
implicit none

PRIVATE
PUBLIC intjo

interface intjo; module procedure &
          intjo_
end interface

contains

subroutine intjo_(yobs,rval,rbias,sval,sbias,ibin)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intjo      calculate RHS for analysis equation
!   prgmmr: derber           org: np23                date: 2003-12-18
!
! abstract: calculate RHS for all variables (nonlinear qc version)
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
!    As a result the same int* routines may be used for use with or without
!    nonlinear quality control.
!    
!
! program history log:
!   2003-12-18  derber
!   2004-07-23  derber  - modify to include conventional sst
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - add nonlinear qc option
!   2004-10-06  kleist  - separate control vector for u,v, & convert int
!                         for wind components into int for st,vp
!   2004-11-30  treadon - add brightness temperatures to nonlinear 
!                         quality control
!   2004-12-03  treadon - replace mpe_iallreduce (IBM extension) with
!                         standard mpi_allreduce
!   2005-01-20  okamoto - add u,v to intrad
!   2005-02-23  wu      - changes related to normalized rh option
!   2005-04-11  treadon - rename intall_qc as intall
!   2005-05-18  yanqiu zhu - add 'use int*mod',and modify call interfaces for using these modules
!   2005-05-24  pondeca - take into consideration that npred=npredp=0
!                         for 2dvar only surface analysis option
!   2005-06-03  parrish - add horizontal derivatives
!   2005-07-10  kleist  - add dynamic constraint term
!   2005-09-29  kleist  - expand Jc term, include time derivatives vector
!   2005-11-21  kleist  - separate tendencies from Jc term, add call to calctends adjoint
!   2005-12-01  cucurull - add code for GPS local bending angle, add use obsmod for ref_obs
!   2005-12-20  parrish - add arguments to call to intt to allow for option of using boundary
!                         layer forward tlm.
!   2006-02-03  derber  - modify to increase reproducibility
!   2006-03-17  park    - correct error in call to intt--rval,sval --> rvaluv,svaluv
!                          in order to correctly pass wind variables.
!   2006-04-06  kleist  - include both Jc formulations
!   2006-07-26  parrish - correct inconsistency in computation of space and time derivatives of q
!                          currently, if derivatives computed, for q it is normalized q, but
!                          should be mixing ratio.
!   2006-07-26  parrish - add strong constraint initialization option
!   2007-03-19  tremolet - binning of observations
!   2007-04-13  tremolet - split jo from other components of intall
!   2007-06-04  derber  - use quad precision to get reproducibility over number of processors
!   2008-11-27  todling  - add tendencies for FOTO support and new interface to int's
!   2009-01-08  todling  - remove reference to ozohead
!   2009-03-23  meunier  - Add call to intlag (lagrangian observations)
!
!   input argument list:
!     ibin
!     yobs
!     sval     - solution on grid
!     sbias
!     rval
!     rbias
!
!   output argument list:      
!     rval     - RHS on grid
!     rbias
!
! remarks:
!     1) if strong initialization, then svalt, svalp, svaluv
!         are all grid fields after strong initialization.
!
!     2) The two interfaces to the int-routines should be temporary.
!        In the framework of the 4dvar-code, foto can be re-implemented as 
!        an approximate M and M' to the model matrices in 4dvar. Once that
!        is done, the int-routines should no longer need the time derivatives.
!        (Todling)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
use kinds, only: r_kind,i_kind,r_quad
use constants, only: ione,zero_quad
use obsmod, only: obs_handle
use jfunc, only: nrclen,nsclen,npclen,l_foto,xhat_dt
use state_vectors
use bias_predictors
use inttmod 
use intwmod
use intpsmod
use intpwmod
use intqmod
use intradmod
use inttcpmod
use intgpsmod
use intrwmod
use intspdmod
use intsrwmod
use intsstmod
use intdwmod
use intpcpmod
use intozmod
use intlagmod
implicit none

! Declare passed variables
integer(i_kind),    intent(in)    :: ibin
type(obs_handle),   intent(in)    :: yobs
type(state_vector), intent(in)    :: sval
type(predictors)  , intent(in)    :: sbias
type(state_vector), intent(inout) :: rval
type(predictors)  , intent(inout) :: rbias

! Declare local variables
integer(i_kind) :: i
real(r_quad),dimension(max(ione,nrclen)):: qpred

!******************************************************************************
  qpred=zero_quad

! Calculate sensible temperature time derivative
  if(l_foto)call tv_to_tsen(xhat_dt%t,xhat_dt%q,xhat_dt%tsen)

! RHS for conventional temperatures
  call intt(yobs%t, &
            rval%tsen,sval%tsen,rval%t,sval%t,rval%q,sval%q, &
            rval%u,sval%u,rval%v,sval%v,rval%p3d,sval%p3d, &
            rval%sst,sval%sst )

! RHS for precipitable water
  call intpw(yobs%pw,rval%q,sval%q)

! RHS for conventional moisture
  call intq(yobs%q,rval%q,sval%q)

! RHS for conventional winds
  call intw(yobs%w,rval%u,rval%v,sval%u,sval%v)

! RHS for radar superob winds
  call intsrw(yobs%srw,rval%u,rval%v,sval%u,sval%v) 

! RHS for lidar winds
  call intdw(yobs%dw,rval%u,rval%v,sval%u,sval%v)

! RHS for radar winds
  call intrw(yobs%rw,rval%u,rval%v,sval%u,sval%v)

! RHS for wind speed observations
  call intspd(yobs%spd,rval%u,rval%v,sval%u,sval%v)

! RHS for ozone observations
  call intoz(yobs%oz,yobs%o3l,rval%oz,sval%oz)

! RHS for surface pressure observations
  call intps(yobs%ps,rval%p3d,sval%p3d)

! RHS for MSLP obs for TCs
  call inttcp(yobs%tcp,rval%p3d,sval%p3d)

! RHS for conventional sst observations
  call intsst(yobs%sst,rval%sst,sval%sst)

! RHS for GPS local observations
  call intgps(yobs%gps, &
              rval%t,rval%q,rval%p3d,sval%t,sval%q,sval%p3d)

! RHS for conventional lag observations
  call intlag(yobs%lag,rval%u,rval%v,sval%u,sval%v,ibin)

! RHS calculation for radiances
  call intrad(yobs%rad, &
              rval%t,rval%q,rval%oz,rval%u,rval%v,rval%sst, &
              sval%t,sval%q,sval%oz,sval%u,sval%v,sval%sst, &
              qpred(1:nsclen),sbias%predr)

! RHS calculation for precipitation
  call intpcp(yobs%pcp, &
              rval%tsen,rval%q,rval%u,rval%v,rval%cw, &
              sval%tsen,sval%q,sval%u,sval%v,sval%cw)

! Take care of background error for bias correction terms

  call mpl_allreduce(nrclen,qpred)

  do i=1,nsclen
    rbias%predr(i)=qpred(i)
  end do
  do i=1,npclen
    rbias%predp(i)=qpred(nsclen+i)
  end do

return
end subroutine intjo_

end module intjomod
