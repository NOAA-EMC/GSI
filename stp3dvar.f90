subroutine stp3dvar(pbc,pstart,dirx,dir_dt)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stp3dvar    calculate penalty and stepsize
!   prgmmr: derber           org: np23                date: 2003-12-18
!
! abstract: calculate contribution from constraints to current penalty and stepsize
!               (nonlinear qc version)
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
!   2007-04-13  tremolet - split 3dvar specific components from sptcalc
!   2007-04-16  kleist  - modified calls to tendency and constraint routines
!   2007-06-04  derber  - use quad precision to get reproduceability over number of processors
!   2007-07-26  cucurull - update gps code to generalized vertical coordinate;
!                          get current solution for 3d pressure (xhat_3dp);
!                          move getprs_tl out of calctends_tl; add dirx3dp
!                          and remove ps in calctends_tl argument list;
!                          use getprs_tl 
!   2007-08-08  derber - optimize, ensure that only necessary time derivatives are calculated
!   2008-11-28  todling - updated call to tv_to_tsen 
!   2008-04-11  safford - rm unused vars and uses
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2008-10-08  derber - move strong balance constraint to background error covariance
!   2008-12-02  todling - revisited split of stpcalc in light of 4dvar merge with May08 version
!
!   input argument list:
!     stpinout - guess stepsize
!     dirx     - search direction for x
!
!   output argument list:
!     dirx     - search direction for state vectors
!     stpinout - final estimate of stepsize
!
!
! remarks:
!     The part of xhat and dirx containing temps and psfc are values before strong initialization,
!     xhatt, xhatp and dirxt, dirxp contain temps and psfc after strong initialization.
!     If strong initialization is turned off, then xhatt, etc are equal to the corresponding 
!     fields in xhat, dirx.
!     xhatuv, xhat_t and dirxuv, dirx_x, dirx_y, are all after
!     strong initialization if it is turned on.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use mpimod, only: levs_id,npe,mype
  use constants, only:  zero,one_tenth,quarter,half,one,two,zero_quad
  use jfunc, only: noz,nq,nt,nsst,ncw,np,iout_iter,nst,nvp,&
       nclen,nclen1,nclen2,nsclen,npclen,xhatsave,yhatsave,factqmin,factqmax,&
       nuvlen,nu,nv,iter,ntendlen,nut,nvt,ntt,nprst,&
       nqt,nozt,ncwt,ndivt,nagvt
  use gridmod, only: latlon1n,latlon11,lat2,lon2,nsig,nsig1o
  use state_vectors
  implicit none

! Declare passed variables
  real(r_quad),dimension(2),intent(inout):: pbc
  real(r_quad),intent(inout):: pstart   
  type(state_vector), intent(inout) :: dirx
  type(state_vector),intent(out)::dir_dt

! Declare local variables
  logical:: tracer
  integer(i_kind) i,k,nnn
  real(r_kind),dimension(nclen)::dirx_x,dirx_y

!************************************************************************************  

! Determine how many vertical levels each mpi task will
! handle in computing horizontal derivatives
  tracer=.true.
  nnn=0
  do k=1,nsig1o
    if (levs_id(k)/=0) nnn=nnn+1
  end do
!   compute derivatives
  call get_derivatives( &
     dirx%u     ,dirx%v     ,dirx%t      ,dirx%p     , &
     dirx%q     ,dirx%oz,    dirx%sst    ,dirx%cw    , &
     dirx_x(nst),dirx_x(nvp),dirx_x(nt)  ,dirx_x(np),  &
     dirx_x(nq) ,dirx_x(noz),dirx_x(nsst),dirx_x(ncw), &
     dirx_y(nst),dirx_y(nvp),dirx_y(nt)  ,dirx_y(np),  &
     dirx_y(nq) ,dirx_y(noz),dirx_y(nsst),dirx_y(ncw), &
     nnn,mype,1)

  call calctends_tl( &
     dirx%u     ,dirx%v      ,dirx%t     ,               &
     dirx%q     ,dirx%oz     ,dirx%cw    ,               &
     dirx_x(nst),dirx_y(nst) ,dirx_x(nvp),dirx_y(nvp),   &
     dirx_x(nt) ,dirx_y(nt)  ,dirx_x(np) ,dirx_y(np),    &
     dirx_x(nq) ,dirx_y(nq)  ,dirx_x(noz),dirx_y(noz),   &
     dirx_x(ncw),dirx_y(ncw) ,     mype,          &
     dir_dt%u,dir_dt%v ,dir_dt%t,dir_dt%p3d, &
     dir_dt%q,dir_dt%oz,dir_dt%cw,dirx%p3d,tracer)

! Convert virtual temperature to sensible temperature for time derivatives
! for search direction
  call tv_to_tsen(dir_dt%t,dir_dt%q,dir_dt%tsen)


  return
end subroutine stp3dvar

