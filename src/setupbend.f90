subroutine setupbend(lunin,mype,awork,nele,nobs,toss_gps_sub,is,init_pass,last_pass)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupbend    compute rhs of oi for gps bending angle
!   prgmmr: cucurull, l.    org: JCSDA/NCEP           date: 2005-12-01
!
! abstract:  For gps bending angle observations, this routine
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   2005-12-01  cucurull - original code
!   2005-12-09  derber   - remove psfcg and use ges_lnps instead
!   2005-12-21  treadon  - add super_gps, move some diagnostics statistics
!                         to genstat_gps
!   2006-01-04  treadon  - correct inconsistency when using qcfail
!   2006-02-02  treadon  - rename lnprsl as ges_lnprsl
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-02-24 cucurull - update QC parameters and compute representativeness error
!                       - fix bug when counting obs that fail gross qc check
!                       - fix bug when rejecting obs that fail sats QC
!   2006-04-14  middlecoff - changed IF test to avoid out-of-bounds-reference on DATA
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2006-07-31  kleist  - change to use ges_ps instead of lnps
!   2006-09-20  cucurull - use geopotential heights at intermediate levels instead 
!                          of midpoint,levels,remove psges, generalize minimization terms
!                          to hybrid coordinate,penalize high level obs,new QC checks,
!                          remove obs above 30 km,add b_tkges, remove dtime,  
!                          improve obs pressure calculation for diagnostic purposes,
!                          increase the extended atmosphere from 6 to 10 levels
!   2006-10-20 cucurull - update QC statistical checks and representativeness error with 
!                         COSMIC data
!                       - add information to diagnostic file
!   2007-01-29 cucurull - remove observations below 6 km
!   2007-03-01 derber - add toss_gps_sub; simplify profile qc loop
!   2007-03-19 tremolet - binning of observations
!   2007-06-05 tremolet - add observation diagnostics structure
!   2007-04-18     su - add nchar and cdiagbuf to diagnostic file
!   2007-06-22 cucurull - generalize qc structure to enable regional GSI;
!                         reduce gpswork2;remove conv_diagsave from argument list;
!                         consistent data counts for qc checks;
!                         update diagnostic information to be consistent with other obs;
!                         modify diagnostic structure;
!                         fix bug for jprof in stats qc
!   2007-07-26 cucurull - update code to generalized vertical coordinate (3d pressure)
!   2007-09-21 cucurull - remove obs above 40km from qc checks
!   2008-04-14 treadon  - remove super_gps (moved to genstats_gps)
!   2008-05-23 safford  - rm unused vars and uses
!   2008-12-03 todling  - revisited Tremolet modifications in light of newer GSI
!                       - changed handle of tail%time
!   2009-08-19 guo      - changed for multi-pass setup with dtime_check().
!   2010-04-16 cucurull - substantial clean up, bugs fixes, and update according to ref    
!   2010-04-23 cucurull - simplify loops and define repe_gps here
!   2010-05-26 cucurull - modify ds
!   2010-06-11 cucurull - update Statistics QC
!   2010-05-24 guo      - remerged/reimplmented multi-pass setup in observer mode;
!   2010-08-09 lueken   - removed n_5km variable from code.
!   2010-08-10 treadon  - remove last check for gpshead allocate; clean up use statements, 
!                         replace (izero,ione) with (0,1), remove _i_kind suffix from integer 
!                         constants
!   2010-08-11 lcucurull - replace tpdpres with tpdpres(nobs) to fix bug in TL code
!   2010-08-18        hu - add tell to mpeu_util declaration
!   2010-10-25 cucurull  - add quality control options for C/NOFS satellite
!   2010-11-8  cucurull  - tune observational errors
!   2010-12-12 cucurull  - generalize number of layers above model top to nsig_ext
!   2011-01-05 cucurull  - add gpstop to reject anything above this value
!   2011-01-07 cucurull  - reject observation if outside new integration grid and compute
!                          the nsig_ext value the user needs to use
!   2011-01-13 lueken    - corrected init_pass and last_pass indentation
!   2011-01-18 cucurull - increase the size of mreal by one element to add gps_dtype information
!
!   input argument list:
!     lunin    - unit from which to read observations
!     mype     - mpi task id
!     nele     - number of data elements per observation
!     nobs     - number of observations
!
!   output argument list:
!     awork    - array containing information for data counts and gross checks
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use mpeu_util, only: die,perr,tell
  use kinds, only: r_kind,i_kind
  use obsmod, only: gpshead,nprof_gps,grids_dim,gpstail,lobsdiag_allocated,&
       gps_allhead,gps_alltail,i_gps_ob_type,obsdiags,lobsdiagsave,nobskeep,&
       time_offset
  use obsmod, only: gps_ob_type
  use obsmod, only: obs_diag

  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use guess_grids, only: ges_lnprsi,hrdifsig,geop_hgti,nfldsig,&
       ges_z,ges_tv,ges_q
  use guess_grids, only: nsig_ext,gpstop
  use gridmod, only: nsig
  use gridmod, only: get_ij,latlon11
  use constants, only: fv,n_a,n_b,n_c,deg2rad,tiny_r_kind,r0_01
  use constants, only: zero,half,one,two,eccentricity,semi_major_axis,&
       grav_equator,somigliana,flattening,grav_ratio,grav,rd,eps,three,four,five
  use lagmod, only: setq, setq_TL
  use lagmod, only: slagdw, slagdw_TL
  use jfunc, only: jiter,miter
  use convinfo, only: cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use m_dtime, only: dtime_setup, dtime_check, dtime_show

  use m_gpsrhs, only: muse
  use m_gpsrhs, only: dbend_loc,xj
  use m_gpsrhs, only: n_t,n_q,n_p,nrefges
  use m_gpsrhs, only: rges,gp2gm,prsltmp_o,tges_o
  use m_gpsrhs, only: error,error_adjst
  use m_gpsrhs, only: ratio_errors
  use m_gpsrhs, only: rdiagbuf,cdiagbuf
  use m_gpsrhs, only: qcfail
  use m_gpsrhs, only: qcfail_loc,qcfail_high,qcfail_gross
  use m_gpsrhs, only: qcfail_stats_1,qcfail_stats_2
  use m_gpsrhs, only: data_ier,data_igps,data_ihgt
  use m_gpsrhs, only: gpsrhs_alloc
  use m_gpsrhs, only: gpsrhs_dealloc
  use m_gpsrhs, only: gpsrhs_aliases
  use m_gpsrhs, only: gpsrhs_unaliases
  implicit none

! Declare passed variables
  integer(i_kind)                         ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)      ,intent(inout) :: awork
  real(r_kind),dimension(max(1,nprof_gps)),intent(inout) :: toss_gps_sub

  integer, intent(in):: is		! index to GPSbend buffer variables
  logical, intent(in):: init_pass	! flag the pass for the first background bin
  logical, intent(in):: last_pass	! flag the pass for the last background bin

! Declare local parameters
  real(r_kind),parameter::  r240 = 240.0_r_kind
  real(r_kind),parameter:: six = 6.0_r_kind
  real(r_kind),parameter:: ten = 10.0_r_kind
  real(r_kind),parameter:: eight = 8.0_r_kind
  real(r_kind),parameter:: nine = 9.0_r_kind
  real(r_kind),parameter:: eleven = 11.0_r_kind
!  real(r_kind),parameter:: ds=5000.0_r_kind
  real(r_kind),parameter:: ds=10000.0_r_kind
  real(r_kind),parameter:: r12=12.0_r_kind
  real(r_kind),parameter:: r18=18.0_r_kind
  real(r_kind),parameter:: r20=20.0_r_kind
  real(r_kind),parameter:: r40=40.0_r_kind
  real(r_kind),parameter:: r1em3 = 1.0e-3_r_kind
  real(r_kind),parameter:: r1em6 = 1.0e-6_r_kind
  character(len=*),parameter :: myname='setupbend'

! Declare local variables

  real(r_kind) cutoff,cutoff1,cutoff2,cutoff3,cutoff12,cutoff23,cutoff4,cutoff34
  real(r_kind) sin2,zsges
  real(r_kind),dimension(grids_dim):: ddnj,grid_s,ref_rad_s

  real(r_kind) rsig,rsig_up,ddbend,tmean,qmean
  real(r_kind) termg,termr,termrg,hob,dbend
  real(r_kind) fact,pw,nrefges1,nrefges2,nrefges3,k4,delz
  real(r_kind) ratio,residual,obserror,obserrlm
  real(r_kind) errinv_input,errinv_adjst,errinv_final,err_final,repe_gps

  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(nsig):: dbenddn,dbenddxi
  real(r_kind) pressure,hob_s,d_ref_rad,d_ref_rad_TL,hob_s_top
  real(r_kind),dimension(4) :: w4,dw4,dw4_TL
  
  integer(i_kind) ier,ilon,ilat,ihgt,igps,itime,ikx,iuse, &
                  iprof,ipctc,iroc,isatid,iptid,ilate,ilone,ioff,igeoid
  integer(i_kind) i,j,k,kk,mreal,nreal,jj,ikxx,ibin
  integer(i_kind) mm1,nsig_up,ihob,istatus
  integer(i_kind) kprof,istat,jprof,k1,k2,nobs_out
  integer(i_kind),dimension(4) :: gps_ij
  integer(i_kind):: satellite_id,transmitter_id

  real(r_kind),dimension(3,nsig+nsig_ext) :: q_w,q_w_tl
  real(r_kind),dimension(nsig) :: hges,irefges,zges
  real(r_kind),dimension(nsig+1) :: prsltmp
  real(r_kind),dimension(nsig,nsig)::dhdp,dndp,dxidp
  real(r_kind),dimension(nsig,nsig)::dhdt,dndt,dxidt,dndq,dxidq
  real(r_kind),dimension(nsig+nsig_ext) :: n_TL
  real(r_kind),dimension(0:nsig+nsig_ext+1) :: ref_rad,xi_TL
  real(r_kind),dimension(nsig+nsig_ext+20) :: ref_rad_out
  real(r_kind) :: dlat,dlate,dlon,rocprof,unprof,dtime,dpressure,trefges
  real(r_kind) :: dbetan,dbetaxi,rdog,elev,alt
  real(r_kind),dimension(nsig):: tges,qges
  real(r_kind),dimension(nobs):: tpdpres


  logical,dimension(nobs):: luse

  logical:: in_curbin, in_anybin, skipiobs,obs_check
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  type(gps_ob_type),pointer:: my_head
  type(obs_diag),pointer:: my_diag

!*******************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse


  ier=1        ! index of obs error in data array (now 1/(original obs error))
  ilon=2       ! index of grid relative obs location (x)
  ilat=3       ! index of grid relative obs location (y)
  ihgt=4       ! index of obs vertical coordinate in data array
  igps=5       ! index of gps data (or residual) in data array
  itime=6      ! index of obs time in data array
  ikxx=7       ! index of observation type
  iprof=8      ! index of profile
  ipctc=9      ! index of percent confidence
  iroc=10      ! index of local radius of curvature (m)
  isatid=11    ! index of satellite identifier
  iptid=12     ! index of platform transmitter id number
  iuse=13      ! index of use parameter
  ilone=14     ! index of earth relative longitude (degrees)
  ilate=15     ! index of earth relative latitude (degrees)
  igeoid=16    ! index of geoid undulation (a value per profile, m) 

! Intialize variables
  nsig_up=nsig+nsig_ext ! extend nsig_ext levels above interface level nsig
  rsig=float(nsig)
  rdog=rd/grav
  rsig_up=float(nsig_up)
  nobs_out=0
  hob_s_top=one
  mm1=mype+1

! Allocate arrays for output to diagnostic file
  mreal=20
  nreal=mreal
  if (lobsdiagsave) nreal=nreal+4*miter+1

  if(init_pass) call gpsrhs_alloc(is,'bend',nobs,nsig,nreal,grids_dim,nsig_ext)
  call gpsrhs_aliases(is)
  if(nreal/=size(rdiagbuf,1)) then
     call perr(myname,'unexpected dimension')
     call perr(myname,'nreal =',nreal)
     call perr(myname,'size(rdiagbuf,1) =',size(rdiagbuf,1))
     call die(myname)
  endif

  if(init_pass) then
     ! initialize explicitly workspace variables,
     ! although they have been initialized during the
     ! buffer_alloc() call.

     data_ier (:)=data(ier ,:)
     data_ihgt(:)=data(ihgt,:)
     data_igps(:)=data(igps,:)
     muse(:)=.false.

     qcfail=.false.
     qcfail_loc=zero;qcfail_gross=zero;qcfail_stats_1=zero;qcfail_stats_2=zero
     qcfail_high=zero
     toss_gps_sub=zero 
     dbend_loc=zero

  else ! (init_pass)

!    Restore those arrays saved from the previous pass

     data(ier ,:)=data_ier (:)
     data(ihgt,:)=data_ihgt(:)
     data(igps,:)=data_igps(:)
  endif ! (init_pass)

! define new equally spaced grid s
  do j=0,grids_dim-1
     grid_s(j+1)=j*ds
  enddo

! A loop over all obs.
  call dtime_setup()
  loopoverobs1: &
  do i=1,nobs ! loop over obs 
     dtime=data(itime,i)
     obs_check=.false. 

     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle	! not interested, not even for ibin
     if(.not.in_curbin) cycle	! skip rest of lookp

     muse(i)=nint(data(iuse,i)) <= jiter
     sin2  = sin(data(ilate,i)*deg2rad)**2
     dlat=data(ilat,i)
     dlon=data(ilon,i)
     rocprof=data(iroc,i)
     unprof=data(igeoid,i)
     tpdpres(i)=data(ihgt,i)
     ikx=nint(data(ikxx,i))

!    Interpolate log(pres),temperature,specific humidity, 
!    corrected geopotential heights and topography to obs location
     call tintrp2a(ges_lnprsi,prsltmp,dlat,dlon,dtime,hrdifsig,&
        1,nsig+1,mype,nfldsig)
     call tintrp2a(ges_tv,tges,dlat,dlon,dtime,hrdifsig,&
        1,nsig,mype,nfldsig)
     call tintrp2a(ges_q,qges,dlat,dlon,dtime,hrdifsig,&
        1,nsig,mype,nfldsig)
     call tintrp2a(geop_hgti,hges,dlat,dlon,dtime,hrdifsig,&
          1,nsig+1,mype,nfldsig)
     call tintrp2a(ges_z,zsges,dlat,dlon,dtime,hrdifsig,&
          1,1,mype,nfldsig)

     prsltmp_o(1:nsig,i)=prsltmp(1:nsig) ! needed in minimization

! Compute refractivity index-radius product at interface
!
!    Convert geopotential height at layer midpoints to geometric height using
!    equations (17, 20, 23) in MJ Mahoney's note "A discussion of various
!    measures of altitude" (2001).  Available on the web at
!    http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
!    termg  = equation 17
!    termr  = equation 21
!    termrg = first term in the denominator of equation 23
!    zges   = equation 23
 
     termg = grav_equator * &
             ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
     termr = semi_major_axis / (one + flattening + grav_ratio - two*flattening*sin2)
     termrg = (termg/grav)*termr

     do k=1,nsig 
        zges(k) = (termr*hges(k)) / (termrg-hges(k))  ! eq (23) at interface (topo corrected)
        gp2gm(k,i)= termr/(termrg-hges(k))+((termr*hges(k))/(termrg-hges(k))**2)
        rges(k,i) = zges(k) + zsges + unprof + rocprof   ! radius r_i

        if(k>1) then
           qmean=(qges(k)+qges(k-1))/two
           tmean=(tges(k)+tges(k-1))/two
        else
           qmean=qges(1)
           tmean=tges(1)
        endif
        tges_o(k,i)=tmean  ! needed in minimization
        fact=(one+fv*qmean)
        pw=eps+qmean*(one-eps)
        k4=n_c-n_a
        pressure=ten*exp(prsltmp(k)) ! pressure of interface level in mb
        nrefges1=n_a*(pressure/tmean)*fact
        nrefges2=n_b*qmean*pressure*fact**2/(tmean**2*pw)
        nrefges3=k4*fact*qmean*pressure/(tmean*pw)
        nrefges(k,i)=nrefges1+nrefges2+nrefges3 ! refractivity N_i
        irefges(k)= one+(r1em6*nrefges(k,i))    ! index of refractivity n_i
        ref_rad(k)=irefges(k)*rges(k,i)         ! refractivity index-radius product x_i
 
!       Terms for the minimization
        n_q(k,i)= &
           n_a*(pressure/tmean)*fv+&
           (n_b/(tmean**2*pw))*pressure*fact*(fact+qmean*two*fv)-&
           (n_b/(tmean**2*pw**2))*pressure*qmean*fact**2*(one-eps)+&
           (k4*pressure/(tmean*pw))*(fv*qmean+fact)-&
           (k4/(tmean*pw**2))*fact*qmean*pressure*(one-eps)
        n_p(k,i)= &
           ten*((n_a/tmean)*fact+&
           (n_b/(tmean**2*pw))*qmean*fact**2+&
           (k4/(tmean*pw))*qmean*fact)
        n_t(k,i)= &
           -n_a*fact*(pressure/tmean**2)-n_b*qmean*fact**2*two*&
           (pressure/(tmean**3*pw))-&
           (k4/(tmean**2*pw))*fact*qmean*pressure
     end do

!    Modify error to account for representativeness error. 

     repe_gps=one
     alt=(tpdpres(i)-rocprof)*r1em3

! UKMET-type processing

     if((data(isatid,i)==41).or.(data(isatid,i)==722).or.&
      (data(isatid,i)==723).or.(data(isatid,i)==4).or.(data(isatid,i)==42)) then
                    
        if((data(ilate,i)> r40).or.(data(ilate,i)< -r40)) then
           if(alt>r12) then
              repe_gps=0.19032_r_kind+0.287535_r_kind*alt-0.00260813_r_kind*alt**2
           else
              repe_gps=-3.20978_r_kind+1.26964_r_kind*alt-0.0622538_r_kind*alt**2 
           endif
        else
           if(alt>r18) then
              repe_gps=-1.87788_r_kind+0.354718_r_kind*alt-0.00313189_r_kind*alt**2
           else
              repe_gps=-2.41024_r_kind+0.806594_r_kind*alt-0.027257_r_kind*alt**2
           endif
        endif
     else 
! CDAAC-type processing

        if((data(ilate,i)> r40).or.(data(ilate,i)< -r40)) then
           if(alt>r12) then
              repe_gps=-0.685627_r_kind+0.377174_r_kind*alt-0.00421934_r_kind*alt**2
           else
              repe_gps=-3.27737_r_kind+1.20003_r_kind*alt-0.0558024_r_kind*alt**2
           endif
        else
           if(alt>r18) then
              repe_gps=-2.73867_r_kind+0.447663_r_kind*alt-0.00475603_r_kind*alt**2
           else
              repe_gps=-3.45303_r_kind+0.908216_r_kind*alt-0.0293331_r_kind*alt**2
          endif
        endif
     endif

     repe_gps=exp(repe_gps) ! one/modified error in (rad-1*1E3)
     repe_gps= r1em3*(one/abs(repe_gps)) ! modified error in rad
     ratio_errors(i) = data(ier,i)/abs(repe_gps)
  
     error(i)=one/data(ier,i) ! one/original error
     data(ier,i)=one/data(ier,i) ! one/original error
     error_adjst(i)= ratio_errors(i)* data(ier,i) !one/adjusted error

!    Remove observation if below surface or at/above the top layer
!    of the model by setting observation (1/error) to zero.
!    Make no adjustment if observation falls within vertical
!    domain.

     hob=tpdpres(i)
     call grdcrd(hob,1,ref_rad(1),nsig,1)
     if (hob<one .or. hob>rsig) then 
        data(ier,i) = zero
        ratio_errors(i) = zero
        muse(i)=.false.
        qcfail_loc(i)=one
     endif 

!    Increment obs counter along with low and high obs counters
     if(luse(i))then
        awork(1)=awork(1)+one
        if(hob > rsig) awork(3)=awork(3)+one
        if(hob <  one) awork(2)=awork(2)+one
     endif

!    Save some diagnostic information
!    occultation identification
     satellite_id         = data(isatid,i) ! receiver occ id
     transmitter_id       = data(iptid,i)  ! transmitter occ id
     write(cdiagbuf(i),'(2(i4.4))') satellite_id,transmitter_id

     rdiagbuf(:,i)         = zero

     rdiagbuf(1,i)         = ictype(ikx)       ! observation type
     rdiagbuf(20,i)        = one               ! uses gps_ref (one = use of bending angle)
     rdiagbuf(2,i)         = data(iprof,i)     ! profile identifier
     rdiagbuf(3,i)         = data(ilate,i)     ! lat in degrees
     rdiagbuf(4,i)         = data(ilone,i)     ! lon in degrees
     rdiagbuf(7,i)         = tpdpres(i)-rocprof   ! impact height in meters
!    rdiagbuf(7,i)         = tpdpres(i)           ! impact parameter in meters
     rdiagbuf(8,i)         = dtime-time_offset ! obs time (hours relative to analysis time)
!    rdiagbuf(9,i)         = data(ipctc,i)     ! input bufr qc - index of per cent confidence
     rdiagbuf(9,i)         = zsges             ! model terrain (m) 
     rdiagbuf(11,i)        = data(iuse,i)      ! data usage flag
     rdiagbuf(19,i)        = hob               ! model vertical grid (interface)
 
     if (ratio_errors(i) > tiny_r_kind)  then ! obs inside model grid

!       get pressure (in mb) and temperature at obs location    
        call tintrp3(ges_lnprsi,dpressure,dlat,dlon,hob,&
               dtime,hrdifsig,1,mype,nfldsig)
        ihob=hob
        k1=min(max(1,ihob),nsig)
        k2=max(1,min(ihob+1,nsig))
        delz=hob-float(k1)
        delz=max(zero,min(delz,one))
        trefges=tges_o(k1,i)*(one-delz)+tges_o(k2,i)*delz

!       Extending atmosphere above interface level nsig
        d_ref_rad=ref_rad(nsig)-ref_rad(nsig-1)
        do k=1,nsig_ext
           ref_rad(nsig+k)=ref_rad(nsig)+ k*d_ref_rad ! extended x_i
           nrefges(nsig+k,i)=nrefges(nsig+k-1,i)**2/nrefges(nsig+k-2,i) ! exended N_i
        end do

!       Lagrange coefficients
        ref_rad(0)=ref_rad(3)
        ref_rad(nsig_up+1)=ref_rad(nsig_up-2)
        do k=1,nsig_up
           call setq(q_w(:,k),ref_rad(k-1:k+1),3)
        enddo

        muse(i)=nint(data(iuse,i)) <= jiter

!       Get refractivity index-radius and [d(ln(n))/dx] in new grid.
        intloop: do j=1,grids_dim
           ref_rad_s(j)=sqrt(grid_s(j)*grid_s(j)+tpdpres(i)*tpdpres(i)) !x_j
           xj(j,i)=ref_rad_s(j)
           hob_s=ref_rad_s(j)
           call grdcrd(hob_s,1,ref_rad(1),nsig_up,1)
           dbend_loc(j,i)=hob_s  !location of x_j with respect to extended x_i
 
           if (hob_s < rsig_up) then  !obs insided the new grid
              ihob=hob_s

!             Compute refractivity and derivative at target points 
!             using Lagrange interpolators
              call slagdw(ref_rad(ihob-1:ihob+2),ref_rad_s(j),&
                   q_w(:,ihob),q_w(:,ihob+1),&
                   w4,dw4,4)
              if(ihob==1) then
                 w4(4)=w4(4)+w4(1); w4(1:3)=w4(2:4);w4(4)=zero
                 dw4(4)=dw4(4)+dw4(1);dw4(1:3)=dw4(2:4);dw4(4)=zero
                 ihob=ihob+1
              endif
              if(ihob==nsig_up-1) then
                 w4(1)=w4(1)+w4(4); w4(2:4)=w4(1:3);w4(1)=zero
                 dw4(1)=dw4(1)+dw4(4); dw4(2:4)=dw4(1:3);dw4(1)=zero
                 ihob=ihob-1
              endif
              ddnj(j)=dot_product(dw4,nrefges(ihob-1:ihob+2,i))!derivative (dN/dx)_j
              ddnj(j)=max(zero,abs(ddnj(j)))
           else
              obs_check=.true.
              if (obs_check) then ! only once per obs here
                 nobs_out=nobs_out+1
                 d_ref_rad=ref_rad(nsig)-ref_rad(nsig-1)
                 do kk=1,20
                    ref_rad_out(nsig_up+kk)=ref_rad(nsig_up)+ kk*d_ref_rad ! extended x_i
                 end do
                 do kk=1,nsig_up
                    ref_rad_out(kk)=ref_rad(kk)
                 enddo
              endif
              hob_s=ref_rad_s(j)
              call grdcrd(hob_s,1,ref_rad_out,nsig_up+20,1)
              hob_s_top=max(hob_s,hob_s_top) 
           endif !obs in new grid
        end do intloop
        if (obs_check) goto 3000 ! reject observation 

!       bending angle (radians)
        dbend=ds*ddnj(1)/ref_rad_s(1)
        do j=2,grids_dim
           ddbend=ds*ddnj(j)/ref_rad_s(j)
           dbend=dbend+two*ddbend
        end do
        dbend=r1em6*tpdpres(i)*dbend  

!       Accumulate diagnostic information
        rdiagbuf( 5,i)  = (data(igps,i)-dbend)/data(igps,i) ! incremental bending angle (x100 %)
        rdiagbuf(17,i)  = data(igps,i)  ! bending angle observation (radians)
        rdiagbuf( 6,i)  = ten*exp(dpressure) ! pressure at obs location (hPa)
        rdiagbuf(18,i)  = trefges ! temperature at obs location (Kelvin)

        data(igps,i)=data(igps,i)-dbend !innovation vector
        data(ihgt,i)=hob

        if(alt <= gpstop) then ! go into qc checks

!          Gross error check
           obserror = one/max(ratio_errors(i)*data(ier,i),tiny_r_kind)
           obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
           residual = abs(data(igps,i))
           ratio    = residual/obserrlm

           if (ratio > cgross(ikx)) then
              if (luse(i)) then
                 awork(4) = awork(4)+one
              endif
              qcfail_gross(i)=one 
              data(ier,i) = zero
              ratio_errors(i) = zero
              muse(i)=.false.
           else
!             Statistics QC check if obs passed gross error check
              cutoff=zero
              cutoff1=(-4.725_r_kind+0.045_r_kind*alt+0.005_r_kind*alt**2)*two/three
              cutoff2=1.5_r_kind+one*cos(data(ilate,i)*deg2rad)
              if(trefges<=r240) then
                 cutoff3=two
              else
                 cutoff3=0.005_r_kind*trefges**2-2.3_r_kind*trefges+266_r_kind
              endif
              cutoff3=cutoff3*two/three
              cutoff4=(four+eight*cos(data(ilate,i)*deg2rad))*two/three

              cutoff12=((36_r_kind-alt)/two)*cutoff2+&
                       ((alt-34_r_kind)/two)*cutoff1
              cutoff23=((eleven-alt)/two)*cutoff3+&
                       ((alt-nine)/two)*cutoff2
              cutoff34=((six-alt)/two)*cutoff4+&
                       ((alt-four)/two)*cutoff3

              if(alt>36_r_kind) cutoff=cutoff1
              if((alt<=36_r_kind).and.(alt>34_r_kind)) cutoff=cutoff12
              if((alt<=34_r_kind).and.(alt>eleven)) cutoff=cutoff2
              if((alt<=eleven).and.(alt>nine)) cutoff=cutoff23
              if((alt<=nine).and.(alt>six)) cutoff=cutoff3
              if((alt<=six).and.(alt>four)) cutoff=cutoff34
              if(alt<=four) cutoff=cutoff4

              cutoff=three*cutoff*r0_01

              if(abs(rdiagbuf(5,i)) > cutoff) then
                 qcfail(i)=.true.
                 qcfail_stats_1(i)=one
                 data(ier,i) = zero
                 ratio_errors(i) = zero
                 muse(i) = .false.
              end if
           end if ! gross qc check

        end if ! qc checks (only below 50km)


!       Remove obs above 50 km  
        if(alt > gpstop) then
           data(ier,i) = zero
           ratio_errors(i) = zero
           qcfail_high(i)=one
           muse(i)=.false.
        endif

     end if ! obs inside the vertical grid

3000 continue
  end do loopoverobs1 ! end of loop over observations

  if (nobs_out>=1) then
     write(6,*)'WARNING GPSRO:',nobs_out,'obs outside integration grid. Increase nsig_ext to',&
     int(hob_s_top)-nsig+1
  endif

! Loop over observation profiles. Compute penalty
! terms, and accumulate statistics.
  if(last_pass) then
     do i=1,nobs

        if(qcfail(i)) then
           kprof = data(iprof,i)
           do j=1,nobs
              jprof = data(iprof,j)
              if( kprof == jprof .and. .not. qcfail(j))then
 
!          Remove data below
                 if(r1em3*rdiagbuf(7,j) < r1em3*rdiagbuf(7,i))then
                    if((rdiagbuf(1,i)==41).or.(rdiagbuf(1,i)==722).or.&
                       (rdiagbuf(1,i)==723).or.(rdiagbuf(1,i)==4).or.(rdiagbuf(1,i)==786)) then
                       if(r1em3*rdiagbuf(7,i)<= ten) then
                          qcfail(j) = .true.
                          qcfail_stats_2(j)=one
                       endif
                    else
                       if(r1em3*rdiagbuf(7,i)< five) then
                          qcfail(j) = .true.
                          qcfail_stats_2(j)=one
                       endif
                    endif
                 endif
              end if
           end do
        endif
     end do

     do i=1,nobs

        alt=r1em3*rdiagbuf(7,i) ! impact height in km
        if(qcfail(i)) then
           kprof = data(iprof,i)
           data(ier,i) = zero
           ratio_errors(i) = zero
           muse(i) = .false.
           if ( (rdiagbuf(1,i)==41).or.(rdiagbuf(1,i)==722).or.&
                (rdiagbuf(1,i)==723).or.(rdiagbuf(1,i)==4).or.(rdiagbuf(1,i)==786)) then
              if(alt<=ten) then
                 toss_gps_sub(kprof) = max(toss_gps_sub(kprof),data(ihgt,i))
              endif
           else
              if (alt < five) then
                 toss_gps_sub(kprof) = max(toss_gps_sub(kprof),data(ihgt,i))
              end if
           end if


!          Counting obs tossed due to the stats qc check
!          This calculation will be updated in genstats_gps due to toss_gps_sub
 
           if (luse(i))then
              if(data(ilate,i)> r20) then
                 awork(22) = awork(22)+one                !NH
              else if(data(ilate,i)< -r20)then
                 awork(23) = awork(23)+one                !SH
              else
                 awork(24) = awork(24)+one                !TR
              end if
           end if
        end if
     end do
  endif ! (last_pass)


! Loop to load arrays used in statistics output
  n_alloc(:)=0
  m_alloc(:)=0
  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime,in_curbin,in_anybin)
     if(.not.in_anybin) cycle

     if(last_pass) then
        if (ratio_errors(i)*data(ier,i) <= tiny_r_kind) muse(i) = .false.
        ikx=nint(data(ikxx,i))

 
        ! flags for observations that failed qc checks
        ! zero = observation is good
 
        if(qcfail_gross(i) == one)   rdiagbuf(10,i) = three
        if(qcfail_stats_1(i) == one) rdiagbuf(10,i) = four
        if(qcfail_stats_2(i) == one) rdiagbuf(10,i) = five !modified in genstats due to toss_gps_sub
        if(qcfail_loc(i) == one)     rdiagbuf(10,i) = one
        if(qcfail_high(i) == one)    rdiagbuf(10,i) = two

        if(muse(i)) then                    ! modified in genstats_gps due to toss_gps_sub
           rdiagbuf(12,i) = one             ! minimization usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,i) = -one
        endif

        if (ratio_errors(i)*data(ier,i)>tiny_r_kind) then
           err_final = ratio_errors(i)*data(ier,i)
        else
           err_final = tiny_r_kind
        endif

        errinv_input  = tiny_r_kind
        errinv_adjst  = tiny_r_kind
        errinv_final  = tiny_r_kind


        if (error(i)>tiny_r_kind)       errinv_input=error(i)
        if (error_adjst(i)>tiny_r_kind) errinv_adjst=error_adjst(i)
        if (err_final>tiny_r_kind)      errinv_final=err_final

        rdiagbuf(13,i) = zero ! nonlinear qc relative weight - will be defined in genstats_gps
        rdiagbuf(14,i) = errinv_input ! original inverse gps obs error (rad**-1)
        rdiagbuf(15,i) = errinv_adjst ! original + represent error inverse gps 
                                      ! obs error (rad**-1)
        rdiagbuf(16,i) = errinv_final ! final inverse observation error due to 
                                      ! superob factor (rad**-1) and qc
                                      ! modified in genstats_gps
     endif ! (last_pass)

!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins, ibin=',nobs_bins,ibin

!    Link obs to diagnostics structure
     if (.not.lobsdiag_allocated) then
        if (.not.associated(obsdiags(i_gps_ob_type,ibin)%head)) then
           allocate(obsdiags(i_gps_ob_type,ibin)%head,stat=istat)
           if (istat/=0) then
              write(6,*)'setupbend: failure to allocate obsdiags',istat
              call stop2(250)
           end if
           obsdiags(i_gps_ob_type,ibin)%tail => obsdiags(i_gps_ob_type,ibin)%head
        else
           allocate(obsdiags(i_gps_ob_type,ibin)%tail%next,stat=istat)
           if (istat/=0) then
              write(6,*)'setupbend: failure to allocate obsdiags',istat
              call stop2(251)
           end if
           obsdiags(i_gps_ob_type,ibin)%tail => obsdiags(i_gps_ob_type,ibin)%tail%next
        end if
        allocate(obsdiags(i_gps_ob_type,ibin)%tail%muse(miter+1))
        allocate(obsdiags(i_gps_ob_type,ibin)%tail%nldepart(miter+1))
        allocate(obsdiags(i_gps_ob_type,ibin)%tail%tldepart(miter))
        allocate(obsdiags(i_gps_ob_type,ibin)%tail%obssen(miter))
        obsdiags(i_gps_ob_type,ibin)%tail%indxglb=i
        obsdiags(i_gps_ob_type,ibin)%tail%nchnperobs=-99999
        obsdiags(i_gps_ob_type,ibin)%tail%luse=.false.
        obsdiags(i_gps_ob_type,ibin)%tail%muse(:)=.false.
        obsdiags(i_gps_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
        obsdiags(i_gps_ob_type,ibin)%tail%tldepart(:)=zero
        obsdiags(i_gps_ob_type,ibin)%tail%wgtjo=-huge(zero)
        obsdiags(i_gps_ob_type,ibin)%tail%obssen(:)=zero

        n_alloc(ibin) = n_alloc(ibin) +1
        obsdiags(i_gps_ob_type,ibin)%tail%idv = is
        obsdiags(i_gps_ob_type,ibin)%tail%iob = i
        obsdiags(i_gps_ob_type,ibin)%tail%ich = 1

     else
        if (.not.associated(obsdiags(i_gps_ob_type,ibin)%tail)) then
           obsdiags(i_gps_ob_type,ibin)%tail => obsdiags(i_gps_ob_type,ibin)%head
        else
           obsdiags(i_gps_ob_type,ibin)%tail => obsdiags(i_gps_ob_type,ibin)%tail%next
        end if
        if (obsdiags(i_gps_ob_type,ibin)%tail%indxglb/=i) then
           write(6,*)'setupbend: index error'
           call stop2(252)
        end if
     endif

     if(last_pass) then
        if (nobskeep>0) muse(i)=obsdiags(i_gps_ob_type,ibin)%tail%muse(nobskeep)

!       Save values needed for generate of statistics for all observations
        if(.not. associated(gps_allhead(ibin)%head))then
           allocate(gps_allhead(ibin)%head,stat=istat)
           if(istat /= 0)write(6,*)' failure to write gps_allhead '
           gps_alltail(ibin)%head => gps_allhead(ibin)%head
        else
           allocate(gps_alltail(ibin)%head%llpoint,stat=istat)
           if(istat /= 0)write(6,*)' failure to write gps_alltail%llpoint '
           gps_alltail(ibin)%head => gps_alltail(ibin)%head%llpoint
        end if
        gps_alltail(ibin)%head%idv=is
        gps_alltail(ibin)%head%iob=i

        allocate(gps_alltail(ibin)%head%rdiag(nreal),stat=istatus)
        if (istatus/=0) write(6,*)'SETUPBEND:  allocate error for gps_alldiag, istatus=',istatus

        gps_alltail(ibin)%head%ratio_err= ratio_errors(i)
        gps_alltail(ibin)%head%obserr   = data(ier,i)
        gps_alltail(ibin)%head%dataerr  = data(ier,i)*data(igps,i)
        gps_alltail(ibin)%head%pg       = cvar_pg(ikx)
        gps_alltail(ibin)%head%b        = cvar_b(ikx)
        gps_alltail(ibin)%head%loc      = data(ihgt,i)
        gps_alltail(ibin)%head%kprof    = data(iprof,i)
        gps_alltail(ibin)%head%type     = data(ikxx,i)
        gps_alltail(ibin)%head%luse     = luse(i) ! logical
        gps_alltail(ibin)%head%muse     = muse(i) ! logical
        gps_alltail(ibin)%head%cdiag    = cdiagbuf(i)

!       Fill obs diagnostics structure
        obsdiags(i_gps_ob_type,ibin)%tail%luse=luse(i)
        obsdiags(i_gps_ob_type,ibin)%tail%muse(jiter)=muse(i)
        obsdiags(i_gps_ob_type,ibin)%tail%nldepart(jiter)=data(igps,i)
        obsdiags(i_gps_ob_type,ibin)%tail%wgtjo=(data(ier,i)*ratio_errors(i))**2

!       Load additional obs diagnostic structure
        if (lobsdiagsave) then
           ioff=mreal
           do jj=1,miter
              ioff=ioff+1
              if (obsdiags(i_gps_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(ioff,i) = one
              else
                 rdiagbuf(ioff,i) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,i) = obsdiags(i_gps_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,i) = obsdiags(i_gps_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,i) = obsdiags(i_gps_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif
 
        do j=1,nreal
           gps_alltail(ibin)%head%rdiag(j)= rdiagbuf(j,i)
        end do

! If obs is "acceptable", load array with obs info for use
! in inner loop minimization (int* and stp* routines)

        if ( muse(i) ) then

           if(.not. associated(gpshead(ibin)%head))then
              allocate(gpshead(ibin)%head,stat=istat)
              if(istat /= 0)write(6,*)' failure to write gpshead '
              gpstail(ibin)%head => gpshead(ibin)%head
           else
              allocate(gpstail(ibin)%head%llpoint,stat=istat)
              if(istat /= 0)write(6,*)' failure to write gpstail%llpoint '
              gpstail(ibin)%head => gpstail(ibin)%head%llpoint
           end if
           m_alloc(ibin) = m_alloc(ibin)+1
           gpstail(ibin)%head%idv=is
           gpstail(ibin)%head%iob=i

           allocate(gpstail(ibin)%head%jac_t(nsig),gpstail(ibin)%head%jac_q(nsig), &
                    gpstail(ibin)%head%jac_p(nsig+1),gpstail(ibin)%head%ij(4,nsig),stat=istatus)
           if (istatus/=0) write(6,*)'SETUPBEND:  allocate error for gps_point, istatus=',istatus

           gps_alltail(ibin)%head%mmpoint  => gpstail(ibin)%head
 
!          Inizialize some variables
           dxidt=zero; dxidp=zero; dxidq=zero
           dhdp=zero; dhdt=zero       
           dndt=zero; dndq=zero; dndp=zero

!          Set (i,j) indices of guess gridpoint that bound obs location
           call get_ij(mm1,data(ilat,i),data(ilon,i),gps_ij,gpstail(ibin)%head%wij(1))
 
           do k=1,nsig
 
              gpstail(ibin)%head%ij(1,k)=gps_ij(1)+(k-1)*latlon11
              gpstail(ibin)%head%ij(2,k)=gps_ij(2)+(k-1)*latlon11
              gpstail(ibin)%head%ij(3,k)=gps_ij(3)+(k-1)*latlon11
              gpstail(ibin)%head%ij(4,k)=gps_ij(4)+(k-1)*latlon11
 
              if(k > 1) then
                 do j=2,k
                    dhdt(k,j-1)= rdog*(prsltmp_o(j-1,i)-prsltmp_o(j,i))
                    dhdp(k,j)= dhdp(k,j)-rdog*(tges_o(j-1,i)/exp(prsltmp_o(j,i)))
                    dhdp(k,j-1)=dhdp(k,j-1)+rdog*(tges_o(j-1,i)/exp(prsltmp_o(j-1,i)))
                 end do
              end if
              if(k == 1)then
                 dndt(k,k)=dndt(k,k)+n_t(k,i)
                 dndq(k,k)=dndq(k,k)+n_q(k,i)
                 dndp(k,k)=dndp(k,k)+n_p(k,i)
              else
                 dndt(k,k)=dndt(k,k)+half*n_t(k,i)
                 dndt(k,k-1)=dndt(k,k-1)+half*n_t(k,i)
                 dndq(k,k)=dndq(k,k)+half*n_q(k,i)
                 dndq(k,k-1)=dndq(k,k-1)+half*n_q(k,i)
                 dndp(k,k)=n_p(k,i)
              end if
           end do

           do k=1,nsig
              irefges(k)=one+r1em6*nrefges(k,i)
              ref_rad(k)=irefges(k)*rges(k,i)
              do j=1,nsig
                 dxidt(k,j)=r1em6*rges(k,i)*dndt(k,j)+irefges(k)*gp2gm(k,i)*dhdt(k,j)
                 dxidq(k,j)=r1em6*rges(k,i)*dndq(k,j)
                 dxidp(k,j)=r1em6*rges(k,i)*dndp(k,j)+irefges(k)*gp2gm(k,i)*dhdp(k,j)
              end do
           end do
           d_ref_rad=ref_rad(nsig)-ref_rad(nsig-1)
           do k=1,nsig_ext
              ref_rad(nsig+k)=ref_rad(nsig) + k*d_ref_rad
           end do
           ref_rad(0)=ref_rad(3)
           ref_rad(nsig_up+1)=ref_rad(nsig_up-2)
           do kk=1,nsig
              xi_TL=zero
              xi_TL(kk)=one
              n_TL=zero
              n_TL(kk)=one
              q_w=zero
              q_w_TL=zero
              d_ref_rad_TL=xi_TL(nsig)-xi_TL(nsig-1)
              do k=1,nsig_ext
                 xi_TL(nsig+k)=xi_TL(nsig)+ k*d_ref_rad_TL
                 n_TL(nsig+k)=(two*nrefges(nsig+k-1,i)*n_TL(nsig+k-1)/nrefges(nsig+k-2,i))-&
                     (nrefges(nsig+k-1,i)**2/nrefges(nsig+k-2,i)**2)*n_TL(nsig+k-2)

              end do
              xi_TL(0)=xi_TL(3)
              xi_TL(nsig_up+1)=xi_TL(nsig_up-2)
              do k=1,nsig_up
                 call setq_TL(q_w(:,k),q_w_TL(:,k),ref_rad(k-1:k+1),xi_TL(k-1:k+1),3)
              enddo
              intloop2: do j=1,grids_dim
                 ihob=dbend_loc(j,i)

! Compute refractivity and derivative at target points using Lagrange interpolators
                 call slagdw_TL(ref_rad(ihob-1:ihob+2),xi_TL(ihob-1:ihob+2),xj(j,i),&
                            q_w(:,ihob),q_w_TL(:,ihob),&
                            q_w(:,ihob+1),q_w_TL(:,ihob+1),&
                            dw4,dw4_TL,4)
                 if(ihob==1) then
                    dw4(4)=dw4(4)+dw4(1);dw4(1:3)=dw4(2:4);dw4(4)=zero
                    dw4_TL(4)=dw4_TL(4)+dw4_TL(1);dw4_TL(1:3)=dw4_TL(2:4);dw4_TL(4)=zero
                    ihob=ihob+1
                 endif
                 if(ihob==nsig_up-1) then
                    dw4(1)=dw4(1)+dw4(4); dw4(2:4)=dw4(1:3);dw4(1)=zero
                    dw4_TL(1)=dw4_TL(1)+dw4_TL(4); dw4_TL(2:4)=dw4_TL(1:3);dw4_TL(1)=zero
                    ihob=ihob-1
                 endif
                 dbetaxi=(r1em6/xj(j,i))*dot_product(dw4_TL,nrefges(ihob-1:ihob+2,i))
                 dbetan =(r1em6/xj(j,i))*dot_product(dw4,n_TL(ihob-1:ihob+2))
                 if(j == 1)then
                    dbenddxi(kk)=dbetaxi
                    dbenddn(kk)=dbetan
                 else
                    dbenddxi(kk)=dbenddxi(kk)+two*dbetaxi
                    dbenddn(kk)=dbenddn(kk)+two*dbetan
                 end if
              end do intloop2
           end do
           do k=1,nsig
              dbenddxi(k)=-dbenddxi(k)*ds*tpdpres(i)
              dbenddn(k)=-dbenddn(k)*ds*tpdpres(i)
           end do
           do k=1,nsig
              gpstail(ibin)%head%jac_t(k)=zero
              gpstail(ibin)%head%jac_q(k)=zero
              gpstail(ibin)%head%jac_p(k)=zero
              do j=1,nsig
                 gpstail(ibin)%head%jac_t(k)=gpstail(ibin)%head%jac_t(k)+dbenddxi(j)*dxidt(j,k)+ &
                                                                         dbenddn(j) * dndt(j,k)
                 gpstail(ibin)%head%jac_q(k)=gpstail(ibin)%head%jac_q(k)+dbenddxi(j)*dxidq(j,k)+ &
                                                                         dbenddn(j) * dndq(j,k)
                 gpstail(ibin)%head%jac_p(k)=gpstail(ibin)%head%jac_p(k)+dbenddxi(j)*dxidp(j,k)+ &
                                                                         dbenddn(j) * dndp(j,k)
              end do
           end do

           gpstail(ibin)%head%jac_p(nsig+1) = zero
           gpstail(ibin)%head%raterr2= ratio_errors(i)**2     
           gpstail(ibin)%head%res    = data(igps,i)
           gpstail(ibin)%head%err2   = data(ier,i)**2
           gpstail(ibin)%head%time   = data(itime,i)
           gpstail(ibin)%head%b      = cvar_b(ikx)
           gpstail(ibin)%head%pg     = cvar_pg(ikx)
           gpstail(ibin)%head%luse   = luse(i)

           gpstail(ibin)%head%diags => obsdiags(i_gps_ob_type,ibin)%tail

           my_head => gpstail(ibin)%head
           my_diag => gpstail(ibin)%head%diags
           if(my_head%idv /= my_diag%idv .or. &
              my_head%iob /= my_diag%iob ) then
              call perr(myname,'mismatching %[head,diags]%(idv,iob,ibin) =', &
                    (/is,i,ibin/))
              call perr(myname,'my_head%(idv,iob) =',(/my_head%idv,my_head%iob/))
              call perr(myname,'my_diag%(idv,iob) =',(/my_diag%idv,my_diag%iob/))
              call die(myname)
           endif
        end if
 
     endif ! (last_pass)
  end do ! i=1,nobs

  ! Save these arrays for later passes
  data_ier (:)=data(ier ,:)
  data_ihgt(:)=data(ihgt,:)
  data_igps(:)=data(igps,:)

  call tell(myname,'counts of obs total  =',n_alloc)
  call tell(myname,'counts of obs in use =',m_alloc)
  call dtime_show(myname,'diagsave:bend',i_gps_ob_type)
  call gpsrhs_unaliases(is)
  if(last_pass) call gpsrhs_dealloc(is)

end subroutine setupbend
