subroutine setupbend(lunin,mype,awork,nele,nobs,toss_gps_sub)
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
!   2010-04-01 cucurull - substantial cleaning and uptade
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
  use kinds, only: r_kind,i_kind,r_single
  use obsmod, only: gpshead,nprof_gps,grids_dim,gpstail,lobsdiag_allocated,&
       gps_allhead,gps_alltail,i_gps_ob_type,obsdiags,lobsdiagsave,nobskeep,&
       time_offset
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use guess_grids, only: ges_lnprsi,hrdifsig,geop_hgti,geop_hgtl,nfldsig,&
       ges_z,ges_tv,ges_q
  use gridmod, only: nsig
  use gridmod, only: get_ij,latlon11
  use constants, only: fv,n_a,n_b,deg2rad,tiny_r_kind,huge_single,half
  use constants, only: izero,ione,zero,half,one,two,eccentricity,semi_major_axis,&
       grav_equator,somigliana,flattening,grav_ratio,grav,rd,eps,three,four,five
  use qcmod, only: repe_gps
  use lagmod
  use jfunc, only: jiter,last,miter
  use convinfo, only: cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  implicit none

! Declare passed variables
  integer(i_kind)                         ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100_i_kind+7*nsig)      ,intent(inout) :: awork
  real(r_kind),dimension(max(ione,nprof_gps)),intent(inout) :: toss_gps_sub

! Declare local parameters
  real(r_kind),parameter:: r0_01 = 0.01_r_kind
  real(r_kind),parameter:: r0_8= 0.8_r_kind
  real(r_kind),parameter:: r1_3 = 1.3_r_kind
  real(r_kind),parameter:: ten = 10.0_r_kind
  real(r_kind),parameter:: eight = 8.0_r_kind
  real(r_kind),parameter:: six = 6.0_r_kind
  real(r_kind),parameter:: ds=5000.0_r_kind
  real(r_kind),parameter:: r15=15.0_r_kind
  real(r_kind),parameter:: r30=30.0_r_kind
  real(r_kind),parameter:: r40=40.0_r_kind
  real(r_kind),parameter:: r1em3 = 1.0e-3_r_kind
  real(r_kind),parameter:: r1em6 = 1.0e-6_r_kind
  real(r_kind),parameter:: seven = 7.0_r_kind

! Declare local variables

  real(r_kind) cutoff1,cutoff2,cutoff3,cutoff12,cutoff23
  real(r_kind) sin2,zsges
  real(r_kind),dimension(grids_dim,nobs) :: dbend_loc,xj
  real(r_kind),dimension(grids_dim):: ddnj,grid_s,ref_rad_s

  real(r_kind) rsig,rsig_up,ddbend,tmean,qmean
  real(r_kind) termg,termr,termrg,hob,hobl
  real(r_kind) fact,pw,nrefges1,nrefges2,nrefges3,k4
  real(r_kind) ratio,residual,obserror,obserrlm
  real(r_kind) errinv_input,errinv_adjst,errinv_final,err_final

  real(r_kind),dimension(nobs):: dpres,dpressure,dbend,error,error_adjst
  real(r_kind),dimension(3,nobs):: gps2work
  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(nobs):: ratio_errors,cutoff
  real(r_kind),dimension(nsig):: hgesl  
  real(r_kind),dimension(nsig):: dbenddn,dbenddxi
  real(r_kind) pressure,hob_s,d_ref_rad,d_ref_rad_TL
  real(r_kind),dimension(4) :: w4,dw4,dw4_TL
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  real(r_single),dimension(nobs):: qcfail_loc,qcfail_high,qcfail_gross
  real(r_single),dimension(nobs):: qcfail_stats_1,qcfail_stats_2 
  
  integer(i_kind) ier,ilon,ilat,ihgt,igps,itime,ikx,iuse, &
                  iprof,ipctc,iroc,isatid,iptid,ilate,ilone,idia,ioff,igeoid
  integer(i_kind) i,j,k,kk,mreal,nreal,jj,ikxx,ibin
  integer(i_kind) mm1,nsig_up,ihob,istatus
  integer(i_kind) kprof,istat,jprof
  integer(i_kind),dimension(4) :: gps_ij
  integer(i_kind):: satellite_id,transmitter_id

  real(r_kind),dimension(3,nsig+10_i_kind) :: q_w,q_w_tl
  real(r_kind),dimension(nsig,nobs):: n_t,n_q,n_p,rges,gp2gm,prsltmp_o,tges_o
  real(r_kind),dimension(nsig) :: hges,irefges,zges
  real(r_kind),dimension(nsig+ione) :: prsltmp
  real(r_kind),dimension(nsig,nsig)::dhdp,dndp,dxidp
  real(r_kind),dimension(nsig,nsig)::dhdt,dndt,dxidt,dndq,dxidq
  real(r_kind),dimension(nsig+10_i_kind) :: n_TL
  real(r_kind),dimension(0:nsig+11_i_kind) :: ref_rad,xi_TL
  real(r_kind),dimension(nsig+10_i_kind,nobs):: nrefges
  real(r_kind) :: dlat,dlate,dlon,rocprof,unprof,tpdpres,dtime,dpress
  real(r_kind) :: dbetan,dbetaxi,rdog,elev
  real(r_kind),dimension(nsig):: tges,qges

  character(8),allocatable,dimension(:):: cdiagbuf

  logical,dimension(nobs):: luse,muse
  logical,dimension(nobs):: qcfail

!*******************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse


  ier=ione            ! index of obs error in data array (now 1/(original obs error))
  ilon=2_i_kind       ! index of grid relative obs location (x)
  ilat=3_i_kind       ! index of grid relative obs location (y)
  ihgt=4_i_kind       ! index of obs vertical coordinate in data array
  igps=5_i_kind       ! index of gps data (or residual) in data array
  itime=6_i_kind      ! index of obs time in data array
  ikxx=7_i_kind       ! index of observation type
  iprof=8_i_kind      ! index of profile
  ipctc=9_i_kind      ! index of percent confidence
  iroc=10_i_kind      ! index of local radius of curvature (m)
  isatid=11_i_kind    ! index of satellite identifier
  iptid=12_i_kind     ! index of platform transmitter id number
  iuse=13_i_kind      ! index of use parameter
  ilone=14_i_kind     ! index of earth relative longitude (degrees)
  ilate=15_i_kind     ! index of earth relative latitude (degrees)
  igeoid=16_i_kind    ! index of geoid undulation (a value per profile, m) 

! Intialize variables
  nsig_up=nsig+10_i_kind ! extend 10 levels above interface level nsig
  rsig=float(nsig)
  rdog=rd/grav
  rsig_up=float(nsig_up)
  mm1=mype+ione
  qcfail=.false.
  qcfail_loc=zero;qcfail_gross=zero;qcfail_stats_1=zero;qcfail_stats_2=zero
  qcfail_high=zero
  toss_gps_sub=zero 
  dpressure=zero
  dbend_loc=zero
  dbend=zero

! Allocate arrays for output to diagnostic file
  mreal=19_i_kind
  nreal=mreal
  if (lobsdiagsave) nreal=nreal+4*miter+ione
  allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))

! define new equally spaced grid s
  do j=0,grids_dim-ione
     grid_s(j+ione)=j*ds
  enddo

  do i=1,nobs ! loop over obs 

     muse(i)=nint(data(iuse,i)) <= jiter
     sin2  = sin(data(ilate,i)*deg2rad)**2
     dlat=data(ilat,i)
     dlon=data(ilon,i)
     rocprof=data(iroc,i)
     unprof=data(igeoid,i)
     tpdpres=data(ihgt,i)
     elev=tpdpres
     ikx=nint(data(ikxx,i))
     dtime=data(itime,i)

     gps2work(1,i)=r1em3*(tpdpres-rocprof) ! impact height in km
     gps2work(2,i)=data(ilate,i)           ! lat in degree

!    Interpolate log(pres),temperature,specific humidity to obs location
     call tintrp2a(ges_lnprsi,prsltmp,dlat,dlon,dtime,hrdifsig,&
        ione,nsig+ione,mype,nfldsig)
     call tintrp2a(ges_tv,tges,dlat,dlon,dtime,hrdifsig,&
        ione,nsig,mype,nfldsig)
     call tintrp2a(ges_q,qges,dlat,dlon,dtime,hrdifsig,&
        ione,nsig,mype,nfldsig)
     call tintrp2a(geop_hgti,hges,dlat,dlon,dtime,hrdifsig,&
          ione,nsig+ione,mype,nfldsig)
     call tintrp2a(geop_hgtl,hgesl,dlat,dlon,dtime,hrdifsig,&
          ione,nsig,mype,nfldsig)
     call tintrp2a(ges_z,zsges,dlat,dlon,dtime,hrdifsig,&
          ione,ione,mype,nfldsig)

!??????? REVIEW     prsltmp_o(1:nsig,i)=prsltmp(1:nsig) ! needed in minimization

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

     do k=1,nsig !! stopping at nsig
        zges(k) = (termr*hges(k)) / (termrg-hges(k))  ! eq (23) at interface and wrt model topography
!!needed ??        gp2gm(k,i)= termr/(termrg-hges(k))+((termr*hges(k))/(termrg-hges(k))**2)
        rges(k,i) = zges(k) + zsges + unprof + rocprof                  ! radius r_i
     end do

     do k=1,nsig ! interface values
        if(k>ione) then
           qmean=(qges(k)+qges(k-ione))/two
           tmean=(tges(k)+tges(k-ione))/two
        else
           qmean=qges(1)
           tmean=tges(1)
        endif
        tges_o(k,i)=tmean  ! needed in minimization
        fact=(one+fv*qmean)
        pw=eps+qmean*(one-eps)
        pressure=ten*exp(prsltmp(k)) ! pressure of interface level in mb
        nrefges1=n_a*(pressure/tmean)*fact
        nrefges2=n_b*qmean*pressure*fact**2/(tmean**2*pw)
        nrefges(k,i)=nrefges1+nrefges2         ! refractivity N_i
        irefges(k)= one+(r1em6*nrefges(k,i))   ! index of refractivity n_i
        ref_rad(k)=irefges(k)*rges(k,i)        ! refractivity index-radius product x_i
 
!       Terms for the minimization
        n_q(k,i)= &
           n_a*(pressure/tmean)*fv+&
           (n_b/(tmean**2*pw))*pressure*fact**2+&
           (n_b/(tmean**2*pw))*pressure*qmean*two*fv*fact-&
           (n_b/(tmean**2*pw**2))*pressure*qmean*fact**2*(one-eps)
        n_p(k,i)= &
           (n_a/tmean)*fact*ten+&
           (n_b/(tmean**2*pw))*ten*qmean*fact**2
        n_t(k,i)= &
           -n_a*fact*(pressure/tmean**2)-n_b*qmean*fact**2*two*&
           (pressure/(tmean**3*pw))
     end do

!    Tune observation error to account for representativeness
!    error. Preliminary values

     repe_gps=one
     if(gps2work(1,i) > r30) then
!       repe_gps=0.2_r_kind
     else
        if(gps2work(1,i) < eight)then
           repe_gps=0.9_r_kind
        else
           repe_gps=half
        endif
     end if

!    Penalize high level obs
     if (gps2work(1,i) >= 25_r_kind) then
        repe_gps = (0.6_r_kind*gps2work(1,i)-14_r_kind)*repe_gps
     endif

     ratio_errors(i) = data(ier,i)/abs(data(ier,i)*repe_gps)
     error(i)=one/data(ier,i) ! one/original error
     data(ier,i)=one/data(ier,i)
     error_adjst(i)= ratio_errors(i)* data(ier,i) !one/adjusted error

!    Remove observation if below surface or at/above the top layer
!    of the model by setting observation (1/error) to zero.
!    Make no adjustment if observation falls within vertical
!    domain.

     hob=tpdpres
     call grdcrd(hob,ione,ref_rad(1),nsig,ione)
     if (hob>one .and. hob<rsig) then 
        call tintrp3(ges_lnprsi,dpress,dlat,dlon,hob,&
               dtime,hrdifsig,1,mype,nfldsig)
        dpressure(i)=ten*exp(dpress) !obs pressure in mb
     else
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

     rdiagbuf(1,i)         = ictype(ikx)     ! observation type
!    rdiagbuf(2,i)         = icsubtype(ikx)  ! observation subtype (not defined yet)
     rdiagbuf(2,i)         = one             ! uses gps_ref (one = use of bending angle)
     rdiagbuf(3,i)         = data(ilate,i)   ! lat in degrees
     rdiagbuf(4,i)         = data(ilone,i)   ! lon in degrees
     rdiagbuf(6,i)         = dpressure(i)    ! guess observation pressure (hPa)
     rdiagbuf(7,i)         = tpdpres-rocprof ! impact height in meters
     rdiagbuf(8,i)         = dtime-time_offset        ! obs time (hours relative to analysis time)
     rdiagbuf(9,i)         = data(ipctc,i)   ! input bufr qc - index of per cent confidence
     rdiagbuf(11,i)        = data(iuse,i)    ! data usage flag
 
     if (hob>one .and. hob<rsig) then ! obs inside model grid

!       Extending atmosphere above interface level nsig
        d_ref_rad=ref_rad(nsig)-ref_rad(nsig-ione)
        do k=1,10
           ref_rad(nsig+k)=ref_rad(nsig)+ k*d_ref_rad ! extended x_i
           nrefges(nsig+k,i)=nrefges(nsig+k-ione,i)**2/nrefges(nsig+k-2_i_kind,i) ! exended N_i
        end do

!       Lagrange coefficients
        ref_rad(0)=ref_rad(3)
        ref_rad(nsig_up+ione)=ref_rad(nsig_up-2_i_kind)
        do k=1,nsig_up
           call setq(q_w(:,k),ref_rad(k-ione:k+ione),3)
        enddo

        dpres(i)=data(ihgt,i)
        muse(i)=nint(data(iuse,i)) <= jiter

!       Get refractivity index-radius and [d(ln(n))/dx] in new grid.
        intloop: do j=1,grids_dim
           ref_rad_s(j)=sqrt(grid_s(j)*grid_s(j)+dpres(i)*dpres(i)) !x_j
           xj(j,i)=ref_rad_s(j)
           hob_s=ref_rad_s(j)
           call grdcrd(hob_s,ione,ref_rad(1),nsig_up,ione)
           dbend_loc(j,i)=hob_s  !location of x_j with respect to extended x_i
 
           if (hob_s < rsig_up) then  !obs insided the new grid
              ihob=hob_s

!             Compute refractivity and derivative at target points 
!             using Lagrange interpolators
              call slagdw(ref_rad(ihob-ione:ihob+2_i_kind),ref_rad_s(j),&
                   q_w(:,ihob),q_w(:,ihob+ione),&
                   w4,dw4,4_i_kind)
              if(ihob==ione) then
                 w4(4)=w4(4)+w4(1); w4(1:3)=w4(2:4);w4(4)=zero
                 dw4(4)=dw4(4)+dw4(1);dw4(1:3)=dw4(2:4);dw4(4)=zero
                 ihob=ihob+ione
              endif
              if(ihob==nsig_up-ione) then
                 w4(1)=w4(1)+w4(4); w4(2:4)=w4(1:3);w4(1)=zero
                 dw4(1)=dw4(1)+dw4(4); dw4(2:4)=dw4(1:3);dw4(1)=zero
                 ihob=ihob-ione
              endif
              ddnj(j)=dot_product(dw4,nrefges(ihob-ione:ihob+2_i_kind,i))!derivative (dN/dx)_j
              ddnj(j)=max(zero,abs(ddnj(j)))
           else
              write(6,*) 'GPS obs outside integration grid','obs=',i
              call stop2(92)
           endif !obs in new grid
        end do intloop

        dbend(i)=ds*(r1em6*ddnj(1)/ref_rad_s(1))
        do j=2,grids_dim
           ddbend=ds*(r1em6*ddnj(j)/ref_rad_s(j))
           dbend(i)=dbend(i)+two*ddbend
        end do
        dbend(i)=dpres(i)*dbend(i)  


!       Accumulate diagnostic information
        gps2work(3,i)=(data(igps,i)-dbend(i))/((data(igps,i)+dbend(i))/two)
        rdiagbuf(5,i)    = gps2work(3,i) ! incremental bending angle (x100 %)
        rdiagbuf (17,i)  = data(igps,i)  ! bending angle observation (degrees)
        rdiagbuf (18,i)  = data(igps,i)-dbend(i) ! obs-ges used in analysis (degrees)
        rdiagbuf (19,i)  = data(igps,i)-dbend(i) ! obs-ges w/o bias correction (future slot)
 

        data(igps,i)=data(igps,i)-dbend(i) !innovation vector
        data(ihgt,i)=hob

        if(gps2work(1,i) <= r40) then ! go into qc checks

!          Gross error checks
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
!             QC checks based on latitude and height
              if(gps2work(2,i) < -r30) then                !SH
                 if(gps2work(1,i) >= r30)then
                    cutoff(i)=three              
                 else
                    if(gps2work(1,i) <= ten)then
                       cutoff(i)=-r0_8*gps2work(1,i)+ten
                    else 
                       cutoff(i)=two
                    end if
                 end if
              else if(gps2work(2,i) > r30) then            !NH
                 if(gps2work(1,i) >= r30)then
                    cutoff(i)=three 
                 else
                    if(gps2work(1,i) <= ten)then
                       cutoff(i)=-r0_8*gps2work(1,i)+ten
                    else
                       cutoff(i)=two
                    end if
                 end if
              else                                         !TR
                 if(gps2work(1,i) >= r30)then
                    cutoff(i)=three
                 else
                    if(gps2work(1,i) <= ten)then
                       cutoff(i)=-r1_3*gps2work(1,i)+r15
                    else 
                       cutoff(i)=two
                    end if
                 end if
              end if
 
              if(gps2work(1,i) >= r30)then
                 cutoff(i)=two*cutoff(i)*r0_01
              else
                 if(gps2work(1,i) <= ten)then
!                   cutoff(i)=three*cutoff(i)*r0_01
                    cutoff(i)=one*cutoff(i)*r0_01
                 else
                    cutoff(i)=four*cutoff(i)*r0_01
                 end if
              end if
 
              if(abs(gps2work(3,i))> cutoff(i)) then
                 qcfail(i)=.true.
                 qcfail_stats_1(i)=one
              endif
           end if ! gross qc check
        end if ! qc checks (only below 40km)


!       Remove obs above 30 km in order to avoid increments at top model
        if(gps2work(1,i) > r30) then
           data(ier,i) = zero
           ratio_errors(i) = zero
           qcfail_high(i)=one
           muse(i)=.false.
        endif

!       Remove data below 6 km.
!       if (gps2work(1,i) <= six)then
!          data(ier,i) = zero
!          ratio_errors(i) = zero
!          muse(i)=.false.
!       endif

     end if ! obs inside the vertical grid
  end do ! end of loop over observations

! Loop over observation profiles. Compute penalty
! terms, and accumulate statistics.
  do i=1,nobs
     kprof = data(iprof,i)

     if(qcfail(i) .and. gps2work(1,i) <= ten)then
        do j=1,nobs
           jprof = data(iprof,j)
           if(kprof == jprof .and. .not. qcfail(j))then
!             Remove data below
              if(gps2work(1,j) < gps2work(1,i))then
                 qcfail(j) = .true. 
                 qcfail_stats_2(j)=one
              end if
           end if
        end do
     end if
  end do

  do i=1,nobs
     kprof = data(iprof,i)

     if(qcfail(i))then
        data(ier,i)=zero
        ratio_errors(i) = zero
        muse(i) = .false.
        if (gps2work(1,i) <= ten )then
           toss_gps_sub(kprof) = max(toss_gps_sub(kprof),data(ihgt,i))
        end if

!       Counting obs tossed due to the stats qc check
!       This calculation will be updated in genstats_gps due to toss_gps_sub

        if (luse(i))then
           if(gps2work(2,i)> r30) then
              awork(22) = awork(22)+one                !NH
           else if(gps2work(2,i)< -r30)then
              awork(23) = awork(23)+one                !SH
           else
              awork(24) = awork(24)+one                !TR
           end if
        end if
     end if

!    Testing superrefraction (pending)

  end do


! Loop to load arrays used in statistics output
  idia=izero
  do i=1,nobs
     if (ratio_errors(i)*data(ier,i) <= tiny_r_kind) muse(i) = .false.

     ikx=nint(data(ikxx,i))
     dtime=data(itime,i)


     ! flags for observations that failed qc checks
     ! zero = observation is good

     if(qcfail_loc(i) == one) rdiagbuf(10,i) = one
     if(qcfail_high(i) == one) rdiagbuf(10,i) = two

     if(qcfail_gross(i) == one) then
        if(qcfail_high(i) == one) then
           rdiagbuf(10,i) = four
        else
           rdiagbuf(10,i) = three
        endif
     else if(qcfail_stats_1(i) == one) then
        if(qcfail_high(i) == one) then
           rdiagbuf(10,i) = six
        else
           rdiagbuf(10,i) = five
        endif
     else if(qcfail_stats_2(i) == one) then
        if(qcfail_high(i) == one) then
           rdiagbuf(10,i) = eight
        else
           rdiagbuf(10,i) = seven
        endif
     end if

     if(muse(i)) then                    ! modified in genstats_gps due to toss_gps_sub
        rdiagbuf(12,i) = one             ! minimization usage flag (1=use, -1=not used)
     else
        rdiagbuf(12,i) = -one
     endif

     if (ratio_errors(i)*data(ier,i)>tiny_r_kind) then
        err_final = ratio_errors(i)*data(ier,i)
     else
        err_final = huge_single
     endif

     errinv_input  = huge_single
     errinv_adjst  = huge_single
     errinv_final  = huge_single

     if (error(i)>tiny_r_kind) errinv_input=error(i)
     if (error_adjst(i)>tiny_r_kind) errinv_adjst=error_adjst(i)
     if (err_final>tiny_r_kind) errinv_final=err_final

     rdiagbuf(13,i) = zero ! nonlinear qc relative weight - will be defined in genstats_gps
     rdiagbuf(14,i) = errinv_input ! original inverse gps obs error (degree**-1)
     rdiagbuf(15,i) = errinv_adjst ! original + represent error inverse gps 
                                   ! obs error (degree**-1)
     rdiagbuf(16,i) = errinv_final ! final inverse observation error due to 
                                   ! superob factor (degree**-1)
                                   ! modified in genstats_gps

!    Link observation to appropriate observation bin
     if (nobs_bins>ione) then
        ibin = NINT( dtime/hr_obsbin ) + ione
     else
        ibin = ione
     endif
     IF (ibin<ione.OR.ibin>nobs_bins) write(6,*)mype,'Error ibin'

     if (nobskeep>izero) muse(i)=obsdiags(i_gps_ob_type,ibin)%tail%muse(nobskeep)

!    Link obs to diagnostics structure
     if (.not.lobsdiag_allocated) then
        if (.not.associated(obsdiags(i_gps_ob_type,ibin)%head)) then
           allocate(obsdiags(i_gps_ob_type,ibin)%head,stat=istat)
           if (istat/=izero) then
              write(6,*)'setupbend: failure to allocate obsdiags',istat
              call stop2(250)
           end if
           obsdiags(i_gps_ob_type,ibin)%tail => obsdiags(i_gps_ob_type,ibin)%head
        else
           allocate(obsdiags(i_gps_ob_type,ibin)%tail%next,stat=istat)
           if (istat/=izero) then
              write(6,*)'setupbend: failure to allocate obsdiags',istat
              call stop2(251)
           end if
           obsdiags(i_gps_ob_type,ibin)%tail => obsdiags(i_gps_ob_type,ibin)%tail%next
        end if
        allocate(obsdiags(i_gps_ob_type,ibin)%tail%muse(miter+ione))
        allocate(obsdiags(i_gps_ob_type,ibin)%tail%nldepart(miter+ione))
        allocate(obsdiags(i_gps_ob_type,ibin)%tail%tldepart(miter))
        allocate(obsdiags(i_gps_ob_type,ibin)%tail%obssen(miter))
        obsdiags(i_gps_ob_type,ibin)%tail%indxglb=i
        obsdiags(i_gps_ob_type,ibin)%tail%nchnperobs=-99999_i_kind
        obsdiags(i_gps_ob_type,ibin)%tail%luse=.false.
        obsdiags(i_gps_ob_type,ibin)%tail%muse(:)=.false.
        obsdiags(i_gps_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
        obsdiags(i_gps_ob_type,ibin)%tail%tldepart(:)=zero
        obsdiags(i_gps_ob_type,ibin)%tail%wgtjo=-huge(zero)
        obsdiags(i_gps_ob_type,ibin)%tail%obssen(:)=zero
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
!    Save values needed for generate of statistics for all observations
 
     if(.not. associated(gps_allhead(ibin)%head))then
        allocate(gps_allhead(ibin)%head,stat=istat)
        if(istat /= izero)write(6,*)' failure to write gps_allhead '
        gps_alltail(ibin)%head => gps_allhead(ibin)%head
     else
        allocate(gps_alltail(ibin)%head%llpoint,stat=istat)
        if(istat /= izero)write(6,*)' failure to write gps_alltail%llpoint '
        gps_alltail(ibin)%head => gps_alltail(ibin)%head%llpoint
     end if
     allocate(gps_alltail(ibin)%head%rdiag(nreal),stat=istatus)
     if (istatus/=izero) write(6,*)'SETUPBEND:  allocate error for gps_alldiag, istatus=',istatus

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
     do j=1,nreal
        gps_alltail(ibin)%head%rdiag(j)= rdiagbuf(j,i)
     end do
 
     obsdiags(i_gps_ob_type,ibin)%tail%luse=luse(i)
     obsdiags(i_gps_ob_type,ibin)%tail%muse(jiter)=muse(i)
     obsdiags(i_gps_ob_type,ibin)%tail%nldepart(jiter)=data(igps,i)
     obsdiags(i_gps_ob_type,ibin)%tail%wgtjo=(data(ier,i)*ratio_errors(i))**2

! If obs is "acceptable", load array with obs info for use
! in inner loop minimization (int* and stp* routines)
     if (.not. last .and. muse(i) ) then
        if(.not. associated(gpshead(ibin)%head))then
           allocate(gpshead(ibin)%head,stat=istat)
           if(istat /= izero)write(6,*)' failure to write gpshead '
           gpstail(ibin)%head => gpshead(ibin)%head
        else
           allocate(gpstail(ibin)%head%llpoint,stat=istat)
           if(istat /= izero)write(6,*)' failure to write gpstail%llpoint '
           gpstail(ibin)%head => gpstail(ibin)%head%llpoint
        end if
        allocate(gpstail(ibin)%head%jac_t(nsig),gpstail(ibin)%head%jac_q(nsig), &
                 gpstail(ibin)%head%jac_p(nsig+ione),gpstail(ibin)%head%ij(4,nsig),stat=istatus)
        if (istatus/=izero) write(6,*)'SETUPBEND:  allocate error for gps_point, istatus=',istatus

        gps_alltail(ibin)%head%mmpoint  => gpstail(ibin)%head
 
! Set (i,j) indices of guess gridpoint that bound obs location
        call get_ij(mm1,data(ilat,i),data(ilon,i),gps_ij,gpstail(ibin)%head%wij(1))

        do j=1,nsig
           gpstail(ibin)%head%ij(1,j)=gps_ij(1)+(j-ione)*latlon11
           gpstail(ibin)%head%ij(2,j)=gps_ij(2)+(j-ione)*latlon11
           gpstail(ibin)%head%ij(3,j)=gps_ij(3)+(j-ione)*latlon11
           gpstail(ibin)%head%ij(4,j)=gps_ij(4)+(j-ione)*latlon11
        enddo
        dhdp=zero
        dhdt=zero
        dhdp=zero
        do k=2,nsig
           do j=2,k
              dhdt(k,j-ione)= rdog*(prsltmp_o(j-ione,i)-prsltmp_o(j,i))
              dhdp(k,j)= dhdp(k,j)-rdog*(tges_o(j-ione,i)/exp(prsltmp_o(j,i)))
              dhdp(k,j-ione)=dhdp(k,j-ione)+rdog*(tges_o(j-ione,i)/exp(prsltmp_o(j-ione,i)))
           end do
        end do
        dndt=zero
        dndq=zero
        dndp=zero
        do k=1,nsig
           if(k == ione)then
              dndt(k,k)=dndt(k,k)+n_t(k,i)
              dndq(k,k)=dndq(k,k)+n_q(k,i)
              dndp(k,k)=dndp(k,k)+n_p(k,i)
           else
              dndt(k,k)=dndt(k,k)+half*n_t(k,i)
              dndt(k,k-ione)=dndt(k,k-ione)+half*n_t(k,i)
              dndq(k,k)=dndq(k,k)+half*n_q(k,i)
              dndq(k,k-ione)=dndq(k,k-ione)+half*n_q(k,i)
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
        d_ref_rad=ref_rad(nsig)-ref_rad(nsig-ione)
        do k=1,10
           ref_rad(nsig+k)=ref_rad(nsig) + k*d_ref_rad
        end do
        ref_rad(0)=ref_rad(3)
        ref_rad(nsig_up+ione)=ref_rad(nsig_up-2_i_kind)
        do kk=1,nsig
           xi_TL=zero
           xi_TL(kk)=one
           n_TL=zero
           n_TL(kk)=one
           q_w=zero
           q_w_TL=zero
           d_ref_rad_TL=xi_TL(nsig)-xi_TL(nsig-ione)
           do k=1,10
              xi_TL(nsig+k)=xi_TL(nsig)+ k*d_ref_rad_TL
              n_TL(nsig+k)=(two*nrefges(nsig+k-ione,i)*n_TL(nsig+k-ione)/nrefges(nsig+k-2_i_kind,i))-&
                  (nrefges(nsig+k-ione,i)**2/nrefges(nsig+k-2_i_kind,i)**2)*n_TL(nsig+k-2_i_kind)

           end do
           xi_TL(0)=xi_TL(3)
           xi_TL(nsig_up+ione)=xi_TL(nsig_up-2_i_kind)
           do k=1,nsig_up
              call setq_TL(q_w(:,k),q_w_TL(:,k),ref_rad(k-ione:k+ione),xi_TL(k-ione:k+ione),3_i_kind)
           enddo
           intloop2: do j=1,grids_dim
              ihob=dbend_loc(j,i)

! Compute refractivity and derivative at target points using Lagrange interpolators
              call slagdw_TL(ref_rad(ihob-ione:ihob+2_i_kind),xi_TL(ihob-ione:ihob+2_i_kind),xj(j,i),&
                         q_w(:,ihob),q_w_TL(:,ihob),&
                         q_w(:,ihob+ione),q_w_TL(:,ihob+ione),&
                         dw4,dw4_TL,4_i_kind)
              if(ihob==ione) then
                 dw4(4)=dw4(4)+dw4(1);dw4(1:3)=dw4(2:4);dw4(4)=zero
                 dw4_TL(4)=dw4_TL(4)+dw4_TL(1);dw4_TL(1:3)=dw4_TL(2:4);dw4_TL(4)=zero
                 ihob=ihob+ione
              endif
              if(ihob==nsig_up-ione) then
                 dw4(1)=dw4(1)+dw4(4); dw4(2:4)=dw4(1:3);dw4(1)=zero
                 dw4_TL(1)=dw4_TL(1)+dw4_TL(4); dw4_TL(2:4)=dw4_TL(1:3);dw4_TL(1)=zero
                 ihob=ihob-ione
              endif
              dbetaxi=(r1em6/xj(j,i))*dot_product(dw4_TL,nrefges(ihob-ione:ihob+2_i_kind,i))
              dbetan =(r1em6/xj(j,i))*dot_product(dw4,n_TL(ihob-ione:ihob+2_i_kind))
              if(j == ione)then
                 dbenddxi(kk)=dbetaxi
                 dbenddn(kk)=dbetan
              else
                 dbenddxi(kk)=dbenddxi(kk)+two*dbetaxi
                 dbenddn(kk)=dbenddn(kk)+two*dbetan
              end if
           end do intloop2
        end do
        do k=1,nsig
           dbenddxi(k)=-dbenddxi(k)*ds*dpres(i)
           dbenddn(k)=-dbenddn(k)*ds*dpres(i)
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

     end if
 
     if (lobsdiagsave.and.luse(i)) then
        rdiagbuf(8,i)= data(ier,i)               ! 1/obserror
        idia=idia+ione
        do j=1,mreal
           rdiagbuf(j,idia)=rdiagbuf(j,i)
        end do
        if (lobsdiagsave) then
           ioff=mreal
           do jj=1,miter
              ioff=ioff+ione
              if (obsdiags(i_gps_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(ioff,idia) = one
              else
                 rdiagbuf(ioff,idia) = -one
              endif
           enddo
           do jj=1,miter+ione
              ioff=ioff+ione
              rdiagbuf(ioff,idia) = obsdiags(i_gps_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+ione
              rdiagbuf(ioff,idia) = obsdiags(i_gps_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+ione
              rdiagbuf(ioff,idia) = obsdiags(i_gps_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif
     endif

  end do
  deallocate(cdiagbuf,rdiagbuf)
 
end subroutine setupbend
