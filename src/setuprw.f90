subroutine setuprw(lunin,mype,bwork,awork,nele,nobs,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setuprw     compute rhs of oi for radar radial winds
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: For radar radial wind observations, this routine
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   1990-10-06  parrish
!   1998-04-10  weiyu yang
!   1999-03-01  wu - ozone processing moved into setuprhs from setupoz
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-17  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - increase size of rwork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-05-27  derber - level output change
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-11-03  treadon - correct error in index values for data array
!   2005-11-29  derber - remove psfcg and use ges_lnps instead
!   2006-01-31  todling/treadon - store wgt/wgtlim in rdiagbuf(6,ii)
!   2006-02-02  treadon - rename lnprsl as ges_lnprsl
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-04-21  parrish - new forward model based on beam vertical uncertainty
!   2006-05-23  parrish - use model terrain at station location for zsges
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-07-31  kleist - use ges_ps instead of lnps
!   2006-08-28      su - fix a bug in variational qc
!   2008-05-23  safford - rm unused vars and uses
!   2008-12-03  todling - changed handle of tail%time
!
!   input argument list:
!     lunin    - unit from which to read observations
!     mype     - mpi task id
!     nele     - number of data elements per observation
!     nobs     - number of observations
!
!   output argument list:
!     bwork    - array containing information about obs-ges statistics
!     awork    - array containing information for data counts and gross checks
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_single,r_double,i_kind,i_long

  use obsmod, only: rwhead,rwtail,rmiss_single,i_rw_ob_type,obsdiags,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use qcmod, only: npres_print,ptop,pbot
  use guess_grids, only: ges_ps,hrdifsig,geop_hgtl,ges_z,nfldsig,&
       ntguessig,ges_lnprsl,ges_u,ges_v,sfcmod_gfs,sfcmod_mm5,comp_fact10
  use gridmod, only: nsig,nlat,nlon,get_ijk
  use constants, only: flattening,semi_major_axis,grav_ratio,zero,grav,wgtlim,&
       half,one,two,grav_equator,eccentricity,somigliana,rad2deg,deg2rad
  use constants, only: tiny_r_kind,half,cg_term,huge_single
  use qcmod, only: npres_print
  use jfunc, only: jiter,last,first,miter
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  implicit none

! Declare passed variables
  logical,intent(in):: conv_diagsave
  integer(i_kind),intent(in):: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig),intent(inout):: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout):: bwork


! Declare local parameters
  real(r_kind),parameter:: r0_001 = 0.001_r_kind
  real(r_kind),parameter:: r8     = 8.0_r_kind
  real(r_kind),parameter:: ten    = 10.0_r_kind
  real(r_kind),parameter:: r200   = 200.0_r_kind
  real(r_kind),parameter:: r2000  = 2000.0_r_kind


! Declare local variables
  real(r_kind) rlow,rhgh,rsig
  real(r_kind) dz,factelv,factdif
  real(r_kind) dlnp,pobl,zob
  real(r_kind) sin2,termg,termr,termrg
  real(r_kind) psges,zsges
  real(r_kind),dimension(nsig):: zges,hges,ugesprofile,vgesprofile
  real(r_kind) prsltmp(nsig)
  real(r_kind) sfcchk  
  real(r_kind) residual,obserrlm,obserror,ratio,scale,val2
  real(r_kind) ress,ressw
  real(r_kind) val,valqc,rwgt
  real(r_kind) cg_w,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2
  real(r_double) rstation_id
  real(r_kind) dlat,dlon,dtime,dpres,ddiff,error,slat
  real(r_kind) sinazm,cosazm,costilt
  real(r_kind) ratio_errors
  real(r_kind) ugesin,vgesin,factw,skint,sfcr
  real(r_kind) rwwind,presw
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf

  integer(i_kind) i,nchar,nreal,k,j,k1,ii
  integer(i_kind) mm1,jj,k2,isli
  integer(i_kind) jsig,ikxx,nn,ibin,ioff
  integer(i_kind) ier,ilat,ilon,ihgt,irwob,ikx,itime,iuse
  integer(i_kind):: ielev,id,itilt,iazm,ilone,ilate,irange
  integer(i_kind):: izsges,ier2,idomsfc,isfcr,iskint,iff10
  
  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf

  logical,dimension(nobs):: luse,muse

  equivalence(rstation_id,station_id)
  real(r_kind) addelev,wrange,beamdepth,elevtop,elevbot
  integer(i_kind) kbeambot,kbeamtop,kbeamdiffmax,kbeamdiffmin
  real(r_kind) uminmin,umaxmax
  integer(i_kind) numequal,numnotequal,kminmin,kmaxmax,istat
  real(r_kind) rwwindprofile

!*******************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse


!    index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ihgt=4      ! index of obs elevation
  irwob=5     ! index of radial wind observation
  iazm=6      ! index of azimuth angle in data array
  itime=7     ! index of observation time in data array
  ikxx=8      ! index of obs type in data array
  itilt=9     ! index of tilt angle in data array
  ielev=10    ! index of radar elevation
  id=11       ! index of station id
  iuse=12     ! index of use parameter
  idomsfc=13  ! index of dominant surface type
  iskint=14   ! index of surface skin temperature
  iff10=15    ! index of 10 meter wind factor
  isfcr=16    ! index of surface roughness
  ilone=17    ! index of longitude (degrees)
  ilate=18    ! index of latitude (degrees)
  irange=19   ! index of range in km of obs from radar
  izsges=20   ! index of model (guess) elevation for radar associated with vad wind
  ier2=21     ! index of original-original obs error

  numequal=0
  numnotequal=0

! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=0
     nchar=1
     nreal=22
     if (lobsdiagsave) nreal=nreal+4*miter+1
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
  end if

  mm1=mype+1
  scale=one
  rsig=nsig

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do
  
  kbeamdiffmin=huge(kbeamdiffmin)
  kbeamdiffmax=-huge(kbeamdiffmax)
  do i=1,nobs
     
     dlat=data(ilat,i)
     dlon=data(ilon,i)
     dtime=data(itime,i)
     dpres=data(ihgt,i)
     ikx = nint(data(ikxx,i))
     error=data(ier2,i)
     slat=data(ilate,i)*deg2rad
     wrange=data(irange,i)
     zsges=data(izsges,i)

!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
       ibin = NINT( dtime/hr_obsbin ) + 1
     else
       ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

!    Link obs to diagnostics structure
     if (.not.lobsdiag_allocated) then
       if (.not.associated(obsdiags(i_rw_ob_type,ibin)%head)) then
         allocate(obsdiags(i_rw_ob_type,ibin)%head,stat=istat)
         if (istat/=0) then
           write(6,*)'setuprw: failure to allocate obsdiags',istat
           call stop2(286)
         end if
         obsdiags(i_rw_ob_type,ibin)%tail => obsdiags(i_rw_ob_type,ibin)%head
       else
         allocate(obsdiags(i_rw_ob_type,ibin)%tail%next,stat=istat)
         if (istat/=0) then
           write(6,*)'setuprw: failure to allocate obsdiags',istat
           call stop2(286)
         end if
         obsdiags(i_rw_ob_type,ibin)%tail => obsdiags(i_rw_ob_type,ibin)%tail%next
       end if
       allocate(obsdiags(i_rw_ob_type,ibin)%tail%muse(miter+1))
       allocate(obsdiags(i_rw_ob_type,ibin)%tail%nldepart(miter+1))
       allocate(obsdiags(i_rw_ob_type,ibin)%tail%tldepart(miter))
       allocate(obsdiags(i_rw_ob_type,ibin)%tail%obssen(miter))
       obsdiags(i_rw_ob_type,ibin)%tail%indxglb=i
       obsdiags(i_rw_ob_type,ibin)%tail%nchnperobs=-99999
       obsdiags(i_rw_ob_type,ibin)%tail%luse=.false.
       obsdiags(i_rw_ob_type,ibin)%tail%muse(:)=.false.
       obsdiags(i_rw_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
       obsdiags(i_rw_ob_type,ibin)%tail%tldepart(:)=zero
       obsdiags(i_rw_ob_type,ibin)%tail%wgtjo=-huge(zero)
       obsdiags(i_rw_ob_type,ibin)%tail%obssen(:)=zero
     else
       if (.not.associated(obsdiags(i_rw_ob_type,ibin)%tail)) then
         obsdiags(i_rw_ob_type,ibin)%tail => obsdiags(i_rw_ob_type,ibin)%head
       else
         obsdiags(i_rw_ob_type,ibin)%tail => obsdiags(i_rw_ob_type,ibin)%tail%next
       end if
       if (obsdiags(i_rw_ob_type,ibin)%tail%indxglb/=i) then
         write(6,*)'setuprw: index error'
         call stop2(288)
       end if
     endif


!    Interpolate log(surface pressure),  
!    log(pres) at mid-layers, and geopotenital height to 
!    observation location.

     factw=data(iff10,i)
     if(sfcmod_gfs .or. sfcmod_mm5) then
        sfcr=data(isfcr,i)
        skint=data(iskint,i)
        isli=data(idomsfc,i)
        call comp_fact10(dlat,dlon,dtime,skint,sfcr,isli,mype,factw)
     end if

     call tintrp2a(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          1,1,mype,nfldsig)
     call tintrp2a(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          1,nsig,mype,nfldsig)
     call tintrp2a(geop_hgtl,hges,dlat,dlon,dtime,hrdifsig,&
          1,nsig,mype,nfldsig)

!    Convert geopotential height at layer midpoints to geometric height using
!    equations (17, 20, 23) in MJ Mahoney's note "A discussion of various
!    measures of altitude" (2001).  Available on the web at
!    http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
!    termg  = equation 17
!    termr  = equation 21
!    termrg = first term in the denominator of equation 23
!    zges   = equation 23
     sin2  = sin(slat)*sin(slat)
     termg = grav_equator * &
          ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
     termr = semi_major_axis /(one + flattening + grav_ratio -  &
          two*flattening*sin2)
     termrg = (termg/grav)*termr
     do k=1,nsig
        zges(k) = (termr*hges(k)) / (termrg-hges(k))  ! eq (23)
     end do

!    Given observation height, (1) adjust 10 meter wind factor if
!    necessary, (2) convert height to grid relative units, (3) compute
!    compute observation pressure (for diagnostic purposes only), and
!    (4) compute location of midpoint of first model layer above surface
!    in grid relative units

!    Adjust 10m wind factor if necessary.  Rarely do we have a
!    lidar obs within 10 meters of the surface.  Almost always,
!    the code below resets the 10m wind factor to 1.0 i.e., no
!    reduction in wind speed due to surface friction).
     if (dpres<ten) then
        dz = ten-dpres
        factw = factw + (factw-zero)/dz
     else
        factw=one
     endif

!    Convert observation height (in dpres) from meters to grid relative
!    units.  Save the observation height in zob for later use.
     zob = dpres
     call grdcrd(dpres,1,zges,nsig,1)

!    Set indices of model levels below (k1) and above (k2) observation.
     k=dpres
     k1=max(1,k)
     k2=min(k+1,nsig)

!    Compute observation pressure (only used for diagnostics)
     dz     = zges(k2)-zges(k1)
     dlnp   = prsltmp(k2)-prsltmp(k1)
     pobl   = prsltmp(k1) + (dlnp/dz)*(zob-zges(k1))
     presw  = ten*exp(pobl)

!    Determine location in terms of grid units for midpoint of
!    first layer above surface
     sfcchk=log(psges)
     call grdcrd(sfcchk,1,prsltmp,nsig,-1)

!    Check to see if observation is below midpoint of first
!    above surface layer.  If so, set rlow to that difference
     rlow=max(sfcchk-dpres,zero)

!    Check to see if observation is above midpoint of layer
!    at the top of the model.  If so, set rhgh to that difference.
     rhgh=max(dpres-r0_001-nsig,zero)

!    Increment obs counter along with low and high obs counters
     if(luse(i))then
        awork(1)=awork(1)+one
        if(rhgh/=zero) awork(2)=awork(2)+one
        if(rlow/=zero) awork(3)=awork(3)+one
     end if
     
!    Adjust observation error.

!    Increase error for observations over high topography
     factelv=one
     if (data(ielev,i) > r2000) then
        factelv=(r2000/data(ielev,i))**2
        if(luse(i))awork(5) = awork(5) + one
     endif

!    Increase error if model and observation topography too different
     factdif=one
     if (abs(zsges-data(ielev,i)) > r200) then
        factdif= (r200/(abs(zsges-data(ielev,i))))**2
        if(luse(i))awork(6) = awork(6) + one
     endif
     
!    Obtain estimated beam spread in vertical
     addelev=max(half*abs(zsges-data(ielev,i)),ten*wrange)
     beamdepth=two*addelev
     elevtop=zob+addelev     !  this is based on 100ft/Nm = 16.5m/km beam spread
     elevbot=zob-addelev     !  for .95 deg beam angle (multiplied by 1.2 to allow
                             !  for propagation uncertainty)
                             !  also, a minimum uncertainty based on difference between
                             !  model surface elevation and actual radar elevation

     call grdcrd(elevtop,1,zges,nsig,1)
     call grdcrd(elevbot,1,zges,nsig,1)
     kbeamtop=ceiling(elevtop)
     kbeambot=floor(elevbot)
     kbeamtop=max(1,min(kbeamtop,nsig))
     kbeambot=max(1,min(kbeambot,nsig))
     kbeamdiffmax=max(kbeamtop-kbeambot,kbeamdiffmax)
     kbeamdiffmin=min(kbeamtop-kbeambot,kbeamdiffmin)
     

     ratio_errors = factdif*factelv*error/(abs(data(ier,i) + 1.0e6*rhgh +  &
          r8*rlow))
     error = one/error

     if(dpres < zero .or. dpres > rsig)ratio_errors = zero

!    Interpolate guess u and v to observation location and time.
     call tintrp3(ges_u,ugesin,dlat,dlon,dpres,dtime,&
          hrdifsig,1,mype,nfldsig)
     call tintrp3(ges_v,vgesin,dlat,dlon,dpres,dtime,&
          hrdifsig,1,mype,nfldsig)
     call tintrp2a(ges_u,ugesprofile,dlat,dlon,dtime,hrdifsig,&
          1,nsig,mype,nfldsig)
     call tintrp2a(ges_v,vgesprofile,dlat,dlon,dtime,hrdifsig,&
          1,nsig,mype,nfldsig)
     


!    Convert guess u,v wind components to radial value consident with obs
     cosazm  = cos(data(iazm,i))  ! cos(azimuth angle)
     sinazm  = sin(data(iazm,i))  ! sin(azimuth angle)
     costilt = cos(data(itilt,i))  ! cos(tilt angle)
!    rwwind = (ugesin*cosazm+vgesin*sinazm)*costilt*factw
     umaxmax=-huge(umaxmax)
     uminmin=huge(uminmin)
     kminmin=kbeambot
     kmaxmax=kbeamtop
     do k=kbeambot,kbeamtop
        rwwindprofile=(ugesprofile(k)*cosazm+vgesprofile(k)*sinazm)*costilt
        if(umaxmax.lt.rwwindprofile) then
           umaxmax=rwwindprofile
           kmaxmax=k
        end if
        if(uminmin.gt.rwwindprofile) then
           uminmin=rwwindprofile
           kminmin=k
        end if
     end do
     rwwind=data(irwob,i)
     if(data(irwob,i).lt.uminmin) then
        rwwind=uminmin
        dpres=kminmin
     end if
     if(data(irwob,i).gt.umaxmax) then
        rwwind=umaxmax
        dpres=kmaxmax
     end if
     if(rwwind.eq.data(irwob,i)) then
        numequal=numequal+1
     else
        numnotequal=numnotequal+1
     end if
     
     ddiff = data(irwob,i) - rwwind

!    Gross error checks
     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = abs(ddiff)
     ratio    = residual/obserrlm
     if (ratio > cgross(ikx) .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(4) = awork(4)+one
        error = zero
        ratio_errors = zero
     end if
     
     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.
     if (nobskeep>0) muse(i)=obsdiags(i_rw_ob_type,ibin)%tail%muse(nobskeep)
     
     val     = error*ddiff

!    Compute penalty terms (linear & nonlinear qc).
     if(luse(i))then
        exp_arg  = -half*val**2
        rat_err2 = ratio_errors**2
        val2=val*val
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_w=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_w*wnotgross)
           term = log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
        else
           term = exp_arg
           wgt  = wgtlim
           rwgt = wgt/wgtlim
        endif
        valqc = -two*rat_err2*term
        
!       Accumulate statistics for obs belonging to this task
        if (muse(i)) then
           if(rwgt < one) awork(21) = awork(21)+one
           jsig = dpres
           jsig=max(1,min(jsig,nsig))
           awork(6*nsig+jsig+100)=awork(6*nsig+jsig+100)+val2*rat_err2
           awork(5*nsig+jsig+100)=awork(5*nsig+jsig+100)+one
           awork(3*nsig+jsig+100)=awork(3*nsig+jsig+100)+valqc
        end if

!       Loop over pressure level groupings and obs to accumulate
!       statistics as a function of observation type.
        ress  = scale*ddiff
        ressw = ress*ress
        nn=1         
        if (.not. muse(i)) then
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        do k = 1,npres_print
           if(presw >=ptop(k) .and. presw<=pbot(k))then
              bwork(k,ikx,1,nn) = bwork(k,ikx,1,nn)+one            ! count
              bwork(k,ikx,2,nn) = bwork(k,ikx,2,nn)+ddiff          ! bias
              bwork(k,ikx,3,nn) = bwork(k,ikx,3,nn)+ressw          ! (o-g)**2
              bwork(k,ikx,4,nn) = bwork(k,ikx,4,nn)+val2*rat_err2  ! penalty
              bwork(k,ikx,5,nn) = bwork(k,ikx,5,nn)+valqc          ! nonlin qc penalty
              
           end if
        end do
     end if

     obsdiags(i_rw_ob_type,ibin)%tail%luse=luse(i)
     obsdiags(i_rw_ob_type,ibin)%tail%muse(jiter)=muse(i)
     obsdiags(i_rw_ob_type,ibin)%tail%nldepart(jiter)=ddiff
     obsdiags(i_rw_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2
     
!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if ( .not. last .and. muse(i)) then

        if(.not. associated(rwhead(ibin)%head))then
            allocate(rwhead(ibin)%head,stat=istat)
            if(istat /= 0)write(6,*)' failure to write rwhead '
            rwtail(ibin)%head => rwhead(ibin)%head
        else
            allocate(rwtail(ibin)%head%llpoint,stat=istat)
            if(istat /= 0)write(6,*)' failure to write rwtail%llpoint '
            rwtail(ibin)%head => rwtail(ibin)%head%llpoint
        end if

!       Set (i,j,k) indices of guess gridpoint that bound obs location
        call get_ijk(mm1,dlat,dlon,dpres,rwtail(ibin)%head%ij(1),rwtail(ibin)%head%wij(1))

        do j=1,8
           rwtail(ibin)%head%wij(j)=factw*costilt*rwtail(ibin)%head%wij(j)  
        end do
        rwtail(ibin)%head%raterr2 = ratio_errors**2  
        rwtail(ibin)%head%cosazm  = cosazm
        rwtail(ibin)%head%sinazm  = sinazm
        rwtail(ibin)%head%res     = ddiff
        rwtail(ibin)%head%err2    = error**2
        rwtail(ibin)%head%time    = dtime
        rwtail(ibin)%head%luse    = luse(i)
        rwtail(ibin)%head%b       = cvar_b(ikx)
        rwtail(ibin)%head%pg      = cvar_pg(ikx)
        rwtail(ibin)%head%diags => obsdiags(i_rw_ob_type,ibin)%tail
        
     endif

!    Save select output for diagnostic file
     if(conv_diagsave)then
        ii=ii+1
        rstation_id     = data(id,i)
        cdiagbuf(ii)    = station_id         ! station id

        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
    
        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(ielev,i)      ! station elevation (meters)
        rdiagbuf(6,ii)  = presw              ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = rmiss_single       ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        err_input = data(ier2,i)
        err_adjst = data(ier,i)
        if (ratio_errors*error>tiny_r_kind) then
           err_final = one/(ratio_errors*error)
        else
           err_final = huge_single
        endif

        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
        if (err_input>tiny_r_kind) errinv_input = one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst = one/err_adjst
        if (err_final>tiny_r_kind) errinv_final = one/err_final

        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (m/s)**-1
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (m/s)**-1
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (m/s)**-1

        rdiagbuf(17,ii) = data(irwob,i)      ! radial wind speed observation (m/s)
        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (m/s)
        rdiagbuf(19,ii) = data(irwob,i)-rwwind  ! obs-ges w/o bias correction (m/s) (future slot)


        rdiagbuf(20,ii)=data(iazm,i)*rad2deg ! azimuth angle
        rdiagbuf(21,ii)=data(itilt,i)*rad2deg! tilt angle
        rdiagbuf(22,ii) = factw              ! 10m wind reduction factor

        if (lobsdiagsave) then
          ioff=22
          do jj=1,miter 
            ioff=ioff+1
            if (obsdiags(i_rw_ob_type,ibin)%tail%muse(jj)) then
              rdiagbuf(ioff,ii) = one
            else
              rdiagbuf(ioff,ii) = -one
            endif
          enddo
          do jj=1,miter+1
            ioff=ioff+1
            rdiagbuf(ioff,ii) = obsdiags(i_rw_ob_type,ibin)%tail%nldepart(jj)
          enddo
          do jj=1,miter
            ioff=ioff+1
            rdiagbuf(ioff,ii) = obsdiags(i_rw_ob_type,ibin)%tail%tldepart(jj)
          enddo
          do jj=1,miter
            ioff=ioff+1
            rdiagbuf(ioff,ii) = obsdiags(i_rw_ob_type,ibin)%tail%obssen(jj)
          enddo
        endif

     end if
  end do


! Write information to diagnostic file
  if(conv_diagsave)then
     write(7)' rw',nchar,nreal,ii,mype
     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
     deallocate(cdiagbuf,rdiagbuf)
  end if

! End of routine
end subroutine setuprw
