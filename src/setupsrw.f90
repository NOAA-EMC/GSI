subroutine setupsrw(lunin,mype,bwork,awork,nele,nobs,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupsrw    compute rhs of oi for radar wind superobs
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
!
! abstract:  For radar wind superobs, this routine
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!            is special for radar wind superobs.  NOTE: not tested yet--awaiting
!            superob test data sets.
!
! program history log:
!   2004-06-22  parrish, document
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - increase size of srwwork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-05-27  derber - level output change
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-11-03  treadon - correct error in ilone,ilate data array indices
!   2005-11-29  derber - remove psfcg and use ges_lnps instead
!   2006-01-23  treadon - check logical last before loading obsmod arrays
!   2006-01-31  todling/treadon - store wgt/wgtlim in rdiagbuf(6,ii)
!   2006-02-02  treadon - rename lnprsl as ges_lnprsl
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - modify handling of multiple data at same location
!                       - unify NL qc
!   2006-07-31  kleist - use ges_ps instead of lnps
!   2006-08-28      su - fix a bug in variational qc
!   2006-10-12  liu - fix bug in spdb: data(ihat2,i) s/b squared
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-08-28      su - modify the gross check error 
!   2008-05-21  safford - rm unused vars and uses
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
! remarks:
!   NOTE:  This routine is not extensively tested yet -- awaiting more
!          superob test data sets
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_single,r_double,i_kind
  use obsmod, only: srwhead,srwtail,rmiss_single,i_srw_ob_type,obsdiags,&
                    obsptr,lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use guess_grids, only: ges_lnprsl,ges_ps,geop_hgtl,&
       hrdifsig,nfldsig,ges_u,ges_v,sfcmod_gfs,sfcmod_mm5,comp_fact10
  use gridmod, only: nsig
  use gridmod, only: get_ijk
  use qcmod, only: npres_print,dfact,dfact1,ptop,pbot
  use constants, only: izero,ione,flattening,semi_major_axis,grav_ratio,wgtlim,&
       zero,two,grav,grav_equator,one,eccentricity,somigliana,deg2rad,&
       tiny_r_kind,half,cg_term,huge_single
  use jfunc, only: jiter,last,miter
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  implicit none

! Declare passed variables
  logical                                          ,intent(in):: conv_diagsave
  integer(i_kind)                                  ,intent(in):: lunin,mype,nele,nobs
  real(r_kind),dimension(7*nsig+100_i_kind)        ,intent(inout)::awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout):: bwork

! Declare local parameters
  real(r_kind),parameter:: r0_001 = 0.001_r_kind
  real(r_kind),parameter:: r8 = 8.0_r_kind
  real(r_kind),parameter:: ten = 10.0_r_kind
! Declare local variables
  
  
  real(r_double) rstation_id
  real(r_kind) scale,ratio,obserror,obserrlm
  real(r_kind) residual,val,ressw,ress
  real(r_kind) dlat,dlon,dtime,d1diff,d2diff,error
  real(r_kind) val1,val2,valqc
  real(r_kind) cg_srw,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2
  real(r_kind) rlow,rhgh,rsig,dz,dlnp,pobl,rwgt
  real(r_kind) zob,termg,termr,termrg,sin2
  real(r_kind) sfcchk,slat,psges,ratio_errors,spdb
  real(r_kind) presw,factw,dpres,ugesin,vgesin,srw1gesin,srw2gesin
  real(r_kind) errinv_input,errinv_adjst,errinv_final,sfcr,skint
  real(r_kind) err_input,err_adjst,err_final,tfact
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nsig):: hges,zges,prsltmp
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  
  integer(i_kind) jsig,mm1,nn,ikxx,ibin,ioff
  integer(i_kind) idomsfc,isfcr,iff10,iskint
  integer(i_kind) i,nchar,nreal,k,j,ii,l,jj,k1,k2,isli
  integer(i_kind) ier,ilon,ilat,ihgt,ihat1,ihat2,id,itime,ikx,ilate,ilone
  integer(i_kind) ibigu11,ibigu21,ibigu12,ibigu22,iuse,irange,istat

  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf

  logical,dimension(nobs):: luse,muse

  equivalence(rstation_id,station_id)
  
!******************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse

!    index information for data array (see reading routine)

  ilon=ione          ! index of grid relative obs location (x)
  ilat=2_i_kind      ! index of grid relative obs location (y)
  ihgt=3_i_kind      ! index of height of observation
  itime=4_i_kind     ! index of observation time in data array
  ihat1=5_i_kind     ! index of yhat_obs(1)  --1st component of superob observation
  ihat2=6_i_kind     ! index of yhat_obs(2)  --2nd component of superob observation
  ier=7_i_kind       ! index of obs error
  ibigu11=8_i_kind   ! index of forward model bigu(1,1)
  ibigu21=9_i_kind   ! index of forward model bigu(2,1)
  ibigu12=10_i_kind  ! index of forward model bigu(1,2)
  ibigu22=11_i_kind  ! index of forward model bigu(2,2)
  ikxx=12_i_kind     ! index of observation type 
  id=13_i_kind       ! index of station id
  iuse=14_i_kind     ! index of use parameter
  idomsfc=15_i_kind  ! index of dominant surface type
  iskint=16_i_kind   ! index of surface skin temperature
  iff10=17_i_kind    ! index of 10 meter wind factor
  isfcr=18_i_kind    ! index of surface roughness
  irange=19_i_kind   ! index of mean range of superob from radar (meters)
  ilone=20_i_kind    ! index of longitude (degrees)
  ilate=21_i_kind    ! index of latitude (degrees)

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do

  dup=one
  do k=1,nobs
     do l=k+ione,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and.  &
           data(ihgt,k) == data(ihgt,l) .and. &
           muse(k) .and. muse(l))then

           tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
           dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
           dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
        end if
     end do
  end do

! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=izero
     nchar=ione
     nreal=24_i_kind
     if (lobsdiagsave) nreal=nreal+7*miter+2_i_kind
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
  end if
  scale=one
  rsig=float(nsig)
  mm1=mype+ione

  do i=1,nobs
! Convert obs lats and lons to grid coordinates
     dlat=data(ilat,i)
     dlon=data(ilon,i)
     dpres=data(ihgt,i)
     dtime=data(itime,i)
     ikx=nint(data(ikxx,i))
     error=data(ier,i)
     slat=data(ilate,i)*deg2rad

!    Link observation to appropriate observation bin
     if (nobs_bins>ione) then
        ibin = NINT( dtime/hr_obsbin ) + ione
     else
        ibin = ione
     endif
     IF (ibin<ione.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

!    Link obs to diagnostics structure
     do jj=1,2
        if (.not.lobsdiag_allocated) then
           if (.not.associated(obsdiags(i_srw_ob_type,ibin)%head)) then
              allocate(obsdiags(i_srw_ob_type,ibin)%head,stat=istat)
              if (istat/=izero) then
                 write(6,*)'setupsrw: failure to allocate obsdiags',istat
                 call stop2(292)
              end if
              obsdiags(i_srw_ob_type,ibin)%tail => obsdiags(i_srw_ob_type,ibin)%head
           else
              allocate(obsdiags(i_srw_ob_type,ibin)%tail%next,stat=istat)
              if (istat/=izero) then
                 write(6,*)'setupsrw: failure to allocate obsdiags',istat
                 call stop2(293)
              end if
              obsdiags(i_srw_ob_type,ibin)%tail => obsdiags(i_srw_ob_type,ibin)%tail%next
           end if
           allocate(obsdiags(i_srw_ob_type,ibin)%tail%muse(miter+ione))
           allocate(obsdiags(i_srw_ob_type,ibin)%tail%nldepart(miter+ione))
           allocate(obsdiags(i_srw_ob_type,ibin)%tail%tldepart(miter))
           allocate(obsdiags(i_srw_ob_type,ibin)%tail%obssen(miter))
           obsdiags(i_srw_ob_type,ibin)%tail%indxglb=i
           obsdiags(i_srw_ob_type,ibin)%tail%nchnperobs=-99999_i_kind
           obsdiags(i_srw_ob_type,ibin)%tail%luse=.false.
           obsdiags(i_srw_ob_type,ibin)%tail%muse(:)=.false.
           obsdiags(i_srw_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
           obsdiags(i_srw_ob_type,ibin)%tail%tldepart(:)=zero
           obsdiags(i_srw_ob_type,ibin)%tail%wgtjo=-huge(zero)
           obsdiags(i_srw_ob_type,ibin)%tail%obssen(:)=zero
        else
           if (.not.associated(obsdiags(i_srw_ob_type,ibin)%tail)) then
              obsdiags(i_srw_ob_type,ibin)%tail => obsdiags(i_srw_ob_type,ibin)%head
           else
              obsdiags(i_srw_ob_type,ibin)%tail => obsdiags(i_srw_ob_type,ibin)%tail%next
           end if
           if (obsdiags(i_srw_ob_type,ibin)%tail%indxglb/=i) then
              write(6,*)'setupsrw: index error'
              call stop2(294)
           end if
        endif
        if (jj==ione) obsptr => obsdiags(i_srw_ob_type,ibin)%tail
     enddo


! Interpolate log(surface pressure), model terrain, 
! and log(pres) at mid-layers to observation location.
     factw=data(iff10,i)
     if(sfcmod_gfs .or. sfcmod_mm5) then
        sfcr=data(isfcr,i)
        skint=data(iskint,i)
        isli=data(idomsfc,i)
        call comp_fact10(dlat,dlon,dtime,skint,sfcr,isli,mype,factw)
     end if

     call tintrp2a(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
       ione,ione,mype,nfldsig)
     call tintrp2a(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
       ione,nsig,mype,nfldsig)
     call tintrp2a(geop_hgtl,hges,dlat,dlon,dtime,hrdifsig,&
       ione,nsig,mype,nfldsig)
! Convert geopotential height at layer midpoints to geometric height using
! equations (17, 20, 23) in MJ Mahoney's note "A discussion of various
! measures of altitude" (2001).  Available on the web at
! http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
! termg  = equation 17
! termr  = equation 21
! termrg = first term in the denominator of equation 23
! zges   = equation 23

     sin2  = sin(slat)*sin(slat)
     termg = grav_equator * &
           ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
     termr = semi_major_axis /(one + flattening + grav_ratio -  &
                 two*flattening*sin2)
     termrg = (termg/grav)*termr
     do k=1,nsig
        zges(k) = (termr*hges(k)) / (termrg-hges(k))  ! eq (23)
     end do

! Given observation height, (1) adjust 10 meter wind factor if
! necessary, (2) convert height to grid relative units, (3) compute
! compute observation pressure (for diagnostic purposes only), and
! (4) compute location of midpoint of first model layer above surface
! in grid relative units

! Adjust 10m wind factor if necessary.  Rarely do we have a
! lidar obs within 10 meters of the surface.  Almost always,
! the code below resets the 10m wind factor to 1.0 (i.e., no
! reduction in wind speed due to surface friction).
     if (dpres<ten) then
        dz = ten-dpres
        factw = factw + (factw-zero)/dz
     else
        factw=one
     endif

! Convert observation height (in dpres) from meters to grid relative
! units.  Save the observation height in zob for later use.
     zob = dpres
     call grdcrd(dpres,ione,zges,nsig,ione)

! Set indices of model levels below (k1) and above (k2) observation.
     k=dpres
     k1=max(ione,k)
     k2=min(k+ione,nsig)

! Compute observation pressure (only used for diagnostics)
     dz     = zges(k2)-zges(k1)
     dlnp   = prsltmp(k2)-prsltmp(k1)
     pobl   = prsltmp(k1) + (dlnp/dz)*(zob-zges(k1))
     presw  = ten*exp(pobl)

! Determine location in terms of grid units for midpoint of
! first layer above surface
     sfcchk=log(psges)
     call grdcrd(sfcchk,ione,prsltmp,nsig,-ione)

! Check to see if observation is below midpoint of first
! above surface layer.  If so, set rlow to that difference

     rlow=max(sfcchk-dpres,zero)

! Check to see if observation is above midpoint of layer
! at the top of the model.  If so, set rhgh to that difference.
     rhgh=max(dpres-r0_001-nsig,zero)

! Increment obs counter along with low and high obs counters
     if(luse(i))then
        awork(1)=awork(1)+one
        if(rhgh/=zero) awork(2)=awork(2)+one
        if(rlow/=zero) awork(3)=awork(3)+one
     end if

! Adjust observation error.
! Variable repe_dw contains a value which should be tuned to
! account for representativeness error.

     ratio_errors = error/abs(error + 1.0e6_r_kind*rhgh + r8*rlow)
     error = one/error

     if(dpres < zero .or. dpres > rsig)ratio_errors = zero


! Interpolate guess u and v to observation location and time.
     call tintrp3(ges_u,ugesin,dlat,dlon,dpres,dtime,&
        hrdifsig,ione,mype,nfldsig)
     call tintrp3(ges_v,vgesin,dlat,dlon,dpres,dtime,&
        hrdifsig,ione,mype,nfldsig)

! Apply bigu to get simulated superob components
     srw1gesin=data(ibigu11,i)*ugesin+data(ibigu12,i)*vgesin   !  bigu(1,1)*u + bigu(1,2)*v
     srw2gesin=data(ibigu21,i)*ugesin+data(ibigu22,i)*vgesin   !  bigu(2,1)*u + bigu(2,2)*v

! Apply 10-meter wind reduction factor to guess winds.

     srw1gesin=factw*srw1gesin
     srw2gesin=factw*srw2gesin

     d1diff=data(ihat1,i)-srw1gesin
     d2diff=data(ihat2,i)-srw2gesin
     spdb= sqrt(data(ihat1,i)**2+data(ihat2,i)**2)- &
           sqrt(srw1gesin**2+srw2gesin**2)

!    Gross error checks
     obserror=one/max(ratio_errors*error,tiny_r_kind)
     obserrlm=max(cermin(ikx),min(cermax(ikx),obserror))
     residual=sqrt(d1diff**2+d2diff**2)
     ratio=residual/obserrlm
     if(ratio > cgross(ikx) .or. ratio_errors < tiny_r_kind) then
        if(luse(i))awork(4)=awork(4)+one
        error=zero
        ratio_errors=zero
     else
        ratio_errors=ratio_errors/sqrt(dup(i))
     end if
     if (ratio_errors*error <= tiny_r_kind) muse(i)=.false.
     if (nobskeep>izero) muse(i)=obsdiags(i_srw_ob_type,ibin)%tail%muse(nobskeep)

!    Compute penalty terms
     val1     = d1diff*error
     val2     = d2diff*error
     if(luse(i))then
        val      = val1*val1 + val2*val2
        exp_arg  = -half*val
        rat_err2 = ratio_errors**2
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_srw=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_srw*wnotgross)
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
        if(muse(i))then
           if(rwgt < one) awork(21) = awork(21)+one
           jsig = dpres
           jsig=max(ione,min(jsig,nsig))
           awork(4*nsig+jsig+100_i_kind)=awork(4*nsig+jsig+100_i_kind)+val1*val1*rat_err2
           awork(5*nsig+jsig+100_i_kind)=awork(5*nsig+jsig+100_i_kind)+val2*val2*rat_err2
           awork(6*nsig+jsig+100_i_kind)=awork(6*nsig+jsig+100_i_kind)+one
           awork(3*nsig+jsig+100_i_kind)=awork(3*nsig+jsig+100_i_kind)+valqc
        endif

        ress = scale*sqrt(d1diff**2+d2diff**2)
        ressw= ress*ress
        nn=ione
        if (.not. muse(i)) then
           nn=2_i_kind
           if(ratio_errors*error >=tiny_r_kind)nn=3_i_kind
        end if
        do k = 1,npres_print
           if(presw >= ptop(k) .and. presw <= pbot(k))then
 
              bwork(k,ikx,1,nn) = bwork(k,ikx,1,nn)+one          !count
              bwork(k,ikx,2,nn) = bwork(k,ikx,2,nn)+spdb         !bias
              bwork(k,ikx,3,nn) = bwork(k,ikx,3,nn)+ressw        !(o-g)**2
              bwork(k,ikx,4,nn) = bwork(k,ikx,4,nn)+val*rat_err2 !penalty
              bwork(k,ikx,5,nn) = bwork(k,ikx,5,nn)+valqc        !nonlin qc penalty

           end if
        end do

     endif

     do jj=1,2
        obsdiags(i_srw_ob_type,ibin)%tail%luse=luse(i)
        obsdiags(i_srw_ob_type,ibin)%tail%muse(jiter)=muse(i)
        if (jj==ione) then
           obsptr%nldepart(jiter)=d1diff
        else
           obsdiags(i_srw_ob_type,ibin)%tail%nldepart(jiter)=d2diff
        endif
        obsdiags(i_srw_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2
     enddo

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if (.not. last .and. muse(i)) then

        if(.not. associated(srwhead(ibin)%head))then
           allocate(srwhead(ibin)%head,stat=istat)
           if(istat /= izero)write(6,*)' failure to write srwhead '
           srwtail(ibin)%head => srwhead(ibin)%head
        else
           allocate(srwtail(ibin)%head%llpoint,stat=istat)
           if(istat /= izero)write(6,*)' failure to write srwtail%llpoint '
           srwtail(ibin)%head => srwtail(ibin)%head%llpoint
        end if

!       Set (i,j,k) indices of guess gridpoint that bound obs location
        call get_ijk(mm1,dlat,dlon,dpres,srwtail(ibin)%head%ij(1),srwtail(ibin)%head%wij(1))

        do j=1,8
           srwtail(ibin)%head%wij(j)=factw*srwtail(ibin)%head%wij(j)      
        end do

        srwtail(ibin)%head%res1=d1diff
        srwtail(ibin)%head%res2=d2diff
        srwtail(ibin)%head%err2=error**2
        srwtail(ibin)%head%raterr2=ratio_errors**2    
        srwtail(ibin)%head%time=dtime
        srwtail(ibin)%head%b=cvar_b(ikx)
        srwtail(ibin)%head%pg=cvar_pg(ikx)
        srwtail(ibin)%head%rsrw(1)=data(ibigu11,i)
        srwtail(ibin)%head%rsrw(2)=data(ibigu21,i)
        srwtail(ibin)%head%rsrw(3)=data(ibigu12,i)
        srwtail(ibin)%head%rsrw(4)=data(ibigu22,i)
        srwtail(ibin)%head%ges1=srw1gesin
        srwtail(ibin)%head%ges2=srw2gesin
        srwtail(ibin)%head%luse=luse(i)
        srwtail(ibin)%head%diagu => obsptr
        srwtail(ibin)%head%diagv => obsdiags(i_srw_ob_type,ibin)%tail

     end if


! Save select output for diagnostic file
     if(conv_diagsave)then
        ii=ii+ione
        rstation_id     = data(id,i)
        cdiagbuf(ii)    = station_id         ! station id

        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
    
        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = rmiss_single       ! station elevation (meters)
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

        err_input = data(ier,i)
        err_adjst = data(ier,i)
        if (ratio_errors*error>tiny_r_kind) then
           err_final = one/(ratio_errors*error)
        else
           err_final = huge_single
        endif

        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
        if (err_input>tiny_r_kind) errinv_input=one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst=one/err_adjst
        if (err_final>tiny_r_kind) errinv_final=one/err_final

        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error

        rdiagbuf(17,ii) = data(ihat1,i)      ! observation
        rdiagbuf(18,ii) = d1diff             ! obs-ges used in analysis
        rdiagbuf(19,ii) = data(ihat1,i)-srw1gesin ! obs-ges w/o bias correction (future slot)

        rdiagbuf(20,ii) = data(ihat2,i)      ! observation
        rdiagbuf(21,ii) = d2diff             ! obs_ges used in analysis
        rdiagbuf(22,ii) = data(ihat2,i)-srw2gesin ! obs-ges w/o bias correction (future slot)

        rdiagbuf(23,ii) = factw              ! 10m wind reduction factor
        rdiagbuf(24,ii) = data(irange,i)      ! superob mean range from radar (m)

        if (lobsdiagsave) then
           ioff=24_i_kind
           do jj=1,miter
              ioff=ioff+ione
              if (obsdiags(i_srw_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+ione
              ioff=ioff+ione
              rdiagbuf(ioff,ii) = obsptr%nldepart(jj)
              ioff=ioff+ione
              rdiagbuf(ioff,ii) = obsdiags(i_srw_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+ione
              rdiagbuf(ioff,ii) = obsptr%tldepart(jj)
              ioff=ioff+ione
              rdiagbuf(ioff,ii) = obsdiags(i_srw_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+ione
              rdiagbuf(ioff,ii) = obsptr%obssen(jj)
              ioff=ioff+ione
              rdiagbuf(ioff,ii) = obsdiags(i_srw_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif

     end if

  end do

! Write information to diagnostic file
  if(conv_diagsave)then
     write(7)'srw',nchar,nreal,ii,mype
     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
     deallocate(cdiagbuf,rdiagbuf)
  end if

! End of routine
end subroutine setupsrw

