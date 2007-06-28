!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  setupw --- Compute rhs of oi for wind component obs
!
! !INTERFACE:
!

subroutine setupw(lunin,mype,bwork,awork,nele,nobs,conv_diagsave,perturb)

! !USES:

  use kinds, only: r_kind,r_single,r_double,i_kind
  use obsmod, only: wtail,whead,rmiss_single
  use qcmod, only: npres_print,ptop,pbot,dfact,dfact1
  use oneobmod, only: oneobtest,oneob_type,magoberr,maginnov 
  use gridmod, only: get_ijk,nsig,lat2,lon2,twodvar_regional
  use guess_grids, only: nfldsig,hrdifsig,ntguessfc,geop_hgtl
  use guess_grids, only: ges_u,ges_v,tropprs,isli,ges_ps,ges_z
  use guess_grids, only: ges_tv,ges_lnprsl,fact10,nfldsfc,hrdifsfc
  use constants, only: zero,half,one,tiny_r_kind,two,one_tenth,cg_term, &
           rad2deg,three,rd,grav,four,huge_single,r1000,wgtlim
  use constants, only: grav_ratio,flattening,grav,zero,deg2rad, &
       grav_equator,somigliana,semi_major_axis,eccentricity
  use jfunc, only: jiter,first,last
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype

  implicit none
  
! !INPUT PARAMETERS:

   integer(i_kind),intent(in) :: lunin ! unit from which to read observations
   integer(i_kind),intent(in) :: mype  ! mpi task id
   integer(i_kind),intent(in) :: nele  ! number of data elements per observation
   integer(i_kind),intent(in) :: nobs  ! number of observations
   logical,intent(in) :: conv_diagsave ! logical to save innovation dignostics
   real(r_kind),dimension(nobs,2),intent(in):: perturb ! observation perturbation factor
   
! !INPUT/OUTPUT PARAMETERS:

   real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork ! obs-ges stats
   real(r_kind),dimension(100+7*nsig),intent(inout) :: awork ! data counts and gross checks

!
! !DESCRIPTION:  For wind component observations, this routine
!  \begin{enumerate}
!       \item reads obs assigned to given mpi task (geographic region),
!       \item simulates obs from guess,
!       \item apply some quality control to obs,
!       \item load weight and innovation arrays used in minimization
!       \item collects statistics for runtime diagnostic output
!       \item writes additional diagnostic information to output file
!  \end{enumerate}
!
! !REVISION HISTORY:
!
!   1990-10-06  parrish
!   1998-04-10  weiyu yang
!   1999-03-01  wu - ozone processing moved into setuprhs from setupoz
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-17  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue; added intent/only's
!   2004-10-06  parrish - increase size of vwork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-05-27  derber - level output change
!   2005-07-22  jung - add modis winds
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-10-21  su  - modified variational qc and diagnose output
!   2005-11-03  treadon - correct error in ilone,ilate data array indices
!   2005-11-22  wu - add option to perturb conventional obs
!   2005-11-29 derber - remove psfcg and use ges_lnps instead
!   2006-01-13 treadon - correct bugs in modis wind qc
!   2006-01-31  todling - storing wgt/wgtlim in diag file instead of wgt only
!   2006-02-02  treadon - rename lnprsl as ges_lnprsl
!   2006-02-08  treadon - correct vertical dimension (nsig) in call tintrp2a(ges_tv...)
!   2006-02-15  treadon - use height when processing type 223, 224, 229 winds
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-03-21  treadon - modify optional perturbation to observation
!   2006-04-03  derber  - fix bugs and move all surface data to height calculation
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - modify handling of multiple data at same location
!   2006-07-31  kleist - use ges_ps instead of ln(ps)
!   2006-08-28      su - fix a bug in variational qc
!   2006-11-30  jung   - add type 259 for modis winds
!
! REMARKS:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
! !AUTHOR: parrish          org: np22                date: 1990-10-06
!
!EOP
!-------------------------------------------------------------------------

! Declare local parameters
  real(r_kind),parameter:: r7=7.0_r_kind
  real(r_kind),parameter:: r10=10.0_r_kind
  real(r_kind),parameter:: r15=15.0_r_kind
  real(r_kind),parameter:: r50=50.0_r_kind
  real(r_kind),parameter:: r200=200.0_r_kind
  real(r_kind),parameter:: r400=400.0_r_kind
  real(r_kind),parameter:: r2000=2000.0_r_kind
  real(r_kind),parameter:: r1e10=1.e10_r_kind


! Declare local variables

  real(r_double) rstation_id
  real(r_kind) qcu,qcv,qc_spd,qc_prs,trop5,slim5,tfact
  real(r_kind) dx,dy,ds,dx1,dy1,ds1,scale,ratio,obserror,obserrlm
  real(r_kind) residual,ressw,ress,val,val2,valqc2,dudiff,dvdiff
  real(r_kind) valqc,valu,valv,dx10,rlow,rhgh,drpx,prsfc
  real(r_kind) cg_w,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2
  real(r_kind) presw,factw,dpres,ugesin,vgesin,rwgt,dpressave
  real(r_kind) sfcchk,prsln2,error,dtime,dlon,dlat,r0_001,rsig,thirty
  real(r_kind) ratio_errors,goverrd,spdges,spdob,ten,psges,zsges
  real(r_kind) slat,sin2,termg,termr,termrg,dlnp,pobl,uob,vob
  real(r_kind) dz,zob,pob,z1,z2,p1,p2,dz21,dlnp21,spdb,dstn
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final
  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(lat2,lon2):: slimask
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nsig)::prsltmp,tges,zges
  real(r_single),allocatable,dimension(:,:)::rdiagbuf

  integer(i_kind) i,nchar,nreal,k,j,n,l,it,ii,itype,itype1,itype2
  integer(i_kind) jlat,jlon,jsig,mm1
  integer(i_kind) k1,k2,ikxx,nn
  integer(i_kind) ier,ilon,ilat,ipres,iuob,ivob,id,itime,ikx,ielev,iqc
  integer(i_kind) ihgt,ier2,iuse,ilate,ilone,istat

  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf

  logical pibal_height,sfc_data
  logical,dimension(nobs):: luse,muse

  equivalence(rstation_id,station_id)

!******************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse

!    index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  ihgt=5      ! index of height
  iuob=6      ! index of u observation
  ivob=7      ! index of v observation
  id=8        ! index of station id
  itime=9     ! index of observation time in data array
  ikxx=10     ! index of ob type
  ielev=11    ! index of station elevation
  iqc=12      ! index of quality mark
  ier2=13     ! index of original-original obs error ratio
  iuse=14     ! index of use parameter
  ilone=15    ! index of longitude (degrees)
  ilate=16    ! index of latitude (degrees)


  mm1=mype+1
  scale=one
  rsig=nsig
  thirty = 30.0_r_kind
  ten = 10.0_r_kind
  r0_001=0.001_r_kind
  goverrd=grav/rd

  do k=1,lon2
     do j=1,lat2
        slimask(j,k) = isli(j,k,ntguessfc)
     end do
  end do

! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=0
     nchar=1
     nreal=23
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
  end if

  do i=1,nobs
    muse(i)=nint(data(iuse,i)) <= jiter
  end do

  dup=one
  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and.  &
           data(ipres,k)== data(ipres,l) .and. &
           data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
           muse(l) .and. muse(k))then

             tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
             dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
             dup(l)=dup(l)+one-tfact*tfact*(one-dfact)

        end if
     end do
  end do

  do i=1,nobs
    dlat=data(ilat,i)
    dlon=data(ilon,i)
    dtime=data(itime,i)
    error=data(ier2,i)
    ikx=nint(data(ikxx,i))

!   Perturb observation.  Note:  if perturb_obs=.false., the
!   array perturb=0.0 and observation is unchanged.
    obserror = max(cermin(ikx),min(cermax(ikx),data(ier,i)))
    uob = data(iuob,i) + perturb(i,1)*obserror
    vob = data(ivob,i) + perturb(i,2)*obserror


    spdob=sqrt(uob*uob+vob*vob)
    call tintrp2a(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
         1,1,mype,nfldsig)
    call tintrp2a(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
         1,nsig,mype,nfldsig)

    itype=ictype(ikx)

!   Type 221=pibal winds contain a mixture of wind observations reported
!   by pressure and others by height.  Those levels only reported by 
!   pressure have a missing value (ie, large) value for the reported 
!   height.  The logic below determines whether to process type 221 
!   wind observations using height or pressure as the vertical coordinate.
!   If height is not bad (less than 1e10), we use height in the
!   forward model.  Otherwise, use reported pressure.

    pibal_height = .false.
    if ((itype==221) .and. (data(ihgt,i)<r1e10)) pibal_height = .true.

!   Process observations reported with height differently than those
!   reported with pressure.  Type 223=profiler and 224=vadwnd are 
!   encoded in NCEP prepbufr files with geometric height above 
!   sea level.  Type 229=pibal profiler is reported using 
!   geopotenital height.  Some type 221=pibal wind observations are
!   also repoted using geopotential height.

    sfc_data = (itype >=280 .and. itype < 290) .and. (.not.twodvar_regional)
    if (pibal_height .or. itype==223 .or. itype==224 .or. itype==229 &
        .or. sfc_data) then

       drpx = zero
       dpres = data(ihgt,i)
       dstn = data(ielev,i)

!      Get guess surface elevation and geopotential height profile 
!      at observation location.
       call tintrp2a(ges_z,zsges,dlat,dlon,dtime,hrdifsig,&
            1,1,mype,nfldsig)
       call tintrp2a(geop_hgtl,zges,dlat,dlon,dtime,hrdifsig,&
            1,nsig,mype,nfldsig)

!      For observation reported with geometric height above sea level,
!      convert geopotential to geometric height.

       if (itype==223 .or. itype==224 .or. sfc_data) then
!         Convert geopotential height at layer midpoints to geometric 
!         height using equations (17, 20, 23) in MJ Mahoney's note 
!         "A discussion of various measures of altitude" (2001).  
!         Available on the web at
!         http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
!         termg  = equation 17
!         termr  = equation 21
!         termrg = first term in the denominator of equation 23
!         zges  = equation 23
       
          slat = data(ilate,i)*deg2rad
          sin2  = sin(slat)*sin(slat)
          termg = grav_equator * &
               ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
          termr = semi_major_axis /(one + flattening + grav_ratio -  &
               two*flattening*sin2)
          termrg = (termg/grav)*termr
          do k=1,nsig
             zges(k) = (termr*zges(k)) / (termrg-zges(k))  ! eq (23)
          end do

       endif
       do k=1,nsig
          zges(k) = zges(k) + zsges  ! Add in surface elevation
       end do

!      Given observation height, (1) adjust 10 meter wind factor if
!      necessary, (2) convert height to grid relative units, (3) compute
!      compute observation pressure (for diagnostic purposes only), and
!      (4) compute location of midpoint of first model layer above surface
!      in grid relative units

!      Adjust 10m wind factor if necessary.  Rarely do we have a
!      profiler/vad obs within 10 meters of the surface.  Almost always,
!      the code below resets the 10m wind factor to 1.0 (i.e., no
!      reduction in wind speed due to surface friction).

!      Convert observation height (in dpres) from meters to grid relative
!      units.  Save the observation height in zob for later use.
       zob = dpres
       call grdcrd(dpres,1,zges,nsig,1)

!      Interpolate guess u and v to observation location and time.

       call tintrp3(ges_u,ugesin,dlat,dlon,dpres,dtime, &
          hrdifsig,1,mype,nfldsig)
       call tintrp3(ges_v,vgesin,dlat,dlon,dpres,dtime, &
          hrdifsig,1,mype,nfldsig)

       if (zob > zges(1)) then
          factw=one
       else
         call tintrp2a(fact10,factw,dlat,dlon,dtime,hrdifsfc,&
             1,1,mype,nfldsfc)
         if (zges(1)-zob <= ten) then
            if(zob-dstn < ten)then
              term = max(zob-dstn,zero)/ten
              factw = term*factw
            end if
         else
            term = (zges(1)-zob)/(zges(1)-ten)
            factw = one-term+factw*term
         end if
         ugesin=factw*ugesin
         vgesin=factw*vgesin

       endif

       if(sfc_data .or. dpres < one) then
         drpx=0.005*abs(dstn-zsges)
       end if

!      Compute observation pressure (only used for diagnostics)

!      Set indices of model levels below (k1) and above (k2) observation.
       if (dpres<one) then
          z1=zsges;   p1=log(psges)
          z2=zges(1); p2=prsltmp(1)
       elseif (dpres>nsig) then
          z1=zges(nsig-1); p1=prsltmp(nsig-1)
          z2=zges(nsig);   p2=prsltmp(nsig)
          drpx = 1.e6_r_kind
       else
          k=dpres
          k1=min(max(1,k),nsig)
          k2=max(1,min(k+1,nsig))
          z1=zges(k1); p1=prsltmp(k1)
          z2=zges(k2); p2=prsltmp(k2)
       endif
       
       dz21     = z2-z1
       dlnp21   = p2-p1
       dz       = zob-z1
       pobl     = p1 + (dlnp21/dz21)*dz
       presw    = ten*exp(pobl)

!      Determine location in terms of grid units for midpoint of
!      first layer above surface
       sfcchk=zsges
       call grdcrd(sfcchk,1,zges,nsig,1)


!   Process observations with reported pressure
    else
       dpres = data(ipres,i)
       presw = ten*exp(dpres)
       dpres = dpres-log(psges)
       drpx=zero
       
       prsfc=psges
       prsln2=log(exp(prsltmp(1))/prsfc)
       dpressave=dpres

!      Put obs pressure in correct units to get grid coord. number
       dpres=log(exp(dpres)*prsfc)
       call grdcrd(dpres,1,prsltmp(1),nsig,-1)

!      Interpolate guess u and v to observation location and time.

       call tintrp3(ges_u,ugesin,dlat,dlon,dpres,dtime, &
          hrdifsig,1,mype,nfldsig)
       call tintrp3(ges_v,vgesin,dlat,dlon,dpres,dtime, &
          hrdifsig,1,mype,nfldsig)
       if(dpressave <= prsln2)then
          factw=one
       else
          call tintrp2a(fact10,factw,dlat,dlon,dtime,hrdifsfc,&
             1,1,mype,nfldsfc)
          call tintrp2a(ges_tv,tges,dlat,dlon,dtime,hrdifsig,&
             1,nsig,mype,nfldsig)
!         Apply 10-meter wind reduction factor to guess winds
          dx10=-goverrd*ten/tges(1)
          if (dpressave < dx10)then
             term=(prsln2-dpressave)/(prsln2-dx10)
             factw=one-term+factw*term
          end if
          ugesin=factw*ugesin   
          vgesin=factw*vgesin

       end if
       
!      Get approx k value of sfc by using surface pressure
       sfcchk=log(psges)
       call grdcrd(sfcchk,1,prsltmp(1),nsig,-1)

    endif


!   Checks based on observation location relative to model surface and top
    rlow=max(sfcchk-dpres,zero)
    rhgh=max(dpres-r0_001-rsig,zero)
    if(luse(i))then
       awork(1) = awork(1) + one
       if(rlow/=zero) awork(2) = awork(2) + one
       if(rhgh/=zero) awork(3) = awork(3) + one
    end if
    ratio_errors=error/((data(ier,i)+drpx+1.0e6*rhgh+four*rlow)*sqrt(dup(i)))

!   Invert observation error
    error=one/error

!   Check to see if observation below model surface or above model top.
!   If so, don't use observation
    if (dpres>rsig) ratio_errors=zero
    if ( (itype==221 .or. itype==223 .or. itype==224 .or. itype==229) &
         .and. (dpres<zero) ) ratio_errors=zero


! Compute innovations
     dudiff=uob-ugesin
     dvdiff=vob-vgesin
     spdb=sqrt(uob**2+vob**2)-sqrt(ugesin**2+vgesin**2)

! QC MODIS winds
    if (itype==257 .or. itype==258 .or. itype==259) then

!       Get guess values of tropopause pressure and sea/land/ice
!       mask at observation location
        call intrp2a(tropprs,trop5,dlat,dlon,1,1,mype)
        call intrp2a(slimask,slim5,dlat,dlon,1,1,mype)
        prsfc = r10*prsfc       ! surface pressure in hPa

!       Compute observed and guess wind speeds (m/s).  
        spdges = sqrt(ugesin* ugesin +vgesin* vgesin )

!       Set and computes modis specific qc parameters
        qcu = r7
        qcv = r7
        qc_spd = (spdges+r15)/three
        
        qc_prs=zero
        if (itype==257) qc_prs = prsfc - r200
        if (itype==258 .or. itype==259) qc_prs = r400

        if ( presw > qc_prs .and. qc_spd < qcu ) then
           qcu = (spdob + r15)/three
           qcv = (qcv*qcu)/r7
        endif

        if (presw < trop5-r50 .or. &                     !  tropopause check
            abs(dudiff) > qcu .or. &                     !  u component check
            abs(dvdiff) > qcv .or. &                     !  v component check
            (presw > prsfc-r200 .and. slim5 > zero))then ! near surface check
           error = zero
        endif
     endif

!    If requested, setup for single obs test.
     if (oneobtest) then
        if (oneob_type.eq.'u') then
           dudiff=maginnov
           dvdiff=zero
        elseif (oneob_type.eq.'v') then
           dudiff=zero
           dvdiff=maginnov
        endif
        error=one/magoberr
        ratio_errors=one
     end if

!    Gross error checks
     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = sqrt(dudiff**2+dvdiff**2)
     ratio    = residual/obserrlm
     if (ratio>cgross(ikx) .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(4) = awork(4)+one
        error = zero
        ratio_errors = zero
     end if

     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.

     valu     = error*dudiff
     valv     = error*dvdiff

!    Compute penalty terms (linear & nonlinear qc).
     if(luse(i))then
        val      = valu*valu+valv*valv
        exp_arg  = -half*val
        rat_err2 = ratio_errors**2
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_w=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_w*wnotgross)
           term =log((arg+wgross)/(one+wgross))
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
           awork(4*nsig+jsig+100)=awork(4*nsig+jsig+100)+valu*valu*rat_err2
           awork(5*nsig+jsig+100)=awork(5*nsig+jsig+100)+valv*valv*rat_err2
           awork(6*nsig+jsig+100)=awork(6*nsig+jsig+100)+one
           awork(3*nsig+jsig+100)=awork(3*nsig+jsig+100)+valqc
        end if

! Loop over pressure level groupings and obs to accumulate statistics
! as a function of observation type.
       ress  = scale*sqrt(dudiff**2+dvdiff**2)
       ressw = ress*ress
       val2    = half*(valu*valu+valv*valv)
       valqc2  = half*valqc
       nn=1
       if (.not. muse(i)) then
         nn=2
         if(ratio_errors*error >=tiny_r_kind)nn=3
       end if
       do k = 1,npres_print
         if(presw >=ptop(k) .and. presw<=pbot(k))then
          bwork(k,ikx,1,nn) = bwork(k,ikx,1,nn)+one            ! count
          bwork(k,ikx,2,nn) = bwork(k,ikx,2,nn)+spdb           ! speed bias
          bwork(k,ikx,3,nn) = bwork(k,ikx,3,nn)+ressw          ! (o-g)**2
          bwork(k,ikx,4,nn) = bwork(k,ikx,4,nn)+val2*rat_err2  ! penalty
          bwork(k,ikx,5,nn) = bwork(k,ikx,5,nn)+valqc2         ! nonlin qc penalty
  
         end if
       end do
     end if

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)

     if (.not. last .and. muse(i)) then

        if(.not. associated(whead))then
            allocate(whead,stat=istat)
            if(istat /= 0)write(6,*)' failure to write whead '
            wtail => whead
        else
            allocate(wtail%llpoint,stat=istat)
            wtail => wtail%llpoint
            if(istat /= 0)write(6,*)' failure to write wtail%llpoint '
        end if

        call get_ijk(mm1,dlat,dlon,dpres,wtail%ij(1),wtail%wij(1))

        do j=1,8
          wtail%wij(j)=factw*wtail%wij(j)              
        end do

        wtail%ures=dudiff
        wtail%vres=dvdiff
        wtail%err2=error**2
        wtail%raterr2=ratio_errors **2  
        wtail%time=dtime
        wtail%b=cvar_b(ikx)
        wtail%pg=cvar_pg(ikx)
        wtail%luse=luse(i)

     end if

!    Save select output for diagnostic file
     if (conv_diagsave .and. luse(i)) then
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
        rdiagbuf(8,ii)  = dtime              ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
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

        rdiagbuf(17,ii) = data(iuob,i)       ! u wind component observation (m/s)
        rdiagbuf(18,ii) = dudiff             ! u obs-ges used in analysis (m/s)
        rdiagbuf(19,ii) = uob-ugesin         ! u obs-ges w/o bias correction (m/s) (future slot)

        rdiagbuf(20,ii) = data(ivob,i)       ! v wind component observation (m/s)
        rdiagbuf(21,ii) = dvdiff             ! v obs-ges used in analysis (m/s)
        rdiagbuf(22,ii) = vob-vgesin         ! v obs-ges w/o bias correction (m/s) (future slot)

        rdiagbuf(23,ii) = factw              ! 10m wind reduction factor

     endif
! End of loop over observations
  end do


! Write information to diagnostic file
  if(conv_diagsave)then
     write(7)' uv',nchar,nreal,ii,mype
     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
     deallocate(cdiagbuf,rdiagbuf)
  end if


! End of routine
end subroutine setupw

