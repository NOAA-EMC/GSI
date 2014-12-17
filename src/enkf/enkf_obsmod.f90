module enkf_obsmod
!$$$  module documentation block
!
! module: obsmod           module to read obs, obs metadata and ob priors
!                          (written out by GSI forward operator)
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: holds observation and observation prior data, and associated
! metadata. Includes subroutines for reading the data from files written
! out by GSI forward operator code, and screening out data with gross errors.
!
! Public Subroutines:
!   readobs: reads obs, obs priors and associated metadata from files 
!    diag* files output from GSI forward operator, distributes data to 
!    all tasks. 
!   obsmod_cleanup: deallocate allocated arrays.
!
! Private Subroutines:
!   screenobs: screen out obs with large observation errors or 
!    that fail background check.
!   channelstats: compute number of obs per satellite sensor/channel
!    (numobspersat) and mean observation error (oberrvarmean) - excluding
!    screened obs. Called by public subroutine
!    readobs.
!
! Public Variables (all defined by subroutine readobs):
!   nobs_conv (integer scalar):  number of "conventional" obs (from prepbufr file).
!   nobs_oz (integer scalar): number of sbuv ozone obs.
!   nobs_sat (integer scalar): number of satellite radiance obs.
!   nobstot (integer scalar): total number of obs (=nobs_conv+nobs_oz+nobs_sat)
!   nobsgood (integer scalar):  total number of obs to assimilate (excluding
!    obs that were screened by subroutine screenobs).
!    Defined in subroutine screenobs, called by readobs.
!   jpch_rad: (integer scalar) total number of satellite sensors/channels
!    (imported from module radinfo).
!   npred: (integer scalar) total number of adaptive bias correction terms
!    (imported from module radinfo).
!   indxsat(nobs_sat): maps satellite radiance observation number
!    (1,2,3...,nobs_sat) to satellite sensor/channel number (1,2,3...jpch_rad).
!    jpch_rad is total number of satellite sensor/channels, defined by module
!    radinfo. 
!   ob(nobstot): real array containing observation values.
!   obloclon(nobstot): real array of observation longitudes (degrees)
!   obloclat(nobstot): real array of observation longitudes (degrees)
!   obpress(nobstot): real array of observation pressures (hPa). For satellite
!    radiances, these are assumed to be the maximum of the weighting function
!    for that sensor/channel.
!   oblnp(nobstot): as above, but log(pressure).
!   obtime(nobstot): real array of observation time offsets (in hours, 
!    relative to analysis time).
!   obtype(nobstot): character(20) array of observation types ('ps','  u','  v',
!    'oz','t','amsua_n15', etc.).
!   oberrvar(nobstot): real array of observation error variances (which have
!    been modified by GSI forward operator).
!   oberrvar_orig(nobstot):  real array of unmodified observation error 
!    variance (as defined by bufr file or external error table).
!   ensmean_ob(nobstot): real array containing ensemble mean ob prior, 
!    including bias correction.
!   ensmean_obnobc(nobstot): as above, but not includeing bias correction.
!   obfit_prior(nobstot): obs(nob)-ensmean_ob(nob) for prior.
!   obfit_post(nobstot): obs(nob)-ensmean_ob(nob) for posterior (not
!    defined until after analysis).
!   obsprd_prior(nobstot): real array of ensemble variance of 
!     observation prior perturbations.
!   obsprd_post(nobstot): real array of ensemble variance of 
!     observation posterior perturbations (not defined until after analysis,
!     and then only on root task).
!   numobspersat(jpch_rad):  number of screened obs for each
!     satellite sensor/channel.
!   oberrvarmean(jpch_rad):  mean observation error variance for
!     each satellite sensor/channel for all screened obs
!   biaspreds(npred+1, nobs_sat):  real array of bias predictors for 
!     each satellite radiance ob (includes non-adaptive scan angle
!     bias correction term in biaspreds(1,1:nobs_sat)).
!   deltapredx(npred,jpch_rad): real array of bias coefficient increments
!     (initialized to zero, updated by analysis).
!   obloc(3,nobsgood): real array of spherical cartesian coordinates
!     (x,y,z) of screened observation locations.
!   stattype(nobsgood):  integer array containing prepbufr report type
!     (e.g. 120 for radiosonde temp) for "conventional" obs (nob <= nobs_conv)
!     and satellite channel number for satellite radiance obs (nob >
!     nobs_conv+nobs_oz). For ozone obs (nobs_conv<nob<nobs_sat), 
!     set to 700 + k, where k is the  level index of the ozone retrieval.
!   nlocconv(blatnum+1,blonnum+1): integer array indicating the number of
!     conventional observations included in this and the previous
!     observation boxes.
!   nlocoz(blatnum+1,blonnum+1): integer array indicating the number of
!     ozone observations included in this and the previous observation boxes.
!   nlocsat(blatnum+1,blonnum+1): integer array indicating the number of
!     satellite radiance observations included in this and the previous
!     observation boxes.
!   boxlat(blatnum): real array containing the latitude of border of
!     observation boxes.
!   boxlon(blonnum): real array containing the longitude of border of
!     observation boxes.
!
! Modules Used: mpisetup, params, kinds, constants, mpi_readobs
!
! program history log:
!   2009-02-23  Initial version.
!   2011-06-20  Added the option of observation box for LETKF.
!
! attributes:
!   language: f95
!
!$$$

use mpisetup
use kinds, only : r_kind, r_double, i_kind, r_single
use constants, only: zero, one, deg2rad, rad2deg, rd, cp, pi
use params, only: & 
      datestring,datapath,sprd_tol,nanals,saterrfact, &
      lnsigcutoffnh, lnsigcutoffsh, lnsigcutofftr, corrlengthnh,&
      corrlengthtr, corrlengthsh, obtimelnh, obtimeltr, obtimelsh,&
      lnsigcutoffsatnh, lnsigcutoffsatsh, lnsigcutoffsattr,&
      varqc, huber, zhuberleft, zhuberright,&
      lnsigcutoffpsnh, lnsigcutoffpssh, lnsigcutoffpstr, boxsize, letkf_flag

use mpi_readobs, only:  mpi_getobs

implicit none
private
public :: readobs, obsmod_cleanup, boxsort

real(r_single), public, allocatable, dimension(:) :: obsprd_prior, ensmean_obnobc,&
 ensmean_ob, ob, oberrvar, obloclon, obloclat, &
 obpress, obtime, oberrvar_orig,&
 oblnp, obfit_prior, prpgerr, oberrvarmean, probgrosserr, &
 lnsigl,corrlengthsq,obtimel, boxlat, boxlon
integer(i_kind), public, allocatable, dimension(:) :: numobspersat
! posterior stats computed in enkf_update
real(r_single), public, allocatable, dimension(:) :: obfit_post, obsprd_post
real(r_single), public, allocatable, dimension(:,:) :: biaspreds
real(r_kind), public, allocatable, dimension(:,:) :: deltapredx
! arrays passed to kdtree2 routines must be single.
real(r_single), public, allocatable, dimension(:,:) :: obloc
integer(i_kind), public, allocatable, dimension(:) :: stattype, indxsat
real(r_single), public, allocatable, dimension(:) :: biasprednorm,biasprednorminv
character(len=20), public, allocatable, dimension(:) :: obtype
integer(i_kind), public ::  nobs_sat, nobs_oz, nobs_conv, nobstot, nobsgood

! anal_ob is only used here and in loadbal. It is deallocated in loadbal.
real(r_single), public, allocatable, dimension(:,:) :: anal_ob
integer(i_kind), public, allocatable, dimension(:,:) :: nlocconv,nlocsat,nlocoz
integer(i_kind), public :: blatnum, blonnum, boxmax


contains

subroutine readobs(npts,lonsgrd,latsgrd)
! reads obs, obs priors and associated metadata from 
! diag* files output from GSI forward operator, distributes data to 
! all tasks.  Ob prior perturbations for each ensemble member
! are written to a temp file, since the entire array can be 
! very large.
use radinfo, only: npred,nusis,nuchan,jpch_rad,iuse_rad,radinfo_read,predx,pg_rad
use convinfo, only: convinfo_read, init_convinfo, cvar_pg, nconvtype, ictype,&
                    ioctype
use ozinfo, only: init_oz, ozinfo_read, pg_oz, jpch_oz, nusis_oz, nulev
use covlocal, only: latval
integer(i_kind),intent(in) :: npts
real(r_single),intent(in) :: lonsgrd(npts)
real(r_single),intent(in) :: latsgrd(npts)
integer nob,n,j,ierr
real(r_double) t1
real(r_single) tdiff,tdiffmax,deglat,radlat,radlon
integer(i_kind) imin,imax,jmin,jmax
! read in conv data info
call init_convinfo()
call convinfo_read()
! read in oz data info
call init_oz()
call ozinfo_read()
! read radiance bias correction info (standard out redirected 
! specified unit number)
call radinfo_read()
!==> read in obs, obs metadata and ob priors.
!  (reading occurs on root, data broadcast to other tasks).
! temp files hxprime_ens_YYYYMMDDHH* are created containing 
! ob prior perturbations for each ensemble member.
! make bias predictors unitless and O(1)
! so bias coefficents have same units as radiance obs.
! (by computing RMS values over many analyses)
if (nproc == 0) print*, 'npred  = ', npred
allocate(biasprednorm(npred),biasprednorminv(npred))
!biasprednorm(1) = 0.01_r_single   ! constant term
!biasprednorm(2) = 2.6e-2_r_single ! scan angle path
!biasprednorm(3) = 1.6e-2_r_single ! total column water
!biasprednorm(5) = 1.9e-2_r_single ! integrated weighted (by weighting fns) lapse rate
!biasprednorm(4) = zero   ! IWLR**2, don't use this predictor (too co-linear)?
!biasprednorm(4) = 1.1e-3_r_single
!biasprednorm(4) = zero   ! don't use this predictor (too co-linear)?
! what the heck, just scale them all by 0.01!
!biasprednorm = 0.01_r_single
biasprednorm=one
biasprednorminv=zero
do n=1,npred
   if (nproc == 0) print *,n,'biasprednorm = ',biasprednorm(n)
   if (biasprednorm(n) > 1.e-7_r_single) biasprednorminv(n)=one/biasprednorm(n)
enddo
! scale bias coefficients.
do j=1,jpch_rad
   predx(:,j) = biasprednorm(:)*predx(:,j)
enddo 
! allocate array for bias correction increments, initialize to zero.
allocate(deltapredx(npred,jpch_rad))
deltapredx = zero
t1 = mpi_wtime()
call mpi_getobs(datapath, datestring, nobs_conv, nobs_oz, nobs_sat, nobstot, &
                obsprd_prior, ensmean_obnobc, ensmean_ob, ob, &
                oberrvar, obloclon, obloclat, obpress, &
                obtime, oberrvar_orig, stattype, obtype, biaspreds,&
                anal_ob,indxsat,nanals)
tdiff = mpi_wtime()-t1
call mpi_reduce(tdiff,tdiffmax,1,mpi_real4,mpi_max,0,mpi_comm_world,ierr)
if (nproc == 0) print *,'max time in mpireadobs  = ',tdiffmax
allocate(obfit_prior(nobstot))
! allocate satellite sensor/channel index array.
call screenobs()
nobsgood=nobstot
! 'good' obs are those we have decided to assimilate (not screened out).
! currently assumed to be all observations. Most obs screened out in read routines
!Any additional obs screened out have their ob err set to a large number.  
if(nproc == 0)write(6,*) 'compressed total number of obs ',nobsgood, '(',nobstot,')'

! Observation box partition for LETKF
if(letkf_flag) then
   ! Create observation box
   jmax=ceiling(maxval(latsgrd(1:npts))*rad2deg/boxsize)+1
   jmin=int(minval(latsgrd(1:npts))*rad2deg/boxsize)-1
   imax=ceiling(maxval(lonsgrd(1:npts))*rad2deg/boxsize)+1
   imin=int(minval(lonsgrd(1:npts))*rad2deg/boxsize)-1
   blatnum=jmax-jmin
   blonnum=imax-imin
   allocate(boxlat(blatnum))
   allocate(boxlon(blonnum))
   allocate(nlocconv(blatnum+1,blonnum+1))
   allocate(nlocsat (blatnum+1,blonnum+1))
   allocate(nlocoz  (blatnum+1,blonnum+1))
   boxlat(1)=real(jmin,r_single)*boxsize
   do j=2,blatnum
      boxlat(j) = boxlat(1)+real(j-1,r_single)*boxsize
   end do
   boxlon(1)=real(imin,r_single)*boxsize
   do j=2,blonnum
      boxlon(j) = boxlon(1)+real(j-1,r_single)*boxsize
   end do
   ! Sorting with each observation box
   boxmax=0
   if(nobs_conv /= 0) then
      call boxsort(1,nobs_conv,.false.,nlocconv)
   else
      nlocconv(:,:)=0
   end if
   if(nobs_oz /= 0) then
      call boxsort(nobs_conv+1,nobs_conv+nobs_oz,.false.,nlocoz)
   else
      nlocoz(:,:)=nobs_conv
   end if
   if(nobs_sat /= 0) then
      call boxsort(nobs_conv+nobs_oz+1,nobs_conv+nobs_oz+nobs_sat,.true.,nlocsat)
   else
      nlocsat(:,:)=nobs_conv+nobs_oz
   end if
   if(nproc == 0) print *,'Max number of observations in the box:',boxmax
   ! deg -> radian for observation box latitude and longitude
   boxlat(1:blatnum)=boxlat(1:blatnum)*deg2rad
   boxlon(1:blonnum)=boxlon(1:blonnum)*deg2rad
end if

allocate(probgrosserr(nobsgood),prpgerr(nobsgood))
! initialize prob of gross error to 0.0 (will be reset by analysis if varqc is true)
probgrosserr = zero
if (varqc .and. .not. huber) then
   ! for flat-tail VarQC, read in a-prior prob of gross error.
   prpgerr = zero ! initialize to zero
   do nob=1,nobsgood
      if (nob <= nobs_conv) then
         ! search for matching record in convinfo file. 
         ! if match found, set prob. of gross error to nonzero value given in
         ! file.
         do j=1,nconvtype
            if (ictype(j) == stattype(nob) .and. &
                ioctype(j) == obtype(nob)(1:3)) then
                prpgerr(nob) = cvar_pg(j)
                exit
            end if     
         enddo
      else if (nob <= nobs_conv+nobs_oz) then
         do j=1,jpch_oz
            if (stattype(nob)-700 == nulev(j) .and. obtype(nob) == nusis_oz(j)) then
                prpgerr(nob) = pg_oz(j)
                exit
            end if
         enddo
      else
         prpgerr(nob) = pg_rad(indxsat(nob-(nobs_conv+nobs_oz)))
      end if
      !if (nproc == 0) print *,nob,obtype(nob)(1:3),prpgerr(nob)
   enddo
endif
! compute number of usuable obs, average ob error for each satellite sensor/channel.
if (nobs_sat > 0) then
  do nob=1,nobs_sat
     do n=2,npred+1
       biaspreds(n,nob)=biaspreds(n,nob)* biasprednorminv(n-1)
     end do
  end do
  call channelstats()
end if

! calculate locations of obs that passed initial screening in cartesian coords.
allocate(obloc(3,nobsgood))
allocate(oblnp(nobstot)) ! log(p) at ob locations.
allocate(corrlengthsq(nobsgood),lnsigl(nobsgood),obtimel(nobsgood))
do nob=1,nobsgood
   oblnp(nob) = -log(obpress(nob)) ! distance measured in log(p) units
   if (obloclon(nob) < zero) obloclon(nob) = obloclon(nob) + 360._r_single
   radlon=deg2rad*obloclon(nob)
   radlat=deg2rad*obloclat(nob)
! cartesian coordinates of 'good' obs.
   obloc(1,nob) = cos(radlat)*cos(radlon)
   obloc(2,nob) = cos(radlat)*sin(radlon)
   obloc(3,nob) = sin(radlat)
   deglat = obloclat(nob)
!  get limits on corrlength,lnsig,and obtime
   if (nob > nobs_conv+nobs_oz) then
      lnsigl(nob) = latval(deglat,lnsigcutoffsatnh,lnsigcutoffsattr,lnsigcutoffsatsh)
   else if (obtype(nob)(1:3) == ' ps') then
      lnsigl(nob) = latval(deglat,lnsigcutoffpsnh,lnsigcutoffpstr,lnsigcutoffpssh)
   else
      lnsigl(nob)=latval(deglat,lnsigcutoffnh,lnsigcutofftr,lnsigcutoffsh)
   end if
   corrlengthsq(nob)=latval(deglat,corrlengthnh,corrlengthtr,corrlengthsh)**2
   obtimel(nob)=latval(deglat,obtimelnh,obtimeltr,obtimelsh)

end do

! these allocated here, but not computed till after the state 
! update in enkf_update.
allocate(obfit_post(nobsgood))
allocate(obsprd_post(nobsgood))
obsprd_post = zero
end subroutine readobs

subroutine screenobs()
! screen out obs with large observation errors or 
! that fail background check.  For screened obs oberrvar is set to 1.e31_r_single
!use radbias, only: apply_biascorr
use radinfo, only: iuse_rad,nuchan,nusis,jpch_rad
real(r_single) fail,failm
integer nn,nob
fail=1.e31_r_single
failm=1.e30_r_single
! apply bias correction here just for debugging purposes.
!call apply_biascorr()
!==> pre-process obs, obs metadata.
do nob=1,nobstot
  if (nob > nobs_conv+nobs_oz) oberrvar(nob) = saterrfact*oberrvar(nob)
  ! empirical adjustment of obs errors for Huber norm from ECMWF RD tech memo
  if (varqc) oberrvar(nob) = oberrvar(nob)*(min(one,0.5_r_single+0.125_r_single*(zhuberleft+zhuberright)))**2

  obfit_prior(nob) = ob(nob)-ensmean_ob(nob)

  ! gross error check.
  if (obsprd_prior(nob) > 1.e9) then
    oberrvar(nob)= fail
    if (obtype(nob)(1:3) == '  u') oberrvar(nob)=fail
    if (obtype(nob)(1:3) == '  v' .and. oberrvar(nob-1) >= failm) oberrvar(nob-1)=fail
    if(nproc == 0)print *, ' ob rejected due to obsprd_prior > 1.e9 ',obsprd_prior(nob)
    cycle
  end if

  if (obtype(nob) == ' ps' .and. (stattype(nob) == 111 .or. stattype(nob) == 112)) then
      ! tcvitals obs get a free pass for background check.
      if (nproc == 0) then
      print *,'tcvitals ps ob found (lat,lon,time,ob,O-F,sprd,oberr_std):'
      write(6,9101) stattype(nob),obloclat(nob),obloclon(nob),obtime(nob),ob(nob),obfit_prior(nob),sqrt(obsprd_prior(nob)),sqrt(oberrvar(nob))
      endif
  else
     ! background checks.
     if (abs(obfit_prior(nob)) > sprd_tol*sqrt(obsprd_prior(nob)+oberrvar(nob))) then
       !print *,obtype(nob)(1:3),stattype(nob),ob(nob),ensmean_ob(nob),obsprd_prior(nob),oberrvar(nob)
       if(nproc == 0)print *, 'reject ob due to obsprd_prior > sprd_tol ',&
          obtype(nob)(1:3),obfit_prior(nob),sprd_tol*sqrt(obsprd_prior(nob)+oberrvar(nob))
       oberrvar(nob)= fail
       if (obtype(nob)(1:3) == '  u') oberrvar(nob)=fail
       if (obtype(nob)(1:3) == '  v' .and. oberrvar(nob-1) >= failm) oberrvar(nob-1)=fail
       cycle
     end if
  end if


enddo
if (nproc == 0) then
   nn = 0
   do nob=1,nobstot
     if(oberrvar(nob) >= failm)nn = nn + 1
   end do
   print *,nobstot-nn,'obs kept'
   print *,nn,'total obs rejected'
end if ! nproc=0
9101 format(i3,7(1x,f7.2))

end subroutine screenobs

subroutine channelstats
use radinfo, only: npred,nusis,nuchan,jpch_rad
implicit none
integer(i_kind) nob,nob2,i
! count number of obs per channel/sensor.
allocate(numobspersat(jpch_rad))
allocate(oberrvarmean(jpch_rad))
numobspersat = 0
oberrvarmean = zero
do nob=1,nobs_sat
   nob2=nob+nobs_conv+nobs_oz
   i=indxsat(nob)
   numobspersat(i) = numobspersat(i) + 1
   oberrvarmean(i) = oberrvarmean(i) + oberrvar(nob2)
enddo
!if (nproc == 0) then
!   print *,'numobspersat:',minval(numobspersat),maxval(numobspersat)
!   print *,'oberrvarmean:',minval(oberrvarmean),maxval(oberrvarmean)
!end if
! average ob error for each channel.
do i=1,jpch_rad
   if (numobspersat(i) > 0) then
      oberrvarmean(i) = oberrvarmean(i)/real(numobspersat(i),r_single)
   else
      oberrvarmean(i) = 9.9e31_r_single
   end if
enddo

end subroutine channelstats

subroutine boxsort(ns,ne,satflag,box)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    boxsort
!
!   prgmmr: ota
!
! abstract:  sorting observations into the observation boxes. observations
!            will be sorted along latitudinal direction first, and
!            longitudinal direction next.
!
! program history log:
!   2011-06-20  ota
!
!   input argument list:
!     ns, ne    - range of input observation indices
!     satflag   - true when input observations are satellite radiances
!
!   output argument list:
!     box       - number of observations included in this and the previous
!                 observation boxes
!
! attributes:
!   language:  f95
!   machine:
!
!$$$ end documentation block
  use radinfo,only: npred
  implicit none
  integer(i_kind),intent(in) :: ns
  integer(i_kind),intent(in) :: ne
  logical,intent(in) :: satflag
  integer(i_kind),intent(out) :: box(blatnum+1,blonnum+1)
  real(r_single),dimension(nanals,ne-ns+1) :: tmpanal_ob
  real(r_single),dimension(ne-ns+1) :: tmpsprd, tmpmean, tmpmean_nobc, tmpob
  real(r_single),dimension(ne-ns+1) :: tmplon, tmplat, tmpstattype, tmppres
  real(r_single),dimension(ne-ns+1) :: tmptime, tmperr, tmperr_orig, tmpfit
  real(r_single),allocatable :: tmpbpred(:,:)
  integer(i_kind),allocatable :: tmpindxsat(:)
  character(len=20) :: tmptype(ne-ns+1)
  integer(i_kind) :: njs(blatnum+1)
  integer(i_kind) :: nj(blatnum+1)
  integer(i_kind) :: i,j,nob,nn,nn0
  ! Count the number of observations in each latitude band
  nj(1:blatnum+1)=0
  do nob=ns,ne
     if(obloclat(nob) >= boxlat(1)) cycle
     nj(1) = nj(1)+1
  end do
  do j=2,blatnum
     do nob=ns,ne
        if(obloclat(nob) >= boxlat(j) .or. obloclat(nob) < boxlat(j-1)) cycle
        nj(j) = nj(j)+1
     end do
  end do
  do nob=ns,ne
     if(obloclat(nob) < boxlat(blatnum)) cycle
     nj(blatnum+1) = nj(blatnum+1)+1
  end do
  njs(1)=0
  do j=2,blatnum+1
     njs(j) = njs(j-1)+nj(j-1)
  end do
  ! Allocate variables for satellite radiance observations
  if(satflag) then
     allocate(tmpbpred(npred,ne-ns+1))
     allocate(tmpindxsat(ne-ns+1))
  end if
  ! Sorting observations with latitude bands
  nn=0
  do nob=ns,ne
     if(obloclat(nob) >= boxlat(1)) cycle
     nn=nn+1
     tmpfit(nn)      =obfit_prior(nob)
     tmpsprd(nn)     =obsprd_prior(nob)
     tmpmean_nobc(nn)=ensmean_obnobc(nob)
     tmpmean(nn)     =ensmean_ob(nob)
     tmpob(nn)       =ob(nob)
     tmperr(nn)      =oberrvar(nob)
     tmplon(nn)      =obloclon(nob)
     tmplat(nn)      =obloclat(nob)
     tmppres(nn)     =obpress(nob)
     tmptime(nn)     =obtime(nob)
     tmperr_orig(nn) =oberrvar_orig(nob)
     tmpstattype(nn) =stattype(nob)
     tmptype(nn)     =obtype(nob)
     tmpanal_ob(:,nn)=anal_ob(:,nob)
     if(satflag) then
        tmpbpred(:,nn)=biaspreds(:,nob-ns+1)
        tmpindxsat(nn)=indxsat(nob-ns+1)
     end if
  end do
  do j=2,blatnum
     nn=njs(j)
     do nob=ns,ne
        if(obloclat(nob) >= boxlat(j) .or. obloclat(nob) < boxlat(j-1)) cycle
        nn=nn+1
        tmpfit(nn)      =obfit_prior(nob)
        tmpsprd(nn)     =obsprd_prior(nob)
        tmpmean_nobc(nn)=ensmean_obnobc(nob)
        tmpmean(nn)     =ensmean_ob(nob)
        tmpob(nn)       =ob(nob)
        tmperr(nn)      =oberrvar(nob)
        tmplon(nn)      =obloclon(nob)
        tmplat(nn)      =obloclat(nob)
        tmppres(nn)     =obpress(nob)
        tmptime(nn)     =obtime(nob)
        tmperr_orig(nn) =oberrvar_orig(nob)
        tmpstattype(nn) =stattype(nob)
        tmptype(nn)     =obtype(nob)
        tmpanal_ob(:,nn)=anal_ob(:,nob)
        if(satflag) then
           tmpbpred(:,nn)=biaspreds(:,nob-ns+1)
           tmpindxsat(nn)=indxsat(nob-ns+1)
        end if
     end do
  end do
  nn=njs(blatnum+1)
  do nob=ns,ne
     if(obloclat(nob) < boxlat(blatnum)) cycle
     nn=nn+1
     tmpfit(nn)      =obfit_prior(nob)
     tmpsprd(nn)     =obsprd_prior(nob)
     tmpmean_nobc(nn)=ensmean_obnobc(nob)
     tmpmean(nn)     =ensmean_ob(nob)
     tmpob(nn)       =ob(nob)
     tmperr(nn)      =oberrvar(nob)
     tmplon(nn)      =obloclon(nob)
     tmplat(nn)      =obloclat(nob)
     tmppres(nn)     =obpress(nob)
     tmptime(nn)     =obtime(nob)
     tmperr_orig(nn) =oberrvar_orig(nob)
     tmpstattype(nn) =stattype(nob)
     tmptype(nn)     =obtype(nob)
     tmpanal_ob(:,nn)=anal_ob(:,nob)
     if(satflag) then
        tmpbpred(:,nn)=biaspreds(:,nob-ns+1)
        tmpindxsat(nn)=indxsat(nob-ns+1)
     end if
  end do
  ! Sorting observations with each observation box
  do j=1,blatnum+1
     if(nj(j) == 0) then
        box(j,:)=ns-1+njs(j)
        cycle
     end if
     nn=ns-1+njs(j)
     nn0=njs(j)
     do nob=njs(j)+1,njs(j)+nj(j)
        if(tmplon(nob) >= boxlon(1)) cycle
        nn=nn+1
        obfit_prior(nn)   =tmpfit(nob)
        obsprd_prior(nn)  =tmpsprd(nob)
        ensmean_obnobc(nn)=tmpmean_nobc(nob)
        ensmean_ob(nn)    =tmpmean(nob)
        ob(nn)            =tmpob(nob)
        oberrvar(nn)      =tmperr(nob)
        obloclon(nn)      =tmplon(nob)
        obloclat(nn)      =tmplat(nob)
        obpress(nn)       =tmppres(nob)
        obtime(nn)        =tmptime(nob)
        oberrvar_orig(nn) =tmperr_orig(nob)
        stattype(nn)      =tmpstattype(nob)
        obtype(nn)        =tmptype(nob)
        anal_ob(:,nn)     =tmpanal_ob(:,nob)
        if(satflag) then
           nn0=nn0+1
           biaspreds(:,nn0)=tmpbpred(:,nob)
           indxsat(nn0)    =tmpindxsat(nob)
        end if
     end do
     box(j,1)=nn
     boxmax=max(boxmax,box(j,1)-ns-njs(j)+1)
     do i=2,blonnum
        do nob=njs(j)+1,njs(j)+nj(j)
           if(tmplon(nob) >= boxlon(i) .or. tmplon(nob) < boxlon(i-1)) cycle
           nn=nn+1
           obfit_prior(nn)   =tmpfit(nob)
           obsprd_prior(nn)  =tmpsprd(nob)
           ensmean_obnobc(nn)=tmpmean_nobc(nob)
           ensmean_ob(nn)    =tmpmean(nob)
           ob(nn)            =tmpob(nob)
           oberrvar(nn)      =tmperr(nob)
           obloclon(nn)      =tmplon(nob)
           obloclat(nn)      =tmplat(nob)
           obpress(nn)       =tmppres(nob)
           obtime(nn)        =tmptime(nob)
           oberrvar_orig(nn) =tmperr_orig(nob)
           stattype(nn)      =tmpstattype(nob)
           obtype(nn)        =tmptype(nob)
           anal_ob(:,nn)     =tmpanal_ob(:,nob)
           if(satflag) then
              nn0=nn0+1
              biaspreds(:,nn0)=tmpbpred(:,nob)
              indxsat(nn0)    =tmpindxsat(nob)
           end if
        end do
        box(j,i)=nn
        boxmax=max(boxmax,box(j,i)-box(j,i-1))
     end do
     do nob=njs(j)+1,njs(j)+nj(j)
        if(tmplon(nob) < boxlon(blonnum)) cycle
        nn=nn+1
        obfit_prior(nn)   =tmpfit(nob)
        obsprd_prior(nn)  =tmpsprd(nob)
        ensmean_obnobc(nn)=tmpmean_nobc(nob)
        ensmean_ob(nn)    =tmpmean(nob)
        ob(nn)            =tmpob(nob)
        oberrvar(nn)      =tmperr(nob)
        obloclon(nn)      =tmplon(nob)
        obloclat(nn)      =tmplat(nob)
        obpress(nn)       =tmppres(nob)
        obtime(nn)        =tmptime(nob)
        oberrvar_orig(nn) =tmperr_orig(nob)
        stattype(nn)      =tmpstattype(nob)
        obtype(nn)        =tmptype(nob)
        anal_ob(:,nn)     =tmpanal_ob(:,nob)
        if(satflag) then
           nn0=nn0+1
           biaspreds(:,nn0)=tmpbpred(:,nob)
           indxsat(nn0)    =tmpindxsat(nob)
        end if
     end do
     box(j,blonnum+1)=nn
     boxmax=max(boxmax,box(j,blonnum+1)-box(j,blonnum))
     ! Check consistency of the sort
     if(nn /= ns-1+njs(j)+nj(j)) then
        if(nproc == 0) print *,'Error in sort process: ',nn,ns-1+njs(j)+nj(j)
        call stop2(999)
     end if
  end do

  ! Deallocate variables for satellite radiance observations
  if(satflag) deallocate(tmpbpred,tmpindxsat)
  return
end subroutine boxsort

subroutine obsmod_cleanup()
! deallocate module-level allocatable arrays
if (allocated(obsprd_prior)) deallocate(obsprd_prior)
if (allocated(obfit_prior)) deallocate(obfit_prior)
if (allocated(obsprd_post)) deallocate(obsprd_post)
if (allocated(obfit_post)) deallocate(obfit_post)
if (allocated(ensmean_ob)) deallocate(ensmean_ob)
if (allocated(ensmean_obnobc)) deallocate(ensmean_obnobc)
if (allocated(ob)) deallocate(ob)
if (allocated(oberrvar)) deallocate(oberrvar)
if (allocated(oberrvar_orig)) deallocate(oberrvar_orig)
if (allocated(oberrvarmean)) deallocate(oberrvarmean)
if (allocated(obloclon)) deallocate(obloclon)
if (allocated(obloclat)) deallocate(obloclat)
if (allocated(obpress)) deallocate(obpress)
if (allocated(obtime)) deallocate(obtime)
if (allocated(oblnp)) deallocate(oblnp)
if (allocated(numobspersat)) deallocate(numobspersat)
if (allocated(obloc)) deallocate(obloc)
if (allocated(biaspreds)) deallocate(biaspreds)
if (allocated(deltapredx)) deallocate(deltapredx)
if (allocated(indxsat)) deallocate(indxsat)
if (allocated(obtype)) deallocate(obtype)
if (allocated(probgrosserr)) deallocate(probgrosserr)
if (allocated(prpgerr)) deallocate(prpgerr)
if (allocated(biasprednorm)) deallocate(biasprednorm)
if (allocated(biasprednorminv)) deallocate(biasprednorminv)
if (allocated(anal_ob)) deallocate(anal_ob)
if (allocated(boxlat)) deallocate(boxlat)
if (allocated(boxlon)) deallocate(boxlon)
if (allocated(nlocconv)) deallocate(nlocconv)
if (allocated(nlocsat)) deallocate(nlocsat)
if (allocated(nlocoz)) deallocate(nlocoz)
end subroutine obsmod_cleanup


end module enkf_obsmod
