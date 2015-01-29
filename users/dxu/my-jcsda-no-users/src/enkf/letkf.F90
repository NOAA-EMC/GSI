module letkf
!$$$  module documentation block
!
! module: letkf                        Update model state variables and
!                                      bias coefficients with the LETKF.
!
! prgmmr: ota              org: np23                   date: 2011-06-01
!
! abstract: Updates the model state using the LETKF (Hunt et al 2007,
!  Physica D, 112-126).
!
!  After the observation variables are updated, the bias coefficients update is done
!  using update_biascorr from module radbias.  This update is done via a
!  matrix inversion using all the observations at once, and a static (diagonal)
!  background error covariance matrix.  If the namelist parameter numiter is >
!  1, this process is repeated numiter times, with each observation variable update using
!  the latest estimate of the bias correction coefficients and each bias
!  coefficient update using the latest estimate of the observation increment
!  (observation minus ensemble mean observation variable).  The model state
!  variables are only updated after the last iteration.  After the update is
!  complete, the variables anal_chunk and ensmean_chunk (from module statevec)
!  contain the updated model state ensemble perturbations and ensemble mean,
!  and predx (from module radinfo) contains the updated bias coefficients.
!  obfit_post and obsprd_post contain the observation increments and observation
!  variable variance.
!
!  Covariance localization is used in the state update to limit the impact 
!  of observations to a specified distance from the observation in the
!  horizontal and vertical.  These distances can be set separately in the
!  NH, tropics and SH, and in the horizontal, vertical and time dimensions,
!  using the namelist parameters  corrlengthnh, corrlengthtr, corrlengthsh,
!  lnsigcutoffnh, lnsigcutofftr, lnsigcutoffsh (lnsigcutoffsatnh,
!  lnsigcutoffsattr, lnsigcutoffsatsh for satellite obs, similar for ps obs)
!  obtimelnh, obtimeltr, obtimelsh. The length scales should be given in km for the
!  horizontal, hours for time, and 'scale heights' (units of -log(p/pref)) in the
!  vertical. The function used for localization (function taper)
!  is imported from module covlocal. Localization requires that
!  every observation have an associated horizontal, vertical and temporal location.
!  For satellite radiance observations the vertical location is given by
!  the maximum in the weighting function associated with that sensor/channel/
!  background state (this computation, along with the rest of the forward
!  operator calcuation, is performed by a separate program using the GSI
!  forward operator code).  Although all the observation variable ensemble
!  members sometimes cannot fit in memory, they are necessary before LETKF core
!  process. So they are saved in all processors.
!
!  Adaptive observation thinning implemented in the serial EnSRF is not 
!  implemented here in the current version.
!
! Public Subroutines:
!  letkf_update: performs the LETKF update (calls update_biascorr to perform
!   the bias coefficient update.  The EnKF/bias coefficient update is 
!   iterated numiter times (parameter numiter from module params).
!
! Public Variables: None
!
! Modules Used: kinds, constants, params, covlocal, mpisetup, loadbal, statevec,
!               enkf_obsmod, radinfo, radbias, gridinfo, common_mtx
!
! program history log:
!   2011-06-01  Created from Whitaker's serial EnSRF core module.
!
! attributes:
!   language: f95
!
!$$$

use mpisetup
use covlocal, only:  taper, latval
use kinds, only: r_double,i_kind,r_kind,r_single
use loadbal, only: numobsperproc, numptsperproc, indxproc_obs, iprocob, &
                   indxproc, lnp_chunk, &
                   ensmean_obchunk, indxob_chunk, oblnp_chunk, nobs_max, &
                   obtime_chunk, grdloc_chunk, obloc_chunk, &
                   anal_obchunk_prior
use statevec, only: ensmean_chunk, anal_chunk
use enkf_obsmod, only: oberrvar, ob, ensmean_ob, obloc, oblnp, &
                  nobsgood, nobs_conv, nobs_oz, nobs_sat,&
                  obfit_prior, obfit_post, obsprd_prior, obsprd_post, obtime,&
                  numobspersat, deltapredx, biaspreds,&
                  biasprednorm, probgrosserr, prpgerr,&
                  corrlengthsq,lnsigl,obtimel,anal_ob,obloclat, obloclon, &
                  boxlat, boxlon, nlocconv, nlocsat, nlocoz, blatnum, blonnum
use constants, only: pi, one, zero, rad2deg, deg2rad
use params, only: sprd_tol, ndim, datapath, nanals,&
                  iassim_order,sortinc,deterministic,numiter,nlevs,nvars,&
                  zhuberleft,zhuberright,varqc,lupd_satbiasc,huber,&
                  corrlengthnh,corrlengthtr,corrlengthsh
use radinfo, only: npred,nusis,nuchan,jpch_rad,predx
use radbias, only: apply_biascorr, update_biascorr
use gridinfo, only: nlevs_pres,index_pres,lonsgrd,latsgrd
use common_mtx, only: mtx_eigen

implicit none

private
public :: letkf_update, obs_local

contains

subroutine letkf_update()
implicit none
! LETKF update.

! local variables.
integer(i_kind) nob,nob1,nob2,nob3,nob4,nf,nobxx,nskip,&
                niter,i,nrej,npt,np
real(r_double) :: t1,t2,t3,t4,t5,t6,tbegin,tend
real(r_kind) r_nanals,r_nanalsm1
real(r_kind) normdepart, pnge, width
real(r_single),allocatable, dimension(:,:) :: anal_obchunk, buffertmp3
real(r_kind),dimension(nobsgood):: oberrvaruse
real(r_single), allocatable, dimension(:) :: buffertmp, buffertmp2
integer(i_kind) ierr
integer(i_kind) nanal,nn,nobm,nsame
logical lastiter
! For LETKF core processes
real(r_kind),allocatable,dimension(:,:) :: hdxf
real(r_kind),allocatable,dimension(:) :: rdiag,dep,rloc,obdep,oberinv
real(r_kind),dimension(nanals,nanals) :: trans
real(r_kind),dimension(nanals) :: work,work2
integer(i_kind),allocatable,dimension(:) :: nobs_use
integer(i_kind),allocatable,dimension(:) :: oupdate, oindex
real(r_kind),dimension(nobsgood) :: invcorlen, invlnsigl, invobtimel, hdist0
real(r_kind) :: hdist, vdist, tdist, pi2, minlat, maxlat, minlon, maxlon
real(r_kind) :: radlat, radlon, latband, lonband, corrlength
real(r_single) :: deglat, dist
integer(i_kind) :: nobsl, ngrd1, nobsl2, imin, imax, jmin, jmax
logical :: firstobs

! allocate temporary array.
allocate(anal_obchunk(nanals,nobs_max))
allocate(obdep(nobsgood))
allocate(oberinv(nobsgood))

! define a few frequently used parameters
r_nanals=one/float(nanals)
r_nanalsm1=one/float(nanals-1)

do niter=1,numiter

  lastiter = niter == numiter
  ! apply bias correction with latest estimate of bias coeffs.
  ! (already done for first iteration)
  if (nobs_sat > 0 .and. niter > 1 ) call apply_biascorr()

  ! reset first guess perturbations at start of each iteration.
  nrej=0
  nsame=0
  anal_obchunk = anal_obchunk_prior
  ! ensmean_ob is updated with latest bias coefficient perturbations.
  ! nob1 is the index of the obs to be processed on this rank
  ! nob2 maps nob1 to 1:nobsgood array (nob)
  do nob1=1,numobsperproc(nproc+1)
     nob2 = indxproc_obs(nproc+1,nob1)
     ensmean_obchunk(nob1) = ensmean_ob(nob2)
  enddo
! reset ob error to account for gross errors 
  if (niter > 1 .and. varqc) then
    if (huber) then ! "huber norm" QC
      do nob=1,nobsgood
        normdepart = obfit_post(nob)/sqrt(oberrvar(nob))
        ! depends of 2 parameters: zhuberright, zhuberleft.
        if (normdepart < -zhuberleft) then
           pnge = zhuberleft/abs(normdepart)
        else if (normdepart > zhuberright) then
           pnge = zhuberright/abs(normdepart)
        else
           pnge = one
        end if
        ! eqn 17 in Dharssi, Lorenc and Inglesby
        ! divide ob error by prob of gross error not occurring.
        oberrvaruse(nob) = oberrvar(nob)/pnge
        ! pnge is the prob that the ob *does not* contain a gross error.
        ! assume rejected if prob of gross err > 50%.
        probgrosserr(nob) = one-pnge
        if (probgrosserr(nob) > 0.5_r_single) then 
           nrej=nrej+1
        endif
      end do
    else ! "flat-tail" QC.
      do nob=1,nobsgood
        ! original form, gross error cutoff a multiple of ob error st dev.
        ! here gross err cutoff proportional to ensemble spread plus ob error
        ! Dharssi, Lorenc and Inglesby eqn (1) a = grosserrw*sqrt(S+R) 
        width = sprd_tol*sqrt(obsprd_prior(nob)+oberrvar(nob))
        pnge = prpgerr(nob)*sqrt(2.*pi*oberrvar(nob))/((one-prpgerr(nob))*(2.*width))
        normdepart = obfit_post(nob)/sqrt(oberrvar(nob))
        pnge = one - (pnge/(pnge+exp(-normdepart**2/2._r_single)))
        ! eqn 17 in Dharssi, Lorenc and Inglesby
        ! divide ob error by prob of gross error not occurring.
        oberrvaruse(nob) = oberrvar(nob)/pnge
        ! pnge is the prob that the ob *does not* contain a gross error.
        ! assume rejected if prob of gross err > 50%.
        probgrosserr(nob) = one-pnge
        if (probgrosserr(nob) > 0.5_r_single) then 
           nrej=nrej+1
        endif
      end do
    endif
  else
     oberrvaruse(1:nobsgood) = oberrvar(1:nobsgood)
  end if

  nobm = 1
  nskip = 0
  tbegin = mpi_wtime()

  ! Compute the inverse of cut-off length and 
  ! the observation departure from first guess
!$omp parallel private(nob)
!$omp do
  do nob=1,nobsgood
     invcorlen(nob)=one/corrlengthsq(nob)
     invlnsigl(nob)=one/lnsigl(nob)
     invobtimel(nob)=one/obtimel(nob)
     oberinv(nob)=one/oberrvaruse(nob)
     obdep(nob)=ob(nob)-ensmean_ob(nob)
  end do
!$omp end do
!$omp end parallel

  allocate(oupdate(numobsperproc(nproc+1)))
  oupdate(1:numobsperproc(nproc+1)) = 0
  pi2 = 2._r_kind * pi
  t2 = zero
  t3 = zero
  t4 = zero
  t5 = zero
!$omp parallel private(nobs_use,nob1,nob2,nob3,nob4,nobsl,nobsl2,radlat,radlon, &
!$omp                  latband,lonband,maxlat,minlat,maxlon,minlon, &
!$omp                  imin,imax,jmin,jmax,hdxf,rdiag,dep,rloc,hdist,vdist, &
!$omp                  tdist,nf,nob,work,trans,firstobs,t1)
  allocate(nobs_use(nobsgood))
  firstobs=.true.
  ! Loop for each observations
!$omp do schedule(dynamic)
  obsloop: do nob1=1,numobsperproc(nproc+1)

     t1 = mpi_wtime()

     nob2=indxproc_obs(nproc+1,nob1)
     if(firstobs) then
        nob3=nob2
        nob4=nob1
     else
        nob4=nob1-1
     end if

     ! If the observation location and time are same as the previous
     ! observation, skip the following steps.
     if(.not. firstobs .and. oblnp_chunk(nob1) == oblnp_chunk(nob4) .and. &
          obtime_chunk(nob1) == obtime_chunk(nob4) .and. &
          sum(abs(obloc_chunk(1:3,nob1)-obloc_chunk(1:3,nob4))) == zero) cycle obsloop

     ! Pick up observation boxes to use in this point
     ! This process is necessary only when the horizontal location is
     ! different from the previous one.
     if(firstobs .or. obloclat(nob2) /= obloclat(nob3) .or. &
          & obloclon(nob2) /= obloclon(nob3)) then
        firstobs=.false.
        nobsl=0
        radlat=obloclat(nob2)*deg2rad
        radlon=obloclon(nob2)*deg2rad
        latband=2._r_kind*asin(0.5_r_kind*sqrt(corrlengthsq(nob2)))
        maxlat=radlat+latband
        minlat=radlat-latband
        lonband=latband/max(min(cos(maxlat),cos(minlat)),1.e-10_r_kind)
        maxlon=radlon+lonband
        minlon=radlon-lonband
        do jmin=1,blatnum
           if(minlat < boxlat(jmin)) exit
        end do
        do jmax=2,blatnum
           if(maxlat < boxlat(jmax)) exit
        end do
        if(maxlat >= boxlat(blatnum)) jmax=blatnum+1
        do imin=1,blonnum
           if(minlon < boxlon(imin)) exit
        end do
        do imax=2,blonnum
           if(maxlon < boxlon(imax)) exit
        end do
        if(maxlon >= boxlon(blonnum)) imax=blonnum+1
        if(lonband >= pi2) then
           imin=1
           imax=blonnum+1
           call obs_local(imin,imax,jmin,jmax,nobsl,nobs_use)
        else if(minlon < zero .and. imin==1) then
           imin=imax
           minlon=minlon+pi2
           do imax=2,blonnum
              if(minlon < boxlon(imax)) exit
           end do
           if(minlon >= boxlon(blonnum)) imax=blonnum+1
           imax=max(imin+1,imax)
           call obs_local(1,imin,jmin,jmax,nobsl,nobs_use)
           call obs_local(imax,blonnum+1,jmin,jmax,nobsl,nobs_use)
        else if(maxlon > pi2 .and. imax==blonnum+1) then
           imax=imin
           maxlon=maxlon-pi2
           do imin=1,blonnum
              if(maxlon < boxlon(imin)) exit
           end do
           imin=min(imax-1,imin)
           call obs_local(1,imin,jmin,jmax,nobsl,nobs_use)
           call obs_local(imax,blonnum+1,jmin,jmax,nobsl,nobs_use)
        else
           call obs_local(imin,imax,jmin,jmax,nobsl,nobs_use)
        end if
        nob3=nob2
     end if

     t2 = t2 + mpi_wtime() - t1
     t1 = mpi_wtime()
     
     ! Skip when no observation is available
     if(nobsl == 0) then
        oupdate(nob1)=1
        nskip=nskip+1
        cycle obsloop
     end if

     ! Pick up variables passed to LETKF core process
     allocate(hdxf(nobsl,nanals))
     allocate(rdiag(nobsl))
     allocate(dep(nobsl))
     allocate(rloc(nobsl))
     nobsl2=1
     do nob=1,nobsl
        nf=nobs_use(nob)
        vdist=oblnp_chunk(nob1)-oblnp(nf)
        if(abs(vdist) >= lnsigl(nf)) cycle
        hdist=(obloc_chunk(1,nob1)-obloc(1,nf))*(obloc_chunk(1,nob1)-obloc(1,nf)) + &
             (obloc_chunk(2,nob1)-obloc(2,nf))*(obloc_chunk(2,nob1)-obloc(2,nf)) + &
             (obloc_chunk(3,nob1)-obloc(3,nf))*(obloc_chunk(3,nob1)-obloc(3,nf))
        if(hdist >= corrlengthsq(nf)) cycle
        tdist=obtime_chunk(nob1)-obtime(nf)
        if(abs(tdist) >= obtimel(nf)) cycle
        vdist=vdist*invlnsigl(nf)
        hdist=hdist*invcorlen(nf)
        tdist=tdist*invobtimel(nf)
        hdxf(nobsl2,1:nanals)=anal_ob(1:nanals,nf)  ! WE NEED anal_ob (global)
        rdiag(nobsl2)=oberinv(nf)
        dep(nobsl2)=obdep(nf)
        dist = sqrt(hdist+vdist*vdist+tdist*tdist)
        rloc(nobsl2)=taper(dist)
        if(rloc(nobsl2) /= zero) then
           nobsl2=nobsl2+1
        end if
     end do

     nobsl2=nobsl2-1

     t3 = t3 + mpi_wtime() - t1
     t1 = mpi_wtime()

     if(nobsl2 == 0) then
        deallocate(hdxf,rdiag,dep,rloc)
        oupdate(nob1) = 1
        cycle obsloop
     end if

     ! Skip the already updated observation priors
     if(oupdate(nob1) == 1) then
        deallocate(hdxf,rdiag,dep,rloc)
        cycle obsloop
     end if

     ! Compute transformation matrix of LETKF
     call letkf_core(nobsl2,hdxf(1:nobsl2,1:nanals),rdiag(1:nobsl2), &
          dep(1:nobsl2),rloc(1:nobsl2),trans)
     deallocate(hdxf,rdiag,dep,rloc)

     t4 = t4 + mpi_wtime() - t1
     t1 = mpi_wtime()

     ! Update observation posterior ensembles
     do nob=nob1,numobsperproc(nproc+1)
        if(oblnp_chunk(nob1) /= oblnp_chunk(nob)) exit
        if(obtime_chunk(nob1) /= obtime_chunk(nob)) exit
        if(sum(abs(obloc_chunk(1:3,nob1)-obloc_chunk(1:3,nob))) > tiny(obloc_chunk(1,nob1))) exit
        if(oupdate(nob) == 1) cycle
        oupdate(nob) = 1
        work(1:nanals) = anal_obchunk(1:nanals,nob)
        work2(1:nanals) = ensmean_obchunk(nob)
        if(r_kind == kind(1.d0)) then
           call dgemv('t',nanals,nanals,1.d0,trans,nanals,work,1,1.d0,work2,1)
        else
           call sgemv('t',nanals,nanals,1.e0,trans,nanals,work,1,1.e0,work2,1)
        end if
        ensmean_obchunk(nob) = sum(work2(1:nanals)) * r_nanals
        anal_obchunk(1:nanals,nob) = work2(1:nanals)-ensmean_obchunk(nob)
     end do

     t5 = t5 + mpi_wtime() - t1

  end do obsloop
!$omp end do
  deallocate(nobs_use)
!$omp end parallel

  deallocate(oupdate)
  t1 = mpi_wtime()
  if (nproc == 0 .and. nskip > 0) print *,nskip,' out of',numobsperproc(nproc+1),'obs skipped'
  if (nproc == 0 .and. nrej >  0) print *,nrej,' obs rejected by varqc'
  if (nproc == 0 .or. nproc == numproc-1) write(6,8003) niter,'timing on proc',nproc,' = ',t1-tbegin,t2,t3,t4,t5,nrej
  8003  format(i2,1x,a14,1x,i5,1x,a3,5(f8.2,1x),i4)

  allocate(buffertmp(nobsgood))
  allocate(buffertmp2(nobsgood))
! distribute the O-A stats to all processors.
  buffertmp=zero
  do nob1=1,numobsperproc(nproc+1)
    nob2=indxproc_obs(nproc+1,nob1)
    buffertmp(nob2) = ensmean_obchunk(nob1)
  end do
  call mpi_allreduce(buffertmp,buffertmp2,nobsgood,mpi_real4,mpi_sum,mpi_comm_world,ierr)
  obfit_post = obfit_prior
  do nobxx=1,nobsgood
      obfit_post(nobxx) = ob(nobxx)-buffertmp2(nobxx)
  end do
  deallocate(buffertmp, buffertmp2)
  t2 = mpi_wtime()
  if (nproc == 0) print *,'time to broadcast obsprd_post and obfit_post = ',t2-t1,' secs, niter =',niter

  ! satellite bias correction update.
  if (nobs_sat > 0 .and. lupd_satbiasc) call update_biascorr(niter)

end do ! niter loop

allocate(buffertmp(nobsgood))
allocate(buffertmp2(nobsgood))
buffertmp=zero
do nob1=1,numobsperproc(nproc+1)
   nob2=indxproc_obs(nproc+1,nob1)
   buffertmp(nob2) = sum(anal_obchunk(1:nanals,nob1)**2)*r_nanalsm1
end do
call mpi_allreduce(buffertmp,buffertmp2,nobsgood,mpi_real4,mpi_sum,mpi_comm_world,ierr)
obsprd_post = 9.9e31
obsprd_post(1:nobsgood) = buffertmp2(1:nobsgood)

predx = predx + deltapredx ! add increment to bias coeffs.
t3 = mpi_wtime()
if (nproc == 0) print *,'time to gather obsprd_post = ',t3-t2,' secs'
deallocate(buffertmp,buffertmp2)

! free temporary arrays.
deallocate(anal_obchunk_prior)

t2 = zero
t3 = zero
t4 = zero
t5 = zero
t6 = mpi_wtime()
! Next, update analysis ensemble on each grid points
! Loop for each horizontal grid points
!$omp parallel private(nobs_use,npt,nob,nobsl,nobsl2,ngrd1,corrlength, &
!$omp                  latband,lonband,maxlat,minlat,maxlon,minlon, &
!$omp                  imin,imax,jmin,jmax,nf,hdist0,vdist,tdist, &
!$omp                  nn,hdxf,rdiag,dep,rloc,i,work,trans,t1, &
!$omp                  oindex,nanal)
allocate(nobs_use(nobsgood))
!$omp do schedule(dynamic)
grdloop: do npt=1,numptsperproc(nproc+1)

   t1 = mpi_wtime()

   ! Pick up observation boxes to use in this grid point
   ! This process is necessary only when the horizontal location is
   ! different from the previous one.
   nobsl=0
   ngrd1=indxproc(nproc+1,npt)
   deglat = latsgrd(ngrd1)*rad2deg
   corrlength=latval(deglat,corrlengthnh,corrlengthtr,corrlengthsh)
   latband=2._r_kind*asin(0.5_r_kind*corrlength)
   maxlat=latsgrd(ngrd1)+latband
   minlat=latsgrd(ngrd1)-latband
   lonband=latband/max(min(cos(maxlat),cos(minlat)),1.e-10_r_kind)
   maxlon=lonsgrd(ngrd1)+lonband
   minlon=lonsgrd(ngrd1)-lonband
   do jmin=1,blatnum
      if(minlat < boxlat(jmin)) exit
   end do
   do jmax=2,blatnum
      if(maxlat < boxlat(jmax)) exit
   end do
   if(maxlat >= boxlat(blatnum)) jmax=blatnum+1
   do imin=1,blonnum
      if(minlon < boxlon(imin)) exit
   end do
   do imax=2,blonnum
      if(maxlon < boxlon(imax)) exit
   end do
   if(maxlon >= boxlon(blonnum)) imax=blonnum+1
   if(lonband >= pi2) then
      imin=1
      imax=blonnum+1
      call obs_local(imin,imax,jmin,jmax,nobsl,nobs_use)
   else if(minlon < zero .and. imin==1) then
      imin=imax
      minlon=minlon+pi2
      do imax=2,blonnum
         if(minlon < boxlon(imax)) exit
      end do
      if(minlon >= boxlon(blonnum)) imax=blonnum+1
      imax=max(imin+1,imax)
      call obs_local(1,imin,jmin,jmax,nobsl,nobs_use)
      call obs_local(imax,blonnum+1,jmin,jmax,nobsl,nobs_use)
   else if(maxlon > pi2 .and. imax==blonnum+1) then
      imax=imin
      maxlon=maxlon-pi2
      do imin=1,blonnum
         if(maxlon < boxlon(imin)) exit
      end do
      imin=min(imax-1,imin)
      call obs_local(1,imin,jmin,jmax,nobsl,nobs_use)
      call obs_local(imax,blonnum+1,jmin,jmax,nobsl,nobs_use)
   else
      call obs_local(imin,imax,jmin,jmax,nobsl,nobs_use)
   end if
   
   ! Calculate horizontal distances
   do nob=1,nobsl
      nf=nobs_use(nob)
      hdist0(nob)=((grdloc_chunk(1,npt)-obloc(1,nf))*(grdloc_chunk(1,npt)-obloc(1,nf)) + &
           (grdloc_chunk(2,npt)-obloc(2,nf))*(grdloc_chunk(2,npt)-obloc(2,nf)) + &
           (grdloc_chunk(3,npt)-obloc(3,nf))*(grdloc_chunk(3,npt)-obloc(3,nf))) &
           * invcorlen(nf)
   end do
   
   t2 = t2 + mpi_wtime() - t1
   t1 = mpi_wtime()

   ! Skip when no observation is available
   if(nobsl == 0) cycle grdloop

   ! Loop for each vertical layers
   verloop: do nn=1,nlevs_pres

      ! Pick up variables passed to LETKF core process
      allocate(hdxf(nobsl,nanals))
      allocate(rdiag(nobsl))
      allocate(dep(nobsl))
      allocate(rloc(nobsl))
      allocate(oindex(nobsl))
      nobsl2=1
      do nob=1,nobsl
         nf=nobs_use(nob)
         vdist=lnp_chunk(npt,nn)-oblnp(nf)
         if(abs(vdist) >= lnsigl(nf)) cycle
         if(hdist0(nob) >= one) cycle
         if(abs(obtime(nf)) >= obtimel(nf)) cycle
         vdist=vdist*invlnsigl(nf)
         tdist=obtime(nf)*invobtimel(nf)
         hdxf(nobsl2,1:nanals)=anal_ob(1:nanals,nf) ! WE NEED anal_ob (global)
         rdiag(nobsl2)=oberinv(nf)
         dep(nobsl2)=obdep(nf)
         dist = sqrt(hdist0(nob)+vdist*vdist+tdist*tdist)
         rloc(nobsl2)=taper(dist)
         oindex(nobsl2)=nf
         if(rloc(nobsl2) > tiny(rloc(nobsl2))) then
            nobsl2=nobsl2+1
         end if
      end do
      nobsl2=nobsl2-1
      if(nobsl2 == 0) then
         deallocate(hdxf,rdiag,dep,rloc,oindex)
         cycle verloop
      end if

      t3 = t3 + mpi_wtime() - t1
      t1 = mpi_wtime()

      ! Compute transformation matrix of LETKF
      call letkf_core(nobsl2,hdxf(1:nobsl2,1:nanals),rdiag(1:nobsl2), &
           dep(1:nobsl2),rloc(1:nobsl2),trans)
      deallocate(hdxf,rdiag,dep,rloc,oindex)

      t4 = t4 + mpi_wtime() - t1
      t1 = mpi_wtime()

      ! Update analysis ensembles
      do i=1,ndim
         if(index_pres(i) /= nn) cycle
         work(1:nanals) = anal_chunk(1:nanals,npt,i)
         work2(1:nanals) = ensmean_chunk(npt,i)
         if(r_kind == kind(1.d0)) then
            call dgemv('t',nanals,nanals,1.d0,trans,nanals,work,1,1.d0, &
                 & work2,1)
         else
            call sgemv('t',nanals,nanals,1.e0,trans,nanals,work,1,1.e0, &
                 & work2,1)
         end if
         ensmean_chunk(npt,i) = sum(work2(1:nanals)) * r_nanals
         anal_chunk(1:nanals,npt,i) = work2(1:nanals)-ensmean_chunk(npt,i)
      end do

      t5 = t5 + mpi_wtime() - t1
      t1 = mpi_wtime()

   end do verloop
end do grdloop
!$omp end do
deallocate(nobs_use)
!$omp end parallel

tend = mpi_wtime()
if (nproc == 0 .or. nproc == numproc-1) print *,'time to process analysis on gridpoint = ',tend-t6,t2,t3,t4,t5,' secs'

deallocate(obdep,oberinv)

! Gathering analysis perturbations projected on the observation space
if(nproc /= 0) then
  deallocate(anal_ob)
  call mpi_send(anal_obchunk,numobsperproc(nproc+1)*nanals,mpi_real4,0, &
       1,mpi_comm_world,ierr)
else
   allocate(buffertmp3(nanals,nobs_max))
   do np=1,numproc-1
      call mpi_recv(buffertmp3,numobsperproc(np+1)*nanals,mpi_real4,np, &
           1,mpi_comm_world,mpi_status,ierr)
      do nob1=1,numobsperproc(np+1)
         nob2 = indxproc_obs(np+1,nob1)
         anal_ob(:,nob2) = buffertmp3(:,nob1)
      end do
   end do
   do nob1=1,numobsperproc(1)
      nob2 = indxproc_obs(1,nob1)
      anal_ob(:,nob2) = anal_obchunk(:,nob1)
   end do
   deallocate(buffertmp3)
end if

deallocate(anal_obchunk) ! this one is allocated in loadbal

return

end subroutine letkf_update

subroutine letkf_core(nobsl,hdxf,rdiaginv,dep,rloc,trans)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    letkf_core
!
!   prgmmr: ota
!
! abstract:  LETKF core subroutine computing transform matrix. BLAS subroutines
!            are used for the computational efficiency.
!
! program history log:
!   2011-06-03  ota: created from miyoshi's LETKF core subroutine
!
!   input argument list:
!     nobsl    - number of observations in the local patch
!     hdxf     - first-guess ensembles on observation space
!     rdiaginv - inverse of diagonal element of observation error covariance
!     dep      - observation departure from first guess mean
!     rloc     - localization function to each observations
!
!   output argument list:
!     trans    - transform matrix for this point
!
! attributes:
!   language:  f95
!   machine:
!
!$$$ end documentation block
implicit none
integer(i_kind)                      ,intent(in ) :: nobsl
real(r_kind),dimension(nobsl ,nanals),intent(in ) :: hdxf
real(r_kind),dimension(nobsl        ),intent(in ) :: rdiaginv
real(r_kind),dimension(nobsl        ),intent(in ) :: dep
real(r_kind),dimension(nobsl        ),intent(in ) :: rloc
real(r_kind),dimension(nanals,nanals),intent(out) :: trans
real(r_kind),dimension(nobsl ,nanals) :: hdxf_rinv
real(r_kind),dimension(nanals,nobsl ) :: work2
real(r_kind),dimension(nanals,nanals) :: eivec,pa,work1
real(r_kind),dimension(nanals) :: eival,work3
real(r_kind),dimension(nobsl) :: rrloc
real(r_kind) :: rho
integer(i_kind) :: i,j,nob,nanal
! hdxf Rinv
do nob=1,nobsl
  rrloc(nob) = rdiaginv(nob) * rloc(nob)
end do
do nanal=1,nanals
   do nob=1,nobsl
      hdxf_rinv(nob,nanal) = hdxf(nob,nanal) * rrloc(nob)
   end do
end do
! hdxf^T Rinv hdxf
!do j=1,nanals
!   do i=1,nanals
!      work1(i,j) = hdxf_rinv(1,i) * hdxf(1,j)
!      do nob=2,nobsl
!         work1(i,j) = work1(i,j) + hdxf_rinv(nob,i) * hdxf(nob,j)
!      end do
!   end do
!end do
if(r_kind == kind(1.d0)) then
   call dgemm('t','n',nanals,nanals,nobsl,1.d0,hdxf_rinv,nobsl, &
        hdxf(1:nobsl,1:nanals),nobsl,0.d0,work1,nanals)
else
   call sgemm('t','n',nanals,nanals,nobsl,1.e0,hdxf_rinv,nobsl, &
        hdxf(1:nobsl,1:nanals),nobsl,0.e0,work1,nanals)
end if
! hdxb^T Rinv hdxb + (m-1) I
do nanal=1,nanals
   work1(nanal,nanal) = work1(nanal,nanal) + real(nanals-1,r_kind)
end do
! eigenvalues and eigenvectors of [ hdxb^T Rinv hdxb + (m-1) I ]
call mtx_eigen(1,nanals,work1,eival,eivec,i)
! Pa = [ hdxb^T Rinv hdxb + (m-1) I ]inv
do j=1,nanals
   do i=1,nanals
      work1(i,j) = eivec(i,j) / eival(j)
   end do
end do
!do j=1,nanals
!   do i=1,nanals
!      pa(i,j) = work1(i,1) * eivec(j,1)
!      do k=2,nanals
!         pa(i,j) = pa(i,j) + work1(i,k) * eivec(j,k)
!      end do
!   end do
!end do
if(r_kind == kind(1.d0)) then
   call dgemm('n','t',nanals,nanals,nanals,1.d0,work1,nanals,eivec,&
        nanals,0.d0,pa,nanals)
else
   call sgemm('n','t',nanals,nanals,nanals,1.e0,work1,nanals,eivec,&
        nanals,0.e0,pa,nanals)
end if
! Pa hdxb_rinv^T
!do nob=1,nobsl
!   do nanal=1,nanals
!      work2(nanal,nob) = pa(nanal,1) * hdxf_rinv(nob,1)
!      do k=2,nanals
!         work2(nanal,nob) = work2(nanal,nob) + pa(nanal,k) * hdxf_rinv(nob,k)
!      end do
!   end do
!end do
if(r_kind == kind(1.d0)) then
   call dgemm('n','t',nanals,nobsl,nanals,1.d0,pa,nanals,hdxf_rinv,&
        nobsl,0.d0,work2,nanals)
else
   call sgemm('n','t',nanals,nobsl,nanals,1.e0,pa,nanals,hdxf_rinv,&
        nobsl,0.e0,work2,nanals)
end if
! Pa hdxb_rinv^T dep
do nanal=1,nanals
   work3(nanal) = work2(nanal,1) * dep(1)
   do nob=2,nobsl
      work3(nanal) = work3(nanal) + work2(nanal,nob) * dep(nob)
   end do
end do
! T = sqrt[(m-1)Pa]
do j=1,nanals
   rho = sqrt( real(nanals-1,r_kind) / eival(j) )
   do i=1,nanals
      work1(i,j) = eivec(i,j) * rho
   end do
end do
if(r_kind == kind(1.d0)) then
   call dgemm('n','t',nanals,nanals,nanals,1.d0,work1,nanals,eivec,&
        & nanals,0.d0,trans,nanals)
else
   call sgemm('n','t',nanals,nanals,nanals,1.e0,work1,nanals,eivec,&
        & nanals,0.e0,trans,nanals)
end if
!do j=1,nanals
!   do i=1,nanals
!      trans(i,j) = work1(i,1) * eivec(j,1)
!      do k=2,nanals
!         trans(i,j) = trans(i,j) + work1(i,k) * eivec(j,k)
!      end do
!   end do
!end do
! T + Pa hdxb_rinv^T dep
do j=1,nanals
   do i=1,nanals
      trans(i,j) = trans(i,j) + work3(i)
   end do
end do

return
end subroutine letkf_core

subroutine obs_local(imin,imax,jmin,jmax,nobsl,nobs_use)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    obs_local
!
!   prgmmr: ota
!
! abstract:  select local observations from the observation box
!
! program history log:
!   2011-06-20  ota
!
!   input argument list:
!     imin imax jmin jmax  - search range for observation box
!     nobsl    - number of observations already collected
!     nobs_use - observation indices selected
!
!   output argument list:
!     nobsl    - number of observations already collected
!     nobs_use - observation indices selected
!
! attributes:
!   language:  f95
!   machine:
!
!$$$ end documentation block
implicit none
integer(i_kind),intent(in) :: imin
integer(i_kind),intent(in) :: imax
integer(i_kind),intent(in) :: jmin
integer(i_kind),intent(in) :: jmax
integer(i_kind),intent(inout) :: nobsl
integer(i_kind),intent(inout) :: nobs_use(nobsgood)
integer(i_kind) :: j,ib1,ib2,ib3,ie1,ie2,ie3,ip
do j=jmin,jmax
   ! Get observation index
   if(imin > 1) then
      ib1 = nlocconv(j,imin-1)+1
      ib2 = nlocoz(j,imin-1)+1
      ib3 = nlocsat(j,imin-1)+1
   else
      if(j > 1) then
         ib1 = nlocconv(j-1,blonnum+1)+1
         ib2 = nlocoz(j-1,blonnum+1)+1
         ib3 = nlocsat(j-1,blonnum+1)+1
      else
         ib1=1
         ib2=nobs_conv+1
         ib3=nobs_conv+nobs_oz+1
      end if
   end if
   ie1 = nlocconv(j,imax)
   ie2 = nlocoz(j,imax)
   ie3 = nlocsat(j,imax)
   if(nobs_conv > 0) then
      do ip=ib1,ie1
         nobsl=nobsl+1
         nobs_use(nobsl)=ip
      end do
   end if
   if(nobs_oz > 0) then
      do ip=ib2,ie2
         nobsl=nobsl+1
         nobs_use(nobsl)=ip
      end do
   end if
   if(nobs_sat > 0) then
      do ip=ib3,ie3
         nobsl=nobsl+1
         nobs_use(nobsl)=ip
      end do
   end if
end do

return
end subroutine obs_local

end module letkf
