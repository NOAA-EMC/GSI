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
!  vertical. Note however, that time localization *not used in LETKF*. 
!  The function used for localization (function taper)
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
!               enkf_obsmod, radinfo, radbias, gridinfo
!
! program history log:
!   2011-06-01  Created from Whitaker's serial EnSRF core module.
!   2015-07-25  Modified to use kdtree for range search instead of
!               original box routine.  Optimizations for no vert localization.
!
! attributes:
!   language: f95
!
!$$$

use mpisetup
use omp_lib, only: omp_get_num_threads
use covlocal, only:  taper, latval
use kinds, only: r_double,i_kind,r_kind,r_single
use loadbal, only: numobsperproc, numptsperproc, indxproc_obs, iprocob, &
                   indxproc, lnp_chunk, &
                   ensmean_obchunk, indxob_chunk, oblnp_chunk, nobs_max, &
                   obtime_chunk, grdloc_chunk, obloc_chunk, &
                   anal_obchunk_prior, kdtree_obs2, kdtree_obs
use statevec, only: ensmean_chunk, anal_chunk
use enkf_obsmod, only: oberrvar, ob, ensmean_ob, obloc, oblnp, &
                  nobsgood, nobs_conv, nobs_oz, nobs_sat,&
                  obfit_prior, obfit_post, obsprd_prior, obsprd_post, obtime,&
                  numobspersat, deltapredx, biaspreds,&
                  biasprednorm, probgrosserr, prpgerr,&
                  corrlengthsq,lnsigl,obtimel,anal_ob,obloclat, obloclon
use constants, only: pi, one, zero, rad2deg, deg2rad
use params, only: sprd_tol, ndim, datapath, nanals, &
                  iassim_order,sortinc,deterministic,numiter,nlevs,nvars,&
                  zhuberleft,zhuberright,varqc,lupd_satbiasc,huber,&
                  corrlengthnh,corrlengthtr,corrlengthsh,nbackgrounds
use radinfo, only: npred,nusis,nuchan,jpch_rad,predx
use radbias, only: apply_biascorr, update_biascorr
use gridinfo, only: nlevs_pres,index_pres,lonsgrd,latsgrd
use kdtree2_module, only: kdtree2_r_nearest, kdtree2_result

implicit none

private
public :: letkf_update

contains

subroutine letkf_update()
implicit none
! LETKF update.

! local variables.
integer(i_kind) nob,nob1,nob2,nf,nobxx,nskip,&
                niter,i,nrej,npt,np
real(r_double) :: t1,t2,t3,t4,t5,tbegin,tend,tmin,tmax,tmean
real(r_kind) r_nanals,r_nanalsm1
real(r_kind) normdepart, pnge, width
real(r_single),allocatable, dimension(:,:) :: anal_obchunk, buffertmp3
real(r_kind),dimension(nobsgood):: oberrvaruse
real(r_single), allocatable, dimension(:) :: buffertmp, buffertmp2
integer(i_kind) ierr
integer(i_kind) nn,nobm,nsame
logical lastiter, vlocal
! For LETKF core processes
real(r_kind),allocatable,dimension(:,:) :: hdxf
real(r_kind),allocatable,dimension(:) :: rdiag,dep,rloc,obdep,oberinv
real(r_kind),dimension(nanals,nanals) :: trans
real(r_kind),dimension(nanals) :: work,work2
logical,dimension(nobs_max) :: oupdate
integer(i_kind),allocatable,dimension(:) :: oindex
real(r_kind),dimension(nobsgood) :: invcorlen, invlnsigl
real(r_kind) :: vdist
real(r_kind) :: corrlength
real(r_single) :: deglat, dist, corrsq, eps
integer(i_kind) :: nobsl, ngrd1, nobsl2, nthreads, nb, &
                   nobslocal_max,nobslocal_maxall
type(kdtree2_result),dimension(:),allocatable :: sresults
logical :: sameloc,samehloc,samevloc

!$omp parallel
nthreads = omp_get_num_threads()
!$omp end parallel
if (nproc == 0) print *,'using',nthreads,' openmp threads'

! allocate temporary array.
allocate(anal_obchunk(nanals,nobs_max))
allocate(obdep(nobsgood))
allocate(oberinv(nobsgood))

! define a few frequently used parameters
r_nanals=one/float(nanals)
r_nanalsm1=one/float(nanals-1)

if (minval(lnsigl) > 1.e3) then
   vlocal = .false.
   if (nproc == 0) print *,'no vertical localization in LETKF'
else
   vlocal = .true.
endif

if (numiter == 0) then
! don't do update in observation space.
  anal_obchunk = anal_obchunk_prior
  ! nob1 is the index of the obs to be processed on this rank
  ! nob2 maps nob1 to 1:nobsgood array (nob)
  do nob1=1,numobsperproc(nproc+1)
     nob2 = indxproc_obs(nproc+1,nob1)
     ensmean_obchunk(nob1) = ensmean_ob(nob2)
  enddo
  oberrvaruse(1:nobsgood) = oberrvar(1:nobsgood)
  ! Compute the inverse of cut-off length and 
  ! the observation departure from first guess
!$omp parallel do private(nob)
  do nob=1,nobsgood
     invcorlen(nob)=one/corrlengthsq(nob)
     invlnsigl(nob)=one/lnsigl(nob)
     oberinv(nob)=one/oberrvaruse(nob)
     obdep(nob)=ob(nob)-ensmean_ob(nob)
  end do
end if

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
  tbegin = mpi_wtime()

  ! Compute the inverse of cut-off length and 
  ! the observation departure from first guess
!$omp parallel do private(nob)
  do nob=1,nobsgood
     invcorlen(nob)=one/corrlengthsq(nob)
     invlnsigl(nob)=one/lnsigl(nob)
     oberinv(nob)=one/oberrvaruse(nob)
     obdep(nob)=ob(nob)-ensmean_ob(nob)
  end do

  t2 = zero
  t3 = zero
  t4 = zero
  t5 = zero
  nskip = 0
  eps = 1.e-5 ! threshold distance for determining if ob is at same location
  oupdate = .false.
  ! Update ensemble in observation space.
  ! Loop for observations on this task.
!$omp parallel do schedule(dynamic) private(nob,nob1,nob2, &
!$omp                  nobsl,nobsl2,corrsq,corrlength, &
!$omp                  hdxf,rdiag,dep,rloc,vdist, &
!$omp                  nf,work,trans,sameloc,samehloc,samevloc, &
!$omp                  work2,dist,oindex,oupdate,sresults) &
!$omp  reduction(+:t1,t2,t3,t4,t5,nskip)
  obsloop: do nob1=1,numobsperproc(nproc+1)

     t1 = mpi_wtime()
     nob2=indxproc_obs(nproc+1,nob1)

     ! no need to recompute LETKF transformation matrix (trans)
     ! if this ob is at same lat/lon/pressure as previous one.
     if(nob1 .ne. 1) then
        samehloc = &
        sum(abs(obloc_chunk(1:3,nob1)-obloc_chunk(1:3,nob1-1))) < eps
        samevloc = abs(oblnp_chunk(nob1)-oblnp_chunk(nob1-1)) < eps
     else
        samehloc = .false.
        samevloc = .false.
     endif
     ! if no vertical localization, can use same LETKF transformation
     ! for all observation variables at the same horizontal location.
     if (.not. vlocal) samevloc = .true.
     sameloc = samehloc .and. samevloc

     ! if horiz and vert location is the same as previous one, this ob can be skipped
     if (sameloc) cycle obsloop

     ! Pick up observations close to current ob (in horizontal).
     ! if ob location is same as previous one, don't recompute.
     if(.not. samehloc .or. .not. allocated(sresults)) then
        if (allocated(sresults)) deallocate(sresults)
        allocate(sresults(nobsgood))
        call kdtree2_r_nearest(tp=kdtree_obs2,qv=obloc_chunk(:,nob1),&
             r2=corrlengthsq(nob2),&
             nfound=nobsl,nalloc=nobsgood,results=sresults)
     endif

     t2 = t2 + mpi_wtime() - t1
     t1 = mpi_wtime()

     ! if ob already updated, skip it.
     if (oupdate(nob1)) then
         nskip=nskip+1
         cycle obsloop
     endif
     
     ! Skip when there are observations in range.
     if(nobsl == 0) then
         oupdate(nob1) = .true.
         cycle obsloop
     end if

     ! Pick up variables passed to LETKF core process
     allocate(oindex(nobsl))
     allocate(rloc(nobsl))
     nobsl2=1
     do nob=1,nobsl
        nf = sresults(nob)%idx
        vdist=(oblnp_chunk(nob1)-oblnp(nf))*invlnsigl(nf)
        if(abs(vdist) >= one) cycle
        dist = sqrt(sresults(nob)%dis*invcorlen(sresults(nob)%idx)+vdist*vdist)
        if (dist >= one) cycle
        rloc(nobsl2)=taper(dist)
        oindex(nobsl2)=nf
        if(rloc(nobsl2) /= zero) then
           nobsl2=nobsl2+1
        end if
     end do
     nobsl2=nobsl2-1
     allocate(rdiag(nobsl2))
     allocate(dep(nobsl2))
     allocate(hdxf(nobsl2,nanals))
     do nob=1,nobsl2
        nf = oindex(nob)
        hdxf(nob,1:nanals)=anal_ob(1:nanals,nf)  ! WE NEED anal_ob (global)
        rdiag(nob)=oberinv(nf)
        dep(nob)=obdep(nf)
     enddo
     deallocate(oindex)

     t3 = t3 + mpi_wtime() - t1
     t1 = mpi_wtime()

     if(nobsl2 == 0) then
        deallocate(hdxf,rdiag,dep,rloc)
        oupdate(nob1) = .true.
        cycle obsloop
     end if

     ! Compute transformation matrix (trans) of LETKF
     call letkf_core(nobsl2,hdxf,rdiag,dep,rloc(1:nobsl2),trans)
     deallocate(hdxf,rdiag,dep,rloc)

     t4 = t4 + mpi_wtime() - t1
     t1 = mpi_wtime()

     ! Update observation space ensembles
     ! loop over all obs not yet processed, update those
     ! at same horizontal and vertical location.
     do nob=nob1,numobsperproc(nproc+1)
        if(oupdate(nob)) cycle
        samehloc = &
        sum(abs(obloc_chunk(1:3,nob1)-obloc_chunk(1:3,nob))) < eps
        samevloc = abs(oblnp_chunk(nob1)-oblnp_chunk(nob)) < eps
        if (.not. samehloc .or. .not. samevloc) cycle
        oupdate(nob) = .true.
        work(1:nanals) = anal_obchunk(1:nanals,nob)
        work2(1:nanals) = ensmean_obchunk(nob)
        if(r_kind == kind(1.d0)) then
           call dgemv('t',nanals,nanals,1.d0,trans,nanals,work,1,1.d0,work2,1)
        else
           call sgemv('t',nanals,nanals,1.e0,trans,nanals,work,1,1.e0,work2,1)
        end if
        ensmean_obchunk(nob) = sum(work2(1:nanals)) * r_nanals
        anal_obchunk(1:nanals,nob) = work2(1:nanals)-ensmean_obchunk(nob)
     enddo

     t5 = t5 + mpi_wtime() - t1

  end do obsloop
!$omp end parallel do
  if (allocated(sresults)) deallocate(sresults)

  tend = mpi_wtime()
  call mpi_reduce(tend-tbegin,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
  tmean = tmean/numproc
  call mpi_reduce(tend-tbegin,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
  call mpi_reduce(tend-tbegin,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
  if (nproc .eq. 0) print *,'min/max/mean time to update obs',tmin,tmax,tmean
  if ((nproc == 0 .or. nproc .eq. numproc-1) .and. nskip > 0) print *,nskip,' obs skipped out of',&
                                                              numobsperproc(nproc+1),' on proc',nproc
  if ((nproc == 0 .or. nproc .eq. numproc-1) .and. nrej >  0) print *,nrej,' obs rejected by varqc on proc',&
                                                              numobsperproc(nproc+1)
  t2 = t2/nthreads; t3 = t3/nthreads; t4 = t4/nthreads; t5 = t5/nthreads
  if (nproc == 0 .or. nproc == numproc-1) write(6,8003) niter,'timing on proc',nproc,' = ',t2,t3,t4,t5,nrej
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

if (numiter > 0) then
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
end if

! free temporary arrays.
deallocate(anal_obchunk_prior)

t2 = zero
t3 = zero
t4 = zero
t5 = zero
tbegin = mpi_wtime()
nobslocal_max = -999

! Update ensemble on model grid.
! Loop for each horizontal grid points on this task.
!$omp parallel do schedule(dynamic) private(npt,nob,nobsl, &
!$omp                  nobsl2,ngrd1,corrlength, &
!$omp                  nf,vdist, &
!$omp                  nn,hdxf,rdiag,dep,rloc,i,work,work2,trans, &
!$omp                  oindex,deglat,dist,corrsq,nb,sresults) &
!$omp  reduction(+:t1,t2,t3,t4,t5) &
!$omp  reduction(max:nobslocal_max)
grdloop: do npt=1,numptsperproc(nproc+1)

   t1 = mpi_wtime()

   ! find obs close to this grid point (using kdtree)
   ngrd1=indxproc(nproc+1,npt)
   deglat = latsgrd(ngrd1)*rad2deg
   corrlength=latval(deglat,corrlengthnh,corrlengthtr,corrlengthsh)
   corrsq = corrlength**2
   allocate(sresults(nobsgood))
   ! kd-tree fixed range search
   call kdtree2_r_nearest(tp=kdtree_obs2,qv=grdloc_chunk(:,npt),r2=corrsq,&
        nfound=nobsl,nalloc=nobsgood,results=sresults)

   t2 = t2 + mpi_wtime() - t1
   t1 = mpi_wtime()

   ! Skip when no observations in local area
   if(nobsl == 0) cycle grdloop

   if (vlocal) then
   ! Loop for each vertical layers
   verloop: do nn=1,nlevs_pres

      ! Pick up variables passed to LETKF core process
      allocate(rloc(nobsl))
      allocate(oindex(nobsl))
      nobsl2=1
      do nob=1,nobsl
         nf = sresults(nob)%idx
         vdist=(lnp_chunk(npt,nn)-oblnp(nf))*invlnsigl(nf)
         if(abs(vdist) >= one) cycle
         dist = sqrt(sresults(nob)%dis*invcorlen(sresults(nob)%idx)+vdist*vdist)
         if (dist >= one) cycle
         rloc(nobsl2)=taper(dist)
         oindex(nobsl2)=nf
         if(rloc(nobsl2) > tiny(rloc(nobsl2))) then
            nobsl2=nobsl2+1
         end if
      end do
      nobsl2=nobsl2-1
      if (nobsl2 > nobslocal_max) nobslocal_max=nobsl2
      if(nobsl2 == 0) then
         deallocate(rloc,oindex)
         cycle verloop
      end if
      allocate(hdxf(nobsl2,nanals))
      allocate(rdiag(nobsl2))
      allocate(dep(nobsl2))
      do nob=1,nobsl2
         nf=oindex(nob)
         hdxf(nob,1:nanals)=anal_ob(1:nanals,nf) ! WE NEED anal_ob (global)
         rdiag(nob)=oberinv(nf)
         dep(nob)=obdep(nf)
      end do
      deallocate(oindex)

      t3 = t3 + mpi_wtime() - t1
      t1 = mpi_wtime()

      ! Compute transformation matrix of LETKF
      call letkf_core(nobsl2,hdxf,rdiag,dep,rloc(1:nobsl2),trans)
      deallocate(hdxf,rdiag,dep,rloc)

      t4 = t4 + mpi_wtime() - t1
      t1 = mpi_wtime()

      ! Update analysis ensembles (all time levels)
      do nb=1,nbackgrounds
      do i=1,ndim
         if(index_pres(i) /= nn) cycle
         work(1:nanals) = anal_chunk(1:nanals,npt,i,nb)
         work2(1:nanals) = ensmean_chunk(npt,i,nb)
         if(r_kind == kind(1.d0)) then
            call dgemv('t',nanals,nanals,1.d0,trans,nanals,work,1,1.d0, &
                 & work2,1)
         else
            call sgemv('t',nanals,nanals,1.e0,trans,nanals,work,1,1.e0, &
                 & work2,1)
         end if
         ensmean_chunk(npt,i,nb) = sum(work2(1:nanals)) * r_nanals
         anal_chunk(1:nanals,npt,i,nb) = work2(1:nanals)-ensmean_chunk(npt,i,nb)
      end do
      end do

      t5 = t5 + mpi_wtime() - t1
      t1 = mpi_wtime()

   end do verloop
   else ! vlocal = .false.: no vertical localization

   ! Pick up variables passed to LETKF core process
   allocate(oindex(nobsl))
   allocate(rloc(nobsl))
   nobsl2=1
   do nob=1,nobsl
      nf = sresults(nob)%idx
      dist = sqrt(sresults(nob)%dis*invcorlen(sresults(nob)%idx))
      if (dist >= one) cycle
      rloc(nobsl2)=taper(dist)
      oindex(nobsl2)=nf
      if(rloc(nobsl2) > tiny(rloc(nobsl2))) then
         nobsl2=nobsl2+1
      end if
   end do
   nobsl2=nobsl2-1 ! total number of obs in local volume.
   if (nobsl2 > nobslocal_max) nobslocal_max=nobsl2

   if(nobsl2 > 0) then ! obs in volume

      allocate(hdxf(nobsl2,nanals))
      allocate(rdiag(nobsl2))
      allocate(dep(nobsl2))
      do nob=1,nobsl2
         nf=oindex(nob)
         hdxf(nob,1:nanals)=anal_ob(1:nanals,nf) ! WE NEED anal_ob (global)
         rdiag(nob)=oberinv(nf)
         dep(nob)=obdep(nf)
      end do
      deallocate(oindex)

      t3 = t3 + mpi_wtime() - t1
      t1 = mpi_wtime()

      ! Compute transformation matrix of LETKF
      call letkf_core(nobsl2,hdxf,rdiag,dep,rloc(1:nobsl2),trans)
      deallocate(hdxf,rdiag,dep,rloc)

      t4 = t4 + mpi_wtime() - t1
      t1 = mpi_wtime()

      ! Update analysis ensembles (all time levels)
      ! since there is no vertical localization, weights computed
      ! for this horizontal grid point can be applied to all points/variables in column.
      do nb=1,nbackgrounds
      do i=1,ndim
         work(1:nanals) = anal_chunk(1:nanals,npt,i,nb)
         work2(1:nanals) = ensmean_chunk(npt,i,nb)
         if(r_kind == kind(1.d0)) then
            call dgemv('t',nanals,nanals,1.d0,trans,nanals,work,1,1.d0, &
                 & work2,1)
         else
            call sgemv('t',nanals,nanals,1.e0,trans,nanals,work,1,1.e0, &
                 & work2,1)
         end if
         ensmean_chunk(npt,i,nb) = sum(work2(1:nanals)) * r_nanals
         anal_chunk(1:nanals,npt,i,nb) = work2(1:nanals)-ensmean_chunk(npt,i,nb)
      end do
      end do

      t5 = t5 + mpi_wtime() - t1
      t1 = mpi_wtime()
   else ! no obs in volume
      deallocate(rloc,oindex)
   end if
   end if
   if (allocated(sresults)) deallocate(sresults)
end do grdloop
!$omp end parallel do

tend = mpi_wtime()
call mpi_reduce(tend-tbegin,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(tend-tbegin,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(tend-tbegin,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,'min/max/mean time to update grid',tmin,tmax,tmean
t2 = t2/nthreads; t3 = t3/nthreads; t4 = t4/nthreads; t5 = t5/nthreads
if (nproc == 0 .or. nproc == numproc-1) print *,'time to process analysis on gridpoint = ',t2,t3,t4,t5,' secs'
call mpi_reduce(nobslocal_max,nobslocal_maxall,1,mpi_integer,mpi_max,0,mpi_comm_world,ierr)
if (nproc == 0) print *,'max number of obs in local volume',nobslocal_maxall

deallocate(obdep,oberinv)

! Gathering updated analysis perturbations projected on the observation space
if (numiter > 0) then
if (nproc /= 0) then
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
end if

deallocate(anal_obchunk) ! these are allocated in loadbal

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
!   2014-06-20  whitaker: optimization for case when no vertical localization
!               is used.  Allow for numiter=0 (skip ob space update). Fixed
!               missing openmp private declarations in obsloop and grdloop.
!               Use openmp reductions for profiling openmp loops. Use LAPACK
!               routine for eigenanalysis.
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
real(r_kind),dimension(nobsl ,nanals),intent(inout) :: hdxf
real(r_kind),dimension(nobsl        ),intent(in ) :: rdiaginv
real(r_kind),dimension(nobsl        ),intent(in ) :: dep
real(r_kind),dimension(nobsl        ),intent(in ) :: rloc
real(r_kind),dimension(nanals,nanals),intent(out) :: trans
real(r_kind), allocatable, dimension(:,:) :: work1,work2,eivec,pa
real(r_kind), allocatable, dimension(:) :: rrloc,eival,work3
real(r_kind) :: rho
integer(i_kind) :: i,j,nob,nanal,ierr,lwork
allocate(work3(nanals),work2(nanals,nobsl))
allocate(eivec(nanals,nanals),pa(nanals,nanals))
allocate(work1(nanals,nanals),eival(nanals),rrloc(nobsl))
! hdxf sqrt(Rinv)
rrloc(1:nobsl) = rdiaginv(1:nobsl) * rloc(1:nobsl)
rho = tiny(rrloc)
where (rrloc < rho) rrloc = rho
rrloc = sqrt(rrloc)
do nanal=1,nanals
   hdxf(1:nobsl,nanal) = hdxf(1:nobsl,nanal) * rrloc(1:nobsl)
end do
! hdxf^T Rinv hdxf
!do j=1,nanals
!   do i=1,nanals
!      work1(i,j) = hdxf(1,i) * hdxf(1,j)
!      do nob=2,nobsl
!         work1(i,j) = work1(i,j) + hdxf(nob,i) * hdxf(nob,j)
!      end do
!   end do
!end do
if(r_kind == kind(1.d0)) then
   call dgemm('t','n',nanals,nanals,nobsl,1.d0,hdxf,nobsl, &
        hdxf,nobsl,0.d0,work1,nanals)
else
   call sgemm('t','n',nanals,nanals,nobsl,1.e0,hdxf,nobsl, &
        hdxf,nobsl,0.e0,work1,nanals)
end if
! hdxb^T Rinv hdxb + (m-1) I
do nanal=1,nanals
   work1(nanal,nanal) = work1(nanal,nanal) + real(nanals-1,r_kind)
end do
! eigenvalues and eigenvectors of [ hdxb^T Rinv hdxb + (m-1) I ]
eivec(:,:) = work1(:,:); lwork = -1
call dsyev('V','L',nanals,eivec,nanals,eival,work1(1,1),lwork,ierr)
lwork = min(nanals*nanals, int(work1(1,1)))
call dsyev('V','L',nanals,eivec,nanals,eival,work1(1,1),lwork,ierr)
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
! convert hdxf * Rinv^T from hdxf * sqrt(Rinv)^T
do nanal=1,nanals
   hdxf(1:nobsl,nanal) = hdxf(1:nobsl,nanal) * rrloc(1:nobsl)
end do
! Pa hdxb_rinv^T
!do nob=1,nobsl
!   do nanal=1,nanals
!      work2(nanal,nob) = pa(nanal,1) * hdxf(nob,1)
!      do k=2,nanals
!         work2(nanal,nob) = work2(nanal,nob) + pa(nanal,k) * hdxf(nob,k)
!      end do
!   end do
!end do
if(r_kind == kind(1.d0)) then
   call dgemm('n','t',nanals,nobsl,nanals,1.d0,pa,nanals,hdxf,&
        nobsl,0.d0,work2,nanals)
else
   call sgemm('n','t',nanals,nobsl,nanals,1.e0,pa,nanals,hdxf,&
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
deallocate(work2,eivec,pa,work1,rrloc,eival,work3)

return
end subroutine letkf_core

end module letkf
