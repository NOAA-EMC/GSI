module letkf
!$$$  module documentation block
!
! module: letkf                        Update model state variables and
!                                      bias coefficients with the LETKF.
!
! prgmmr: ota              org: np23                   date: 2011-06-01
!         updates, optimizations by whitaker
!
! abstract: Updates the model state using the LETKF (Hunt et al 2007,
!  Physica D, 112-126). Uses 'gain form' of LETKF algorithm described
!  Bishop et al 2017 (https://doi.org/10.1175/MWR-D-17-0102.1).
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
!  process. So they are saved in all processors. If the code is compiled with
!  -DMPI3, a single copy of the observation space ensemble is stored on each
!  compute node and shared among processors.
!
!  Adaptive observation thinning implemented in the serial EnSRF is not 
!  implemented here in the current version. The parameter nobsl_max controls
!  the maximum number of obs that will be assimilated in each local patch.
!  (the nobsl_max closest are chosen by default, if dfs_sort=T then they
!   are ranked by decreasing DFS).
!
!  Vertical covariance localization can be turned off with letkf_novlocal.
!  (this is done automatically when model space vertical localization 
!   with modulated ensembles is enabled via neigv>0)
!  If neigv > 0 the eigenvectors of the localization
!  matrix are read from a file called 'vlocal_eig.dat' (created by an external
!  python utility).
!
!  Updating the state in observation space is not supported in the LETKF - 
!  use lupd_obspace_serial=.true. to perform the observation space update
!  using the serial EnSRF.
!
! Public Subroutines:
!  letkf_update: performs the LETKF update
!
! Public Variables: None
!
! Modules Used: kinds, constants, params, covlocal, mpisetup, loadbal, statevec,
!               enkf_obsmod, radinfo, radbias, gridinfo
!
! program history log:
!   2011-06-01  ota: Created from Whitaker's serial EnSRF core module.
!   2015-07-25  whitaker: Optimization for case when no vertical localization
!               is used. Fixed missing openmp private declarations in obsloop and grdloop.
!               Use openmp reductions for profiling openmp loops. Use kdtree
!               for range search instead of original box routine. Modify
!               ob space update to use weights computed at nearest grid point.
!   2016-02-01  whitaker: Use MPI-3 shared memory pointers to reduce memory
!               footprint by only allocating observation prior ensemble
!               array on one MPI task per node. Also ensure posterior
!               perturbation mean is zero.
!   2016-05-02  shlyaeva: Modification for reading state vector from table.
!   2016-07-05  whitaker: remove buggy code for observation space update.
!               Rely on serial EnSRF to perform observation space update
!               using logical lupd_obspace_serial.
!   2016-11-29  shlyaeva: Modification for using control vector (control and
!               state used to be the same) and the "chunks" come from loadbal
!   2018-05-31  whitaker:  add modulated ensemble model-space vertical
!               localization (when neigv>0) and ob selection using DFS 
!               (when dfs_sort=T). Add options for DEnKF and gain form of LETKF.

!
! attributes:
!   language: f95
!
!$$$

use mpisetup
use random_normal, only : rnorm, set_random_seed
use, intrinsic :: iso_c_binding
use omp_lib, only: omp_get_num_threads,omp_get_thread_num
use covlocal, only:  taper, latval
use kinds, only: r_double,i_kind,r_kind,r_single,num_bytes_for_r_single
use loadbal, only: numptsperproc, npts_max, &
                   indxproc, lnp_chunk, &
                   grdloc_chunk, kdtree_obs2, &
                   ensmean_chunk, anal_chunk
use controlvec, only: ncdim, index_pres
use enkf_obsmod, only: oberrvar, ob, ensmean_ob, obloc, oblnp, &
                  nobstot, nobs_conv, nobs_oz, nobs_sat,&
                  obfit_prior, obfit_post, obsprd_prior, obsprd_post,&
                  numobspersat, biaspreds, corrlengthsq,&
                  probgrosserr, prpgerr, obtype, obpress,&
                  lnsigl, anal_ob, anal_ob_modens, obloclat, obloclon, stattype
use constants, only: pi, one, zero, rad2deg, deg2rad
use params, only: sprd_tol, datapath, nanals, iseed_perturbed_obs,&
                  iassim_order,sortinc,deterministic,nlevs,&
                  zhuberleft,zhuberright,varqc,lupd_satbiasc,huber,letkf_novlocal,&
                  lupd_obspace_serial,corrlengthnh,corrlengthtr,corrlengthsh,&
                  getkf,getkf_inflation,denkf,nbackgrounds,nobsl_max,neigv,vlocal_evecs,dfs_sort
use gridinfo, only: nlevs_pres,lonsgrd,latsgrd,logp,npts,gridloc
use kdtree2_module, only: kdtree2, kdtree2_create, kdtree2_destroy, &
                          kdtree2_result, kdtree2_n_nearest, kdtree2_r_nearest
use sorting, only: quicksort
use radbias, only: apply_biascorr

implicit none

private
public :: letkf_update

contains

subroutine letkf_update()
implicit none
! LETKF update.

! local variables.
integer(i_kind) nob,nf,nanal,nens,&
                i,nlev,nrej,npt,nn,nnmax,ierr
integer(i_kind) nobsl, ngrd1, nobsl2, nthreads, nb, &
                nobslocal_min,nobslocal_max, &
                nobslocal_minall,nobslocal_maxall
integer(i_kind),allocatable,dimension(:) :: oindex
real(r_single) :: deglat, dist, corrsq, oberrfact, gain, r, trpa, trpa_raw
real(r_double) :: t1,t2,t3,t4,t5,tbegin,tend,tmin,tmax,tmean
real(r_kind) r_nanals,r_nanalsm1
real(r_kind) normdepart, pnge, width
real(r_kind),dimension(nobstot):: oberrvaruse
real(r_kind) vdist
real(r_kind) corrlength
logical vlocal, kdobs
! For LETKF core processes
real(r_kind),allocatable,dimension(:,:) :: hxens
real(r_single),allocatable,dimension(:,:) :: obens
real(r_single),allocatable,dimension(:,:,:) :: ens_tmp
real(r_single),allocatable,dimension(:,:) :: wts_ensperts,pa
real(r_single),allocatable,dimension(:) :: dfs,wts_ensmean
real(r_kind),allocatable,dimension(:) :: rdiag,rloc,statesprd_prior
real(r_single),allocatable,dimension(:) :: dep,kfgain
! kdtree stuff
type(kdtree2_result),dimension(:),allocatable :: sresults
integer(i_kind), dimension(:), allocatable :: indxassim, indxob
#ifdef MPI3
! pointers used for MPI-3 shared memory manipulations.
real(r_single), pointer, dimension(:,:) :: anal_ob_fp ! Fortran pointer
type(c_ptr)                             :: anal_ob_cp ! C pointer
real(r_single), pointer, dimension(:,:) :: anal_ob_modens_fp ! Fortran pointer
type(c_ptr)                             :: anal_ob_modens_cp ! C pointer
integer disp_unit, shm_win, shm_win2
integer(MPI_ADDRESS_KIND) :: win_size, nsize, nsize2, win_size2
integer(MPI_ADDRESS_KIND) :: segment_size
#endif
real(r_single), allocatable, dimension(:) :: buffer
real(r_kind) eps

eps = epsilon(0.0_r_single) ! real(4) machine precision

!$omp parallel
nthreads = omp_get_num_threads()
!$omp end parallel

if (nproc == 0) print *,'using',nthreads,' openmp threads'

! define a few frequently used parameters
r_nanals=one/float(nanals)
r_nanalsm1=one/float(nanals-1)

kdobs=associated(kdtree_obs2)

t1 = mpi_wtime()
#ifdef MPI3
! setup shared memory segment on each node that points to
! observation prior ensemble.
! shared window size will be zero except on root task of
! shared memory group on each node.
disp_unit = num_bytes_for_r_single ! anal_ob is r_single
nsize = nobstot*nanals
nsize2 = nobstot*nanals*neigv
if (neigv > 0) then
   nens = nanals*neigv ! modulated ensemble size
else
   nens = nanals
endif
if (nproc_shm == 0) then
   win_size = nsize*disp_unit
   win_size2 = nsize2*disp_unit
else
   win_size = 0
   win_size2 = 0
endif
call MPI_Win_allocate_shared(win_size, disp_unit, MPI_INFO_NULL,&
                             mpi_comm_shmem, anal_ob_cp, shm_win, ierr)
if (neigv > 0) then
   call MPI_Win_allocate_shared(win_size2, disp_unit, MPI_INFO_NULL,&
                                mpi_comm_shmem, anal_ob_modens_cp, shm_win2, ierr)
endif
if (nproc_shm == 0) then
   ! create shared memory segment on each shared mem comm
   call MPI_Win_lock(MPI_LOCK_EXCLUSIVE,0,MPI_MODE_NOCHECK,shm_win,ierr)
   call c_f_pointer(anal_ob_cp, anal_ob_fp, [nanals, nobstot])
   ! bcast entire obs prior ensemble from root task 
   ! to a single task on each node, assign to shared memory window.
   ! send one ensemble member at a time.
   allocate(buffer(nobstot))
   do nanal=1,nanals
      if (nproc == 0) buffer(1:nobstot) = anal_ob(nanal,1:nobstot)
      if (nproc_shm == 0) then
         call mpi_bcast(buffer,nobstot,mpi_real4,0,mpi_comm_shmemroot,ierr)
         anal_ob_fp(nanal,1:nobstot) = buffer(1:nobstot)
      end if 
   end do
   if (neigv > 0) then
      call MPI_Win_lock(MPI_LOCK_EXCLUSIVE,0,MPI_MODE_NOCHECK,shm_win2,ierr)
      call c_f_pointer(anal_ob_modens_cp, anal_ob_modens_fp, [nens, nobstot])
      do nanal=1,nens
         if (nproc == 0) buffer(1:nobstot) = anal_ob_modens(nanal,1:nobstot)
         if (nproc_shm == 0) then
            call mpi_bcast(buffer,nobstot,mpi_real4,0,mpi_comm_shmemroot,ierr)
            anal_ob_modens_fp(nanal,1:nobstot) = buffer(1:nobstot)
         end if 
      end do
   endif
   deallocate(buffer)
   call MPI_Win_unlock(0, shm_win, ierr)
   if (neigv > 0) call MPI_Win_unlock(0, shm_win2, ierr)
   nullify(anal_ob_fp)
   if (neigv > 0) nullify(anal_ob_modens_fp)
   ! don't need anal_ob anymore
   if (allocated(anal_ob)) deallocate(anal_ob)
   if (allocated(anal_ob_modens)) deallocate(anal_ob_modens)
endif
! barrier here to make sure no tasks try to access shared
! memory segment before it is created.
call mpi_barrier(mpi_comm_world, ierr)
! associate fortran pointer with c pointer to shared memory 
! segment (containing observation prior ensemble) on each task.
call MPI_Win_shared_query(shm_win, 0, segment_size, disp_unit, anal_ob_cp, ierr)
call c_f_pointer(anal_ob_cp, anal_ob_fp, [nanals, nobstot])
if (neigv > 0) then
   call MPI_Win_shared_query(shm_win2, 0, segment_size, disp_unit, anal_ob_modens_cp, ierr)
   call c_f_pointer(anal_ob_modens_cp, anal_ob_modens_fp, [nens, nobstot])
endif
#else
! if MPI3 not available, need anal_ob on every MPI task
! broadcast observation prior ensemble from root one ensemble member at a time.
allocate(buffer(nobstot))
! allocate anal_ob on non-root tasks
if (nproc .ne. 0) allocate(anal_ob(nanals,nobstot))
if (neigv > 0 .and. nproc .ne. 0) allocate(anal_ob_modens(nens,nobstot))
! bcast anal_ob from root one member at a time.
do nanal=1,nanals
   buffer(1:nobstot) = anal_ob(nanal,1:nobstot)
   call mpi_bcast(buffer,nobstot,mpi_real4,0,mpi_comm_world,ierr)
   if (nproc .ne. 0) anal_ob(nanal,1:nobstot) = buffer(1:nobstot)
end do
if (neigv > 0) then
   do nanal=1,nens
      buffer(1:nobstot) = anal_ob_modens(nanal,1:nobstot)
      call mpi_bcast(buffer,nobstot,mpi_real4,0,mpi_comm_world,ierr)
      if (nproc .ne. 0) anal_ob_modens(nanal,1:nobstot) = buffer(1:nobstot)
   end do
endif
deallocate(buffer)
#endif
t2 = mpi_wtime()
if (nproc .eq. 0) print *,'time to broadcast ob prior ensemble = ',t2-t1

if (nproc .eq. 0 .and. .not. deterministic) then
   print *,'perturbed obs LETKF'
endif
if (minval(lnsigl) > 1.e3 .or. letkf_novlocal) then
   vlocal = .false.
   if (nproc == 0) print *,'no vertical localization in LETKF'
   ! if no vertical localization, weights
   ! need only be computed once for each column.
   nnmax = 1
else
   vlocal = .true.
   ! if vertical localization on, analysis weights
   ! need to be computed for every vertical level.
   nnmax = nlevs_pres
endif
if (nproc == 0 .and. .not. deterministic) then
   print *,'warning - perturbed obs not used in LETKF (deterministic=F ignored)'
endif

! apply bias correction with latest estimate of bias coeffs
! (if bias correction update in ob space turned on).
if (nobs_sat > 0 .and. lupd_satbiasc .and. lupd_obspace_serial) call apply_biascorr()

nrej=0
! reset ob error to account for gross errors 
if (varqc .and. lupd_obspace_serial) then
    if (huber) then ! "huber norm" QC
      do nob=1,nobstot
        ! observation space update performed in serial filter 
        ! using lupd_obspace_serial
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
      do nob=1,nobstot
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
     oberrvaruse(1:nobstot) = oberrvar(1:nobstot)
end if

tbegin = mpi_wtime()

t2 = zero
t3 = zero
t4 = zero
t5 = zero
tbegin = mpi_wtime()
nobslocal_max = -999
nobslocal_min = nobstot

if (nobsl_max > 0 .and. dfs_sort) then
    allocate(statesprd_prior(ncdim))
endif

! Update ensemble on model grid.
! Loop for each horizontal grid points on this task.
!$omp parallel do schedule(dynamic) private(npt,nob,nobsl, &
!$omp                  gain,nobsl2,oberrfact,ngrd1,corrlength,ens_tmp, &
!$omp                  nf,vdist,obens,indxassim,indxob, &
!$omp                  nn,hxens,wts_ensmean,dfs,rdiag,dep,rloc,i, &
!$omp                  oindex,deglat,dist,corrsq,nb,sresults, &
!$omp                  wts_ensperts,pa,trpa,trpa_raw,kfgain) &
!$omp  reduction(+:t1,t2,t3,t4,t5) &
!$omp  reduction(max:nobslocal_max) &
!$omp  reduction(min:nobslocal_min) 
grdloop: do npt=1,numptsperproc(nproc+1)

   t1 = mpi_wtime()
   if (.not. allocated(ens_tmp)) allocate(ens_tmp(nens,ncdim,nbackgrounds))
   ! find obs close to this grid point (using kdtree)
   ngrd1=indxproc(nproc+1,npt)
   deglat = latsgrd(ngrd1)*rad2deg
   corrlength=latval(deglat,corrlengthnh,corrlengthtr,corrlengthsh)
   corrsq = corrlength**2
   allocate(sresults(nobstot))
   do nb=1,nbackgrounds
      do i=1,ncdim ! state space ensemble spread for column being updated
         nlev = index_pres(i) ! vertical index for i'th control variable
         if (nlev .eq. nlevs+1) nlev=1 ! 2d fields, assume surface
         if (neigv > 0 ) then
            call expand_ens(neigv,nanals, &
                            anal_chunk(1:nanals,npt,i,nb), &
                            ens_tmp(:,i,nb),vlocal_evecs(:,nlev))
         else
            ens_tmp(:,i,nb) = anal_chunk(:,npt,i,nb)
         endif
      enddo
   enddo
   ! kd-tree fixed range search
   !if (allocated(sresults)) deallocate(sresults)
   if (nobsl_max > 0) then ! only use nobsl_max nearest obs (sorted by distance).
       if (dfs_sort) then ! sort by DFS instead of distance.
          do i=1,ncdim ! state space ensemble spread for column being updated
             statesprd_prior(i) =  &
             sqrt(sum(ens_tmp(:,i,(nbackgrounds/2)+1)**2)*r_nanalsm1)
          enddo
          allocate(dfs(nobstot))
          allocate(rloc(nobstot))
          allocate(indxob(nobstot))
          ! calculate integrated DFS for each ob in local volume
          nobsl = 0
          do nob=1,nobstot
             rloc(nob) = sum((obloc(:,nob)-grdloc_chunk(:,npt))**2,1)
             dist = sqrt(rloc(nob)/corrlengthsq(nob))
             if (dist < 1.0 - eps .and. &
                 oberrvaruse(nob) < 1.e10_r_single) then
                nobsl = nobsl + 1
                dfs(nobsl) = 0.
                indxob(nobsl) = nob
                oberrfact = taper(dist)
                do i=1,ncdim
#ifdef MPI3
                    gain = sum(ens_tmp(1:nanals,i,(nbackgrounds/2)+1)*anal_ob_fp(1:nanals,nob))*r_nanalsm1
#else
                    gain = sum(ens_tmp(1:nanals,i,(nbackgrounds/2)+1)*anal_ob(1:nanals,nob))*r_nanalsm1
#endif
! DFS is estimated increment normalized by spread, summed over all variables in column, for middle of window
                    gain = gain/(obsprd_prior(nob) + oberrvaruse(nob)/oberrfact)
                    gain = gain/statesprd_prior(i)
                    dfs(nobsl) = dfs(nobsl)+abs(gain*(ob(nob)-ensmean_ob(nob)))
                enddo
             endif
          enddo
          ! sort on DFS
          allocate(indxassim(nobsl))
          call quicksort(nobsl,dfs(1:nobsl),indxassim)
          nf = 0 ! results ordered by DFS, largest to smallest
          nobsl2 = min(nobsl_max,nobsl)
          do nob=nobsl,nobsl-nobsl2+1,-1
             nf = nf + 1
             sresults(nf)%dis = rloc(indxob(indxassim(nob)))
             sresults(nf)%idx = indxob(indxassim(nob))
             !if (nproc == 0 .and. npt == 1) &
             !print *,nf,sresults(nf)%idx,dfs(indxassim(nob)),sqrt(sresults(nf)%dis/corrlengthsq(sresults(nf)%idx)),obtype(sresults(nf)%idx)
          enddo
          deallocate(rloc,dfs,indxassim,indxob)
          nobsl = nobsl2
       else
          if (kdobs) then
             call kdtree2_n_nearest(tp=kdtree_obs2,qv=grdloc_chunk(:,npt),nn=nobsl_max,&
                  results=sresults)
             nobsl = nobsl_max
          else
             nobsl = 0
             do nob = 1, nobstot
                r = sum( (grdloc_chunk(:,npt)-obloc(:,nob))**2, 1)
                if (r < corrsq) then
                   nobsl = nobsl + 1
                   sresults(nobsl)%idx = nob
                   sresults(nobsl)%dis = r
                endif
             enddo
             nobsl_max = nobsl
          endif
       endif
   else ! find all obs within localization radius (sorted by distance).
       if (kdobs) then
         call kdtree2_r_nearest(tp=kdtree_obs2,qv=grdloc_chunk(:,npt),r2=corrsq,&
              nfound=nobsl,nalloc=nobstot,results=sresults)
       else
         nobsl = 0
         do nob = 1, nobstot
            r = sum( (grdloc_chunk(:,npt)-obloc(:,nob))**2, 1)
            if (r < corrsq) then
              nobsl = nobsl + 1
              sresults(nobsl)%idx = nob
              sresults(nobsl)%dis = r
            endif
         enddo
       endif
   endif

   t2 = t2 + mpi_wtime() - t1
   t1 = mpi_wtime()

   ! Skip when no observations in local area
   if(nobsl == 0) then
      if (allocated(sresults)) deallocate(sresults)
      if (allocated(ens_tmp)) deallocate(ens_tmp)
      cycle grdloop
   endif

   ! Loop through vertical levels (nnmax=1 if no vertical localization)
   verloop: do nn=1,nnmax

      ! Pick up variables passed to LETKF core process
      allocate(rloc(nobsl))
      allocate(oindex(nobsl))
      nobsl2=1
      do nob=1,nobsl
         nf = sresults(nob)%idx
         ! skip 'screened' obs.
         if (oberrvaruse(nf) > 1.e10_r_single) cycle
         if (vlocal) then
            vdist=(lnp_chunk(npt,nn)-oblnp(nf))/lnsigl(nf)
            if(abs(vdist) >= one) cycle
         else
            vdist = zero
         endif
         dist = sqrt(sresults(nob)%dis/corrlengthsq(sresults(nob)%idx)+vdist*vdist)
         if (dist >= one) cycle
         rloc(nobsl2)=taper(dist)
         oindex(nobsl2)=nf
         if(rloc(nobsl2) > eps) nobsl2=nobsl2+1
      end do
      nobsl2=nobsl2-1
      if (nobsl2 > nobslocal_max) nobslocal_max=nobsl2
      if (nobsl2 < nobslocal_min) nobslocal_min=nobsl2
      if(nobsl2 == 0) then
         deallocate(rloc,oindex)
         cycle verloop
      end if
      allocate(hxens(nens,nobsl2))
      allocate(obens(nanals,nobsl2))
      allocate(rdiag(nobsl2))
      allocate(dep(nobsl2))
      do nob=1,nobsl2
         nf=oindex(nob)
         if (neigv > 0) then
#ifdef MPI3
         hxens(1:nens,nob)=anal_ob_modens_fp(1:nens,nf) 
#else
         hxens(1:nens,nob)=anal_ob_modens(1:nens,nf) 
#endif
         else
#ifdef MPI3
         hxens(1:nens,nob)=anal_ob_fp(1:nens,nf) 
#else
         hxens(1:nens,nob)=anal_ob(1:nens,nf) 
#endif
         endif
         obens(1:nanals,nob) = &
#ifdef MPI3
         anal_ob_fp(1:nanals,nf) 
#else
         anal_ob(1:nanals,nf) 
#endif
         rdiag(nob)=one/oberrvaruse(nf)
         dep(nob)=ob(nf)-ensmean_ob(nf)
      end do
      deallocate(oindex)
      t3 = t3 + mpi_wtime() - t1
      t1 = mpi_wtime()
  
      ! use gain form of LETKF (to make modulated ensemble vertical localization
      ! possible)
      allocate(wts_ensperts(nens,nanals),wts_ensmean(nens))
      ! compute analysis weights for mean and ensemble perturbations given 
      ! ensemble in observation space, ob departures and ob errors.
      ! note: if modelspace_vloc=F, hxens and obens are identical (but hxens is
      ! is used as workspace and is modified on output), and analysis
      ! weights for ensemble perturbations represent posterior ens perturbations, not
      ! analysis increments for ensemble perturbations.
      call letkf_core(nobsl2,hxens,obens,dep,wts_ensmean,wts_ensperts,pa,&
                      rdiag,rloc(1:nobsl2),nens,nens/nanals,getkf_inflation,denkf,getkf)

      t4 = t4 + mpi_wtime() - t1
      t1 = mpi_wtime()

      ! Update analysis ensembles (all time levels)
      ! analysis increments represented as a linear combination
      ! of (modulated) prior ensemble perturbations.
      do nb=1,nbackgrounds
      do i=1,ncdim
         ! if not vlocal, update all state variables in column.
         if(vlocal .and. index_pres(i) /= nn) cycle
         ensmean_chunk(npt,i,nb) = ensmean_chunk(npt,i,nb) + &
         sum(wts_ensmean*ens_tmp(:,i,nb))
         if (getkf) then ! gain formulation
            do nanal=1,nanals 
               anal_chunk(nanal,npt,i,nb) = anal_chunk(nanal,npt,i,nb) + &
               sum(wts_ensperts(:,nanal)*ens_tmp(:,i,nb))
            enddo
            if (.not. denkf .and. getkf_inflation) then
               ! inflate posterior perturbations so analysis variance 
               ! in original low-rank ensemble is the same as modulated ensemble
               ! (eqn 30 in https://doi.org/10.1175/MWR-D-17-0102.1)
               trpa = 0.0_r_single
               do nanal=1,nens
                  trpa = trpa + &
                  sum(pa(:,nanal)*ens_tmp(:,i,nb))*ens_tmp(nanal,i,nb)
               enddo
               trpa = max(eps,trpa)
               trpa_raw = max(eps,r_nanalsm1*sum(anal_chunk(:,npt,i,nb)**2))
               anal_chunk(:,npt,i,nb) = sqrt(trpa/trpa_raw)*anal_chunk(:,npt,i,nb)
               !if (nproc == 0 .and. omp_get_thread_num() == 0 .and. i .eq. ncdim) print *,'i,trpa,trpa_raw,inflation = ',i,trpa,trpa_raw,sqrt(trpa/trpa_raw)
            endif
         else ! original LETKF formulation
            do nanal=1,nanals 
               anal_chunk(nanal,npt,i,nb) = &
               sum(wts_ensperts(:,nanal)*ens_tmp(:,i,nb))
            enddo
         endif
      enddo
      enddo
      deallocate(wts_ensperts,wts_ensmean,dep,obens,rloc,rdiag,hxens)
      if (getkf_inflation) deallocate(pa)

      t5 = t5 + mpi_wtime() - t1
      t1 = mpi_wtime()

   end do verloop

   if (allocated(sresults)) deallocate(sresults)
   if (allocated(ens_tmp)) deallocate(ens_tmp)
end do grdloop
!$omp end parallel do

if (allocated(statesprd_prior)) deallocate(statesprd_prior)

! make sure posterior perturbations still have zero mean.
! (roundoff errors can accumulate)
!$omp parallel do schedule(dynamic) private(npt,nb,i)
do npt=1,npts_max
   do nb=1,nbackgrounds
      do i=1,ncdim
         anal_chunk(1:nanals,npt,i,nb) = anal_chunk(1:nanals,npt,i,nb)-&
         sum(anal_chunk(1:nanals,npt,i,nb),1)*r_nanals
      end do
   end do
enddo
!$omp end parallel do

tend = mpi_wtime()
call mpi_reduce(tend-tbegin,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(tend-tbegin,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(tend-tbegin,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,'min/max/mean time to do letkf update ',tmin,tmax,tmean
t2 = t2/nthreads; t3 = t3/nthreads; t4 = t4/nthreads; t5 = t5/nthreads
if (nproc == 0) print *,'time to process analysis on gridpoint = ',t2,t3,t4,t5,' secs on task',nproc
call mpi_reduce(t2,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(t2,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(t2,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,',min/max/mean t2 = ',tmin,tmax,tmean
call mpi_reduce(t3,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(t3,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(t3,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,',min/max/mean t3 = ',tmin,tmax,tmean
call mpi_reduce(t4,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(t4,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(t4,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,',min/max/mean t4 = ',tmin,tmax,tmean
call mpi_reduce(t5,tmean,1,mpi_real8,mpi_sum,0,mpi_comm_world,ierr)
tmean = tmean/numproc
call mpi_reduce(t5,tmin,1,mpi_real8,mpi_min,0,mpi_comm_world,ierr)
call mpi_reduce(t5,tmax,1,mpi_real8,mpi_max,0,mpi_comm_world,ierr)
if (nproc .eq. 0) print *,',min/max/mean t5 = ',tmin,tmax,tmean
call mpi_reduce(nobslocal_max,nobslocal_maxall,1,mpi_integer,mpi_max,0,mpi_comm_world,ierr)
call mpi_reduce(nobslocal_min,nobslocal_minall,1,mpi_integer,mpi_max,0,mpi_comm_world,ierr)
if (nproc == 0) print *,'min/max number of obs in local volume',nobslocal_minall,nobslocal_maxall
if (nrej > 0 .and. nproc == 0) print *, nrej,' obs rejected by varqc'
  
! free shared memory segement, fortran pointer to that memory.
#ifdef MPI3
nullify(anal_ob_fp)
call MPI_Win_free(shm_win, ierr)
if (neigv > 0) then
   nullify(anal_ob_modens_fp)
   call MPI_Win_free(shm_win2, ierr)
endif
#endif
! deallocate anal_ob on non-root tasks.
if (nproc .ne. 0 .and. allocated(anal_ob)) deallocate(anal_ob)
if (nproc .ne. 0 .and. allocated(anal_ob_modens)) deallocate(anal_ob_modens)
if (allocated(ens_tmp)) deallocate(ens_tmp)

return

end subroutine letkf_update

subroutine letkf_core(nobsl,hxens,hxens_orig,dep,wts_ensmean,wts_ensperts,paens,&
                      rdiaginv,rloc,nanals,neigv,getkf_inflation,denkf,getkf)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    letkf_core
!
!   prgmmr: whitaker
!
! abstract:  LETKF core subroutine. Returns analysis weights
! for ensemble mean update and ensemble perturbation update (increments
! represented as a linear combination of prior ensemble perturbations).
! Uses 'gain form' LETKF, which works when ensemble used to estimate
! covariances not the same as ensemble being updated.  If neigv=1
! (no modulated ensemble model-space localization), then the 
! traditional form of the LETKF analysis weights for ensemble
! perturbations are returned (which represents posterior
! ensemble perturbations, not analysis increments, as a linear
! combination of prior ensemble perturbations).
!
! program history log:
!   2011-06-03  ota: created from miyoshi's LETKF core subroutine
!   2014-06-20  whitaker: Use LAPACK routine dsyev for eigenanalysis.
!   2016-02-01  whitaker: Use LAPACK dsyevr for eigenanalysis (faster
!               than dsyev in most cases). 
!   2018-07-01  whitaker: implement gain form of LETKF from Bishop et al 2017
!   (https://doi.org/10.1175/MWR-D-17-0102.1), allow for use of modulated 
!   ensemble vert localization (ensemble used to estimate posterior covariance
!   in ensemble space different than ensemble being updated). Add denkf,
!   getkf,getkf_inflation options.
!
!   input argument list:
!     nobsl    - number of observations in the local patch
!     hxens    - on input: first-guess modulated ensemble in observation space (Yb)
!                on output: overwritten with Yb * R**-1.
!     hxens_orig  - first-guess original ensembles in observation space.
!                   not used if neigv=1. 
!     dep      - nobsl observation departures (y-Hxmean)
!     rdiaginv - inverse of diagonal element of observation error covariance
!     rloc     - localization function for each ob (based on distance to
!                analysis point)
!     nanals   - number of ensemble members (1st dimension of hxens)
!     neigv    - for modulated ensemble model-space localization, number
!                of eigenvectors of vertical localization (1 if not using
!                model space localization).  1st dimension of hxens_orig is 
!                nanals/neigv.
!     getkf_inflation - if true (and getkf=T,denkf=F), 
!                return posterior covariance matrix in
!                needed to compute getkf inflation (eqn 30 in Bishop et al
!                2017).
!     denkf - if true, use DEnKF approximation (implies getkf=T)
!             See Sakov and Oke 2008 https://doi.org/10.1111/j.1600-0870.2007.00299.x
!     getkf - if true, use gain formulation
!
!   output argument list:
!
!     wts_ensmean - Factor used to compute ens mean analysis increment
!       by pre-multiplying with
!       model space ensemble perts. In notation from Bishop et al 2017,
!       wts_ensmean = C (Gamma + I)**-1 C^T (HZ)^ T R**-1/2 (y - Hxmean)
!       where HZ^T = Yb*R**-1/2 (YbRinvsqrt),
!       C are eigenvectors of (HZ)^T HZ and Gamma are eigenvalues
!       Has dimension (nanals) - increment is weighted average of ens
!       perts, wts_ensmean are weights. 
!
!     wts_ensperts  - if getkf=T same as above, but for computing increments to 
!       ensemble perturbations. From Bishop et al 2017 eqn 29
!       wts_ensperts = -C [ (I - (Gamma+I)**-1/2)*Gamma**-1 ] C^T (HZ)^T R**-1/2 Hxprime
!       Has dimension (nanals,nanals/neigv), analysis weights for each
!       member. Hxprime (hxens_orig) is the original, unmodulated
!       ensemble in observation space, HZ is the modulated ensemble in
!       ob space times R**-1/2. If denkf=T, wts_ensperts is approximated
!       as wts_ensperts = -0.5*C (Gamma + I)**-1 C^T (HZ)^ T R**-1/2 Hxprime
!       If getkf=F and denkf=F, then the original LETKF formulation is used with
!       wts_ensperts =
!       C (Gamma + I)**-1/2 C^T (square root of analysis error cov in ensemble space)
!       and these weights are applied to transform the background ensemble into an
!       analysis ensemble.  Note that modulated ensemble vertical localization
!       requires the gain form (getkf=T and/or denkf=T) since this form of the weights
!       requires that the background ensemble used to compute covariances is
!       the same ensemble being updated.
!
!     pa - only allocated and returned
!       if getkf_inflation=T (and denkf=F).  In this case
!       pa is allocated dimension (nanals,nanals) and contains posterior 
!       covariance matrix in (modulated) ensemble space.
!
! attributes:
!   language:  f95
!   machine:
!
!$$$ end documentation block

implicit none
integer(i_kind), intent(in) :: nobsl,nanals,neigv
real(r_kind),dimension(nobsl),intent(in ) :: rdiaginv,rloc
real(r_kind),dimension(nanals,nobsl),intent(inout)  :: hxens
real(r_single),dimension(nanals/neigv,nobsl),intent(in)  :: hxens_orig
real(r_single),dimension(nobsl),intent(in)  :: dep
real(r_single),dimension(nanals),intent(out)  :: wts_ensmean
real(r_single),dimension(nanals,nanals/neigv),intent(out)  :: wts_ensperts
real(r_single),dimension(:,:),allocatable, intent(inout) :: paens
! local variables.
real(r_kind),allocatable,dimension(:,:) :: work3,evecs
real(r_single),allocatable,dimension(:,:) :: swork2,pa,swork3,shxens
real(r_single),allocatable,dimension(:) :: swork1
real(r_kind),allocatable,dimension(:) :: rrloc,evals,gammapI,gamma_inv
real(r_kind) eps
integer(i_kind) :: nanal,ierr,lwork,liwork
!for LAPACK dsyevr
integer(i_kind) isuppz(2*nanals)
real(r_kind) vl,vu,normfact
integer(i_kind), allocatable, dimension(:) :: iwork
real(r_kind), dimension(:), allocatable :: work1
logical, intent(in) :: getkf_inflation,denkf,getkf

if (neigv < 1) then
  print *,'neigv must be >=1 in letkf_core'
  call stop2(992)
endif

allocate(work3(nanals,nanals),evecs(nanals,nanals))
allocate(rrloc(nobsl),gammapI(nanals),evals(nanals),gamma_inv(nanals))
! for dsyevr
allocate(iwork(10*nanals),work1(70*nanals))
! for dsyevd
!allocate(iwork(3+5*nanals),work1(1+6*nanals+2*nanals*nanals))

! HZ^T = hxens sqrt(Rinv)
rrloc = rdiaginv * rloc
eps = epsilon(0.0_r_single)
where (rrloc < eps) rrloc = eps
rrloc = sqrt(rrloc)
normfact = sqrt(real((nanals/neigv)-1,r_kind))
! normalize so dot product is covariance
do nanal=1,nanals
   hxens(nanal,1:nobsl) = hxens(nanal,1:nobsl) * &
   rrloc(1:nobsl)/normfact
end do

! compute eigenvectors/eigenvalues of HZ^T HZ (left SV)
! (in Bishop paper HZ is nobsl, nanals, here is it nanals, nobsl)
lwork = size(work1); liwork = size(iwork)
if(r_kind == kind(1.d0)) then ! double precision 
   !work3 = matmul(hxens,transpose(hxens))
   call dgemm('n','t',nanals,nanals,nobsl,1.d0,hxens,nanals, &
               hxens,nanals,0.d0,work3,nanals)
   ! evecs contains eigenvectors of HZ^T HZ, or left singular vectors of HZ
   ! evals contains eigenvalues (singular values squared)
   call dsyevr('V','A','L',nanals,work3,nanals,vl,vu,1,nanals,-1.d0,nanals,evals,evecs, &
               nanals,isuppz,work1,lwork,iwork,liwork,ierr)
! use LAPACK dsyevd instead of dsyevr
   !evecs = work3
   !call dsyevd('V','L',nanals,evecs,nanals,evals,work1,lwork,iwork,liwork,ierr)
else ! single precision
   call sgemm('n','t',nanals,nanals,nobsl,1.e0,hxens,nanals, &
               hxens,nanals,0.e0,work3,nanals)
   call ssyevr('V','A','L',nanals,work3,nanals,vl,vu,1,nanals,-1.e0,nanals,evals,evecs, &
               nanals,isuppz,work1,lwork,iwork,liwork,ierr)
! use LAPACK dsyevd instead of dsyevr
   !evecs = work3
   !call ssyevd('V','L',nanals,evecs,nanals,evals,work1,lwork,iwork,liwork,ierr)
end if
if (ierr .ne. 0) print *,'warning: dsyev* failed, ierr=',ierr
deallocate(work1,iwork,work3) ! no longer needed
gamma_inv = 0.0_r_kind
do nanal=1,nanals
   if (evals(nanal) > eps) then
       gamma_inv(nanal) = 1./evals(nanal)
   else
       evals(nanal) = 0.0_r_kind
   endif
enddo
! gammapI used in calculation of posterior cov in ensemble space
gammapI = evals+1.0
deallocate(evals)

! create HZ^T R**-1/2 
allocate(shxens(nanals,nobsl))
do nanal=1,nanals
   shxens(nanal,1:nobsl) = hxens(nanal,1:nobsl) * rrloc(1:nobsl)
end do
deallocate(rrloc)

! compute factor to multiply with model space ensemble perturbations
! to compute analysis increment (for mean update), save in single precision.
! This is the factor C (Gamma + I)**-1 C^T (HZ)^ T R**-1/2 (y - HXmean)
! in Bishop paper (eqs 10-12).

allocate(swork3(nanals,nanals),swork2(nanals,nanals),pa(nanals,nanals))
do nanal=1,nanals
   swork3(nanal,:) = evecs(nanal,:)/gammapI
   swork2(nanal,:) = evecs(nanal,:)
enddo

! pa = C (Gamma + I)**-1 C^T (analysis error cov in ensemble space)
!pa = matmul(swork3,transpose(swork2))
call sgemm('n','t',nanals,nanals,nanals,1.e0,swork3,nanals,swork2,&
            nanals,0.e0,pa,nanals)
! work1 = (HZ)^ T R**-1/2 (y - HXmean)
! (nanals, nobsl) x (nobsl,) = (nanals,)
! in Bishop paper HZ is nobsl, nanals, here is it nanals, nobsl
allocate(swork1(nanals))
do nanal=1,nanals
   swork1(nanal) = sum(shxens(nanal,:)*dep(:))
end do
! wts_ensmean = C (Gamma + I)**-1 C^T (HZ)^ T R**-1/2 (y - HXmean)
! (nanals, nanals) x (nanals,) = (nanals,)
do nanal=1,nanals
   wts_ensmean(nanal) = sum(pa(nanal,:)*swork1(:))/normfact
end do

if (.not. denkf .and. getkf_inflation) then
   allocate(paens(nanals,nanals))
   paens = pa/normfact**2
endif
deallocate(swork1)

! compute factor to multiply with model space ensemble perturbations
! to compute analysis increment (for perturbation update), save in single precision.
! This is -C [ (I - (Gamma+I)**-1/2)*Gamma**-1 ] C^T (HZ)^T R**-1/2 HXprime
! in Bishop paper (eqn 29).
! For DEnKF factor is -0.5*C (Gamma + I)**-1 C^T (HZ)^ T R**-1/2 HXprime
! = -0.5 Pa (HZ)^ T R**-1/2 HXprime (Pa already computed)

if (getkf .or. denkf) then ! use Gain formulation for LETKF weights

if (denkf) then
   ! use Pa = C (Gamma + I)**-1 C^T (already computed)
   ! wts_ensperts = -0.5 Pa (HZ)^ T R**-1/2 HXprime
   pa = 0.5*pa
else
   gammapI = sqrt(1.0/gammapI)
   do nanal=1,nanals
      swork3(nanal,:) = &
      evecs(nanal,:)*(1.-gammapI(:))*gamma_inv(:)
   enddo
   ! swork2 still contains eigenvectors, over-write pa
   ! pa = C [ (I - (Gamma+I)**-1/2)*Gamma**-1 ] C^T
   !pa = matmul(swork3,transpose(swork2))
   call sgemm('n','t',nanals,nanals,nanals,1.e0,swork3,nanals,swork2,&
               nanals,0.e0,pa,nanals)
endif
deallocate(swork2,swork3)

! work2 = (HZ)^ T R**-1/2 HXprime
! (nanals, nobsl) x (nobsl, nanals/neigv) = (nanals, nanals/neigv)
! in Bishop paper HZ is nobsl, nanals, here is it nanals, nobsl
! HXprime in paper is nobsl, nanals/neigv here it is nanals/neigv, nobsl
allocate(swork2(nanals,nanals/neigv))
!swork2 = matmul(shxens,transpose(hxens_orig))
call sgemm('n','t',nanals,nanals/neigv,nobsl,1.e0,&
            shxens,nanals,hxens_orig,nanals/neigv,0.e0,swork2,nanals)
! wts_ensperts = -C [ (I - (Gamma+I)**-1/2)*Gamma**-1 ] C^T (HZ)^T R**-1/2 HXprime
! (nanals, nanals) x (nanals, nanals/eigv) = (nanals, nanals/neigv)
! if denkf, wts_ensperts = -0.5 C (Gamma + I)**-1 C^T (HZ)^T R**-1/2 HXprime
!wts_ensperts = -matmul(pa, swork2)/normfact
call sgemm('n','n',nanals,nanals/neigv,nanals,-1.e0,&
            pa,nanals,swork2,nanals,0.e0,wts_ensperts,nanals)
wts_ensperts = wts_ensperts/normfact

! clean up
deallocate(shxens,swork2,pa)

else  ! use original LETKF formulation (won't work if neigv != 1)

if (neigv > 1) then
  print *,'neigv must be 1 in letkf_core if getkf=F'
  call stop2(993)
endif
! compute sqrt(Pa) - analysis weights
! (apply to prior ensemble to determine posterior ensemble,
!  not analysis increments as in Gain formulation)
! hxens_orig not used
! saves two matrix multiplications (nanals, nobsl) x (nobsl, nanals) and
! (nanals, nanals) x (nanals, nanals)
deallocate(shxens,pa)
gammapI = sqrt(1.0/gammapI)
do nanal=1,nanals
   swork3(nanal,:) = evecs(nanal,:)*gammapI
enddo
! swork2 already contains evecs
! wts_ensperts = 
! C (Gamma + I)**-1/2 C^T (square root of analysis error cov in ensemble space)
!wts_ensperts = matmul(swork3,transpose(swork2))
call sgemm('n','t',nanals,nanals,nanals,1.0,swork3,nanals,swork2,&
            nanals,0.e0,wts_ensperts,nanals)
deallocate(swork3,swork2)

endif

deallocate(evecs,gammapI,gamma_inv)

return
end subroutine letkf_core

end module letkf
