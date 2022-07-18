module efsoi
!$$$  module documentation block
!
! module: efsoi                        Update observation impact estimates
!                                      with the EnSRF framework.
!
! prgmmr: ota              org: np23                   date: 2011-12-01
! prgmmr: groff            org: emc                    date: 2018-05-24
!
! abstract:
!  Computes the observation impact estimates using the EnKF analysis products.
!  Formulation is based on Kalnay et al (2012, Tellus A, submitted).
!  Parallel processing is following the way of the serial EnSRF.
!
! Public Subroutines:
!  efsoi_update: performs the Forecast Sensitivity to Observations update within
!              the EnSRF framework. The EnKF other than the serial EnSRF 
!              (e.g. the LETKF) can be also used here (the method is 
!              independent on the type of EnKF).
!
! Public Variables: None
!
! Modules Used: kinds, constants, params, covlocal, mpisetup, loadbal, statevec,
!               kdtree2_module, obsmod, gridinfo, obs_sensitivity
!
! program history log:
!   2011-12-01  Created from the LETKF core module.
!   2012-04-04  Changed to EnSRF framework for saving memory
!   2018-05-24  Renamed module added non-bias corrected EFSOI
!
! attributes:
!   language: f95
!
!$$$

use mpi
use mpisetup, only: nproc, numproc, mpi_realkind
use covlocal, only:  taper
use kinds, only: r_single, r_double, i_kind, r_kind
use kdtree2_module, only: kdtree2_r_nearest, kdtree2_result
use loadbal_efsoi, only: numptsperproc, indxproc, lnp_chunk, kdtree_grid, &
                   iprocob, indxob_chunk, anal_obchunk_prior, numobsperproc, &
                   indxproc_obs, nobs_max
use scatter_chunks_efsoi, only: fcerror_chunk, anal_chunk
use enkf_obsmod, only: oberrvar, ob, ensmean_ob, obloc, obloclon, obloclat, oblnp, &
                       obtime, nobstot, corrlengthsq, lnsigl, obtimel, anal_ob_post
use constants, only: constants_initialized, pi, zero, one
use params, only: nanals, corrlengthnh, corrlengthtr, corrlengthsh, &
                  tar_minlon, tar_maxlon, tar_minlat, tar_maxlat, &
                  tar_minlev, tar_maxlev
use gridinfo, only: nlevs_pres, lonsgrd, latsgrd
use statevec_efsoi, only: id_u, id_v, id_t, id_q, id_ps
use enkf_obs_sensitivity, only: obsense_kin, obsense_dry, obsense_moist, adloc_chunk

implicit none

private
public :: efsoi_update

contains

subroutine efsoi_update()
implicit none
real(r_kind),allocatable,dimension(:) :: djdy_kin, djdy_dry, djdy_moist
real(r_kind),allocatable,dimension(:) :: uvwork, tpwork, qwork
real(r_kind),allocatable,dimension(:) :: taper_disgrd
real(r_kind),dimension(nobstot) :: obdep, oberinv
real(r_single),dimension(nobstot) :: invobtimel, invlnsigl
real(r_single),dimension(nobstot) :: invcorlen
real(r_kind),allocatable,dimension(:,:) :: buffertmp
type(kdtree2_result),allocatable,dimension(:) :: sresults1
real(r_kind) :: anal_obtmp(nanals)
real(r_kind) :: r,taper1,taper3,taperv
real(r_single) :: lnsig
real(r_double) :: t1, t2, t3, t4, tbegin, tend
integer(i_kind) :: np, nob, nob1, nob2, nobm, npob, nf2, i, ii, npt, nanal, nn
integer(i_kind) :: nnpt, nsame, nskip, ngrd1
integer(i_kind) :: ierr
logical :: kdgrid
integer status(MPI_STATUS_SIZE)

if (.not. constants_initialized) then
    print *,'constants not initialized (with init_constants, init_constants_derived)'
    call stop2(28)
end if

allocate(sresults1(nlevs_pres*numptsperproc(nproc+1)))
allocate(taper_disgrd(nlevs_pres*numptsperproc(nproc+1)))

! Compute the inverse of cut-off length and 
! the observation departure from first guess
!$omp parallel do private(nob)
do nob=1,nobstot
   invcorlen(nob) = one/corrlengthsq(nob)
   invlnsigl(nob) = one/lnsigl(nob)
   invobtimel(nob)= one/obtimel(nob)
   oberinv(nob)   = one/oberrvar(nob)
   obdep(nob)     = ob(nob)-ensmean_ob(nob)
end do


kdgrid=associated(kdtree_grid)
allocate(djdy_kin(nobstot))
allocate(djdy_dry(nobstot))
allocate(djdy_moist(nobstot))
djdy_kin(1:nobstot) = zero
djdy_dry(1:nobstot) = zero
djdy_moist(1:nobstot) = zero
allocate(uvwork(nanals))
allocate(tpwork(nanals))
allocate(qwork(nanals))

nsame = 0
nskip = 0
nobm = 1
t2 = zero
t3 = zero
t4 = zero
tbegin = mpi_wtime()

! 
! Loop for each observations
obsloop: do nob=1,nobstot


   if (nproc == 0 .and. modulo(nob , 10000) == 0 ) print *,'doing nob=',nob

   t1 = mpi_wtime()

   if(oberrvar(nob) > 1.e10_r_kind .or. abs(obtime(nob)) >= obtimel(nob))then
      nskip = nskip + 1
      cycle obsloop
   end if

   ! what processor is this ob on?
   npob = iprocob(nob)

   ! get anal_ob from that processor and send to other processors.
   if (nproc == npob) then
      nob1 = indxob_chunk(nob)
      anal_obtmp(1:nanals) = anal_obchunk_prior(:,nob1)
   end if
 
   call mpi_bcast(anal_obtmp,nanals,mpi_realkind,npob,mpi_comm_world,ierr)
   
   anal_obtmp(1:nanals) = anal_obtmp(1:nanals) * oberinv(nob)

   t2 = t2 + mpi_wtime() - t1
   t1 = mpi_wtime()





   ! ============================================================  
   ! Only need to recalculate nearest points when lat/lon is different
   ! ============================================================  
   if(nob == 1 .or. &
         abs(obloclat(nob)-obloclat(nobm)) .gt. tiny(obloclat(nob)) .or. &
         abs(obloclon(nob)-obloclon(nobm)) .gt. tiny(obloclon(nob)) .or. &
         abs(corrlengthsq(nob)-corrlengthsq(nobm)) .gt. tiny(corrlengthsq(nob))) then
		 
      nobm=nob
      ! determine localization length scales based on latitude of ob.
      
      nf2=0
      ! search analysis grid points for those within corrlength of 
      ! ob being assimilated (using a kd-tree for speed).
      if (kdgrid) then
         call kdtree2_r_nearest(tp=kdtree_grid,qv=obloc(:,nob), &
              r2=corrlengthsq(nob), &
              nfound=nf2,nalloc=nlevs_pres*numptsperproc(nproc+1), &
              results=sresults1)
      else
         ! use brute force search if number of grid points on this proc <= 3
         do nn=1,nlevs_pres
            do npt=1,numptsperproc(nproc+1)
               nnpt = nlevs_pres * (npt-1) + nn
               r = sum( (obloc(:,nob)-adloc_chunk(:,nnpt))**2, 1 )
               if (r < corrlengthsq(nob)) then
                  nf2 = nf2 + 1
                  sresults1(nf2)%idx = nnpt
                  sresults1(nf2)%dis = r
               end if
            end do
         end do
      end if
	  
	  !$omp parallel do private(nob1)
      do nob1=1,nf2
         taper_disgrd(nob1) = taper(sqrt(sresults1(nob1)%dis*invcorlen(nob)))
      end do
	  
   else
      nsame=nsame+1
   end if

   t3 = t3 + mpi_wtime() - t1
   t1 = mpi_wtime()

   ! ============================================================  
   ! forecast error sensitivity to the observations
   ! ============================================================  
   if (nf2 > 0) then
	   
      taper3=taper(obtime(nob)*invobtimel(nob))
	  
      uvwork(1:nanals) = zero
      tpwork(1:nanals) = zero
      qwork(1:nanals)  = zero
	  
      ! rho * X_f^T (e_t|0 + e_t|-6) / 2
      ! contributions from (U,V), (T,Ps) and Q are computed separately
      
      do ii=1,nf2
         taper1=taper_disgrd(ii)*taper3
		 
         i = (sresults1(ii)%idx-1)/nlevs_pres+1
		 
         ngrd1=indxproc(nproc+1,i)
		 
         if(tar_minlon <= tar_maxlon .and. &
              & (lonsgrd(ngrd1) < tar_minlon .or. lonsgrd(ngrd1) > tar_maxlon .or. &
              & latsgrd(ngrd1) < tar_minlat .or. latsgrd(ngrd1) > tar_maxlat)) &
              & cycle
			  
         if(tar_minlon > tar_maxlon .and. &
              & ((lonsgrd(ngrd1) < tar_minlon .and. lonsgrd(ngrd1) > tar_maxlon) .or. &
              & latsgrd(ngrd1) < tar_minlat .or. latsgrd(ngrd1) > tar_maxlat)) &
              & cycle
			  
         nn = sresults1(ii)%idx - (i-1)*nlevs_pres
		 
         if((tar_minlev /= 1 .or. nn /= nlevs_pres) &
              & .and. (nn > tar_maxlev .or. nn < tar_minlev)) cycle
         lnsig = abs(lnp_chunk(i,nn)-oblnp(nob))
		 
         if(lnsig < lnsigl(nob))then
            taperv=taper1*taper(lnsig*invlnsigl(nob))
         else
            cycle
         end if
		 
         if(nn == nlevs_pres) then
            do nanal=1,nanals
               tpwork(nanal) = tpwork(nanal) + taperv &
                    & * anal_chunk(nanal,i,id_ps) * fcerror_chunk(i,id_ps)
            end do
            cycle
         end if


         ! 		 
         do nanal=1,nanals
            uvwork(nanal) = uvwork(nanal) + taperv &
                 & * (anal_chunk(nanal,i,id_u(nn)) * fcerror_chunk(i,id_u(nn)) &
                 & + anal_chunk(nanal,i,id_v(nn)) * fcerror_chunk(i,id_v(nn)))
				 
            tpwork(nanal) = tpwork(nanal) + taperv &
                 & * anal_chunk(nanal,i,id_t(nn)) * fcerror_chunk(i,id_t(nn))
				 
            if(id_q(nn) > 0) qwork(nanal) = qwork(nanal) + taperv &
                 & * anal_chunk(nanal,i,id_q(nn)) * fcerror_chunk(i,id_q(nn))
         end do
		 
      end do
	  
      ! R^-1 HX_a [rho * X_f^T (e_t|0 + e_t|-6) / 2]
      do nanal=1,nanals
         djdy_kin(nob) 		= djdy_kin(nob)   + anal_obtmp(nanal) * uvwork(nanal)
         djdy_dry(nob) 		= djdy_dry(nob)   + anal_obtmp(nanal) * tpwork(nanal)
         djdy_moist(nob) 	= djdy_moist(nob) + anal_obtmp(nanal) * qwork(nanal)
      end do
	  
   end if ! end of, if (nf2 > 0) then
  
   
   t4 = t4 + mpi_wtime() - t1
   t1 = mpi_wtime()

end do obsloop

tend = mpi_wtime()
if (nproc == 0 .or. nproc == numproc-1) print *,'time to process FSO on gridpoint = ',tend-tbegin,t2,t3,t4,' secs'

if (nproc == 0 .and. nskip > 0) print *,nskip,' out of',nobstot,'obs skipped'
if (nproc == 0 .and. nsame > 0) print *,nsame,' out of', nobstot-nskip,' same lat/lon'

! Observation sensitivity post process
!$omp parallel do private(nob)
do nob=1,nobstot
   djdy_dry(nob) 		= djdy_dry(nob)   + djdy_kin(nob)
   djdy_moist(nob) 		= djdy_moist(nob) + djdy_dry(nob)
   obsense_kin(nob) 	= djdy_kin(nob)   * obdep(nob)
   obsense_dry(nob) 	= djdy_dry(nob)   * obdep(nob)
   obsense_moist(nob) 	= djdy_moist(nob) * obdep(nob)
end do

! Gathering analysis perturbations projected on the observation space
if(nproc /= 0) then
  call mpi_send(anal_obchunk_prior,numobsperproc(nproc+1)*nanals,mpi_real4,0, &
       1,mpi_comm_world,ierr)
else
!   allocate(anal_ob_post(1:nanals,nobstot))
   allocate(buffertmp(nanals,nobs_max))
   do np=1,numproc-1
      call mpi_recv(buffertmp,numobsperproc(np+1)*nanals,mpi_real4,np, &
           1,mpi_comm_world,status,ierr)
      do nob1=1,numobsperproc(np+1)
         nob2 = indxproc_obs(np+1,nob1)
         anal_ob_post(:,nob2) = buffertmp(:,nob1)
      end do
   end do
   do nob1=1,numobsperproc(1)
      nob2 = indxproc_obs(1,nob1)
      anal_ob_post(:,nob2) = anal_obchunk_prior(:,nob1)
   end do
   deallocate(buffertmp)
end if

t1 = mpi_wtime() - tend
if (nproc == 0) print *,'time to observation sensitivity post process = ',t1,' secs'

deallocate(anal_obchunk_prior)
deallocate(sresults1)
deallocate(djdy_kin,djdy_dry,djdy_moist,taper_disgrd,uvwork,tpwork,qwork)

return
end subroutine efsoi_update
end module efsoi
