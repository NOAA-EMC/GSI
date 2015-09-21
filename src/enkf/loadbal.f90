module loadbal
!$$$  module documentation block
!
! module: loadbal          decompose ob priors and horizontal grid points
!                          to minimize load imbalance. Creates various
!                          arrays that define the decomposition.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: 
!
! Public Subroutines:
!  load_balance: set up decomposition (for ob priors and analysis grid points)
!   that minimizes load imbalance.  
!   The decomposition uses "Graham's rule", which simply
!   stated, assigns each new work item to the task that currently has the 
!   smallest load.
!  loadbal_cleanup: deallocate allocated arrays.
!
! Private Subroutines:
!  estimate_work_enkf1: estimate work needed to update each analysis grid
!   point (considering all the observations within the localization radius).
!  estimate_work_enkf2: estimate work needed to update each observation prior
!   (considering all the observations within the localization radius of each
!    observation). 
! Public Variables (all defined by subroutine load_balance):
!  npts_min: (integer scalar) smallest number of grid points assigned to a task.
!  npts_max: (integer scalar) maximum number of grid points assigned to a task.
!  nobs_min: (integer scalar) smallest number of observation priors assigned to a task.
!  nobs_max: (integer scalar) maximum number of observation priors assigned to a task.
!  numproc: (integer scalar) total number of MPI tasks (from module mpisetup)
!  nobsgood: (integer scalar) total number of obs to be assimilated (from module
!   enkf_obsmod).
!  numobsperproc(numproc): integer array containing # of ob priors assigned to
!   each task.
!  numptsperproc(numproc): integer array containing # of grid points assigned to
!   each task.
!  indxproc(numproc,npts_max): integer array with the indices (1,2,...npts) of 
!   analysis grid points assigned to each task.
!  indxproc_obs(numproc,nobs_max): integer array with the indices (1,2,...nobsgood) of 
!   observation priors assigned to that task.
!  iprocob(nobsgood): integer array containing the task number that has been
!   assigned to update each observation prior.
!  indxob_chunk(nobsgood): integer array that maps the index of the ob priors
!   being assimilated (1,2,3...nobsgood) to the index of the obs being 
!   updated on that task (1,2,3,...numobsperproc(nproc)) - inverse of
!   indxproc_obs.
!  ensmean_obchunk(nobs_max): real array of ensemble mean observation priors
!   being updated on that task (use indxproc_obs to find
!   corresponding index in ensemble_ob).
!  obloc_chunk(3,nobs_max): real array of spherical cartesian coordinates
!   of ob priors being updated on this task.
!  grdloc_chunk(3,npts_max): real array of spherical cartesian coordinates
!   of analysis grid points being updated on this task.
!  lnp_chunk(npts_max,ndim): real array of log(pressures) of state variables
!   being updated on this task.
!  oblnp_chunk(nobs_max,ndim): real array of log(pressures) of ob priors
!   being updated on this task.
!  obtime_chunk(nobs_max): real array of ob times of ob priors
!   being updated on this task (expressed as an offset from the analysis time in
!   hours).
!  anal_obchunk_prior(nanals,nobs_max): real array of observation prior 
!   ensemble perturbations to be updated on this task.
!  kdtree_grid: pointer to kd-tree structure used for nearest neighbor searches
!   in grid point space (only searches grid points assigned to this task).
!  kdtree_obs: pointer to kd-tree structure used for nearest neighbor searches
!   in observation space (only searches ob locations assigned to this task).
!   
!
! Modules Used: mpisetup, params, kinds, constants, enkf_obsmod, gridinfo,
!               kdtree_module, covlocal
!
! program history log:
!   2009-02-23  Initial version.
!   2011-06-21  Added the option of observation box selection for LETKF.
!
! attributes:
!   language: f95
!
!$$$

use mpisetup
use params, only: ndim, datapath, nanals, simple_partition, letkf_flag
use enkf_obsmod, only: nobsgood, obloc, oblnp, ensmean_ob, obtime, anal_ob, corrlengthsq, nobstot, &
     boxmax, nlocconv, nlocoz, nlocsat, blatnum, blonnum, nobs_conv, nobs_oz
use kinds, only: r_kind, i_kind, r_double, r_single
use kdtree2_module, only: kdtree2, kdtree2_create, kdtree2_destroy, &
                          kdtree2_result, kdtree2_r_nearest
use gridinfo, only: gridloc, logp, latsgrd, nlevs_pres, npts

implicit none
private
public :: load_balance, loadbal_cleanup

real(r_single),public, allocatable, dimension(:,:) :: lnp_chunk, &
                                                    anal_obchunk_prior
! arrays passed to kdtree2 routines need to be single
real(r_single),public, allocatable, dimension(:,:) :: obloc_chunk, grdloc_chunk
real(r_single),public, allocatable, dimension(:) :: oblnp_chunk, &
 obtime_chunk, ensmean_obchunk
integer(i_kind),public, allocatable, dimension(:) :: iprocob, indxob_chunk,&
                          numptsperproc, numobsperproc
integer(i_kind),public, allocatable, dimension(:,:) :: indxproc, indxproc_obs
integer(i_kind),public :: npts_min, npts_max, nobs_min, nobs_max, numobsmax1, &
 numobsmax2
! kd-tree structures.
type(kdtree2),public,pointer :: kdtree_obs, kdtree_grid


contains

subroutine load_balance()
use random_normal, only : set_random_seed
! set up decomposition (for ob priors and analysis grid points)
! that minimizes load imbalance. 
! Uses "Graham's rule", which simply
! stated, assigns each new work item to the task that currently has the 
! smallest load.
implicit none
integer(i_kind), allocatable, dimension(:) :: rtmp,numobs, numobs1
integer(i_kind) np,nob,i,j,n,nn,nob1,nob2,ierr,ib1,ib2,ib3,ie1,ie2,ie3
real(r_double) t1

! partition state vector for enkf using Grahams rule..
! ("When a new job arrives, allocate it to the server 
! that currently has the smallest load")
allocate(numobs(npts),numobs1(npts))
allocate(numobsperproc(numproc))
allocate(numptsperproc(numproc))
allocate(rtmp(numproc))
t1 = mpi_wtime()
call estimate_work_enkf1(numobs) ! fill numobs array with number of obs per horiz point
! distribute the results of estimate_work to all processors.
call mpi_allreduce(numobs,numobs1,npts,mpi_integer,mpi_sum,mpi_comm_world,ierr)
if (nproc == 0) print *,'time in estimate_work_enkf1 = ',mpi_wtime()-t1,' secs'
! loop over horizontal grid points on analysis grid.
t1 = mpi_wtime()
rtmp = 0
numptsperproc = 0
do n=1,npts
   np = minloc(rtmp,dim=1)
   ! np is processor with the fewest number of obs to process
   ! add this grid point to list for nmin
   rtmp(np) = rtmp(np)+numobs1(n)
   numptsperproc(np) = numptsperproc(np)+1
end do
npts_max = maxval(numptsperproc)
npts_min = minval(numptsperproc)
allocate(indxproc(numproc,npts_max))
! indxproc(np,i) is i'th horiz grid index for processor np.
! there are numptsperpoc(np) i values for processor np
rtmp = 0
numptsperproc = 0
do n=1,npts
   np = minloc(rtmp,dim=1)
   rtmp(np) = rtmp(np)+numobs1(n)
   numptsperproc(np) = numptsperproc(np)+1 ! recalculate
   indxproc(np,numptsperproc(np)) = n
end do
if (nproc == 0) then
    print *,'npts = ',npts
    print *,'min/max number of points per proc = ',npts_min,npts_max
    print *,'time to do model space decomp = ',mpi_wtime()-t1
end if

! partition obs for enkf using Graham's rule ...
deallocate(numobs,numobs1)
if(simple_partition)then
  deallocate(rtmp)
  t1 = mpi_wtime()
  numobsperproc = 0
  allocate(iprocob(nobsgood))
  np=0
  do n=1,nobsgood
     np=np+1
     if(np > numproc)np = 1
     ! np is processor with the fewest number of close obs to process
     numobsperproc(np) = numobsperproc(np)+1
     iprocob(n) = np-1
  enddo
else if(letkf_flag) then
  t1 = mpi_wtime()
  rtmp = 0
  numobsperproc = 0
  allocate(iprocob(nobsgood))
  iprocob(1:nobsgood)=-1
  ! Loop over each observation box
  do j=1,blatnum+1
     do i=1,blonnum+1
        if(i /= 1) then
           ib1=nlocconv(j,i-1)+1
           ib2=nlocoz(j,i-1)+1
           ib3=nlocsat(j,i-1)+1
        else
           if(j /= 1) then
              ib1=nlocconv(j-1,blonnum+1)+1
              ib2=nlocoz(j-1,blonnum+1)+1
              ib3=nlocsat(j-1,blonnum+1)+1
           else
              ib1=1
              ib2=nobs_conv+1
              ib3=nobs_conv+nobs_oz+1
           end if
        end if
        ie1=nlocconv(j,i)
        ie2=nlocoz(j,i)
        ie3=nlocsat(j,i)
        ! For conventional observations
        do nob=ib1,ie1
           ! Search for same observation location and time
           ! The search range is limited within the same observation box
           do nob2=nob-1,ib1,-1
              if(obloc(1,nob2)/=obloc(1,nob)) cycle
              if(obloc(2,nob2)/=obloc(2,nob)) cycle
              if(obloc(3,nob2)/=obloc(3,nob)) cycle
              if(oblnp(nob2)/=oblnp(nob)) cycle
              if(obtime(nob2)/=obtime(nob)) cycle
              np=iprocob(nob2)+1
              rtmp(np) = rtmp(np)+1
              numobsperproc(np) = numobsperproc(np)+1
              iprocob(nob) = iprocob(nob2)
              exit
           end do
           ! If there is no desirable observation, simply distribute to the smallest node.
           if(iprocob(nob) == -1) then
              np=minloc(rtmp,dim=1)
              rtmp(np) = rtmp(np)+1
              numobsperproc(np) = numobsperproc(np)+1
              iprocob(nob) = np-1
           end if
        end do
        ! For ozone observations
        do nob=ib2,ie2
           ! Search for same observation location and time
           ! The search range is limited within the same observation box
           do nob2=nob-1,ib2,-1
              if(obloc(1,nob2)/=obloc(1,nob)) cycle
              if(obloc(2,nob2)/=obloc(2,nob)) cycle
              if(obloc(3,nob2)/=obloc(3,nob)) cycle
              if(oblnp(nob2)/=oblnp(nob)) cycle
              if(obtime(nob2)/=obtime(nob)) cycle
              np=iprocob(nob2)+1
              rtmp(np) = rtmp(np)+1
              numobsperproc(np) = numobsperproc(np)+1
              iprocob(nob) = iprocob(nob2)
              exit
           end do
           ! If there is no desirable observation, simply distribute to the smallest node.
           if(iprocob(nob) == -1) then
              np=minloc(rtmp,dim=1)
              rtmp(np) = rtmp(np)+1
              numobsperproc(np) = numobsperproc(np)+1
              iprocob(nob) = np-1
           end if
        end do
        ! For satellite radiance observations
        do nob=ib3,ie3
           ! Search for same observation location and time
           ! The search range is limited within the same observation box
           do nob2=nob-1,ib3,-1
              if(obloc(1,nob2)/=obloc(1,nob)) cycle
              if(obloc(2,nob2)/=obloc(2,nob)) cycle
              if(obloc(3,nob2)/=obloc(3,nob)) cycle
              if(oblnp(nob2)/=oblnp(nob)) cycle
              if(obtime(nob2)/=obtime(nob)) cycle
              np=iprocob(nob2)+1
              rtmp(np) = rtmp(np)+1
              numobsperproc(np) = numobsperproc(np)+1
              iprocob(nob) = iprocob(nob2)
              exit
           end do
           ! If there is no desirable observation, simply distribute to the smallest node.
           if(iprocob(nob) == -1) then
              np=minloc(rtmp,dim=1)
              rtmp(np) = rtmp(np)+1
              numobsperproc(np) = numobsperproc(np)+1
              iprocob(nob) = np-1
           end if
        end do
     end do
  end do
  deallocate(rtmp)
else
  allocate(numobs(nobsgood),numobs1(nobsgood))
  t1 = mpi_wtime()
  call estimate_work_enkf2(numobs) ! fill numobs array with number of obs close to each ob
  ! distribute the results of estimate_work to all processors.
  call mpi_allreduce(numobs,numobs1,nobsgood,mpi_integer,mpi_sum,mpi_comm_world,ierr)
  if (nproc == 0) print *,'time in estimate_work_enkf2 = ',mpi_wtime()-t1,' secs'
  t1 = mpi_wtime()
  rtmp = 0
  numobsperproc = 0
  allocate(iprocob(nobsgood))
  np=0
  do n=1,nobsgood
     np = minloc(rtmp,dim=1)
     ! np is processor with the fewest number of close obs to process
     rtmp(np) = rtmp(np)+1
     numobsperproc(np) = numobsperproc(np)+1
     iprocob(n) = np-1
  enddo
  deallocate(rtmp)
  deallocate(numobs,numobs1)
end if
nobs_min = minval(numobsperproc)
nobs_max = maxval(numobsperproc)
allocate(indxproc_obs(numproc,nobs_max))
numobsperproc = 0
do n=1,nobsgood
   np=iprocob(n)+1
   numobsperproc(np) = numobsperproc(np)+1 ! recalculate
   ! indxproc_obs(np,i) is i'th ob index for processor np.
   ! there are numobsperpoc(np) i values for processor np
   indxproc_obs(np,numobsperproc(np)) = n
end do
if (nproc == 0) then
    print *,'nobsgood = ',nobsgood
    print *,'min/max number of obs per proc = ',nobs_min,nobs_max
    print *,'time to do ob space decomp = ',mpi_wtime()-t1
end if

! send out observation priors to be updated on each processor.
allocate(anal_obchunk_prior(nanals,nobs_max))
if(nproc == 0) then
   print *,'sending out observation prior ensemble perts from root ...'
   t1 = mpi_wtime()
end if
if(letkf_flag) then
   call mpi_bcast(anal_ob,nobstot*nanals,mpi_real4,0,mpi_comm_world,ierr)
   do nob1=1,numobsperproc(nproc+1)
      nob2 = indxproc_obs(nproc+1,nob1)
      anal_obchunk_prior(1:nanals,nob1) = anal_ob(1:nanals,nob2)
   end do
else
   !allocate(rtmp(nobs_max))
   if(nproc == 0) then
      ! send one big message to each task.
      do np=1,numproc-1
         do nob1=1,numobsperproc(np+1)
            nob2 = indxproc_obs(np+1,nob1)
            anal_obchunk_prior(:,nob1) = anal_ob(:,nob2)
         end do
         call mpi_send(anal_obchunk_prior,nobs_max*nanals,mpi_real4,np, &
              1,mpi_comm_world,ierr)
      end do
      ! anal_obchunk_prior on root (no send necessary)
      do nob1=1,numobsperproc(1)
         nob2 = indxproc_obs(1,nob1)
         anal_obchunk_prior(:,nob1) = anal_ob(:,nob2)
      end do
      ! now we don't need anal_ob anymore for EnKF. (only defined on root)
      deallocate(anal_ob)
   else
      ! recv one large message on each task.
      call mpi_recv(anal_obchunk_prior,nobs_max*nanals,mpi_real4,0, &
           1,mpi_comm_world,mpi_status,ierr)
   end if
   !deallocate(rtmp)
end if
call mpi_barrier(mpi_comm_world, ierr)
if(nproc == 0) print *,'... took ',mpi_wtime()-t1,' secs'

! setup kdtree pointers for grid and ob locations assigned to this processor.
allocate(obloc_chunk(3,numobsperproc(nproc+1)))
allocate(grdloc_chunk(3,numptsperproc(nproc+1)))
allocate(lnp_chunk(numptsperproc(nproc+1),nlevs_pres))
do nob=1,numobsperproc(nproc+1)
   obloc_chunk(:,nob) = obloc(:,indxproc_obs(nproc+1,nob))
enddo
do i=1,numptsperproc(nproc+1)
   grdloc_chunk(:,i) = gridloc(:,indxproc(nproc+1,i))
   do nn=1,nlevs_pres
      lnp_chunk(i,nn) = logp(indxproc(nproc+1,i),nn)
   end do
end do
! don't need these anymore
! deallocate here to save memory (allocated in gridinfo).
deallocate(logp,gridloc)

! set up kd-trees to search only the subset
! of gridpoints, obs to be updated on this processor..
if (numptsperproc(nproc+1) >= 3) then
   kdtree_grid => kdtree2_create(grdloc_chunk,sort=.false.,rearrange=.true.)
endif
if (numobsperproc(nproc+1) >= 3) then
   kdtree_obs  => kdtree2_create(obloc_chunk,sort=.false.,rearrange=.true.)
endif

! nob1 is the index of the obs to be processed on this rank
! nob2 maps nob1 to 1:nobsgood array (nobx)
allocate(oblnp_chunk(numobsperproc(nproc+1)))
allocate(obtime_chunk(numobsperproc(nproc+1)))
allocate(ensmean_obchunk(numobsperproc(nproc+1)))
allocate(indxob_chunk(nobsgood))
indxob_chunk = -1
do nob1=1,numobsperproc(nproc+1)
   nob2 = indxproc_obs(nproc+1,nob1)
   oblnp_chunk(nob1) = oblnp(nob2)
   obtime_chunk(nob1) = obtime(nob2)
   indxob_chunk(nob2) = nob1
   ensmean_obchunk(nob1) = ensmean_ob(nob2)
end do

end subroutine load_balance

subroutine estimate_work_enkf1(numobs)
! estimate work needed to update each analysis grid
! point (considering all the observations within the localization radius).

implicit none
integer(i_kind), dimension(:), intent(inout) :: numobs

integer nob,n1,n2,i,ideln

ideln = int(real(npts)/real(numproc))
n1 = 1 + nproc*ideln
n2 = (nproc+1)*ideln
if (nproc == numproc-1) n2 = npts

! loop over 'good' obs.
numobs = 1 ! set min # of obs to 1, not 0 (so single ob test behaves)
!$omp parallel do  schedule(dynamic,1) private(nob,i)
obsloop: do i=n1,n2
    do nob=1,nobsgood
       if (sum((obloc(1:3,nob)-gridloc(1:3,i))**2,1) < corrlengthsq(nob)) &
       numobs(i) = numobs(i) + 1
    end do ! i
end do obsloop

end subroutine estimate_work_enkf1

subroutine estimate_work_enkf2(numobs)
! estimate work needed to update each observation prior
! (considering all the observations within the localization radius of each

implicit none
integer(i_kind), dimension(:), intent(inout) :: numobs

integer(i_kind)  nob,nob2,n1,n2,ideln

if (nobsgood > numproc) then
   ideln = int(real(nobsgood)/real(numproc))
   n1 = 1 + nproc*ideln
   n2 = (nproc+1)*ideln
   if (nproc == numproc-1) n2 = nobsgood
else
   if(nproc < nobsgood)then
     n1 = nproc+1
     n2 = n1
   else
     n1=1
     n2=0
   end if
end if

! loop over 'good' obs.
numobs = 0
!$omp parallel do  schedule(dynamic,1) private(nob,nob2)
obsloop: do nob2=n1,n2
    do nob=1,nobsgood
    ! find number of obs close to this ob.
       if (sum((obloc(1:3,nob)-obloc(1:3,nob2))**2,1) < corrlengthsq(nob))&
       numobs(nob2) = numobs(nob2) + 1
    end do ! loop over obs on this processor
end do obsloop

end subroutine estimate_work_enkf2

subroutine loadbal_cleanup()
! deallocate module-level allocatable arrays
if (allocated(obloc_chunk)) deallocate(obloc_chunk)
if (allocated(grdloc_chunk)) deallocate(grdloc_chunk)
if (allocated(lnp_chunk)) deallocate(lnp_chunk)
if (allocated(oblnp_chunk)) deallocate(oblnp_chunk)
if (allocated(obtime_chunk)) deallocate(obtime_chunk)
if (allocated(ensmean_obchunk)) deallocate(ensmean_obchunk)
if (allocated(iprocob)) deallocate(iprocob)
if (allocated(indxob_chunk)) deallocate(indxob_chunk)
if (allocated(indxproc_obs)) deallocate(indxproc_obs)
if (allocated(numptsperproc)) deallocate(numptsperproc)
if (allocated(numobsperproc)) deallocate(numobsperproc)
if (allocated(indxproc)) deallocate(indxproc)
if (associated(kdtree_obs)) call kdtree2_destroy(kdtree_obs)
if (associated(kdtree_grid)) call kdtree2_destroy(kdtree_grid)
end subroutine loadbal_cleanup

end module loadbal
