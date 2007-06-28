subroutine setuprhsall(ndata,channelinfo,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  setuprhsall   sets up rhs of oi 
!   prgmmr: derber           org: np23                date: 2003-05-22
!
! abstract: This routine sets up the right hand side (rhs) of the 
!           analysis equation.  Functions performed in this routine
!           include:
!             a) calculate increments between current solutions and obs,
!             b) generate statistical summaries of quality control and innovations,
!             c) generate diagnostic files (optional), and
!             d) prepare/save information for use in inner minimization loop
!
! program history log:
!   2003-05-22  derber
!   2003-12-23  kleist  - ozone calculation modified to use guess pressure
!   2004-06-17  treadon - update documentation
!   2004-07-23  derber  - modify to include conventional sst
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - increase dimension of work arrays for nonlin qc
!   2004-12-08  xu li   - replace local logical flag retrieval with that in radinfo
!   2004-12-22  treadon - restructure code to compute and write out 
!                         innovation information on select outer iterations
!   2005-01-20  okamoto - add ssmi/amsre/ssmis
!   2005-03-30  lpchang - statsoz call was passing ozmz var unnecessarily
!   2005-04-18  treadon - deallocate fbias
!   2005-05-27  derber  - level output change
!   2005-07-06  derber  - include mhs and hirs/4
!   2005-06-14  wu      - add OMI oz
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - simplify data file info handling
!   2005-10-20  kazumori - modify for real AMSR-E data process
!   2005-12-01  cucurull - add GPS bending angle
!   2005-12-21  treadon  - modify processing of GPS data
!   2006-01-09  derber - move create/destroy array, compute_derived, q_diag
!                        from glbsoi outer loop into this routine
!   2006-01-12  treadon - add channelinfo
!   2006-02-03  derber  - modify for new obs control and obs count- clean up!
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-03-21  treadon - add code to generate optional observation perturbations
!   2006-07-28  derber  - modify code for new inner loop obs data structure
!   2006-07-29  treadon - remove create_atm_grids and destroy_atm_grids
!   2006-07-31  kleist - change call to atm arrays routines
!   2007-03-01  treadon - add toss_gps and toss_gps1
!   2007-06-08  kleist/treadon - add prefix (task id or path) to diag_conv_file
!
!   input argument list:
!     ndata(*,1)- number of prefiles retained for further processing
!     ndata(*,2)- number of observations read
!     ndata(*,3)- number of observations keep after read
!     channelinfo - structure containing satellite sensor information
!     mype     - mpi task id
!
!   output argument list:
!
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: izero,zero,rozcon,two
  use obsmod, only: nsat1,dtype,iadate,&
       nchan_total,ndat,diag_conv,obs_setup,lunobs_obs,&
       mype_dw,mype_rw,mype_srw,mype_sst,dirname,&
       mype_gps,mype_uv,mype_ps,mype_t,mype_pw,mype_q,write_diag,&
       nprof_gps,ditype,perturb_fact,perturb_obs,ran01dom
  use radinfo, only: mype_rad,diag_rad,jpch,retrieval,fbias
  use pcpinfo, only: diag_pcp
  use ozinfo, only: diag_ozone,mype_oz,jpch_oz
  use mpimod, only: ierror,mpi_integer,mpi_max,mpi_comm_world,&
       mpi_rtype,mpi_sum,npe
  use gridmod, only: lat1,lon1,nsig,twodvar_regional

  use jfunc, only: jiter,jiterstart,miter,first,last,switch_on_derivatives
  use qcmod, only: npres_print
  use crtm_module, only: crtm_channelinfo_type
  use convinfo, only: nconvtype
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype
  integer(i_kind),dimension(ndat,3),intent(in):: ndata
  type(crtm_channelinfo_type):: channelinfo

! Delcare local variables
  logical rad_diagsave,ozone_diagsave,pcp_diagsave,conv_diagsave

  character(12):: string
  character(10)::obstype
  character(20)::isis
  character(128):: diag_conv_file

  integer(i_kind) lunin,nobs,nchanl,nreal,nele,i,j,k,&
       is,idate,i_dw,i_rw,i_srw,i_sst,i_gps,i_uv,i_ps,&
       i_t,i_pw,i_q,krsize,nsize
  integer(i_kind),allocatable,dimension(:):: nrnd

  real(r_kind):: rseed
  real(r_kind),dimension(40,ndat):: aivals,aivals1
  real(r_kind),dimension(7,jpch):: stats,stats1
  real(r_kind),dimension(9,jpch_oz):: stats_oz,stats_oz1
  real(r_kind),dimension(npres_print,nconvtype,5,3):: bwork,bwork1
  real(r_kind),dimension(7*nsig+100,10)::awork,awork1
  real(r_kind),dimension(nsig,max(1,nprof_gps)):: super_gps1,super_gps
  real(r_kind),dimension(max(1,nprof_gps)):: toss_gps1,toss_gps
  real(r_kind),allocatable,dimension(:,:):: perturb
  
!******************************************************************************
! Initialize variables and constants.
  first = jiter == jiterstart   ! .true. on first outer iter
  last  = jiter == miter+1      ! .true. following last outer iter

! Set diagnostic output flag

  rad_diagsave  = write_diag(jiter) .and. diag_rad
  pcp_diagsave  = write_diag(jiter) .and. diag_pcp
  conv_diagsave = write_diag(jiter) .and. diag_conv
  ozone_diagsave= write_diag(jiter) .and. diag_ozone

  aivals   = zero
  stats    = zero
  stats_oz = zero
  nchan_total=izero
  super_gps1=zero
  super_gps=zero
  toss_gps1=zero
  toss_gps=zero

  i_ps = 1
  i_uv = 2
  i_t  = 3
  i_q  = 4
  i_pw = 5
  i_rw = 6
  i_dw = 7
  i_srw= 8
  i_gps= 9
  i_sst= 10

  awork=zero
  bwork=zero

! Compute derived quantities on grid
  call compute_derived(mype)

  lunin=1
  open(lunin,file=obs_setup,form='unformatted')
  rewind lunin
  
! If requested, create conventional diagnostic files
  if(conv_diagsave)then
     write(string,900) jiter
900  format('conv_',i2.2)
     diag_conv_file=trim(dirname) // trim(string)
     open(7,file=trim(diag_conv_file),form='unformatted')
     rewind 7
     idate=iadate(4)+iadate(3)*100+iadate(2)*10000+iadate(1)*1000000
     if(mype == 0)write(7)idate
  end if



! Loop over data types to process
  do is=1,ndat
     nobs=nsat1(is)

     if(nobs > 0)then

       read(lunin,end=125) obstype,isis,nreal,nchanl
       nele=nreal+nchanl


!      Allocate and zero innovation perturbation arrays
       nsize=max(1,nchanl)
       if (obstype=='uv' .or. obstype=='spd') nsize=2
       if (ditype(is) /= 'rad') then
          allocate(perturb(nobs,nsize))
          do j=1,nsize
             do i=1,nobs
                perturb(i,j)=zero
             end do
          end do
       else
          allocate(perturb(nsize,nobs))
          do j=1,nobs
             do i=1,nsize
                perturb(i,j)=zero
             end do
          end do
       endif

!      Optionally set perturbation factor for observations
       if (perturb_obs) then
          rseed=iadate(4)+iadate(3)*100+iadate(2)*10000+iadate(1)*1000000+mype+nobs
          call random_seed(size=krsize)
          allocate(nrnd(krsize))
          do i=1,krsize
             nrnd(i)=rseed
          end do
          call random_seed(put=nrnd)
          deallocate(nrnd)
          if (ditype(is) /= 'rad') then
             do j=1,nsize
                do i=1,nobs
                   perturb(i,j) = ran01dom()*perturb_fact
                end do
             end do
          else
             do j=1,nobs
                do i=1,nsize
                   perturb(i,j) = ran01dom()*perturb_fact
                end do
             end do
          endif
       endif


!      Set up for radiance data
       if(ditype(is) == 'rad')then
          call setuprad(lunin,&
             mype,aivals,stats,nchanl,nreal,nobs,&
             obstype,isis,is,rad_diagsave,channelinfo,perturb)

!      Set up for precipitation data
       else if(ditype(is) == 'pcp')then
          call setuppcp(lunin,mype,&
             aivals,nele,nobs,obstype,isis,is,pcp_diagsave,perturb)

!      Set up temperature data
       else if(ditype(is) == 'conv')then
          if(obstype=='t')then
             call setupt(lunin,mype,bwork,awork(1,i_t),nele,nobs,conv_diagsave,perturb)

!         Set up uv wind data
          else if(obstype=='uv')then
             call setupw(lunin,mype,bwork,awork(1,i_uv),nele,nobs,conv_diagsave,perturb)

!         Set up wind speed data
          else if(obstype=='spd')then
            call setupspd(lunin,mype,bwork,awork(1,i_uv),nele,nobs,conv_diagsave,perturb)

!         Set up surface pressure data
          else if(obstype=='ps')then
             call setupps(lunin,mype,bwork,awork(1,i_ps),nele,nobs,conv_diagsave,perturb)

!         Set up moisture data
          else if(obstype=='q') then
             call setupq(lunin,mype,bwork,awork(1,i_q),nele,nobs,conv_diagsave,perturb)

!         Set up lidar wind data
          else if(obstype=='dw')then
             call setupdw(lunin,mype,bwork,awork(1,i_dw),nele,nobs,conv_diagsave)

!         Set up radar wind data
          else if(obstype=='rw')then
             call setuprw(lunin,mype,bwork,awork(1,i_rw),nele,nobs,conv_diagsave)

!         Set up total precipitable water (total column water) data
          else if(obstype=='pw')then
             call setuppw(lunin,mype,bwork,awork(1,i_pw),nele,nobs,conv_diagsave)

!         Set up superob radar wind data
          else if(obstype=='srw')then
             call setupsrw(lunin,mype,bwork,awork(1,i_srw),nele,nobs,conv_diagsave)

!         Set up conventional sst data
          else if(obstype=='sst') then 
             call setupsst(lunin,mype,bwork,awork(1,i_sst),nele,nobs,conv_diagsave)
          end if

!      Set up ozone (sbuv2/omi) data
       else if(ditype(is) == 'ozone')then
          call setupoz(lunin,mype,stats_oz,nchanl,nreal,nobs,&
               obstype,isis,is,ozone_diagsave,perturb)


!      Set up GPS local refractivity data
       else if(ditype(is) == 'gps')then
          if(obstype=='gps_ref')then
             call setupref(lunin,mype,awork(1,i_gps),nele,nobs,conv_diagsave,&
                super_gps1,toss_gps1)

!         Set up GPS local bending angle data
          else if(obstype=='gps_bnd')then
             call setupbend(lunin,mype,awork(1,i_gps),nele,nobs,conv_diagsave,&
                super_gps1,toss_gps1)
          end if
       end if

       deallocate(perturb)

     end if

  end do
125 continue
  close(lunin)
  if (conv_diagsave) close(7)


! Get moisture diagnostics
  call q_diag(mype)


! Collect and compute statistics for GPS data
  call mpi_allreduce(super_gps1,super_gps,nsig*nprof_gps,mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)
  call mpi_allreduce(toss_gps1,toss_gps,nprof_gps,mpi_rtype,mpi_max,&
       mpi_comm_world,ierror)
  call genstats_gps(bwork,awork(1,i_gps),super_gps,toss_gps)

! Collect satellite and precip. statistics
  call mpi_reduce(aivals,aivals1,40*ndat,mpi_rtype,mpi_sum,mype_rad, &
       mpi_comm_world,ierror)

  call mpi_reduce(stats,stats1,7*jpch,mpi_rtype,mpi_sum,mype_rad, &
       mpi_comm_world,ierror)

  call mpi_reduce(stats_oz,stats_oz1,9*jpch_oz,mpi_rtype,mpi_sum,mype_oz, &
       mpi_comm_world,ierror)

! Collect conventional data statistics
  
  call mpi_allreduce(bwork,bwork1,npres_print*nconvtype*5*3,mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)
  
  call mpi_allreduce(awork,awork1,10*(7*nsig+100),mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)

! Compute and print statistics for radiance, precipitation, and ozone data.
! These data types are NOT processed when running in 2dvar mode.  Hence
! the check on the 2dvar flag below.

  if (.not.twodvar_regional) then

!    Compute and print statistics for radiance data
     if(mype==mype_rad) call statsrad(aivals1,stats1,ndata)

!    Compute and print statistics for precipitation data
     if(mype==mype_rad) call statspcp(aivals1,ndata)

!    Compute and print statistics for ozone
     if (mype==mype_oz) call statsoz(stats_oz1,ndata)

  endif

! Compute and print statistics for "conventional" data
  call statsconv(mype,&
       i_ps,i_uv,i_srw,i_t,i_q,i_pw,i_rw,i_dw,i_gps,i_sst, &
       bwork1,awork1,ndata)

! If only performing sst retrieval, end program execution
  if(retrieval)then
     deallocate(fbias)
     if(mype==0)then
        write(6,*)'SETUPRHSALL:  normal completion for retrieval'
        call w3tage('GLOBAL_SSI')
     end if
     call mpi_finalize(ierror)
     stop
  end if

  return
end subroutine setuprhsall
