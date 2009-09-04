subroutine setuprhsall(ndata,mype)
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
!   2007-02-21  sienkiewicz - add MLS ozone changes
!   2007-03-01  treadon - add toss_gps and toss_gps_sub
!   2007-03-10      su - move the observation perturbation to each setup routine 
!   2007-03-19  tremolet - Jo table
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-06-08  kleist/treadon - add prefix (task id or path) to diag_conv_file
!   2007-07-09  tremolet - observation sensitivity
!   2007-06-20  cucurull - changes related to gps diagnostics
!   2007-06-29  jung - change channelinfo to array
!   2007-09-30  todling  - add timer
!   2007-10-03  todling  - add observer split option
!   2007-12-15  todling  - add prefix to diag filenames
!   2008-03-28    wu - move optional randon seed for perturb_obs to read_obs
!   2008-04-14  treadon - remove super_gps, toss_gps (moved into genstats_gps)
!   2008-05-23  safford - rm unused vars and uses
!   2008-12-08  todling - move 3dprs/geop-hght calculation from compute_derivate into here
!   2009-01-17  todling - update interface to intjo
!   2009-03-05  meunier - add call to lagragean operator
!
!   input argument list:
!     ndata(*,1)- number of prefiles retained for further processing
!     ndata(*,2)- number of observations read
!     ndata(*,3)- number of observations keep after read
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
  use kinds, only: r_kind,i_kind,r_quad
  use constants, only: izero,zero
  use guess_grids, only: load_prsges,load_geop_hgt
  use obsmod, only: nsat1,iadate,nobs_type,obscounts,&
       nchan_total,ndat,obs_setup,&
       dirname,write_diag,nprof_gps,ditype,obsdiags,lobserver,&
       destroyobs,inquire_obsdiags,lobskeep,nobskeep,lobsdiag_allocated
  use obs_sensitivity, only: lobsensfc, lsensrecompute
  use radinfo, only: mype_rad,diag_rad,jpch_rad,retrieval,fbias
  use pcpinfo, only: diag_pcp
  use ozinfo, only: diag_ozone,mype_oz,jpch_oz
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,mpi_sum
  use gridmod, only: nsig,twodvar_regional
  use gsi_4dvar, only: nobs_bins,l4dvar
  use jfunc, only: jiter,jiterstart,miter,first,last
  use qcmod, only: npres_print
  use convinfo, only: nconvtype,diag_conv
  use timermod, only: timer_ini,timer_fnl
  use lag_fields, only: lag_presetup,lag_state_write,lag_state_read,lag_destroy_uv
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype
  integer(i_kind),dimension(ndat,3),intent(in):: ndata


! Delcare local variables
  logical rad_diagsave,ozone_diagsave,pcp_diagsave,conv_diagsave,llouter,getodiag

  character(80):: string
  character(10)::obstype
  character(20)::isis
  character(128):: diag_conv_file
  character(len=12) :: clfile

  integer(i_kind) lunin,nobs,nchanl,nreal,nele,&
       is,idate,i_dw,i_rw,i_srw,i_sst,i_tcp,i_gps,i_uv,i_ps,i_lag,&
       i_t,i_pw,i_q,i_o3,iobs,nprt,ii,jj

  real(r_quad):: zjo
  real(r_kind),dimension(40,ndat):: aivals,aivals1
  real(r_kind),dimension(7,jpch_rad):: stats,stats1
  real(r_kind),dimension(9,jpch_oz):: stats_oz,stats_oz1
  real(r_kind),dimension(npres_print,nconvtype,5,3):: bwork,bwork1
  real(r_kind),dimension(7*nsig+100,13)::awork,awork1
  real(r_kind),dimension(max(1,nprof_gps)):: toss_gps_sub
  
!******************************************************************************
! Initialize timer
  call timer_ini('setuprhsall')

! Initialize variables and constants.
  first = jiter == jiterstart   ! .true. on first outer iter
  last  = jiter == miter+1      ! .true. following last outer iter
  llouter=.true.

! Set diagnostic output flag

  rad_diagsave  = write_diag(jiter) .and. diag_rad
  pcp_diagsave  = write_diag(jiter) .and. diag_pcp
  conv_diagsave = write_diag(jiter) .and. diag_conv
  ozone_diagsave= write_diag(jiter) .and. diag_ozone

  aivals   = zero
  stats    = zero
  stats_oz = zero
  nchan_total=izero
  toss_gps_sub=zero

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
  i_o3 = 11
  i_tcp= 12
  i_lag= 13

  awork=zero
  bwork=zero

! Reset observation pointers
  call destroyobs

! Read observation diagnostics if available
  if (l4dvar) then
    getodiag=(.not.lobserver) .or. (lobserver.and.jiter>1)
    clfile='obsdiags.ZZZ'
    if (lobsensfc .and. .not.lsensrecompute) then
      write(clfile(10:12),'(I3.3)') miter
      call read_obsdiags(clfile)
      call inquire_obsdiags(miter)
    else if (getodiag) then
      if (.not.lobserver) then
        write(clfile(10:12),'(I3.3)') jiter
        call read_obsdiags(clfile)
        call inquire_obsdiags(miter)
      endif
    endif
  endif

  if (allocated(obscounts)) then
     write(6,*)'setuprhsall: obscounts allocated'
     call stop2(285)
  end if
  allocate(obscounts(nobs_type,nobs_bins))

  if (jiter>1.and.lobskeep) then
    nobskeep=1
  else
    nobskeep=0
  endif

! The 3d pressure and geopotential grids are initially loaded at
! the end of the call to read_guess.  Thus, we don't need to call 
! load_prsges and load_geop_hgt on the first outer loop.  We need 
! to update these 3d pressure arrays on all subsequent outer loops.
! Hence, the conditional call to load_prsges and load_geop_hgt

  if (jiter>jiterstart) then
     call load_prsges
     call load_geop_hgt
!    if (sfcmod_gfs .or. sfcmod_mm5) then
!       if (mype==0) write(6,*)'COMPUTE_DERIVED:  call load_fact10'
!       call load_fact10
!    endif
  endif

! Compute derived quantities on grid
  if(.not. lobserver) call compute_derived(mype)

! Initialize observer for Lagrangian data
  if ( (l4dvar.and.lobserver) .or. .not.l4dvar ) then
    ! Init for Lagrangian data assimilation (gather winds and NL integration)
    call lag_presetup()
    ! Save state for inner loop if in 4Dvar observer mode
    if (l4dvar.and.lobserver) then
      call lag_state_write()
    end if
  else
    ! Init for Lagrangian data assimilation (read saved parameters)
    call lag_state_read()
  end if
  ! ------------------------------------------------------------------------

  if ( (l4dvar.and.lobserver) .or. .not.l4dvar ) then

! Reset observation pointers
  do ii=1,nobs_bins
    do jj=1,nobs_type
      obsdiags(jj,ii)%tail => NULL()
    enddo
  enddo

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

!      Set up for radiance data
       if(ditype(is) == 'rad')then

          call setuprad(lunin,&
             mype,aivals,stats,nchanl,nreal,nobs,&
             obstype,isis,is,rad_diagsave)

!      Set up for precipitation data
       else if(ditype(is) == 'pcp')then
          call setuppcp(lunin,mype,&
             aivals,nele,nobs,obstype,isis,is,pcp_diagsave)

!      Set up temperature data
       else if(ditype(is) == 'conv')then
          if(obstype=='t')then
             call setupt(lunin,mype,bwork,awork(1,i_t),nele,nobs,conv_diagsave)

!         Set up uv wind data
          else if(obstype=='uv')then
             call setupw(lunin,mype,bwork,awork(1,i_uv),nele,nobs,conv_diagsave)

!         Set up wind speed data
          else if(obstype=='spd')then
            call setupspd(lunin,mype,bwork,awork(1,i_uv),nele,nobs,conv_diagsave)

!         Set up surface pressure data
          else if(obstype=='ps')then
             call setupps(lunin,mype,bwork,awork(1,i_ps),nele,nobs,conv_diagsave)

!         Set up tc-mslp data
          else if(obstype=='tcp')then
             call setuptcp(lunin,mype,bwork,awork(1,i_tcp),nele,nobs)

!         Set up moisture data
          else if(obstype=='q') then
             call setupq(lunin,mype,bwork,awork(1,i_q),nele,nobs,conv_diagsave)

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

!         Set up conventional lagrangian data
          else if(obstype=='lag') then 
             call setuplag(lunin,mype,bwork,awork(1,i_lag),nele,nobs,conv_diagsave)

          end if

!      Set up ozone (sbuv/omi/mls) data
       else if(ditype(is) == 'ozone')then
          if (obstype == 'mlsoz') then
             call setupo3lv(lunin,mype,bwork,awork(1,i_o3),nele,nobs,&
                  isis,is,obstype,ozone_diagsave)
          else
             call setupoz(lunin,mype,stats_oz,nchanl,nreal,nobs,&
                  obstype,isis,is,ozone_diagsave)
          end if

!      Set up GPS local refractivity data
       else if(ditype(is) == 'gps')then
          if(obstype=='gps_ref')then
             call setupref(lunin,mype,awork(1,i_gps),nele,nobs,toss_gps_sub)

!         Set up GPS local bending angle data
          else if(obstype=='gps_bnd')then
             call setupbend(lunin,mype,awork(1,i_gps),nele,nobs,toss_gps_sub)
          end if
       end if

     end if

  end do
125 continue
  close(lunin)

  endif ! < lobserver >
  lobsdiag_allocated=.true.

! Deallocate wind field array for Lagrangian data assimilation
  call lag_destroy_uv()

! Setup observation vectors
  call setupyobs

! Finalize qc and accumulate statistics for GPSRO data
  call genstats_gps(bwork,awork(1,i_gps),toss_gps_sub,conv_diagsave,mype)

  if (conv_diagsave) close(7)

  call inquire_obsdiags(miter)

! Get moisture diagnostics
! call q_diag(mype)

! Collect satellite and precip. statistics
  call mpi_reduce(aivals,aivals1,40*ndat,mpi_rtype,mpi_sum,mype_rad, &
       mpi_comm_world,ierror)

  call mpi_reduce(stats,stats1,7*jpch_rad,mpi_rtype,mpi_sum,mype_rad, &
       mpi_comm_world,ierror)

  call mpi_reduce(stats_oz,stats_oz1,9*jpch_oz,mpi_rtype,mpi_sum,mype_oz, &
       mpi_comm_world,ierror)

! Collect conventional data statistics
  
  call mpi_allreduce(bwork,bwork1,npres_print*nconvtype*5*3,mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)
  
  call mpi_allreduce(awork,awork1,13*(7*nsig+100),mpi_rtype,mpi_sum, &
       mpi_comm_world,ierror)

! Compute and print statistics for radiance, precipitation, and ozone data.
! These data types are NOT processed when running in 2dvar mode.  Hence
! the check on the 2dvar flag below.

  if ( (l4dvar.and.lobserver) .or. .not.l4dvar ) then

  if (.not.twodvar_regional) then

!    Compute and print statistics for radiance data
     if(mype==mype_rad) call statsrad(aivals1,stats1,ndata)

!    Compute and print statistics for precipitation data
     if(mype==mype_rad) call statspcp(aivals1,ndata)

!    Compute and print statistics for ozone
     if (mype==mype_oz) call statsoz(stats_oz1,bwork1,awork1(1,i_o3),ndata)

  endif

! Compute and print statistics for "conventional" data
  call statsconv(mype,&
       i_ps,i_uv,i_srw,i_t,i_q,i_pw,i_rw,i_dw,i_gps,i_sst,i_tcp,i_lag, &
       bwork1,awork1,ndata)

  endif  ! < .not. lobserver >

! Print Jo table
  nprt=2
  llouter=.true.
  call evaljo(zjo,iobs,nprt,llouter)

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

! Finalize timer
  call timer_fnl('setuprhsall')

  return
end subroutine setuprhsall
