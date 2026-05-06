module m_gpsStats
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_gpsStats
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2015-08-28
!
! abstract: a modular wrapper of (gsp_allhead, gps_alltail), and genstats_gps()
!
! program history log:
!   2007-06-22  cucurull - modify gps_all_ob_type structure
!   2015-08-28  j guo    - created this module on top of genstats_gps();
!                        . completed with type/data components from obsmod;
!                        . changed code where this module needs to be used;
!                        . added this document block;
!                        . for earlier history log, see the history log section
!                          inside ::genstats_gps() below.
!   2016-05-18  j guo    - Made the type private, since this is only a single
!                          instance module object, with its components defined
!                          as module variables (gps_allhead, and gsp_alltail).
!                        . Removed old interface names, which are no longer used
!                          in this version of GSI.
!                        . Edited the in-file documentation.
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

  use m_gpsNode, only: gps_ob_type => gpsNode
  use kinds , only: r_kind,i_kind
  implicit none
  private	! except
  
        ! Data Structure:
  !public:: gps_all_ob_type      ! currently not required to be public

    type gps_all_ob_type
      type(gps_all_ob_type),pointer :: llpoint => NULL()
      type(gps_ob_type),pointer :: mmpoint => NULL()
      real(r_kind)    :: ratio_err                        
      real(r_kind)    :: obserr                        
      real(r_kind)    :: dataerr                       
      real(r_kind)    :: pg                       
      real(r_kind)    :: b                      
      real(r_kind)    :: loc                    
      real(r_kind)    :: type               

      real(r_kind),dimension(:),pointer :: rdiag => NULL()
      integer(i_kind) :: kprof
      logical         :: luse          !  flag indicating if ob is used in pen.

      logical         :: muse          !  flag indicating if ob is used in pen.
      character(8)    :: cdiag

      integer(i_kind) :: idv,iob	      ! device id and obs index for sorting
      real   (r_kind) :: elat, elon      ! earth lat-lon for redistribution
      !real   (r_kind) :: dlat, dlon      ! earth lat-lon for redistribution
    end type gps_all_ob_type

    type gps_all_ob_head
      integer(i_kind):: n_alloc=0
      type(gps_all_ob_type),pointer :: head => NULL()
    end type gps_all_ob_head

        ! Data objects:

  public:: gps_allhead
  public:: gps_alltail

        ! interfaces:

  public:: gpsStats_create
  public:: gpsStats_destroy

        interface gpsStats_create ; module procedure create_; end interface
        interface gpsStats_destroy; module procedure destroy_genstats_gps; end interface

  public:: gpsStats_genStats

        interface gpsStats_genStats; module procedure genstats_gps; end interface

        ! Synopsis:
        !       - []_create: allocated in gsimod::gsimain_initialize().  It was
        !         done through ::create_obsmod_vars(), and now next to it.
        !
        !       - externally built, node-by-node, in setupbend() or setupref().
        !         This is the reason why the ADT can not be defined as "private".
        !
        !       - []_genStats: used to update m_rhs::[ab]work, in setuprhsall(),
        !         through ::genstats_gps();
        !
        !       - []_destroy: deallocated within genstats_gps(), when it it
        !         finished.
        !
        ! The use of []_create()/[]_destroy() pair is obviously not symmetric.  It
        ! would cleaner if they are be moved to the level of setuprhsall(),
        ! where m_rhs::[ab]work are computed.  e.g.,
        !
        !       if(init_pass) call gpsStats_create()
        !       ...
        !       if(last_pass) then
        !         call gpsStats_genstats(bwork,awork,...)
        !         call gpsStats_destroy()
        !       endif
        !
        ! As it is now, the second half has been done, but the first half
        ! stayed at where it has been, for later.

! Most implementations of this module, are snap-shots of gps_all_ob_type, from
! obsmod, its original home.  These implementations include, the type
! definition (gps_all_ob_type and gsp_app_ob_head), instantiation of the type
! (gps_allhead and gsp_alltail), and interfaces for the operations of this
! object ([]_create, []_genstats, []_destroy).  The part of implementation for
! the node-growing, is remained in setupbend() and setupref() as is.

  type(gps_all_ob_head),dimension(:),pointer :: gps_allhead => null()
  type(gps_all_ob_head),dimension(:),pointer :: gps_alltail => null()

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='genstats_gps'
contains

  subroutine create_()
    use gsi_4dvar, only: nobs_bins
    implicit none

    ALLOCATE(gps_allhead(nobs_bins))
    ALLOCATE(gps_alltail(nobs_bins))
  end subroutine create_

  subroutine destroy_genstats_gps()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_genstats_gps
!     prgmmr:    treadon     org: np20                date: 2005-12-21
!
! abstract:  deallocate arrays holding gps information
!
! program history log:
!   2005-12-21  treadon
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$  end documentation block
    use gsi_4dvar, only: nobs_bins
    use kinds, only: i_kind
    implicit none

    integer(i_kind):: istatus,ii

    do ii=1,nobs_bins
       gps_alltail(ii)%head => gps_allhead(ii)%head
       do while (associated(gps_alltail(ii)%head))
          gps_allhead(ii)%head => gps_alltail(ii)%head%llpoint
          deallocate(gps_alltail(ii)%head,stat=istatus)
          if (istatus/=0) write(6,*)'DESTROY_GENSTATS_GPS: deallocate error for gps_all, istatus=',istatus
          gps_alltail(ii)%head => gps_allhead(ii)%head
       end do
    end do

    return
  end subroutine destroy_genstats_gps

subroutine genstats_gps(bwork,awork,toss_gps_sub,conv_diagsave,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genstats_gps    generate statistics for gps observations
!   prgmmr: treadon          org: np20                date: 2005-12-21
!
! abstract:  For gps observations, this routine
!              a) collects statistics for runtime diagnostic output
!              f) adjusts observation error ratio based on superobs factor
!
! program history log:
!   2005-12-21  treadon
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2006-09-20  cucurull - replace superobs factor for obs in a top (non-full) layer 
!   2007-03-01  treadon - add array toss_gps
!   2007-03-19  tremolet - binning of observations
!   2007-06-21 cucurull - add conv_diagsave and mype in argument list; 
!                         modify qc and output for diagnostic file based on toss_gps
!                         print out diagnostic files if requested
!                         add wgtlim and huge_single in constants module
!   2008-02-27 cucurull - modify diagnostics output file
!   2008-04-14 treadon  - compute super_gps within this routine
!   2008-06-04 safford  - rm unused vars and uses
!   2008-09-05 lueken   - merged ed's changes into q1fy09 code
!   2008-25-08 todling  - adapt obs-binning change to GSI-May2008
!   2009-02-05 cucurull - modify latitude range four statistics output
!   2009-10-22     shen - add high_gps
!   2010-04-09 cucurull - fix several bugs for high_gps (diag information, counters, 
!                       - consider failure of gross check, obs-binning structures, QC for CL profiles)
!                       - reorganize high_gps structure
!                       - modify dimension of diagnostic structure
!   2010-07-23 treadon  - add ratio_error=zero to reqional QC block, replace (izero,ione) with (0,1),
!                         remove _i_kind suffix from integer constants, clean up use statements
!   2010-08-17 treadon  - convert high_gps from m to km one time only; break out regional
!                         QC as separate if/then block (global will bypass); replace 
!                         ratio_errors_reg with logical toss
!   2010-10-25 cucurull - add quality control options for C/NOFS satellite
!   2011-01-18 cucurull - increase the size of nreal and mreal by one element to 
!                         add gps_dtype information
!   2012-10-16 cucurull - increase the size of nreal and mreal by one element to
!                         add qrefges information, replace qcfail=5 by 4, add regional QC for MetOpB
!                         add dtype, dobs to distinguish use of toss_gps between ref/bending, add SR QC for obs
!   2014-01-28  todling - write sensitivity slot indicator (ioff) to header of diagfile
!   2014-12-13 derber   - minor optimization modifications
!   2015-07-28 cucurull - add QC for regional bending angle assimilation
!   2015-08-28  guo     - wrapped as a  module (m_gpsStats)
!                         moved the call to obsmod::destroy_genstats_gps() to
!                         where this routine was used (setuprhsall()), with its
!                         new module interface name. gpsStats_destroy().
!   2016-11-29 shlyaeva - increase the size of nreal for saving linearized Hx for EnKF
!   2016-12-09 mccarty  - add ncdiag writing support
!   2024-12-17 li       - add new hybrid obs error model by Chris Riedel
!
!   input argument list:
!     toss_gps_sub  - array of qc'd profile heights
!     conv_diagsave - logical to save innovation diagnostics
!     mype          - mpi task id
!
!   output argument list:
!     bwork    - array containing information about obs-ges statistics
!     awork    - array containing information for data counts and gross checks
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obsdiagNode_set
  use obsmod, only: nprof_gps,lobsdiag_forenkf
  use obsmod, only: lobsdiagsave,luse_obsdiag,nobs_gps 
  use obsmod, only: binary_diag,netcdf_diag,dirname,ianldate
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
                          nc_diag_write, nc_diag_data2d, nc_diag_metadata_to_single
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close
  use gridmod, only: nsig,regional
  use constants, only: tiny_r_kind,half,wgtlim,one,two,zero,five,four
  use qcmod, only: npres_print,ptop,pbot
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,mpi_sum,mpi_max,mpi_integer
  use jfunc, only: jiter,miter,jiterstart
  use gsi_4dvar, only: nobs_bins
  use convinfo, only: nconvtype
  implicit none

! Declare passed variables
  logical                                          ,intent(in):: conv_diagsave
  integer(i_kind)                                  ,intent(in) :: mype
  real(r_kind),dimension(100+7*nsig)               ,intent(inout):: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout):: bwork
  real(r_kind),dimension(max(1,nprof_gps))         ,intent(in):: toss_gps_sub

! Declare local parameters
  real(r_kind),parameter:: ten   = 10.0_r_kind
  real(r_kind),parameter:: six   = 6.0_r_kind
  real(r_kind),parameter:: r1em3 = 1.0e-3_r_kind
  real(r_kind),parameter:: r20   = 20.0_r_kind
  real(r_kind),parameter:: scale = 100.0_r_kind

! Declare local variables
  logical:: luse,muse,toss,save_jacobian
  integer(i_kind):: k,jsig,icnt,khgt,kprof,ikx,nn,j,nchar,nreal,mreal,ii,ioff
  real(r_kind):: pressure,arg,wgross,wgt,term,cg_gps,valqc,elev,satid,dtype,dobs
  real(r_kind):: ress,val,ratio_errors,ratio_errors_adjst,val2
  real(r_kind):: exp_arg,data_ikx,data_rinc,cg_term,rat_err2,elat
  real(r_kind):: wnotgross,data_ipg,data_ier,data_ib,factor,super_gps_up,rhgt
  real(r_kind),dimension(nsig,max(1,nprof_gps)):: super_gps_sub,super_gps
  real(r_kind),dimension(max(1,nprof_gps)):: toss_gps
  real(r_kind),dimension(max(1,nprof_gps)):: high_gps,high_gps_sub
  real(r_kind),dimension(max(1,nprof_gps)):: dobs_height,dobs_height_sub

  real(r_single),allocatable,dimension(:,:)::sdiag
  character(8),allocatable,dimension(:):: cdiag
  !integer(i_kind), dimension(max(1,nprof_gps)) :: hold_profs
  !integer(i_kind),allocatable, dimension(:) :: unique_IDs
  !real(r_kind),dimension(nobs_gps) :: array_hght,array_gps,array_qc, &
  !                                    array_prof
  real(r_kind),allocatable,dimension(:) :: array_hght,array_gps
  real(r_kind),allocatable,dimension(:) :: array_qc,array_prof
  real(r_kind),allocatable,dimension(:) :: collect_hght,collect_gps,collect_qc, &
                                      collect_prof
  real(r_kind),dimension(max(1,nobs_gps)) :: holder_hght,holder_gps,holder_qc, &
                                      holder_prof
  real(r_kind),dimension(max(1,nprof_gps)) :: STD4060
  real(r_kind),allocatable,dimension(:) :: profile_benda,profile_impact, &
                                           sorted_profile_benda,sorted_profile_impact
  real(r_kind) :: old_err,new_err,input_impact,input_std4060,input_FracLsw, &
                  relative_error
  !!!!!!!!!!!!!!!!!!!!!

  type(obs_diag), pointer :: obsptr => NULL()
  integer(i_kind),allocatable,dimension(:) :: revcounts,displs,indices
  integer(i_kind) :: nnz, nind,nobs,total_size,prof_size
  type(gps_ob_type), pointer:: gpsptr
  type(gps_all_ob_type), pointer:: gps_allptr

  save_jacobian = conv_diagsave .and. jiter==jiterstart .and. lobsdiag_forenkf

!*******************************************************************************
! Check to see if there are any profiles to process.  If none, return.
  if (nprof_gps==0) then
     if (mype==0) write(6,*)'GENSTATS_GPS:  no profiles to process (nprof_gfs=',nprof_gps,'), EXIT routine'
     return
  endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call mpi_comm_size(mpi_comm_world,total_size,ierror)
  nobs = 0
  holder_hght(:) = 0.0_r_kind;holder_gps(:) = 0.0_r_kind
  holder_qc(:) = 0.0_r_kind;holder_prof(:) = 0.0_r_kind
  DO ii=1,nobs_bins
    gps_allptr => gps_allhead(ii)%head
    do while (associated(gps_allptr))
      luse = gps_allptr%luse
      if (luse) then
       nobs = nobs + 1
       holder_hght(nobs) = gps_allptr%rdiag(7)
       holder_gps(nobs) = gps_allptr%rdiag(17)
       holder_qc(nobs) = gps_allptr%rdiag(10)
       holder_prof(nobs) = gps_allptr%rdiag(2)
      endif
      gps_allptr => gps_allptr%llpoint
    end do
  END DO

  allocate(collect_hght(max(1,nobs)), source=zero)
  allocate(collect_gps(max(1,nobs)), source=zero)
  allocate(collect_qc(max(1,nobs)), source=zero)
  allocate(collect_prof(max(1,nobs)), source=zero)

  if (nobs > 0) then
     do ii=1,nobs
        collect_hght(ii) = holder_hght(ii)
        collect_gps(ii)  = holder_gps(ii)
        collect_qc(ii)   = holder_qc(ii)
        collect_prof(ii) = holder_prof(ii)
     end do
  endif
  
  allocate(revcounts(total_size),array_hght(nobs_gps),array_gps(nobs_gps), &
       array_qc(nobs_gps),array_prof(nobs_gps))
       
  call mpi_gather(nobs,1,mpi_integer, &
                  revcounts,1,mpi_integer,0,mpi_comm_world,ierror)
  allocate(displs(total_size))
  if (mype == 0) then
    displs(1) = 0
    do ii=2,total_size
      displs(ii) = displs(ii-1) + revcounts(ii-1)
    enddo
    !write(6,*) "CHECK DISPLS: ",displs
    array_hght(:) = 0.0_r_kind
    array_gps(:) = 0.0_r_kind
    array_qc(:) = 0.0_r_kind
    array_prof(:) = 0.0_r_kind
  endif
  call mpi_gatherv(collect_hght,nobs,mpi_rtype,&
                  array_hght,revcounts,displs,mpi_rtype, &
                  0,mpi_comm_world,ierror)
  call mpi_gatherv(collect_gps,nobs,mpi_rtype,&
                  array_gps,revcounts,displs,mpi_rtype, &
                  0,mpi_comm_world,ierror)
  call mpi_gatherv(collect_qc,nobs,mpi_rtype,&
                  array_qc,revcounts,displs,mpi_rtype, &
                  0,mpi_comm_world,ierror)
  call mpi_gatherv(collect_prof,nobs,mpi_rtype,&
                  array_prof,revcounts,displs,mpi_rtype, &
                  0,mpi_comm_world,ierror)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!! Compute STD4060 for hybrid error model
  if (mype == 0) then
    STD4060(:) = 0.0_r_kind
    !$omp parallel do default(none), schedule(dynamic,1), &
    !$omp& private(ii,prof_size,profile_benda,profile_impact,indices,sorted_profile_benda,sorted_profile_impact,k), &
    !$omp& shared(nprof_gps,array_prof,array_hght,array_qc,array_gps,STD4060)
    do ii=1,nprof_gps
      prof_size = count(((array_prof(:) == ii).and.(array_hght(:)>=40.E3).and.(array_hght(:)<=60.E3).and.(array_qc/=6.0_r_kind)))
      if (prof_size <= 15) then
        STD4060(ii) = 40.0_r_kind
        cycle
      endif
      allocate(profile_benda(prof_size),profile_impact(prof_size),indices(prof_size),sorted_profile_benda(prof_size),&
             sorted_profile_impact(prof_size))
      profile_benda(:) = pack(array_gps,((array_prof == ii).and.(array_hght(:)>=40.E3).and.(array_hght(:)<=60.E3).and.(array_qc/=6.0_r_kind)))
      profile_impact(:) = pack(array_hght,((array_prof == ii).and.(array_hght>=40.E3).and.(array_hght<=60.E3).and.(array_qc/=6.0_r_kind)))
      call sort(prof_size,profile_impact,indices)
      do k=1,prof_size
        sorted_profile_benda(k) = profile_benda(indices(k))
        sorted_profile_impact(k) = profile_impact(indices(k))
      enddo
      call cal_std4060(prof_size,sorted_profile_benda,sorted_profile_impact,STD4060(ii))
      if (STD4060(ii) > 40.0_r_kind) then
        STD4060(ii) = 40.0_r_kind
      endif
      deallocate(profile_benda,profile_impact,indices,sorted_profile_benda, &
                 sorted_profile_impact)
    enddo
  endif
 call mpi_bcast(STD4060,nprof_gps,mpi_rtype,0,mpi_comm_world,ierror)
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 deallocate(collect_hght,collect_gps,collect_qc,collect_prof)
 deallocate(revcounts,displs,array_hght,array_gps,array_qc,array_prof)
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! compute error
 DO ii=1,nobs_bins
    gps_allptr => gps_allhead(ii)%head
    do while (associated(gps_allptr))
      muse = gps_allptr%muse
      if (muse) then
        if (gps_allptr%rdiag(25) == 2.0) then
          input_impact = gps_allptr%rdiag(7)
          call compute_threeCH_uncert(input_impact*r1em3,relative_error)
          new_err = (relative_error/100._r_kind) * gps_allptr%rdiag(17)
          if (new_err < 1.0D-6) new_err = 1.0D-6
          input_std4060 = STD4060(int(gps_allptr%rdiag(2)))
          gps_allptr%rdiag(24) = input_std4060
          old_err = (1.0_r_kind/gps_allptr%rdiag(14))
          gps_allptr%ratio_err = old_err/new_err
          gpsptr       => gps_allptr%mmpoint
          if (associated(gpsptr)) then
            gpsptr%raterr2 = gps_allptr%ratio_err **2
          endif
          gps_allptr%rdiag(15) = gps_allptr%ratio_err * gps_allptr%rdiag(14)
          gps_allptr%rdiag(16) = one/new_err
        else
          input_impact = gps_allptr%rdiag(7)
          input_FracLsw = gps_allptr%rdiag(23)
          if (input_FracLsw > 40.0_r_kind) then
            input_FracLsw = 40.0_r_kind
          endif
          input_std4060 = STD4060(int(gps_allptr%rdiag(2)))
          call error_model(input_impact,input_FracLsw,input_std4060,relative_error)
          new_err = (relative_error/100._r_kind) * gps_allptr%rdiag(17)
          if (new_err < 1.0D-6) new_err = 1.0D-6
          gps_allptr%rdiag(24) = input_std4060
          old_err = (1.0_r_kind/gps_allptr%rdiag(14))
          gps_allptr%ratio_err = old_err/new_err
          gpsptr       => gps_allptr%mmpoint
          if (associated(gpsptr)) then
            gpsptr%raterr2 = gps_allptr%ratio_err **2
          endif
          gps_allptr%rdiag(15) = gps_allptr%ratio_err * gps_allptr%rdiag(14)
          gps_allptr%rdiag(16) = one/new_err
        endif
      endif
      gps_allptr => gps_allptr%llpoint
    end do
  END DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Reduce sub-domain specific QC'd profile height cutoff values to
! maximum global value for each profile
  toss_gps=zero
  call mpi_allreduce(toss_gps_sub,toss_gps,nprof_gps,mpi_rtype,mpi_max,&
       mpi_comm_world,ierror)

! If netcdf diag, initialize it
  if (conv_diagsave.and.netcdf_diag) call init_netcdf_diag_

! Get height of maximum bending angle
  dobs_height_sub = zero
  DO ii=1,nobs_bins
     gps_allptr => gps_allhead(ii)%head
     do while (associated(gps_allptr))

!       Load local work variables
        kprof        = gps_allptr%kprof
        dtype        = gps_allptr%rdiag(20)
        dobs         = gps_allptr%rdiag(17)

        if (dtype == one .and. toss_gps(kprof) > zero .and. dobs == toss_gps(kprof)) then
           dobs_height_sub(kprof) = gps_allptr%rdiag(7)
        endif

        gps_allptr => gps_allptr%llpoint

!    End loop over observations
     end do

! End of loop over time bins
  END DO

! Reduce sub-domain specific QC'd profile height to maximum global value for each profile
  dobs_height=zero
  call mpi_allreduce(dobs_height_sub,dobs_height,nprof_gps,mpi_rtype,mpi_max,&
       mpi_comm_world,ierror)


! Compute superobs factor on sub-domains using global QC'd profile height
  super_gps_sub=zero
  high_gps_sub = zero
  DO ii=1,nobs_bins
     gps_allptr => gps_allhead(ii)%head
     do while (associated(gps_allptr))

!       Load local work variables
        ratio_errors = gps_allptr%ratio_err
        data_ier     = gps_allptr%obserr
        luse         = gps_allptr%luse
        kprof        = gps_allptr%kprof
        dtype        = gps_allptr%rdiag(20)

!       Accumulate superobs factors and get highest good gps obs within a profile

        if (dtype == zero) then ! refractivity
          rhgt         = gps_allptr%loc
          if (rhgt >toss_gps(kprof)) then
             if(ratio_errors*data_ier>tiny_r_kind) then
                elev         = gps_allptr%rdiag(7)
                high_gps_sub(kprof)=max(high_gps_sub(kprof),elev)
                if(luse) then
                   khgt         = gps_allptr%loc
                   k=min(max(1,khgt),nsig)
                   super_gps_sub(k,kprof)=super_gps_sub(k,kprof)+one
                endif
             endif
          endif

        else ! bending angle
            dobs         = gps_allptr%rdiag(17)
            if(toss_gps(kprof) == zero .or. (toss_gps(kprof) > zero .and. dobs < toss_gps(kprof))) then ! will not fail SR from obs qc
              elev         = gps_allptr%rdiag(7)
              if(elev > dobs_height(kprof)) then
                if(ratio_errors*data_ier>tiny_r_kind) then
                   high_gps_sub(kprof)=max(high_gps_sub(kprof),elev)
                   if(luse) then
                      khgt         = gps_allptr%loc
                      k=min(max(1,khgt),nsig)
                      super_gps_sub(k,kprof)=super_gps_sub(k,kprof)+one
                   endif
                endif
              endif
            endif
        endif


        gps_allptr => gps_allptr%llpoint
        
!    End loop over observations
     end do

! End of loop over time bins     
  END DO

  super_gps = zero
  high_gps = zero
! Reduce sub-domain specifc superobs factors to global values for each profile
  call mpi_allreduce(super_gps_sub,super_gps,nsig*nprof_gps,mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)

! Reduce sub-domain specific high_gps values to global values for each profile
  call mpi_allreduce(high_gps_sub,high_gps,nprof_gps,mpi_rtype,mpi_max,&
       mpi_comm_world,ierror)

! Convert high_gps from meters to kilometers
  high_gps = r1em3*high_gps
  

! If generating diagnostic output, need to determine dimension of output arrays.
  nreal=0
  ioff =nreal
  if (conv_diagsave) then
     icnt = zero
     DO ii=1,nobs_bins
        gps_allptr => gps_allhead(ii)%head
        do while (associated(gps_allptr))
           luse         = gps_allptr%luse
           if(luse)icnt=icnt+1
           gps_allptr => gps_allptr%llpoint
        end do
     END DO
     if(icnt > 0)then
        nreal =25
        ioff  =nreal
        if (lobsdiagsave) nreal=nreal+4*miter+1
        if (save_jacobian) then
          nnz   = 3*nsig
          nind  = 3
          nreal = nreal + 2*nind + nnz + 2
        endif
        allocate(cdiag(icnt),sdiag(nreal,icnt))
     end if
  endif



! Loop over data to apply final qc, superobs factors, accumulate
! statistics and (optionally) load diagnostic output arrays
  icnt=0
  DO ii=1,nobs_bins
     gps_allptr => gps_allhead(ii)%head
     do while (associated(gps_allptr))

!       Load local work variables
        ratio_errors = gps_allptr%ratio_err
        data_ier     = gps_allptr%obserr
        luse         = gps_allptr%luse
        muse         = gps_allptr%muse
        khgt         = gps_allptr%loc
        kprof        = gps_allptr%kprof
        dtype        = gps_allptr%rdiag(20)
        gpsptr       => gps_allptr%mmpoint
        if(muse .and. associated(gpsptr) .and. luse_obsdiag)then
           obsptr       => gpsptr%diags
        endif

!       Determine model level to which observation is mapped to
        k=min(max(1,khgt),nsig)
 
!       Normalize ratio_errors by superobs factor.  Update ratio_error 
!       term used in minimization
        super_gps_up=zero
 
        if (super_gps(k,kprof)>tiny_r_kind) then
           do j=min(k+1,nsig),nsig
              super_gps_up = max(super_gps_up,super_gps(j,kprof))
           enddo

           if (super_gps_up >tiny_r_kind) then
              factor = one / sqrt(super_gps(k,kprof))
           else
              factor = one / sqrt(max(super_gps(k-1,kprof),super_gps(k,kprof)))
           endif
           ratio_errors_adjst = ratio_errors
           ratio_errors = ratio_errors * factor
           if(conv_diagsave .and. luse) then
              if(gps_allptr%rdiag(15) >tiny_r_kind) gps_allptr%rdiag(15)=ratio_errors_adjst*data_ier
              if(gps_allptr%rdiag(16) >tiny_r_kind) gps_allptr%rdiag(16)=ratio_errors*data_ier
           endif
 
!          Adjust error ratio for observations used in inner loop
           if (associated(gpsptr)) then
              gpsptr%raterr2 = ratio_errors **2
              if(associated(obsptr) .and. luse_obsdiag)then
                 !-- obsptr%wgtjo=(ratio_errors*data_ier)**2
                 call obsdiagNode_set(obsptr,wgtjo=(ratio_errors*data_ier)**2)
              end if
           endif
        endif


!       For given profile, check if observation level is below level at 
!       which profile data is tossed.   If so, set error parameter to 
!       zero (effectively tossing the obs).
 
        rhgt = gps_allptr%loc
        mreal = 22
        if(dtype == zero) then !refractivity
          if (rhgt<=toss_gps(kprof)) then
             if(ratio_errors*data_ier > tiny_r_kind) then ! obs was good
                if (luse) then
                   if(conv_diagsave) then
                      gps_allptr%rdiag(10) = four
                      gps_allptr%rdiag(12) = -one
                      gps_allptr%rdiag(16) = zero
                      if(lobsdiagsave) gps_allptr%rdiag(mreal+jiter) = -one
                   endif
                   elat         = gps_allptr%rdiag(3)
                   if(elat > r20) then
                      awork(22) = awork(22)+one
                   else if(elat< -r20)then
                      awork(23) = awork(23)+one
                   else
                      awork(24) = awork(24)+one
                   end if
                endif
             endif
             ratio_errors = zero
             if (associated(gpsptr)) then
                gpsptr%raterr2 = ratio_errors **2
                if(associated(obsptr) .and. luse_obsdiag)then
                   !-- obsptr%wgtjo=zero
                   !-- obsptr%muse(jiter)=.false.
                   call obsdiagNode_set(obsptr,wgtjo=zero,jiter=jiter,muse=.false.)
                end if
             endif
          endif
        else
          elev         = gps_allptr%rdiag(7)
          dobs         = gps_allptr%rdiag(17)
          if  (toss_gps(kprof) > zero .and. (dobs == toss_gps(kprof) .or. elev < dobs_height(kprof))) then ! SR from obs
              if(ratio_errors*data_ier > tiny_r_kind) then ! obs was good
                 if (luse) then
                    if(conv_diagsave) then
                      gps_allptr%rdiag(10) = four
                      gps_allptr%rdiag(12) = -one
                      gps_allptr%rdiag(15) = zero
                      gps_allptr%rdiag(16) = zero
                      if(lobsdiagsave) gps_allptr%rdiag(mreal+jiter) = -one
                    endif
                    elat         = gps_allptr%rdiag(3)
                    if(elat > r20) then
                      awork(22) = awork(22)+one
                    else if(elat< -r20)then
                      awork(23) = awork(23)+one
                    else
                      awork(24) = awork(24)+one
                    end if
                 endif
              endif
              ratio_errors = zero
              if (associated(gpsptr)) then
                 gpsptr%raterr2 = ratio_errors **2
                 if(associated(obsptr) .and. luse_obsdiag)then
                    !-- obsptr%wgtjo=zero
                    !-- obsptr%muse(jiter)=.false.
                    call obsdiagNode_set(obsptr,wgtjo=zero,jiter=jiter,muse=.false.)
                 end if
              endif
          endif
          if (gps_allptr%rdiag(10) == six) muse =.false.
        endif



!       Regional QC.  Remove obs if highest good obs in
!       profile is below platform specific threshold height.
        if(regional) then
           toss=.false.
           if(ratio_errors*data_ier > tiny_r_kind) then
             if(dtype==zero) then !refractivity
                satid        = gps_allptr%rdiag(1)
                if((satid==41).or.(satid==722).or.(satid==723).or.(satid==4).or.(satid==786).or.(satid==3)) then
                   if ((high_gps(kprof)) < ten)  toss=.true.
                else ! OL
                   if ((high_gps(kprof)) < five) toss=.true.
                endif
             else !bending angle
                if ((high_gps(kprof)) <= six)  toss=.true.
             endif
           endif
           if (toss) then
              if (luse) then
                 if(conv_diagsave) then
                    gps_allptr%rdiag(10) = four
                    gps_allptr%rdiag(12) = -one
                    gps_allptr%rdiag(15) = -one
                    gps_allptr%rdiag(16) = zero
                    if(lobsdiagsave) gps_allptr%rdiag(mreal+jiter) = -one
                 endif
                 elat         = gps_allptr%rdiag(3)
                 if(elat > r20) then
                    awork(22) = awork(22)+one
                 else if(elat< -r20)then
                    awork(23) = awork(23)+one
                 else
                    awork(24) = awork(24)+one
                 end if
              end if
              ratio_errors = zero
              if (associated(gpsptr)) then
                 gpsptr%raterr2 = ratio_errors **2
                 if(associated(obsptr) .and. luse_obsdiag)then
                    !-- obsptr%wgtjo=zero
                    !-- obsptr%muse(jiter)=.false.
                    call obsdiagNode_set(obsptr,wgtjo=zero,jiter=jiter,muse=.false.)
                 end if
              endif
           endif
        endif ! regional

!       Compute penalty terms
        if (ratio_errors*data_ier <= tiny_r_kind) muse = .false.
        if(luse)then
           val          = gps_allptr%dataerr
           val2     = val*val
           exp_arg  = -half*val2
           rat_err2 = ratio_errors**2
           data_ipg     = gps_allptr%pg
           if (data_ipg > tiny_r_kind) then
              data_ib      = gps_allptr%b
              cg_gps=cg_term/data_ib
              wnotgross= one-data_ipg
              wgross   = data_ipg*cg_gps
              arg      = exp(exp_arg)
              term     = log(wnotgross*arg+wgross)
              wgt      = wnotgross*arg/(wnotgross*arg+wgross)
           else
              term = exp_arg
              wgt  = one
           endif
           if(conv_diagsave) gps_allptr%rdiag(13) = wgt/wgtlim
           valqc = -two*rat_err2*term
         

!          Accumulate statistics for obs belonging to this task
!          based on interface (not mid-point) level
           val2=val2*rat_err2
           if(muse)then
              if(wgt < wgtlim) awork(21) = awork(21)+one

!             Accumulate values for penalty and data count
              jsig=max(1,khgt)
              awork(jsig+3*nsig+100)=awork(jsig+3*nsig+100)+valqc
              awork(jsig+5*nsig+100)=awork(jsig+5*nsig+100)+one
              awork(jsig+6*nsig+100)=awork(jsig+6*nsig+100)+val2
              nn=1
           else
              nn=2
              if(ratio_errors*data_ier >=tiny_r_kind)nn=3
           endif

           data_ikx     = gps_allptr%type
           ikx          = nint(data_ikx)
           pressure     = gps_allptr%rdiag(6)
           data_rinc    = gps_allptr%rdiag(5)*scale
!          Loop over pressure level groupings and obs to accumulate
!          statistics as a function of observation type.
           do k = 1,npres_print
              if(pressure>ptop(k) .and. pressure<=pbot(k))then
                 ress=data_rinc
                 
                 bwork(k,ikx,1,nn)  = bwork(k,ikx,1,nn)+one           ! count
                 bwork(k,ikx,2,nn)  = bwork(k,ikx,2,nn)+ress          ! (o-g)
                 bwork(k,ikx,3,nn)  = bwork(k,ikx,3,nn)+ress*ress     ! (o-g)**2
                 bwork(k,ikx,4,nn)  = bwork(k,ikx,4,nn)+val2          ! penalty
                 bwork(k,ikx,5,nn)  = bwork(k,ikx,5,nn)+valqc         ! nonlin qc penalty
                 
              end if
           end do
        end if

!       Transfer diagnostic information to output arrays
        if(conv_diagsave .and. luse) then
           icnt=icnt+1
           cdiag(icnt) = gps_allptr%cdiag
           do j=1,nreal
              sdiag(j,icnt)= gps_allptr%rdiag(j)
           enddo
        endif


        if (conv_diagsave .and. netcdf_diag .and. luse) call contents_netcdf_diag_
        
        gps_allptr => gps_allptr%llpoint

!    End loop over observations
     end do

! End of loop over time bins
  END DO

! If requested, write information to diagnostic file
  if(conv_diagsave) then
    if (netcdf_diag) call nc_diag_write
    if (binary_diag .and. icnt > 0)then
        nchar = 1
        write(7)'gps',nchar,nreal,icnt,mype,ioff
        write(7)cdiag,sdiag
        deallocate(cdiag,sdiag)
    endif
  endif


! Destroy arrays holding gps data
  call destroy_genstats_gps
contains
subroutine sort(num,x,indx)
integer(i_kind),  intent(in)  :: num
real(r_kind),     intent(in)  :: x(num)
integer(i_kind),  intent(inout) :: indx(num)

integer(i_kind)  :: ind, i, j, value_indx, line
real(r_kind) :: values

! Initialize the index array to input order
do i = 1, num
   indx(i) = i
end do

! Only one element, just send it back
if(num <= 1) return

line = num / 2 + 1
ind = num
do
  if(line > 1) then
      line = line - 1
      values = x(indx(line))
      value_indx = indx(line)
   else
      values = x(indx(ind))
      value_indx = indx(ind)

      indx(ind) = indx(1)
      ind = ind - 1
      if(ind == 1) then
         indx(1) = value_indx
         return
      endif
   endif

   i = line
   j = 2 * line

   do while(j <= ind)
      if(j < ind) then
         if(x(indx(j)) < x(indx(j + 1))) j = j + 1
      endif
      if(values < x(indx(j))) then
         indx(i) = indx(j)
         i = j
         j = 2 * j
      else
         j = ind + 1
      endif
   end do

   indx(i) = value_indx

end do
end subroutine sort
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine lsqs(n,x,y,m,c)
  ! Subroutine computes a least squares fit
  ! Returns the slope and intercept
  integer(i_kind) ,intent(in) :: n
  real(r_kind) ,intent(in) :: x(n)
  real(r_kind) ,intent(in) :: y(n)
  real(r_kind) ,intent(out) :: m,c
  !!!!
  real(r_kind) :: top,bot
  !real(r_kind) :: bot
  integer(i_kind) :: ii
  !!!!
  real(r_kind) x_avg,y_avg
  !!!!
  x_avg = sum(x)/float(n)
  y_avg = sum(y)/float(n)
  !Sum of terms
  top = 0.0
  bot = 0.0
  do ii=1,n
    top = top + ((x(ii) - x_avg)*y(ii))
    bot = bot + ((x(ii) - x_avg)**2)
  enddo
  ! Calculate Slope
  m = top/bot
  ! Calculate intercept
  c = y_avg - (m*x_avg)
  return
end subroutine lsqs
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cal_std4060(num_pts,benda,impacts,stddev)
  integer(i_kind) ,intent(in) :: num_pts
  real(r_kind) ,intent(in) :: benda(num_pts)
  real(r_kind) ,intent(in) :: impacts(num_pts)
  real(r_kind) ,intent(out) :: stddev
  !
  real(r_kind) least_squares_fit(num_pts),norm_fit(num_pts)
  real(r_kind) slope,intercept,mean
  !
  call lsqs(num_pts, impacts, log(benda),slope,intercept)
  !
  least_squares_fit = exp(slope*impacts + intercept)
  norm_fit = (least_squares_fit - benda)/least_squares_fit * 100.0_r_kind
  !
  mean = sum(norm_fit)/float(num_pts)
  stddev = sqrt(sum((norm_fit - mean)**2)/(float(num_pts)))
  return
end subroutine cal_std4060
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine std4060_errmodel(input_hght,input_std4060,error)
  real(r_kind) ,intent(in) :: input_hght,input_std4060
  real(r_kind) ,intent(out) :: error
  !
  integer(i_kind) :: n_coefs_x = 4
  integer(i_kind) :: n_coefs_y = 5
  integer(i_kind) :: aa,bb
  real(r_kind), dimension(4,5) ::  coefs
  !
  coefs(:,:) = 0.0_r_kind
  coefs(1,:) = (/ 1.25000000000000000000000000D+00,0.00000000000000000000000000D+00,&
                  0.00000000000000000000000000D+00,0.00000000000000000000000000D+00,&
                  0.00000000000000000000000000D+00 /)
  coefs(2,:) = (/ 1.99128045049602960634847933D-06, 6.75863476049698542422747641D-06,&
                  8.25883030063171995856295508D-07,-2.74722471254849586614206177D-08,&
                  1.82378331607358961583961066D-10 /)
  coefs(3,:) = (/ -6.55621669612269976911399984D-09,3.15300096566169468056300701D-09,&
                  -3.77925688851882344702164472D-10,1.06712353466978524015944812D-11,&
                  -8.65642894028274826191107202D-14 /)
  coefs(4,:) = (/  2.66873781200705724938244662D-13,-7.28569101066463132754885337D-14,&
                   1.68201368786068197554617494D-14,-5.49152377448293150781835625D-16,&
                   5.43287924985643301111631898D-18 /)
  !!!!
  error = 0.0_r_kind
  do aa=1, n_coefs_x
    do bb=1,n_coefs_y
      error = error + (coefs(aa,bb)*(input_hght**(aa-1))*(input_std4060**(bb-1)))
    enddo
  enddo
  return
end subroutine std4060_errmodel
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine lsw_errmodel(input_hght,input_lsw,error)
  real(r_kind) ,intent(in) :: input_hght,input_lsw
  real(r_kind) ,intent(out) :: error
  !
  integer(i_kind) :: n_coefs_x = 4
  integer(i_kind) :: n_coefs_y = 4
  integer(i_kind) :: aa,bb,a,b
  real(r_kind), dimension(4,4) ::  coefs
  !
  coefs(:,:) = 0.0_r_kind
  coefs(1,:) = (/ 1.25000000000000000000000000D+00,0.00000000000000000000000000D+00,&
                  0.00000000000000000000000000D+00,0.00000000000000000000000000D+00 /)
  coefs(2,:) = (/ 8.79644062400577173878313264D-04,5.00524082314834873816411509D-04,&
                 -7.06272962581374830816913560D-06,2.41695480315650456302628568D-08 /)
  coefs(3,:) = (/-1.32890117750533099169737636D-07,-1.00068031233479483283742046D-07,&
                  8.15492853573235497611204411D-10,1.37351779555849664534897326D-12 /)
  coefs(4,:) = (/ 6.84487441321994421597774228D-12,6.99590380726247359733035140D-12,&
                 -3.31568154481349481116764491D-14,-2.52493347933840834626797529D-16 /)

  !!!!
  error = 0.0_r_kind
  do aa=1, n_coefs_x
    do bb=1,n_coefs_y
      error = error + (coefs(aa,bb)*(input_hght**(aa-1))*(input_lsw**(bb-1)))
    enddo
  enddo
  return
end subroutine lsw_errmodel
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine error_model(impact_hght,lsw,std4060,errstd)
  real(r_kind) ,intent(in) :: impact_hght,lsw,std4060
  real(r_kind) ,intent(inout) :: errstd
  !
  real(r_kind),parameter :: min_error = 1.25_r_kind
  real(r_kind),parameter :: lsw_boundary = 10.E3
  real(r_kind),parameter :: std4060_boundary = 30.E3
  !!!!
  if (impact_hght <= lsw_boundary) then
    call lsw_errmodel(lsw_boundary - impact_hght,lsw,errstd)
  else if (impact_hght >= std4060_boundary) then
    call std4060_errmodel(impact_hght-std4060_boundary,std4060,errstd)
  else
    errstd = min_error
  endif
  return
end subroutine error_model
!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Subroutine to compute statistical error (from 3CHMethod) for a given
!! height using a spline fit
subroutine compute_threeCH_uncert(input_impact,error)
    use kinds, only: r_kind,i_kind,r_single
    real(r_kind), intent(in) :: input_impact
    real(r_kind), intent(out) :: error
    !!Define coefs arrays
    real(r_kind),dimension(66) :: a,b,c,d
    real(r_single),dimension(67) :: interp_impact
    integer(i_kind) :: l,level_index
    real(r_kind) :: interp_err,min_error
    !!!!!!!!!!!!!
    a(:) = 0.0_r_kind
    a(:) = (/ -1.8072472782194637_r_kind,-4.553293905977256_r_kind,-1.042632730866977_r_kind,9.100931165806573_r_kind,1.6912359147188525_r_kind,-1.2982719737865533_r_kind,0.9843419625507028_r_kind,-0.7317115785860473_r_kind,&
              -0.4110048129739834_r_kind,1.183432121346243_r_kind,0.07889575704898277_r_kind,0.1037405051596565_r_kind,0.12554091733179007_r_kind,-0.13836375493037956_r_kind,-0.31793666720456226_r_kind,1.2790616977422875_r_kind,&
               0.009323454981171042_r_kind,-0.03068971657715175_r_kind,-0.012434437302992873_r_kind,0.06721442044491856_r_kind,3.7505350493542786D-06,-0.0334244677836707_r_kind,-0.02819257166121869_r_kind,-0.2493119206629355_r_kind,&
              -0.016830473861980624_r_kind,0.4977689706039675_r_kind,0.17375087190815963_r_kind,-0.01710203102076152_r_kind,-0.00047596910806305126_r_kind,0.005636654598357445_r_kind,0.0004980015635230243_r_kind,-0.006028788035461538_r_kind,&
              -0.007278154670571774_r_kind,0.005612112069916075_r_kind,-0.003520194640361976_r_kind,-0.010140903000390833_r_kind,0.012442340314416822_r_kind,-0.013974982052712498_r_kind,-0.006212583721363296_r_kind,0.009663352106410006_r_kind,&
               0.0006611375699540623_r_kind,-0.021742887710101355_r_kind,0.01441853441306784_r_kind,-0.007862465965627119_r_kind,0.017098553464325428_r_kind,0.011225188873242864_r_kind,-0.052329516422997324_r_kind,0.02090873344068611_r_kind,&
              -0.026410224720464726_r_kind,0.01045825824505231_r_kind,0.03388155526222891_r_kind,0.007517729045328103_r_kind,-0.05995059920209567_r_kind,-0.0418887916237658_r_kind,0.0779203213611881_r_kind,-0.06077763068895248_r_kind,&
               0.07575414794899071_r_kind,-0.014759941559792988_r_kind,-0.01213449120653376_r_kind,0.020914920476412746_r_kind,-0.04953774742002803_r_kind,0.25641102955203987_r_kind,-0.23665477425886117_r_kind,-0.03603425208201827_r_kind,&
               0.14615239845937822_r_kind,-0.06624648006639511_r_kind /)

    b(:) = 0.0_r_kind
    b(:) = (/ -2.0561518447819296_r_kind,0.22049510820670193_r_kind,-0.4190389657895395_r_kind,-9.198242153793373_r_kind,-0.9470909055722503_r_kind,1.1951837100964111_r_kind,-0.6014140841997015_r_kind,0.6781444896434916_r_kind,&
               0.15003497758938922_r_kind,-0.8993086265952344_r_kind,0.20466466226031566_r_kind,0.18582750259752912_r_kind,0.170732464302648_r_kind,0.29451752503827366_r_kind,0.28822391802870695_r_kind,-0.6705595296494151_r_kind,&
               0.13353892042182558_r_kind,0.07289920169604404_r_kind,0.0208726883903759_r_kind,-0.07184111814190572_r_kind,0.0003379043049377586_r_kind,0.09345868741198415_r_kind,0.09717121911947513_r_kind,0.31906967975033607_r_kind,&
              -0.018060435327087093_r_kind,-0.9433878688650862_r_kind,-0.13252807790515786_r_kind,0.06912762861774696_r_kind,0.015100757244949484_r_kind,0.0004800603807383688_r_kind,0.0010927527090346537_r_kind,0.015791494516346934_r_kind,&
               0.01563726374305644_r_kind,-0.00441866911778142_r_kind,0.01567255621904334_r_kind,0.023098270970024787_r_kind,-0.00936941457649551_r_kind,0.038235607387624285_r_kind,0.018612906103360458_r_kind,-0.0028718956893171443_r_kind,&
               0.01997358604096397_r_kind,0.04864454552748257_r_kind,-0.009024393794918734_r_kind,0.031210993807183396_r_kind,0.000167081939952074_r_kind,0.031472883074656566_r_kind,0.12331185037652542_r_kind,-0.0008671938265990553_r_kind,&
               0.07384170712382765_r_kind,0.011975287250526767_r_kind,0.0023017716173047154_r_kind,0.07679146481147481_r_kind,0.1722967313253665_r_kind,0.09943744879226468_r_kind,-0.06185583905726744_r_kind,0.16991926963328696_r_kind,&
              -0.024082417185988136_r_kind,0.16481175816820093_r_kind,0.16889848541950192_r_kind,0.14476259759272248_r_kind,0.26699662506041877_r_kind,-0.06743664171335473_r_kind,0.8258835018193778_r_kind,0.4525933320273996_r_kind,&
               0.2969649654467448_r_kind,0.7880092025646874_r_kind /)

    c(:) = 0.0_r_kind
    c(:) = (/ 7.80707613866254_r_kind,4.395488835216012_r_kind,1.2010135139397722_r_kind,0.0_r_kind,-2.3725437794384443_r_kind,-2.051207748971555_r_kind,-1.8297280192150587_r_kind,-1.692885631501733_r_kind,-1.5635248257977767_r_kind,&
              -1.721743457938875_r_kind,-1.733477993524427_r_kind,-1.4696415134773744_r_kind,-1.206008632010103_r_kind,-0.9411204797086128_r_kind,-0.7503757708681239_r_kind,-0.7006043532428387_r_kind,-0.41186760958553814_r_kind,&
              -0.11681940379837381_r_kind,-0.06309015013774097_r_kind,-0.0586480852659678_r_kind,-0.0006870602150235801_r_kind,0.0_r_kind,0.08664397147295622_r_kind,0.19640869472825037_r_kind,0.08661229224011606_r_kind,0.0_r_kind,&
              -0.3934688259182698_r_kind,-0.13727236600410664_r_kind,-0.050323201830897266_r_kind,-0.021549594665187452_r_kind,-0.0036795101086383804_r_kind,0.0_r_kind,0.013496624926309255_r_kind,0.022936688400706822_r_kind,0.030935686374892208_r_kind,&
               0.05172021489189296_r_kind,0.06749404783077004_r_kind,0.08608223962102947_r_kind,0.12062850823814054_r_kind,0.13921656928077156_r_kind,0.16246283422136729_r_kind,0.2043934190131574_r_kind,0.23645384693781848_r_kind,&
               0.26166066258718457_r_kind,0.30049525230467_r_kind,0.35212507657755043_r_kind,0.4487464093465922_r_kind,0.5383815608306511_r_kind,0.5993733734995114_r_kind,0.6678261135857724_r_kind,0.723151462821983_r_kind,0.8293996718432791_r_kind,&
               1.0055357886022132_r_kind,1.1702774536466594_r_kind,1.243485976359891_r_kind,1.3535352623289207_r_kind,1.511040909528637_r_kind,1.690138519003633_r_kind,1.975482210660656_r_kind,2.2768757078800586_r_kind,2.6291456644947417_r_kind,&
               3.014525672355495_r_kind,3.6488854775849053_r_kind,4.590688158447078_r_kind,5.387772066255823_r_kind,6.420159192527448_r_kind /)
    d(:) = 0.0_r_kind
    d(:) = (/  6.6174092389358306_r_kind,9.781003437294185_r_kind,11.46470989370671_r_kind,11.830127817870839_r_kind,10.668183675148317_r_kind,9.456543548375889_r_kind,8.567451604690895_r_kind,7.625276819352278_r_kind,6.856906178689028_r_kind,&
               6.061276908565739_r_kind,5.123507038115774_r_kind,4.317796176549762_r_kind,3.642399858605414_r_kind,3.0977712733424982_r_kind,2.683544945381463_r_kind,2.3406709560540073_r_kind,1.9826116092380202_r_kind,1.7136063750554786_r_kind,&
               1.6389964563759971_r_kind,1.5843445573256392_r_kind,1.5210697743626842_r_kind,1.5207243689876477_r_kind,1.5807585886159612_r_kind,1.7363812075471738_r_kind,2.002547661362825_r_kind,2.054269044413873_r_kind,1.6086501461527545_r_kind,&
               1.2564041142374864_r_kind,1.1711573458303652_r_kind,1.1354589321363544_r_kind,1.1200260524502628_r_kind,1.117937296614182_r_kind,1.1277000030950675_r_kind,1.1495557370938614_r_kind,1.1736858684467029_r_kind,1.2167739164002764_r_kind,&
               1.2814514992618034_r_kind,1.3520184728304947_r_kind,1.462361337786436_r_kind,1.5953901684065737_r_kind,1.741398194104438_r_kind,1.9244957519367234_r_kind,2.155790828767262_r_kind,2.3976388163232296_r_kind,2.6826480067519705_r_kind,&
               3.000408894460918_r_kind,3.395232042986368_r_kind,3.914960786286488_r_kind,4.473383886731226_r_kind,5.120188742634101_r_kind,5.810448401715452_r_kind,6.569783191416969_r_kind,7.483492057117051_r_kind,8.601373977842535_r_kind,&
               9.829200088657693_r_kind,11.088750547321505_r_kind,12.55142744859476_r_kind,14.1141400888864_r_kind,15.95433042449844_r_kind,18.086576629372065_r_kind,20.52912985532126_r_kind,23.37573439745639_r_kind,26.57923445765057_r_kind,&
               30.817348662795993_r_kind,35.82459590118845_r_kind,41.6554853313504_r_kind /)


    interp_impact(:) = (/ 2._r_single,  2.5_r_single,  3._r_single,3.5_r_single, 4._r_single,  4.5_r_single,  5._r_single,  5.5_r_single,6._r_single, &
                          6.5_r_single,  7._r_single, 7.5_r_single,8._r_single, 8.5_r_single,  9._r_single,  9.5_r_single, 10._r_single,11._r_single,  &
                          12._r_single, 13._r_single, 14._r_single,15._r_single, 16._r_single, 17._r_single, 18._r_single, 19._r_single,20._r_single, &
                          21._r_single, 22._r_single, 23._r_single,24._r_single, 25._r_single, 26._r_single, 27._r_single, 28._r_single,29._r_single, &
                          30._r_single, 31._r_single, 32._r_single,33._r_single, 34._r_single, 35._r_single, 36._r_single, 37._r_single,38._r_single, 39._r_single,&
                         40._r_single, 41._r_single, 42._r_single, 43._r_single, 44._r_single, 45._r_single, 46._r_single, 47._r_single,48._r_single,49._r_single,&
                         50._r_single, 51._r_single, 52._r_single, 53._r_single,54._r_single, 55._r_single, 56._r_single, 57._r_single, 58._r_single,59._r_single, 60._r_single /)
 !!!!!!!!!!!!!!!
  min_error = 1.25_r_kind

  if (input_impact <= interp_impact(1)) then
    error = a(1)*(interp_impact(1) - interp_impact(1))**3 + b(1)*(interp_impact(1)-interp_impact(1))**2 + &
            c(1)*(interp_impact(1)-interp_impact(1)) + d(1)
  else if  (input_impact > interp_impact(67)) then
    error = 48.79740724637614_r_kind
  else
    level_index = 1
    do l=1,size(interp_impact)
      if (interp_impact(l) >= input_impact) then
        level_index = l
        exit
      else
        continue
      endif
    enddo
    interp_err = a(level_index-1)*(input_impact - interp_impact(level_index-1))**3 + &
                 b(level_index-1)*(input_impact-interp_impact(level_index-1))**2 + &
                 c(level_index-1)*(input_impact-interp_impact(level_index-1)) + d(level_index-1)
    if (input_impact > 9.0_r_kind .and. input_impact < 11.0_r_kind) then
      error = ((11.0_r_kind - input_impact)/2.0_r_kind)*interp_err + ((input_impact - 9.0_r_kind)/2.0_r_kind)*min_error
    else if (input_impact > 29.0_r_kind .and. input_impact < 31.0_r_kind) then
      error = ((31.0_r_kind - input_impact)/2.0_r_kind)*min_error + ((input_impact - 29.0_r_kind)/2.0_r_kind)*interp_err
    else
      if (input_impact <= 9.0_r_kind .or. input_impact >= 31.0_r_kind) then
        error = interp_err
      else
        error = min_error
      endif
    endif
  endif
  return
end subroutine compute_threeCH_uncert

subroutine init_netcdf_diag_
  integer(i_kind) ncd_fileid, ncd_nobs
  character(len=80) string
  character(len=128) diag_conv_file
  logical append_diag  
  logical,parameter::verbose=.false.

     write(string,900) jiter
900  format('conv_gps_',i2.2,'.nc4')
     diag_conv_file=trim(dirname) // trim(string)

     inquire(file=diag_conv_file, exist=append_diag)

     if (append_diag) then
        call nc_diag_read_init(diag_conv_file,ncd_fileid)
        ncd_nobs = nc_diag_read_get_dim(ncd_fileid,'nobs')
        call nc_diag_read_close(diag_conv_file)

        if (ncd_nobs > 0) then
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists.  Appending.  nobs,mype=',ncd_nobs,mype
        else
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists but contains no obs.  Not appending. nobs,mype=',ncd_nobs,mype
           append_diag = .false. ! if there are no obs in existing file, then do not try to append
        endif
     end if

     call nc_diag_init(diag_conv_file, append=append_diag)

     if (.not. append_diag) then ! don't write headers on append - the module will break?
        call nc_diag_header("date_time",ianldate )
        if (save_jacobian) then
          nnz   = 3*nsig
          nind  = 3
          call nc_diag_header("jac_nnz", nnz)
          call nc_diag_header("jac_nind", nind)
        endif
     endif
end subroutine init_netcdf_diag_

subroutine contents_binary_diag_
end subroutine contents_binary_diag_

subroutine contents_netcdf_diag_
  use sparsearr, only: sparr2, readarray, fullarray
  integer(i_kind),dimension(miter) :: obsdiag_iuse
  integer(i_kind)                  :: obstype, obssubtype
  type(sparr2) :: dhx_dx

! Observation class
  character(7),parameter     :: obsclass = '    gps'

           call nc_diag_metadata("Station_ID",                            gps_allptr%cdiag             )
           call nc_diag_metadata("Observation_Class",                     obsclass                     )
           obstype    = gps_allptr%rdiag(1) 
           obssubtype = gps_allptr%rdiag(2)
           call nc_diag_metadata("Observation_Type",                      obstype                      )
           call nc_diag_metadata("Observation_Subtype",                   obssubtype                   )
           call nc_diag_metadata_to_single("Latitude",                    gps_allptr%rdiag(3)          )
           call nc_diag_metadata_to_single("Longitude",                   gps_allptr%rdiag(4)          )
           call nc_diag_metadata_to_single("Incremental_Bending_Angle",   gps_allptr%rdiag(5)          )
           call nc_diag_metadata_to_single("Pressure",                    gps_allptr%rdiag(6)          )
           call nc_diag_metadata_to_single("Height",                      gps_allptr%rdiag(7)          )
           call nc_diag_metadata_to_single("Time",                        gps_allptr%rdiag(8)          )
           call nc_diag_metadata_to_single("Model_Elevation",             gps_allptr%rdiag(9)          )
           call nc_diag_metadata_to_single("Setup_QC_Mark",               gps_allptr%rdiag(10)         )
           call nc_diag_metadata_to_single("Prep_Use_Flag",               gps_allptr%rdiag(11)         )
           call nc_diag_metadata_to_single("Analysis_Use_Flag",           gps_allptr%rdiag(12)         )

           call nc_diag_metadata_to_single("Nonlinear_QC_Rel_Wgt",        gps_allptr%rdiag(13)         )
           call nc_diag_metadata_to_single("Errinv_Input",                gps_allptr%rdiag(14)         )
           call nc_diag_metadata_to_single("Errinv_Adjust",               gps_allptr%rdiag(15)         )
           call nc_diag_metadata_to_single("Errinv_Final",                gps_allptr%rdiag(16)         )
           call nc_diag_metadata_to_single("Observation",                 gps_allptr%rdiag(17)         )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_adjusted", gps_allptr%rdiag(17),gps_allptr%rdiag(5),"*")
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_unadjusted",gps_allptr%rdiag(17),gps_allptr%rdiag(5),"*")
           call nc_diag_metadata_to_single("GPS_Type",                    gps_allptr%rdiag(20)         )
           call nc_diag_metadata_to_single("Temperature_at_Obs_Location", gps_allptr%rdiag(18)         )
           call nc_diag_metadata_to_single("Specific_Humidity_at_Obs_Location",gps_allptr%rdiag(21)    )

           call nc_diag_metadata_to_single("Frac_LSW",                    gps_allptr%rdiag(23)   )
           call nc_diag_metadata_to_single("STD4060",                     gps_allptr%rdiag(24)   )
           call nc_diag_metadata_to_single("LSWFlag",                     gps_allptr%rdiag(25)   )

           if (save_jacobian) then
              call readarray(dhx_dx, gps_allptr%rdiag(ioff+1:nreal))
              call nc_diag_data2d("Observation_Operator_Jacobian_stind", dhx_dx%st_ind(1:dhx_dx%nind))
              call nc_diag_data2d("Observation_Operator_Jacobian_endind", dhx_dx%end_ind(1:dhx_dx%nind))
              call nc_diag_data2d("Observation_Operator_Jacobian_val", real(dhx_dx%val(1:dhx_dx%nnz),r_single))
           endif



!           call nc_diag_data2d("T_Jacobian",                              gps_allptr%mmpoint%jac_t             )
           if (lobsdiagsave) then
              print *,'ERROR: OBSDIAGSAVE SKIPPED IN NCDIAG DEVELOPMENT.  STOPPING.'
              call stop2(55)
!              do jj=1,miter
!                 if (gps_allptr%diags%muse(jj)) then
!                    obsdiag_iuse(jj) =  one
!                 else
!                    obsdiag_iuse(jj) = -one
!                 endif
!              enddo
!
!              call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse                             )
!              call nc_diag_data2d("ObsDiagSave_nldepart", gps_allptr%diags%nldepart )
!              call nc_diag_data2d("ObsDiagSave_tldepart", gps_allptr%diags%tldepart )
!              call nc_diag_data2d("ObsDiagSave_obssen",   gps_allptr%diags%obssen   )
           endif
end subroutine contents_netcdf_diag_
end subroutine genstats_gps

end module m_gpsStats
