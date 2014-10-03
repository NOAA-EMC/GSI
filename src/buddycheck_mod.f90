module buddycheck_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    adjust_cloudobs_mod
!   prgmmr: carley          org: np22                date: 2014-09-12
!
! abstract: Module contains routines performing buddychecks
!
! subroutines included:
!   sub buddy_check_t          -   routine to call from setupt to perform buddy check on
!                                   T innovations
!   sub execute_buddy_check    -   private routine which performs actual buddy check operation
!                                   (e.g. is called by buddy_check_t)
!
! functions included:
!   gc_dist:                   -   returns the great circle distance (m) between 
!                                   two pairs of lat/lon points
!
! attributes:
!   language: f90/95/2003
!   machine:  WCOSS
!
!$$$ end documentation block

  implicit none

! set default to private
!  private
! set subroutines to public
  public :: buddy_check_t
  public :: execute_buddy_check
  public :: gc_dist

contains

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  buddy_check_t --- Perform buddy check for temperature observations
!
! !INTERFACE:
!
subroutine buddy_check_t(is,data,luse,mype,nele,nobs,muse,buddyuse)

! !USES:

  use kinds, only: r_kind,i_kind
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use gridmod, only: nsig,twodvar_regional,regional
  use jfunc, only: jiter
  use guess_grids, only: nfldsig, hrdifsig,ges_lnprsl,&
       geop_hgtl,ges_tsen,pt_ll
  use constants, only: zero,one,r10
  use obsmod, only: bmiss,sfcmodel  
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  use aircraftinfo, only: aircraft_t_bc_pof,aircraft_t_bc, &
       aircraft_t_bc_ext
  use convinfo, only: ictype
  implicit none

! !INPUT PARAMETERS:

  real(r_kind)                                     , intent(in   ) :: data(nele,nobs)  ! data array containing all observations
  integer(i_kind)                                  , intent(in   ) :: mype    ! mpi task id
  integer(i_kind)                                  , intent(in   ) :: nele    ! number of data elements per observation
  integer(i_kind)                                  , intent(in   ) :: nobs    ! number of observations
  integer(i_kind)                                  , intent(in   ) :: is      ! ndat index
  logical                                          , intent(in   ) :: luse(nobs),muse(nobs)
! !OUTPUT PARAMETERS:
  integer(i_kind)                                  , intent(  out) :: buddyuse(nobs) !Holds info pertaining to buddycheck. 
                                                                                     !  1 = pass
                                                                                     !  0 = buddy check no performed
                                                                                     ! -1 = fail
! !INPUT/OUTPUT PARAMETERS:
!
!
! !DESCRIPTION:  This routine creates temperature innovations and then performs a simple two-pass 
!                  buddy check on the innovations.  An array indicating which observations have
!                  passed, failed, or were not checked is returned.
!
!
!
! !REVISION HISTORY:
!
!   2014-09-05  Carley - Originator. Initial version based off setupt.f90 to obtain innovations
!
! !REMARKS:
!   language: f90/95
!   machine:  WCOSS
!
! !AUTHOR: 
!   Carley          org: np22                date: 2014-09-05
!
!EOP
!-------------------------------------------------------------------------

! Declare local parameters
  real(r_kind),parameter:: r0_001 = 0.001_r_kind
  real(r_kind),parameter:: r0_7=0.7_r_kind
  real(r_kind),parameter:: r8 = 8.0_r_kind

  character(len=*),parameter :: myname='buddy_check_t'

! Declare external calls for code analysis
  external:: SFC_WTQ_FWD
  external:: get_tlm_tsfc
  external:: tintrp2a1,tintrp2a11
  external:: tintrp31
  external:: grdcrd1
  external:: stop2

! Declare local variables

  

  real(r_kind) rsig
  real(r_kind) psges
  real(r_kind) tges
  real(r_kind) tob
  real(r_kind) dlon,dlat,dtime,dpres,prest

  real(r_kind),dimension(nsig):: prsltmp

  real(r_kind) tgges,roges
  real(r_kind),dimension(nsig):: tvtmp,qtmp,utmp,vtmp,hsges
  real(r_kind),dimension(nobs,5):: vals ! innovation, lat, lon, elev., usage from read_prepbufr
  real(r_kind) u10ges,v10ges,t2ges,q2ges,psges2,f10ges,range,difmax

  real(r_kind),dimension(nsig):: prsltmp2

  integer(i_kind) ii
  integer(i_kind) mm1
  integer(i_kind) itype,msges,iqt,i
  integer(i_kind) ier,ilon,ilat,ipres,itob,id,itime,ikx,iqc,iptrb,icat,ipof,ivvlc,idx
  integer(i_kind) ier2,iuse,ilate,ilone,ikxx,istnelv,iobshgt,izz,iprvd,isprvd
  integer(i_kind) regime,istat
  integer(i_kind) idomsfc,iskint,iff10,isfcr
  
  logical sfctype
  logical iqtflg

  logical:: in_curbin, in_anybin
  logical proceed
 
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_u
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_v
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_q

  print*, myname,': JRC Entered with PE',mype

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

!    index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  itob=5      ! index of t observation
  id=6        ! index of station id
  itime=7     ! index of observation time in data array
  ikxx=8      ! index of ob type
  iqt=9       ! index of flag indicating if moisture ob available
  iqc=10      ! index of quality mark
  ier2=11     ! index of original-original obs error ratio
  iuse=12     ! index of use parameter
  idomsfc=13  ! index of dominant surface type
  iskint=14   ! index of surface skin temperature
  iff10=15    ! index of 10 meter wind factor
  isfcr=16    ! index of surface roughness
  ilone=17    ! index of longitude (degrees)
  ilate=18    ! index of latitude (degrees)
  istnelv=19  ! index of station elevation (m)
  iobshgt=20  ! index of observation height (m)
  izz=21      ! index of surface height
  iprvd=22    ! index of observation provider
  isprvd=23   ! index of observation subprovider
  icat=24     ! index of data level category
  if (aircraft_t_bc_pof .or. aircraft_t_bc .or. aircraft_t_bc_ext) then
     ipof=25     ! index of data pof
     ivvlc=26    ! index of data vertical velocity
     idx=27      ! index of tail number
     iptrb=28    ! index of t perturbation
  else
     iptrb=25    ! index of t perturbation
  end if


  rsig=float(nsig)
  mm1=mype+1

  !initialize buddyuse to 1, start by assuming all obs are good!
  buddyuse=1
  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if( (.not.in_anybin) .or. (.not.in_curbin) .or. (.not. luse(i)) .or. (.not. muse(i)) ) then
       !If outside the time window - don't run the buddy check since we will not be using this ob anyway
       ! Also do not run if muse or luse is false
       buddyuse(i)=0
       vals(i,1)=bmiss
       vals(i,2)=data(ilate,i)
       vals(i,3)=data(ilone,i)
       vals(i,4)=data(iobshgt,i)
       vals(i,5)=data(iuse,i)
       cycle
     end if   
         
     ! Convert obs lats and lons to grid coordinates
     dlat=data(ilat,i)
     dlon=data(ilon,i)
     dpres=data(ipres,i)
     ikx=nint(data(ikxx,i))
     itype=ictype(ikx)
     prest=r10*exp(dpres)     ! in mb
     sfctype=(itype>179.and.itype<190).or.(itype>=192.and.itype<=199)  
     iqtflg=nint(data(iqt,i)) == 0
!    Load observation value
     tob=data(itob,i)    

! Interpolate log(ps) & log(pres) at mid-layers to obs locations/times
     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
          mype,nfldsig)
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
          nsig,mype,nfldsig)

!    Put obs pressure in correct units to get grid coord. number
     call grdcrd1(dpres,prsltmp(1),nsig,-1)

! Implementation of forward model ----------

     if(sfctype.and.sfcmodel) then
        tgges=data(iskint,i)
        roges=data(isfcr,i)

        msges = 0
        if(itype == 180 .or. itype == 182 .or. itype == 183) then    !sea
           msges=0
        elseif(itype == 181 .or. itype == 187 .or. itype == 188) then  !land
           msges=1
        endif

        call tintrp2a1(ges_tv,tvtmp,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
        call tintrp2a1(ges_q,qtmp,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
        call tintrp2a1(ges_u,utmp,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
        call tintrp2a1(ges_v,vtmp,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
        call tintrp2a1(geop_hgtl,hsges,dlat,dlon,dtime,hrdifsig,&
             nsig,mype,nfldsig)
  
        psges2  = psges          ! keep in cb
        prsltmp2 = exp(prsltmp)  ! convert from ln p to cb
        call SFC_WTQ_FWD (psges2, tgges,&
             prsltmp2(1), tvtmp(1), qtmp(1), utmp(1), vtmp(1), &
             prsltmp2(2), tvtmp(2), qtmp(2), hsges(1), roges, msges, &
             f10ges,u10ges,v10ges, t2ges, q2ges, regime, iqtflg)
        tges = t2ges

     else
        if(iqtflg)then
!          Interpolate guess tv to observation location and time
           call tintrp31(ges_tv,tges,dlat,dlon,dpres,dtime, &
                hrdifsig,mype,nfldsig)

        else
!          Interpolate guess tsen to observation location and time
           call tintrp31(ges_tsen,tges,dlat,dlon,dpres,dtime, &
                hrdifsig,mype,nfldsig)
        end if
     endif
    
     if(sfctype.and.sfcmodel)  dpres = one     ! place sfc T obs at the model sfc

     if (dpres > rsig )then
        if( regional .and. prest > pt_ll )then
           dpres=rsig
        else
           !Do no use obs outside the domain
           buddyuse(i)=0
           vals(i,1)=bmiss
           vals(i,2)=data(ilate,i)
           vals(i,3)=data(ilone,i)
           vals(i,4)=data(iobshgt,i)
           vals(i,5)=data(iuse,i)
           cycle
        endif
     endif

! Compute innovation and store remaining data
     vals(i,1) = tob-tges
     vals(i,2)=data(ilate,i)
     vals(i,3)=data(ilone,i)
     vals(i,4)=data(iobshgt,i)
     vals(i,5)=data(iuse,ii)     
! End of loop over observations
  end do
! Release memory of local guess arrays
  call final_vars_
  ! - Now call buddy check routine
  range = 108000.0_r_kind ! Radius within which we check for an ob's buddies (units are m)
  difmax= 8.0_r_kind      ! Max difference allowed relative to buddies

  print*, myname,': JRC - Calling execute_buddy_check'
  
  call execute_buddy_check(mype,is,nobs,vals,range,difmax,buddyuse)


! End of routine

  return
  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::u' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::v' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::tv', ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::q', ivar, istatus )
  proceed=proceed.and.ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get ps ...
     varname='ps'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_z))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_ps(size(rank2,1),size(rank2,2),nfldsig))
         ges_ps(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_ps(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get u ...
     varname='u'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_u))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_u(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_u(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_u(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get v ...
     varname='v'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_v))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_v(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_v(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_v(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get tv ...
     varname='tv'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_tv))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_tv(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_tv(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_tv(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get q ...
     varname='q'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_q))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_q(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_q(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_q(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine final_vars_
    if(allocated(ges_q )) deallocate(ges_q )
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_v )) deallocate(ges_v )
    if(allocated(ges_u )) deallocate(ges_u )
    if(allocated(ges_ps)) deallocate(ges_ps)
  end subroutine final_vars_

end subroutine buddy_check_t



subroutine execute_buddy_check(mype,is,numobs,pevals,range,difmax,pebuddyuse)

! !USES:

  use mpimod, only: ierror,mpi_rtype,mpi_itype,mpi_sum,mpi_comm_world
  use kinds, only: r_kind,i_kind
  use gridmod, only: nsig,twodvar_regional,regional
  use constants, only: zero, one,one_tenth,r100,tiny_r_kind
  use obsmod, only: obs_sub_comm,bmiss

  implicit none

! !INPUT PARAMETERS:
  real(r_kind)                                     , intent(in   ) :: pevals(numobs,5)  ! data array containing all observations
  real(r_kind)                                     , intent(in   ) :: range,& ! Radius within which we check for an ob's buddies (units are m)
                                                                      difmax  ! Max difference allowed relative to buddies
  integer(i_kind)                                  , intent(in   ) :: mype    ! mpi task id
  integer(i_kind)                                  , intent(in   ) :: numobs  ! number of observations on this task
  integer(i_kind)                                  , intent(in   ) :: is      ! ndat index
! !OUTPUT PARAMETERS:
  integer(i_kind)                                  , intent(inout) :: pebuddyuse(numobs) !Holds info pertaining to buddycheck. 
                                                                                     !  1 = pass
                                                                                     !  0 = buddy check no performed
                                                                                     ! -1 = fail
! !INPUT/OUTPUT PARAMETERS:
!
!
! !DESCRIPTION:  This routine performs a simple two-pass buddy check on provided innovations.  
!                   An array indicating which observations have passed, failed, or were not 
!                   checked is returned.
!
!                This buddy check algorithm is based upon that found in the WRFDA 
!                   routine da_buddy_qc, written by Yong-Run Guo 10/10/2008.
!
!
! !REVISION HISTORY:
!
!   2014-09-09  Carley - Originator. Initial version based off setupt.f90.  Methods are
!                        based on WRFVAR code var/da/da_tools/da_buddy_qc.inc (Yong-Run Guo)
!
! !REMARKS:
!   language: f90/95
!   machine:  WCOSS
!
! !AUTHOR: 
!   Carley          org: np22                date: 2014-09-09
!
!EOP
!-------------------------------------------------------------------------


! External routines
  external:: mpi_allgather
  external:: mpi_allreduce
  external:: mpi_allgatherv


! Declare local variables

  real(r_kind) :: average,myinnov,mylat,mylon,myelev,sum,&
                  diff_check_1,diff_check_2,diff_j,err,fact
  real(r_kind) :: distance,vdist
  integer(i_kind) :: i,j,buddy_num,numobs_global,kob,buddy_num_final
  integer(i_kind) :: submm1,mynpe,ji,newrank,rem,amount,mydata
  integer(i_kind),allocatable,dimension(:) :: buddyuse_global,buddyuse,idisp,ircnt
  integer(i_kind),allocatable,dimension(:) :: locdisp,locsendcnt
  real(r_kind) :: pevals1d(numobs*5)
  real(r_kind),allocatable,dimension(:,:) :: vals_global,vals
  real(r_kind),allocatable,dimension(:) :: vals1dglob,myvals1d,diff
  real(r_kind),allocatable,dimension(:) :: innovsglobal,peinnovs,tmp1d,dum
  character(len=*),parameter :: myname='execute_buddy_check'

  ! Obtain the number of tasks here by using the communicator setup in obs_para.f90
  ! Note that the only communicator that gets activated here is the one associated with nobs>0
  !  since only pe's with obs on their subdomains will execute any of the setup* routines within
  !  setuprhsall.f90

  call mpi_comm_size(obs_sub_comm(is), mynpe, ierror) 
  call mpi_comm_rank(obs_sub_comm(is), newrank, ierror)
  submm1=newrank+1
  allocate(idisp(mynpe),ircnt(mynpe),locdisp(mynpe),locsendcnt(mynpe))
  idisp=0;ircnt=0

  call mpi_allgather(numobs,1,mpi_itype,ircnt,1,mpi_itype,obs_sub_comm(is),ierror)


  ! Now we need to calculate the displacement array here since we can't use the one from the
  !  global obsveration data array since that one is not ordered in any particular manner
  !  and nor does it necessarily matter (i.e. ob at n=1 could be in Oregon,
  !  ob at n=2 may be in Florida, and then ob at n=3 might be back in Oregon)

  idisp(1)=0  !first spot will always be zero
  if (mynpe >=2) then  
     do i=2,mynpe
        idisp(i)=idisp(i-1)+ircnt(i-1)
     end do
  end if
!  write(6,1000)myname,'DISPLS:',mype,(idisp(i),i=1,mynpe)
!  write(6,1000)myname,' IRCNT:',mype,(ircnt(i),i=1,mynpe)
!1000  format(2A10,1x,I3,1x,8I10,/,(10X,10I10))  

  ! Get the total, global number of obs/innovations we need to consider

  call mpi_allreduce(numobs,numobs_global,1,mpi_itype,mpi_sum,obs_sub_comm(is),ierror)

 !  write(6,'(2A,I3,1x,I10)')myname,'mype,numobs_global',mype,numobs_global

  ! Allocate the memory for everything and initialize certain fields to zero
  allocate(vals_global(numobs_global,5),buddyuse_global(numobs_global),vals1dglob(numobs_global*5),diff(numobs_global))
  vals_global=zero

  ! Gather all pevals(:,:) on subdomains into vals_global on every task, noting that pevals takes the following
  !   organization:
  !                pevals(i,1)=ob-ges
  !                pevals(i,2)=earth lat
  !                pevals(i,3)=earth lon
  !                pevals(i,4)=ob height
  !                pevals(i,5)=rusage
     

  !put pevals in a 1d array for easy mpi comms
  ji=0
  do i=1,numobs       
     do j=1,5
        ji=ji+1
        pevals1d(ji)=pevals(i,j)
     end do
  end do

  call mpi_allgatherv(pevals1d,numobs*5,mpi_rtype,vals1dglob,ircnt*5,idisp*5,mpi_rtype,obs_sub_comm(is),ierror)

   ! put vals1dglob to 2d vals_glob so things are more human-readable
  ji=0
  do i=1,numobs_global       
     do j=1,5
        ji=ji+1
        vals_global(i,j)=vals1dglob(ji)
     end do
  end do
  ! Gather all pebuddyuse params into a global copy (buddyuse_global) that all tasks have

  call mpi_allgatherv(pebuddyuse,numobs,mpi_itype,buddyuse_global,ircnt,idisp,mpi_itype,obs_sub_comm(is),ierror)


! Alright - now this may seem silly but we need to scatter obs to new subdomains because there
!           is a severe load imbalance issue otherwise (some tasks have 10s of obs and others have
!           1000s or more).


! First figure out the best decomposition given the tasks present *in this routine*

  rem=mod(numobs_global,mynpe)
  amount=int(numobs_global/mynpe)
  do i=1,mynpe
     locsendcnt(i)=amount
  end do
  if (rem /= 0 .and. mynpe>1) locsendcnt(mynpe)=locsendcnt(mynpe)+rem

  if (newrank==0) then
     write(6,1000)myname,' obs per pe ob subdomain',(locsendcnt(i),i=1,mynpe)
1000  format(2A,1x,1x,8I10,/,(10X,10I10))  
  end if
  
  locdisp=0
  locdisp(1)=0  !first spot will always be zero
  if (mynpe >=2) then  
     do i=2,mynpe
        locdisp(i)=locdisp(i-1)+locsendcnt(i-1)
     end do
  end if

! Now scatterv vals1dglob to workers for better load balance

  mydata=locsendcnt(submm1)
  allocate(myvals1d(mydata*5),vals(mydata,5),buddyuse(mydata))
  myvals1d=0
  call mpi_scatterv(vals1dglob,locsendcnt*5,locdisp*5,mpi_rtype,myvals1d,mydata*5,mpi_rtype,0,obs_sub_comm(is),ierror) !carley bugfix (must scale displ by 5)

  ji=0
  do i=1,mydata     
     do j=1,5
        ji=ji+1
        vals(i,j)=myvals1d(ji)
     end do
  end do

! Scatter the buddy use array
  call mpi_scatterv(buddyuse_global,locsendcnt,locdisp,mpi_itype,buddyuse,mydata,mpi_itype,0,obs_sub_comm(is),ierror)
  


 ! Now - finally - start what we came her for originally, the buddy check!

 ! Begin with main loop through all obs on pe subdomain
  main: do i=1,mydata

    average = zero
    if (vals(i,5)>= r100 .or. buddyuse(i)==0 .or. (abs(vals(i,1)-bmiss)<=tiny_r_kind) )  then    ! No buddy check is applied to bad data (rusage => 100)       
        buddyuse(i)=0                                 ! Do not run buddy check if buddyuse is already 0 - this ob may have luse=.false., be outside the timewindow, etc 
       cycle main
    end if

    !  Now find the distance between all pairs of innovations (numobs_global) and get the number
    !  within our range, find number of observations within a given distance, 
    !  and store that innovation

    buddy_num = 0
    myinnov=vals(i,1)
    mylat=vals(i,2) 
    mylon=vals(i,3)
    myelev=vals(i,4)
    diff=zero
    inner: do j = 1, numobs_global
       if (buddyuse_global(j)==0 .or. vals_global(j,5) >= r100 .or. (abs(vals_global(i,1)-bmiss)<=tiny_r_kind) ) cycle inner !do not use buddies who are not eligible
    
       distance=gc_dist(mylat,mylon,vals_global(j,2),vals_global(j,3))  !units are meters       
       vdist=abs(myelev-vals_global(j,4))                               !units are meters
       ! distance must be greater than 0.1 so we don't include the test ob in the buddy list
       if (distance <= range .and. distance >= one_tenth .and. vdist <= 200._r_kind) then
!          if (submm1==mynpe) write(6,'(A,f7.3,1x,f7.3,A,f7.3,1x,f7.3,A,f10.3,A,f7.3)')'MyLatLon ',mylat,mylon,&
!                                ' BuddyLatlon ',vals_global(j,2),vals_global(j,3),' GCdist (km) ',   &
!                                distance/1000., ' vdist(m) ',vdist

          buddy_num = buddy_num + 1  ! We have found a buddy, so increment the counter
          ! innovation (O-B) ==> diff:
          diff(buddy_num)=vals_global(j,1) !save the innovation associated with this buddy
       end if
    end do inner

    !-  initialize buddy_num_final to 0
    buddy_num_final=0
    ! Sum all the buddys' innovations 

    sum = zero    
    do j = 1, buddy_num
       sum = sum + diff(j)
    end do 

    !  Check to see if there are any buddies, compute the mean innovation.

    if (buddy_num > 0) average = sum / buddy_num

 !  Check if there are any bad obs among the obs located within the 
 !  the radius surrounding the test ob.

    diff_check_1 = difmax*1.25_r_kind
    diff_check_2 = difmax

    if (buddy_num >= 2) then
       kob = buddy_num
       do j = 1, buddy_num        
          diff_j = abs(diff(j)-average)
          if ( abs(diff(j)) > diff_check_1 .and. diff_j > diff_check_2 ) then
             ! Bad obs:                 Innovation itself: diff(numj) > diff_check_1
             !        The distance between the innovation and average > diff_check_2
             kob = kob - 1
             sum = sum - diff(j)
          end if
       end do

       ! Set the final number of buddies here - after we've possibly removed some bad ones
       buddy_num_final = kob

       !  We may have removed too many observations.

       if (kob > 2) then
       !  Information for buddy check for specific obs(num)
          average = sum / kob
          err = myinnov - average
       else
       ! Not enough buddies, cycle back to the beginning of the main loop
          err = zero
          buddyuse(i)=0
          cycle main           
       end if

    else 
       ! Not enough buddies, cycle back to the beginning of the main loop
       buddyuse(i)=0
       cycle main       
    end if
 ! If the buddy number is ONLY 2, increase the tolerance value:
    fact=1.0
    if (buddy_num_final == 2) fact=1.2

!  Now do the final buddy check
     if (abs(err) > fact*difmax) then
        buddyuse(i) = -1
        if(mod(i,100)==0) write(6,'(f10.4,A,f10.4,A,I10,A,I10)')myinnov,'Fails! with err = ', err,' having ',buddy_num_final,' buddies. Local ob number: ',i 
     else
        buddyuse(i)=1
        if(mod(i,100)==0) write(6,'(f10.4,A,f10.4,A,I10,A,I10)')myinnov,'Passes! with err = ', err,' having ',buddy_num_final,' buddies. Local ob number: ',i         
     end if
  end do main


  !MPI comms to put buddyuse back on pebuddyuse
 
  ! mpi_gatherv
  buddyuse_global=0
  ! receive count and displacement will be identical to prior scatter send count
  call mpi_gatherv(buddyuse,mydata,mpi_itype,buddyuse_global,locsendcnt,locdisp,mpi_itype,0,obs_sub_comm(is),ierror)

  ! mpi_scatterv back to pe subdomains (pebuddyuse)
  ! use the ircnt and idisp arrays obtained earlier to put arrays on all procs.  They will be the same here
  pebuddyuse=0
  call mpi_scatterv(buddyuse_global,ircnt,idisp,mpi_itype,pebuddyuse,numobs,mpi_itype,0,obs_sub_comm(is),ierror)  

  !let's scatter the innovations back and compare against what they were originally to make sure MPI comms were done
  ! correctly

  ! mpi_gatherv
  allocate(innovsglobal(numobs_global),peinnovs(numobs),tmp1d(mydata),dum(numobs))
  innovsglobal=zero
  tmp1d=vals(:,1) !store innovs
  ! receive count and displacement will be identical to prior scatter send count
  call mpi_gatherv(tmp1d,mydata,mpi_rtype,innovsglobal,locsendcnt,locdisp,mpi_rtype,0,obs_sub_comm(is),ierror)
  !call mpi_allgatherv(tmp1d,mydata,mpi_rtype,locsendcnt,locdisp,innovsglobal,mpi_rtype,obs_sub_comm(is),ierror)

  if (newrank==0) write(6,'(A,2(f10.3,3x))')'gatherv check',maxval(innovsglobal-vals_global(:,1)),minval(innovsglobal-vals_global(:,1))

  ! mpi_scatterv back to pe subdomains (pebuddyuse)
  ! use the ircnt and idisp arrays obtained earlier to put arrays on all procs.  They will be the same here
  peinnovs=zero
  dum=-999.



  call mpi_scatterv(innovsglobal,ircnt,idisp,mpi_rtype,peinnovs,numobs,mpi_rtype,0,obs_sub_comm(is),ierror)
  dum=peinnovs-pevals(:,1)  
  write(6,'(2A,I3,A,F15.3,2x F15.3)'),myname,'mype',mype,': Max/mins of scatter/gather steps', maxval(dum),minval(dum) !,maxval(peinnovs),minval(peinnovs),maxval(pevals(:,1)),minval(pevals(:,1))  

  deallocate(innovsglobal,peinnovs,tmp1d)


  deallocate(vals_global,buddyuse_global,idisp,ircnt,vals1dglob,&
             locdisp,locsendcnt,myvals1d,vals,buddyuse,diff)
  return

 

  end subroutine execute_buddy_check

  real(r_kind) function gc_dist(inlat1,inlon1,inlat2,inlon2)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    gc_dist
  !   prgmmr:                  org:                     date:
  !
  ! abstract: Return the great circle distance (m) between a two pairs of lat/lon points
  !
  ! program history log:
  !   2014-09-09  carley - added subprogram doc block
  !
  !   input argument list:
  !   lat1,lon1,lat2,lon2 in degrees
  !
  !   output argument list:
  !    gc_dist
  !
  ! attributes:
  !   language: f90
  !   machine:
  !
  !$$$ end documentation block

  use constants, only: rearth,deg2rad,one,two
  use kinds, only: r_kind
  implicit none
  real(r_kind),intent (in) :: inlat1,inlon1,inlat2,inlon2
  real(r_kind) :: lat1,lon1,lat2,lon2
  real(r_kind) :: dLat,dLon,a,c

  lat2=inlat2*deg2rad
  lat1=inlat1*deg2rad
  lon1=inlon1*deg2rad
  lon2=inlon2*deg2rad
  dLat = lat2 - lat1
  dLon = lon2 - lon1
  a = sin(dLat / two) * sin(dLat / two) +  cos(lat1) * cos(lat2) * sin(dLon / two) * sin(dLon / two)
  c = two * atan2(sqrt(a), sqrt(one - a))
  gc_dist = rearth * c

  end function gc_dist  
end module buddycheck_mod


      
