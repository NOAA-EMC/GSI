subroutine read_goesndr(mype,val_goes,ithin,rmesh,jsatid,infile,&
     lunout,obstype,nread,ndata,nodata,twind,gstime,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_goesndr                   read goes sounder data
!   prgmmr: yang             org: np23                date: 1998-05-15
!
! abstract:  This routine reads GOES sounder radiance (brightness
!            temperature) files.  Optionally, the data are thinned to 
!            a specified resolution using simple quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   1998-05-15 weiyu yang
!   1999-08-24 derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-05-28 kleist - update subroutine call
!   2004-06-16 treadon - update documentation
!   2004-07-23 derber - make changes to eliminate obs. earlier in thinning
!   2004-07-29 treadon - add only to module use, add intent in/out
!   2005-01-26 derber - land/sea determination and weighting for data selection
!   2005-07-08 derber - clean up, fix bugs, and improve observation selection
!   2005-09-08  derber - modify to use input group time window
!   2005-09-28  derber - modify to produce consistent surface info
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-11-22  derber  - include mean in bias correction
!   2005-11-29  parrish - modify getsfc to work for different regional options
!   2006-02-01  parrish - remove getsfc (different version called now in read_obs)
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-03-07  derber  - combine reading of 1x1 and 5x5 (prepbufr) files
!   2006-04-27  derber - clean up code
!   2006-07-28  derber  - add solar and satellite azimuth angles remove isflg from output
!                       - add ability to read g9,g11,g13
!   2006-09-21  treadon - replace serial bufr i/o with parallel bufr i/o (mpi_io)
!
!   input argument list:
!     mype     - mpi task id
!     val_goes - weighting factor applied to super obs
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window(hours)
!     gstime   - guess time
!     sis      - sensor/instrument/satellite indicator
!     mype_root - "root" task for sub-communicator
!     mype_sub - mpi task id within sub-communicator
!     npe_sub  - number of data read tasks
!     mpi_comm_sub - sub-communicator for data read
!
!   output argument list:
!     nread    - number of BUFR GOES sounder observations read
!     ndata    - number of BUFR GOES sounder profiles retained for further processing
!     nodata   - number of BUFR GOES sounder observations retained for further processing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
             checkob,finalcheck
  use radinfo, only: cbias,newchn,predx
  use gridmod, only: diagnostic_reg,nlat,nlon,regional,tll2xy,txy2ll,rlats,rlons
  use constants, only: deg2rad,zero,one,izero,ione,rad2deg
  use obsmod, only: iadate
  use mpi_bufr_mod, only: mpi_openbf,mpi_closbf,nblocks,mpi_nextblock,&
       mpi_readmg,mpi_ireadsb,mpi_ufbint,mpi_ufbrep

  implicit none

! Declare passed variables
  character(10),intent(in):: infile,obstype,jsatid
  character(20),intent(in):: sis
  integer(i_kind),intent(in):: mype,lunout,ithin
  integer(i_kind),intent(inout):: ndata,nodata,nread
  real(r_kind),intent(in):: val_goes,rmesh,twind,gstime
  integer(i_kind)  ,intent(in) :: mype_root
  integer(i_kind)  ,intent(in) :: mype_sub
  integer(i_kind)  ,intent(in) :: npe_sub
  integer(i_kind)  ,intent(in) :: mpi_comm_sub


! Declare local parameters
  integer(i_kind),parameter:: maxinfo=18
  integer(i_kind),parameter:: mfov=25   ! maximum number of fovs (currently 5x5)

  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=50.0_r_kind
  real(r_kind),parameter:: tbmax=550.0_r_kind
  character(80),parameter:: hdstr = &
               'CLON CLAT ELEV SOEL BEARAZ SOLAZI SAID DINU YEAR MNTH DAYS HOUR MINU SECO ACAV' 
  character(80),parameter:: hdstr5 = &
               'XOB YOB ELEV SOEL BEARAZ SOLAZI SAID TYP ACAV DHR SID '
  character(80),parameter:: rbstr = 'TMBR'

! Declare local variables
  logical g8,g9,g10,g11,g12,g13,outside,iuse,g5x5

  character(8)  subset
  character(10) date

  integer(i_kind) ihh,kx,levs,idetect
  integer(i_kind) lnbufr,nchanl,nreal,iy,iret,ksatid
  integer(i_kind) jdate,idate,im,idd,iout
  integer(i_kind) ilat, ilon, isflg
  integer(i_kind) itx,k,i,itt,iskip,l,ifov,n
  integer(i_kind) ichan8,ich8
  integer(i_kind) nele,iscan,isc,nmind
  integer(i_kind) ntest,ireadmg,ireadsb
  integer(i_kind),dimension(5):: idate5

  real(r_kind) dlon,dlat,time,timedif,rflag,sstx,emiss
  real(r_kind) dlon_earth,dlat_earth
  real(r_kind) ch8,sstime
  real(r_kind) pred,crit1,tdiff,dist1
  real(r_kind) disterr,disterrmax,dlon00,dlat00,r01

  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_kind),allocatable,dimension(:):: data_crit
  integer(i_kind),allocatable,dimension(:):: idata_itx
  integer(i_kind):: mmblocks
  integer(i_kind):: isubset

  real(r_double),dimension(15):: hdr
  real(r_double),dimension(18):: grad



!**************************************************************************
! Start routine here.  Set constants.  Initialize variables
  lnbufr = 10
  disterrmax=zero
  ntest  = 0
  nreal  = maxinfo
  ich8   = 8        !channel 8
  ndata  = 0
  nchanl = 18
  ifov = -999
  r01 = 0.01_r_kind

  ilon=3
  ilat=4

  rlndsea(0) = 0._r_kind
  rlndsea(1) = 15._r_kind
  rlndsea(2) = 10._r_kind
  rlndsea(3) = 15._r_kind
  rlndsea(4) = 30._r_kind

! Make thinning grids
  call makegrids(rmesh)
 
!  check to see if prepbufr file

  g5x5 = jsatid == 'g08_prep' .or. jsatid == 'g09_prep' .or.     &
         jsatid == 'g10_prep' .or. jsatid == 'g11_prep' .or.     &
         jsatid == 'g12_prep' .or. jsatid == 'g13_prep'

! Set array index for surface-sensing channels
  ichan8  = newchn(sis, ich8)

! Open then read the bufr data
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)
  call readmg(lnbufr,subset,idate,iret)
  call closbf(lnbufr)
  close(lnbufr)
  if(iret/=0) goto 1000

! Compare obs time to analysis date.
  write(date,'( i10)') idate
  read (date,'(i4,3i2)') iy,im,idd,ihh
  if (mype_sub==mype_root) &
       write(6,*)'READ_GOESNDR:  bufr file date is ',iy,im,idd,ihh
  if(iy/=iadate(1).or.im/=iadate(2).or.idd/=iadate(3).or.&
       ihh/=iadate(4)) then
     write(6,*)'***READ_GOESNDR ERROR*** incompatable analysis ',&
          'and observation date/time'
     write(6,*)' year  anal/obs ',iadate(1),iy
     write(6,*)' month anal/obs ',iadate(2),im
     write(6,*)' day   anal/obs ',iadate(3),idd
     write(6,*)' hour  anal/obs ',iadate(4),ihh
     goto 1000
  end if


! Allocate arrays to hold data
  nele=nreal+nchanl
  allocate(data_all(nele,itxmax),data_crit(itxmax),idata_itx(itxmax))


! Open up bufr file for mpi-io access
  call mpi_openbf(infile,npe_sub,mype_sub,mpi_comm_sub)

! Big loop to read data file
  mpi_loop: do mmblocks=0,nblocks-1,npe_sub
     if(mmblocks+mype_sub.gt.nblocks-1) then
        exit
     endif
     call mpi_nextblock(mmblocks+mype_sub)
     block_loop: do
        call mpi_readmg(lnbufr,subset,idate,iret)
        if (iret /=0) exit
        read(subset,'(2x,i6)')isubset
        read_loop: do while (mpi_ireadsb(lnbufr)==0)


!    Extract type, date, and location information
     if(g5x5)then
!    Prepbufr file
       call mpi_ufbint(lnbufr,hdr,11,1,iret,hdstr5)
       kx = hdr(8)
       if(kx /= 164 .and. kx /= 165 .and. kx /= 174 .and. kx /= 175)cycle read_loop
!      If not goes data over ocean , read next bufr record
!      if(kx /= 174 .and. kx /= 175)cycle read_loop

       ksatid=hdr(7)
       g8  = ksatid==252 .and. jsatid=='g08_prep'
       g9  = ksatid==253 .and. jsatid=='g09_prep'
       g10 = ksatid==254 .and. jsatid=='g10_prep'
       g11 = ksatid==255 .and. jsatid=='g11_prep'
       g12 = ksatid==256 .and. jsatid=='g12_prep'
       g13 = ksatid==257 .and. jsatid=='g13_prep'
!      if not g8, g10, or g12 read next bufr record
       if (.not. g8  .and. .not. g9 .and. .not. g10 .and. .not. g11 .and.  &
           .not. g12 .and. .not. g13) cycle read_loop
!      Extract number of averaged FOVS
       ifov = hdr(9) ! number of averaged FOVS 
       if(ifov <= 3) cycle read_loop
!      Extract obs time difference. 
       tdiff=hdr(10)  ! relative obs time in hours
     else
!      GOES 1x1 or 5x5 file
       call mpi_ufbint(lnbufr,hdr,15,1,iret,hdstr)

       ksatid=hdr(7)   !bufr satellite id
       idetect=nint(hdr(8))
       if(obstype /= 'sndr' .and. ( &
         (obstype == 'sndrd1' .and. idetect /= 1) .or. &
         (obstype == 'sndrd2' .and. idetect /= 2) .or. &
         (obstype == 'sndrd3' .and. idetect /= 3) .or. &
         (obstype == 'sndrd4' .and. idetect /= 4)))cycle read_loop
       g8  = ksatid==252 .and. jsatid=='g08'
       g9  = ksatid==253 .and. jsatid=='g09'
       g10 = ksatid==254 .and. jsatid=='g10'
       g11 = ksatid==255 .and. jsatid=='g11'
       g12 = ksatid==256 .and. jsatid=='g12'
       g13 = ksatid==257 .and. jsatid=='g13'
!      if not g8, g10, or g12 read next bufr record
       if (.not. g8  .and. .not. g9 .and. .not. g10 .and.  .not. g11 .and.  &
           .not. g12 .and. .not. g13) cycle read_loop
       ifov = hdr(15) ! number of averaged FOVS 
       if(ifov < mfov .and. ifov > 0)then
        if(ifov <= 3) cycle read_loop
       end if

!      Extract obs time.  If not within analysis window, skip obs
!      Extract date information.  If time outside window, skip this obs
       idate5(1) = hdr(9) !year
       idate5(2) = hdr(10) !month
       idate5(3) = hdr(11) !day
       idate5(4) = hdr(12) !hour
       idate5(5) = hdr(13) !minute
       isc       = hdr(14) !second
       call w3fs21(idate5,nmind)
       sstime=float(nmind) + isc/60.0_r_kind
       tdiff=(sstime-gstime)/60.0_r_kind
     end if

!    If not within analysis window, skip obs
     if (abs(tdiff)>twind) cycle read_loop

!       Convert obs location to radians
     if (hdr(1)>=r360) hdr(1)=hdr(1)-r360
     if (hdr(1)< zero) hdr(1)=hdr(1)+r360

     dlon_earth = hdr(1)*deg2rad   !convert degrees to radians
     dlat_earth = hdr(2)*deg2rad

     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if(diagnostic_reg) then
           call txy2ll(dlon,dlat,dlon00,dlat00)
           ntest=ntest+1
           disterr=acosd(sin(dlat_earth)*sin(dlat00)+cos(dlat_earth)*cos(dlat00)* &
                (sin(dlon_earth)*sin(dlon00)+cos(dlon_earth)*cos(dlon00)))
           disterrmax=max(disterrmax,disterr)
        end if
      
!          Check to see if in domain
        if(outside) cycle read_loop

     else
        dlon = dlon_earth 
        dlat = dlat_earth 
        call grdcrd(dlat,1,rlats,nlat,1)
        call grdcrd(dlon,1,rlons,nlon,1)
     endif


!    Set common predictor parameters

     timedif = 6.0_r_kind*abs(tdiff)        ! range:  0 to 18

!    Increment goes sounder data counter
!    Extract brightness temperatures
     call mpi_ufbint(lnbufr,grad,1,18,levs,rbstr)

     iskip = 0
     do l=1,nchanl

        if( grad(l) < tbmin .or. grad(l) > tbmax )then
           iskip = iskip + 1
           if(l == ich8)iskip = nchanl
        else
           nread=nread+1
        endif
     end do

     if( iskip >= nchanl )cycle read_loop

     crit1=0.01_r_kind+timedif
     if(ifov < mfov .and. ifov > 0)then
       crit1=crit1+2.0_r_kind*float(mfov-ifov)
     end if

     call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse)
     if(.not. iuse)cycle read_loop


!    "Score" observation.   We use this information to id "best" obs.

!    Locate the observation on the analysis grid.  Get sst and land/sea/ice
!    mask.  

!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed 
!      sfcpct(0:3)- percentage of 4 surface types
!                 (0) - sea percentage
!                 (1) - land percentage
!                 (2) - sea ice percentage
!                 (3) - snow percentage

     call deter_sfc(dlat,dlon,isflg,sfcpct,sstx)

!      If not goes data over ocean , read next bufr record
     if(isflg /= 0) cycle read_loop

     crit1 = crit1 + rlndsea(isflg)  
     call checkob(dist1,crit1,itx,iuse)
     if(.not. iuse)cycle read_loop

!    Set data quality predictor
     iscan   = nint(hdr(3))+0.001_r_kind   ! "scan" position
     ch8     = grad(ich8) -cbias(iscan,ichan8) - r01*predx(ichan8,1)
     emiss=0.992-0.013*(hdr(3)/65.)**3.5-0.026*(hdr(3)/65.)**7.0
     pred = abs(ch8-sstx*emiss)

!    Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
     crit1 = crit1+10.0_r_kind*pred  

     call finalcheck(dist1,crit1,ndata,itx,iout,iuse,sis)
     if(.not. iuse)cycle read_loop

!    Transfer observation location and other data to local arrays

     data_all(1,iout) = ksatid                       ! satellite id
     data_all(2,iout) = tdiff                        ! time (hours)
     data_all(3,iout) = dlon                         ! grid relative longitude
     data_all(4,iout) = dlat                         ! grid relative latitude
     data_all(5,iout) = hdr(3)*deg2rad               ! satellite zenith angle
     data_all(6,iout) = hdr(5)                       ! satellite zenith angle
     data_all(7,iout) = zero                         ! local zenith angle
     data_all(8,iout) = iscan                        ! "scan" position
     data_all(9,iout) = hdr(4)                       ! solar zenith angle
     data_all(10,iout) = hdr(6)                       ! solar zenith angle
     data_all(11,iout)= sfcpct(0)                    ! ocean percentage
     data_all(12,iout)= sfcpct(1)                    ! land percentage
     data_all(13,iout)= sfcpct(2)                    ! ice percentage
     data_all(14,iout)= sfcpct(3)                    ! snow percentage
     data_all(15,iout)= dlon_earth*rad2deg           ! earth relative longitude (degrees)
     data_all(16,iout)= dlat_earth*rad2deg           ! earth relative latitude (degrees)

     data_all(17,iout)= val_goes
     data_all(18,iout)= itt
     do k=1,nchanl
        data_all(k+nreal,iout)=grad(k)
     end do


     idata_itx(iout) = itx                  ! thinning grid location
     data_crit(iout) = crit1*dist1          ! observation "score"

  end do read_loop
end do block_loop
end do mpi_loop

! If multiple tasks read input bufr file, allow each tasks to write out
! information it retained and then let single task merge files together

  if (npe_sub>1) &
       call combine_radobs(mype,mype_sub,mype_root,npe_sub,mpi_comm_sub,&
       nele,itxmax,nread,ndata,data_all,data_crit,idata_itx)


! Allow single task to check for bad obs, update superobs sum,
! and write out data to scratch file for further processing.
  if (mype_sub==mype_root) then

!    Identify "bad" observation (unreasonable brightness temperatures).
!    Update superobs sum according to observation location

     do n=1,ndata
        do i=1,nchanl
           if(data_all(i+nreal,n) > tbmin .and. &
                data_all(i+nreal,n) < tbmax)nodata=nodata+1
        end do
        itt=nint(data_all(nreal,n))
        super_val(itt)=super_val(itt)+val_goes
     end do

!    Write final set of "best" observations to output file
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  
  endif

! Close bufr file
  call mpi_closbf
  call closbf(lnbufr)

! Deallocate data arrays
  deallocate(data_all,data_crit,idata_itx)


! Deallocate satthin arrays
1000 continue
  call destroygrids

  if(diagnostic_reg .and. ntest>0 .and. mype_sub==mype_root) &
       write(6,*)'READ_SSMI:  mype,ntest,disterrmax=',&
       mype,ntest,disterrmax


! End of routine
  return
end subroutine read_goesndr
