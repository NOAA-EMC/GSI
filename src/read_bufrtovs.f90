subroutine read_bufrtovs(mype,val_tovs,ithin,isfcalc,&
     rmesh,jsatid,gstime,infile,lunout,obstype,&
     nread,ndata,nodata,twind,sis, &
     mype_root,mype_sub,npe_sub,mpi_comm_sub, &
     mype_sub_read,npe_sub_read,lll)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_bufrtovs                  read bufr tovs 1b data
!   prgmmr: treadon          org: np23                date: 2003-09-13
!
! abstract:  This routine reads BUFR format TOVS 1b radiance 
!            (brightness temperature) files.  Optionally, the data 
!            are thinned to a specified resolution using simple 
!            quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2003-09-13 treadon
!   2004-05-28 kleist  - update subroutine call
!   2004-06-16 treadon - update documentation
!   2004-07-23 derber  - make changes to eliminate obs. earlier in thinning
!   2004-07-29 treadon - add only to module use, add intent in/out
!   2004-10-15 derber  - various changes to "quality" prediction used 
!                        in data selection algorithm
!   2005-01-26 derber  - land/sea determination and weighting for data selection
!   2005-02-10 treadon - correct spelling in runtime print message; specify
!                        _r_kind precision for real constants
!   2005-07-06 derber  - add mhs and hirs/4 from NOAA-18, clean up code and 
!                        modify data selection criteria
!   2005-09-08  derber - modify to use input group time window
!   2005-09-28  derber - modify to produce consistent surface info 
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-10-24  derber - use more precise MHS scan properties (step & start)
!   2005-10-26  treadon - clean up formatting, add clarifying comments, correct
!                         ndata,ndata1 printout
!   2005-11-22  derber  - include mean in bias correction
!   2005-11-29  parrish - modify getsfc to work for different regional options
!   2006-02-01  parrish - remove getsfc (different version called now in read_obs)
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-02-11  liu - add ssu
!   2006-03-07  derber - correct error in nodata count
!   2006-04-27  derber - clean up code
!   2006-05-02  treadon - move close lnbufr to after 900 continue
!   2006-05-19  eliu    - add logic to reset relative weight when all channels not used
!   2006-07-28  derber  - add solar and satellite azimuth angles remove isflg from output
!   2007-01-18  derber - add kidsat for metop
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2007-08-31  h.liu - add kidsat for n05, n06, n07, n08, n09, n10, n11, n12,tiros-n
!   2008-05-27  safford - rm unused vars
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2008-10-14  derber  - properly use EARS data and use MPI_IO
!   2009-01-02  todling - remove unused vars
!   2009-01-09  gayno   - add option to calculate surface fields based on 
!                         size/shape of field of view.
!   2009-04-17  todling - zero out azimuth angle when unavailable (otherwise can't use old files)
!   2009-04-18  woollen - improve mpi_io interface with bufrlib routines
!   2009-04-21  derber  - add ithin to call to makegrids
!   2009-12-20  gayno - modify for updated version of FOV surface code which calculates
!                       relative antenna power for some instruments.
!
!   input argument list:
!     mype     - mpi task id
!     val_tovs - weighting factor applied to super obs
!     ithin    - flag to thin data
!     isfcalc  - method to calculate surface fields within FOV
!                when one, calculate accounting for size/shape of FOV.
!                otherwise, use bilinear interpolation.
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window(hours)
!     sis      - sensor/instrument/satellite indicator
!     mype_root - "root" task for sub-communicator
!     mype_sub - mpi task id within sub-communicator
!     npe_sub  - number of data read tasks
!     mpi_comm_sub - sub-communicator for data read
!     mype_sub_read
!     npe_sub_read
!     lll
!
!   output argument list:
!     nread    - number of BUFR TOVS 1b observations read
!     ndata    - number of BUFR TOVS 1b profiles retained for further processing
!     nodata   - number of BUFR TOVS 1b observations retained for further processing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,destroygrids,checkob, &
           finalcheck,map2tgrid,score_crit
  use radinfo, only: iuse_rad,newchn,cbias,predx,nusis,jpch_rad,air_rad,ang_rad
  use radinfo, only: crtm_coeffs_path
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,tll2xy,txy2ll,rlats,rlons
  use constants, only: izero,ione,deg2rad,zero,one,two,three,five,izero,rad2deg,r60inv,r1000,h300
  use obsmod, only: offtime_data
  use crtm_parameters, only: MAX_SENSOR_ZENITH_ANGLE
  use crtm_spccoeff, only: sc
  use crtm_module, only: crtm_destroy,crtm_init,success,crtm_channelinfo_type
  use calc_fov_crosstrk, only : instrument_init, fov_cleanup, fov_check
  use gsi_4dvar, only: iadatebgn,iadateend,l4dvar,iwinbgn,winlen
  use antcorr_application
  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: infile,obstype,jsatid
  character(len=*),intent(in   ) :: sis
  integer(i_kind) ,intent(in   ) :: mype,lunout,ithin,isfcalc
  integer(i_kind) ,intent(inout) :: nread
  integer(i_kind) ,intent(  out) :: ndata,nodata
  real(r_kind)    ,intent(in   ) :: rmesh,gstime,twind
  real(r_kind)    ,intent(inout) :: val_tovs
  integer(i_kind) ,intent(in   ) :: mype_root
  integer(i_kind) ,intent(in   ) :: mype_sub,mype_sub_read
  integer(i_kind) ,intent(in   ) :: npe_sub,npe_sub_read
  integer(i_kind) ,intent(in   ) :: mpi_comm_sub
  integer(i_kind) ,intent(in   ) :: lll

! Declare local parameters

  character(8),parameter:: fov_flag="crosstrk"
  integer(i_kind),parameter:: n1bhdr=13_i_kind
  integer(i_kind),parameter:: n2bhdr=14_i_kind
  integer(i_kind),parameter:: maxinfo=33_i_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=50.0_r_kind
  real(r_kind),parameter:: tbmax=550.0_r_kind

! Declare local variables
  logical hirs,msu,amsua,amsub,mhs,hirs4,hirs3,hirs2,ssu
  logical outside,iuse,assim,valid

  character(14):: infile2
  character(8) subset
  character(80) hdr1b,hdr2b

  integer(i_kind) ireadsb,ireadmg,irec,isub,next
  integer(i_kind) i,j,k,ifov,ntest
  integer(i_kind) iret,idate,nchanl,n,idomsfc
  integer(i_kind) ich1,ich2,ich8,ich15,kidsat,instrument
  integer(i_kind) nmind,itx,nele,itt,ninstruments
  integer(i_kind) iskip,ichan2,ichan1,ichan15
  integer(i_kind) lnbufr,ksatid,ichan8,isflg,ichan3,ich3,ich4,ich6
  integer(i_kind) ilat,ilon,ifovmod
  integer(i_kind),dimension(5):: idate5
  integer(i_kind) instr,ichan
  integer(i_kind):: error_status
  character(len=20),dimension(1):: sensorlist
  type(crtm_channelinfo_type),dimension(1) :: channelinfo

  real(r_kind) cosza,sfcr
  real(r_kind) ch1,ch2,ch3,ch8,d0,d1,d2,ch15,qval
  real(r_kind) ch1flg
  real(r_kind) expansion
  real(r_kind),dimension(0:3):: sfcpct
  real(r_kind),dimension(0:3):: ts
  real(r_kind) :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10

  real(r_kind) pred
  real(r_kind) rsat,dlat,panglr,dlon,rato,sstime,tdiff,t4dv
  real(r_kind) dlon_earth_deg,dlat_earth_deg,sat_aziang
  real(r_kind) dlon_earth,dlat_earth,r01
  real(r_kind) crit1,step,start,ch8flg,dist1
  real(r_kind) terrain,timedif,lza,df2,tt,lzaest
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),allocatable,dimension(:,:):: data_all

  real(r_double),allocatable,dimension(:):: data1b8,data1b8x
  real(r_double),dimension(n1bhdr):: bfr1bhdr
  real(r_double),dimension(n2bhdr):: bfr2bhdr

  real(r_kind) disterr,disterrmax,dlon00,dlat00

!**************************************************************************
! Initialize variables

  lnbufr = 15_i_kind
  disterrmax=zero
  ntest=izero
  ndata  = izero
  nodata  = izero
  nread  = izero

  ilon=3_i_kind
  ilat=4_i_kind
  

! Make thinning grids
  call makegrids(rmesh,ithin)

! Set various variables depending on type of data to be read

  hirs2 =    obstype == 'hirs2'
  hirs3 =    obstype == 'hirs3'
  hirs4 =    obstype == 'hirs4'
  hirs =     hirs2 .or. hirs3 .or. hirs4
  msu=       obstype == 'msu'
  amsua=     obstype == 'amsua'
  amsub=     obstype == 'amsub'
  mhs  =     obstype == 'mhs'
  ssu =      obstype == 'ssu'

!  instrument specific variables
  d1 =  0.754_r_kind
  d2 = -2.265_r_kind 
  r01 = 0.01_r_kind
  ich1   = ione       !1
  ich2   = 2_i_kind   !2
  ich3   = 3_i_kind   !3
  ich4   = 4_i_kind   !4
  ich6   = 6_i_kind   !6
  ich8   = 8_i_kind   !8
  ich15  = 15_i_kind  !15
! Set array index for surface-sensing channels
  ichan1  = newchn(sis,ich1)
  ichan2  = newchn(sis,ich2)
  ichan3  = newchn(sis,ich3)
  if (hirs) then
     ichan8  = newchn(sis,ich8)
  endif
  if (amsua) ichan15 = newchn(sis,ich15)

  if(jsatid == 'n05')kidsat=705_i_kind
  if(jsatid == 'n06')kidsat=706_i_kind
  if(jsatid == 'n07')kidsat=707_i_kind
  if(jsatid == 'tirosn')kidsat=708_i_kind
  if(jsatid == 'n08')kidsat=200_i_kind
  if(jsatid == 'n09')kidsat=201_i_kind
  if(jsatid == 'n10')kidsat=202_i_kind
  if(jsatid == 'n11')kidsat=203_i_kind
  if(jsatid == 'n12')kidsat=204_i_kind

  if(jsatid == 'n14')kidsat=205_i_kind
  if(jsatid == 'n15')kidsat=206_i_kind
  if(jsatid == 'n16')kidsat=207_i_kind
  if(jsatid == 'n17')kidsat=208_i_kind
  if(jsatid == 'n18')kidsat=209_i_kind
  if(jsatid == 'n19')kidsat=223_i_kind
  if(jsatid == 'metop-a')kidsat=4_i_kind
  if(jsatid == 'metop-b')kidsat=5_i_kind
  if(jsatid == 'metop-c')kidsat=6_i_kind


  rato=1.1363987_r_kind
  if ( hirs ) then
     step   = 1.80_r_kind
     start  = -49.5_r_kind
     nchanl=19_i_kind
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = zero
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 30._r_kind
     if (isfcalc == ione) then
        expansion=one  ! use one for ir sensors
        ichan=-999_i_kind  ! not used for hirs
        if (hirs2) then
           if(kidsat==203_i_kind.or.kidsat==205_i_kind)then
              instr=5_i_kind ! hirs-2i on noaa 11 and 14
           elseif(kidsat==708_i_kind.or.kidsat==706_i_kind.or.kidsat==707_i_kind.or. &
                  kidsat==200_i_kind.or.kidsat==201_i_kind.or.kidsat==202_i_kind.or. &
                  kidsat==204_i_kind)then
              instr=4_i_kind ! hirs-2 on tiros-n,noaa6-10,noaa12
           else  ! sensor/sat mismatch
              write(6,*) 'READ_BUFRTOVS:  *** WARNING: HIRS2 SENSOR/SAT MISMATCH'
              instr=5_i_kind ! set to something to prevent abort
           endif
        elseif (hirs4) then
           instr=8_i_kind
        elseif (hirs3) then
           if(kidsat==206_i_kind) then
              instr=6_i_kind   ! noaa 15
           elseif(kidsat==207_i_kind.or.kidsat==208_i_kind)then
              instr=7_i_kind   ! noaa 16/17
           else
              write(6,*) 'READ_BUFRTOVS:  *** WARNING: HIRS3 SENSOR/SAT MISMATCH'
              instr=7_i_kind
           endif
        endif
     endif  ! isfcalc == 1

  else if ( msu ) then
     step   = 9.474_r_kind
     start  = -47.37_r_kind
     nchanl=4_i_kind
     if (isfcalc==ione) then
        instr=10_i_kind
        ichan=-999_i_kind
        expansion=2.9_r_kind
     endif
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = zero
     rlndsea(1) = 20._r_kind
     rlndsea(2) = 15._r_kind
     rlndsea(3) = 20._r_kind
     rlndsea(4) = 100._r_kind
  else if ( amsua ) then
     step   = three + one/three
     start = -48._r_kind - one/three
!    start  = -48.33_r_kind
     nchanl=15_i_kind
     if (isfcalc==ione) then
        instr=11_i_kind
        ichan=15_i_kind  ! pick a surface sens. channel
        expansion=2.9_r_kind ! use almost three for microwave sensors.
     endif
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = zero
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
  else if ( amsub )  then
     step   = 1.1_r_kind
     start  = -48.95_r_kind
     nchanl=5_i_kind
     if (isfcalc==ione) then
        instr=12_i_kind
        ichan=-999_i_kind
        expansion=2.9_r_kind
     endif
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = zero
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 20._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
  else if ( mhs )  then
     step   = 10.0_r_kind/9.0_r_kind
     start  = -445.0_r_kind/9.0_r_kind
     nchanl=5_i_kind
     if (isfcalc==ione) then
        instr=13_i_kind
        ichan=ione  ! all channels give similar result.
        expansion=2.9_r_kind
     endif
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = zero
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 20._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
  else if ( ssu ) then
     step  =  10.00_r_kind
     start = -35.00_r_kind
     nchanl=3_i_kind
     if (isfcalc==ione) then
        instr=9_i_kind
        ichan=-999_i_kind  ! not used for this sensor
        expansion=one
     endif
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = zero
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 30._r_kind
  end if

! Initialize variables for use by FOV-based surface code.
  if (isfcalc == ione) then
     call instrument_init(instr,jsatid,expansion)
  endif

! If all channels of a given sensor are set to monitor or not
! assimilate mode (iuse_rad<1), reset relative weight to zero.
! We do not want such observations affecting the relative
! weighting between observations within a given thinning group.

  assim=.false.
  search: do i=1,jpch_rad
     if ((nusis(i)==sis) .and. (iuse_rad(i)>izero)) then
        assim=.true.
        exit search
     endif
  end do search
  if (.not.assim) val_tovs=zero


! Allocate arrays to hold all data for given satellite
  nele=maxinfo+nchanl
  allocate(data_all(nele,itxmax),data1b8(nchanl))
  if(lll ==2_i_kind)allocate(data1b8x(nchanl))


! Big loop over standard data feed and possible ears data

!    Set bufr subset names based on type of data to read
  if(lll == 2_i_kind .and. amsua .and. kidsat >= 200_i_kind .and. kidsat <= 207_i_kind)go to 500

!  Open unit to satellite bufr file
  infile2=infile
  if(lll == 2)infile2=trim(infile)//'ears'
  open(lnbufr,file=infile2,form='unformatted',status = 'old',err = 500)
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)
  call readmg(lnbufr,subset,idate,iret)

!  Extract date and check for consistency with analysis date     
  write(6,*)'READ_BUFRTOVS: bufr file date is ',idate,infile2,jsatid
  IF (idate<iadatebgn.OR.idate>iadateend) THEN
     if(offtime_data) then
        write(6,*)'***READ_BUFRTOVS analysis and data file date differ, but use anyway'
     else
        write(6,*)'***READ_BUFRTOVS ERROR*** ',&
           'incompatable analysis and observation date/time'
     end if
     write(6,*)'Analysis start  :',iadatebgn
     write(6,*)'Analysis end    :',iadateend
     write(6,*)'Observation time:',idate
     if(.not.offtime_data) go to 500
  ENDIF

  if(lll == 2_i_kind)then
     sensorlist(1)=sis
     if( crtm_coeffs_path /= "" ) then
        if(mype_sub_read==mype_root) write(6,*)'READ_BUFRTOVS: crtm_init() on path "'//trim(crtm_coeffs_path)//'"'
        error_status = crtm_init(channelinfo,SensorID=sensorlist,&
          Process_ID=mype_sub_read,Output_Process_ID=mype_root, &
          File_Path = crtm_coeffs_path )
     else
        error_status = crtm_init(channelinfo,SensorID=sensorlist,&
          Process_ID=mype_sub_read,Output_Process_ID=mype_root)
     endif
     if (error_status /= success) then
        write(6,*)'READ_BUFRTOVS:  ***ERROR*** crtm_init error_status=',error_status,&
             '   TERMINATE PROGRAM EXECUTION'
        call stop2(71)
     endif
     ninstruments = size(sc)
     instrument_loop: do n=1,ninstruments
        if(sis == sc(n)%sensor_id)then
           instrument=n
           exit instrument_loop
        end if
     end do instrument_loop
     if(instrument == izero)then
        write(6,*)' failure to find instrument in read_bufrtovs ',sis
     end if
  end if

!  Reopen unit to satellite bufr file
  call closbf(lnbufr)
  open(lnbufr,file=infile2,form='unformatted',status = 'old',err = 500)
  call openbf(lnbufr,'IN',lnbufr)
   
!  Loop to read bufr file
  next=mype_sub_read+ione
  read_subset: do while(ireadmg(lnbufr,subset,idate)>=izero)
     call ufbcnt(lnbufr,irec,isub)
     if(irec/=next)cycle
     next=next+npe_sub_read
     read_loop: do while (ireadsb(lnbufr)==izero)

!          Read header record.  (lll=1 is normal feed, 2=EARS data)
           hdr1b ='SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON CLATH CLONH HOLS'
           call ufbint(lnbufr,bfr1bhdr,n1bhdr,ione,iret,hdr1b)

!          Extract satellite id.  If not the one we want, read next record
           rsat=bfr1bhdr(1) 
           ksatid=nint(bfr1bhdr(1))
           if(ksatid /= kidsat) cycle read_subset

!          Extract observation location and other required information
           if(abs(bfr1bhdr(11)) <= 91._r_kind .and. abs(bfr1bhdr(12)) <= 361._r_kind)then
              dlat_earth = bfr1bhdr(11)
              dlon_earth = bfr1bhdr(12)
           else
              dlat_earth = bfr1bhdr(9)
              dlon_earth = bfr1bhdr(10)
           end if
           if(dlon_earth<zero)  dlon_earth = dlon_earth+r360
           if(dlon_earth>=r360) dlon_earth = dlon_earth-r360
           dlat_earth_deg = dlat_earth
           dlon_earth_deg = dlon_earth
           dlat_earth = dlat_earth*deg2rad
           dlon_earth = dlon_earth*deg2rad
           

!          Regional case
           if(regional)then
              call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
              if(diagnostic_reg) then
                 call txy2ll(dlon,dlat,dlon00,dlat00)
                 ntest=ntest+ione
                 disterr=acos(sin(dlat_earth)*sin(dlat00)+cos(dlat_earth)*cos(dlat00)* &
                      (sin(dlon_earth)*sin(dlon00)+cos(dlon_earth)*cos(dlon00)))*rad2deg
                 disterrmax=max(disterrmax,disterr)
              end if
              
!             Check to see if in domain
              if(outside) cycle read_loop

!          Global case
           else
              dlat=dlat_earth
              dlon=dlon_earth
              call grdcrd(dlat,ione,rlats,nlat,ione)
              call grdcrd(dlon,ione,rlons,nlon,ione)
           endif

!          Extract date information.  If time outside window, skip this obs
           idate5(1) = bfr1bhdr(3) !year
           idate5(2) = bfr1bhdr(4) !month
           idate5(3) = bfr1bhdr(5) !day
           idate5(4) = bfr1bhdr(6) !hour
           idate5(5) = bfr1bhdr(7) !minute
           call w3fs21(idate5,nmind)
           t4dv= (real((nmind-iwinbgn),r_kind) + bfr1bhdr(8)*r60inv)*r60inv    ! add in seconds
           if (l4dvar) then
              if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
           else
              sstime= real(nmind,r_kind) + bfr1bhdr(8)*r60inv    ! add in seconds
              tdiff=(sstime-gstime)*r60inv
              if(abs(tdiff) > twind) cycle read_loop
           endif

!          If msu, drop obs from first (1) and last (11) scan positions
           ifov = nint(bfr1bhdr(2))
           if (msu .and. (ifov==ione .or. ifov==11_i_kind)) cycle read_loop

           nread=nread+nchanl

           if (l4dvar) then
              timedif = zero
           else
              timedif = two*abs(tdiff)        ! range:  0 to 6
           endif
           terrain = 50._r_kind
           if(lll == ione)terrain = 0.01_r_kind*abs(bfr1bhdr(13))                   
           crit1 = 0.01_r_kind+terrain + (lll-ione)*500.0_r_kind + timedif 

           hdr2b ='SAZA SOZA BEARAZ SOLAZI'
           call ufbint(lnbufr,bfr2bhdr,n2bhdr,ione,iret,hdr2b)

           sat_aziang=bfr2bhdr(3)
           if (abs(sat_aziang) > r360) then
              sat_aziang=zero
!_RT              write(6,*) 'READ_BUFRTOVS: bad azimuth angle ',sat_aziang
!_RT              cycle read_loop
           endif

!          Read data record.  Increment data counter
           if(lll == ione)then
              call ufbrep(lnbufr,data1b8,ione,nchanl,iret,'TMBR')
           else
              call ufbrep(lnbufr,data1b8,ione,nchanl,iret,'TMBRST')
              data1b8x=data1b8
              if(.not. hirs)then
                 call remove_antcorr(sc(instrument)%ac,ifov,data1b8)
                 do j=1,nchanl
                    if(data1b8x(j) > r1000)data1b8(j) = 1000000._r_kind
                 end do
              end if
           end if

!          Transfer observed brightness temperature to work array.  If any
!          temperature exceeds limits, reset observation to "bad" value
           iskip=izero
           do j=1,nchanl
              if (data1b8(j) < tbmin .or. data1b8(j) > tbmax) then
                 iskip = iskip + ione

!                Remove profiles where key channels are bad  
                 if(( msu  .and.  j == ich1) .or.                                 &
                      (amsua .and. (j == ich1 .or. j == ich2 .or. j == ich3 .or.    &
                      j == ich4 .or. j==ich6 .or. j == ich15 )) .or. &
                      (hirs  .and. (j == ich8 )) .or.                               &
                      (amsub .and.  j == ich1) .or.                                 &
                      (mhs   .and.  (j == ich1 .or. j == ich2))) iskip = iskip+nchanl
              endif
           end do
           if (iskip >= nchanl) cycle read_loop
!          Map obs to thinning grid
           call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
           if(.not. iuse)cycle read_loop

!          Determine surface properties based on 
!          sst and land/sea/ice mask   
!
!          isflg    - surface flag
!                     0 sea
!                     1 land
!                     2 sea ice
!                     3 snow
!                     4 mixed                       

!          FOV-based surface code requires fov number.  if out-of-range, then
!          skip this ob.

           if (isfcalc == ione) then
              call fov_check(ifov,instr,valid)
              if (.not. valid) cycle read_loop
           end if

!          When isfcalc is one, calculate surface fields based on size/shape of fov.
!          Otherwise, use bilinear method.

           if (isfcalc == ione) then
              call deter_sfc_fov(fov_flag,ifov,instr,ichan,sat_aziang,dlat_earth_deg,&
                                 dlon_earth_deg,expansion,t4dv,isflg,idomsfc, &
                                 sfcpct,vfr,sty,vty,stp,sm,ff10,sfcr,zz,sn,ts,tsavg)
           else
              call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg, &
                     idomsfc,sfcpct,ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
           endif


           crit1 = crit1 + rlndsea(isflg) + 10._r_kind*float(iskip)
           call checkob(dist1,crit1,itx,iuse)
           if(.not. iuse)cycle read_loop

!          Set common predictor parameters
           ifovmod=ifov

!          Account for assymetry due to satellite build error
           if(hirs .and. ((jsatid == 'n16') .or. (jsatid == 'n17'))) &
                  ifovmod=ifovmod+ione

           panglr=(start+float(ifovmod-ione)*step)*deg2rad
           lzaest = asin(rato*sin(panglr))
           if( msu .or. hirs2 .or. ssu)then
              lza = lzaest
           else
              lza = bfr2bhdr(1)*deg2rad      ! local zenith angle
              if((amsua .and. ifovmod <= 15_i_kind) .or.        &
                 (amsub .and. ifovmod <= 45_i_kind) .or.        &
                 (mhs   .and. ifovmod <= 45_i_kind) .or.        &
                 (hirs  .and. ifovmod <= 28_i_kind)) lza=-lza
           end if

!  Check for errors in satellite zenith angles 
           if(abs(lzaest-lza)*rad2deg > one) then
              write(6,*)' READ_BUFRTOVS WARNING uncertainty in lza ', &
                      lza*rad2deg,lzaest*rad2deg,sis,ifovmod,start,step
              cycle read_loop
           end if

           if(abs(lza)*rad2deg > MAX_SENSOR_ZENITH_ANGLE) then
              write(6,*)'READ_BUFRTOVS WARNING lza error ',bfr2bhdr(1),panglr
              cycle read_loop
           end if

!          Set data quality predictor
           if (msu) then
              ch1    = data1b8(ich1)-ang_rad(ichan1)*cbias(ifov,ichan1)- &
                       r01*predx(1,ichan1)*air_rad(ichan1)
              ch1flg = tsavg-ch1
              if(isflg == izero)then
                 pred = 100._r_kind-min(ch1flg,100.0_r_kind)
              else
                 pred = abs(ch1flg)
              end if
           else if (hirs) then
              ch8    = data1b8(ich8) -ang_rad(ichan8)*cbias(ifov,ichan8)- &
                       r01*predx(1,ichan8)*air_rad(ichan8)
              ch8flg = tsavg-ch8
              pred   = 10.0_r_kind*max(zero,ch8flg)
           else if (amsua) then
!   Remove angle dependent pattern (not mean)
              ch1 = data1b8(ich1)-ang_rad(ichan1)*cbias(ifov,ichan1)+ &
                    air_rad(ichan1)*cbias(15,ichan1)
              ch2 = data1b8(ich2)-ang_rad(ichan2)*cbias(ifov,ichan2)+ &
                    air_rad(ichan2)*cbias(15,ichan2)   
              if (isflg == izero .and. ch1<285.0_r_kind .and. ch2<285.0_r_kind) then
                 cosza = cos(lza)
                 d0    = 8.24_r_kind - 2.622_r_kind*cosza + 1.846_r_kind*cosza*cosza
                 qval  = cosza*(d0+d1*log(285.0_r_kind-ch1)+d2*log(285.0_r_kind-ch2))
                 pred  = max(zero,qval)*100.0_r_kind
              else
                 ch3  = data1b8(ich3)-ang_rad(ichan3)*cbias(ifov,ichan3)+ &
                        air_rad(ichan3)*cbias(15,ichan3)   
                 ch15 = data1b8(ich15)-ang_rad(ichan15)*cbias(ifov,ichan15)+ &
                        air_rad(ichan15)*cbias(15,ichan15)
                 pred = abs(ch1-ch15)
                 if(ch1-ch15 >= three) then
                    df2  = 5.10_r_kind +0.78_r_kind*ch1-0.96_r_kind*ch3
                    tt   = 168._r_kind-0.49_r_kind*ch15
                    if(ch1 > 261._r_kind .or. ch1 >= tt .or. & 
                         (ch15 <= 273._r_kind .and. df2 >= 0.6_r_kind))then
                       pred = 100._r_kind
                    end if
                 end if
              endif
           
!          sval=-113.2_r_kind+(2.41_r_kind-0.0049_r_kind*ch1)*ch1 +  &
!               0.454_r_kind*ch2-ch15

           else if (amsub .or. mhs) then
              ch1 = data1b8(ich1)-ang_rad(ichan1)*cbias(ifov,ichan1)- &
                    r01*predx(1,ichan1)*air_rad(ichan1)
              ch2 = data1b8(ich2)-ang_rad(ichan2)*cbias(ifov,ichan2)- &
                    r01*predx(1,ichan2)*air_rad(ichan2)
              if(isflg == izero)then
                 cosza = cos(lza)
!                 pred = (ch1-ch2)/cosza+30.0_r_kind
                 if(ch2 < h300)then 
                    pred = (0.13_r_kind*(ch1+33.58_r_kind*log(h300-ch2)- &
                         341.17_r_kind))*five
                 else
                    pred = 100._r_kind
                 end if
              else
                 pred = 42.72_r_kind + 0.85_r_kind*ch1-ch2
              end if
              pred = max(zero,pred)
           endif
           
!          Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
           crit1 = crit1+pred 
           call finalcheck(dist1,crit1,itx,iuse)
           if(.not. iuse)cycle read_loop

!          Load selected observation into data array
              
           data_all(1 ,itx)= rsat                      ! satellite ID
           data_all(2 ,itx)= t4dv                      ! time
           data_all(3 ,itx)= dlon                      ! grid relative longitude
           data_all(4 ,itx)= dlat                      ! grid relative latitude
           data_all(5 ,itx)= lza                       ! local zenith angle
           data_all(6 ,itx)= bfr2bhdr(3)              ! local azimuth angle
           data_all(7 ,itx)= panglr                    ! look angle
           data_all(8 ,itx)= ifov                      ! scan position
           data_all(9 ,itx)= bfr2bhdr(2)              ! solar zenith angle
           data_all(10,itx)= bfr2bhdr(4)               ! solar azimuth angle
           data_all(11,itx) = sfcpct(0)                ! sea percentage of
           data_all(12,itx) = sfcpct(1)                ! land percentage
           data_all(13,itx) = sfcpct(2)                ! sea ice percentage
           data_all(14,itx) = sfcpct(3)                ! snow percentage
           data_all(15,itx)= ts(0)                     ! ocean skin temperature
           data_all(16,itx)= ts(1)                     ! land skin temperature
           data_all(17,itx)= ts(2)                     ! ice skin temperature
           data_all(18,itx)= ts(3)                     ! snow skin temperature
           data_all(19,itx)= tsavg                     ! average skin temperature
           data_all(20,itx)= vty                       ! vegetation type
           data_all(21,itx)= vfr                       ! vegetation fraction
           data_all(22,itx)= sty                       ! soil type
           data_all(23,itx)= stp                       ! soil temperature
           data_all(24,itx)= sm                        ! soil moisture
           data_all(25,itx)= sn                        ! snow depth
           data_all(26,itx)= zz                        ! surface height
           data_all(27,itx)= idomsfc + 0.001_r_kind    ! dominate surface type
           data_all(28,itx)= sfcr                      ! surface roughness
           data_all(29,itx)= ff10                      ! ten meter wind factor
           data_all(30,itx) = dlon_earth_deg           ! earth relative longitude (deg)
           data_all(31,itx) = dlat_earth_deg           ! earth relative latitude (deg)

           data_all(32,itx)= val_tovs
           data_all(33,itx)= itt

           do i=1,nchanl
              data_all(i+maxinfo,itx)=data1b8(i)
           end do


!       End of bufr read loops
     enddo read_loop
  enddo read_subset
  call closbf(lnbufr)


  if(lll == 2_i_kind)then
! deallocate crtm info
     error_status = crtm_destroy(channelinfo)
     if (error_status /= success) &
        write(6,*)'OBSERVER:  ***ERROR*** crtm_destroy error_status=',error_status
  end if

!   Jump here when there is a problem opening the bufr file
500  continue
  deallocate(data1b8)
  if (lll==2_i_kind) deallocate(data1b8x)


  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
       nele,itxmax,nread,ndata,data_all,score_crit)

! 
  if(mype_sub==mype_root)then
     do n=1,ndata
        do i=1,nchanl
           if(data_all(i+maxinfo,n) > tbmin .and. &
              data_all(i+maxinfo,n) < tbmax)nodata=nodata+ione
        end do
        itt=nint(data_all(maxinfo,n))
        super_val(itt)=super_val(itt)+val_tovs

     end do

!    Write final set of "best" observations to output file
     write(lunout) obstype,sis,maxinfo,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  end if

! Deallocate local arrays
  deallocate(data_all)

! Deallocate satthin arrays
  call destroygrids
 
! Deallocate FOV surface code arrays and nullify pointers.
  if (isfcalc == ione) call fov_cleanup

  if(diagnostic_reg.and.ntest>izero) write(6,*)'READ_BUFRTOVS:  ',&
       'mype,ntest,disterrmax=',mype,ntest,disterrmax

! End of routine
  return

end subroutine read_bufrtovs

subroutine deter_sfc(alat,alon,dlat_earth,dlon_earth,obstime,isflg, &
       idomsfc,sfcpct,ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_sfc                     determine land surface type
!   prgmmr: derber           org: np2                date: 2005-01-27
!
! abstract:  determines land surface type based on surrounding land
!            surface types
!
! program history log:
!   2005-01-27 derber
!   2005-03-03 treadon - add implicit none, define zero
!   2006-02-01 parrish  - change names of sno,isli,sst
!
!   input argument list:
!     alat
!     alon
!     obstime- observation time relative to analysis time
!     dlat_earth
!     dlon_earth
!
!   output argument list:
!      isflg    - surface flag
!                 0 sea
!                 1 land
!                 2 sea ice
!                 3 snow
!                 4 mixed
!      sfcpct(0:3)- percentage of 4 surface types
!                 (0) - sea percentage
!                 (1) - land percentage
!                 (2) - sea ice percentage
!                 (3) - snow percentage
!      tsavg - sea surface temperature
!      idomsfc
!      ts
!      dfcr
!      vty,vfr,sty,stp,sm,sn,zz,ff10
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
     use kinds, only: r_kind,i_kind
     use satthin, only: sno_full,isli_full,sst_full,soil_moi_full, &
           soil_temp_full,soil_type_full,veg_frac_full,veg_type_full, &
           fact10_full,zs_full,sfc_rough_full
     use constants, only: izero,ione,zero,one,one_tenth
     use gridmod, only: nlat,nlon,regional,tll2xy,nlat_sfc,nlon_sfc,rlats_sfc,rlons_sfc
     use guess_grids, only: nfldsfc,hrdifsfc
     implicit none

     real(r_kind)               ,intent(in   ) :: dlat_earth,dlon_earth,obstime,alat,alon
     integer(i_kind)            ,intent(  out) :: isflg,idomsfc
     real(r_kind),dimension(0:3),intent(  out) :: sfcpct
     real(r_kind),dimension(0:3),intent(  out) :: ts
     real(r_kind)               ,intent(  out) :: tsavg,sfcr
     real(r_kind)               ,intent(  out) :: vty,vfr,sty,stp,sm,sn,zz,ff10

     real(r_kind),parameter:: minsnow=one_tenth

     integer(i_kind) istyp00,istyp01,istyp10,istyp11
     integer(i_kind):: itsfc,itsfcp
     integer(i_kind):: ix,iy,ixp,iyp,j
     real(r_kind):: dx,dy,dx1,dy1,w00,w10,w01,w11,dtsfc,dtsfcp,wgtmin
     real(r_kind):: sno00,sno01,sno10,sno11,dlat,dlon
     real(r_kind):: sst00,sst01,sst10,sst11
     real(r_kind),dimension(0:3)::wgtavg

!  First do surface field since it is on model grid
     iy=int(alon); ix=int(alat)
     dy  =alon-iy; dx  =alat-ix
     dx1 =one-dx;    dy1 =one-dy
     w00=dx1*dy1; w10=dx*dy1; w01=dx1*dy; w11=dx*dy

     ix=min(max(ione,ix),nlat); iy=min(max(izero,iy),nlon)
     ixp=min(nlat,ix+ione); iyp=iy+ione
     if(iy==izero) iy=nlon
     if(iyp==nlon+ione) iyp=ione

!    Interpolate fields which only vary in space (no time component)
!       zz   = surface height
     zz   = zs_full(ix,iy) *w00 + zs_full(ixp,iy) *w10 + &
            zs_full(ix,iyp)*w01 + zs_full(ixp,iyp)*w11

     if(regional)then
!       call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        dlat=alat
        dlon=alon
     else
        dlat=dlat_earth
        dlon=dlon_earth
        call grdcrd(dlat,ione,rlats_sfc,nlat_sfc,ione)
        call grdcrd(dlon,ione,rlons_sfc,nlon_sfc,ione)
     end if
     iy=int(dlon); ix=int(dlat)
     dy  =dlon-iy; dx  =dlat-ix
     dx1 =one-dx;    dy1 =one-dy
     w00=dx1*dy1; w10=dx*dy1; w01=dx1*dy; w11=one-w00-w10-w01

     ix=min(max(ione,ix),nlat_sfc); iy=min(max(izero,iy),nlon_sfc)
     ixp=min(nlat_sfc,ix+ione); iyp=iy+ione
     if(iy==izero) iy=nlon_sfc
     if(iyp==nlon_sfc+ione) iyp=ione

!    Get time interpolation factors for surface files
     if(obstime > hrdifsfc(1) .and. obstime <= hrdifsfc(nfldsfc))then
        do j=1,nfldsfc-ione
           if(obstime > hrdifsfc(j) .and. obstime <= hrdifsfc(j+ione))then
              itsfc=j
              itsfcp=j+ione
              dtsfc=(hrdifsfc(j+ione)-obstime)/(hrdifsfc(j+ione)-hrdifsfc(j))
           end if
        end do
     else if(obstime <=hrdifsfc(1))then
        itsfc=ione
        itsfcp=ione
        dtsfc=one
     else
        itsfc=nfldsfc
        itsfcp=nfldsfc
        dtsfc=one
     end if
     dtsfcp=one-dtsfc

!    Set surface type flag.  Begin by assuming obs over ice-free water

     istyp00 = isli_full(ix ,iy )
     istyp10 = isli_full(ixp,iy )
     istyp01 = isli_full(ix ,iyp)
     istyp11 = isli_full(ixp,iyp)

     sno00= sno_full(ix ,iy ,itsfc)*dtsfc+sno_full(ix ,iy ,itsfcp)*dtsfcp
     sno01= sno_full(ix ,iyp,itsfc)*dtsfc+sno_full(ix ,iyp,itsfcp)*dtsfcp
     sno10= sno_full(ixp,iy ,itsfc)*dtsfc+sno_full(ixp,iy ,itsfcp)*dtsfcp
     sno11= sno_full(ixp,iyp,itsfc)*dtsfc+sno_full(ixp,iyp,itsfcp)*dtsfcp

     sst00= sst_full(ix ,iy ,itsfc)*dtsfc+sst_full(ix ,iy ,itsfcp)*dtsfcp
     sst01= sst_full(ix ,iyp,itsfc)*dtsfc+sst_full(ix ,iyp,itsfcp)*dtsfcp
     sst10= sst_full(ixp,iy ,itsfc)*dtsfc+sst_full(ixp,iy ,itsfcp)*dtsfcp
     sst11= sst_full(ixp,iyp,itsfc)*dtsfc+sst_full(ixp,iyp,itsfcp)*dtsfcp

!    Interpolate sst to obs location

     tsavg=sst00*w00+sst10*w10+sst01*w01+sst11*w11

     if(istyp00 >=ione .and. sno00 > minsnow)istyp00 = 3_i_kind
     if(istyp01 >=ione .and. sno01 > minsnow)istyp01 = 3_i_kind
     if(istyp10 >=ione .and. sno10 > minsnow)istyp10 = 3_i_kind
     if(istyp11 >=ione .and. sno11 > minsnow)istyp11 = 3_i_kind

     sfcpct = zero
     sfcpct(istyp00)=sfcpct(istyp00)+w00
     sfcpct(istyp01)=sfcpct(istyp01)+w01
     sfcpct(istyp10)=sfcpct(istyp10)+w10
     sfcpct(istyp11)=sfcpct(istyp11)+w11

     isflg = izero
     if(sfcpct(0) > 0.99_r_kind)then
        isflg = izero
     else if(sfcpct(1) > 0.99_r_kind)then
        isflg = ione
     else if(sfcpct(2) > 0.99_r_kind)then
        isflg = 2_i_kind
     else if(sfcpct(3) > 0.99_r_kind)then
        isflg = 3_i_kind
     else
        isflg = 4_i_kind
     end if

!       vty  = vegetation type
!       sty  = soil type

     ts(0:3)=zero
     wgtavg(0:3)=zero
     vfr=zero
     stp=zero
     sty=zero
     vty=zero
     sm=zero
     sn=zero
     idomsfc=isli_full(ix ,iy )
     wgtmin = w00
     if(istyp00 == ione)then
        vty  = veg_type_full(ix ,iy)
        sty  = soil_type_full(ix ,iy)
        wgtavg(1) = wgtavg(1) + w00
        ts(1)=ts(1)+w00*sst00
        vfr  =vfr  +w00*(veg_frac_full(ix ,iy ,itsfc ) *dtsfc+   &
                         veg_frac_full(ix ,iy ,itsfcp) *dtsfcp)
        stp  =stp  +w00*(soil_temp_full(ix ,iy ,itsfc )*dtsfc+   &
                         soil_temp_full(ix ,iy ,itsfcp)*dtsfcp)
        sm   =sm   +w00*(soil_moi_full(ix ,iy ,itsfc ) *dtsfc+   &
                         soil_moi_full(ix ,iy ,itsfcp) *dtsfcp)
     else if(istyp00 == 2_i_kind)then
        wgtavg(2) = wgtavg(2) + w00
        ts(2)=ts(2)+w00*sst00
     else if(istyp00 == 3_i_kind)then
        wgtavg(3) = wgtavg(3) + w00
        ts(3)=ts(3)+w00*sst00
        sn = sn + w00*sno00
     else
        wgtavg(0) = wgtavg(0) + w00
        ts(0)=ts(0)+w00*sst00
     end if
     if(istyp01 == ione)then
        if(wgtmin < w01 .or. (vty == zero .and. sty == zero))then
           vty  = veg_type_full(ix ,iyp)
           sty  = soil_type_full(ix ,iyp)
        end if
        wgtavg(1) = wgtavg(1) + w01
        ts(1)=ts(1)+w01*sst01
        vfr  =vfr  +w01*(veg_frac_full(ix ,iyp,itsfc ) *dtsfc+   &
                         veg_frac_full(ix ,iyp,itsfcp) *dtsfcp)
        stp  =stp  +w01*(soil_temp_full(ix ,iyp,itsfc )*dtsfc+   &
                         soil_temp_full(ix ,iyp,itsfcp)*dtsfcp)
        sm   =sm   +w01*(soil_moi_full(ix ,iyp,itsfc ) *dtsfc+   &
                         soil_moi_full(ix ,iyp,itsfcp) *dtsfcp)
     else if(istyp01 == 2_i_kind)then
        wgtavg(2) = wgtavg(2) + w01
        ts(2)=ts(2)+w01*sst01
     else if(istyp01 == 3_i_kind)then
        wgtavg(3) = wgtavg(3) + w01
        ts(3)=ts(3)+w01*sst01
        sn = sn + w01*sno01
     else
        wgtavg(0) = wgtavg(0) + w01
        ts(0)=ts(0)+w01*sst01
     end if
     if(wgtmin < w01)then
        idomsfc=isli_full(ix ,iyp)
        wgtmin = w01
     end if
     if(istyp10 == ione)then
        if(wgtmin < w10 .or. (vty == zero .and. sty == zero))then
           vty  = veg_type_full(ixp,iy)
           sty  = soil_type_full(ixp,iy)
        end if
        wgtavg(1) = wgtavg(1) + w10
        ts(1)=ts(1)+w10*sst10
        vfr  =vfr  +w10*(veg_frac_full(ixp,iy ,itsfc ) *dtsfc+   &
                         veg_frac_full(ixp,iy ,itsfcp) *dtsfcp)
        stp  =stp  +w10*(soil_temp_full(ixp,iy ,itsfc )*dtsfc+   &
                         soil_temp_full(ixp,iy ,itsfcp)*dtsfcp)
        sm   =sm   +w10*(soil_moi_full(ixp,iy ,itsfc ) *dtsfc+   &
                         soil_moi_full(ixp,iy ,itsfcp) *dtsfcp)
     else if(istyp10 == 2_i_kind)then
        wgtavg(2) = wgtavg(2) + w10
        ts(2)=ts(2)+w10*sst10
     else if(istyp10 == 3_i_kind)then
        wgtavg(3) = wgtavg(3) + w10
        ts(3)=ts(3)+w10*sst10
        sn = sn + w10*sno10
     else
        wgtavg(0) = wgtavg(0) + w10
        ts(0)=ts(0)+w10*sst10
     end if
     if(wgtmin < w10)then
        idomsfc=isli_full(ixp,iy )
        wgtmin = w10
     end if
     if(istyp11 == ione)then
        if(wgtmin < w11 .or. (vty == zero .and. sty == zero))then
           vty  = veg_type_full(ixp,iyp)
           sty  = soil_type_full(ixp,iyp)
        endif
        wgtavg(1) = wgtavg(1) + w11
        ts(1)=ts(1)+w11*sst11
        vfr  =vfr  +w11*(veg_frac_full(ixp,iyp,itsfc ) *dtsfc+   &
                         veg_frac_full(ixp,iyp,itsfcp) *dtsfcp)
        stp  =stp  +w11*(soil_temp_full(ixp,iyp,itsfc )*dtsfc+   &
                         soil_temp_full(ixp,iyp,itsfcp)*dtsfcp)
        sm   =sm   +w11*(soil_moi_full(ixp,iyp,itsfc ) *dtsfc+   &
                         soil_moi_full(ixp,iyp,itsfcp) *dtsfcp)
     else if(istyp11 == 2_i_kind)then
        wgtavg(2) = wgtavg(2) + w11
        ts(2)=ts(2)+w11*sst11
     else if(istyp11 == 3_i_kind)then
        wgtavg(3) = wgtavg(3) + w11
        ts(3)=ts(3)+w11*sst11
        sn = sn + w11*sno11
     else
        wgtavg(0) = wgtavg(0) + w11
        ts(0)=ts(0)+w11*sst11
     end if
     if(wgtmin < w11)then
        idomsfc=isli_full(ixp,iyp)
        wgtmin = w11
     end if

     if(wgtavg(0) > zero)then
        ts(0) = ts(0)/wgtavg(0)
     else
        ts(0) = tsavg
     end if
     if(wgtavg(1) > zero)then
        ts(1) = ts(1)/wgtavg(1)
        sm = sm/wgtavg(1)
        vfr = vfr/wgtavg(1)
        stp = stp/wgtavg(1)
     else
        ts(1) = tsavg
        sm=one
     end if
     if(wgtavg(2) > zero)then
        ts(2) = ts(2)/wgtavg(2)
     else
        ts(2) = tsavg
     end if
     if(wgtavg(3) > zero)then
        ts(3) = ts(3)/wgtavg(3)
        sn = sn/wgtavg(3)
     else
        ts(3) = tsavg
     end if
!    ts(0)=max(ts(0),270._r_kind)
!    ts(2)=min(ts(2),280._r_kind)
!    ts(3)=min(ts(3),280._r_kind)

!    Space-time interpolation of fields from surface wind speed

     ff10=(fact10_full(ix ,iy ,itsfc )*w00+ &
           fact10_full(ixp,iy ,itsfc )*w10+ &
           fact10_full(ix ,iyp,itsfc )*w01+ &
           fact10_full(ixp,iyp,itsfc )*w11)*dtsfc + &
          (fact10_full(ix ,iy ,itsfcp)*w00+ &
           fact10_full(ixp,iy ,itsfcp)*w10+ &
           fact10_full(ix ,iyp,itsfcp)*w01+ &
           fact10_full(ixp,iyp,itsfcp)*w11)*dtsfcp

     sfcr=(sfc_rough_full(ix ,iy ,itsfc )*w00+ &
           sfc_rough_full(ixp,iy ,itsfc )*w10+ &
           sfc_rough_full(ix ,iyp,itsfc )*w01+ &
           sfc_rough_full(ixp,iyp,itsfc )*w11)*dtsfc + &
          (sfc_rough_full(ix ,iy ,itsfcp)*w00+ &
           sfc_rough_full(ixp,iy ,itsfcp)*w10+ &
           sfc_rough_full(ix ,iyp,itsfcp)*w01+ &
           sfc_rough_full(ixp,iyp,itsfcp)*w11)*dtsfcp

     return
end subroutine deter_sfc

subroutine deter_sfc_type(dlat_earth,dlon_earth,obstime,isflg,tsavg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_sfc                     determine land surface type
!   prgmmr: derber           org: np2                date: 2005-01-27
!
! abstract:  determines land surface type based on surrounding land
!            surface types
!
! program history log:
!   2005-01-27 derber
!   2005-03-03 treadon - add implicit none, define zero
!   2006-02-01 parrish  - change names of sno,isli,sst
!
!   input argument list:
!     dlat_earth
!     dlon_earth
!     obstime
!
!   output argument list:
!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed
!     tsavg
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
     use kinds, only: r_kind,i_kind
     use satthin, only: isli_full,sst_full,sno_full
     use constants, only: izero,ione,zero,one,one_tenth
     use gridmod, only: regional,tll2xy,nlat_sfc,nlon_sfc,rlats_sfc,rlons_sfc
     use guess_grids, only: nfldsfc,hrdifsfc
     implicit none

     real(r_kind)   ,intent(in   ) :: dlat_earth,dlon_earth,obstime

     integer(i_kind),intent(  out) :: isflg
     real(r_kind)   ,intent(  out) :: tsavg

     integer(i_kind) istyp00,istyp01,istyp10,istyp11
     integer(i_kind):: ix,iy,ixp,iyp,j,itsfc,itsfcp
     real(r_kind):: dx,dy,dx1,dy1,w00,w10,w01,w11,dtsfc
     real(r_kind):: dlat,dlon,dtsfcp
     real(r_kind):: sst00,sst01,sst10,sst11
     real(r_kind):: sno00,sno01,sno10,sno11

     real(r_kind),parameter:: minsnow=one_tenth

     real(r_kind),dimension(0:3):: sfcpct
     logical :: outside


     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
     else
        dlat=dlat_earth
        dlon=dlon_earth
        call grdcrd(dlat,ione,rlats_sfc,nlat_sfc,ione)
        call grdcrd(dlon,ione,rlons_sfc,nlon_sfc,ione)
     end if
     iy=int(dlon); ix=int(dlat)
     dy  =dlon-iy; dx  =dlat-ix
     dx1 =one-dx;    dy1 =one-dy
     w00=dx1*dy1; w10=dx*dy1; w01=dx1*dy; w11=one-w00-w10-w01

     ix=min(max(ione,ix),nlat_sfc); iy=min(max(izero,iy),nlon_sfc)
     ixp=min(nlat_sfc,ix+ione); iyp=iy+ione
     if(iy==izero) iy=nlon_sfc
     if(iyp==nlon_sfc+ione) iyp=ione

!    Get time interpolation factors for surface files
     if(obstime > hrdifsfc(1) .and. obstime <= hrdifsfc(nfldsfc))then
        do j=1,nfldsfc-ione
           if(obstime > hrdifsfc(j) .and. obstime <= hrdifsfc(j+ione))then
              itsfc=j
              itsfcp=j+ione
              dtsfc=(hrdifsfc(j+ione)-obstime)/(hrdifsfc(j+ione)-hrdifsfc(j))
           end if
        end do
     else if(obstime <=hrdifsfc(1))then
        itsfc=ione
        itsfcp=ione
        dtsfc=one
     else
        itsfc=nfldsfc
        itsfcp=nfldsfc
        dtsfc=one
     end if
     dtsfcp=one-dtsfc

!    Set surface type flag.  Begin by assuming obs over ice-free water

     istyp00 = isli_full(ix ,iy )
     istyp10 = isli_full(ixp,iy )
     istyp01 = isli_full(ix ,iyp)
     istyp11 = isli_full(ixp,iyp)

     sno00= sno_full(ix ,iy ,itsfc)*dtsfc+sno_full(ix ,iy ,itsfcp)*dtsfcp
     sno01= sno_full(ix ,iyp,itsfc)*dtsfc+sno_full(ix ,iyp,itsfcp)*dtsfcp
     sno10= sno_full(ixp,iy ,itsfc)*dtsfc+sno_full(ixp,iy ,itsfcp)*dtsfcp
     sno11= sno_full(ixp,iyp,itsfc)*dtsfc+sno_full(ixp,iyp,itsfcp)*dtsfcp


     sst00= sst_full(ix ,iy ,itsfc)*dtsfc+sst_full(ix ,iy ,itsfcp)*dtsfcp
     sst01= sst_full(ix ,iyp,itsfc)*dtsfc+sst_full(ix ,iyp,itsfcp)*dtsfcp
     sst10= sst_full(ixp,iy ,itsfc)*dtsfc+sst_full(ixp,iy ,itsfcp)*dtsfcp
     sst11= sst_full(ixp,iyp,itsfc)*dtsfc+sst_full(ixp,iyp,itsfcp)*dtsfcp

!    Interpolate sst to obs location

     tsavg=sst00*w00+sst10*w10+sst01*w01+sst11*w11

     if(istyp00 >=ione .and. sno00 > minsnow)istyp00 = 3_i_kind
     if(istyp01 >=ione .and. sno01 > minsnow)istyp01 = 3_i_kind
     if(istyp10 >=ione .and. sno10 > minsnow)istyp10 = 3_i_kind
     if(istyp11 >=ione .and. sno11 > minsnow)istyp11 = 3_i_kind

     sfcpct = zero
     sfcpct(istyp00)=sfcpct(istyp00)+w00
     sfcpct(istyp01)=sfcpct(istyp01)+w01
     sfcpct(istyp10)=sfcpct(istyp10)+w10
     sfcpct(istyp11)=sfcpct(istyp11)+w11

     isflg = izero
     if(sfcpct(0) > 0.99_r_kind)then
        isflg = izero
     else if(sfcpct(1) > 0.99_r_kind)then
        isflg = ione
     else if(sfcpct(2) > 0.99_r_kind)then
        isflg = 2_i_kind
     else if(sfcpct(3) > 0.99_r_kind)then
        isflg = 3_i_kind
     else
        isflg = 4_i_kind
     end if
     return
end subroutine deter_sfc_type

subroutine deter_sfc2(dlat_earth,dlon_earth,obstime,idomsfc,tsavg,ff10,sfcr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_sfc                     determine land surface type
!   prgmmr: derber           org: np2                date: 2005-01-27
!
! abstract:  determines land surface type based on surrounding land
!            surface types
!
! program history log:
!   2005-01-27 derber
!   2005-03-03 treadon - add implicit none, define zero
!   2006-02-01 parrish  - change names of sno,isli,sst
!
!   input argument list:
!     dlat_earth
!     dlon_earth
!     obstime- observation time relative to analysis time
!
!   output argument list:
!     tsavg - sea surface temperature
!     idomsfc
!     sfcr
!     ff10
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
     use kinds, only: r_kind,i_kind
     use satthin, only: isli_full,sst_full,fact10_full,sfc_rough_full
     use constants, only: izero,ione,one
     use gridmod, only: regional,tll2xy,nlat_sfc,nlon_sfc,rlats_sfc,rlons_sfc
     use guess_grids, only: nfldsfc,hrdifsfc
     implicit none

     real(r_kind)   ,intent(in   ) :: dlat_earth,dlon_earth,obstime

     integer(i_kind),intent(  out) :: idomsfc
     real(r_kind)   ,intent(  out) :: tsavg,sfcr
     real(r_kind)   ,intent(  out) :: ff10

     integer(i_kind):: itsfc,itsfcp
     integer(i_kind):: ix,iy,ixp,iyp,j
     real(r_kind):: dx,dy,dx1,dy1,w00,w10,w01,w11,dtsfc,dtsfcp,wgtmin
     real(r_kind):: sst00,sst01,sst10,sst11,dlat,dlon
     logical outside

     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
     else
        dlat=dlat_earth
        dlon=dlon_earth
        call grdcrd(dlat,ione,rlats_sfc,nlat_sfc,ione)
        call grdcrd(dlon,ione,rlons_sfc,nlon_sfc,ione)
     end if

     iy=int(dlon); ix=int(dlat)
     dy  =dlon-iy; dx  =dlat-ix
     dx1 =one-dx;    dy1 =one-dy
     w00=dx1*dy1; w10=dx*dy1; w01=dx1*dy; w11=dx*dy

     ix=min(max(ione,ix),nlat_sfc); iy=min(max(izero,iy),nlon_sfc)
     ixp=min(nlat_sfc,ix+ione); iyp=iy+ione
     if(iy==izero) iy=nlon_sfc
     if(iyp==nlon_sfc+ione) iyp=ione


!    Get time interpolation factors for surface files
     if(obstime > hrdifsfc(1) .and. obstime <= hrdifsfc(nfldsfc))then
        do j=1,nfldsfc-ione
           if(obstime > hrdifsfc(j) .and. obstime <= hrdifsfc(j+ione))then
              itsfc=j
              itsfcp=j+ione
              dtsfc=(hrdifsfc(j+ione)-obstime)/(hrdifsfc(j+ione)-hrdifsfc(j))
           end if
        end do
     else if(obstime <=hrdifsfc(1))then
        itsfc=ione
        itsfcp=ione
        dtsfc=one
     else
        itsfc=nfldsfc
        itsfcp=nfldsfc
        dtsfc=one
     end if
     dtsfcp=one-dtsfc

     sst00= sst_full(ix ,iy ,itsfc)*dtsfc+sst_full(ix ,iy ,itsfcp)*dtsfcp
     sst01= sst_full(ix ,iyp,itsfc)*dtsfc+sst_full(ix ,iyp,itsfcp)*dtsfcp
     sst10= sst_full(ixp,iy ,itsfc)*dtsfc+sst_full(ixp,iy ,itsfcp)*dtsfcp
     sst11= sst_full(ixp,iyp,itsfc)*dtsfc+sst_full(ixp,iyp,itsfcp)*dtsfcp

!    Interpolate sst to obs location

     tsavg=sst00*w00+sst10*w10+sst01*w01+sst11*w11

     idomsfc=isli_full(ix ,iy )
     wgtmin = w00
     if(wgtmin < w01 )then
        idomsfc=isli_full(ix ,iyp)
        wgtmin = w01
     end if
     if(wgtmin < w10)then
        idomsfc=isli_full(ixp,iy )
        wgtmin = w10
     end if
     if(wgtmin < w11)then
        idomsfc=isli_full(ixp,iyp)
        wgtmin = w11
     end if
     if((isli_full(ix ,iy ) /= isli_full(ix ,iyp)) .or. &
        (isli_full(ix ,iy ) /= isli_full(ixp,iy )) .or. &
        (isli_full(ix ,iy ) /= isli_full(ixp,iyp)) .or. &
        (isli_full(ixp,iy ) /= isli_full(ix ,iyp)) .or. &
        (isli_full(ixp,iy ) /= isli_full(ixp,iyp)) .or. &
        (isli_full(ix ,iyp) /= isli_full(ixp,iyp)) ) idomsfc = idomsfc+3_i_kind

!    Space-time interpolation of fields from surface wind speed

     ff10=(fact10_full(ix ,iy ,itsfc )*w00+ &
           fact10_full(ixp,iy ,itsfc )*w10+ &
           fact10_full(ix ,iyp,itsfc )*w01+ &
           fact10_full(ixp,iyp,itsfc )*w11)*dtsfc + &
          (fact10_full(ix ,iy ,itsfcp)*w00+ &
           fact10_full(ixp,iy ,itsfcp)*w10+ &
           fact10_full(ix ,iyp,itsfcp)*w01+ &
           fact10_full(ixp,iyp,itsfcp)*w11)*dtsfcp

     sfcr=(sfc_rough_full(ix ,iy ,itsfc )*w00+ &
           sfc_rough_full(ixp,iy ,itsfc )*w10+ &
           sfc_rough_full(ix ,iyp,itsfc )*w01+ &
           sfc_rough_full(ixp,iyp,itsfc )*w11)*dtsfc + &
          (sfc_rough_full(ix ,iy ,itsfcp)*w00+ &
           sfc_rough_full(ixp,iy ,itsfcp)*w10+ &
           sfc_rough_full(ix ,iyp,itsfcp)*w01+ &
           sfc_rough_full(ixp,iyp,itsfcp)*w11)*dtsfcp

     return
end subroutine deter_sfc2
subroutine deter_sfc_fov(fov_flag,ifov,instr,ichan,sat_aziang,dlat_earth_deg,&
                         dlon_earth_deg,expansion,obstime,isflg,idomsfc, &
                         sfcpct,vfr,sty,vty,stp,sm,ff10,sfcr,zz,sn,ts,tsavg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_sfc_fov           determine surface characteristics
!   prgmmr: gayno            org: np2                date: 2008-11-04
!
! abstract:  determines surface characteristics within a field of view
!            based on model information and the size/shape of the 
!            field of view.
!
! program history log:
!   2008-11-04 gayno
!   2009-12-20 gayno - modify to use relative antenna power.
!
!   input argument list:
!     fov_flag        - is this a crosstrack or conical instrument?
!     ichan           - channel number - conical scanners only
!     ifov            - field of view number
!     instr           - instrument number
!     sat_aziang      - satellite azimuth angle
!     dlat_earth_deg  - latitude of fov center (degrees)
!     dlon_earth_deg  - longitude of fov center (degrees)
!     expansion       - fov expansion factor
!     obstime         - observation time
!
!   output argument list:
!     idomsfc  - dominate surface type
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed
!     sfcpct(0:3)- percentage of 4 surface types
!                (0) - sea percentage
!                (1) - land percentage
!                (2) - sea ice percentage
!                (3) - snow percentage
!     vfr      - vegetation fraction
!     sty      - dominate soil type
!     vty      - dominate vegetation type
!     stp      - top layer soil temperature
!     sm       - top layer soil moisture
!     ff10     - wind factor
!     sfcr     - surface roughness lenght
!     zz       - terrain height
!     sn       - snow depth
!     ts       - skin temperature for each surface type
!     tsavg    - average skin temperature
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use calc_fov_crosstrk, only    : npoly, fov_ellipse_crosstrk, inside_fov_crosstrk
  use calc_fov_conical, only : fov_ellipse_conical, inside_fov_conical
  use constants, only   : ione, deg2rad, rad2deg, one, zero, two
  use gridmod, only     : nlat_sfc, rlats_sfc, dx_gfs, regional, tll2xy, txy2ll
  use guess_grids, only : nfldsfc, hrdifsfc
  use kinds, only       : i_kind, r_kind

  implicit none

! Declare passed variables.
  character(len=*), intent(in   ) :: fov_flag
  integer(i_kind) , intent(in   ) :: ifov, instr
  integer(i_kind) , intent(in   ) :: ichan
  real(r_kind)    , intent(in   ) :: dlat_earth_deg, dlon_earth_deg, sat_aziang
  real(r_kind)    , intent(in   ) :: expansion, obstime

  integer(i_kind) , intent(  out) :: isflg, idomsfc
  real(r_kind)    , intent(  out) :: sfcpct(0:3), sty, vty, vfr, stp, sm
  real(r_kind)    , intent(  out) :: ff10, sfcr, zz, sn, ts(0:3), tsavg

! Declare local variables.
  integer(i_kind)              :: i, ii, iii, j, jj, jjj
  integer(i_kind)              :: is(npoly), js(npoly)
  integer(i_kind)              :: n, nearest_i, nearest_j
  integer(i_kind), allocatable :: max_i(:), min_i(:)
  integer(i_kind)              :: ifull, jstart, jend
  integer(i_kind)              :: itsfc, itsfcp
  integer(i_kind)              :: subgrid_lengths_x, subgrid_lengths_y
  logical                      :: outside
  real(r_kind)                 :: lats_edge_fov(npoly), lons_edge_fov(npoly)
  real(r_kind)                 :: lat_mdl, lon_mdl
  real(r_kind)                 :: lat_rad, lon_rad
  real(r_kind)                 :: dtsfc, dtsfcp
  real(r_kind)                 :: x, xstart, xend, y, ystart, yend
  real(r_kind)                 :: dx_fov, dx_fov_max, dy_fov, power
  real(r_kind)                 :: del, mid, rlon1, rlon2
  real(r_kind), allocatable    :: y_off(:), x_off(:)

  type surface2
     sequence
     real(r_kind) :: vfr
     real(r_kind) :: sfcr
     real(r_kind) :: sm
     real(r_kind) :: stp
     real(r_kind) :: ff10
     real(r_kind) :: sn
     real(r_kind) :: ts
  end type surface2

  type(surface2) :: sfc_mdl  ! holds time interpolated surface data

  type surface
     sequence
     real(r_kind) :: vfr
     real(r_kind) :: sm
     real(r_kind) :: stp
     real(r_kind) :: ff10
     real(r_kind) :: sfcr
     real(r_kind) :: zz
     real(r_kind) :: sn
     real(r_kind), dimension(0:3)  :: ts
     real(r_kind), dimension(0:3)  :: count
     real(r_kind), dimension(0:24) :: count_vty
     real(r_kind), dimension(0:16) :: count_sty
  end type surface

  type(surface) :: sfc_sum  ! holds sums during integration across fov

! Get time interpolation factors for surface files

  if(obstime > hrdifsfc(1) .and. obstime <= hrdifsfc(nfldsfc))then
     do j=1,nfldsfc-ione
        if(obstime > hrdifsfc(j) .and. obstime <= hrdifsfc(j+ione))then
           itsfc=j
           itsfcp=j+ione
           dtsfc=(hrdifsfc(j+ione)-obstime)/(hrdifsfc(j+ione)-hrdifsfc(j))
        end if
     end do
  else if(obstime <=hrdifsfc(1))then
     itsfc=ione
     itsfcp=ione
     dtsfc=one
  else
     itsfc=nfldsfc
     itsfcp=nfldsfc
     dtsfc=one
  end if
  dtsfcp=one-dtsfc
! The algorithm that locates the fov breaks down if its crosses the pole.
! so, just assign surface characteristics using a nearest neighbor
! approach.

  if (abs(dlat_earth_deg) > 88.0_r_kind) then
!    print*,'USE NEAREST NEIGHBOR NEAR POLE'
     if (regional) then
        lat_rad = dlat_earth_deg*deg2rad
        lon_rad = dlon_earth_deg*deg2rad
        call tll2xy(lon_rad,lat_rad,x,y,outside)
        nearest_i = nint(x)
        nearest_j = nint(y)
     else
        y = dlat_earth_deg*deg2rad
        call grdcrd(y,ione,rlats_sfc,nlat_sfc,ione)
        nearest_j = nint(y)
        jj = nearest_j
        if (jj > nlat_sfc/2_i_kind) jj = nlat_sfc - nearest_j + ione
        x = (dlon_earth_deg/dx_gfs(jj)) + one
        nearest_i = nint(x)
        call reduce2full(nearest_i,nearest_j,ifull)
        nearest_i = ifull
     end if
     call time_int_sfc(nearest_i,nearest_j,itsfc,itsfcp,dtsfc,dtsfcp,sfc_mdl)
     call init_sfc(sfc_sum)
     power = one
     call accum_sfc(nearest_i,nearest_j,power,sfc_mdl,sfc_sum)
     call calc_sfc(sfc_sum,isflg,idomsfc,sfcpct,vfr,sty,vty,sm,stp,ff10,sfcr,zz,sn,ts,tsavg)
     return
  endif

! Determine the edge of the fov.  the fov is described by a polygon with
! with npoly-1 vertices.  the routine returns the lat/lon of the intersection
! of the vertices.  the size of the polygon is a function of the
! expansion factor.

  if (fov_flag=="crosstrk")then
     call fov_ellipse_crosstrk(ifov,instr,sat_aziang,dlat_earth_deg,dlon_earth_deg, &
                               lats_edge_fov,lons_edge_fov)
  elseif(fov_flag=="conical")then
     call fov_ellipse_conical(ichan,sat_aziang,dlat_earth_deg,dlon_earth_deg, &
                              lats_edge_fov,lons_edge_fov)
  endif

  if (regional) then
     xstart=999999._r_kind
     xend=-999999._r_kind
     ystart=999999._r_kind
     yend=-999999._r_kind
     do n = 1, npoly
        lat_rad = lats_edge_fov(n)*deg2rad
        lon_rad = lons_edge_fov(n)*deg2rad
        call tll2xy(lon_rad,lat_rad,x,y,outside)
! if any part of fov is outside grid, just set is/js to
! be the near grid point at fov center.  we already know the
! center grid point is inside the grid as that is checked
! in calling routine.
        if (outside) then
!       print*,"FOV ON EDGE OF GRID"
           lat_rad = dlat_earth_deg*deg2rad
           lon_rad = dlon_earth_deg*deg2rad
           call tll2xy(lon_rad,lat_rad,x,y,outside)
           js = nint(y)
           is = nint(x)
           exit
        endif
        xstart=min(xstart,x)
        xend=max(xend,x)
        ystart=min(ystart,y)
        yend=max(yend,y)
        is(n)=nint(x)
        js(n)=nint(y)
     enddo
     jstart=minval(js)
     jend=maxval(js)
     allocate (max_i(jstart:jend))
     allocate (min_i(jstart:jend))
     max_i = -999_i_kind
     min_i = 999999_i_kind
     do j = jstart, jend
        do n = 1, npoly
           if (js(n) == j) then
              max_i(j) = max(max_i(j),is(n))
              min_i(j) = min(min_i(j),is(n))
           endif
        enddo
     enddo
  else   ! global grid
!  Locate the fov on the model grid.  in the "j" direction, this is
!  based on the latitudinal extent of the fov.
     yend = maxval(lats_edge_fov)*deg2rad
     call grdcrd(yend,ione,rlats_sfc,nlat_sfc,ione)
     ystart = minval(lats_edge_fov)*deg2rad
     call grdcrd(ystart,ione,rlats_sfc,nlat_sfc,ione)
!  Note two extra rows are added for the n/s poles.
     jstart = nint(ystart)
     jstart = max(jstart,2_i_kind)
     jend = nint(yend)
     jend = min(jend,nlat_sfc-ione)
!  Locate the extent of the fov on the model grid in the "i" direction.  note, the
!  algorithm works on the reduced gaussian grid.  hence, the starting/ending
!  "i" indices are a function of "j".
     allocate (max_i(jstart:jend))
     allocate (min_i(jstart:jend))
     do j = jstart, jend
        jj = j
        if (jj > nlat_sfc/2_i_kind) jj = nlat_sfc - j + ione
        x = (minval(lons_edge_fov)/dx_gfs(jj)) + one
        nearest_i = nint(x)
        min_i(j) = nearest_i
        x = (maxval(lons_edge_fov)/dx_gfs(jj)) + one
        nearest_i = nint(x)
        max_i(j) = nearest_i
     enddo
  end if  ! is this regional or global

! In this case, the fov is located completely within one grid
! point. this is common when the fov is small compared with
! the model grid resolution.

  if ((jstart == jend) .and. (max_i(jstart) == min_i(jstart))) then
!   print*,'ONLY ONE MODEL POINT WITHIN FOV'
     nearest_j = jstart
     nearest_i = max_i(jstart)
     if (.not. regional) then
        call reduce2full(nearest_i,nearest_j,ifull)
        nearest_i = ifull
     endif
     call time_int_sfc(nearest_i,nearest_j,itsfc,itsfcp,dtsfc,dtsfcp,sfc_mdl)
     call init_sfc(sfc_sum)
     power = one
     call accum_sfc(nearest_i,nearest_j,power,sfc_mdl,sfc_sum)
     call calc_sfc(sfc_sum,isflg,idomsfc,sfcpct,vfr,sty,vty,sm,stp,ff10,sfcr,zz,sn,ts,tsavg)
     deallocate(max_i,min_i)
     return
  endif

! Find the size of the fov in model grid units.  if the
! fov is small compared to the model grid, there is the
! possibility that there are no model grid boxes inside the fov.
! (a grid box is "inside" the fov if its center is within
! the fov.) this situation can be avoided by
! "chopping" the model grid boxes into smaller pieces, which
! is done below.  first, find the n/s size of the fov
! in grid units.

  if (regional) then
     dy_fov = abs(yend-ystart)
     dx_fov = abs(xend-xstart)
  else  ! global
     dy_fov = abs(yend-ystart)  ! n/s size of fov in grid units.
!  Find the e/w size of the fov in grid units.  note: the resolution
!  of the model decreases towards the poles.  use the max
!  value of model grid spacing in this calculation to make
!  the most conservative estimate as to whether or not to
!  "chop" the model grid boxes.
     dx_fov_max = zero
     do j = jstart,jend
        jj = j
        if (jj > nlat_sfc/2_i_kind) jj = nlat_sfc - j + ione
        dx_fov_max = max(dx_fov_max, dx_gfs(jj))
     enddo
!  When taking the longitudinal difference, don't worry
!  about greenwich or the dateline as the fov code calculates
!  longitude relative to the center of the fov.
     rlon1 = maxval(lons_edge_fov)
     rlon2 = minval(lons_edge_fov)
     dx_fov = (rlon1-rlon2)/dx_fov_max
  end if  ! is this regional or global?

! if the fov is small compared to the model resolution, there
! is a possibility that there will be no model points located
! within the fov.  to prevent this, the model grid may be
! subdivided into smaller pieces.  this subdivision is
! done separately for each direction.

  subgrid_lengths_x = nint(one/dx_fov) + ione
  subgrid_lengths_y = nint(one/dy_fov) + ione

  99 continue

! If the fov is very small compared to the model grid, it
! is more computationally efficient to take a simple average.

  if (subgrid_lengths_x > 7_i_kind .or. subgrid_lengths_y > 7_i_kind) then
!   print*,'FOV MUCH SMALLER THAN MODEL GRID POINTS, TAKE SIMPLE AVERAGE.'
     call init_sfc(sfc_sum)
     if (regional) then
        do j = jstart, jend
           do i = min_i(j), max_i(j)
              call time_int_sfc(i,j,itsfc,itsfcp,dtsfc,dtsfcp,sfc_mdl)
              power = one
              call accum_sfc(i,j,power,sfc_mdl,sfc_sum)
           enddo
        enddo
     else  ! global
        do j = jstart, jend
           do i = min_i(j), max_i(j)
              ii = i
              call reduce2full(ii,j,ifull)
              call time_int_sfc(ifull,j,itsfc,itsfcp,dtsfc,dtsfcp,sfc_mdl)
              power = one
              call accum_sfc(ifull,j,power,sfc_mdl,sfc_sum)
           enddo
        enddo
     endif
     call calc_sfc(sfc_sum,isflg,idomsfc,sfcpct,vfr,sty,vty,sm,stp,ff10,sfcr,zz,sn,ts,tsavg)
     deallocate(max_i,min_i)
     return
  endif

  mid = (float(subgrid_lengths_y)-one)/two + one
  del = one/ float(subgrid_lengths_y)

  allocate (y_off(subgrid_lengths_y))

  do i= 1, subgrid_lengths_y
     y_off(i) = (float(i)-mid)*del
  enddo

  mid = (float(subgrid_lengths_x)-one)/two + one
  del = one / float(subgrid_lengths_x)

  allocate (x_off(subgrid_lengths_x))
  do i= 1, subgrid_lengths_x
     x_off(i) = (float(i)-mid)*del
  enddo

! Determine the surface characteristics by integrating over the
! fov.

  call init_sfc(sfc_sum)

  if (regional) then
     do j = jstart, jend
        do i = min_i(j), max_i(j)
           call time_int_sfc(i,j,itsfc,itsfcp,dtsfc,dtsfcp,sfc_mdl)
           do jjj = 1, subgrid_lengths_y
              y = float(j) + y_off(jjj)
              do iii = 1, subgrid_lengths_x
                 x = float(i) + x_off(iii)
                 call txy2ll(x,y,lon_rad,lat_rad)
                 lat_mdl = lat_rad*rad2deg
                 lon_mdl = lon_rad*rad2deg
                 if (lon_mdl < zero) lon_mdl = lon_mdl + 360._r_kind
                 if (lon_mdl >= 360._r_kind) lon_mdl = lon_mdl - 360._r_kind
                 if (fov_flag=="crosstrk")then
                    call inside_fov_crosstrk(instr,ifov,sat_aziang, &
                                            dlat_earth_deg,dlon_earth_deg, &
                                            lat_mdl,    lon_mdl,  &
                                            expansion, ichan, power )
                 elseif (fov_flag=="conical")then
                    call inside_fov_conical(instr,ichan,sat_aziang, &
                                           dlat_earth_deg,dlon_earth_deg,&
                                           lat_mdl,    lon_mdl,  &
                                           expansion, power )
                 endif
                 call accum_sfc(i,j,power,sfc_mdl,sfc_sum)
              enddo
           enddo
        enddo
     enddo
  else
     do j = jstart, jend
        jj = j
        if (j > nlat_sfc/2_i_kind) jj = nlat_sfc - j + ione
        do i = min_i(j), max_i(j)
           call reduce2full(i,j,ifull)
           call time_int_sfc(ifull,j,itsfc,itsfcp,dtsfc,dtsfcp,sfc_mdl)
           do jjj = 1, subgrid_lengths_y
              if (y_off(jjj) >= zero) then
                 lat_mdl = (one-y_off(jjj))*rlats_sfc(j)+y_off(jjj)*rlats_sfc(j+ione)
              else
                 lat_mdl = (one+y_off(jjj))*rlats_sfc(j)-y_off(jjj)*rlats_sfc(j-ione)
              endif
              lat_mdl = lat_mdl * rad2deg
              do iii = 1, subgrid_lengths_x
!           Note, near greenwich, "i" index may be out of range.  that is
!           ok here when calculating longitude even if the value is
!           greater than 360. the ellipse code works from longitude relative
!           to the center of the fov.
                 lon_mdl = (float(i)+x_off(iii) - one) * dx_gfs(jj)
                 if (fov_flag=="crosstrk")then
                    call inside_fov_crosstrk(instr,ifov,sat_aziang, &
                                            dlat_earth_deg,dlon_earth_deg, &
                                            lat_mdl,    lon_mdl,  &
                                            expansion, ichan, power )
                 elseif (fov_flag=="conical")then
                    call inside_fov_conical(instr,ichan,sat_aziang, &
                                           dlat_earth_deg,dlon_earth_deg,&
                                           lat_mdl,    lon_mdl,  &
                                           expansion, power )
                 endif
                 call accum_sfc(ifull,j,power,sfc_mdl,sfc_sum)
              enddo
           enddo
        enddo
     enddo
  endif  ! regional or global
  deallocate (x_off, y_off)

! If there were no model points within the fov, the model points need to be
! "chopped" into smaller pieces.

  if (sum(sfc_sum%count) == zero) then
     close(9)
     subgrid_lengths_x = subgrid_lengths_x + ione
     subgrid_lengths_y = subgrid_lengths_y + ione
!    print*,'NO GRID POINTS INSIDE FOV, CHOP MODEL BOX INTO FINER PIECES',subgrid_lengths_x,subgrid_lengths_y
     goto 99
  else
     call calc_sfc(sfc_sum,isflg,idomsfc,sfcpct,vfr,sty,vty,sm,stp,ff10,sfcr,zz,sn,ts,tsavg)
  endif

  deallocate (max_i, min_i)

  return
end subroutine deter_sfc_fov
subroutine reduce2full(ireduce, j, ifull)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    reduce2full        find "i" index on "full" gfs grid
!   prgmmr: gayno            org: np2                date: 2008-11-04
!
! abstract:  for a given "i" index on the gfs "reduced" grid, find
!            the corresponding "i" index on the "full" grid.
!
! program history log:
!   2008-11-04 gayno
!
!   input argument list:
!     ireduce    - "i" index on reduced gfs grid
!     j          - "j" index (same for both grids)
!
!   output argument list:
!     ifull      - "i" index on full gfs grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only     : i_kind, r_kind
  use constants, only : izero, ione
  use gridmod, only   : nlat_sfc, nlon_sfc, lpl_gfs

  implicit none

! Declare passed variables.
  integer(i_kind), intent(in   ) :: ireduce, j
  integer(i_kind), intent(  out) :: ifull

! Declare local variables.
  integer(i_kind)              :: ii, jj, m1, m2
  real(r_kind)                 :: r, x1

  jj = j
  if (j > nlat_sfc/2_i_kind) jj = nlat_sfc - j + ione
  m2 = lpl_gfs(jj)
  m1 = nlon_sfc
  r=real(m1)/real(m2)
  ii = ireduce
  if (ii <= izero) ii = ii + lpl_gfs(jj)
  if (ii > lpl_gfs(jj)) ii = ii - lpl_gfs(jj)
  x1=(ii-ione)*r
  ifull=mod(nint(x1),m1)+ione
  return
end subroutine reduce2full
subroutine init_sfc(sfc_sum)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_sfc               initialize surface structure
!   prgmmr: gayno            org: np2                date: 2008-11-04
!
! abstract:  initialize to zero all elements of the surface type
!            data structure.
!
! program history log:
!   2008-11-04 gayno
!
!   input argument list:
!     none
!
!   output argument list:
!     sfc_sum   - holds 'sums' used in the calculation of surface fields
!                 within a fov.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only     : r_kind
  use constants, only : zero

  implicit none

  type surface
     sequence
     real(r_kind) :: vfr
     real(r_kind) :: sm
     real(r_kind) :: stp
     real(r_kind) :: ff10
     real(r_kind) :: sfcr
     real(r_kind) :: zz
     real(r_kind) :: sn
     real(r_kind), dimension(0:3)  :: ts
     real(r_kind), dimension(0:3)  :: count
     real(r_kind), dimension(0:24) :: count_vty
     real(r_kind), dimension(0:16) :: count_sty
  end type surface

  type(surface), intent(out) :: sfc_sum

! The surface characteristics of a fov are determined by:
!
! sum(sfc_field*ant_power) / sum(ant_power)
!
! This structure holds the various 'sums' and thus must
! be initialized to zero before use.

  sfc_sum%vfr        = zero  ! greenness
  sfc_sum%sm         = zero  ! soil moisture - top layer
  sfc_sum%stp        = zero  ! soil temperature - top layer
  sfc_sum%ff10       = zero  ! wind factor
  sfc_sum%sfcr       = zero  ! roughness length
  sfc_sum%zz         = zero  ! terrain
  sfc_sum%sn         = zero  ! snow depth
  sfc_sum%ts         = zero  ! skin temperature for each surface type
                             ! 0-water,1-land,2-ice,3-snow
  sfc_sum%count      = zero  ! sum of the antenna power for each
                             ! surface type
  sfc_sum%count_vty  = zero  ! vegetation type
  sfc_sum%count_sty  = zero  ! soil type

  return

end subroutine init_sfc
subroutine time_int_sfc(ix,iy,itsfc,itsfcp,dtsfc,dtsfcp,sfc_mdl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    time_int_sfc           time interpolate surface data
!   prgmmr: gayno            org: np2                date: 2008-11-04
!
! abstract:  time interpolate surface data to the observation time
!
! program history log:
!   2008-11-04 gayno
!
!   input argument list:
!     ix, iy       - x/y indices of grid point
!     dtsfc/dtsfcp - time interpolation factors
!     itsfc/itsfcp - bounding indices of data
!
!   output argument list:
!     sfc_mdl - holds surface information for a single model point
!               valid at the observation time
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only    : i_kind, r_kind
  use satthin, only  : sno_full, veg_frac_full, soil_temp_full, &
                       soil_moi_full, fact10_full, sfc_rough_full, &
                       sst_full

  implicit none

! Declare passed variables.
  integer(i_kind), intent(in   ) :: ix,iy,itsfc, itsfcp
  real(r_kind)   , intent(in   ) :: dtsfc, dtsfcp

  type surface2
     sequence
     real(r_kind) :: vfr
     real(r_kind) :: sfcr
     real(r_kind) :: sm
     real(r_kind) :: stp
     real(r_kind) :: ff10
     real(r_kind) :: sn
     real(r_kind) :: ts
  end type surface2

  type(surface2) , intent(  out) :: sfc_mdl

! Note, indices are reversed (y/x).

  sfc_mdl%sn=sno_full(iy,ix,itsfc)*dtsfc+sno_full(iy,ix,itsfcp)*dtsfcp
  sfc_mdl%vfr=veg_frac_full(iy,ix,itsfc)*dtsfc+veg_frac_full(iy,ix,itsfcp)*dtsfcp
  sfc_mdl%stp=soil_temp_full(iy,ix,itsfc)*dtsfc+soil_temp_full(iy,ix,itsfcp)*dtsfcp
  sfc_mdl%sm=soil_moi_full(iy,ix,itsfc)*dtsfc+soil_moi_full(iy,ix,itsfcp)*dtsfcp
  sfc_mdl%ff10=fact10_full(iy,ix,itsfc)*dtsfc+fact10_full(iy,ix,itsfcp)*dtsfcp
  sfc_mdl%sfcr=sfc_rough_full(iy,ix,itsfc)*dtsfc+sfc_rough_full(iy,ix,itsfcp)*dtsfcp
  sfc_mdl%ts=sst_full(iy,ix,itsfc)*dtsfc+sst_full(iy,ix,itsfcp)*dtsfcp

  return
end subroutine time_int_sfc
subroutine accum_sfc(i,j,power,sfc_mdl,sfc_sum)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    accum_sfc           "accumulate" surface fields
!   prgmmr: gayno            org: np2                date: 2008-11-04
!
! abstract:  The surface characteristics of a fov are determined by:
!            sum(sfc_field*ant_power) / sum(ant_power)
!            this routine determines the required "sums".
!
! program history log:
!   2008-11-04 gayno
!
!   input argument list:
!     i, j         - i/j indices of model grid point
!     power        - antenna power
!     sfc_sum      - holds required surface data "sums"
!
!   output argument list:
!     sfc_mdl - holds surface information for a single model point
!               valid at the observation time
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only     : i_kind, r_kind
  use constants, only : ione,zero,one_tenth
  use gridmod, only   : regional
  use satthin, only   : isli_full,soil_type_full,veg_type_full,zs_full,zs_full_gfs

  implicit none

! Declare passed variables.
  integer(i_kind), intent(in   ) :: i, j
  real(r_kind)   , intent(in   ) :: power

  type surface2
     sequence
     real(r_kind) :: vfr    ! greenness (veg fraction)
     real(r_kind) :: sfcr   ! roughness length
     real(r_kind) :: sm     ! soil moisture
     real(r_kind) :: stp    ! soil temperature
     real(r_kind) :: ff10   ! wind factor
     real(r_kind) :: sn     ! snow depth
     real(r_kind) :: ts     ! skin temperature
  end type surface2

  type(surface2) , intent(in   ) :: sfc_mdl

  type surface
     sequence
     real(r_kind) :: vfr    ! greenness (veg fraction)
     real(r_kind) :: sm     ! soil moisture
     real(r_kind) :: stp    ! soil temperautre
     real(r_kind) :: ff10   ! wind factor
     real(r_kind) :: sfcr   ! roughness length
     real(r_kind) :: zz     ! terrain height
     real(r_kind) :: sn     ! snow depth
     real(r_kind), dimension(0:3)  :: ts        ! skin temp (each land type)
     real(r_kind), dimension(0:3)  :: count     ! count of each land type
     real(r_kind), dimension(0:24) :: count_vty ! count of each landuse type
     real(r_kind), dimension(0:16) :: count_sty ! count of each soil type
  end type surface

  type(surface)  , intent(inout) :: sfc_sum

! Declare local parameters.
  real(r_kind),parameter:: minsnow=one_tenth

! Declare local variables.
  integer(i_kind)             :: mask, sty, vty

  if (power == zero) return

  mask=isli_full(j,i)
  if (mask>=ione.and.sfc_mdl%sn>minsnow) mask=3_i_kind

  if (mask==ione) then  ! bare (non-snow covered) land
     vty=nint(veg_type_full(j,i))
     sfc_sum%count_vty(vty)=sfc_sum%count_vty(vty)+power
     sty=nint(soil_type_full(j,i))
     sfc_sum%count_sty(sty)=sfc_sum%count_sty(sty)+power
     sfc_sum%vfr=sfc_sum%vfr + (power*sfc_mdl%vfr)
     sfc_sum%sm=sfc_sum%sm + (power*sfc_mdl%sm)
     sfc_sum%stp=sfc_sum%stp + (power*sfc_mdl%stp)
  elseif (mask==3_i_kind) then  ! snow cover land or sea ice
     sfc_sum%sn=sfc_sum%sn + (power*sfc_mdl%sn)
  endif

! wind factor, roughness length and terrain are summed
! across all surface types.
  sfc_sum%ff10=sfc_sum%ff10 + (power*sfc_mdl%ff10)
  sfc_sum%sfcr=sfc_sum%sfcr + (power*sfc_mdl%sfcr)
  if (regional) then
     sfc_sum%zz=sfc_sum%zz + (power*zs_full(j,i))
  else
     sfc_sum%zz=sfc_sum%zz + (power*zs_full_gfs(j,i))
  endif

! keep track of skin temperature for each surface type
  sfc_sum%ts(mask)=sfc_sum%ts(mask) + (power*sfc_mdl%ts)
! keep count of each surface type
  sfc_sum%count(mask) = power + sfc_sum%count(mask)

  return
end subroutine accum_sfc
subroutine calc_sfc(sfc_sum,isflg,idomsfc,sfcpct,vfr,sty,vty,sm, &
                    stp,ff10,sfcr,zz,sn,ts,tsavg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calc_sfc            calculate surface fields
!   prgmmr: gayno            org: np2                date: 2008-11-04
!
! abstract:  The surface characteristics of a fov are determined by:
!            sum(sfc_field*ant_power) / sum(ant_power)
!
! program history log:
!   2008-11-04 gayno
!
!   input argument list:
!     sfc_sum      - holds required surface data "sums"
!
!   output argument list:
!     idomsfc  - dominate surface type
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!     isflg    - surface flag
!                0 sea
!                1 land
!                2 sea ice
!                3 snow
!                4 mixed
!     sfcpct(0:3)- percentage of 4 surface types
!                (0) - sea percentage
!                (1) - land percentage
!                (2) - sea ice percentage
!                (3) - snow percentage
!     vfr      - vegetation fraction
!     sty      - dominate soil type
!     vty      - dominate vegetation type
!     stp      - top layer soil temperature
!     sm       - top layer soil moisture
!     ff10     - wind factor
!     sfcr     - surface roughness lenght
!     zz       - terrain height
!     sn       - snow depth
!     ts       - skin temperature for each surface type
!     tsavg    - average skin temperature
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use constants, only : izero, ione, zero, one
  use kinds, only : i_kind, r_kind

  implicit none

! Declare passed variables
  integer(i_kind), intent(  out) :: isflg, idomsfc(1)
  real(r_kind)   , intent(  out) :: sm, stp, sty, vty, vfr, sfcpct(0:3)
  real(r_kind)   , intent(  out) :: ff10, sfcr, zz, sn, ts(0:3), tsavg

  type surface
     sequence
     real(r_kind) :: vfr
     real(r_kind) :: sm
     real(r_kind) :: stp
     real(r_kind) :: ff10
     real(r_kind) :: sfcr
     real(r_kind) :: zz
     real(r_kind) :: sn
     real(r_kind), dimension(0:3)  :: ts
     real(r_kind), dimension(0:3)  :: count
     real(r_kind), dimension(0:24) :: count_vty
     real(r_kind), dimension(0:16) :: count_sty
  end type surface

  type(surface)  , intent(in   ) :: sfc_sum

! Declare local variables
  integer(i_kind)   :: itmp(1), n
  real(r_kind)      :: count_tot, count_land, count_sty_tot, &
                       count_snow, count_vty_tot, count_ice, count_water

  count_tot = sum(sfc_sum%count)

! skin temperature over entire fov
  tsavg = sum(sfc_sum%ts(0:3))/count_tot

! landuse is predominate type
  count_vty_tot = sum(sfc_sum%count_vty)
  if (count_vty_tot==zero) then
     vty=zero
  else
     itmp=lbound(sfc_sum%count_vty)-ione+maxloc(sfc_sum%count_vty)
     vty=float(itmp(1))
  endif

! soil type is predominate type
  count_sty_tot = sum(sfc_sum%count_sty)
  if (count_sty_tot==zero) then
     sty=zero
  else
     itmp=lbound(sfc_sum%count_sty)-ione+maxloc(sfc_sum%count_sty)
     sty=float(itmp(1))
  endif

! fields for bare (non-snow covered) land
  count_land = sfc_sum%count(1)
  if (count_land>zero) then
     vfr = sfc_sum%vfr / count_land
     stp = sfc_sum%stp / count_land
     sm  = sfc_sum%sm / count_land
     ts(1) = sfc_sum%ts(1) / count_land
  else
     vfr = zero
     stp = zero
     sm  = one
     ts(1) = tsavg
  endif

! fields for open water
  count_water=sfc_sum%count(0)
  if(count_water>zero)then
     ts(0)=sfc_sum%ts(0)/count_water
  else
     ts(0)=tsavg
  endif

! fields for non-snow covered sea ice
  count_ice=sfc_sum%count(2)
  if(count_ice>zero)then
     ts(2)=sfc_sum%ts(2)/count_ice
  else
     ts(2)=tsavg
  endif

! fields for snow covered land and sea ice
  count_snow=sfc_sum%count(3)
  if(count_snow>zero)then
     sn=sfc_sum%sn/count_snow
     ts(3) = sfc_sum%ts(3) / count_snow
  else
     sn=zero
     ts(3) = tsavg
  endif

! percent of each surface type
  sfcpct=zero
  do n = 0, 3
     sfcpct(n) = sfc_sum%count(n) / count_tot
  enddo

  idomsfc=lbound(sfcpct)+maxloc(sfcpct)-ione

! wind factor, roughness and terrain are determined
! over entire fov.
  ff10 = sfc_sum%ff10/count_tot
  sfcr = sfc_sum%sfcr/count_tot
  zz   = sfc_sum%zz/count_tot

  isflg = izero
  if(sfcpct(0) > 0.99_r_kind)then
     isflg = izero      ! open water
  else if(sfcpct(1) > 0.99_r_kind)then
     isflg = ione       ! bare land
  else if(sfcpct(2) > 0.99_r_kind)then
     isflg = 2_i_kind   ! bare sea ice
  else if(sfcpct(3) > 0.99_r_kind)then
     isflg = 3_i_kind   ! snow covered land and sea ice
  else
     isflg = 4_i_kind   ! mixture
  end if

  return
 end subroutine calc_sfc

