subroutine read_airs(mype,val_airs,ithin,rmesh,jsatid,gstime,&
     infile,lunout,obstype,nread,ndata,nodata,twind,sis,&
     mype_root,mype_sub,npe_sub,mpi_comm_sub)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_airs                  read bufr format airs data
! prgmmr :   tahara          org: np20                date: 2002-12-03
!
! abstract:  This routine reads BUFR format AQUA radiance (brightness
!            temperature) files.  Optionally, the data are thinned to 
!            a specified resolution using simple quality control checks.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2002-12-03  tahara  - read aqua data in new bufr format
!   2004-05-28  kleist  - subroutine call update
!   2004-06-16  treadon - update documentation
!   2004-07-23  derber  - make changes to eliminate obs. earlier in thinning
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-08-25  eliu    - added option to read separate bufr table
!   2004-10-15  derber  - increase weight given to surface channel check
!                         in AIRS data selection algorithm
!   2005-01-26  derber - land/sea determination and weighting for data selection
!   2005-07-07  derber - clean up code and improve selection criteria
!   2005-09-08  derber - modify to use input group time window
!   2005-09-28  derber - modify to produce consistent surface info
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-11-22  derber  - include mean in bias correction
!   2005-11-29  parrish - modify getsfc to work for different regional options
!   2006-02-01  parrish - remove getsfc (different version called now in read_obs)
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-03-07  derber - correct error in nodata count
!   2006-03-09  jung - correct sat zenith angle error (used before defined)
!   2006-04-21  keyser/treadon - modify ufbseq calls to account for change
!                                in NCEP bufr sequence for AIRS data
!   2006-07-28  derber - modify reads so ufbseq not necessary
!                      - add solar and satellite azimuth angles remove isflg from output
!   2006-08-25  treadon - replace serial bufr i/o with parallel bufr i/o (mpi_io)
!
!   input argument list:
!     mype     - mpi task id
!     val_airs - weighting factor applied to super obs
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - sensor/instrument/satellite indicator
!     mype_root - "root" task for sub-communicator
!     mype_sub - mpi task id within sub-communicator
!     npe_sub  - number of data read tasks
!     mpi_comm_sub - sub-communicator for data read
!
!   output argument list:
!     nread    - number of BUFR AQUA observations read
!     ndata    - number of BUFR AQUA profiles retained for further processing
!     nodata   - number of BUFR AQUA observations retained for further processing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
! Use modules
  use kinds, only: r_kind,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
               finalcheck,checkob
  use radinfo, only: cbias,newchn,predx
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,&
       tll2xy,txy2ll,rlats,rlons
  use constants, only: zero,deg2rad,one,three,izero,ione,rad2deg
  use obsmod, only: iadate
  use mpi_bufr_mod, only: mpi_openbf,mpi_closbf,nblocks,mpi_nextblock,&
       mpi_readmg,mpi_ireadsb,mpi_ufbint,mpi_ufbrep

  implicit none


! Number of channels for sensors in BUFR
  integer(i_kind),parameter :: n_airschan = 281		!--- 281 subset ch out of 2378 ch for AIRS
  integer(i_kind),parameter :: n_amsuchan =  15
  integer(i_kind),parameter :: n_hsbchan  =   4
  integer(i_kind),parameter :: n_totchan  = n_amsuchan+n_airschan+n_hsbchan+1
  integer(i_kind),parameter :: maxinfo    =  18


! BUFR format for AQUASPOT 
! Input variables
  integer(i_kind)  ,intent(in) :: mype
  real(r_kind)     ,intent(in) :: twind
  real(r_kind)     ,intent(in) :: val_airs
  integer(i_kind)  ,intent(in) :: ithin
  character(len=10),intent(in) :: jsatid
  character(len=10),intent(in) :: infile
  character(len=10),intent(in) :: obstype
  real(r_kind)     ,intent(in) :: gstime
  integer(i_kind)  ,intent(in) :: lunout
  real(r_kind)     ,intent(in) :: rmesh
  character(len=20),intent(in) :: sis
  integer(i_kind)  ,intent(in) :: mype_root
  integer(i_kind)  ,intent(in) :: mype_sub
  integer(i_kind)  ,intent(in) :: npe_sub
  integer(i_kind)  ,intent(in) :: mpi_comm_sub  
  

! Output variables
  integer(i_kind),intent(inout) :: nread
  integer(i_kind)  ,intent(out) :: ndata,nodata
  

! BUFR file sequencial number
  character(len=512)  :: table_file
  integer(i_kind)     :: lnbufr = 10
  integer(i_kind)     :: lnbufrtab = 11

! Variables for BUFR IO    
  real(r_double),dimension(2) :: aquaspot
  real(r_double),dimension(12,3) :: allspot
  real(r_double),dimension(n_totchan) :: allchan
  real(r_double)     :: scbtseqn
  
  real(r_kind)      :: step, start
  character(len=8)  :: subset
  character(len=4)  :: senname
  character(len=80) :: allspotlist
  integer(i_kind)   :: nchanl,nchanlr
  integer(i_kind)   :: iret,ireadmg,ireadsb


! Work variables for time
  integer(i_kind)   :: idate
  integer(i_kind)   :: idate5(5)
  character(len=10) :: date
  real(r_kind)      :: sstime, tdiff
  integer(i_kind)   :: nmind
  integer(i_kind)   :: iy, im, idd, ihh


! Other work variables
  integer(i_kind)  :: nreal, ichsst, ichansst, isflg,ioffset
  integer(i_kind)  :: itx, k, nele, itt, iout,n,iscbtseqn,ix
  real(r_kind),dimension(0:4)::rlndsea
  real(r_kind)     :: chsstf,chsst
  real(r_kind)     :: w00, w10, w01, w11, ch15, ch3, df2, tt
  real(r_kind)     :: sstx, dlon, dlat
  real(r_kind)     :: dlon_earth,dlat_earth, tb,lza
  real(r_kind)     :: timedif, pred, crit1, qval, ch1, ch2, d0, cosza, dist1
  real(r_kind)     :: sat_zenang, sol_zenang, sat_aziang, sol_aziang
  real(r_kind)     :: ch8ch18, ch8ch19, ch18ch19, tmpinv
  real(r_kind),dimension(0:3) :: sfcpct
  integer(i_kind)  :: ifov, ioff, ilat, ilon 
  logical          :: outside,iuse
  integer(i_kind)  :: i, l, ll, iskip
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_kind),allocatable,dimension(:):: data_crit
  integer(i_kind),allocatable,dimension(:):: idata_itx
  integer(i_kind):: mmblocks
  integer(i_kind):: isubset



! Set standard parameters
  real(r_kind),parameter:: R60    =  60._r_kind
  real(r_kind),parameter:: R90    =  90._r_kind
  real(r_kind),parameter:: R360   = 360._r_kind
  real(r_kind),parameter:: d1     = 0.754_r_kind
  real(r_kind),parameter:: d2     = -2.265_r_kind
  real(r_kind),parameter:: tbmin  = 50._r_kind
  real(r_kind),parameter:: tbmax  = 550._r_kind

  real(r_kind) disterr,disterrmax,rlon00,rlat00,r01
  integer(i_kind) ntest

  logical           :: airs, amsua, hsb, airstab


! Initialize variables
  disterrmax=zero
  ntest=0
  nreal  = maxinfo
  ndata = 0
  nodata = 0
  airs=      obstype == 'airs'
  amsua=     obstype == 'amsua'
  hsb=       obstype == 'hsb'
  r01=0.01_r_kind

  ilon=3
  ilat=4

  if(airs)then
     ix=1
     step   = 1.1_r_kind
     start = -48.9_r_kind
     senname = 'AIRS'
     nchanl  = n_airschan
     nchanlr = n_airschan
     ioff=newchn(sis,1)-1
     ioffset=0
     ichansst   = newchn(sis,914)
     ichsst     = ichansst-ioff+ioffset
     rlndsea(0) = zero                       
     rlndsea(1) = 10._r_kind
     rlndsea(2) = 15._r_kind
     rlndsea(3) = 10._r_kind
     rlndsea(4) = 30._r_kind
     if (mype_sub==mype_root) &
          write(6,*)'READ_AIRS:  airs offset ',ioff,ichansst,ichsst
  else if(amsua)then
     ix=2
     step   = three + one/three
     start = -48. - one/three
!    start  = -48.33_r_kind
     senname = 'AMSU'
     nchanl  = n_amsuchan
     nchanlr = n_amsuchan
     ioff=newchn(sis,1)-1
     ioffset=n_airschan
     ichansst   = newchn(sis,1)
     ichsst     = ioffset +1            !channel 1
     rlndsea(0) = zero                       
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 20._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
  else if(hsb)then
     ix=3
     step   = 1.1_r_kind
     start  = -48.95_r_kind
     senname = 'HSB'
     nchanl  = n_hsbchan
     nchanlr = n_hsbchan+1
     ioff=newchn(sis,1)-1
     ioffset=iscbtseqn+n_amsuchan
     ichansst   = newchn(sis,4)
     ichsst     = ichansst-ioff+ioffset
     rlndsea(0) = zero                       
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 20._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
  endif
  allspotlist='SIID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA BEARAZ FOVN'


! Make thinning grids
  call makegrids(rmesh)

! Open BUFR file
  open(lnbufr,file=infile,form='unformatted')

! Open BUFR table
  table_file = 'airs_bufr.table'      ! make table file name
  inquire(file=table_file,exist=airstab)
  if (airstab) then
     if (mype_sub==mype_root) &
          write(6,*)'READ_AIRS:  Reading BUFR Table A file: ',table_file
     open(lnbufrtab,file=table_file)
     call openbf(lnbufr,'IN',lnbufrtab)
  else
     call openbf(lnbufr,'IN',lnbufr)
  endif
  call datelen(10)

! Read header
  call readmg(lnbufr,subset,idate,iret)
  call closbf(lnbufr)
  close(lnbufr)
  if( iret /= 0 ) goto 1000     ! no data?

  write(date,'( i10)') idate
  read(date,'(i4,3i2)') iy,im,idd,ihh
  if (mype_sub==mype_root) &
       write(6,*) 'READ_AIRS:     bufr file date is ',iy,im,idd,ihh
  
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

!    Read AIRSSPOT , AMSUSPOT and HSBSPOT

     call mpi_ufbrep(lnbufr,allspot,12,3,iret,allspotlist)

     if(iret /= 3) cycle read_loop

     dlat_earth = allspot(8,ix)
     dlon_earth = allspot(9,ix)
!    Check observing position
     if( abs(dlat_earth) > R90  .or. abs(dlon_earth) > R360 .or. &
        (abs(dlat_earth) == R90 .and. dlon_earth /= ZERO) )then
!       write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
!            ' STRANGE OBS POINT (LAT,LON):', dlat_earth, dlon_earth
        cycle read_loop
     endif

!    Retrieve observing position
     if(dlon_earth >= R360)then
        dlon_earth = dlon_earth - R360
     else if(dlon_earth < ZERO)then
        dlon_earth = dlon_earth + R360
     endif

     dlat_earth = dlat_earth * deg2rad
     dlon_earth = dlon_earth * deg2rad

!    If regional, map obs lat,lon to rotated grid.
     if(regional)then

!    Convert to rotated coordinate.  dlon centered on 180 (pi),
!    so always positive for limited area
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if(diagnostic_reg) then
           call txy2ll(dlon,dlat,rlon00,rlat00)
           ntest=ntest+1
           disterr=acosd(sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00)))
           disterrmax=max(disterrmax,disterr)
        end if

!    Check to see if in domain.  outside=.true. if dlon_earth,
!    dlat_earth outside domain, =.false. if inside
        if(outside) cycle read_loop

!    Gobal case 
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd(dlat,1,rlats,nlat,1)
        call grdcrd(dlon,1,rlons,nlon,1)
     endif

!    Check obs time
     idate5(1) = nint(allspot(2,ix)) ! year
     idate5(2) = nint(allspot(3,ix)) ! month
     idate5(3) = nint(allspot(4,ix)) ! day
     idate5(4) = nint(allspot(5,ix)) ! hour
     idate5(5) = nint(allspot(6,ix)) ! minute

     if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
         idate5(2) <    1 .or. idate5(2) >   12 .or. &
         idate5(3) <    1 .or. idate5(3) >   31 .or. &
         idate5(4) <    0 .or. idate5(4) >   24 .or. &
         idate5(5) <    0 .or. idate5(5) >   60 )then

         write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
             ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
         cycle read_loop

     endif

!    Retrieve obs time
     call w3fs21(idate5,nmind)
     sstime = float(nmind) + allspot(7,ix)/R60 ! add in seconds
     tdiff  = (sstime - gstime)/R60
     if (abs(tdiff)>twind) cycle read_loop
     
!    Check observational info
     sat_zenang  = allspot(10,ix) 
     ifov = nint( allspot(12,ix) )
     if( ifov <    izero .or. ifov > 100 .or. abs(sat_zenang) > 360._r_kind ) then

        write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
             ' STRANGE OBS INFO(FOV,SAZA):', allspot(12,ix), allspot(10,ix)
        cycle read_loop

     endif

! Check that number of airs channel equals n_airschan
! only done until they match for one record and ndata is updated

!    if(ndata == 0)then
!      call mpi_ufbint(lnbufr,scbtseqn,1,1,iscbtseqn,'(SCBTSEQN)')
!      iscbtseqn = nint(scbtseqn)
!      if(iscbtseqn /= n_airschan)then
!         write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' SEQUENCE:', &
!             iscbtseqn, ' CH DATA IS READ INSTEAD OF ',n_airschan
!         cycle read_loop
!      end if
!    end if

!    Read AIRSCHAN or AMSUCHAN or HSBCHAN

     call mpi_ufbrep(lnbufr,allchan,1,n_totchan,iret,'TMBR')

     if( iret /= n_totchan)then
        write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
             iret, ' CH DATA IS READ INSTEAD OF ',n_totchan
        cycle read_loop
     endif


!    check for missing channels (if key channel reject)
     iskip = 0
     do l=1+ioffset,nchanl+ioffset
        
        if( allchan(l) < tbmin .or. allchan(l) > tbmax )then
           iskip = iskip + 1
           if(airs) then
             if(l == ichsst) cycle read_loop
           else if(amsua)then
             ll=l-ioffset
             if (ll == 1 .or. ll ==2 .or. ll== 3 .or. ll == 4 .or. &
                 ll == 6 .or. ll == 15)cycle read_loop
           else
             ll=l-ioffset
             if(ll == 1 .or. ll == 2)cycle read_loop
           end if
        else
           nread=nread+1
        endif
     end do

     if( iskip >= nchanl )cycle read_loop

     timedif = 6.0_r_kind*abs(tdiff)        ! range:  0 to 18
     crit1 = 0.01_r_kind+timedif 
     call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse)
     if(.not. iuse)cycle read_loop

!   "Score" observation.  We use this information to identify "best" obs
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

     crit1=crit1 + rlndsea(isflg)
     call checkob(dist1,crit1,itx,iuse)
     if(.not. iuse)cycle read_loop
!    Set common predictor parameters

     sat_zenang  = sat_zenang  * deg2rad
     sat_aziang  = allspot(11,ix)  

!    Read AQUASPOT
     call mpi_ufbint(lnbufr,aquaspot,2,1,iret,'SOZA SOLAZI')
     sol_zenang = aquaspot(1)


     if(amsua)then

         if(ifov <= 15)sat_zenang = -sat_zenang
         ch1    = allchan(ichsst)-cbias(ifov,ichansst)+cbias(15,ichansst)
         ch2    = allchan(ichsst+1)-cbias(ifov,ichansst+1)+cbias(15,ichansst+1)
         ch3    = allchan(ichsst+2)-cbias(ifov,ichansst+2)+cbias(15,ichansst+2)
         ch15   = allchan(ichsst+14)-cbias(ifov,ichansst+14)+cbias(15,ichansst+14)
         if (isflg == 0 .and. ch1<285.0_r_kind .and. ch2<285.0_r_kind) then
            cosza = cos(sat_zenang)
            d0  =8.24_r_kind - 2.622_r_kind*cosza + 1.846_r_kind*cosza*cosza
            qval=cosza*(d0+d1*log(285.0_r_kind-ch1)+d2*log(285.0_r_kind-ch2))
            pred=max(zero,qval)*100.0_r_kind
         else
            tt=168.-0.49*ch15
            df2 = 5.10_r_kind +0.78*ch1-0.96*ch3
            pred=zero
            if(ch1-ch15 >= 3._r_kind)then
              if(ch1 > 261._r_kind .or. ch1 >= tt .or. &
                  (ch15 <= 273._r_kind .and. df2 >= 0.6_r_kind))then
                 pred=100._r_kind
              end if
            end if
         endif

     else

       if ( isflg == 0 ) then
! cloud checks over ocean
         chsst = 8.28206 - 0.97957 * allchan(126+ioffset) + 0.60529 * &  ! AIRS science team
            allchan(129+ioffset) + 1.74444 * allchan(165+ioffset) &      ! SST calculation for
            - .40379 * allchan(166+ioffset)                              ! AIRS data
! 917 cm-1 minus 2500 cm-1 cloud test valid at night for land/ocean:
! beyond threshold, negative >> cirrus (ice), positive >> stratus (water)
! 917 cm-1 minus 2664 cm-1 cloud test valid at night for land/ocean:
! beyond threshold, negative >> cirrus ( ice), positive >> stratus (water)
! 2500 cm-1 minus 2664 cm-1 cloud test valid at night for land/ocean:
! sensitivity test li, Jun et al. (2000) JAM
           ch8ch18 = abs(allchan(125+ioffset) - allchan(263+ioffset) - .10)
           ch8ch19 = abs(allchan(125+ioffset) - allchan(281+ioffset) + .39)
           ch18ch19 = abs(allchan(263+ioffset) - allchan(281+ioffset) + .49)
           if (sol_zenang > 89.0 .and. ch8ch18 < .75 .and. ch8ch19 < .55 .and. &
               ch18ch19 < .50 .and. (chsst-sstx) > -6.0) then
              chsst = sstx
           endif
       elseif ( isflg == 1 ) then
! cloud checks over land
           chsst = allchan(123+ioffset)
           ch8ch18 = abs(allchan(125+ioffset) - allchan(263+ioffset) - .39)
           ch8ch19 = abs(allchan(125+ioffset) - allchan(281+ioffset) + .13)
           ch18ch19 = abs(allchan(263+ioffset) - allchan(281+ioffset) + .52)
           if (sol_zenang > 89.0 .and. ch8ch18 < .75 .and. ch8ch19 < .70 .and. &
               ch18ch19 < .55 .and. (chsst-sstx) > -10.0) then
                chsst = sstx
             endif
        elseif ( isflg == 2 .or. isflg == 3 ) then

! cloud checks over snow and ice
! 801 cm-1 minus 1103 cm-1 test:
! less than -0.05 >> ice cloud; greater than 1.0 >> water cloud
! 965 cm-1 minus 1103 cm-1 test:
! greater than 1.0 >> water cloud
! these tests should not be solar zenigh angle dependent.
! Holz and Ackerman 2006 AMS Sat Conf.

           chsst = allchan(128+ioffset)
           ch8ch18 = allchan(119+ioffset) - allchan(157+ioffset)
           ch8ch19 = allchan(129+ioffset) - allchan(157+ioffset)
           if (ch8ch18 > -.05 .and. ch8ch18 < 1.0 .and. &
               ch8ch19 > -.05 .and. ch8ch19 < 1.0 .and. &
               chsst < 263.0) then
                 chsst = sstx
           endif
           if ( allchan(108+ioffset) > allchan(107+ioffset) .and. &
                allchan(115+ioffset) > allchan(114+ioffset) .and. &
                allchan(181+ioffset) > allchan(180+ioffset) .and. &
                allchan(194+ioffset) > allchan(195+ioffset)) then
                tmpinv = allchan(87+ioffset)
                l = 88+ioffset
                do k = 88+ioffset,125+ioffset
                  if ( allchan(k) > tmpinv ) then
                       tmpinv = allchan(k)
                       l = k
                  endif
                end do
                if ( tmpinv > allchan(125+ioffset) + 5.0) then
                       chsst = sstx
                 endif
             endif
       else
           chsst = allchan(ichsst)
       endif
       chsstf = sstx-chsst
       chsstf = max(zero,chsstf)
       pred = 15._r_kind*chsstf

       if(ifov <= 45)sat_zenang = -sat_zenang

     end if

     
!    Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
!    crit1 = pred + rlndsea(isflg) + timedif +10.0_r_kind*float(iskip)
     crit1 = crit1+pred 

!    Map obs to grids
     call finalcheck(dist1,crit1,ndata,itx,iout,iuse,sis)

     if(.not. iuse)cycle read_loop

     sol_aziang = aquaspot(2)
     lza = (start + float(ifov-1)*step)*deg2rad

     data_all(1,iout) = 49                  ! satellite ID (temp. 49)
     data_all(2,iout) = tdiff               ! time diff (obs-anal) (hrs)
     data_all(3,iout) = dlon                ! grid relative longitude
     data_all(4,iout) = dlat                ! grid relative latitude
     data_all(5,iout) = sat_zenang          ! satellite zenith angle (rad)
     data_all(6,iout) = sat_aziang          ! satellite azimuth angle (deg)
     data_all(7,iout) = lza                 ! look angle (rad)
     data_all(8,iout) = ifov                ! fov number
     data_all(9,iout) = sol_zenang          ! solar zenith angle (deg)
     data_all(10,iout)= sol_aziang          ! solar azimuth angle (deg)
     data_all(11,iout)= sfcpct(0)           ! ocean percentage
     data_all(12,iout)= sfcpct(1)           ! land percentage
     data_all(13,iout)= sfcpct(2)           ! ice percentage
     data_all(14,iout)= sfcpct(3)           ! snow percentage
     data_all(15,iout)= dlon_earth*rad2deg  ! earth relative longitude (degrees)
     data_all(16,iout)= dlat_earth*rad2deg  ! earth relative latitude (degrees)

     data_all(17,iout)= val_airs
     data_all(18,iout)= itt
     do l=1,nchanl
        data_all(l+nreal,iout) = allchan(l+ioffset)   ! brightness temerature
     end do

     idata_itx(iout) = itx                  ! thinning grid location
     data_crit(iout) = crit1*dist1          ! observation "score"

    enddo read_loop
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
        super_val(itt)=super_val(itt)+val_airs
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

  if(diagnostic_reg .and. ntest > 0 .and. mype_sub==mype_root) &
       write(6,*)'READ_AIRS:  mype,ntest,disterrmax=',&
       mype,ntest,disterrmax
  
  return
end subroutine read_airs
