subroutine read_airs(mype,val_airs,ithin,isfcalc,rmesh,jsatid,gstime,&
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
!   2006-05-19  eliu   - add logic to reset relative weight when all channels not used
!   2006-07-28  derber - modify reads so ufbseq not necessary
!                      - add solar and satellite azimuth angles remove isflg from output
!   2006-08-25  treadon - replace serial bufr i/o with parallel bufr i/o (mpi_io)
!   2006-12-15  todling - trim table filename (also made shorter word!)
!   2007-01-17  liu     - fix in channel numbering in weight reset logics
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2008-04-21  safford - rm unused vars and uses
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2009-01-09  gayno   - new option to calculate surface fields within FOV
!   2009-04-18  woollen - improve mpi_io interface with bufrlib routines
!   2009-04-21  derber  - add ithin to call to makegrids
!
!   input argument list:
!     mype     - mpi task id
!     val_airs - weighting factor applied to super obs
!     ithin    - flag to thin data
!     isfcalc  - specify method to calculate surface fields within FOV
!                when set to one, integrate surface info across FOV.
!                when not one, use bilinear interpolation.
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
  use kinds, only: r_kind,r_single,r_double,i_kind
  use satthin, only: super_val,itxmax,makegrids,map2tgrid,destroygrids, &
               finalcheck,checkob,score_crit
  use radinfo, only: cbias,newchn,iuse_rad,nusis,jpch_rad,ang_rad 
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,&
       tll2xy,txy2ll,rlats,rlons
  use constants, only: izero,ione,zero,deg2rad,one,three,five,rad2deg,r60inv
  use gsi_4dvar, only: l4dvar, iwinbgn, winlen
  use calc_fov_crosstrk, only : instrument_init, fov_cleanup, fov_check

  implicit none


! Number of channels for sensors in BUFR
  integer(i_kind),parameter :: n_airschan = 281_i_kind                           !--- 281 subset ch out of 2378 ch for AIRS
  integer(i_kind),parameter :: n_amsuchan =  15_i_kind
  integer(i_kind),parameter :: n_hsbchan  =   4_i_kind
  integer(i_kind),parameter :: n_totchan  = n_amsuchan+n_airschan+n_hsbchan+ione
  integer(i_kind),parameter :: maxinfo    =  33_i_kind


! BUFR format for AQUASPOT 
! Input variables
  integer(i_kind)  ,intent(in   ) :: mype
  real(r_kind)     ,intent(in   ) :: twind
  integer(i_kind)  ,intent(in   ) :: ithin
  integer(i_kind)  ,intent(in   ) :: isfcalc
  character(len=*) ,intent(in   ) :: jsatid
  character(len=*) ,intent(in   ) :: infile
  character(len=*) ,intent(in   ) :: obstype
  real(r_kind)     ,intent(in   ) :: gstime
  integer(i_kind)  ,intent(in   ) :: lunout
  real(r_kind)     ,intent(in   ) :: rmesh
  character(len=*) ,intent(in   ) :: sis
  integer(i_kind)  ,intent(in   ) :: mype_root
  integer(i_kind)  ,intent(in   ) :: mype_sub
  integer(i_kind)  ,intent(in   ) :: npe_sub
  integer(i_kind)  ,intent(in   ) :: mpi_comm_sub  
  

! Output variables
  integer(i_kind)  ,intent(inout) :: nread
  integer(i_kind)  ,intent(  out) :: ndata,nodata
  
! Input/Output variables
  real(r_kind)     ,intent(inout) :: val_airs

! BUFR file sequencial number
  character(len=512)  :: table_file
  integer(i_kind)     :: lnbufr = 10_i_kind
  integer(i_kind)     :: lnbufrtab = 11_i_kind
  integer(i_kind)     :: irec,isub,next

! Variables for BUFR IO    
  real(r_double),dimension(2) :: aquaspot
  real(r_double),dimension(12,3) :: allspot
  real(r_double),dimension(n_totchan) :: allchan
  
  real(r_kind)      :: step, start
  character(len=8)  :: subset
  character(len=4)  :: senname
  character(len=80) :: allspotlist
  integer(i_kind)   :: nchanl,nchanlr
  integer(i_kind)   :: iret, ireadmg,ireadsb


! Work variables for time
  integer(i_kind)   :: idate
  integer(i_kind)   :: idate5(5)
  real(r_kind)      :: sstime, tdiff, t4dv
  integer(i_kind)   :: nmind


! Other work variables
  integer(i_kind)  :: nreal, ichsst, ichansst, isflg,ioffset
  integer(i_kind)  :: itx, k, nele, itt, n,iscbtseqn,ix
  real(r_kind)     :: chsstf,chsst,sfcr
  real(r_kind)     :: ch15, ch3, df2, tt
  real(r_kind)     :: dlon, dlat
  real(r_kind)     :: dlon_earth,dlat_earth, lza
  real(r_kind)     :: timedif, pred, crit1, qval, ch1, ch2, d0, cosza, dist1
  real(r_kind)     :: sat_zenang, sol_zenang, sat_aziang, sol_aziang
  real(r_kind)     :: ch8ch18, ch8ch19, ch18ch19, tmpinv
  real(r_kind)     :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real(r_kind),dimension(0:4) :: rlndsea
  real(r_kind),dimension(0:3) :: sfcpct
  real(r_kind),dimension(0:3) :: ts

  integer(i_kind)  :: ifov, ioff, ilat, ilon, instr, ichan
  logical          :: outside,iuse,assim,lluse,valid
  integer(i_kind)  :: i, l, ll, iskip
  real(r_kind),allocatable,dimension(:,:):: data_all
  real(r_kind) :: dlat_earth_deg, dlon_earth_deg
  integer(i_kind):: idomsfc



! Set standard parameters
  character(8),parameter:: fov_flag="crosstrk"
  real(r_kind),parameter:: expansion=2.9_r_kind
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
  ntest=izero
  nreal  = maxinfo
  ndata = izero
  nodata = izero
  airs=      obstype == 'airs'
  amsua=     obstype == 'amsua'
  hsb=       obstype == 'hsb'
  r01=0.01_r_kind

  ilon=3_i_kind
  ilat=4_i_kind

  if(isfcalc==ione) ichan = 999_i_kind  ! for deter_sfc_fov code. not used yet.

  if(airs)then
     ix=ione
     step   = 1.1_r_kind
     start = -48.9_r_kind
     senname = 'AIRS'
     nchanl  = n_airschan
     nchanlr = n_airschan
     ioff=newchn(sis,1)-ione
     ioffset=izero
     ichansst   = newchn(sis,914)
     ichsst     = ichansst-ioff+ioffset
     rlndsea(0) = zero                       
     rlndsea(1) = 10._r_kind
     rlndsea(2) = 15._r_kind
     rlndsea(3) = 10._r_kind
     rlndsea(4) = 30._r_kind
     if(isfcalc==ione) instr=12_i_kind ! according to tom kleespies, airs is the same as amsu-b.
     if (mype_sub==mype_root) &
          write(6,*)'READ_AIRS:  airs offset ',ioff,ichansst,ichsst
  else if(amsua)then
     ix=2_i_kind
     step   = three + one/three
     start = -48._r_kind - one/three
!    start  = -48.33_r_kind
     senname = 'AMSU'
     nchanl  = n_amsuchan
     nchanlr = n_amsuchan
     ioff=newchn(sis,1)-ione
     ioffset=n_airschan
     ichansst   = newchn(sis,1)
     ichsst     = ioffset +ione            !channel 1
     rlndsea(0) = zero                       
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 20._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
     if(isfcalc==ione) instr=11_i_kind
  else if(hsb)then
     ix=3_i_kind
     step   = 1.1_r_kind
     start  = -48.95_r_kind
     senname = 'HSB'
     nchanl  = n_hsbchan
     nchanlr = n_hsbchan+ione
     ioff=newchn(sis,1)-ione
     ioffset=iscbtseqn+n_amsuchan
     ichansst   = newchn(sis,4)
     ichsst     = ichansst-ioff+ioffset
     rlndsea(0) = zero                       
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 20._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
     if(isfcalc==ione) instr=12_i_kind ! similar to amsu-b according to tom kleespies
  endif

  if (isfcalc == ione) then
     call instrument_init(instr,jsatid,expansion)
  endif

  allspotlist='SIID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA BEARAZ FOVN'

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
  if (.not.assim) val_airs=zero

! Make thinning grids
  call makegrids(rmesh,ithin)

! Open BUFR file
  open(lnbufr,file=infile,form='unformatted')

! Open BUFR table
  table_file = 'airs_bufr.table'      ! make table file name
  inquire(file=table_file,exist=airstab)
  if (airstab) then
     if (mype_sub==mype_root) &
          write(6,*)'READ_AIRS:  Reading BUFR Table A file: ',trim(table_file)
     open(lnbufrtab,file=trim(table_file))
     call openbf(lnbufr,'IN',lnbufrtab)
  else
     call openbf(lnbufr,'IN',lnbufr)
  endif
  call datelen(10)

! Allocate arrays to hold data
  nele=nreal+nchanl
  allocate(data_all(nele,itxmax))

! Big loop to read data file
  next=mype_sub+ione
  do while(ireadmg(lnbufr,subset,idate)>=izero)
     call ufbcnt(lnbufr,irec,isub)
     if(irec/=next)cycle;next=next+npe_sub
     read_loop: do while (ireadsb(lnbufr)==izero)

!       Read AIRSSPOT , AMSUSPOT and HSBSPOT

        call ufbrep(lnbufr,allspot,12_i_kind,3_i_kind,iret,allspotlist)

        if(iret /= 3_i_kind) cycle read_loop

        dlat_earth = allspot(8,ix)
        dlon_earth = allspot(9,ix)
!       Check observing position
        if( abs(dlat_earth) > R90  .or. abs(dlon_earth) > R360 .or. &
           (abs(dlat_earth) == R90 .and. dlon_earth /= ZERO) )then
!          write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
!               ' STRANGE OBS POINT (LAT,LON):', dlat_earth, dlon_earth
           cycle read_loop
        endif

!       Retrieve observing position
        if(dlon_earth >= R360)then
           dlon_earth = dlon_earth - R360
        else if(dlon_earth < ZERO)then
           dlon_earth = dlon_earth + R360
        endif

        dlat_earth_deg = dlat_earth
        dlon_earth_deg = dlon_earth

        dlat_earth = dlat_earth * deg2rad
        dlon_earth = dlon_earth * deg2rad

        sat_aziang=allspot(11,ix)
        if (abs(sat_aziang) > r360) then
           write(6,*)  'READ_AIRS: bad azimuth angle ',sat_aziang
           cycle read_loop
        endif

!       If regional, map obs lat,lon to rotated grid.
        if(regional)then

!       Convert to rotated coordinate.  dlon centered on 180 (pi),
!       so always positive for limited area
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
           if(diagnostic_reg) then
              call txy2ll(dlon,dlat,rlon00,rlat00)
              ntest=ntest+ione
              disterr=acos(sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                   (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00)))*rad2deg
              disterrmax=max(disterrmax,disterr)
           end if

!       Check to see if in domain.  outside=.true. if dlon_earth,
!       dlat_earth outside domain, =.false. if inside
           if(outside) cycle read_loop

!       Gobal case 
        else
           dlat = dlat_earth
           dlon = dlon_earth
           call grdcrd(dlat,ione,rlats,nlat,ione)
           call grdcrd(dlon,ione,rlons,nlon,ione)
        endif

!       Check obs time
        idate5(1) = nint(allspot(2,ix)) ! year
        idate5(2) = nint(allspot(3,ix)) ! month
        idate5(3) = nint(allspot(4,ix)) ! day
        idate5(4) = nint(allspot(5,ix)) ! hour
        idate5(5) = nint(allspot(6,ix)) ! minute

        if( idate5(1) < 1900_i_kind .or. idate5(1) > 3000_i_kind .or. &
            idate5(2) < ione        .or. idate5(2) >   12_i_kind .or. &
            idate5(3) < ione        .or. idate5(3) >   31_i_kind .or. &
            idate5(4) <izero        .or. idate5(4) >   24_i_kind .or. &
            idate5(5) <izero        .or. idate5(5) >   60_i_kind )then

            write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
            cycle read_loop

        endif

!       Retrieve obs time
        call w3fs21(idate5,nmind)
        t4dv = (real((nmind-iwinbgn),r_kind) + real(allspot(7,ix),r_kind)*r60inv)*r60inv ! add in seconds
        if (l4dvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
        else
           sstime = real(nmind,r_kind) + real(allspot(7,ix),r_kind)*r60inv ! add in seconds
           tdiff = (sstime - gstime)*r60inv
           if (abs(tdiff)>twind) cycle read_loop
        endif

        nread = nread + nchanl

        if (l4dvar) then
           crit1 = 0.01_r_kind
        else
           timedif = 6.0_r_kind*abs(tdiff)        ! range:  0 to 18
           crit1 = 0.01_r_kind+timedif 
        endif
        call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
        if(.not. iuse)cycle read_loop

!       Check observational info
        sat_zenang  = allspot(10,ix) 
        ifov = nint( allspot(12,ix) )
        if( ifov <    izero .or. ifov > 100_i_kind .or. abs(sat_zenang) > 360._r_kind ) then

           write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                ' STRANGE OBS INFO(FOV,SAZA):', allspot(12,ix), allspot(10,ix)
           cycle read_loop

        endif

!      "Score" observation.  We use this information to identify "best" obs
!       Locate the observation on the analysis grid.  Get sst and land/sea/ice
!       mask.  
!        isflg    - surface flag
!                   0 sea
!                   1 land
!                   2 sea ice
!                   3 snow
!                   4 mixed 
        if (isfcalc == ione) then
           call fov_check(ifov,instr,valid)
           if (.not. valid) cycle read_loop
        endif

        if (isfcalc == ione) then
           call deter_sfc_fov(fov_flag,ifov,instr,ichan,sat_aziang,dlat_earth_deg, &
                             dlon_earth_deg,expansion,t4dv,isflg,idomsfc, &
                             sfcpct,vfr,sty,vty,stp,sm,ff10,sfcr,zz,sn,ts,tsavg)
        else
           call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg, &
                 idomsfc,sfcpct,ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
        endif

        crit1=crit1 + rlndsea(isflg)
        call checkob(dist1,crit1,itx,iuse)
        if(.not. iuse)cycle read_loop

!       Check that number of airs channel equals n_airschan
!       only done until they match for one record and ndata is updated

!       if(ndata == izero)then
!          call ufbint(lnbufr,scbtseqn,ione,ione,iscbtseqn,'(SCBTSEQN)')
!          iscbtseqn = nint(scbtseqn)
!          if(iscbtseqn /= n_airschan)then
!             write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' SEQUENCE:', &
!                 iscbtseqn, ' CH DATA IS READ INSTEAD OF ',n_airschan
!             cycle read_loop
!          end if
!        end if

!        Read AIRSCHAN or AMSUCHAN or HSBCHAN

         call ufbrep(lnbufr,allchan,ione,n_totchan,iret,'TMBR')

         if( iret /= n_totchan)then
            write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                 iret, ' CH DATA IS READ INSTEAD OF ',n_totchan
            cycle read_loop
         endif


!        check for missing channels (if key channel reject)
         iskip = izero
         do l=ione+ioffset,nchanl+ioffset
            ll=(l-ioffset)+ioff
            lluse = iuse_rad(ll) >= izero
            if( lluse .and. (allchan(l)<tbmin .or. allchan(l)>tbmax) ) then
               iskip = iskip + ione
               if(airs) then
                  if(l == ichsst) cycle read_loop
               else if(amsua)then
                  ll=l-ioffset
                  if (ll == ione     .or. ll == 2_i_kind .or. ll == 3_i_kind .or. ll == 4_i_kind .or. &
                      ll == 6_i_kind .or. ll == 15_i_kind)cycle read_loop
               else
                  ll=l-ioffset
                  if(ll == ione .or. ll == 2_i_kind)cycle read_loop
               end if
            endif
         end do

         if( iskip >= nchanl )cycle read_loop


!        Set common predictor parameters

         sat_zenang  = sat_zenang  * deg2rad
         sat_aziang  = allspot(11,ix)  

!        Read AQUASPOT
         call ufbint(lnbufr,aquaspot,2_i_kind,ione,iret,'SOZA SOLAZI')
         sol_zenang = aquaspot(1)


         if(amsua)then

             if(ifov <= 15_i_kind)sat_zenang = -sat_zenang
             ch1    = allchan(ichsst     )-ang_rad(ichansst               )* &
                                      (cbias(ifov,ichansst          )-cbias(15,ichansst          ))
             ch2    = allchan(ichsst+ione)-ang_rad(ichansst+ione          )* &
                                      (cbias(ifov,ichansst+ione     )-cbias(15,ichansst+ione     ))
             ch3    = allchan(ichsst+2_i_kind)-ang_rad(ichansst+2_i_kind  )* &
                                      (cbias(ifov,ichansst+2_i_kind )-cbias(15,ichansst+2_i_kind ))
             ch15   = allchan(ichsst+14_i_kind)-ang_rad(ichansst+14_i_kind)* &
                                      (cbias(ifov,ichansst+14_i_kind)-cbias(15,ichansst+14_i_kind))
             if (isflg == izero .and. ch1<285.0_r_kind .and. ch2<285.0_r_kind) then
                cosza = cos(sat_zenang)
                d0  =8.24_r_kind - 2.622_r_kind*cosza + 1.846_r_kind*cosza*cosza
                qval=cosza*(d0+d1*log(285.0_r_kind-ch1)+d2*log(285.0_r_kind-ch2))
                pred=max(zero,qval)*100.0_r_kind
             else
                tt=168._r_kind-0.49_r_kind*ch15
                df2 = 5.10_r_kind +0.78_r_kind*ch1-0.96_r_kind*ch3
                pred=zero
                if(ch1-ch15 >= three)then
                   if(ch1 > 261._r_kind .or. ch1 >= tt .or. &
                      (ch15 <= 273._r_kind .and. df2 >= 0.6_r_kind))then
                      pred=100._r_kind
                   end if
                end if
             endif

         else

             if ( isflg == izero ) then
! cloud checks over ocean
                chsst = 8.28206_r_single - 0.97957_r_single * allchan(126_i_kind+ioffset) + 0.60529_r_single * &  ! AIRS science team
                   allchan(129_i_kind+ioffset) + 1.74444_r_single * allchan(165_i_kind+ioffset) &                 ! SST calculation for
                   - .40379_r_single * allchan(166_i_kind+ioffset)                                                ! AIRS data
! 917 cm-1 minus 2500 cm-1 cloud test valid at night for land/ocean:
! beyond threshold, negative >> cirrus (ice), positive >> stratus (water)
! 917 cm-1 minus 2664 cm-1 cloud test valid at night for land/ocean:
! beyond threshold, negative >> cirrus ( ice), positive >> stratus (water)
! 2500 cm-1 minus 2664 cm-1 cloud test valid at night for land/ocean:
! sensitivity test li, Jun et al. (2000) JAM
                ch8ch18 = abs(allchan(125_i_kind+ioffset) - allchan(263_i_kind+ioffset) - .10_r_single)
                ch8ch19 = abs(allchan(125_i_kind+ioffset) - allchan(281_i_kind+ioffset) + .39_r_single)
                ch18ch19 = abs(allchan(263_i_kind+ioffset) - allchan(281_i_kind+ioffset) + .49_r_single)
                if (sol_zenang > 89.0_r_kind .and. ch8ch18 < .75_r_kind .and. ch8ch19 < .55_r_kind .and. &
                    ch18ch19 < .50_r_kind .and. (chsst-tsavg) > -6.0_r_kind) then
                    chsst = tsavg
                endif
             elseif ( isflg == ione ) then
! cloud checks over land
                chsst = allchan(123_i_kind+ioffset)
                ch8ch18 = abs(allchan(125_i_kind+ioffset) - allchan(263_i_kind+ioffset) - .39_r_single)
                ch8ch19 = abs(allchan(125_i_kind+ioffset) - allchan(281_i_kind+ioffset) + .13_r_single)
                ch18ch19 = abs(allchan(263_i_kind+ioffset) - allchan(281_i_kind+ioffset) + .52_r_single)
                if (sol_zenang > 89.0_r_kind .and. ch8ch18 < .75_r_kind .and. ch8ch19 < .70_r_kind .and. &
                    ch18ch19 < .55_r_kind .and. (chsst-tsavg) > -10.0_r_kind) then
                    chsst = tsavg
                endif
             elseif ( isflg == 2_i_kind .or. isflg == 3_i_kind ) then

! cloud checks over snow and ice
! 801 cm-1 minus 1103 cm-1 test:
! less than -0.05 >> ice cloud; greater than 1.0 >> water cloud
! 965 cm-1 minus 1103 cm-1 test:
! greater than 1.0 >> water cloud
! these tests should not be solar zenigh angle dependent.
! Holz and Ackerman 2006 AMS Sat Conf.

                chsst = allchan(128_i_kind+ioffset)
                ch8ch18 = allchan(119_i_kind+ioffset) - allchan(157_i_kind+ioffset)
                ch8ch19 = allchan(129_i_kind+ioffset) - allchan(157_i_kind+ioffset)
                if (ch8ch18 > -.05_r_single .and. ch8ch18 < one .and. &
                    ch8ch19 > -.05_r_single .and. ch8ch19 < one .and. &
                    chsst < 263.0_r_kind) then
                    chsst = tsavg
                endif
                if ( allchan(108_i_kind+ioffset) > allchan(107_i_kind+ioffset) .and. &
                     allchan(115_i_kind+ioffset) > allchan(114_i_kind+ioffset) .and. &
                     allchan(181_i_kind+ioffset) > allchan(180_i_kind+ioffset) .and. &
                     allchan(194_i_kind+ioffset) > allchan(195_i_kind+ioffset)) then
                   tmpinv = allchan(87_i_kind+ioffset)
                   l = 88_i_kind+ioffset
                   do k = 88_i_kind+ioffset,125_i_kind+ioffset
                      if ( allchan(k) > tmpinv ) then
                           tmpinv = allchan(k)
                           l = k
                      endif
                   end do
                   if ( tmpinv > allchan(125_i_kind+ioffset) + five) then
                      chsst = tsavg
                   endif
                endif
             else
                chsst = allchan(ichsst)
             endif
             chsstf = tsavg-chsst
             chsstf = max(zero,chsstf)
             pred = 15._r_kind*chsstf

             if(ifov <= 45_i_kind)sat_zenang = -sat_zenang

         end if

     
!        Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
!        crit1 = pred + rlndsea(isflg) + timedif +10.0_r_kind*float(iskip)
         crit1 = crit1+pred 

!        Map obs to grids
         call finalcheck(dist1,crit1,itx,iuse)

         if(.not. iuse)cycle read_loop

         sol_aziang = aquaspot(2)
         lza = (start + float(ifov-ione)*step)*deg2rad

         data_all(1,itx) = 49_i_kind              ! satellite ID (temp. 49)
         data_all(2,itx) = t4dv                   ! time diff (obs-anal) (hrs)
         data_all(3,itx) = dlon                   ! grid relative longitude
         data_all(4,itx) = dlat                   ! grid relative latitude
         data_all(5,itx) = sat_zenang             ! satellite zenith angle (rad)
         data_all(6,itx) = sat_aziang             ! satellite azimuth angle (deg)
         data_all(7,itx) = lza                    ! look angle (rad)
         data_all(8,itx) = ifov                   ! fov number
         data_all(9,itx) = sol_zenang             ! solar zenith angle (deg)
         data_all(10,itx)= sol_aziang             ! solar azimuth angle (deg)
         data_all(11,itx) = sfcpct(0)             ! sea percentage of
         data_all(12,itx) = sfcpct(1)             ! land percentage
         data_all(13,itx) = sfcpct(2)             ! sea ice percentage
         data_all(14,itx) = sfcpct(3)             ! snow percentage
         data_all(15,itx)= ts(0)                  ! ocean skin temperature
         data_all(16,itx)= ts(1)                  ! land skin temperature
         data_all(17,itx)= ts(2)                  ! ice skin temperature
         data_all(18,itx)= ts(3)                  ! snow skin temperature
         data_all(19,itx)= tsavg                  ! average skin temperature
         data_all(20,itx)= vty                    ! vegetation type
         data_all(21,itx)= vfr                    ! vegetation fraction
         data_all(22,itx)= sty                    ! soil type
         data_all(23,itx)= stp                    ! soil temperature
         data_all(24,itx)= sm                     ! soil moisture
         data_all(25,itx)= sn                     ! snow depth
         data_all(26,itx)= zz                     ! surface height
         data_all(27,itx)= idomsfc + 0.001_r_kind ! dominate surface type
         data_all(28,itx)= sfcr                   ! surface roughness
         data_all(29,itx)= ff10                   ! ten meter wind factor
         data_all(30,itx)= dlon_earth_deg         ! earth relative longitude (degrees)
         data_all(31,itx)= dlat_earth_deg         ! earth relative latitude (degrees)

         data_all(32,itx)= val_airs
         data_all(33,itx)= itt
         do l=1,nchanl
            data_all(l+nreal,itx) = allchan(l+ioffset)   ! brightness temerature
         end do


     enddo read_loop
  enddo

! If multiple tasks read input bufr file, allow each tasks to write out
! information it retained and then let single task merge files together

  call combine_radobs(mype_sub,mype_root,npe_sub,mpi_comm_sub,&
       nele,itxmax,nread,ndata,data_all,score_crit)


! Allow single task to check for bad obs, update superobs sum,
! and write out data to scratch file for further processing.
  if (mype_sub==mype_root.and.ndata>izero) then

!    Identify "bad" observation (unreasonable brightness temperatures).
!    Update superobs sum according to observation location

     do n=1,ndata
        do i=1,nchanl
           if(data_all(i+nreal,n) > tbmin .and. &
                data_all(i+nreal,n) < tbmax)nodata=nodata+ione
        end do
        itt=nint(data_all(nreal,n))
        super_val(itt)=super_val(itt)+val_airs
     end do

!    Write final set of "best" observations to output file
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)
  
  endif

1000 continue

  deallocate(data_all) ! Deallocate data arrays
  call destroygrids    ! Deallocate satthin arrays
  call closbf(lnbufr)  ! Close bufr file

  if (isfcalc == ione) then
    call fov_cleanup
  endif

  if(diagnostic_reg .and. ntest > izero .and. mype_sub==mype_root) &
       write(6,*)'READ_AIRS:  mype,ntest,disterrmax=',&
       mype,ntest,disterrmax

  return
end subroutine read_airs
