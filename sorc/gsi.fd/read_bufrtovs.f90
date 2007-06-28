subroutine read_bufrtovs(mype,val_tovs,ithin,&
     rmesh,jsatid,gstime,infile,lunout,obstype,&
     nread,ndata,nodata,twind,sis)
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
!   2006-07-28  derber  - add solar and satellite azimuth angles remove isflg from output
!   2007-01-18  derber - add kidsat for metop
!   2007-04-12  treadon - metop satellite id updated to be consistent with bufr file change
!
!   input argument list:
!     mype     - mpi task id
!     val_tovs - weighting factor applied to super obs
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     jsatid   - satellite to read
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window(hours)
!     sis      - sensor/instrument/satellite indicator
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
           finalcheck,map2tgrid
  use radinfo, only: iuse_rad,newchn,cbias,predx
  use gridmod, only: diagnostic_reg,regional,nlat,nlon,tll2xy,txy2ll,rlats,rlons
  use constants, only: deg2rad,zero,one,three,izero,ione,rad2deg
  use obsmod, only: iadate
  implicit none

! Declare passed variables
  character(10),intent(in):: infile,obstype,jsatid
  character(20),intent(in):: sis
  integer(i_kind),intent(in):: mype,lunout,ithin
  integer(i_kind),intent(inout):: nread
  integer(i_kind),intent(out):: ndata,nodata
  real(r_kind),intent(in):: val_tovs,rmesh,gstime,twind

! Declare local parameters
  integer(i_kind),parameter:: n1bhdr=15
  integer(i_kind),parameter:: maxinfo=18
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: tbmin=50.0_r_kind
  real(r_kind),parameter:: tbmax=550.0_r_kind

! Declare local variables
  logical hirs,msu,amsua,amsub,mhs,hirs4,hirs2,ssu
  logical outside,iuse

  character(10) date
  character(14):: infile2
  character(8) subset,subfgn
  character(80) hdr1b

  integer(i_kind) ihh,i,j,k,ifov,idd,jdate,isc,ireadmg,ireadsb,ntest
  integer(i_kind) iret,idate,im,iy,iyr,nchanl,n
  integer(i_kind) ich1,ich2,ich8,ich15,kidsat
  integer(i_kind) nmind,itx,nele,nk,itt,iout
  integer(i_kind) chan1,iskip,ichan2,ichan1,ichan15
  integer(i_kind) lnbufr,ksatid,ichan8,isflg,ichan3,ich3,ich4,ich6
  integer(i_kind) ilat,ilon,lll,nread1,ndata1
  integer(i_kind),dimension(5):: idate5

  real(r_kind) cosza
  real(r_kind) ch1,ch2,ch3,ch8,ch10,ch8ch10,d0,d1,d2,sval,ch15,qval
  real(r_kind) ch1flg,df1
  real(r_kind),dimension(0:3):: sfcpct

  real(r_kind) pred
  real(r_kind) rsat,dlat,panglr,dlon,rato,sstime,tdiff
  real(r_kind) dlon_earth,dlat_earth,r01
  real(r_kind) crit1,step,start,ch8flg,sstx,dist1
  real(r_kind) terrain,timedif,lza,df2,tt
  real(r_kind),dimension(0:4):: rlndsea
  real(r_kind),allocatable,dimension(:,:):: data_all

  real(r_double),allocatable,dimension(:):: data1b8
  real(r_double),dimension(n1bhdr):: bfr1bhdr

  real(r_kind) disterr,disterrmax,dlon00,dlat00

!**************************************************************************
! Initialize variables

  lnbufr = 15
  disterrmax=zero
  ntest=0
  ndata  = 0
  nodata  = 0
  nread  = 0

! Make thinning grids
  call makegrids(rmesh)


! Set various variables depending on type of data to be read

  hirs2 =    obstype == 'hirs2'
  hirs4 =    obstype == 'hirs4'
  hirs =     hirs2 .or. obstype == 'hirs3' .or. hirs4
  msu=       obstype == 'msu'
  amsua=     obstype == 'amsua'
  amsub=     obstype == 'amsub'
  mhs  =     obstype == 'mhs'
  ssu =      obstype == 'ssu'

!  instrument specific variables
  d1 =  0.754_r_kind
  d2 = -2.265_r_kind 
  r01 = 0.01_r_kind
  ich1   = 1   !1
  ich2   = 2   !2
  ich3   = 3   !3
  ich4   = 4   !4
  ich6   = 6   !6
  ich8   = 8   !8
  ich15  = 15  !15
! Set array index for surface-sensing channels
  ichan1  = newchn(sis,ich1)
  ichan2  = newchn(sis,ich2)
  ichan3  = newchn(sis,ich3)
  if (hirs) then
     ichan8  = newchn(sis,ich8)
  endif
  if (amsua) ichan15 = newchn(sis,ich15)
  if(jsatid == 'n14')kidsat=14+191
  if(jsatid == 'n15')kidsat=15+191
  if(jsatid == 'n16')kidsat=16+191
  if(jsatid == 'n17')kidsat=17+191
  if(jsatid == 'n18')kidsat=18+191
  if(jsatid == 'metop-a')kidsat=4
  if(jsatid == 'metop-b')kidsat=5
  if(jsatid == 'metop-c')kidsat=6


  if ( hirs ) then
     step   = 1.80_r_kind
     start  = -49.5_r_kind
     nchanl=19
     rato=1.1363987_r_kind
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = 0._r_kind
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 30._r_kind
  else if ( msu ) then
     step   = 9.474_r_kind
     start  = -47.37_r_kind
     nchanl=4
     rato=1.1363987_r_kind
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = 0._r_kind
     rlndsea(1) = 20._r_kind
     rlndsea(2) = 15._r_kind
     rlndsea(3) = 20._r_kind
     rlndsea(4) = 100._r_kind
  else if ( amsua ) then
     step   = three + one/three
     start = -48. - one/three
!    start  = -48.33_r_kind
     nchanl=15
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = 0._r_kind
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
  else if ( amsub )  then
     step   = 1.1_r_kind
     start  = -48.95_r_kind
     nchanl=5
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = 0._r_kind
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 20._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
  else if ( mhs )  then
     step   = 10.0_r_kind/9.0_r_kind
     start  = -445.0_r_kind/9.0_r_kind
     nchanl=5
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = 0._r_kind
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 20._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 100._r_kind
  else if ( ssu ) then
     step  =  10.00_r_kind
     start = -35.00_r_kind
     nchanl=3
     rato=1.1363987_r_kind
!   Set rlndsea for types we would prefer selecting
     rlndsea(0) = 0._r_kind
     rlndsea(1) = 15._r_kind
     rlndsea(2) = 10._r_kind
     rlndsea(3) = 15._r_kind
     rlndsea(4) = 30._r_kind
  end if


! Write header record to scratch file.  Also allocate array
! to hold all data for given satellite
  nele=maxinfo+nchanl
  ilon=3
  ilat=4
  write(lunout) obstype,sis,maxinfo,nchanl,ilat,ilon
  allocate(data_all(nele,itxmax),data1b8(nchanl))


! Big loop over standard data feed and possible ears data
  ndata1=0
  do lll = 1,2                                
     nread1 = 0

!    Set bufr subset names based on type of data to read
     if(lll == 1)then
        if ( hirs ) then
           subfgn='NC021025'
           if (jsatid=='n14') subfgn='NC021021'
           if(hirs4) subfgn='NC021028'
        else if ( msu ) then
           subfgn='NC021022'
        else if ( amsua ) then
           subfgn='NC021023'
        else if ( amsub )  then
           subfgn='NC021024'
        else if ( mhs )  then
           subfgn='NC021027'
        else if ( ssu ) then
           subfgn='NC021020'
        end if

!    EARS data feed
     else
        if ( hirs ) then
           subfgn='NC021035'
           if (jsatid=='n14') subfgn='NC021031'
           if(hirs4) subfgn='NC021038'
        else if ( msu ) then
           subfgn='NC021032'
        else if ( amsua ) then
           subfgn='NC021033'
        else if ( amsub )  then
           subfgn='NC021034'
        else if ( mhs )  then
           subfgn='NC021037'
        end if
     end if

!    Open unit to satellite bufr file
     infile2=infile
     if(lll == 2)infile2=trim(infile)//'ears'
     open(lnbufr,file=infile2,form='unformatted',status = 'old',err = 500)
     call openbf(lnbufr,'IN',lnbufr)
     call datelen(10)
     call readmg(lnbufr,subset,idate,iret)
     if( subset /= subfgn) then
        write(6,*) 'READ_BUFRTOVS:  *** WARNING: ',&
             'THE FILE TITLE DOES NOT MATCH DATA SUBSET'
        write(6,*) '  infile=', lnbufr, infile2,' subset=',&
             subset, ' subfgn=',subfgn,' ',obstype,' ',jsatid
        write(6,*) 'SKIP PROCESSING OF THIS 1B FILE'
        go to 900
     end if

!    Extract date and check for consistency with analysis date     
     iy=0
     im=0
     idd=0
     ihh=0
     write(date,'( i10)') idate
     read(date,'(i4,3i2)') iy,im,idd,ihh
     write(6,*) 'READ_BUFRTOVS: bufr file date is ',iy,im,idd,ihh,infile2
     if(im/=iadate(2).or.idd/=iadate(3)) then
        write(6,*)'***READ_BUFRTOVS ERROR*** ',&
             'incompatable analysis and observation date/time'
        write(6,*)' year  anal/obs ',iadate(1),iy
        write(6,*)' month anal/obs ',iadate(2),im
        write(6,*)' day   anal/obs ',iadate(3),idd
        write(6,*)' hour  anal/obs ',iadate(4),ihh
        go to 900
     end if
     
!    Loop to read bufr file
     do while (IREADMG(lnbufr,subset,idate)==0)
        read_loop: do while (IREADSB(lnbufr)==0 .and. subset==subfgn)

!          Read header record.  (lll=1 is normal feed, 2=EARS data)
           if(lll == 1)then
              hdr1b ='SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON SAZA SOZA BEARAZ SOLAZI HOLS '
           else
              hdr1b ='SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA SOZA BEARAZ SOLAZI '
           end if
           call ufbint(lnbufr,bfr1bhdr,n1bhdr,1,iret,hdr1b)

!          Extract satellite id.  If not the one we want, read next record
           rsat=bfr1bhdr(1) 
           ksatid=nint(bfr1bhdr(1))
           if(ksatid /= kidsat) cycle read_loop

!          Extract observation location and other required information
           dlat_earth = bfr1bhdr(9)
           dlon_earth = bfr1bhdr(10)
           if(dlon_earth<zero)  dlon_earth = dlon_earth+r360
           if(dlon_earth>=r360) dlon_earth = dlon_earth-r360
           dlat_earth = dlat_earth*deg2rad
           dlon_earth = dlon_earth*deg2rad
           
!          Regional case
           if(regional)then
              call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
              if(diagnostic_reg) then
                 call txy2ll(dlon,dlat,dlon00,dlat00)
                 ntest=ntest+1
                 disterr=acosd(sin(dlat_earth)*sin(dlat00)+cos(dlat_earth)*cos(dlat00)* &
                      (sin(dlon_earth)*sin(dlon00)+cos(dlon_earth)*cos(dlon00)))
                 disterrmax=max(disterrmax,disterr)
              end if
              
!             Check to see if in domain
              if(outside) cycle read_loop

!          Global case
           else
              dlat=dlat_earth
              dlon=dlon_earth
              call grdcrd(dlat,1,rlats,nlat,1)
              call grdcrd(dlon,1,rlons,nlon,1)
           endif

!          Extract date information.  If time outside window, skip this obs
           idate5(1) = bfr1bhdr(3) !year
           idate5(2) = bfr1bhdr(4) !month
           idate5(3) = bfr1bhdr(5) !day
           idate5(4) = bfr1bhdr(6) !hour
           idate5(5) = bfr1bhdr(7) !minute
           isc       = bfr1bhdr(8) !second
           call w3fs21(idate5,nmind)
           sstime=float(nmind) + isc/60.0_r_kind
           tdiff=(sstime-gstime)/60.0_r_kind
           if(abs(tdiff) > twind) cycle read_loop

!          If msu, drop obs from first (1) and last (11) scan positions
           ifov = nint(bfr1bhdr(2))
           if (msu .and. (ifov==1 .or. ifov==11)) cycle read_loop

!          Read data record.  Increment data counter
           if(lll == 1)then
              call ufbrep(lnbufr,data1b8,1,nchanl,iret,'TMBR')
           else
              call ufbrep(lnbufr,data1b8,1,nchanl,iret,'TMBRST')
           end if

!          Transfer observed brightness temperature to work array.  If any
!          temperature exceeds limits, reset observation to "bad" value
           iskip=0
           do j=1,nchanl
              if (data1b8(j) < tbmin .or. data1b8(j) > tbmax) then
                 iskip = iskip + 1

!                Remove profiles where key channels are bad  
                 if(( msu  .and.  j == ich1) .or.                                 &
                      (amsua .and. (j == ich1 .or. j == ich2 .or. j == ich3 .or.    &
                      j == ich4 .or. j==ich6 .or. j == ich15 )) .or. &
                      (hirs  .and. (j == ich8 )) .or.                               &
                      (amsub .and.  j == ich1) .or.                                 &
                      (mhs   .and.  (j == ich1 .or. j == ich2))) iskip = iskip+nchanl
              else
                 nread1=nread1+1
              endif
           end do
           if (iskip >= nchanl) cycle read_loop
           timedif = 2.0_r_kind*abs(tdiff)        ! range:  0 to 6
           terrain = 50._r_kind
           if(lll == 1)terrain = 0.01_r_kind*abs(bfr1bhdr(15))                   
           crit1 = 0.01_r_kind+terrain + (lll-1)*500.0_r_kind + timedif + 10._r_kind*float(iskip)
!          Map obs to thinning grid
           call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse)
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
!          sfcpct(0:3)- percentage of 4 surface types
!                 (0) - sea percentage
!                 (1) - land percentage
!                 (2) - sea ice percentage
!                 (3) - snow percentage

           call deter_sfc(dlat,dlon,isflg,sfcpct,sstx)

           crit1 = crit1 + rlndsea(isflg) 
           call checkob(dist1,crit1,itx,iuse)
           if(.not. iuse)cycle read_loop

!          Set common predictor parameters
           panglr=(start+float(ifov-1)*step)*deg2rad
           if( msu .or. hirs2 .or. ssu)then
              lza = asin(rato*sin(panglr))
           else
              lza = bfr1bhdr(11)*deg2rad      ! local zenith angle
              if((amsua .and. ifov <= 15) .or.        &
                   (amsub .and. ifov <= 45) .or.        &
                   (mhs   .and. ifov <= 45) .or.        &
                   (hirs  .and. ifov <= 28)) lza=-lza
           end if

!          Set data quality predictor
           if (msu) then
              ch1    = data1b8(ich1)-cbias(ifov,ichan1)-r01*predx(ichan1,1)
              ch1flg = sstx-ch1
              if(isflg == 0)then
                 pred = 100.-min(ch1flg,100.0_r_kind)
              else
                 pred = abs(ch1flg)
              end if
           else if (hirs) then
              ch8    = data1b8(ich8) -cbias(ifov,ichan8)-r01*predx(ichan8,1)
              ch8flg = sstx-ch8
              pred   = 10.0_r_kind*max(zero,ch8flg)
           else if (amsua) then
!   Remove angle dependent pattern (not mean)
              ch1 = data1b8(ich1)-cbias(ifov,ichan1)+cbias(15,ichan1)
              ch2 = data1b8(ich2)-cbias(ifov,ichan2)+cbias(15,ichan2)   
              if (isflg == 0 .and. ch1<285.0_r_kind .and. ch2<285.0_r_kind) then
                 cosza = cos(lza)
                 d0    = 8.24_r_kind - 2.622_r_kind*cosza + 1.846_r_kind*cosza*cosza
                 qval  = cosza*(d0+d1*log(285.0_r_kind-ch1)+d2*log(285.0_r_kind-ch2))
                 pred  = max(zero,qval)*100.0_r_kind
              else
                 ch3  = data1b8(ich3)-cbias(ifov,ichan3)+cbias(15,ichan3)   
                 ch15 = data1b8(ich15)-cbias(ifov,ichan15)+cbias(15,ichan15)
                 pred = abs(ch1-ch15)
                 if(ch1-ch15 >= 3._r_kind)then
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
              cosza = cos(lza)
              ch1 = data1b8(ich1)-cbias(ifov,ichan1)-r01*predx(ichan1,1)
              ch2 = data1b8(ich2)-cbias(ifov,ichan2)-r01*predx(ichan2,1)
              if(isflg == 0)then
!                pred = (ch1-ch2)/cosza+30.0_r_kind
                 if(ch2 < 300.)then 
                    pred = (0.13_r_kind*(ch1+33.58_r_kind*log(300._r_kind-ch2)- &
                         341.17_r_kind))*5.0_r_kind
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
           call finalcheck(dist1,crit1,ndata,itx,iout,iuse,sis)
           if(.not. iuse)cycle read_loop

!          Load selected observation into data array
              
           data_all(1 ,iout)= rsat                      ! satellite ID
           data_all(2 ,iout)= tdiff                     ! time
           data_all(3 ,iout)= dlon                      ! grid relative longitude
           data_all(4 ,iout)= dlat                      ! grid relative latitude
           data_all(5 ,iout)= lza                       ! local zenith angle
           data_all(6 ,iout)= bfr1bhdr(13)              ! local azimuth angle
           data_all(7 ,iout)= panglr                    ! look angle
           data_all(8 ,iout)= ifov                      ! scan position
           data_all(9 ,iout)= bfr1bhdr(12)              ! solar zenith angle
           data_all(10,iout)= bfr1bhdr(14)              ! solar azimuth angle
           data_all(11,iout)= sfcpct(0)                 ! ocean percentage
           data_all(12,iout)= sfcpct(1)                 ! land percentage
           data_all(13,iout)= sfcpct(2)                 ! ice percentage
           data_all(14,iout)= sfcpct(3)                 ! snow percentage
           data_all(15,iout)= dlon_earth*rad2deg        ! earth relative longitude (degrees)
           data_all(16,iout)= dlat_earth*rad2deg        ! earth relative latitude (degrees)
           
           data_all(17,iout)= val_tovs
           data_all(18,iout)= itt
              
           do i=1,nchanl
              data_all(i+maxinfo,iout)=data1b8(i)
           end do


!       End of bufr read loops
        end do read_loop
     end do

!   Jump here when there is a problem opening the bufr file
500  continue

!    Write data counts to stdout
     nread=nread+nread1
     ndata1=ndata-ndata1

! End of loop over regular and EARS data feeds
  end do

! 
  do n=1,ndata
     do i=1,nchanl
         if(data_all(i+maxinfo,n) > tbmin .and. &
            data_all(i+maxinfo,n) < tbmax)nodata=nodata+1
     end do
     itt=nint(data_all(maxinfo,n))
     super_val(itt)=super_val(itt)+val_tovs
  end do

! Write retained data to local file
  write(lunout) ((data_all(k,n),k=1,nele),n=1,ndata)

! Deallocate local arrays
  deallocate(data_all,data1b8)

! Deallocate satthin arrays
900 continue
  call closbf(lnbufr)
  close(lnbufr)
  call destroygrids


  if(diagnostic_reg.and.ntest.gt.0) write(6,*)'READ_BUFRTOVS:  ',&
       'mype,ntest,disterrmax=',mype,ntest,disterrmax

! End of routine
  return
end subroutine read_bufrtovs
subroutine deter_sfc(dlat,dlon,isflg,sfcpct,sstx)
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
!     dlat   - grid relative latitude
!     dlon   - grid relative longitude
!
!   output argument list:
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
!      sstx - sea surface temperature
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
     use kinds, only: r_kind,i_kind
     use satthin, only: sno_full,isli_full,sst_full
     use constants, only: zero,one
     use gridmod, only: rlats,rlons,nlat,nlon
     implicit none
     real(r_kind),parameter:: minsnow=0.1_r_kind
     integer(i_kind),intent(out):: isflg
     real(r_kind),intent(in) :: dlat,dlon
     real(r_kind),intent(out) :: sstx
     real(r_kind),dimension(0:3),intent(out) :: sfcpct
     integer(i_kind) jsli00,jsli01,jsli10,jsli11
     integer(i_kind):: iavgsli
     integer(i_kind):: klat1,klon1,klatp1,klonp1
     real(r_kind):: dx,dy,dx1,dy1,w00,w10,w01,w11
     logical :: sea,land,ice,snow

     klon1=int(dlon); klat1=int(dlat)
     dx  =dlon-klon1; dy  =dlat-klat1
     dx1 =one-dx;    dy1 =one-dy
     w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy
     
     klat1=min(max(1,klat1),nlat); klon1=min(max(0,klon1),nlon)
     if(klon1==0) klon1=nlon
     klatp1=min(nlat,klat1+1); klonp1=klon1+1
     if(klonp1==nlon+1) klonp1=1
      
!    Interpolate sst to obs location
     sstx=w00*sst_full(klat1,klon1 ) + w10*sst_full(klatp1,klon1 ) + &
          w01*sst_full(klat1,klonp1) + w11*sst_full(klatp1,klonp1)

!    Set surface type flag.  Begin by assuming obs over ice-free water

     jsli00 = isli_full(klat1 ,klon1 )
     jsli10 = isli_full(klatp1,klon1 )
     jsli01 = isli_full(klat1 ,klonp1)
     jsli11 = isli_full(klatp1,klonp1)

     if(sno_full(klat1 ,klon1 ) > minsnow .and. jsli00 >= 1) jsli00 = 3
     if(sno_full(klatp1,klon1 ) > minsnow .and. jsli10 >= 1) jsli10 = 3
     if(sno_full(klat1 ,klonp1) > minsnow .and. jsli01 >= 1) jsli01 = 3
     if(sno_full(klatp1,klonp1) > minsnow .and. jsli11 >= 1) jsli11 = 3

     sfcpct = zero
     sfcpct(jsli00)=sfcpct(jsli00)+w00
     sfcpct(jsli01)=sfcpct(jsli01)+w01
     sfcpct(jsli10)=sfcpct(jsli10)+w10
     sfcpct(jsli11)=sfcpct(jsli11)+w11

     isflg = 0
     if(sfcpct(0) > 0.99_r_kind)then
       isflg = 0
     else if(sfcpct(1) > 0.99_r_kind)then
       isflg = 1
     else if(sfcpct(2) > 0.99_r_kind)then
       isflg = 2
     else if(sfcpct(3) > 0.99_r_kind)then
       isflg = 3
     else 
       isflg = 4
     end if
    return
end subroutine deter_sfc 
