  subroutine read_pcp(nread,ndata,nodata,mype,jsatid,infile,lunout,obstype, &
              twind,sis)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_pcp                           read pcp rate data
!   prgmmr: treadon          org: np23                date: 1998-05-15
!
! abstract:  This routine reads precipitation rate observations from
!            various platforms/retrievals.  Currently supported 
!            data sources include SSM/I, TMI, AMSU, and STAGE3
!            prepcipitation rates.  Please note that only the SSM/I
!            and TMI sections of the routine have been extensively
!            tested.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   1998-05-15  yang, weiyu
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-16  treadon - update documentation
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-09-17  todling - fix intent of jsatid
!   2004-10-28  treadon - replace parameter "tiny" with "tiny_r_kind"
!   2004-11-12  treadon - add code to read ssmi rain rates from prepbufr file
!   2005-01-27  treadon - change call to rdsfull
!   2005-04-22  treadon - correct ssmi read code to reflect mnemonic change from REQ6 to REQV
!   2005-08-16  guo - add gmao surface interface
!   2005-09-08  derber - modify to use input group time window
!   2005-09-28  derber - modify to produce consistent surface info
!   2005-10-06  treadon - allocate, load, and deallocate surface arrays needed by deter_sfc
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-11-29  parrish - modify getsfc to work for different regional options
!   2005-12-08  treadon - remove local land/sea/ice mask array since not used, remove
!                         gmao surface interface since not needed
!   2006-02-01  parrish - remove getsfc, destroy_sfc (different version called in read_obs)
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-05-25  treadon - replace obstype "pcp_ssm/i" with "pcp_ssmi"
!
!   input argument list:
!     mype     - mpi task id
!     jsatid   - satellite ide to read
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!
!   output argument list:
!     nread    - number of precipitation rate observations read
!     ndata    - number of precipitation rate profiles retained for further processing
!     nodata   - number of precipitation rate observations retained for further processing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use gridmod, only: nlat,nlon,regional,tll2xy,rlats,rlons
  use constants, only: izero,zero,deg2rad,quarter,tiny_r_kind,one,two,rad2deg
  use obsmod, only: iadate

  implicit none

! Declare passed variables
  character(10),intent(in):: obstype,infile,jsatid
  character(20),intent(in):: sis
  integer(i_kind),intent(in):: lunout,mype
  integer(i_kind),intent(inout):: nread
  integer(i_kind),intent(inout):: ndata,nodata
  real(r_kind),intent(in):: twind

! Declare local parameters
  real(r_kind),parameter:: r60=60.0_r_kind
  real(r_kind),parameter:: r100=100.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: r3600=3600.0_r_kind

! Declare local variables
  logical pcp_ssmi,pcp_tmi,pcp_amsu,pcp_stage3,outside

  character(6) ptype,tag
  character(8) subset
  character(40) strhdr7,strsmi4,strsmi2_old,strsmi2,strtmi7,stramb5
  character(10) date

  integer(i_kind) imn,k,i,iyr,lnbufr,maxobs,isflg
  integer(i_kind) ihh,idd,im,kx,jdate
  integer(i_kind) mincy,ndatout,nreal,nchanl,iy,iret,idate,itype,ihr,idy,imo
  integer(i_kind) minobs,isc,lndsea,ilat,ilon
  integer(i_kind) ithin
  integer(i_kind) idate5(5)

  real(r_kind) scli,sclw,dlon,dlat,scnt
  real(r_kind) sstx,dlat_earth,dlon_earth
  real(r_kind) scnv,stdv,spcp,time1,rmesh
  real(r_kind) bmiss
  real(r_kind),dimension(0:3)::sfcpct
  real(r_kind),allocatable,dimension(:,:)::  pcpdata_all
  real(r_kind) pcpdata(12)
  real(r_double) hdr7(7),pcpdat(7),pcpprd(2,2)

  data strhdr7  / 'RPID YEAR MNTH DAYS HOUR MINU SECO' /
  data strsmi4  / 'CLAT CLON NMCT ACAV' /
  data strsmi2_old  / 'FOST REQ6' /
  data strsmi2  / 'FOST REQV' /
  data strtmi7  / 'CLAT CLON TRRT CRRT RCWA PCIA ACAV' /
  data stramb5  / 'CLAT CLON REQV SNCV ICEP' /


  data lnbufr /10/
  data bmiss / 10e10 /
  data ithin / -9 /
  data rmesh / -99.999 /


!**************************************************************************
! Initialize variables
  maxobs=1e6
  nchanl = izero
  pcp_ssmi=  obstype == 'pcp_ssmi'
  pcp_tmi=   obstype == 'pcp_tmi'
  pcp_amsu=  obstype == 'pcp_amsu'
  pcp_stage3=obstype == 'pcp_stage3'
  if (pcp_ssmi) then
     nreal=10
     ptype='ssmi'
  endif
  if (pcp_tmi)  then
     nreal=12
     ptype='tmi'
  endif
  if (pcp_amsu) then
     nreal=10
     ptype='amsu'
  endif
  if (pcp_stage3) then
     nreal=10
     ptype='stage3'
  endif
  ndatout=nreal+nchanl


! Open and read the bufr data
  call closbf(lnbufr)
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)
  call readmg(lnbufr,subset,idate,iret)
  if (iret/=izero) goto 110

  iy=izero; im=izero; idd=izero; ihh=izero
  write(date,'( i10)') idate
  read (date,'(i4,3i2)') iy,im,idd,ihh
  write(6,*)'READ_PCP:      ',ptype,'_pcp bufr file date is ',iy,im,idd,ihh,infile
  if(iy/=iadate(1).or.im/=iadate(2).or.idd/=iadate(3).or.&
       ihh/=iadate(4)) then
     write(6,*)'***READ_PCP ERROR*** incompatable analysis and observation ',&
          'date/time'
     write(6,*)' year  anal/obs ',iadate(1),iy
     write(6,*)' month anal/obs ',iadate(2),im
     write(6,*)' day   anal/obs ',iadate(3),idd
     write(6,*)' hour  anal/obs ',iadate(4),ihh
     call stop2(93)
  end if
         
  idate5(1) = iy    ! year
  idate5(2) = im    ! month
  idate5(3) = idd   ! day
  idate5(4) = ihh   ! hour
  idate5(5) = izero ! minute
  call w3fs21(idate5,mincy)


! Write header record to pcp obs output file
  ilon=3
  ilat=4
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon

  allocate(pcpdata_all(12,maxobs))

! Big loop over bufr file	
10 call readsb(lnbufr,iret)
     if(iret/=izero) then
        call readmg(lnbufr,subset,jdate,iret)
        if(iret/=izero) go to 100
        go to 10
     end if

  
!    Extract satellite id and observation date/time
     call ufbint(lnbufr,hdr7,7,1,iret,strhdr7)

     iyr = hdr7(2)
     imo = hdr7(3)
     idy = hdr7(4)
     ihr = hdr7(5)
     imn = hdr7(6)
     isc = hdr7(7)
     
     idate5(1) = iyr
     idate5(2) = imo
     idate5(3) = idy
     idate5(4) = ihr
     idate5(5) = imn
     call w3fs21(idate5,minobs)
     time1   = (minobs - mincy)/r60

     if (abs(time1) > twind) then
        write(6,*)'READ_PCP:      time outside window ',time1
        goto 10
     endif

     if (pcp_ssmi)   kx = 264
     if (pcp_tmi)    kx = 211
     if (pcp_amsu)   kx = 258
     if (pcp_stage3) kx = 260

        
!    Extract observation location and value(s)
     if (pcp_ssmi) then

        call ufbint(lnbufr,pcpdat,4,1,iret,strsmi4)
        itype = nint(pcpdat(3))
        scnt  = pcpdat(4)
        if (itype.ne.66) goto 10

!       Transition across PREPBUFR mnemonic change from REQ6 to REQV

        call ufbrep(lnbufr,pcpprd,2,2,iret,strsmi2_old)
        if(min(pcpprd(2,1),pcpprd(2,2)).ge.bmiss) &
         call ufbrep(lnbufr,pcpprd,2,2,iret,strsmi2)
        spcp = bmiss
        if (nint(pcpprd(1,1))==4)  spcp=pcpprd(2,1)*3600.
        if (nint(pcpprd(1,2))==10) stdv=pcpprd(2,2)*3600.

!       Check for negative, very large, or missing pcp.
!       If any case is found, skip this observation.
        if ( (spcp<zero) .or. (spcp>r100) .or. &
             (abs(spcp-bmiss)<tiny_r_kind) ) goto 10

     elseif (pcp_tmi) then
        call ufbint(lnbufr,pcpdat,7,1,iret,strtmi7)
        spcp=bmiss; scnv=bmiss
        spcp = pcpdat(3)  ! total rain
        scnv = pcpdat(4)  ! convective rain
        sclw = pcpdat(5)  ! clw
        scli = pcpdat(6)  ! cli
        scnt = pcpdat(7)  ! number of obs used for superobs

!       Check for negative, very large, or missing pcp.
!       If any case is found, skip this observation.
        if ( (spcp<zero) .or. (spcp>r100) .or. &
             (abs(spcp-bmiss)<tiny_r_kind) ) goto 10

     elseif (pcp_amsu) then
        call ufbint(lnbufr,pcpdat,5,1,iret,stramb5)
        spcp   = pcpdat(3)*r3600   ! convert to mm/hr
        lndsea = nint(pcpdat(4))   ! water=0, land=1, coast=2
        itype  = nint(pcpdat(5))   ! water=0, land=1, coast=-1

        if (lndsea==2 .or. itype==-1) goto 10  ! skip coastal points

!       Check for negative, very large, or missing pcp.
!       If any case is found, skip this observation.
        if ( (spcp<zero) .or. (spcp>r100) .or. &
             (abs(spcp-bmiss)<tiny_r_kind) ) goto 10


     elseif (pcp_stage3) then
        spcp=bmiss

!       Check for negative, very large, or missing pcp.
!       If any case is found, skip this observation.
        if ( (spcp<zero) .or. (spcp>r100) .or. &
             (abs(spcp-bmiss)<tiny_r_kind) ) goto 10
        
     endif


!    If regional mode, see if observation falls within limited area domain
     dlat_earth = pcpdat(1)
     dlon_earth = pcpdat(2)
     if (dlon_earth< zero) dlon_earth=dlon_earth+r360
     if (dlon_earth>=r360) dlon_earth=dlon_earth-r360
     dlat_earth=dlat_earth*deg2rad
     dlon_earth=dlon_earth*deg2rad
     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if (outside) go to 10

!    Global case.  Convert observation (lat,lon) to radians
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd(dlat,1,rlats,nlat,1)
        call grdcrd(dlon,1,rlons,nlon,1)
     endif

!
!    Do we want to keep this observation?
     nread = nread + 1
     ndata = min(ndata + 1,maxobs)
     nodata = nodata + 1
!

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

!    Load output array
     pcpdata=zero

     pcpdata(1) = kx                    ! satellite id
     pcpdata(2) = time1                 ! time relative to cycle (hours)
     pcpdata(3) = dlon                  ! grid relative longitude
     pcpdata(4) = dlat                  ! grid relative latitude
     pcpdata(5) = isflg + .001          ! surface tag
     if (pcp_ssmi) then
        pcpdata(6) = spcp               ! mean precipitation
        pcpdata(7) = stdv               ! standard deviation of superobs
        pcpdata(8) = scnt               ! number of obs used to make superobs
        pcpdata(9) = dlon_earth*rad2deg ! earth relative longitude (degrees)
        pcpdata(10)= dlat_earth*rad2deg ! earth relative latitude (degrees)
     elseif (pcp_tmi) then
        pcpdata(6) = spcp               ! total precipitation (mm/hr)
        pcpdata(7) = scnv               ! convective precipitation (mm/hr)
        pcpdata(8) = sclw               ! cloud water (mm)
        pcpdata(9) = scli               ! cloud ice (mm)
        pcpdata(10)= scnt               ! number of obs used to make superobs
        pcpdata(11)= dlon_earth*rad2deg ! earth relative longitude (degrees)
        pcpdata(12)= dlat_earth*rad2deg ! earth relative latitude (degrees)
     elseif (pcp_amsu) then
        pcpdata(6) = spcp               ! mean precipitation
        pcpdata(7) = 0                  ! standard deviation of superobs (not yet)
        pcpdata(8) = itype              ! type of algorithm
        pcpdata(9) = dlon_earth*rad2deg ! earth relative longitude (degrees)
        pcpdata(10)= dlat_earth*rad2deg ! earth relative latitude (degrees)
     elseif (pcp_stage3) then
        pcpdata(6) = spcp               ! mean precipitation
        pcpdata(7) = stdv               ! standard deviation of superobs
        pcpdata(8) = scnt               ! number of obs used to make superobs
        pcpdata(9) = dlon_earth*rad2deg ! earth relative longitude (degrees)
        pcpdata(10)= dlat_earth*rad2deg ! earth relative latitude (degrees)
     endif


!    Write data to output file
     do k=1,ndatout
       pcpdata_all(k,ndata)=pcpdata(k)
     end do


!
! End of big loop over bufr file.  Process next observation.
  go to 10


! Jump here when the end of the bufr file is reach or there
! is some other problem reading the bufr file
100 continue


! Write retained data to local file
  write(lunout) ((pcpdata_all(k,i),k=1,ndatout),i=1,ndata)
  deallocate(pcpdata_all)


! Jump here if there is a problem opening the bufr file
110 continue
  call closbf(lnbufr)

! End of routine
  return
end subroutine read_pcp
