subroutine read_ozone(nread,ndata,nodata,jsatid,infile,gstime,lunout, &
           obstype,twind,sis)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_ozone                    read ozone (sbuv2) data
!   prgmmr: yang             org: np23                date: 1998-05-15
!
! abstract:  This routine reads SBUV/2 ozone observations.  Both layer
!            and total column values are read in.  The routine has
!            the ability to read both IEEE and BUFR format SBUV/2
!            ozone data files.
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
!   2004-09-17  todling - fixed intent of jsatid
!   2004-12-02  todling - compilation in OSF1 forces big_endian for bufr files;
!                         need to force little_endian for ieee files
!   2004-12-22  kokron  - change cpp tokens to add support for ifort compiler
!                         efc does not have a convert option so it should use
!                         the other 'open'
!   2005-03-14  treadon - define numeric constants to r_kind precision
!   2005-05-12  wu - add OMI total ozone 
!   2005-06-27  guo     - bug fix: hour read from header was incorrect
!   2005-09-08  derber - modify to use input group time window
!   2005-09-19  treadon - add check on NOAA-17 sbuv data (toss bad data)
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-12-23  treadon - bound longitude to be less than 360.0
!   2006-01-26  treadon - remove ieee sbuv option
!   2006-02-03  derber  - modify for new obs control and obs count
!
!   input argument list:
!     obstype  - observation type to process
!     jsatid   - satellite id to read
!     infile   - unit from which to read ozone data
!     gstime   - analysis time in minutes from reference date
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!
!   output argument list:
!     nread    - number of sbuv2/omi ozone observations read
!     ndata    - number of sbuv2/omi ozone profiles retained for further processing
!     nodata   - number of sbuv2/omi ozone observations retained for further processing
!
! remarks:
!   NCEP stopped producing IEEE format sbuv2 ozone files in April 2004.  
!   Hence, the IEEE portion of this routine no future application.  It 
!   is retained in the GSI package for use with retrospective runs.  The
!   IEEE portion of this routine may be removed from the GSI at a later date.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use gridmod, only: nlat,nlon,regional,tll2xy,rlats,rlons
  use constants, only: deg2rad,zero,rad2deg
  use obsmod, only: iadate,nloz
  implicit none

! Declare local parameters
  real(r_kind),parameter:: r60  = 60.0_r_kind
  real(r_kind),parameter:: r76  = 76.0_r_kind
  real(r_kind),parameter:: r100 = 100.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind

! Declare passed variables
  character(10),intent(in):: obstype,infile,jsatid
  character(20),intent(in):: sis
  integer(i_kind),intent(in):: lunout
  integer(i_kind),intent(inout):: nread
  integer(i_kind),intent(inout):: ndata,nodata
  real(r_kind),intent(in):: gstime,twind

! Declare local variables
  logical outside
  
  integer(i_kind) maxobs,nozdat
  character(8) subset
  character(10) date
  character(63) lozstr
  character(49) ozstr

  integer(i_kind) idate,jdate,ksatid,kk,iy,iret,im,ihh,idd,lunin
  integer(i_kind) nmind,idayyr8,idaywk8,i
  integer(i_kind) imin,isec
  integer(i_kind) jdn8,nxdata,n,jda8,jmo8,iyear8
  integer(i_kind) nmrecs,k,ilat,ilon,nreal,nchanl
  integer(i_kind) ithin,kidsat
  integer(i_kind) idate5(5)
  integer(i_kind) kdata(50,210)

  real(r_kind) tdiff,sstime,slons,slats,dlon,dlat,rmesh
  real(r_kind) slons0,slats0,rsat,toto3,solzen,solzenp,dlat_earth,dlon_earth
  real(r_kind),dimension(nloz+1):: poz
! maximum number of observations set to 
  real(r_kind),allocatable,dimension(:,:):: ozout
  real(r_double),dimension(nloz):: ozone_8
  real(r_double),dimension(10):: hdroz
  real(r_double) totoz

  data lozstr &
       / 'OSP12 OSP11 OSP10 OSP9 OSP8 OSP7 OSP6 OSP5 OSP4 OSP3 OSP2 OSP1 ' /
  data ozstr  /'SAID CLAT CLON YEAR MNTH DAYS HOUR MINU OSZA OPSZ' /
  data lunin / 10 /
  data ithin / -9 /
  data rmesh / -99.999 /


!**************************************************************************
! Set constants.  Initialize variables
  rsat=999.
  maxobs=1e6
  nreal=6
  ilon=3
  ilat=4


! Setup for ozone or ozone_omi
  if (obstype == 'sbuv2' ) then
     nchanl=nloz+1
  else if ( obstype == 'omi') then
     nchanl=1
  endif
  nozdat=nreal+nchanl

! allocate data array 
  allocate (ozout(nozdat,maxobs))

! Separately process sbuv or omi ozone
  
  if (obstype == 'sbuv2' ) then

     open(lunin,file=infile,form='unformatted')
     nmrecs=0
     call openbf(lunin,'IN',lunin)
     call datelen(10)
     call readmg(lunin,subset,idate,iret)
     iy=0
     im=0
     idd=0
     ihh=0
     if(iret/=0) goto 160
     write(date,'( i10)') idate
     read (date,'(i4,3i2)') iy,im,idd,ihh
     write(6,*)'READ_OZONE:    ozone bufr file date is ',iy,im,idd,ihh,infile
     if(iy/=iadate(1).or.im/=iadate(2).or.idd/=iadate(3).or.&
          ihh/=iadate(4)) then
        write(6,*)'***READ_OZONE ERROR*** incompatable analysis and observation ',&
              'date/time'
        write(6,*)' year  anal/obs ',iadate(1),iy
        write(6,*)' month anal/obs ',iadate(2),im
        write(6,*)' day   anal/obs ',iadate(3),idd
        write(6,*)' hour  anal/obs ',iadate(4),ihh
        call stop2(95)
     end if
     
110  continue
     call readsb(lunin,iret)
     if (iret/=0) then
        call readmg(lunin,subset,jdate,iret)
        if (iret/=0) goto 150
        goto 110
     endif
     
!    extract header information
!    BUFR code values for satellite identifiers are listed in
!    Dennis Keyser's website,
!    http://www.emc.ncep.noaa.gov/mmb/papers/keyser/Satellite_Historical.txt

     call ufbint(lunin,hdroz,10,1,iret,ozstr)
     rsat = hdroz(1)-191.0; ksatid=rsat
     if(jsatid == 'n14')kidsat = 14
     if(jsatid == 'n15')kidsat = 15
     if(jsatid == 'n16')kidsat = 16
     if(jsatid == 'n17')kidsat = 17
     if(jsatid == 'n18')kidsat = 18

     if (ksatid /= kidsat) go to 110

     nmrecs=nmrecs+nloz+1
    
!    Convert observation location to radians
     slats0= hdroz(2)
     slons0= hdroz(3)
     if(slons0< zero) slons0=slons0+r360
     if(slons0>=r360) slons0=slons0-r360
     dlat_earth = slats0 * deg2rad
     dlon_earth = slons0 * deg2rad

     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if(outside) go to 110
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd(dlat,1,rlats,nlat,1)
        call grdcrd(dlon,1,rlons,nlon,1)
     endif
     
!    Special check for NOAA-17
!    NOAA-17 SBUV/2 has a stray light problem which produces
!    erroneous ozone profile retrievals for a limited portion
!    of its measurements. The contaminated signals only occur
!    in the Southern Hemisphere and only for Solar Zenith
!    Angles (SZA) greater than 76 Degrees.

     solzen = hdroz(9)
     solzenp= hdroz(10)
     if (ksatid==17 .and. dlat_earth<zero .and. solzenp > r76) goto 110

!    Convert observation time to relative time
     idate5(1) = hdroz(4)  !year
     idate5(2) = hdroz(5)  !month
     idate5(3) = hdroz(6)  !day
     idate5(4) = hdroz(7)  !hour
     idate5(5) = hdroz(8)  !minute
     call w3fs21(idate5,nmind)
     sstime=float(nmind)
     tdiff=(sstime-gstime)/r60
     if(abs(tdiff) > twind)then
        write(6,*)'READ_OZONE:  obs time idate5=',idate5,', tdiff=',&
             tdiff,' is outside time window=',twind
        goto 110
     end if
     
!    Extract layer ozone values
     call ufbint(lunin,ozone_8,nloz,1,iret,lozstr)
     do k=1,nloz
        kk=nloz-k+1
        poz(k) = ozone_8(kk)
     end do
   
!    Extract total ozone
     call ufbint(lunin,totoz,1,1,iret,'OTSP')            
     poz(nloz+1) = totoz

!    If 1st layer value is missing, toss entire observation
     
     if (poz(1)<10000) then

!       Write ozone record to output file
        ndata=min(ndata+1,maxobs)
        nodata=nodata+nloz+1
        ozout(1,ndata)=rsat
        ozout(2,ndata)=tdiff
        ozout(3,ndata)=dlon               ! grid relative longitude
        ozout(4,ndata)=dlat               ! grid relative latitude
        ozout(5,ndata)=dlon_earth*rad2deg ! earth relative longitude (degrees)
        ozout(6,ndata)=dlat_earth*rad2deg ! earth relative latitude (degrees)
        do k=1,nloz+1
           ozout(k+6,ndata)=poz(k)
        end do
        
     endif
  
     goto 110

!    End of bufr ozone block

! Process OMI data
  else if ( obstype == 'omi') then
     nmrecs=0
     open(lunin,file=infile,form='formatted')
 100 format(6i6,3f10.2)
     do n=1,400000
        read(lunin,100,end=150,err=160) iy,im,idd,ihh,imin,isec,slats,slons,toto3
!    convert observation location to radians
        slons0=slons
        slats0=slats
        if(slons0< zero) slons0=slons0+r360
        if(slons0>=r360) slons0=slons0-r360
        dlat_earth = slats0 * deg2rad
        dlon_earth = slons0 * deg2rad

        if(regional)then
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
           if(outside) cycle    
        else
           dlat = dlat_earth
           dlon = dlon_earth
           call grdcrd(dlat,1,rlats,nlat,1)
           call grdcrd(dlon,1,rlons,nlon,1)
        endif
        nmrecs=nmrecs+1
        idate5(1) = iy !year
        idate5(2) = im !month
        idate5(3) = idd !day
        idate5(4) = ihh !hour
        idate5(5) = imin !minute
        call w3fs21(idate5,nmind)
        sstime=float(nmind)
        tdiff=(sstime-gstime)/r60
        if(abs(tdiff) > twind)then
           write(6,*)'READ_OZONE: omi obs time idate5=',idate5,', tdiff=',&
                tdiff,' is outside time window=',twind
           cycle
        end if

        ndata=ndata+1
        nodata=nodata+1
        ozout(1,ndata)=rsat
        ozout(2,ndata)=tdiff
        ozout(3,ndata)=dlon               ! grid relative longitude
        ozout(4,ndata)=dlat               ! grid relative latitude
        ozout(5,ndata)=dlon_earth*rad2deg ! earth relative longitude (degrees)
        ozout(6,ndata)=dlat_earth*rad2deg ! earth relative latitude (degrees)
        ozout(7,ndata)=toto3
        
!    End of loop over observations
     enddo

! End of OMI block
  endif

! Jump here when eof detected
150 continue
! Write header record and data to output file for further processing
  write(6,*)'READ_OZONE:  write to lunout=',lunout,', obstype,sis,nreal,nchanl,ilat,ilon= ',&
      obstype,' ',sis,' ',nreal,nchanl,ilat,ilon
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((ozout(k,i),k=1,nozdat),i=1,ndata)
  nread=nmrecs


! Close unit to input data file
160 continue
  deallocate(ozout)
  if (obstype /= 'omi') call closbf(lunin)
  close(lunin)

  return
  
end subroutine read_ozone
