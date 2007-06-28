subroutine read_lidar(nread,ndata,nodata,infile,obstype,lunout,twind,sis)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_lidar                   read doppler lidar winds
!   prgmmr: yang             org: np20                date: 1998-05-15
!
! abstract:  This routine reads doppler lidar wind files.  
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
!   2005-08-02  derber - modify to use convinfo file
!   2005-09-08  derber - modify to use input group time window
!   2005-10-11  treadon - change convinfo read to free format
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-10-26  treadon - add routine tag to convinfo printout
!   2006-02-03  derber  - add new obs control
!   2006-02-08  derber  - modify to use new convinfo module
!   2006-02-24  derber  - modify to take advantage of convinfo module
!
!   input argument list:
!     infile   - unit from which to read BUFR data
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     twind    - input group time window (hours)
!
!   output argument list:
!     nread    - number of doppler lidar wind observations read
!     ndata    - number of doppler lidar wind profiles retained for further processing
!     nodata   - number of doppler lidar wind observations retained for further processing
!     sis      - satellite/instrument/sensor indicator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use gridmod, only: nlat,nlon,regional,tll2xy,rlats,rlons
  use convinfo, only: nconvtype,ctwind,cgross,cermax,cermin,cvar_b,cvar_pg, &
        ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype
  use constants, only: deg2rad,rad2deg,zero
  use obsmod, only: iadate
  implicit none

! Declare passed variables
  character(10),intent(in):: obstype,infile
  character(20),intent(in):: sis
  integer(i_kind),intent(in):: lunout
  integer(i_kind),intent(inout):: nread,ndata,nodata
  real(r_kind),intent(in):: twind

! Declare local parameters
  integer(i_kind),parameter:: maxobs=2e6
  integer(i_kind),parameter:: maxdat=16
  real(r_kind),parameter:: r360=360.0_r_kind

! Declare local variables
  logical dwl,outside

  character(40) hdstr
  character(44) dwstr
  character(10) date
  character(8) subset

  integer(i_kind) lunin,i,kx,ithin,ilat,ikx
  integer(i_kind) jdate,ihh,idd,idate,iret,im,iy,k,levs
  integer(i_kind) nbadlat,nmrecs,lat,ilon,nreal,nchanl


  real(r_kind) time,rmesh,usage,dlat,dlon,dlat_earth,dlon_earth
  real(r_kind) rlon0,rlat0,loswind
  real(r_kind),allocatable,dimension(:,:):: cdata_all

  real(r_double) rstation_id
  real(r_double),dimension(5):: hdr
  real(r_double),dimension(8):: dwld

  data hdstr  /'SID XOB YOB DHR TYP'/
  data dwstr  /'ADWL ELEV BORA NOLS NOLC ADPL LOSC SDLE'/
  
  data lunin / 10 /
  data ithin / -9 /
  data rmesh / -99.999 /


!**************************************************************************
! Initialize variables
  nbadlat=0
  nmrecs=0
  nreal=maxdat
  nchanl=0
  ilon=2
  ilat=3

  allocate(cdata_all(maxdat,maxobs))


! Open, then read date from bufr data
  open(lunin,file=infile,form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)
  call readmg(lunin,subset,idate,iret)
  if(iret/=0) goto 1010


! If date in lidar file does not agree with analysis date, 
! print message and stop program execution.
  write(date,'( i10)') idate
  read (date,'(i4,3i2)') iy,im,idd,ihh
  write(6,*)'READ_LIDAR: bufr file date is ',iy,im,idd,ihh
  if(iy/=iadate(1).or.im/=iadate(2).or.idd/=iadate(3).or.&
       ihh/=iadate(4)) then
     write(6,*)'***READ_LIDAR ERROR*** incompatable analysis ',&
          'and observation date/time'
     write(6,*)' year  anal/obs ',iadate(1),iy
     write(6,*)' month anal/obs ',iadate(2),im
     write(6,*)' day   anal/obs ',iadate(3),idd
     write(6,*)' hour  anal/obs ',iadate(4),ihh
     call stop2(91)
  end if


! Big loop over bufr file	

10 call readsb(lunin,iret) 
     if(iret/=0) then
        call readmg(lunin,subset,jdate,iret)
        if(iret/=0) go to 1000
        go to 10
     end if
     nmrecs=nmrecs+1

!    Extract type, date, and location information
     call ufbint(lunin,hdr,5,1,iret,hdstr)

     kx=hdr(5)
     
     ikx=0
     do i=1,nconvtype
       if(trim(obstype) == trim(ioctype(i)) .and. kx == ictype(i))ikx = i
     end do
!    Determine if this is doppler wind lidar report
     dwl= (kx /= 0) .and. (subset=='DWLDAT')
     if(.not. dwl) go to 10
     nread=nread+1

     time=hdr(4)
     if (abs(time) > ctwind(ikx) .or. abs(time) > twind) go to 10

     rstation_id=hdr(1)

     if (abs(hdr(3))<89.9) then      !msq
        if (hdr(2) >= r360) hdr(2)=hdr(2)-r360
        if (hdr(2) < zero)  hdr(2)=hdr(2)+r360

        dlat_earth = hdr(3) * deg2rad
        dlon_earth = hdr(2) * deg2rad

        if(regional)then
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
           if (outside) go to 10
        else
           dlat = dlat_earth
           dlon = dlon_earth
           call grdcrd(dlat,1,rlats,nlat,1)
           call grdcrd(dlon,1,rlons,nlon,1)
        endif



!    If wind data, extract observation.
        nodata=min(nodata+1,maxobs)
        ndata=min(ndata+1,maxobs)
        usage = 0.
        if(icuse(ikx) < 0)usage=100.
        if(ncnumgrp(ikx) > 0 )then                     ! cross validation on
          if(mod(ndata,ncnumgrp(ikx))== ncgroup(ikx)-1)usage=ncmiter(ikx)
        end if

        call ufbint(lunin,dwld,8,1,levs,dwstr)

        loswind=dwld(7)/(cos(dwld(2)*deg2rad)*cos(dlat_earth))   ! obs wind (line of sight component)
        cdata_all(1,ndata)=ikx                    ! obs type
        cdata_all(2,ndata)=dlon                   ! grid relative longitude
        cdata_all(3,ndata)=dlat                   ! grid relative latitude
        cdata_all(4,ndata)=time                   ! obs time (analyis relative hour)
        cdata_all(5,ndata)=dwld(1)                ! obs height (altitude) (m)
        cdata_all(6,ndata)=dwld(2)*deg2rad        ! elevation angle (radians)
        cdata_all(7,ndata)=dwld(3)*deg2rad        ! bearing or azimuth (radians)
        cdata_all(8,ndata)=dwld(4)                ! number of laser shots
        cdata_all(9,ndata)=dwld(5)                ! number of cloud laser shots
        cdata_all(10,ndata)=dwld(6)               ! atmospheric depth
        cdata_all(11,ndata)=loswind               ! obs wind (line of sight component) msq
        cdata_all(12,ndata)=dwld(8)/cos(dlat_earth)! standard deviation (obs error) msq
        cdata_all(13,ndata)=rstation_id           ! station id
        cdata_all(14,ndata)=usage                 ! usage parameter
        cdata_all(15,ndata)=dlon_earth*rad2deg    ! earth relative longitude (degrees)
        cdata_all(16,ndata)=dlat_earth*rad2deg    ! earth relative latitude (degrees)

     else
        nbadlat=nbadlat+1
     endif

! End of bufr read loop
  go to 10


! Normal exit
1000 continue


! Write observations to scratch file
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,maxdat),i=1,ndata)

  write(6,*)'READ_LIDAR:  number of bad latitudes = ',nbadlat

! Close unit to bufr file
1010 continue
  deallocate(cdata_all)
  call closbf(lunin)

! End of routine
  return
end subroutine read_lidar
