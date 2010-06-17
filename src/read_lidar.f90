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
!   2006-07-27  msq/terry - removed cosine factor for line of sight winds and obs err
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2008-04-18  safford - rm unused vars
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
  use convinfo, only: nconvtype,ctwind, &
        ncmiter,ncgroup,ncnumgrp,icuse,ictype,ioctype
  use constants, only: izero,ione,deg2rad,rad2deg,zero,r60inv
  use obsmod, only: iadate,offtime_data
  use gsi_4dvar, only: l4dvar,time_4dvar,winlen
  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: obstype,infile
  character(len=*),intent(in   ) :: sis
  integer(i_kind) ,intent(in   ) :: lunout
  integer(i_kind) ,intent(inout) :: nread,ndata,nodata
  real(r_kind)    ,intent(in   ) :: twind

! Declare local parameters
  integer(i_kind),parameter:: maxobs=2e6_i_kind
  integer(i_kind),parameter:: maxdat=20_i_kind
  real(r_double),parameter:: r360=360.0_r_double

! Declare local variables
  logical dwl,outside

  character(40) hdstr
  character(44) dwstr
  character(10) date
  character(8) subset

  integer(i_kind) lunin,i,kx,ilat,ikx,idomsfc
  integer(i_kind) jdate,ihh,idd,idate,iret,im,iy,k,levs
  integer(i_kind) nbadlat,nmrecs,ilon,nreal,nchanl


  real(r_kind) time,usage,dlat,dlon,dlat_earth,dlon_earth
  real(r_kind) loswind,sfcr,tsavg,ff10,toff,t4dv
  real(r_kind),allocatable,dimension(:,:):: cdata_all

  real(r_double) rstation_id
  real(r_double),dimension(5):: hdr
  real(r_double),dimension(8):: dwld

  integer(i_kind) idate5(5),minobs,minan
  real(r_kind) time_correction

  data hdstr  /'SID XOB YOB DHR TYP'/
  data dwstr  /'ADWL ELEV BORA NOLS NOLC ADPL LOSC SDLE'/
  
  data lunin / 10_i_kind /


!**************************************************************************
! Initialize variables
  nbadlat=izero
  nmrecs=izero
  nreal=maxdat
  nchanl=izero
  ilon=2_i_kind
  ilat=3_i_kind

  allocate(cdata_all(maxdat,maxobs))


! Open, then read date from bufr data
  open(lunin,file=infile,form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)
  call readmg(lunin,subset,idate,iret)
  if(iret/=izero) goto 1010

! Time offset
  call time_4dvar(idate,toff)

! If date in lidar file does not agree with analysis date, 
! print message and stop program execution.
  write(date,'( i10)') idate
  read (date,'(i4,3i2)') iy,im,idd,ihh
  if(offtime_data) then

!       in time correction for observations to account for analysis
!                    time being different from obs file time.
     write(date,'( i10)') idate
     read (date,'(i4,3i2)') iy,im,idd,ihh
     idate5(1)=iy
     idate5(2)=im
     idate5(3)=idd
     idate5(4)=ihh
     idate5(5)=izero
     call w3fs21(idate5,minobs)    !  obs ref time in minutes relative to historic date
     idate5(1)=iadate(1)
     idate5(2)=iadate(2)
     idate5(3)=iadate(3)
     idate5(4)=iadate(4)
     idate5(5)=izero
     call w3fs21(idate5,minan)    !  analysis ref time in minutes relative to historic date

!     add obs reference time, then subtract analysis time to get obs time relative to analysis

     time_correction=float(minobs-minan)*r60inv

  else
     time_correction=zero
  end if

  write(6,*)'READ_LIDAR: time offset is ',toff,' hours.'

! Big loop over bufr file	

10 call readsb(lunin,iret) 
  if(iret/=izero) then
     call readmg(lunin,subset,jdate,iret)
     if(iret/=izero) go to 1000
     go to 10
  end if
  nmrecs=nmrecs+ione

! Extract type, date, and location information
  call ufbint(lunin,hdr,5_i_kind,ione,iret,hdstr)

  kx=hdr(5)
     
  ikx=izero
  do i=1,nconvtype
     if(trim(obstype) == trim(ioctype(i)) .and. kx == ictype(i))ikx = i
  end do
! Determine if this is doppler wind lidar report
  dwl= (kx /= izero) .and. (subset=='DWLDAT')
  if(.not. dwl) go to 10
  nread=nread+ione

  t4dv = toff + hdr(4)
  if (l4dvar) then
     if (t4dv<zero .OR. t4dv>winlen) go to 10
  else
     time=hdr(4) + time_correction
     if (abs(time) > ctwind(ikx) .or. abs(time) > twind) go to 10
  endif

  rstation_id=hdr(1)

  if (abs(hdr(3))<89.9_r_double .and. abs(hdr(2))<=r360) then      !msq
     if (hdr(2) == r360) hdr(2)=hdr(2)-r360
     if (hdr(2) < zero)  hdr(2)=hdr(2)+r360

     dlat_earth = hdr(3) * deg2rad
     dlon_earth = hdr(2) * deg2rad

     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if (outside) go to 10
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd(dlat,ione,rlats,nlat,ione)
        call grdcrd(dlon,ione,rlons,nlon,ione)
     endif



!    If wind data, extract observation.
     nodata=min(nodata+ione,maxobs)
     ndata=min(ndata+ione,maxobs)
     usage = zero
     if(icuse(ikx) < izero)usage=100._r_kind
     if(ncnumgrp(ikx) > izero )then                     ! cross validation on
        if(mod(ndata,ncnumgrp(ikx))== ncgroup(ikx)-ione)usage=ncmiter(ikx)
     end if

     call ufbint(lunin,dwld,8_i_kind,ione,levs,dwstr)

     loswind=dwld(7)/(cos(dwld(2)*deg2rad))    ! obs wind (line of sight component)
     call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr)
     cdata_all(1,ndata)=ikx                    ! obs type
     cdata_all(2,ndata)=dlon                   ! grid relative longitude
     cdata_all(3,ndata)=dlat                   ! grid relative latitude
     cdata_all(4,ndata)=t4dv                   ! obs time (analyis relative hour)
     cdata_all(5,ndata)=dwld(1)                ! obs height (altitude) (m)
     cdata_all(6,ndata)=dwld(2)*deg2rad        ! elevation angle (radians)
     cdata_all(7,ndata)=dwld(3)*deg2rad        ! bearing or azimuth (radians)
     cdata_all(8,ndata)=dwld(4)                ! number of laser shots
     cdata_all(9,ndata)=dwld(5)                ! number of cloud laser shots
     cdata_all(10,ndata)=dwld(6)               ! atmospheric depth
     cdata_all(11,ndata)=loswind               ! obs wind (line of sight component) msq
     cdata_all(12,ndata)=dwld(8)               ! standard deviation (obs error) msq
     cdata_all(13,ndata)=rstation_id           ! station id
     cdata_all(14,ndata)=usage                 ! usage parameter
     cdata_all(15,ndata)=idomsfc+0.001_r_kind  ! dominate surface type
     cdata_all(16,ndata)=tsavg                 ! skin temperature      
     cdata_all(17,ndata)=ff10                  ! 10 meter wind factor  
     cdata_all(18,ndata)=sfcr                  ! surface roughness     
     cdata_all(19,ndata)=dlon_earth*rad2deg    ! earth relative longitude (degrees)
     cdata_all(20,ndata)=dlat_earth*rad2deg    ! earth relative latitude (degrees)

  else
     nbadlat=nbadlat+ione
  endif

! End of bufr read loop
  go to 10


! Normal exit
1000 continue


! Write observations to scratch file
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,maxdat),i=1,ndata)

  write(6,*)'READ_LIDAR:  number of bad latitudes or longitudes = ',nbadlat

! Close unit to bufr file
1010 continue
  deallocate(cdata_all)
  call closbf(lunin)

! End of routine
  return
end subroutine read_lidar
