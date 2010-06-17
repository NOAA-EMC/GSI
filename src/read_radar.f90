!  SUBSET=NC006001 -- level 3 superobs
!  SUBSET=NC006002 -- level 2.5 superobs
subroutine read_radar(nread,ndata,nodata,infile,lunout,obstype,twind,sis)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_radar                    read radar radial winds
!   prgmmr: yang             org: np23                date: 1998-05-15
!
! abstract:  This routine reads radar radial wind files.
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
!   2005-06-10  devenyi/treadon - correct subset declaration
!   2005-08-02  derber - modify to use convinfo file
!   2005-09-08  derber - modify to use input group time window
!   2005-10-11  treadon - change convinfo read to free format
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-10-26  treadon - add routine tag to convinfo printout
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-02-08  derber  - modify to use new convinfo module
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-04-21  parrish - modify to use level 2, 2.5, and/or 3 radar wind 
!                         superobs, with qc based on vad wind data.
!   2006-05-23  parrish - interpolate model elevation to vad wind site
!   2006-07-28  derber  - use r1000 from constants
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2008-04-17  safford - rm unused vars and uses
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2009-06-08  parrish - remove erroneous call to cosd, sind
!
!   input argument list:
!     infile   - file from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
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
!$$$  end documentation block
  use kinds, only: r_kind,r_single,r_double,i_kind,i_byte
  use constants, only: izero,ione,zero,half,one,deg2rad,rearth,rad2deg, &
                       one_tenth,r1000,r60inv
  use qcmod, only: erradar_inflate,vadfile
  use obsmod, only: iadate
  use gsi_4dvar, only: l4dvar,iwinbgn,winlen,time_4dvar
  use gridmod, only: regional,nlat,nlon,tll2xy,rlats,rlons,rotate_wind_ll2xy
  use gridmod, only: check_rotate_wind
  use convinfo, only: nconvtype,ctwind, &
       ncmiter,ncgroup,ncnumgrp,icuse,ictype,ioctype
  implicit none 
  
! Declare passed variables
  character(len=*),intent(in   ) :: obstype,infile
  character(len=*),intent(in   ) :: sis
  real(r_kind)    ,intent(in   ) :: twind
  integer(i_kind) ,intent(in   ) :: lunout
  integer(i_kind) ,intent(inout) :: nread,ndata,nodata

! Declare local parameters
  integer(i_kind),parameter:: maxlevs=1500_i_kind
  integer(i_kind),parameter:: maxdat=21_i_kind
  integer(i_kind),parameter:: maxvad=500_i_kind
! integer(i_kind),parameter:: maxvadbins=20_i_kind
  integer(i_kind),parameter:: maxvadbins=15_i_kind
  real(r_single),parameter:: r4_single = 4.0_r_single

  real(r_kind),parameter:: dzvad=304.8_r_kind  !  vad reports are every 1000 ft = 304.8 meters
  real(r_kind),parameter:: r3_5 = 3.5_r_kind
  real(r_kind),parameter:: r6 = 6.0_r_kind
  real(r_kind),parameter:: r8 = 8.0_r_kind
  real(r_kind),parameter:: r90 = 90.0_r_kind
  real(r_kind),parameter:: r100 = 100.0_r_kind
  real(r_kind),parameter:: r200 = 200.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: r400 = 400.0_r_kind
  real(r_kind),parameter:: r50000 = 50000.0_r_kind

! Declare local variables
  logical good,outside,good0,lexist1,lexist2
  
  character(10) date
  character(50) hdrstr,datstr
  character(8) subset,subset_check(2)
  character(30) outmessage
  
  integer(i_kind) lnbufr,i,k,maxobs
  integer(i_kind) nmrecs,ibadazm,ibadwnd,ibaddist,ibadheight,ibadvad,kthin
  integer(i_kind) iyr,imo,idy,ihr,imn
  integer(i_kind) ibadstaheight,ibaderror,notgood,idate,iheightbelowsta,ibadfit
  integer(i_kind) notgood0
  integer(i_kind) novadmatch,ioutofvadrange
  integer(i_kind) iy,im,idd,ihh,iret,levs,mincy,minobs,kx0,kxadd,kx
  integer(i_kind) nreal,nchanl,ilat,ilon,ikx
  integer(i_kind),dimension(5):: idate5
  integer(i_kind) ivad,ivadz,nvad,idomsfc
  
  real(r_kind) timeb,usage,ff10,sfcr,skint,t4dv,t4dvo,toff
  real(r_kind) eradkm,dlat_earth,dlon_earth
  real(r_kind) dlat,dlon,staheight,tiltangle,clon,slon,clat,slat
  real(r_kind) timeo,clonh,slonh,clath,slath,cdist,dist
  real(r_kind) rwnd,azm,height,error,wqm
  real(r_kind) azm_earth,cosazm_earth,sinazm_earth,cosazm,sinazm
  real(r_kind):: zsges
  
  real(r_kind),dimension(maxdat):: cdata
  real(r_kind),allocatable,dimension(:,:):: cdata_all
  
  real(r_double) rstation_id
  real(r_double),dimension(10):: hdr
  character(8) cstaid
  character(4) this_staid
  equivalence (this_staid,cstaid)
  equivalence (cstaid,rstation_id)
  real(r_double),dimension(7,maxlevs):: radar_obs
  real(r_double),dimension(4,maxlevs):: vad_obs
  
  character(8) vadid(maxvad)
  real(r_kind) vadlat(maxvad),vadlon(maxvad),vadqm(maxvad,maxvadbins)
  real(r_kind) vadu(maxvad,maxvadbins),vadv(maxvad,maxvadbins)
  real(r_kind) vadcount(maxvad,maxvadbins)
  real(r_kind),dimension(maxvad,maxvadbins)::vadfit2,vadcount2,vadwgt2
  real(r_kind),dimension(maxvad,maxvadbins)::vadfit2_5,vadcount2_5,vadwgt2_5
  real(r_kind),dimension(maxvad,maxvadbins)::vadfit3,vadcount3,vadwgt3
  real(r_kind) zob,vadqmmin,vadqmmax
  integer(i_kind) level2(maxvad),level2_5(maxvad),level3(maxvad),level3_tossed_by_2_5(maxvad)
  integer(i_kind) loop,numcut
  integer(i_kind) numhits(0:maxvad)
  real(r_kind) timemax,timemin,errmax,errmin
  real(r_kind) dlatmax,dlonmax,dlatmin,dlonmin
  real(r_kind) xscale,xscalei
  integer(i_kind) max_rrr,nboxmax
  integer(i_kind) irrr,iaaa,iaaamax,iaaamin
  integer(i_byte),allocatable::nobs_box(:,:,:,:)
  real(r_kind) dlonvad,dlatvad,vadlon_earth,vadlat_earth
  real(r_single) this_stalat,this_stalon,this_stahgt,thistime,thislat,thislon
  real(r_single) thishgt,thisvr,corrected_azimuth,thiserr,corrected_tilt
  integer(i_kind) nsuper2_in,nsuper2_kept
  integer(i_kind) nsuper2_5_in,nsuper2_5_kept
  integer(i_kind) nsuper3_in,nsuper3_kept
  real(r_kind) errzmax
  real(r_kind) thisfit,thisvadspd,thisfit2,uob,vob,thiswgt
! real(r_kind) dist2min,dist2max
! real(r_kind) dist2_5min,dist2_5max
  real(r_kind) vad_leash
  
  data lnbufr/10_i_kind/
  data hdrstr / 'CLAT CLON SELV ANEL YEAR MNTH DAYS HOUR MINU MGPT' /
  data datstr / 'STDM SUPLAT SUPLON HEIT RWND RWAZ RSTD' /
  
!***********************************************************************************

! Check to see if radar wind files exist.  If none exist, exit this routine.
  inquire(file='radar_supobs_from_level2',exist=lexist1)
  inquire(file=infile,exist=lexist2)
  if (.not.lexist1 .and. .not.lexist2) goto 900


! Initialize variables
! vad_leash=.1_r_kind
  vad_leash=.3_r_kind
 !xscale=5000._r_kind
 !xscale=10000._r_kind
  xscale=20000._r_kind
  write(6,*)'READ_RADAR:  set vad_leash,xscale=',vad_leash,xscale
  write(6,*)'READ_RADAR:  set maxvadbins,maxbadbins*dzvad=',maxvadbins,&
       maxvadbins*dzvad
  xscalei=one/xscale
  max_rrr=nint(100000.0_r_kind*xscalei)
  nboxmax=ione
  iaaamax=-huge(iaaamax)
  iaaamin=huge(iaaamin)


  eradkm=rearth*0.001_r_kind
  kx0=22500_i_kind
  maxobs=2e6_i_kind
  nreal=maxdat
  nchanl=izero
  ilon=2_i_kind
  ilat=3_i_kind

  nmrecs=izero

  allocate(cdata_all(maxdat,maxobs))

  errzmax=zero
  nvad=izero
  vadlon=zero
  vadlat=zero
  vadqm=-99999_r_kind
  vadu=zero
  vadv=zero
  vadcount=zero
  vadqmmax=-huge(vadqmmax)
  vadqmmin=huge(vadqmmin)


! First read in all vad winds so can use vad wind quality marks to decide 
! which radar data to keep
! Open, then read bufr data
  open(lnbufr,file=vadfile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)
  call readmg(lnbufr,subset,idate,iret)
  if(iret/=izero) go to 20

! Time offset
  call time_4dvar(idate,toff)

  write(date,'( i10)') idate
  read (date,'(i4,3i2)') iy,im,idd,ihh 
  write(6,*)'READ_RADAR:  first read vad winds--use vad quality marks to qc 2.5/3 radar winds'

! Big loop over vadwnd bufr file
10 call readsb(lnbufr,iret)
  if(iret/=izero) then
     call readmg(lnbufr,subset,idate,iret)
     if(iret/=izero) go to 20
     go to 10
  end if
  nmrecs = nmrecs+ione

! Read header.  Extract station infomration
  call ufbint(lnbufr,hdr,6_i_kind,ione,levs,'SID XOB YOB DHR TYP SAID ')
  kx=nint(hdr(5))
  if(kx /= 224_i_kind) go to 10       !  for now just hardwire vad wind type
                               !  and don't worry about subtypes
! Is vadwnd in convinfo file
  ikx=izero
  do i=1,nconvtype
     if(kx == ictype(i)) then
        ikx=i
        exit
     end if
  end do
  if(ikx == izero) go to 10

! Time check
  t4dv=toff+hdr(4)
  if (l4dvar) then
     if (t4dv<zero .OR. t4dv>winlen) go to 10 ! outside time window
  else
     timeb=hdr(4)
     if(abs(timeb) > ctwind(ikx) .or. abs(timeb) > half) go to 10 ! outside time window 
  endif

! Create table of vad lat-lons and quality marks in 500m increments
! for cross-referencing bird qc against radar winds
  rstation_id=hdr(1)      !station id
  dlon_earth=hdr(2)       !station lat (degrees)
  dlat_earth=hdr(3)       !station lon (degrees)

  if (abs(dlat_earth)>90.0_r_kind .or. abs(dlon_earth)>r360) go to 10  ! bad lat/lon
  if (dlon_earth==r360) dlon_earth=dlon_earth-r360
  if (dlon_earth<zero ) dlon_earth=dlon_earth+r360
  dlat_earth = dlat_earth * deg2rad
  dlon_earth = dlon_earth * deg2rad
  ivad=izero
  if(nvad>izero) then
     do i=1,nvad
        if(modulo(rad2deg*abs(dlon_earth-vadlon(i)),r360)<one_tenth.and. &
             rad2deg*abs(dlat_earth-vadlat(i))<one_tenth) then
           ivad=i
           exit
        end if
     end do
  end if
  if(ivad==izero) then
     nvad=nvad+ione
     if(nvad>maxvad) then
        write(6,*)'READ_RADAR:  ***ERROR*** MORE THAN ',maxvad,' RADARS:  PROGRAM STOPS'
        call stop2(84)
     end if
     ivad=nvad
     vadlon(ivad)=dlon_earth
     vadlat(ivad)=dlat_earth
     vadid(ivad)=cstaid
  end if

! Update vadqm table
  call ufbint(lnbufr,vad_obs,4_i_kind,maxlevs,levs,'ZOB WQM UOB VOB ')
  if(levs>maxlevs) then
     write(6,*)'READ_RADAR:  ***ERROR*** need to increase read_radar bufr size since ',&
          ' number of levs=',levs,' > maxlevs=',maxlevs
     call stop2(84)
  endif

  do k=1,levs
     wqm=vad_obs(2,k)
     zob=vad_obs(1,k)
     uob=vad_obs(3,k)
     vob=vad_obs(4,k)
     ivadz=nint(zob/dzvad)
     if(ivadz<ione.or.ivadz>maxvadbins) cycle
     errzmax=max(abs(zob-ivadz*dzvad),errzmax)
     vadqm(ivad,ivadz)=max(vadqm(ivad,ivadz),wqm)
     vadqmmax=max(vadqmmax,wqm)
     vadqmmin=min(vadqmmin,wqm)
     vadu(ivad,ivadz)=vadu(ivad,ivadz)+uob
     vadv(ivad,ivadz)=vadv(ivad,ivadz)+vob
     vadcount(ivad,ivadz)=vadcount(ivad,ivadz)+one
  end do
     

! End of bufr read loop
  go to 10

! Normal exit
20 continue
  call closbf(lnbufr)


! Print vadwnd table
  if(nvad>izero) then
     do ivad=1,nvad
        do ivadz=1,maxvadbins
           vadu(ivad,ivadz)=vadu(ivad,ivadz)/max(one,vadcount(ivad,ivadz))
           vadv(ivad,ivadz)=vadv(ivad,ivadz)/max(one,vadcount(ivad,ivadz))
        end do
        write(6,'(" n,lat,lon,qm=",i3,2f8.2,2x,25i3)') &
             ivad,vadlat(ivad)*rad2deg,vadlon(ivad)*rad2deg,(max(-9_i_kind,nint(vadqm(ivad,k))),k=1,maxvadbins)
     end do
  end if
  write(6,*)' errzmax=',errzmax
  
!  Allocate thinning grids around each radar
!  space needed is nvad*max_rrr*max_rrr*8*max_zzz
!
!      max_rrr=20
!      maxvadbins=20
!      nvad=150
!      space=150*20*20*8*20 = 64000*150=9600000  peanuts
  
  allocate(nobs_box(max_rrr,8*max_rrr,maxvadbins,nvad))
  nobs_box=0_i_byte

! Set level2_5 to 0.  Then loop over routine twice, first looking for
! level 2.5 data, and setting level2_5=count of 2.5 data for any 2.5 data
! available that passes the vad tests.  The second pass puts in level 3
! data where it is available and no level 2.5 data was saved/available 
! (level2_5=0)

  dlatmax=-huge(dlatmax)
  dlonmax=-huge(dlonmax)
  dlatmin=huge(dlatmin)
  dlonmin=huge(dlonmin)
  vadfit2=zero
  vadfit2_5=zero
  vadfit3=zero
  vadwgt2=zero
  vadwgt2_5=zero
  vadwgt3=zero
  vadcount2=zero
  vadcount2_5=zero
  vadcount3=zero
  level2=izero
  level2_5=izero
  level3=izero
  level3_tossed_by_2_5=izero
  subset_check(1)='NC006002'
  subset_check(2)='NC006001'

! First process any level 2 superobs.
! Initialize variables.
  ikx=izero
  do i=1,nconvtype
     if(trim(ioctype(i)) == trim(obstype))ikx = i
  end do
  
  timemax=-huge(timemax)
  timemin=huge(timemin)
  errmax=-huge(errmax)
  errmin=huge(errmin)
  loop=izero

  numhits=izero
  ibadazm=izero
  ibadwnd=izero
  ibaddist=izero
  ibadheight=izero
  ibadstaheight=izero
  iheightbelowsta=izero
  ibaderror=izero
  ibadvad=izero
  ibadfit=izero
  ioutofvadrange=izero
  kthin=izero
  novadmatch=izero
  notgood=izero
  notgood0=izero
  nsuper2_in=izero
  nsuper2_kept=izero

  if(loop==izero) outmessage='level 2 superobs:'

! Open sequential file containing superobs
  open(lnbufr,file='radar_supobs_from_level2',form='unformatted')
  rewind lnbufr

 ! dist2max=-huge(dist2max)
 ! dist2min=huge(dist2min)

! Loop to read superobs data file
  do
     read(lnbufr,iostat=iret)this_staid,this_stalat,this_stalon,this_stahgt, &
          thistime,thislat,thislon,thishgt,thisvr,corrected_azimuth,thiserr,corrected_tilt
     if(iret/=izero) exit
     nsuper2_in=nsuper2_in+ione

     dlat_earth=this_stalat    !station lat (degrees)
     dlon_earth=this_stalon    !station lon (degrees)
     if (abs(dlat_earth)>90.0_r_kind .or. abs(dlon_earth)>r360) cycle
     if (dlon_earth==r360) dlon_earth=dlon_earth-r360
     if (dlon_earth<zero ) dlon_earth=dlon_earth+r360
     dlat_earth = dlat_earth * deg2rad
     dlon_earth = dlon_earth * deg2rad
     
     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if (outside) cycle
        dlatmax=max(dlat,dlatmax)
        dlonmax=max(dlon,dlonmax)
        dlatmin=min(dlat,dlatmin)
        dlonmin=min(dlon,dlonmin)
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd(dlat,ione,rlats,nlat,ione)
        call grdcrd(dlon,ione,rlons,nlon,ione)
     endif
     
     clon=cos(dlon_earth)
     slon=sin(dlon_earth)
     clat=cos(dlat_earth)
     slat=sin(dlat_earth)
     staheight=this_stahgt    !station elevation
     tiltangle=corrected_tilt*deg2rad

!    Find vad wind match
     ivad=izero
     do k=1,nvad
        cdist=sin(vadlat(k))*slat+cos(vadlat(k))*clat* &
             (sin(vadlon(k))*slon+cos(vadlon(k))*clon)
        cdist=max(-one,min(cdist,one))
        dist=rad2deg*acos(cdist)
        
        if(dist < 0.2_r_kind) then
           ivad=k
           exit
        end if
     end do
     numhits(ivad)=numhits(ivad)+ione
     if(ivad==izero) then
        novadmatch=novadmatch+ione
        cycle
     end if
     
     vadlon_earth=vadlon(ivad)
     vadlat_earth=vadlat(ivad)
     if(regional)then
        call tll2xy(vadlon_earth,vadlat_earth,dlonvad,dlatvad,outside)
        if (outside) cycle
        dlatmax=max(dlatvad,dlatmax)
        dlonmax=max(dlonvad,dlonmax)
        dlatmin=min(dlatvad,dlatmin)
        dlonmin=min(dlonvad,dlonmin)
     else
        dlatvad = vadlat_earth
        dlonvad = vadlon_earth
        call grdcrd(dlatvad,ione,rlats,nlat,ione)
        call grdcrd(dlonvad,ione,rlons,nlon,ione)
     endif

!    Get model terrain at VAD wind location
     call deter_zsfc_model(dlatvad,dlonvad,zsges)

     t4dvo=toff+thistime
     timemax=max(timemax,t4dvo)
     timemin=min(timemin,t4dvo)

!    Exclude data if it does not fall within time window
     if (l4dvar) then
        if (t4dvo<zero .OR. t4dvo>winlen) cycle
     else
        timeo=thistime
        if(abs(timeo)>half ) cycle
     endif

!    Get observation (lon,lat).  Compute distance from radar.
     dlat_earth=thislat
     dlon_earth=thislon
     if (abs(dlat_earth)>90.0_r_kind .or. abs(dlon_earth)>r360) cycle
     if(dlon_earth==r360) dlon_earth=dlon_earth-r360
     if(dlon_earth<zero ) dlon_earth=dlon_earth+r360
     
     dlat_earth = dlat_earth*deg2rad
     dlon_earth = dlon_earth*deg2rad
     if(regional) then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if (outside) cycle
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd(dlat,ione,rlats,nlat,ione)
        call grdcrd(dlon,ione,rlons,nlon,ione)
     endif
     
     clonh=cos(dlon_earth)
     slonh=sin(dlon_earth)
     clath=cos(dlat_earth)
     slath=sin(dlat_earth)
     cdist=slat*slath+clat*clath*(slon*slonh+clon*clonh)
     cdist=max(-one,min(cdist,one))
     dist=eradkm*acos(cdist)
     irrr=nint(dist*1000*xscalei)
     if(irrr<=izero .or. irrr>max_rrr) cycle

!    Extract radial wind data
     height= thishgt
     rwnd  = thisvr
     azm_earth = corrected_azimuth
     if(regional) then
        cosazm_earth=cos(azm_earth*deg2rad)
        sinazm_earth=sin(azm_earth*deg2rad)
        call rotate_wind_ll2xy(cosazm_earth,sinazm_earth,cosazm,sinazm,dlon_earth,dlon,dlat)
        azm=atan2(sinazm,cosazm)*rad2deg
     else
        azm=azm_earth
     end if
     iaaa=azm/(r360/(r8*irrr))
     iaaa=mod(iaaa,8*irrr)
     if(iaaa<izero) iaaa=iaaa+8*irrr
     iaaa=iaaa+ione
     iaaamax=max(iaaamax,iaaa)
     iaaamin=min(iaaamin,iaaa)
          
     error = erradar_inflate*thiserr
     errmax=max(error,errmax)
     if(thiserr>zero) errmin=min(error,errmin)
     
!    Perform limited qc based on azimuth angle, radial wind
!    speed, distance from radar site, elevation of radar,
!    height of observation, observation error, and goodness of fit to vad wind

     good0=.true.
     if(abs(azm)>r400) then
        ibadazm=ibadazm+ione; good0=.false.
     end if
     if(abs(rwnd)>r200) then
        ibadwnd=ibadwnd+ione; good0=.false.
     end if
     if(dist>r400) then
        ibaddist=ibaddist+ione; good0=.false.
     end if
     if(staheight<-r1000.or.staheight>r50000) then
        ibadstaheight=ibadstaheight+ione; good0=.false.
     end if
     if(height<-r1000.or.height>r50000) then
        ibadheight=ibadheight+ione; good0=.false.
     end if
     if(height<staheight) then
        iheightbelowsta=iheightbelowsta+ione ; good0=.false.
     end if
     if(thiserr>r6 .or. thiserr<=zero) then
        ibaderror=ibaderror+ione; good0=.false.
     end if
     good=.true.
     if(.not.good0) then
        notgood0=notgood0+ione
        cycle
     else

!       Check fit to vad wind and vad wind quality mark
        ivadz=nint(thishgt/dzvad)
        if(ivadz>maxvadbins.or.ivadz<ione) then
           ioutofvadrange=ioutofvadrange+ione
           cycle
        end if
        thiswgt=one/max(r4_single,thiserr**2)
        thisfit2=(vadu(ivad,ivadz)*cos(azm_earth*deg2rad)+vadv(ivad,ivadz)*sin(azm_earth*deg2rad)-thisvr)**2
        thisfit=sqrt(thisfit2)
        thisvadspd=sqrt(vadu(ivad,ivadz)**2+vadv(ivad,ivadz)**2)
        vadfit2(ivad,ivadz)=vadfit2(ivad,ivadz)+thiswgt*thisfit2
        vadcount2(ivad,ivadz)=vadcount2(ivad,ivadz)+one
        vadwgt2(ivad,ivadz)=vadwgt2(ivad,ivadz)+thiswgt
        if(thisfit/max(one,thisvadspd)>vad_leash) then
           ibadfit=ibadfit+ione; good=.false.
        end if
        if(nobs_box(irrr,iaaa,ivadz,ivad)>nboxmax) then
           kthin=kthin+ione
           good=.false.
        end if
        if(vadqm(ivad,ivadz) > r3_5  .or.  vadqm(ivad,ivadz) < -one) then
           ibadvad=ibadvad+ione ; good=.false.
        end if
     end if
     
!    If data is good, load into output array
     if(good) then
        nsuper2_kept=nsuper2_kept+ione
        level2(ivad)=level2(ivad)+ione
        nobs_box(irrr,iaaa,ivadz,ivad)=nobs_box(irrr,iaaa,ivadz,ivad)+1_i_byte
        ndata    =min(ndata+ione,maxobs)
        nodata   =min(nodata+ione,maxobs)  !number of obs not used (no meaning here)
        usage = zero
        if(icuse(ikx) < izero)usage=r100
        if(ncnumgrp(ikx) > izero )then                     ! cross validation on
           if(mod(ndata,ncnumgrp(ikx))== ncgroup(ikx)-ione)usage=ncmiter(ikx)
        end if

        call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,skint,ff10,sfcr)

        cdata(1) = error             ! wind obs error (m/s)
        cdata(2) = dlon              ! grid relative longitude
        cdata(3) = dlat              ! grid relative latitude
        cdata(4) = height            ! obs absolute height (m)
        cdata(5) = rwnd              ! wind obs (m/s)
        cdata(6) = azm*deg2rad       ! azimuth angle (radians)
        cdata(7) = t4dv              ! obs time (hour)
        cdata(8) = ikx               ! type               
        cdata(9) = tiltangle         ! tilt angle (radians)
        cdata(10)= staheight         ! station elevation (m)
        cdata(11)= rstation_id       ! station id
        cdata(12)= usage             ! usage parameter
        cdata(13)= idomsfc           ! dominate surface type
        cdata(14)= skint             ! skin temperature
        cdata(15)= ff10              ! 10 meter wind factor
        cdata(16)= sfcr              ! surface roughness
        cdata(17)=dlon_earth*rad2deg ! earth relative longitude (degrees)
        cdata(18)=dlat_earth*rad2deg ! earth relative latitude (degrees)
        cdata(19)=dist               ! range from radar in km (used to estimate beam spread)
        cdata(20)=zsges              ! model elevation at radar site
        cdata(21)=thiserr

!       if(vadid(ivad)=='0303LWX') then
!          dist2max=max(dist2max,dist)
!          dist2min=min(dist2min,dist)
!       end if

        do i=1,maxdat
           cdata_all(i,ndata)=cdata(i)
        end do
        
     else
        notgood = notgood + ione
     end if
     
  end do

  close(lnbufr)	! A simple unformatted fortran file should not be mixed with a bufr I/O
  write(6,*)'READ_RADAR:  ',trim(outmessage),' reached eof on 2/2.5/3 superob radar file'

  write(6,*)'READ_RADAR: nsuper2_in,nsuper2_kept=',nsuper2_in,nsuper2_kept
  write(6,*)'READ_RADAR: # no vad match   =',novadmatch
  write(6,*)'READ_RADAR: # out of vadrange=',ioutofvadrange
  write(6,*)'READ_RADAR: # bad azimuths=',ibadazm
  write(6,*)'READ_RADAR: # bad winds   =',ibadwnd
  write(6,*)'READ_RADAR: # bad dists   =',ibaddist
  write(6,*)'READ_RADAR: # bad stahgts =',ibadstaheight
  write(6,*)'READ_RADAR: # bad obshgts =',ibadheight
  write(6,*)'READ_RADAR: # bad errors  =',ibaderror
  write(6,*)'READ_RADAR: # bad vadwnd  =',ibadvad
  write(6,*)'READ_RADAR: # bad fit     =',ibadfit 
  write(6,*)'READ_RADAR: # num thinned =',kthin
  write(6,*)'READ_RADAR: # notgood0    =',notgood0
  write(6,*)'READ_RADAR: # notgood     =',notgood
  write(6,*)'READ_RADAR: # hgt belowsta=',iheightbelowsta
  write(6,*)'READ_RADAR: timemin,max   =',timemin,timemax
  write(6,*)'READ_RADAR: errmin,max    =',errmin,errmax
  write(6,*)'READ_RADAR: dlatmin,max,dlonmin,max=',dlatmin,dlatmax,dlonmin,dlonmax
  write(6,*)'READ_RADAR: iaaamin,max,8*max_rrr  =',iaaamin,iaaamax,8*max_rrr


!  Next process level 2.5 and 3 superobs

!  Bigger loop over first level 2.5 data, and then level3 data

  timemax=-huge(timemax)
  timemin=huge(timemin)
  errmax=-huge(errmax)
  errmin=huge(errmin)
  nsuper2_5_in=izero
  nsuper3_in=izero
  nsuper2_5_kept=izero
  nsuper3_kept=izero
  do loop=1,2

     numhits=izero
     ibadazm=izero
     ibadwnd=izero
     ibaddist=izero
     ibadheight=izero
     ibadstaheight=izero
     iheightbelowsta=izero
     ibaderror=izero
     ibadvad=izero
     ibadfit=izero
     ioutofvadrange=izero
     kthin=izero
     novadmatch=izero
     notgood=izero
     notgood0=izero
!    dist2_5max=-huge(dist2_5max)
!    dist2_5min=huge(dist2_5min)

     if(loop==ione)     outmessage='level 2.5 superobs:'
     if(loop==2_i_kind) outmessage='level 3 superobs:'

!    Open, then read bufr data
     open(lnbufr,file=infile,form='unformatted')

     call openbf(lnbufr,'IN',lnbufr)
     call datelen(10)
     call readmg(lnbufr,subset,idate,iret)
     if(iret/=izero) then
        call closbf(lnbufr)
        go to 1000
     end if

     idate5(1) = iy    ! year
     idate5(2) = im    ! month
     idate5(3) = idd   ! day
     idate5(4) = ihh   ! hour
     idate5(5) = izero ! minute
     call w3fs21(idate5,mincy)


     nmrecs=izero
!    Big loop over bufr file

50   call readsb(lnbufr,iret)
60   continue
     if(iret/=izero) then
        call readmg(lnbufr,subset,idate,iret)
        if(iret/=izero) go to 1000
        go to 50
     end if
     if(subset/=subset_check(loop)) then
       iret=99_i_kind
       go to 60
     end if
     nmrecs = nmrecs+ione
     

!    Read header.  Extract station infomration
     call ufbint(lnbufr,hdr,10_i_kind,ione,levs,hdrstr)

 !   rstation_id=hdr(1)        !station id
     write(cstaid,'(2i4)')idint(hdr(1)),idint(hdr(2))
     if(cstaid(1:1)==' ')cstaid(1:1)='S'
     dlat_earth=hdr(1)         !station lat (degrees)
     dlon_earth=hdr(2)         !station lon (degrees)
     if (abs(dlat_earth)>90.0_r_kind .or. abs(dlon_earth)>r360) go to 50
     if (dlon_earth==r360) dlon_earth=dlon_earth-r360
     if (dlon_earth<zero ) dlon_earth=dlon_earth+r360
     dlat_earth = dlat_earth * deg2rad
     dlon_earth = dlon_earth * deg2rad
     
     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
        if (outside) go to 50
        dlatmax=max(dlat,dlatmax)
        dlonmax=max(dlon,dlonmax)
        dlatmin=min(dlat,dlatmin)
        dlonmin=min(dlon,dlonmin)
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd(dlat,ione,rlats,nlat,ione)
        call grdcrd(dlon,ione,rlons,nlon,ione)
     endif
     
     clon=cos(dlon_earth)
     slon=sin(dlon_earth)
     clat=cos(dlat_earth)
     slat=sin(dlat_earth)
     staheight=hdr(3)    !station elevation
     tiltangle=hdr(4)*deg2rad

!    Find vad wind match
     ivad=izero
     do k=1,nvad
        cdist=sin(vadlat(k))*slat+cos(vadlat(k))*clat* &
             (sin(vadlon(k))*slon+cos(vadlon(k))*clon)
        cdist=max(-one,min(cdist,one))
        dist=rad2deg*acos(cdist)
        
        if(dist < 0.2_r_kind) then
           ivad=k
           exit
        end if
     end do
     numhits(ivad)=numhits(ivad)+ione
     if(ivad==izero) then
        novadmatch=novadmatch+ione
        go to 50
     end if
     
     vadlon_earth=vadlon(ivad)
     vadlat_earth=vadlat(ivad)
     if(regional)then
        call tll2xy(vadlon_earth,vadlat_earth,dlonvad,dlatvad,outside)
        if (outside) go to 50
        dlatmax=max(dlatvad,dlatmax)
        dlonmax=max(dlonvad,dlonmax)
        dlatmin=min(dlatvad,dlatmin)
        dlonmin=min(dlonvad,dlonmin)
     else
        dlatvad = vadlat_earth
        dlonvad = vadlon_earth
        call grdcrd(dlatvad,ione,rlats,nlat,ione)
        call grdcrd(dlonvad,ione,rlons,nlon,ione)
     endif

!    Get model terrain at VAD wind location
     call deter_zsfc_model(dlatvad,dlonvad,zsges)

     iyr = hdr(5)
     imo = hdr(6)
     idy = hdr(7)
     ihr = hdr(8)
     imn = hdr(9)

     idate5(1) = iyr
     idate5(2) = imo
     idate5(3) = idy
     idate5(4) = ihr
     idate5(5) = imn
     ikx=izero
     do i=1,nconvtype
        if(trim(ioctype(i)) == trim(obstype))ikx = i
     end do
     if(ikx==izero) go to 50
     call w3fs21(idate5,minobs)
     t4dv=real(minobs-iwinbgn,r_kind)*r60inv
     if (l4dvar) then
        if (t4dv<zero .OR. t4dv>winlen) goto 50
     else
        timeb = real(minobs-mincy,r_kind)*r60inv
!       if (abs(timeb)>twind .or. abs(timeb) > ctwind(ikx)) then
        if (abs(timeb)>half .or. abs(timeb) > ctwind(ikx)) then 
!          write(6,*)'READ_RADAR:  time outside window ',timeb,' skip this obs'
           goto 50
        endif
     endif

!    Go through the data levels
     call ufbint(lnbufr,radar_obs,7_i_kind,maxlevs,levs,datstr)
     if(levs>maxlevs) then
        write(6,*)'READ_RADAR:  ***ERROR*** increase read_radar bufr size since ',&
             'number of levs=',levs,' > maxlevs=',maxlevs
        call stop2(84)
     endif

     numcut=izero
     do k=1,levs
        if(loop==ione)     nsuper2_5_in=nsuper2_5_in+ione
        if(loop==2_i_kind) nsuper3_in=nsuper3_in+ione
        nread=nread+ione
        t4dvo=real(minobs+radar_obs(1,k)-iwinbgn,r_kind)*r60inv
        timemax=max(timemax,t4dvo)
        timemin=min(timemin,t4dvo)
        if(loop==2_i_kind .and. ivad> izero .and. level2_5(ivad)/=izero) then
           level3_tossed_by_2_5(ivad)=level3_tossed_by_2_5(ivad)+ione
           numcut=numcut+ione
           cycle
        end if

!       Exclude data if it does not fall within time window
        if (l4dvar) then
           if (t4dvo<zero .OR. t4dvo>winlen) cycle
           timeo=t4dv
        else
           timeo=(real(minobs-mincy,r_kind)+real(radar_obs(1,k),r_kind))*r60inv
           if(abs(timeo)>twind .or. abs(timeo) > ctwind(ikx)) then
!             write(6,*)'READ_RADAR:  time outside window ',timeo,&
!                ' skip obs ',nread,' at lev=',k
              cycle
           end if
        end if

!       Get observation (lon,lat).  Compute distance from radar.
        if(abs(radar_obs(2,k))>90.0_r_kind .or. abs(radar_obs(3,k))>r360) cycle
        if(radar_obs(3,k)==r360) radar_obs(3,k)=radar_obs(3,k)-r360
        if(radar_obs(3,k)<zero ) radar_obs(3,k)=radar_obs(3,k)+r360

        dlat_earth = radar_obs(2,k)*deg2rad
        dlon_earth = radar_obs(3,k)*deg2rad
        if(regional) then
           call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
           if (outside) cycle
        else
           dlat = dlat_earth
           dlon = dlon_earth
           call grdcrd(dlat,ione,rlats,nlat,ione)
           call grdcrd(dlon,ione,rlons,nlon,ione)
        endif
        
        clonh=cos(dlon_earth)
        slonh=sin(dlon_earth)
        clath=cos(dlat_earth)
        slath=sin(dlat_earth)
        cdist=slat*slath+clat*clath*(slon*slonh+clon*clonh)
        cdist=max(-one,min(cdist,one))
        dist=eradkm*acos(cdist)
        irrr=nint(dist*1000*xscalei)
        if(irrr<=izero .or. irrr>max_rrr) cycle

!       Set observation "type" to be function of distance from radar
        kxadd=nint(dist*one_tenth)
        kx=kx0+kxadd

!       Extract radial wind data
        height= radar_obs(4,k)
        rwnd  = radar_obs(5,k)
        azm_earth   = r90-radar_obs(6,k)
        if(regional) then
           cosazm_earth=cos(azm_earth*deg2rad)
           sinazm_earth=sin(azm_earth*deg2rad)
           call rotate_wind_ll2xy(cosazm_earth,sinazm_earth,cosazm,sinazm,dlon_earth,dlon,dlat)
           azm=atan2(sinazm,cosazm)*rad2deg
        else
           azm=azm_earth
        end if
        iaaa=azm/(r360/(r8*irrr))
        iaaa=mod(iaaa,8*irrr)
        if(iaaa<izero) iaaa=iaaa+8*irrr
        iaaa=iaaa+ione
        iaaamax=max(iaaamax,iaaa)
        iaaamin=min(iaaamin,iaaa)
        
        error = erradar_inflate*radar_obs(7,k)
        errmax=max(error,errmax)
        if(radar_obs(7,k)>zero) errmin=min(error,errmin)
        
!       Perform limited qc based on azimuth angle, radial wind
!       speed, distance from radar site, elevation of radar,
!       height of observation, observation error.

        good0=.true.
        if(abs(azm)>r400) then
           ibadazm=ibadazm+ione; good0=.false.
        end if
        if(abs(rwnd)>r200) then
           ibadwnd=ibadwnd+ione; good0=.false.
        end if
        if(dist>r400) then
           ibaddist=ibaddist+ione; good0=.false.
        end if
        if(staheight<-r1000 .or. staheight>r50000) then
           ibadstaheight=ibadstaheight+ione; good0=.false.
        end if
        if(height<-r1000 .or. height>r50000) then
           ibadheight=ibadheight+ione; good0=.false.
        end if
        if(height<staheight) then
           iheightbelowsta=iheightbelowsta+ione ; good0=.false.
        end if
        if(radar_obs(7,k)>r6 .or. radar_obs(7,k)<=zero) then
           ibaderror=ibaderror+ione; good0=.false.
        end if
        good=.true.
        if(.not.good0) then
           notgood0=notgood0+ione
           cycle
        else

!          Check against vad wind quality mark
           ivadz=nint(height/dzvad)
           if(ivadz>maxvadbins.or.ivadz<ione) then
              ioutofvadrange=ioutofvadrange+ione
              cycle
           end if
           thiserr = radar_obs(7,k)
           thiswgt=one/max(r4_single,thiserr**2)
           thisfit2=(vadu(ivad,ivadz)*cos(azm_earth*deg2rad)+vadv(ivad,ivadz)*sin(azm_earth*deg2rad)-rwnd)**2
           thisfit=sqrt(thisfit2)
           thisvadspd=sqrt(vadu(ivad,ivadz)**2+vadv(ivad,ivadz)**2)
           if(loop==ione) then
              vadfit2_5(ivad,ivadz)=vadfit2_5(ivad,ivadz)+thiswgt*thisfit2
              vadcount2_5(ivad,ivadz)=vadcount2_5(ivad,ivadz)+one
              vadwgt2_5(ivad,ivadz)=vadwgt2_5(ivad,ivadz)+thiswgt
           else
              vadfit3(ivad,ivadz)=vadfit3(ivad,ivadz)+thiswgt*thisfit2
              vadcount3(ivad,ivadz)=vadcount3(ivad,ivadz)+one
              vadwgt3(ivad,ivadz)=vadwgt3(ivad,ivadz)+thiswgt
           end if
           if(thisfit/max(one,thisvadspd)>vad_leash) then
              ibadfit=ibadfit+ione; good=.false.
           end if
           if(nobs_box(irrr,iaaa,ivadz,ivad)>nboxmax) then
              kthin=kthin+ione
              good=.false.
           end if
           if(vadqm(ivad,ivadz)>r3_5 .or. vadqm(ivad,ivadz)<-one) then
              ibadvad=ibadvad+ione ; good=.false.
           end if
        end if

!       If data is good, load into output array
        if(good) then
           if(loop==ione.and.ivad>izero) then
              nsuper2_5_kept=nsuper2_5_kept+ione
              level2_5(ivad)=level2_5(ivad)+ione
           end if
           if(loop==2_i_kind.and.ivad>izero) then
              nsuper3_kept=nsuper3_kept+ione
              level3(ivad)=level3(ivad)+ione
           end if
           nobs_box(irrr,iaaa,ivadz,ivad)=nobs_box(irrr,iaaa,ivadz,ivad)+1_i_byte
           ndata  = min(ndata+ione,maxobs)
           nodata = min(nodata+ione,maxobs)  !number of obs not used (no meaning here)
           usage  = zero
           if(icuse(ikx) < izero)usage=r100
           if(ncnumgrp(ikx) > izero )then                     ! cross validation on
              if(mod(ndata,ncnumgrp(ikx))== ncgroup(ikx)-ione)usage=ncmiter(ikx)
           end if
           
           call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,skint,ff10,sfcr)
           
           cdata(1) = error           ! wind obs error (m/s)
           cdata(2) = dlon            ! grid relative longitude
           cdata(3) = dlat            ! grid relative latitude
           cdata(4) = height          ! obs absolute height (m)
           cdata(5) = rwnd            ! wind obs (m/s)
           cdata(6) = azm*deg2rad     ! azimuth angle (radians)
           cdata(7) = t4dvo           ! obs time (hour)
           cdata(8) = ikx             ! type               
           cdata(9) = tiltangle       ! tilt angle (radians)
           cdata(10)= staheight       ! station elevation (m)
           cdata(11)= rstation_id     ! station id
           cdata(12)= usage           ! usage parameter
           cdata(13)= idomsfc         ! dominate surface type
           cdata(14)= skint           ! skin temperature
           cdata(15)= ff10            ! 10 meter wind factor
           cdata(16)= sfcr            ! surface roughness
           cdata(17)=dlon_earth*rad2deg ! earth relative longitude (degrees)
           cdata(18)=dlat_earth*rad2deg ! earth relative latitude (degrees)
           cdata(19)=dist             ! range from radar in km (used to estimate beam spread)
           cdata(20)=zsges            ! model elevation at radar site
           cdata(21)=radar_obs(7,k)   ! original error from bufr file

           do i=1,maxdat
              cdata_all(i,ndata)=cdata(i)
           end do
           
        else
           notgood = notgood + ione
        end if
        
!    End of k loop over levs
     end do

!    End of bufr read loop
     go to 50

!    Normal exit
1000 continue
     call closbf(lnbufr)


!    Close unit to bufr file
     write(6,*)'READ_RADAR:  ',trim(outmessage),' reached eof on 2.5/3 superob radar file.'

     if(loop==ione)     write(6,*)'READ_RADAR:  nsuper2_5_in,nsuper2_5_kept=',nsuper2_5_in,nsuper2_5_kept
     if(loop==2_i_kind) write(6,*)'READ_RADAR:  nsuper3_in,nsuper3_kept=',nsuper3_in,nsuper3_kept
     write(6,*)'READ_RADAR: # no vad match   =',novadmatch
     write(6,*)'READ_RADAR: # out of vadrange=',ioutofvadrange
     write(6,*)'READ_RADAR: # bad azimuths=',ibadazm
     write(6,*)'READ_RADAR: # bad winds   =',ibadwnd
     write(6,*)'READ_RADAR: # bad dists   =',ibaddist
     write(6,*)'READ_RADAR: # bad stahgts =',ibadstaheight
     write(6,*)'READ_RADAR: # bad obshgts =',ibadheight
     write(6,*)'READ_RADAR: # bad errors  =',ibaderror
     write(6,*)'READ_RADAR: # bad vadwnd  =',ibadvad
     write(6,*)'READ_RADAR: # bad fit     =',ibadfit 
     write(6,*)'READ_RADAR: # num thinned =',kthin
     write(6,*)'READ_RADAR: # notgood0    =',notgood0
     write(6,*)'READ_RADAR: # notgood     =',notgood
     write(6,*)'READ_RADAR: # hgt belowsta=',iheightbelowsta
     write(6,*)'READ_RADAR: timemin,max   =',timemin,timemax
     write(6,*)'READ_RADAR: errmin,max    =',errmin,errmax
     write(6,*)'READ_RADAR: dlatmin,max,dlonmin,max=',dlatmin,dlatmax,dlonmin,dlonmax
     write(6,*)'READ_RADAR: iaaamin,max,8*max_rrr  =',iaaamin,iaaamax,8*max_rrr

  end do       !   end bigger loop over first level 2.5, then level 3 radar data


! Write out vad statistics
  do ivad=1,nvad
     write(6,'(" fit of 2, 2.5, 3 data to vad station, lat, lon = ",a8,2f14.2)') &
          vadid(ivad),vadlat(ivad)*rad2deg,vadlon(ivad)*rad2deg
     do ivadz=1,maxvadbins
        if(vadcount2(ivad,ivadz)>half) then
           vadfit2(ivad,ivadz)=sqrt(vadfit2(ivad,ivadz)/vadwgt2(ivad,ivadz))
        else
           vadfit2(ivad,ivadz)=zero
        end if
        if(vadcount2_5(ivad,ivadz)>half) then
           vadfit2_5(ivad,ivadz)=sqrt(vadfit2_5(ivad,ivadz)/vadwgt2_5(ivad,ivadz))
        else
           vadfit2_5(ivad,ivadz)=zero
        end if
        if(vadcount3(ivad,ivadz)>half) then
           vadfit3(ivad,ivadz)=sqrt(vadfit3(ivad,ivadz)/vadwgt3(ivad,ivadz))
        else
           vadfit3(ivad,ivadz)=zero
        end if
        write(6,'(" h,f2,f2.5,f3=",i7,f10.2,"/",i5,f10.2,"/",i5,f10.2,"/",i5)')nint(ivadz*dzvad),&
             vadfit2(ivad,ivadz),nint(vadcount2(ivad,ivadz)),&
             vadfit2_5(ivad,ivadz),nint(vadcount2_5(ivad,ivadz)),&
             vadfit3(ivad,ivadz),nint(vadcount3(ivad,ivadz))
     end do
  end do

  
! Write observation to scratch file
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,maxdat),i=1,ndata)
  deallocate(cdata_all)
  deallocate(nobs_box)
  
900 continue


! Generate stats on regional wind rotation
  if (regional) call check_rotate_wind('read_radar')


  return
end subroutine read_radar

subroutine deter_zsfc_model(dlat,dlon,zsfc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    deter_zsfc_model         determine model sfc elevation
!   prgmmr: parrish          org: np2                date: 2006-05-23
!
! abstract:  determines model sfc elevation
!
! program history log:
!   2006-05-23 parrish
!
!   input argument list:
!     dlat   - grid relative latitude
!     dlon   - grid relative longitude
!
!   output argument list:
!     zsfc     - model surface elevation (meters)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use satthin, only: zs_full
  use constants, only: izero,ione,one
  use gridmod, only: nlat,nlon
  implicit none

  real(r_kind),intent(in   ) :: dlat,dlon
  real(r_kind),intent(  out) :: zsfc

  integer(i_kind):: klat1,klon1,klatp1,klonp1
  real(r_kind):: dx,dy,dx1,dy1,w00,w10,w01,w11
  
  klon1=int(dlon); klat1=int(dlat)
  dx  =dlon-klon1; dy  =dlat-klat1
  dx1 =one-dx;    dy1 =one-dy
  w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy
  
  klat1=min(max(ione,klat1),nlat); klon1=min(max(izero,klon1),nlon)
  if(klon1==izero) klon1=nlon
  klatp1=min(nlat,klat1+ione); klonp1=klon1+ione
  if(klonp1==nlon+ione) klonp1=ione

! Interpolate zsfc to obs location
  zsfc=w00*zs_full(klat1,klon1 ) + w10*zs_full(klatp1,klon1 ) + &
       w01*zs_full(klat1,klonp1) + w11*zs_full(klatp1,klonp1)
  
  return
end subroutine deter_zsfc_model
