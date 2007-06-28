subroutine read_gps(nread,ndata,nodata,infile,lunout,obstype,twind, &
             nprof_gps,sis)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: read_gps                   read in and reformat gps data
!   prgmmr: l.cucurull       org: JCSDA/NCEP          date: 2004-03-18
!
! abstract:  This routine reads in and reformats gps radio occultation data.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2004-03-18  cucurull - testing a gps ref profile
!   2004-06-04  cucurull - reading available gps ref profiles at analysis time
!   2004-06-24  treadon  - update documentation
!   2004-07-29  treadon  - add only to module use, add intent in/out
!   2004-11-18  cucurull - increase number of fields read
!   2004-01-26  cucurull - replace error estimation, add check for time
!   2005-03-03  cucurull - reading files in bufr format
!   2005-03-28  cucurull - reading satellite information from bufr file for diagnostics
!   2005-06-01  cucurull - update time QC 
!   2005-08-02  derber - modify to use convinfo file
!   2005-09-08  derber - modify to use input group time window
!   2005-10-11  treadon - change convinfo read to free format
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-10-26  treadon - add routine tag to convinfo printout
!   2005-12-01  cucurull - add logical ref_obs
!                          .true.  will read refractivity
!                          .false. will read bending angle
!                        - add preliminary QC checks for refractivity and bending
!                        - add errors for bending angle
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-02-08  derber  - modify to use new convinfo module
!   2006-02-13 cucurull - modify errors for refractivity and increase QC checks
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-09-08 cucurull - modify bufr variables for COSMIC
!   2006-10-13 cucurull - add QC checks
!
!   input argument list:
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!
!   output argument list:
!     nread    - number of gps observations read
!     ndata    - number of gps profiles retained for further processing
!     nodata   - number of gps observations retained for further processing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind,r_double
  use constants, only: izero,deg2rad,rearth,zero,three,t0c,rad2deg
  use obsmod, only: iadate,ref_obs
  use convinfo, only: nconvtype,ctwind,cgross,cermax,cermin,cvar_b,cvar_pg, &
        ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype
  use gridmod, only: regional,nlon,nlat,tll2xy,rlats,rlons
  implicit none

! Declare passed variables
  character(10),intent(in):: obstype,infile
  character(20),intent(in):: sis
  real(r_kind),intent(in):: twind
  integer(i_kind),intent(in):: lunout
  integer(i_kind),intent(inout):: nread,ndata,nodata
  integer(i_kind),intent(inout):: nprof_gps

! Declare local parameters  
  integer(i_kind),parameter:: maxlevs=500
  integer(i_kind),parameter:: maxinfo=15
  real(r_kind),parameter:: r60=60.0_r_kind
  real(r_kind),parameter:: r100=100.0_r_kind
  real(r_kind),parameter:: r10000=10000.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: r5000=5000.0_r_kind
  real(r_kind),parameter:: r25000=25000.0_r_kind
  real(r_kind),parameter:: r31000=31000.0_r_kind
  real(r_kind),parameter:: r7000=7000.0_r_kind


! Declare local variables
  logical good,outside,lexist
  
  character(10) date
  character(40) filename
  character(80) hdr1a
  character,dimension(8):: subset

  
  integer(i_kind) lnbufr,i,k,maxobs,ireadmg,ireadsb,said,ptid
  integer(i_kind) nmrecs
  integer(i_kind) iyr,imo,idy,ihr,imn,isc,ithin
  integer(i_kind) notgood,idate
  integer(i_kind) iy,im,idd,ihh,iy2,iret,levs,levsr,mincy,minobs
  integer(i_kind) nreal,nchanl,ilat,ilon
  integer(i_kind),dimension(5):: idate5,idate5_mod
  integer(i_kind)             :: qf,geo_height
  integer(i_kind)             :: idum,irec,ikx
  
  real(r_kind) timeb,timeo,rmesh
  real(r_kind) pcc,usage,dlat,dlat_earth,dlon,dlon_earth
  real(r_kind) rlat0,rlon0
  real(r_kind) height,rlat,rlon,ref,bend,impact,roc,&
               bend_error,ref_error,new_ref_error,ref_bufr,bend_pccf,ref_pccf

  real(r_kind),allocatable,dimension(:,:):: cdata_all
 
  integer(i_kind),parameter:: n1ahdr=9
  real(r_double),dimension(n1ahdr):: bfr1ahdr
  real(r_double),dimension(25,maxlevs):: data1b
  real(r_double),dimension(25,maxlevs):: data2a
 
  data lnbufr/10/
  data ithin / -9 /
  data rmesh / -99.999 /
  data hdr1a / 'YEAR MNTH DAYS HOUR MINU PCCF ELRC SAID PTID' / 
  
!***********************************************************************************

  maxobs=2e6
  nreal=maxinfo
  nchanl=0
  ilon=2
  ilat=3

  nmrecs=izero
  notgood=izero
  ikx = 0
  do i=1,nconvtype
    if(trim(obstype) == trim(ioctype(i)))ikx = i
  end do
  if(ikx == 0)then
    write(6,*)' READ GPS: NO DATA REQUESTED '
    return
  end if


! Open file for input, then read bufr data
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)
  call readmg(lnbufr,subset,idate,iret)
  if (iret/=izero) goto 1010

  write(date,'( i10)') idate
  read (date,'(i4,3i2)') iy,im,idd,ihh
  write(6,*)'READ_GPS: bufr message date is ',iy,im,idd,ihh,infile
  if(iy/=iadate(1).or.im/=iadate(2)) then
     write(6,*)'***READ_GPS ERROR*** incompatable analysis ',&
          'and observation date/time'
     write(6,*)' year  anal/obs ',iadate(1),iy
     write(6,*)' month anal/obs ',iadate(2),im
     write(6,*)' day   anal/obs ',iadate(3),idd
     write(6,*)' hour  anal/obs ',iadate(4),ihh
     goto 1010
  end if

! Allocate work array to hold observations
  allocate(cdata_all(nreal,maxobs))

! Big loop over the bufr file

 do while(ireadmg(lnbufr,subset,idate)==0)
  read_loop:  do while(ireadsb(lnbufr)==0)

! Read/decode data in subset

! Extract header information
   call ufbint(lnbufr,bfr1ahdr,n1ahdr,1,iret,hdr1a)

! observation time in minutes
   idate5(1) = bfr1ahdr(1) ! year
   idate5(2) = bfr1ahdr(2) ! month
   idate5(3) = bfr1ahdr(3) ! day
   idate5(4) = bfr1ahdr(4) ! hour
   idate5(5) = bfr1ahdr(5) ! minute
   pcc=bfr1ahdr(6)         ! profile per cent confidence
   roc=bfr1ahdr(7)         ! Earth local radius of curvature
   said=bfr1ahdr(8)        ! Satellite identifier
   ptid=bfr1ahdr(9)        ! Platform transmitter ID number
   call w3fs21(idate5,minobs)

! check time window in subset
   call w3fs21(iadate,mincy) ! analysis time in minutes
   timeo=(minobs-mincy)/r60
   if (abs(timeo)>ctwind(ikx) .or. abs(timeo) > twind) then
      write(6,*)'READ_GPS:      time outside window ',&
           timeo,' skip this report'
      cycle read_loop
   endif

   nmrecs = nmrecs+1 ! count reports in bufr file

   usage = 0.
   if(icuse(ikx) < 0)usage=100.
   if(ncnumgrp(ikx) > 0 )then                     ! cross validation on
     if(mod(nmrecs,ncnumgrp(ikx))== ncgroup(ikx)-1)usage=ncmiter(ikx)
   end if

!  check we have the same number of levels for ref and bending angle
!  when ref_obs on
   call ufbseq(lnbufr,data1b,25,maxlevs,levs,'ROSEQ1')  ! bending angle for COSMIC
   call ufbseq(lnbufr,data2a,25,maxlevs,levsr,'ROSEQ3') ! refractivity for COSMIC
!  call ufbseq(lnbufr,data1b,25,maxlevs,levs,'REP1')  ! bending angle for CHAMP
!  call ufbseq(lnbufr,data2a,25,maxlevs,levsr,'REP2') ! refractivity for CHAMP
   if ((ref_obs).and.(levs/=levsr)) then
    write(6,*) 'there are ',levs, ' levels in REP1 and',levsr,& 
                 ' levels in REP2'
    call stop2(92)
   endif

   do k=1, levs
     nread=nread+1     ! count observations
     rlat=data1b(1,k)  ! earth relative latitude (degrees)
     rlon=data1b(2,k)  ! earth relative longitude (degrees)
     impact=data1b(5,k)
     bend=data1b(6,k)
     bend_error=data1b(8,k)
     bend_pccf=data1b(10,k)
     height=data2a(1,k)
     ref=data2a(2,k)
     ref_error=data2a(4,k)
     ref_pccf=data2a(6,k)

! Check domain in regional model

! Preliminary (sanity) QC checks
     good=.true.
     if((rlat>=1.e+9_r_kind).or.(rlon>=1.e+9_r_kind).or.(height<=zero).or.(pcc<100.0_r_kind)) then
      good=.false.
     endif
     if (ref_obs) then
      if ((ref<=zero).or.(height>=1.e+9_r_kind)) then
         good=.false.
      endif
     else
      if ((bend>=1.e+9_r_kind).or.(impact>=1.e+9_r_kind).or.(impact<roc).or.&
        (bend<zero)) then
         good=.false.
      endif
     endif

! If observation is "good" load into output array
     if(good) then

! Compute error for the refractivity based on Kuo et al. 2003
 
      if(ref_obs) then
! Tropics
       if((rlat>=-30.0_r_kind).and.(rlat<=30.0_r_kind)) then
          if ((height .ge. r7000) .and. (height.le.r31000)) then
             ref_error = (ref/r100)*(0.1125_r_kind+(1.25e-5_r_kind*height))
          elseif (height.gt.r31000) then
              ref_error = (ref/r100)*0.5_r_kind
          elseif (height.lt.r7000) then
              ref_error = (ref/r100)*(3.0_r_kind-(4.e-4_r_kind*height))
          else
          write(6,*)'***READ_GPS HEIGHT ERROR***'
          stop
          endif
       else
! Mid-latitudes
           if ((height .ge. r5000) .and. (height.le.r25000)) then
              ref_error = (ref/r100)*0.3_r_kind
           elseif ((height .ge. r25000) .and. (height.le.r31000)) then
              ref_error = (ref/r100)*(-3.45_r_kind+(1.5e-4_r_kind*height))
           elseif (height.gt.r31000) then
              ref_error = (ref/r100)*1.2_r_kind
           elseif (height.lt.r5000) then
              ref_error = (ref/r100)*(0.75_r_kind-(9.e-5_r_kind*height))
           else
              write(6,*)'***READ_GPS HEIGHT ERROR***'
              stop
           endif
       endif
      else                      ! bending angle
       if((impact-roc) <= r10000) then 
        bend_error=(-bend*0.09_r_kind/r10000)*(impact-roc)+bend*1.e-1_r_kind
       else
        bend_error=max(7.e-6_r_kind,bend*1.e-2_r_kind)
       endif
      endif

       if (rlon>=r360)  rlon=rlon-r360
       if (rlon<zero  ) rlon=rlon+r360

       dlat_earth = rlat * deg2rad  !convert to radians
       dlon_earth = rlon * deg2rad

       if(regional)then
          call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
          if (outside) cycle read_loop
       else
          dlat = dlat_earth
          dlon = dlon_earth
          call grdcrd(dlat,1,rlats,nlat,1)
          call grdcrd(dlon,1,rlons,nlon,1)
       endif

       ndata    = min(ndata+1,maxobs)
       nodata    = min(nodata+1,maxobs)

       
       if (ref_obs) then
        cdata_all(1,ndata) = ref_error      ! gps ref obs error (units of N)
        cdata_all(4,ndata) = height         ! geometric height above geoid (m)
        cdata_all(5,ndata) = ref            ! refractivity obs (units of N)
!       cdata_all(9,ndata) = ref_pccf       ! per cent confidence
       else
        cdata_all(1,ndata) = bend_error     ! gps bending error (radians)
        cdata_all(4,ndata) = impact         ! impact parameter (m)
        cdata_all(5,ndata) = bend           ! bending angle obs (radians)
!       cdata_all(9,ndata) = bend_pccf      ! per cent confidence (%)
       endif
       cdata_all(9,ndata) = pcc             ! profile per cent confidence (0 or 100)
       cdata_all(2,ndata) = dlon            ! grid relative longitude
       cdata_all(3,ndata) = dlat            ! grid relative latitude
       cdata_all(6,ndata) = timeo           ! time relative to analysis (hour) 
       cdata_all(7,ndata) = ikx             ! type assigned to ref data
       cdata_all(8,ndata) = nmrecs          ! profile number
       cdata_all(10,ndata)= roc             ! local radius of curvature (m)
       cdata_all(11,ndata)= said            ! satellite identifier
       cdata_all(12,ndata)= ptid            ! platform transmitter id number
       cdata_all(13,ndata)= usage           ! usage parameter
       cdata_all(14,ndata)= dlon_earth*rad2deg  ! earth relative longitude (degrees)
       cdata_all(15,ndata)= dlat_earth*rad2deg  ! earth relative latitude (degrees)

    else
           notgood = notgood + 1
    end if


! End of k loop over levs
  end do

   enddo read_loop                     ! subsets
  enddo                     ! messages

! Write observation to scratch file
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon,nmrecs
  write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)
  deallocate(cdata_all)
  
! Close unit to input file
1010 continue
  call closbf(lnbufr)

  nprof_gps = nmrecs
  write(6,*)'READ_GPS:  # bad height data=', notgood
  write(6,*)'READ_GPS:  ref_obs,nprof_gps = ',ref_obs,nprof_gps

  return
end subroutine read_gps



