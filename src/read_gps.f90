subroutine read_gps(nread,ndata,nodata,infile,lunout,obstype,twind, &
             nprof_gps,sis)
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
!   2007-03-01 tremolet - measure time from beginning of assimilation window
!   2008-02-02 treadon  - sort out gpsro bufr by satellite id
!   2008-02-06 cucurull - modify to support move from DDS to GTS/NC gpsro data feed
!   2008-04-21 safford  - rm unused vars and uses
!   2008-09-25 treadon  - skip report if ref_obs=.t. but no refractivity data
!   2009-02-05 cucurull - assing instrument error (ref) to a nominal value
!   2009-04-01 cucurull - add QC for Metop/GRAS
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
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_double
  use constants, only: izero,ione,deg2rad,zero,rad2deg,r60inv
  use obsmod, only: iadate,ref_obs
  use gsi_4dvar, only: l4dvar,iwinbgn,winlen
  use convinfo, only: nconvtype,ctwind,cermax, &
        ncmiter,ncgroup,ncnumgrp,icuse,ictype,ioctype
  use gridmod, only: regional,nlon,nlat,tll2xy,rlats,rlons
  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: obstype,infile
  character(len=*),intent(in   ) :: sis
  real(r_kind)    ,intent(in   ) :: twind
  integer(i_kind) ,intent(in   ) :: lunout
  integer(i_kind) ,intent(inout) :: nread,ndata,nodata
  integer(i_kind) ,intent(inout) :: nprof_gps

! Declare local parameters  
  integer(i_kind),parameter:: maxlevs=500_i_kind
  integer(i_kind),parameter:: maxinfo=16_i_kind
  integer(i_kind),parameter:: said_unknown=401_i_kind
  real(r_kind),parameter:: r100=100.0_r_kind
  real(r_kind),parameter:: r10000=10000.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind
  real(r_kind),parameter:: r5000=5000.0_r_kind
  real(r_kind),parameter:: r25000=25000.0_r_kind
  real(r_kind),parameter:: r31000=31000.0_r_kind
  real(r_kind),parameter:: r7000=7000.0_r_kind


! Declare local variables
  logical good,outside
  
  character(10) nemo
  character(80) hdr1a
  character,dimension(8):: subset
  character(len=16),allocatable,dimension(:):: gpsro_ctype

  
  integer(i_kind) lnbufr,i,k,maxobs,ireadmg,ireadsb,said,ptid
  integer(i_kind) nmrecs
  integer(i_kind) notgood,idate
  integer(i_kind) iret,levs,levsr,mincy,minobs
  integer(i_kind) nreal,nchanl,ilat,ilon
  integer(i_kind),dimension(5):: idate5
  integer(i_kind)             :: ikx
  integer(i_kind):: ngpsro_type,ikx_unknown,igpsro_type
  integer(i_kind),parameter:: mxib=31_i_kind
  integer(i_kind) ibit(mxib),nib
  logical six


  integer(i_kind),allocatable,dimension(:):: gpsro_itype,gpsro_ikx,nmrecs_id
  
  real(r_kind) timeo,t4dv
  real(r_kind) pcc,qfro,usage,dlat,dlat_earth,dlon,dlon_earth
  real(r_kind) height,rlat,rlon,ref,bend,impact,roc,geoid,&
               bend_error,ref_error,bend_pccf,ref_pccf

  real(r_kind),allocatable,dimension(:,:):: cdata_all
 
  integer(i_kind),parameter:: n1ahdr=10_i_kind
  real(r_double),dimension(n1ahdr):: bfr1ahdr
  real(r_double),dimension(25,maxlevs):: data1b
  real(r_double),dimension(25,maxlevs):: data2a
 
  data lnbufr/10_i_kind/
  data hdr1a / 'YEAR MNTH DAYS HOUR MINU PCCF ELRC SAID PTID GEODU' / 
  data nemo /'QFRO'/
  
!***********************************************************************************

  maxobs=2e6_i_kind
  nreal=maxinfo
  nchanl=izero
  ilon=2_i_kind
  ilat=3_i_kind

  nmrecs=izero
  notgood=izero

! Check convinfo file to see requesting to process gpsro data
  ikx = izero
  do i=1,nconvtype
     if ( trim(obstype)==trim(ioctype(i))) ikx=ikx+ione
  end do

! If no data requested to be process, exit routine
  if(ikx==izero)then
     write(6,*)'READ GPS:  CONVINFO DOES NOT INCLUDE ANY ',trim(obstype),' DATA'
     return
  end if

! Allocate and load arrays to contain gpsro types.
  ngpsro_type=ikx
  allocate(gpsro_ctype(ngpsro_type), gpsro_itype(ngpsro_type), &
       gpsro_ikx(ngpsro_type),nmrecs_id(ngpsro_type))
  nmrecs_id=izero
  ikx=izero
  ikx_unknown=izero
  do i=1,nconvtype
     if ( trim(obstype)==trim(ioctype(i))) then
        ikx=ikx+ione
        gpsro_ctype(ikx)=ioctype(i)
        gpsro_itype(ikx)=ictype(i)
        gpsro_ikx(ikx)  =i
        if (ictype(i)==said_unknown) ikx_unknown=i
     endif
  end do


! Open file for input, then read bufr data
  open(lnbufr,file=infile,form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)
  call readmg(lnbufr,subset,idate,iret)
  if (iret/=izero) goto 1010

! Allocate work array to hold observations
  allocate(cdata_all(nreal,maxobs))

! Big loop over the bufr file

  do while(ireadmg(lnbufr,subset,idate)==izero)
     read_loop:  do while(ireadsb(lnbufr)==izero)

! Read/decode data in subset

! Extract header information
        call ufbint(lnbufr,bfr1ahdr,n1ahdr,ione,iret,hdr1a)
        call ufbint(lnbufr,qfro,ione,ione,iret,nemo)

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
        geoid=bfr1ahdr(10)      ! Geoid undulation
        call w3fs21(idate5,minobs)

! Locate satellite id in convinfo file
        ikx = izero
        find_loop: do i=1,ngpsro_type
           if ( (trim(obstype)==trim(gpsro_ctype(i))) .and. (said == gpsro_itype(i)) ) then
              ikx=gpsro_ikx(i)
              igpsro_type = i
              exit find_loop
           endif
        end do find_loop
        if (ikx==izero) ikx=ikx_unknown
   
! check time window in subset
        t4dv=real((minobs-iwinbgn),r_kind)*r60inv
        if (l4dvar) then
           if (t4dv<zero .OR. t4dv>winlen) then
              write(6,*)'READ_GPS:      time outside window ',&
                   t4dv,' skip this report'
              cycle read_loop
           endif
        else
           call w3fs21(iadate,mincy) ! analysis time in minutes
           timeo=real(minobs-mincy,r_kind)*r60inv
           if (abs(timeo)>ctwind(ikx) .or. abs(timeo) > twind) then
              write(6,*)'READ_GPS:      time outside window ',&
                   timeo,' skip this report'
              cycle read_loop
           endif
        endif
 
! Check profile quality flags
        if ( (said > gpsro_itype(5)).and.(said < gpsro_itype(12)) ) then  !Cosmic
           if(pcc==zero) then
              write(6,*)'READ_GPS:  **WARNING** bad COSMIC SAID=',said,'PTID=',ptid,'profile',&
                  ' SKIP this report'
              cycle read_loop
           endif
        endif

        if (said == gpsro_itype(2)) then ! Gras
           call upftbv(lnbufr,nemo,qfro,mxib,ibit,nib)
           six = .false.
           if(nib > izero) then
              do i=1,nib
                 if(ibit(i)== 6_i_kind) then
                    six = .true.
                    exit
                 endif
              enddo
           endif

           if(six) then
              write(6,*)'READ_GPS:  **WARNING** bad GRAS SAID=',said,'PTID=',ptid,'profile',&
                   ' SKIP this report'
              cycle read_loop
           endif
        endif

!  Check we have the same number of levels for ref and bending angle
!  when ref_obs on
        call ufbseq(lnbufr,data1b,25_i_kind,maxlevs,levs,'ROSEQ1')  ! bending angle
        call ufbseq(lnbufr,data2a,25_i_kind,maxlevs,levsr,'ROSEQ3') ! refractivity
        if ((ref_obs).and.(levs/=levsr)) then
           write(6,*) 'READ_GPS:  **WARNING** said,ptid=',said,ptid,&
                ' with gps_bnd levs=',levs,&
                ' and gps_ref levsr=',levsr,&
                ' SKIP this report'
           cycle read_loop
        endif

!  Increment report counters
        nmrecs = nmrecs + ione      ! count reports in bufr file
        nmrecs_id(igpsro_type) = nmrecs_id(igpsro_type) + ione

!  Set usage flag
        usage = zero
        if(icuse(ikx) < izero)usage=r100
        if(ncnumgrp(ikx) > izero )then                     ! cross validation on
           if(mod(nmrecs,ncnumgrp(ikx))== ncgroup(ikx)-ione)usage=ncmiter(ikx)
        end if

!  Loop over levs in profile
        do k=1, levs
           nread=nread+ione  ! count observations
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

! Preliminary (sanity) QC checks for bad and missing data
           good=.true.
           if((abs(rlat)>90._r_kind).or.(abs(rlon)>r360).or.(height<=zero)) then
              good=.false.
           endif
           if (ref_obs) then
              if ((ref>=1.e+9_r_kind).or.(ref<=zero).or.(height>=1.e+9_r_kind)) then
                 good=.false.
              endif
           else
              if ((bend>=1.e+9_r_kind).or.(bend<zero).or.(impact>=1.e+9_r_kind).or.(impact<roc)) then
                 good=.false.
              endif
           endif

! If observation is "good" load into output array
           if(good) then

! Assign preliminary errors

              if(ref_obs) then
                 ref_error = ref*0.01_r_kind
              else                      ! bending angle
                 if((impact-roc) <= r10000) then
                    bend_error=(-bend*0.09_r_kind/r10000)*(impact-roc)+bend*1.e-1_r_kind
                 else
                    bend_error=max(7.e-6_r_kind,bend*1.e-2_r_kind)
                 endif
              endif

              if (rlon==r360)  rlon=zero
              if (rlon<zero  ) rlon=rlon+r360

              dlat_earth = rlat * deg2rad  !convert to radians
              dlon_earth = rlon * deg2rad
 
              if(regional)then
                 call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
                 if (outside) cycle read_loop
              else
                 dlat = dlat_earth
                 dlon = dlon_earth
                 call grdcrd(dlat,ione,rlats,nlat,ione)
                 call grdcrd(dlon,ione,rlons,nlon,ione)
              endif

              ndata  = min(ndata +ione,maxobs)
              nodata = min(nodata+ione,maxobs)
 
       
              if (ref_obs) then
                 cdata_all(1,ndata) = ref_error      ! gps ref obs error (units of N)
                 cdata_all(4,ndata) = height         ! geometric height above geoid (m)
                 cdata_all(5,ndata) = ref            ! refractivity obs (units of N)
!                cdata_all(9,ndata) = ref_pccf       ! per cent confidence
              else
                 cdata_all(1,ndata) = bend_error     ! gps bending error (radians)
                 cdata_all(4,ndata) = impact         ! impact parameter (m)
                 cdata_all(5,ndata) = bend           ! bending angle obs (radians)
!                cdata_all(9,ndata) = bend_pccf      ! per cent confidence (%)
              endif
              cdata_all(9,ndata) = pcc             ! profile per cent confidence (0 or 100)
              cdata_all(2,ndata) = dlon            ! grid relative longitude
              cdata_all(3,ndata) = dlat            ! grid relative latitude
              cdata_all(6,ndata) = t4dv            ! time relative to analysis (hour) 
              cdata_all(7,ndata) = ikx             ! type assigned to ref data
              cdata_all(8,ndata) = nmrecs          ! profile number
              cdata_all(10,ndata)= roc             ! local radius of curvature (m)
              cdata_all(11,ndata)= said            ! satellite identifier
              cdata_all(12,ndata)= ptid            ! platform transmitter id number
              cdata_all(13,ndata)= usage           ! usage parameter
              cdata_all(14,ndata)= dlon_earth*rad2deg  ! earth relative longitude (degrees)
              cdata_all(15,ndata)= dlat_earth*rad2deg  ! earth relative latitude (degrees)
              cdata_all(16,ndata)= geoid           ! geoid undulation (m)
 
           else
              notgood = notgood + ione
           end if


! End of k loop over levs
        end do

     enddo read_loop        ! subsets
  enddo                     ! messages

! Write observation to scratch file
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon,nmrecs
  write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)
  deallocate(cdata_all)
  
! Close unit to input file
1010 continue
  call closbf(lnbufr)

  nprof_gps = nmrecs
  write(6,*)'READ_GPS:  # bad or missing data=', notgood
  do i=1,ngpsro_type
     if (nmrecs_id(i)>izero) &
          write(6,1020)'READ_GPS:  LEO_id,nprof_gps = ',gpsro_itype(i),nmrecs_id(i)
  end do
  write(6,1020)'READ_GPS:  ref_obs,nprof_gps= ',ref_obs,nprof_gps
1020 format(a31,2(i6,1x))

! Deallocate arrays
  deallocate(gpsro_ctype,gpsro_itype,gpsro_ikx,nmrecs_id)

  return
end subroutine read_gps



