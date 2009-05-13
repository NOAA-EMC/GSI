module radinfo 
!$$$   module documentation block
!                .      .    .                                       .
! module:    radinfo   
!
! abstract:  This module contains variables and routines related
!            to information for the use of satellite radiance data.
!
! program history log:
!   1995-07-06  derber
!   2004-05-13  kleist, documentation
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2004-11-23  treadon - change 110 format statement
!   2004-11-30  xu li   - add array fbias for AVHRR bias correction
!   2004-12-08  xu li   - add logical flag retrieval to module
!   2004-12-22  treadon - rename logical "idiag_rad" as "diag_rad"
!   2005-02-03  xu li   - add SST analysis read and move sub intgrid2 from sst_retrieval to this module
!   2005-03-25  xu li   - modify sub rdgrbsst and remove intgrid2
!   2005-04-18  treadon - make rdgrbsst a separate subroutine
!   2005-09-28  derber  - change radinfo input file add ermax_rad,b_rad and pg_rad
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-04-27  derber  - remove jppf
!   2008-04-23  safford - add standard documentation block
!
! subroutines included:
!   init_rad            - set satellite related variables to defaults
!   init_rad_vars       - initialize satellite related variables
!   radinfo_read        - read in sat info and biases, including read 
!                          sst_an and avhrr bias correction
!   radinfo_write       - write out satellite biases
!
! functions included:
!   newchn
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block


! !USES:

  use kinds, only: r_kind,i_kind
  implicit none

  integer(i_kind),parameter:: numt = 33   ! size of AVHRR bias correction file

  logical diag_rad   ! logical to turn off or on the diagnostic radiance file (true=on)
  logical retrieval  ! logical to turn off or on the SST retrieval with AVHRR data

  integer(i_kind) jpch_rad   ! number of channels*sat
  integer(i_kind) npred      ! number of radiance biases predictors
  integer(i_kind) mype_rad   ! task id for writing out radiance diagnostics
  integer(i_kind) npred1     ! number of radiance biases predictors minus one

  real(r_kind),allocatable,dimension(:):: varch       ! variance for each satellite channel
  real(r_kind),allocatable,dimension(:):: ermax_rad   ! error maximum (qc)
  real(r_kind),allocatable,dimension(:):: b_rad       ! variational b value
  real(r_kind),allocatable,dimension(:):: pg_rad      ! variational pg value
  real(r_kind),allocatable,dimension(:):: tlapmean    ! mean lapse rate (fixed from input file)
  real(r_kind),allocatable,dimension(:,:):: fbias     ! bias for AVHRR siumulated radiance
  real(r_kind),allocatable,dimension(:,:):: cbias     ! angle dependent bias for satellite channels
  real(r_kind),allocatable,dimension(:,:):: predx     ! coefficients for predictor part of bias correction

  integer(i_kind),allocatable,dimension(:):: nuchan    ! satellite channel
  integer(i_kind),allocatable,dimension(:):: iuse_rad  ! use to turn off satellite radiance data
  integer(i_kind),allocatable,dimension(:):: ifactq    ! scaling parameter for d(Tb)/dq sensitivity

  character(len=20),allocatable,dimension(:):: nusis   ! sensor/instrument/satellite indicator
  character(len=256),save:: crtm_coeffs_path = "./" ! path of CRTM_Coeffs files


contains


  subroutine init_rad
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    int_rad
!
!   prgrmmr:     derber      org: np23                date: 1995-07-06
!
! abstract:  This routine sets default values for variables used in
!            the radiance processing routines.
!
! program history log:
!   1995-07-06  derber
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2008-04-23  safford -- add standard subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

    implicit none

    jpch_rad = 0         ! total number of channels over all instruments & satellites
    retrieval = .false.  ! .true. = apply physical SST retrieval with AVHRR data
    diag_rad = .true.    ! .true.=generate radiance diagnostic file
    mype_rad = 0         ! mpi task to collect and print radiance use information on/from
    npred=5              ! number of bias correction predictors
  end subroutine init_rad


  subroutine init_rad_vars
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_rad_vars
!
!   prgrmmr:     derber      org: np23                date: 1995-07-06
!
! abstract:  This routine sets parameters used in the radiance 
!            assimilation.  The parameters below depend on values
!            which may be altered by the SETUP namelist.
!
! program history log:
!   1995-07-06  derber
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2008-04-23  safford -- add standard subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

    implicit none

    npred1=npred-1
    
    return
  end subroutine init_rad_vars


  subroutine radinfo_read
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radinfo_read
!
!   prgrmmr:     yang        org: np20                date: 1998-05-15
!
! abstract:  This routine reads the satinfo, satbias\_angle, and
!            satbias files.  
!
!            The satinfo file contains information about the channels,
!            sensors, and satellites.  It specifies observation error
!            for the channels, how to use the channels (assimilate,
!            monitor, etc), the type of channel (ir or microwave),
!            and other useful information.  
!
!            The satbias\_angle file contains the angle dependent part
!            of the brightness temperature bias for each channel/
!            instrument/satellite.  Also included in this file is 
!            the mean temperature lapse rate for each channels 
!            weighted by the weighting function for the given channel/
!            instrument.
!
!            The satbias\_in file contains the coefficients for the
!            predictive part of the bias correction.
!
!
! program history log:
!   1998-05-15  yang, weiyu
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2004-11-30  xu li- read SST dependent bias for AVHRR radiance (NOAA-16 & NOAA-17) 
!                      and SST analysis when retrieval = .true.
!   2005-02-08  xu li- read SST analysis when retrieval = .true.
!   2005-10-11  treadon - change satinfo read to free format
!   2005-11-30  li - fix a bug in the format to read avhrr SST dependent BC
!   2007-03-13  derber  - modify to allow input bias correction files of different lengths and orders
!   2007-06-29  treadon - determine/build n_sensors and sensorlist from satinfo file
!   2008-04-23  safford - add standard doc block, rm unused vars and uses
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:

    use obsmod, only: iout_rad
    use constants, only: zero
    use mpimod, only: mype
    implicit none

! !INPUT PARAMETERS:


    integer(i_kind) i,j,k,ich,lunin,nlines
    integer(i_kind) ip,istat,n,ichan
    real(r_kind),dimension(npred):: predr
    real(r_kind) tlapm
    real(r_kind),dimension(90)::cbiasx
    character(len=1):: cflg
    character(len=120) crecord
    character(len=20) :: isis 
    character(len=20),allocatable,dimension(:):: satsenlist
    real(r_kind),dimension(numt):: fbiasx     ! contains SST dependent bias  for SST retrieval
    logical,allocatable,dimension(:):: nfound
    logical cfound

    data lunin / 49 /

!============================================================================

!   Determine number of entries in satellite information file
    open(lunin,file='satinfo',form='formatted')
    j=0
    nlines=0
    read1:  do
       read(lunin,100,iostat=istat) cflg,crecord
       if (istat /= 0) exit
       nlines=nlines+1
       if (cflg == '!') cycle
       j=j+1
    end do read1
    if (istat>0) then
       close(lunin)
       write(6,*)'RADINFO_READ:  ***ERROR*** error reading radinfo, istat=',istat
       write(6,*)'RADINFO_READ:  stop program execution'
       call stop2(79)
    endif
    jpch_rad = j


!   Allocate arrays to hold radiance information
!     nuchan    - channel number
!     nusis     - sensor/instrument/satellite
!     iuse_rad  - use parameter
!     ifactq    - scaling parameter for d(Tb)/dq sensitivity
!     varch     - variance for each channel

    allocate(nuchan(jpch_rad),nusis(jpch_rad),&
         iuse_rad(0:jpch_rad),ifactq(jpch_rad),varch(jpch_rad),&
         ermax_rad(jpch_rad),b_rad(jpch_rad),pg_rad(jpch_rad))
    allocate(satsenlist(jpch_rad),nfound(jpch_rad))
    iuse_rad(0)=-999
    ifactq=0


!   All mpi tasks open and read radiance information file.
!   Task mype_rad writes information to radiance runtime file

    if (mype==mype_rad) then
       open(iout_rad)
       write(iout_rad,*)'RADINFO_READ:  jpch_rad=',jpch_rad
    endif
    rewind(lunin)
    j=0
    do k=1,nlines
       read(lunin,100) cflg,crecord
       if (cflg == '!') cycle
       j=j+1
       read(crecord,*) nusis(j),nuchan(j),iuse_rad(j),&
            varch(j),ermax_rad(j),b_rad(j),pg_rad(j)
       if (mype==mype_rad) write(iout_rad,110) j,nusis(j), &
            nuchan(j),varch(j),iuse_rad(j),ermax_rad(j), &
            b_rad(j),pg_rad(j)
    end do
    close(lunin)
100 format(a1,a120)
110   format(i4,1x,a20,' chan= ',i4,  &
            ' var= ',f7.3,' use= ',i2,' ermax= ',F7.3, &
            ' b_rad= ',F7.2,' pg_rad=',F7.2)


!   Allocate arrays to receive angle dependent bias information.
!   Open file to bias file (satang=satbias_angle).  Read data.

    allocate(cbias(90,jpch_rad),tlapmean(jpch_rad))
    cbias=zero
    tlapmean=zero

    open(lunin,file='satbias_angle',form='formatted')
    nfound = .false.
    read2: do
       read(lunin,'(I5,1x,A20,2x,I4,e15.6/9(4x,10f7.3/))',iostat=istat) &
            ich,isis,ichan,tlapm,(cbiasx(ip),ip=1,90)
       if (istat /= 0) exit
       cfound = .false.
       do j =1,jpch_rad
          if(trim(isis) == trim(nusis(j)) .and. ichan == nuchan(j))then
             cfound = .true.
             nfound(j) = .true.
             do i=1,90
                cbias(i,j)=cbiasx(i)
             end do
             tlapmean(j)=tlapm
          end if
       end do
       if(.not. cfound .and. mype == 0) &
            write(6,*) '***WARNING instrument/channel ',isis,ichan, &
            'found in satbias_angle file but not found in satinfo'
    end do read2
    close(lunin)
    if (istat>0) then
       write(6,*)'RADINFO_READ:  ***ERROR*** error reading satbias_angle, istat=',istat
       write(6,*)'RADINFO_READ:  stop program execution'
       call stop2(79)
    endif

    if (mype==mype_rad) then
       write(iout_rad,*)'RADINFO_READ:  read satbias_angle file'
       do j=1,jpch_rad
          if(.not. nfound(j))write(iout_rad,*) 'RADINFO_READ: ***WARNING instrument/channel ',&
               nusis(j),nuchan(j),' not found in satbias_angle file - set to zero '
       end do
    end if

    if ( .not. retrieval ) then

!   Allocate array to hold coefficients for predictive (air mass) part of 
!   bias correction.  Open unit to input file.  Read data.
    allocate(predx(npred,jpch_rad))
    do j=1,jpch_rad
       do i=1,npred
          predx(i,j)=zero
       end do
    end do

    open(lunin,file='satbias_in' ,form='formatted')
    nfound = .false.
    read3: do
       read(lunin,'(I5,1x,A20,1x,I5,10f12.6)',iostat=istat) ich,isis,&
            ichan,(predr(ip),ip=1,npred)
       if (istat /= 0) exit
       cfound = .false.
       do j =1,jpch_rad
          if(trim(isis) == trim(nusis(j)) .and. ichan == nuchan(j))then
             cfound = .true.
             nfound(j) = .true.
             do i=1,npred
                predx(i,j)=predr(i)
             end do
          end if
       end do
       if(mype == 0 .and. .not. cfound) &
            write(6,*) '***WARNING instrument/channel ',isis,ichan, &
            'found in satbias_in file but not found in satinfo'
    end do read3
    close(lunin)
    if (istat>0) then
       write(6,*)'RADINFO_READ:  ***ERROR*** error reading satbias_in, istat=',istat
       write(6,*)'RADINFO_READ:  stop program execution'
       call stop2(79)
    endif

    if (mype==mype_rad) then
       write(iout_rad,*)'RADINFO_READ:  guess air mass bias correction coefficients below'
       do j=1,jpch_rad
          if (nfound(j)) then
             write(iout_rad,140) j,trim(nusis(j)),(predx(n,j),n=1,npred)
          else
             write(iout_rad,*) '***WARNING instrument/channel ',&
             nusis(j),nuchan(j),' not found in satbias_in file - set to zero '
          endif
      end do
140    format(i4,1x,a20,10f12.6)
    endif

    endif

! Read SST dependent radiance bias correction lookup table
   if (retrieval) then
      
       allocate(fbias(numt,jpch_rad))
       fbias=zero
       
       if(mype==mype_rad) write(iout_rad,*) &
            'RADINFO_READ:  read SST & D/N dependent bias correction from ',lunin
       open(lunin,file='satbias_sst',form='formatted')
       rewind (lunin)

!      Loop over satellites sensors & channels
       read4: do
           read(lunin,'(I5,1x,a20,1x,I5/3(4x,11f10.3/) )',iostat=istat) ich,isis,ichan,(fbiasx(i),i=1,numt)
           if (istat /= 0) exit
           cfound = .false.
           do j=1,jpch_rad
             if(trim(isis) == trim(nusis(j)) .and. ichan == nuchan(j))then
                cfound = .true.
                do i=1,numt
                   fbias(i,j)=fbiasx(i)
                end do
                tlapmean(j)=tlapm
             end if
           end do
           if(.not. cfound)write(6,*) ' WARNING instrument/channel ',isis,ichan, &
              'found in satbias_sst file and not found in satinfo'
      end do read4
      close(lunin)
    endif           ! endif for if (retrieval) then


!   Close unit for runtime output.  Return to calling routine
    if(mype==mype_rad)close(iout_rad)
    return

  end subroutine radinfo_read


  subroutine radinfo_write
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radinfo_write
!
!   prgrmmr:     yang        org: np20                date: 1998-05-15
!
! abstract:  This routine writes an updated version of the predictive
!            part of the bias correction.
!
! program history log:
!   1998-05-15  yang, weiyu
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2008-04-23  safford - add standard subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:

    implicit none

    integer(i_kind) lunout,jch,ip
    data lunout / 51 /


!   Open unit to output file.  Write updated coefficients.  Close unit.
    open(lunout,file='satbias_out',form='formatted')
    rewind lunout
    do jch=1,jpch_rad
       write(lunout,'(I5,1x,a20,1x,i5,10f12.6)') jch,nusis(jch),nuchan(jch),&
            (predx(ip,jch),ip=1,npred)
    end do
    close(lunout)

!   Deallocate data arrays for bias correction and those which hold
!   information from satinfo file.
    deallocate (predx,cbias,tlapmean,nuchan,nusis,iuse_rad, &
         ifactq,varch)
    return
  end subroutine radinfo_write



  integer(i_kind) function newchn(sis,ichan)   ! "satinfo-relative" index of 
                                               ! (sis,ichan) combination
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    function newchn
!
!   prgrmmr:     derber      org: np23                date: 1997-08-13
!
! abstract:  For a given satellite and channel produce a combined 
!            channel number based on input from the satinfo file.
!            If the requested channel/satellite combination is
!            not found, the function returns a zero value.
!
! program history log:
!   1997-08-13  derber
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2008-04-23  safford - add standard subprogram doc block, rm unused uses
!
!   input argument list:
!     sis     - satellite to search for
!     ichan   - channel number to search for
!
!   return:
!             - combined channel number
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:
 
    implicit none

! !INPUT PARAMETERS:

    character(len=20), intent(in)::  sis   ! satellite to search for
    integer(i_kind), intent(in)::  ichan   ! channel number to search for


    integer(i_kind) j
    do j=1,jpch_rad
       if ( nuchan(j)==ichan .and. nusis(j)==sis) then
          newchn=j
          return
       end if
    end do
    write(6,*) 'NEWCHN:  channel=',ichan,' sensor/instrument/satellite=',sis, &
         ' not present in satinfo file'
    newchn=0
    return
  end function newchn
  
end module radinfo
