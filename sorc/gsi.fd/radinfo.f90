!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  radinfo --- Provide information on satellite radiance
!
! !INTERFACE:
!
module radinfo 

! !USES:

  use kinds, only:r_kind,i_kind
  implicit none

! !DESCRIPTION:  This module contains variables and routines related
!            to information for the use of satellite radiance 
!            data.
!
! !REVISION HISTORY:
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
!   2006-04-27  derber - remove jppf
!
! !CALLING SEQUENCE:
!   sub init_rad       - set satellite related variables to defaults
!   sub init_rad_vars  - initialize satellite related variables
!   sub radinfo_read   - read in sat info and biases, including read sst_an and avhrr bias correction
!   sub radinfo_write  - write out satellite biases
!
! !PUBLIC MEMBER FUNCTIONS:
!   newchn
!
! !REMARKS:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!   derber           org: np23                date: 1995-07-06
!
!EOP
!-------------------------------------------------------------------------

  integer(i_kind),parameter:: numt = 33   ! size of AVHRR bias correction file

  logical diag_rad   ! logical to turn off or on the diagnostic radiance file (true=on)
  logical retrieval  ! logical to turn off or on the SST retrieval with AVHRR data

  integer(i_kind) jpch       ! number of channels*sat
  integer(i_kind) jpchread   ! total number of channels read for radiances bias correction
  integer(i_kind) npred      ! number of radiance biases predictors
  integer(i_kind) mype_rad   ! task id for writing out radiance diagnostics
  integer(i_kind) npred1     ! number of radiance biases predictors minus one

  real(r_kind),allocatable,dimension(:):: varch       ! variance for each satellite channel
  real(r_kind),allocatable,dimension(:):: ermax_rad   ! error maximum (qc)
  real(r_kind),allocatable,dimension(:):: b_rad       ! variational b value
  real(r_kind),allocatable,dimension(:):: pg_rad      ! variational pg value
  real(r_kind),allocatable,dimension(:):: tlapmean    ! mean lapse rate (fixed from input file)
  real(r_kind),allocatable,dimension(:,:,:):: fbias   ! bias for AVHRR siumulated radiance
  real(r_kind),allocatable,dimension(:,:):: cbias     ! angle dependent bias for satellite channels
  real(r_kind),allocatable,dimension(:,:):: predx     ! coefficients for predictor part of bias correction

  integer(i_kind),allocatable,dimension(:):: nuchan    ! satellite channel
  integer(i_kind),allocatable,dimension(:):: iuse_rad  ! use to turn off satellite radiance data
  integer(i_kind),allocatable,dimension(:):: ifactq    ! scaling parameter for d(Tb)/dq sensitivity

  character(len=20),allocatable,dimension(:):: nusis   ! sensor/instrument/satellite indicator


contains


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_rad --- Initialize parameters for radiance data
!
! !INTERFACE:
!
  subroutine init_rad

! !USES:

! !DESCRIPTION:  This routine sets default values for variables used in
!            the radiance processing routines
!
! !REVISION HISTORY:
!   1995-07-06  derber
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:    
!   derber      org: np23                date: 1995-07-06
!
!EOP
!-------------------------------------------------------------------------
    implicit none

    jpch = 142           ! total number of channels over all instruments & satellites
    jpchread = -10       ! number of lines to read from satinfo file
    retrieval = .false.  ! .true. = apply physical SST retrieval with AVHRR data
    diag_rad = .true.    ! .true.=generate radiance diagnostic file
    mype_rad = 0         ! mpi task to collect and print radiance use information on/from
    npred=5              ! number of bias correction predictors
  end subroutine init_rad


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_rad_vars --- Set radiance parameters
!
! !INTERFACE:
!
  subroutine init_rad_vars

! !USES:

    implicit none

! !DESCRIPTION:  This routine sets parameters used in the radiance 
!            assimilation.  The parameters below depend on values
!            which may be altered by the SETUP namelist.
!
! !REVISION HISTORY:
!   1995-07-06  derber
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:    
!   derber      org: np23                date: 1995-07-06
!
!EOP
!-------------------------------------------------------------------------
    
    if(jpchread <=0)jpchread=jpch
    npred1=npred-1
    
    return
  end subroutine init_rad_vars


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: radinfo_read --- Read satinfo,satang,satbias
!
! !INTERFACE:
!
  subroutine radinfo_read(mype)

! !USES:

    use kinds, only: r_kind,i_kind
    use obsmod, only: iout_rad
    use constants, only: zero
    implicit none

! !INPUT PARAMETERS:

    integer(i_kind), intent(in)::  mype   ! mpi task id

! !DESCRIPTION:  This routine reads the satinfo, satbias\_angle, and
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
! !REVISION HISTORY:
!   1998-05-15  yang, weiyu
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!   2004-11-30  xu li- read SST dependent bias for AVHRR radiance (NOAA-16 & NOAA-17) 
!                      and SST analysis when retrieval = .true.
!   2005-02-08  xu li- read SST analysis when retrieval = .true.
!   2005-10-11  treadon - change satinfo read to free format
!   2005-11-30  li - fix a bug in the format to read avhrr SST dependent BC
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:    
!   yang        org: np20                date: 1998-05-15
!
!EOP
!-------------------------------------------------------------------------
    
    integer(i_kind) i,j,ich,lunin
    integer(i_kind) jch,ip,n,ichan
    real(r_kind),dimension(npred):: predr
    real(r_kind) wavelength,fact,tlapm
    real(r_kind),dimension(90)::cbiasx
    character(len=80) :: file_sst             ! file name of SST analysis
    character(len=20) :: isis 
    real(r_kind),dimension(numt,2):: fbiasx   ! contains AVHRR bias for 
                                              ! channel 3,4,5; NOAA-16 & 17

    data lunin / 49 /

!============================================================================
!   Open unit to receive runtime informational output
    if(mype==mype_rad)open(iout_rad)

!   Allocate arrays. Open unit to satinfo file.  Read data.
!     nuchan    - channel number
!     nusis     - sensor/instrument/satellite
!     iuse_rad  - use parameter
!     ifactq    - scaling parameter for d(Tb)/dq sensitivity
!     varch     - variance for each channel

    allocate(nuchan(jpch),nusis(jpch),&
         iuse_rad(0:jpch),ifactq(jpch),varch(jpch),&
         ermax_rad(jpch),b_rad(jpch),pg_rad(jpch))
    iuse_rad(0)=-999
    ifactq=0

    lunin = 49
    open(lunin,file='satinfo',form='formatted')
    do j=1,jpch
       read(lunin,*,err=150,end=160) nusis(j), &
            nuchan(j),iuse_rad(j),varch(j),ermax_rad(j), &
            b_rad(j),pg_rad(j)
    end do
    close(lunin)

!   Echo satinfo file to radiance output unit, iout_rad
    if (mype==mype_rad) then
       do j=1,jpch
          write(iout_rad,110) j,nusis(j),nuchan(j),&
               varch(j),iuse_rad(j),ermax_rad(j),b_rad(j),pg_rad(j)
       end do
110   format(i3,1x,a20,' chan= ',i4,  &
            ' var= ',f7.3,' use= ',i2,' ermax= ',F7.3, &
            ' b_rad= ',F7.2,' pg_rad=',F7.2)
    endif



!   Allocate arrays to receive angle dependent bias information.
!   Open file to bias file (satang=satbias_angle).  Read data.
    allocate(cbias(90,jpch),tlapmean(jpch))
    cbias=zero
    tlapmean=zero

    if (mype==mype_rad) write(iout_rad,*) &
         'RADINFO_READ:  read angle dependent bias correction from ',lunin

    open(lunin,file='satbias_angle',form='formatted')
    do jch=1,jpchread
       read(lunin,'(I5,1x,A20,1x,I5,e15.6/9(4x,10f7.3/))',end=120) &
            ich,isis,ichan,tlapm,(cbiasx(ip),ip=1,90)
       if(ich > 0)then
          do i=1,90
             cbias(i,ich)=cbiasx(i)
          end do
          tlapmean(ich)=tlapm
       else
          if(mype == 0) write(6,*) &
               'RADINFO_READ:  bias correction channel not used ',isis,ichan
       end if
    end do
120 continue
    close(lunin)


! Read SST analysis and SST dependent radiance bias correction for AVHRR
   if (retrieval) then
      
       allocate(fbias(numt,jpch,2))
       fbias=zero
       
       if(mype==mype_rad) write(iout_rad,*) &
            'RADINFO_READ:  read SST & D/N dependent bias correction from ',lunin
       open(lunin,file='satbias_sst',form='formatted')
       rewind (lunin)

!      Loop over 2 satellites (NOAA-17 and -18 AVHRR)       
       do j = 1, 2
          do jch=1,3
           read(lunin,'(I5,1x,A20,1x,I5,e15.6/3(4x,10f9.3/),4x,3f9.3/ )') &
                  ich,isis,ichan,tlapm,(fbiasx(i,j),i=1,numt)
             if(ich > 0)then
                do i=1,numt
                   fbias(i,ich,j)=fbiasx(i,j)
                end do
                tlapmean(ich)=tlapm
             else
                if(mype == 0)write(6,*)'RADINFO_READ:  sst dependent bias correction ',&
                     'channel not used ',isis,ichan
             end if
          end do    ! end loop over channels
       enddo        ! end loop over satellites
       close(lunin)
    endif           ! endif for if (retrieval) then


!   Allocate array to hold coefficients for predictive part of bias correction.
!   Open unit to input file.  Read data.
    allocate(predx(jpch,npred))
    do j=1,npred
       do i=1,jpch
          predx(i,j)=zero
       end do
    end do

    open(lunin,file='satbias_in' ,form='formatted')
    if(mype==mype_rad) &
         write(iout_rad,*)'RADINFO_READ:  read satbias coefs from lunin=',lunin,&
         ' with npred=',npred 

    do jch=1,jpchread
       read(lunin,'(I5,1x,A20,1x,I5,10f12.6)',end=140) ich,isis,ichan,(predr(ip),ip=1,npred)
       if(ich > 0)then
          do i=1,npred
             predx(ich,i)=predr(i)
          end do
       else
          if(mype == 0) write(6,*) &
               'RADINFO_READ:  time dependent bias coefficients not used for',&
               isis,ichan
       end if
       if(mype==mype_rad)write(iout_rad,130) jch,(predx(jch,n),n=1,npred)
    end do
130 format(1x,'jch=',i3,10f12.6)
140 continue
    close(lunin)


!   Close unit for runtime output.  Return to calling routine
    if(mype==mype_rad)close(iout_rad)
    return


!   Error condtion branches below

!   Error reading satinfo file
150 continue
    write(6,*)'RADINFO_READ:  ***ERROR*** error reading satinfo ',mype
    go to 170

!   Premature end of file on satinfo file
160 continue
    write(6,*)'RADINFO_READ:  ***ERROR*** fewer coefficients than jpch=',jpch,' in satinfo '

170 continue
    close(lunin)
    write(6,*)'RADINFO_READ:  stop program execution'
    call stop2(79)

    return
  end subroutine radinfo_read


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: radinfo_write --- Write satbias_out file
!
! !INTERFACE:
!
  subroutine radinfo_write

! !USES:

    use obsmod, only: iout_rad
    implicit none

! !DESCRIPTION:  This routine writes an updated version of the predictive
!            part of the bias correction.
!
! !REVISION HISTORY:
!   1998-05-15  yang, weiyu
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:    
!   yang        org: np20                date: 1998-05-15
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) lunout,jch,ip
    data lunout / 51 /


!   Open unit to output file.  Write updated coefficients.  Close unit.
    open(lunout,file='satbias_out',form='formatted')
    rewind lunout
    do jch=1,jpch
       write(lunout,'(I5,1x,a20,1x,i5,10f12.6)') jch,nusis(jch),nuchan(jch),&
            (predx(jch,ip),ip=1,npred)
    end do
    close(lunout)

!   Deallocate data arrays for bias correction and those which hold
!   information from satinfo file.
    deallocate (predx,cbias,tlapmean,nuchan,nusis,iuse_rad, &
         ifactq,varch)
    return
  end subroutine radinfo_write

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: newchn --- Locate channel/satellite
!
! !INTERFACE:
!
  integer(i_kind) function newchn(sis,ichan)   ! "satinfo-relative" index of 
                                               ! (sis,ichan) combination

! !USES:
 
    use kinds, only: r_kind,i_kind
    implicit none

! !INPUT PARAMETERS:

    character(len=20), intent(in)::  sis   ! satellite to search for
    integer(i_kind), intent(in)::  ichan   ! channel number to search for

! !DESCRIPTION:  For a given satellite and channel produce a combined 
!            channel number based on input from the satinfo file.
!            If the requested channel/satellite combination is
!            not found, the function returns a zero value.
!    
! !REVISION HISTORY:
!   1997-08-13  derber
!   2004-06-22  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:    
!   derber      org: np23                date: 1997-08-13
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) j
    do j=1,jpch
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
