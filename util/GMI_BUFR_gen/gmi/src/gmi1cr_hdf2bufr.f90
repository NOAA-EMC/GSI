!###########################################################################
! Program history log:
! 2014-02-24  J.Jin      -- READ GMI 1B/1C proxy data in HDF5 format
! 2014-06-10  ejones     -- Read GMI 1CR data in HDF5 format
! 2014-06-23  J.Jin      -- Reorganize the GMI HDF reading subroutine.
!                           The new code also reads level 1B data, which requires
!                           GMI 1B HDF files are in the same directory as 1C-R 
!                           files are. 
! 2014-07-11  J.Jin      -- read all geo-information about angles for both swaths
!                           and save them in 2-element arrays
! 10/10/2014  Yelena    Add 'need_date,need_syn'  as Input --> to write
!                       Bufr  only for needed date,synoptic
!                          
! 2014-11-06  J.Jin      -- Revise version number. For example, V03A was saved as V3.
!                           Now it is saved as V3.01. The bufr tabl is also revised. 
! 2014-11-08  J.Jin      -- Check scan number in HDF files. Stop if it is zero which
!                           means no data are actually stored in the HDF file. 

!
! 2015-08-20  J.Jin**    -- Merges Erin Jones's code read and write RFIflag.

! 2016-03-11  J.Jin      -- Read GMI V04 data which is based XCAL2015.
! 2016-03-15  J.Jin      -- Enable it to read data with general 1C-R name format 
!                           "1C-R.GPM.GMI.XCALyyyy...".


!
! Usage:
!
!   Input files: inputFileName
!        
!   Output files:
!
!   Subprograms called:
!     Library:
!       BUFRLIB   - OPENBF  CLOSBF   OPENMB    WRITSB  UFBSEQ
!       HDF5      - h5open_f  h5close_f      h5fopen_f     h5fclose_f
!                   h5gopen_f h5gclose_f
!                   h5aopen_f h5aget_space_f h5aget_info_f h5aget_type_f
!                   h5aread_f h5aclose_f
!                   h5dopen_f h5dread_f      h5dclose_f
!
!###########################################################################

PROGRAM ONECR_HDF_to_BUFR
   USE HDF5 
   USE hdf5rd_mod, only: hdf5rd_i, hdf5rd_f
!---------------------------------------------------------------------------
  IMPLICIT NONE

  CHARACTER (LEN = 80)          :: inputBuffer,prefix_sub,argv
  CHARACTER (LEN = 80)          :: inputFileName, groupname, subgroupname, swathheader,&
                                    dsetname, timename, SCstatname, qname,tmbrname
  CHARACTER (LEN = 80)          :: outputPrefix 
  CHARACTER (LEN = 80)          :: inputFileName1b
! CHARACTER (LEN = 80), PARAMETER  :: hdf1cr_prefix='1C-R.GPM.GMI.XCAL2014-N',  hdf1b_prefix='1B.GPM.GMI.TB2014'
#  JJJ
  CHARACTER (LEN = 80), PARAMETER  :: hdf1cr_prefix='1C-R.GPM.GMI.XCAL',  hdf1b_prefix='1B.GPM.GMI.TB'

  CHARACTER (LEN = 80), PARAMETER  :: prefix='gmi_L1CR.'
#  JJJ
  CHARACTER (LEN = 4)              :: xcalyear
  CHARACTER (LEN = 4),  PARAMETER  :: xcal='XCAL'


  INTEGER(HID_T)                 :: file_id, group_id, dset_id, attr_id, space_id, memtype_id, type_id, error, subgroup_id, subdset_id
  INTEGER(HSIZE_T),DIMENSION(:), allocatable :: data_dims
  INTEGER(HSIZE_T),DIMENSION(1) :: attr_dims
  CHARACTER (LEN=1000)           :: attr_data

  INTEGER                       :: ng, nscan, npixel, nchanl, nmd, nmds, ntd, nsc, nqd

!  11/02/2015   Added JJJ
  INTEGER                       :: nfreq

  INTEGER                       :: str1,str2
  INTEGER                       :: iscan
  INTEGER                       :: ntds=5
  INTEGER                       :: nscs=3 
  INTEGER                       :: nqds=3
  INTEGER, PARAMETER            :: ngs=2, nchanlall=13
  CHARACTER(LEN=40),DIMENSION(2):: groupnames=(/'S1','S2'/)
  CHARACTER(LEN=40),DIMENSION(2):: gswathheaders=(/'S1_SwathHeader','S2_SwathHeader'/)
  integer, dimension(2)         :: nchanls=(/9,4/)

  INTEGER, ALLOCATABLE          :: year(:) 
!  INTEGER                       :: month1, day1, hour1, minute1, second1

  INTEGER, ALLOCATABLE   :: time(:), month(:), day(:), hour(:), &
                                                    minute(:), second(:)

!  CHARACTER(LEN=5), ALLOCATABLE   :: time(:), month(:), day(:), hour(:), &
!                                                    minute(:), second(:)

!  CHARACTER(LEN=5)              :: mn, dd, hh, mm, ss


!  INTEGER, ALLOCATABLE        :: qual(:), dataQuality(:), geoWarning(:), geoError(:)
!  INTEGER, ALLOCATABLE        :: Quality(:,:),Quality1cr(:,:,:)

!  11/02/2015   Added JJJ
  INTEGER, ALLOCATABLE        :: Quality(:,:),Quality1cr(:,:,:), RFIflag(:,:,:), RFIflag1cr(:,:,:)


                                                                        
  REAL,    ALLOCATABLE        :: scStats(:), scLatitude(:), scLongitude(:), scAltitude(:)

  REAL,      ALLOCATABLE        :: data_set(:,:), Latitude(:,:), Longitude(:,:),  &
                                   incidenceAngle(:,:), satAzimuthAngle(:,:),     &
                                   solarAzimuthAngle(:,:),solarZenAngle(:,:),     &
                                   satLocZenAngle(:,:), incidenceAngle1cr(:,:,:), &
                                   satAzimuthAngle1cr(:,:,:),                     &
                                   solarAzimuthAngle1cr(:,:,:),                   &
                                   solarZenAngle1cr(:,:,:)
  CHARACTER(LEN=80),dimension(6):: dsetnames = (/'Latitude','Longitude',            &
                                    'incidenceAngle','satAzimuthAngle',           &
                                    'solarAzimuthAngle','solarZenAngle'/)

  CHARACTER(LEN=80),dimension(3):: qnames = (/'dataQuality', 'geoError', 'geoWarning'/)

  CHARACTER(LEN=80),dimension(5):: timenames =(/'Month', 'DayOfMonth', 'Hour', &
                                   'Minute', 'Second'/)

  CHARACTER(LEN=80),dimension(3):: SCstatnamesC =(/'SCaltitude', 'SClatitude',&
                                   'SClongitude'/)
  CHARACTER(LEN=80),dimension(3):: SCstatnamesB =(/'scAlt', 'scLat', 'scLon'/)

  CHARACTER(LEN=80),dimension(3):: SCstatnames

!  INTEGER, DIMENSION(:), ALLOCATABLE ::  monthint, dayint, hrint, minint, secint
   
  REAL,      ALLOCATABLE        :: Tb(:, :, :),Tb1cr(:,:,:)

  INTEGER                       :: GranuleNumber,   NumberScansBeforeGranule,   &
                                   NumberScansGranule,  NumberScansAfterGranule, &
                                   MissingData

  INTEGER                       :: exitStatus

  LOGICAL :: f_corder_valid ! Indicates whether the creation order 
                                         ! data is valid for this attribute 
  INTEGER :: corder         ! Is a positive integer containing the 
                                         ! creation order of the attribute
  INTEGER :: cset           ! Indicates the character set used for 
                                         ! the ! attribute’s name
  INTEGER(HSIZE_T)  :: data_size 
                                         ! Indicates the size, in the number
                                         ! of characters, of the attribute
  INTEGER  :: hdferr         ! Error code:
                                         ! 0 on success and -1 on failure

  REAL,         ALLOCATABLE     :: sunGlintAngle1b(:,:)
  INTEGER,      ALLOCATABLE     :: sunGlintAngle1c(:,:),sunGlintAngle1cr(:,:,:)
  LOGICAL                       :: gmi1b, gmi1c,gmi1cr
! INTEGER                       :: l,n,dim1,dim2,dim3,version

!   New  11/06/2014
  INTEGER                       :: l,n,dim1,dim2,dim3,version, ichar_v
  CHARACTER(LEN=1)              :: version_a
  REAL*4                        :: gmi_version


! YELENA
       integer                  :: need_date,need_syn,ier
       integer                  :: argc, iargc, iarg
      logical                  :: ffound
    CHARACTER (LEN = 8 )       ::  wanted_date
    CHARACTER (LEN = 2)       ::  wanted_syn
    

    
      need_date = 0
      need_syn = 0
      ffound = .false.
      argc = iargc()


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Get HDF file name
! CALL getarg(1,inputBuffer)

!       print *,'argc = ',argc


!  YELENA   Get  DATE and SYNOPTIC needed, and HDF file name
      if (argc < 1 .or. argc > 6) then
         call usage()
      endif
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     iarg = 0

      wanted_date =' '
      wanted_syn =' '
      do while (iarg < argc)
         iarg = iarg + 1
         call getarg( iarg, argv )
         if (index(argv,'-d') > 0) then
            if ( iarg+1 > argc ) call usage()
            iarg = iarg + 1
            call getarg( iarg,wanted_date )

         else if (index( argv, '-t') > 0) then
            if ( iarg+1 > argc ) call usage()
            iarg = iarg + 1
            call getarg( iarg, wanted_syn )
         else if (index( argv, '-f') > 0) then
            if ( iarg+1 > argc ) call usage()
               iarg = iarg + 1
            call getarg( iarg, inputFileName )
         endif
      end do

        
 print *, ' wanted date = ',wanted_date,' wanted synoptic= ',wanted_syn
 print *, ' inputFileName = ',inputFileName

! READ(inputBuffer, *) inputFileName
  print *, ' '
  print *, '------ new HDF file -------', trim(inputFileName)


         read(wanted_date,'(i8)',iostat=ier) need_date
        read(wanted_syn,'(i2)',iostat=ier) need_syn

   print *, ' need date = ',need_date,' needed synoptic= ',need_syn
          if ( wanted_date .eq.' ') then
             print *,'NO WANTED_DATE : stop'
             stop
          endif
          if ( wanted_syn .eq.' ') then
             print *,'NO   WANTED SYNOPTIC TIME  : stop'
             stop
          endif

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  gmi1cr = index(inputFileName, '1C-R.GPM.GMI.') > 0
  if (gmi1cr) then
    nmds=3
    tmbrname='Tc'
    outputPrefix=inputFileName(0:54)
  else
 print *, '------ new HDF file -------', trim(inputFileName)
    print*, 'File = ',trim(inputFileName),'   is not a GMI 1CR file.'
       stop
  endif

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Initialize FORTRAN interface.
  CALL h5open_f(error)
  ! Open file to read VD (table) information
  call h5fopen_f(inputFileName, H5F_ACC_RDONLY_F, file_id, error)
  if(error/=0) then
     print*, 'Error. Cannot open the HDF file. Program stop'
     stop
  endif

  ! ---------------------------------------------------------------------------
  ! ---------------------------------------------------------------------
  !get the file header info
  call h5aopen_f(file_id, 'FileHeader', attr_id, error)
  call h5aget_type_f(attr_id, type_id, error)
  attr_dims(1)=0
  call h5aread_f(attr_id, type_id, attr_data, attr_dims, error)
  GranuleNumber = inquire_attr_sub(attr_data, 'GranuleNumber')
  MissingData = inquire_attr_sub(attr_data, 'MissingData')
  print *, 'GranuleNumber, MissingData'
  print *, GranuleNumber, MissingData
  call h5aclose_f(attr_id, error)

!-----------------------------------------------------------------------
  ! open group1 to pull most information
  groupname = trim(groupnames(1)) 
  print *, '-------'
  print *, 'group:', trim(groupnames(1))
  call h5gopen_f(file_id, trim(groupname), group_id, error)
!----------------------------------------------------------------------- 
  !get the swath header information for S1
  swathheader = trim(gswathheaders(1))
  call h5aopen_f(group_id, trim(swathheader), attr_id, error)
  call h5aget_type_f(attr_id, type_id, hdferr) 

  attr_dims(1)=1
  call h5aread_f(attr_id,type_id, attr_data, attr_dims, error) 
   NumberScansBeforeGranule = inquire_attr_sub(attr_data, 'NumberScansBeforeGranule')
   NumberScansAfterGranule  = inquire_attr_sub(attr_data, 'NumberScansAfterGranule' )
   !nscan = inquire_attr_sub(attr_data, 'MaximumNumberScansTotal')
   nscan = inquire_attr_sub(attr_data, 'NumberScansGranule')
   npixel = inquire_attr_sub(attr_data, 'NumberPixels' )
   print *, 'NumberScansBeforeGranule, NumberScansAfterGranule, nscan, npixel'
   print *,  NumberScansBeforeGranule, NumberScansAfterGranule, nscan, npixel
  call h5aclose_f(attr_id, error)
  ! Terminate access to the SD interface and close the file.
  call h5fclose_f(file_id, error)
  ! Close FORTRAN interface.
  CALL h5close_f(error) 
  if (nscan < 1 ) then
    print*, 'Number of scans in the HDF file =', nscan
    print*, 'Error. Data are missing in HDF file. Program stopped.'
    stop
  endif
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 allocate (year(nScan),month(nScan),day(nScan),hour(nScan),minute(nScan),second(nScan))
  groupname='S1'
  subgroupname='ScanTime'
  dim1=nScan;           dim2=1;             dim3=1
  dsetname='Year'
  call  hdf5rd_i(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,year)
  dsetname='Month'
  call  hdf5rd_i(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,month)
  dsetname='DayOfMonth'
  call  hdf5rd_i(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,day)
  dsetname='Hour'
  call  hdf5rd_i(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,hour)
  dsetname='Minute'
  call  hdf5rd_i(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,minute)
  dsetname='Second'
  call  hdf5rd_i(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,second)
!-----------------------------------------------------------------------
  allocate (Tb1cr(nchanlall, npixel, nscan))
  groupname='S1'
  subgroupname=''
  dsetname='Tc'
  dim1=nchanls(1);      dim2=npixel;        dim3=nscan
  allocate (Tb(dim1, dim2,dim3))
  call  hdf5rd_f(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,Tb)
  Tb1cr(1:9, :,:) = Tb(:,:,:)
  deallocate (Tb)

  groupname='S2'
  dim1=nchanls(2);      dim2=npixel;        dim3=nscan
  allocate (Tb(dim1, dim2,dim3))
  call  hdf5rd_f(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,Tb)
  Tb1cr(10:nchanlall, :,:) = Tb(:,:,:)
  deallocate (Tb)
!-----------------------------------------------------------------------
  allocate (Quality1cr(nchanlall, npixel, nscan))
  allocate (incidenceAngle1cr(ngs,npixel, nscan))
  allocate (sunGlintAngle1cr(ngs,npixel, nscan))

  groupname='S1'
  subgroupname=''
  dim1=npixel;        dim2=nscan;           dim3=1
  allocate (Quality(dim1,dim2),  incidenceAngle(dim1,dim2), sunGlintAngle1c(dim1,dim2) )
  dsetname='Quality'
  call  hdf5rd_i(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,Quality)
  dsetname='incidenceAngle'
  call  hdf5rd_f(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,incidenceAngle)
  dsetname='sunGlintAngle'
  call  hdf5rd_i(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,sunGlintAngle1c)
  do l=1,9 
    Quality1cr(l,:,:) = Quality(:,:)
  enddo
    incidenceAngle1cr(1,:,:) = incidenceAngle(:,:)
    sunGlintAngle1cr(1,:,:) = sunGlintAngle1c(:,:)
  deallocate (Quality,incidenceAngle, sunGlintAngle1c)

  groupname='S2'
  allocate (Quality(dim1,dim2),  incidenceAngle(dim1,dim2), sunGlintAngle1c(dim1,dim2) )
  dsetname='Quality'
  call  hdf5rd_i(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,Quality)
  dsetname='incidenceAngle'
  call  hdf5rd_f(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,incidenceAngle)
  dsetname='sunGlintAngle'
  call  hdf5rd_i(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,sunGlintAngle1c)
  do l=10,nchanlall
    Quality1cr(l,:,:) = Quality(:,:)
  enddo
    incidenceAngle1cr(2,:,:) = incidenceAngle(:,:)
    sunGlintAngle1cr(2,:,:) = sunGlintAngle1c(:,:)
  deallocate (Quality,incidenceAngle, sunGlintAngle1c)

!-----------------------------------------------------------------------
  allocate (SClatitude(nScan),SClongitude(nScan),SCaltitude(nScan))
  groupname='S1'
  subgroupname='SCstatus'
  dim1=nScan;           dim2=1;             dim3=1
  dsetname='SClatitude'
  call  hdf5rd_f(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,SClatitude)
  dsetname='SClongitude'
  call  hdf5rd_f(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,SClongitude)
  dsetname='SCaltitude'
  call  hdf5rd_f(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,SCaltitude)

!-----------------------------------------------------------------------
  allocate (Latitude(npixel, nscan), Longitude(npixel, nscan) )
  groupname='S1'
  subgroupname=''
  dim1=npixel;        dim2=nscan;           dim3=1
  dsetname='Latitude'
  call  hdf5rd_f(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,Latitude)
  dsetname='Longitude'
  call  hdf5rd_f(inputFileName,groupname,subgroupname,dsetname,dim1,dim2,dim3,Longitude)

!-----------------------------------------------------------------------
  ! read these from level 1B data files.
! JJJ
  str1 = index(inputFileName,xcal)
  if (str1 == 0 ) then
    print*, "The input 1C-R file's name does not have the format like 1C-R.GPM.GMI.XCAL2015-C...."
    print*, "The program cannot construct 1B file's name accordingly. Prog Stop 4."
    stop
  endif
  xcalyear= inputFileName((str1+4):(str1+7))

  str1 = 0
! end JJJ  16 Mar 2016

  str1 = index(inputFileName, trim(hdf1cr_prefix))
  str2 = len_trim(inputFileName)
  if(str1 > 1) then
!  inputFileName1b = inputFileName(1:(str1-1))//trim(hdf1b_prefix)//inputFileName((str1+len_trim(hdf1cr_prefix)):str2) 
! else
!  inputFileName1b = trim(hdf1b_prefix)//inputFileName(len_trim(hdf1cr_prefix)+1:str2) 
! JJJ  16 Mar 2016
   inputFileName1b = inputFileName(1:(str1-1))//trim(hdf1b_prefix)//xcalyear//inputFileName((str1+len_trim(hdf1cr_prefix)+6) :str2)
  else
   inputFileName1b = trim(hdf1b_prefix)//xcalyear//inputFileName((len_trim(hdf1cr_prefix)+7):str2)
! End JJJ 16 Mar 2016
  endif
    print *, inputFileName1b
  allocate (satAzimuthAngle(npixel, nscan), solarAzimuthAngle(npixel, nscan), &
            solarZenAngle(npixel, nscan))
  allocate (satAzimuthAngle1cr(ngs, npixel, nscan), solarAzimuthAngle1cr(ngs,npixel,nscan),&
            solarZenAngle1cr(ngs, npixel, nscan))

! 11/02/2015 JJJ
  allocate (RFIflag(5,npixel, nscan), RFIflag1cr(13,npixel,nscan))


  groupname='S1'
  subgroupname=''
  dim1=npixel;        dim2=nscan;           dim3=1
! JJJ   16 March 2016
  nfreq=5

  dsetname='satAzimuthAngle'
  call  hdf5rd_f(inputFileName1b,groupname,subgroupname,dsetname,dim1,dim2,dim3,satAzimuthAngle)
  dsetname='solarAzimuthAngle'
  call  hdf5rd_f(inputFileName1b,groupname,subgroupname,dsetname,dim1,dim2,dim3,solarAzimuthAngle)
  dsetname='solarZenAngle'
  call  hdf5rd_f(inputFileName1b,groupname,subgroupname,dsetname,dim1,dim2,dim3,solarZenAngle)

! 11/02/2015 JJJ
  dsetname='RFIFlag'
  call  hdf5rd_i(inputFileName1b,groupname,subgroupname,dsetname,nfreq,dim1,dim2,RFIflag)

  satAzimuthAngle1cr(1,:,:) = satAzimuthAngle(:,:)
  solarAzimuthAngle1cr(1,:,:) = solarAzimuthAngle(:,:)
  solarZenAngle1cr(1,:,:) = solarZenAngle(:,:)

!   JIANJUN - YELENA   November 2015
!  DEFINE RFIflag1cr

  RFIflag1cr(1,:,:) = RFIflag(1,:,:)
  RFIflag1cr(2,:,:) = RFIflag(1,:,:)
  RFIflag1cr(3,:,:) = RFIflag(2,:,:)
  RFIflag1cr(4,:,:) = RFIflag(2,:,:)
  RFIflag1cr(5,:,:) = RFIflag(3,:,:)
  RFIflag1cr(6,:,:) = RFIflag(4,:,:)
  RFIflag1cr(7,:,:) = RFIflag(4,:,:)
  RFIflag1cr(8,:,:) = RFIflag(5,:,:)
  RFIflag1cr(9,:,:) = RFIflag(5,:,:)



  groupname='S2'
  subgroupname=''
  dim1=npixel;        dim2=nscan;           dim3=1

!   JIANJUN - YELENA   November 2015
!  DEFINE nfreq
  nfreq=2

  dsetname='satAzimuthAngle'
  call  hdf5rd_f(inputFileName1b,groupname,subgroupname,dsetname,dim1,dim2,dim3,satAzimuthAngle)
  dsetname='solarAzimuthAngle'
  call  hdf5rd_f(inputFileName1b,groupname,subgroupname,dsetname,dim1,dim2,dim3,solarAzimuthAngle)
  dsetname='solarZenAngle'
  call  hdf5rd_f(inputFileName1b,groupname,subgroupname,dsetname,dim1,dim2,dim3,solarZenAngle)

!   JIANJUN - YELENA   November 2015
!  DEFINE RFIflag1cr   
  dsetname='RFIFlag'
  call  hdf5rd_i(inputFileName1b,groupname,subgroupname,dsetname,nfreq,dim1,dim2,RFIflag)

  satAzimuthAngle1cr(2,:,:) = satAzimuthAngle(:,:)
  solarAzimuthAngle1cr(2,:,:) = solarAzimuthAngle(:,:)
  solarZenAngle1cr(2,:,:) = solarZenAngle(:,:)

!   JIANJUN - YELENA   November 2015
!  DEFINE RFIflag1cr   

  RFIflag1cr(10,:,:) = RFIflag(1,:,:)
  RFIflag1cr(11,:,:) = RFIflag(1,:,:)
  RFIflag1cr(12,:,:) = RFIflag(2,:,:)
  RFIflag1cr(13,:,:) = RFIflag(2,:,:)

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 !write BUFR file
  !str1=index(inputFileName,'.RT-H5')
  !prefix_sub=inputFileName(1:(str1-1))
  !prefix = trim(prefix_sub)
  str1=index(inputFileName,'.V')
  read( inputFileName((str1+2) : (str1+3)), '(i)') version
  print *, 'version=', version

!   JIANJUN - YELENA   November 2015
!  READ VERSION_a  

   read( inputFileName((str1+4) : (str1+4)), '(a)') version_a
   print *, 'version=', version, version_a

!******************************************************
  read( inputFileName((str1+4) : (str1+4)), '(a)') version_a
  print *, 'version=', version, version_a
  ichar_v = ichar(version_a)  ! ANSI code 
  if( ichar_v >= 65 .and. ichar_v <=90) then
      gmi_version = version + (float(ichar_v)-64)/100
  else if(ichar_v >= 97 .and. ichar_v <=122) then
      gmi_version = version + (float(ichar_v)-96)/100
  else
      print*, 'Error, file name format like *.V03A* has been changed. This program cannot get the version number correctly.'
      print*, 'Program stopped.'
      stop
  endif


!******************************************************
  !     do n=1,nscan
  !        do l=1,npixel          
  !        write(106,1061) l,Latitude(l,n), Longitude(l,n),int(Quality1cr(1:13,l,n))
  !        enddo
  !     enddo
  !1061     format(2x,i5,2f8.2,3x,13i4)

! CALL write_gmi_bufr_1cr(prefix, version, nscan, npixel, nchanlall, year, month, day, hour, minute, second, Latitude, Longitude, Quality1cr, scLatitude, scLongitude, scAltitude,satAzimuthAngle1cr, solarAzimuthAngle1cr, solarZenAngle1cr, incidenceAngle1cr, Tb1cr, GranuleNumber, NumberScansBeforeGranule,NumberScansGranule, NumberScansAfterGranule, sunGlintAngle1cr, MissingData, exitStatus)

!  YELENA  
!  need_date,need_syn  - additional argument

! CALL write_gmi_bufr_1cr(prefix, version, nscan, npixel, nchanlall, year, month, day, hour, minute, second, Latitude, Longitude, Quality1cr, scLatitude, scLongitude, scAltitude,satAzimuthAngle1cr, solarAzimuthAngle1cr, solarZenAngle1cr, incidenceAngle1cr, Tb1cr, GranuleNumber, NumberScansBeforeGranule,NumberScansGranule, NumberScansAfterGranule, sunGlintAngle1cr, MissingData,need_date,need_syn,exitStatus)


! CALL write_gmi_bufr_1cr(prefix, gmi_version, nscan, npixel, nchanlall, year, month, day, hour, minute, second,&
!                        Latitude, Longitude, Quality1cr, scLatitude, scLongitude, scAltitude,satAzimuthAngle1cr,&
!                        solarAzimuthAngle1cr, solarZenAngle1cr, incidenceAngle1cr, &
!                        Tb1cr, GranuleNumber, NumberScansBeforeGranule,NumberScansGranule, NumberScansAfterGranule, &
!                        sunGlintAngle1cr, MissingData,need_date,need_syn,exitStatus)


!   JIANJUN - YELENA   November 2015
!  RFIflag1cr  - additional argument



  CALL write_gmi_bufr_1cr(prefix, gmi_version, nscan, npixel, nchanlall, year, month, day, hour, minute, second,&
                         Latitude, Longitude, Quality1cr, scLatitude, scLongitude, scAltitude,satAzimuthAngle1cr,&
                         solarAzimuthAngle1cr, solarZenAngle1cr, incidenceAngle1cr, &
                         Tb1cr, GranuleNumber, NumberScansBeforeGranule,NumberScansGranule, NumberScansAfterGranule, &
                   sunGlintAngle1cr,RFIflag1cr, MissingData,need_date,need_syn,exitStatus)

  deallocate (year,month,day,hour,minute,second)
  deallocate (Tb1cr)
  deallocate (Quality1cr,incidenceAngle1cr, sunGlintAngle1cr)
  deallocate (SClatitude,SClongitude,SCaltitude)
  deallocate (Latitude, Longitude)
  deallocate (satAzimuthAngle, solarAzimuthAngle, &
            solarZenAngle)
  deallocate (satAzimuthAngle1cr, solarAzimuthAngle1cr, &
            solarZenAngle1cr)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Contains
      subroutine usage()
      print *, 'Usage: gmi1cr_bufr.x  [-d yyyymmdd] [-t synoptic] \   '
      print *, '                h5filename                             '
      print *, '  -d need_date    optional: date to process            '
      print *, '  -t need_syn     optional: synoptic to process        '
      print *, '  h5filename     name of input HDF5 file               '
      stop

      end subroutine usage


  Function inquire_attr_sub(attr_buffer, attrsub_name) result(attrsub_ival)
! find the substring attrsub_name.
!---------------------------------------------------------------------------
  IMPLICIT NONE
  CHARACTER (LEN =*),INTENT(IN) :: attr_buffer
  character (len=*), INTENT(IN) :: attrsub_name
  integer*8                     ::  attrsub_ival
  character(len=100)            :: attrsub_cval
  integer                       :: str_id0, str_ida, str_id1, str_id2,&
                                   str_idx, str_idy

  str_id0 = index(attr_buffer, attrsub_name)
  if (str_id0 <= 0) then
     print *,  attrsub_name, ' is not found in '
     print *,  attr_buffer
     attrsub_ival = -999
     print *,  ' attrsub_ival = -999 '
     return
  endif
  str_ida = len(attr_buffer)
  str_id1 = index( attr_buffer(str_id0:str_ida),'=')
  str_id2 = index( attr_buffer(str_id0:str_ida),';')
  str_idx = str_id0 + str_id1
  str_idy = str_id0 + str_id2 - 2
  attrsub_cval = attr_buffer(str_idx:str_idy)
  read(attrsub_cval,'(I10)') attrsub_ival

  END function inquire_attr_sub

END PROGRAM ONECR_HDF_to_BUFR
!


