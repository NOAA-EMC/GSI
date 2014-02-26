!$Id: rdr2tdr_f16_ssmis.f90 1043 2008-05-01 21:05:18Z sidb $
!===============================================================
! Name:       rdr2tdr_f16_ssmis
!
!
! Type:         Main Program
!
!
! Description:
!       Program to read calibrated SSMIS TDR and then write out
!       img, env, las and uas files.
!
! Modules needed:
!       - IO_InstrConfig
!       - IO_MeasurData
!       - IO_Misc
!       - misc
!       - Consts
!       - ErrorHandling
!       - io_ssmis
!
! History:
!       05-01-2008      Wanchun Chen, PSGS Inc @ NOAA/NESDIS/STAR
!
!===============================================================
Program rdr2tdr_f16_ssmis

  USE IO_MeasurData
  USE IO_Misc
  USE ErrorHandling
  USE io_ssmis
  USE Consts
  IMPLICIT NONE
 
  !---- Input file name
  CHARACTER(LEN=250) ssmisTdrFile
  
  !---- intermediate file arrays to hold file names
  CHARACTER(LEN=250), DIMENSION(:), POINTER     :: rdrFiles
  CHARACTER(LEN=250), DIMENSION(:), POINTER     :: imgFiles
  CHARACTER(LEN=250), DIMENSION(:), POINTER     :: envFiles
  CHARACTER(LEN=250), DIMENSION(:), POINTER     :: lasFiles
  CHARACTER(LEN=250), DIMENSION(:), POINTER     :: uasFiles
  
  !---- Output file names 
  CHARACTER(LEN=250) MeasureFile_img
  CHARACTER(LEN=250) MeasureFile_env
  CHARACTER(LEN=250) MeasureFile_las
  CHARACTER(LEN=250) MeasureFile_uas
  
  !---- Input structure
  TYPE(rev_structure) :: rev_hdr
  TYPE (scan_structure),  DIMENSION(MAX_NPOINTS) :: scan_hdr
  TYPE (ephem_structure), DIMENSION(MAX_NPOINTS,3) :: ephem
  TYPE (img_structure),   DIMENSION(MAX_NPOINTS,NSCENES_IMG) :: img
  TYPE (env_structure),   DIMENSION(MAX_NPOINTS,NSCENES_ENV) :: env
  TYPE (las_structure),   DIMENSION(MAX_NPOINTS,NSCENES_LAS) :: las
  TYPE (uas_structure),   DIMENSION(MAX_NPOINTS,NSCENES_UAS) :: uas
  TYPE (aux_structure),   DIMENSION(MAX_NPOINTS) :: aux
  
  INTEGER       :: year, jday, hour, minute, nscans
  INTEGER       :: scanDay, scanYear, scanUTC
  
  INTEGER, PARAMETER :: NFOVS_IMG=180, NCHAN_IMG = 6
  INTEGER, PARAMETER :: NFOVS_ENV=90,  NCHAN_ENV = 5
  INTEGER, PARAMETER :: NFOVS_LAS=60,  NCHAN_LAS = 8
  INTEGER, PARAMETER :: NFOVS_UAS=30,  NCHAN_UAS = 5

  !---- Output TB
  real, dimension(NFOVS_IMG, NCHAN_IMG) :: tb_img
  real, dimension(NFOVS_ENV, NCHAN_ENV) :: tb_env
  real, dimension(NFOVS_LAS, NCHAN_LAS) :: tb_las
  real, dimension(NFOVS_UAS, NCHAN_UAS) :: tb_uas
  !---- Output angle, lat/lon, zenith angle and solar angle
  real, dimension(NFOVS_IMG) :: angle_img, lat_img, lon_img, RelAziAngle_img, SolZenAngle_img 
  real, dimension(NFOVS_ENV) :: angle_env, lat_env, lon_env, RelAziAngle_env, SolZenAngle_env
  real, dimension(NFOVS_LAS) :: angle_las, lat_las, lon_las, RelAziAngle_las, SolZenAngle_las
  real, dimension(NFOVS_UAS) :: angle_uas, lat_uas, lon_uas, RelAziAngle_uas, SolZenAngle_uas  
    
  INTEGER, PARAMETER    :: NQC = 2
  INTEGER, DIMENSION(NQC) :: qc
  INTEGER               :: node
  
  !---- internal variables
  real          :: lat1, lat2                           ! used to decide asc/dec
  integer       :: iu_list, iu_img,iu_env,iu_las,iu_uas ! file unit numbers
  integer       :: ifile, nFiles, i, j
  
  REAL, PARAMETER ::  cfr_ch01 = 50.300   
  REAL, PARAMETER ::  cfr_ch02 = 52.800   
  REAL, PARAMETER ::  cfr_ch03 = 53.596   
  REAL, PARAMETER ::  cfr_ch04 = 54.400   
  REAL, PARAMETER ::  cfr_ch05 = 55.500   
  REAL, PARAMETER ::  cfr_ch06 = 57.290   
  REAL, PARAMETER ::  cfr_ch07 = 59.400   
  REAL, PARAMETER ::  cfr_ch08 = 150.000  
  REAL, PARAMETER ::  cfr_ch09 = 183.310  
  REAL, PARAMETER ::  cfr_ch10 = 183.310  
  REAL, PARAMETER ::  cfr_ch11 = 183.310  
  REAL, PARAMETER ::  cfr_ch12 = 19.350   
  REAL, PARAMETER ::  cfr_ch13 = 19.350   
  REAL, PARAMETER ::  cfr_ch14 = 22.235   
  REAL, PARAMETER ::  cfr_ch15 = 37.000   
  REAL, PARAMETER ::  cfr_ch16 = 37.000   
  REAL, PARAMETER ::  cfr_ch17 = 91.655   
  REAL, PARAMETER ::  cfr_ch18 = 91.655   
  REAL, PARAMETER ::  cfr_ch19 = 63.283248
  REAL, PARAMETER ::  cfr_ch20 = 60.792668
  REAL, PARAMETER ::  cfr_ch21 = 60.792668
  REAL, PARAMETER ::  cfr_ch22 = 60.792668
  REAL, PARAMETER ::  cfr_ch23 = 60.792668
  REAL, PARAMETER ::  cfr_ch24 = 60.792668

  INTEGER, PARAMETER ::  pol_ch01 = 4
  INTEGER, PARAMETER ::  pol_ch02 = 4
  INTEGER, PARAMETER ::  pol_ch03 = 4
  INTEGER, PARAMETER ::  pol_ch04 = 4
  INTEGER, PARAMETER ::  pol_ch05 = 4
  INTEGER, PARAMETER ::  pol_ch06 = 2
  INTEGER, PARAMETER ::  pol_ch07 = 2
  INTEGER, PARAMETER ::  pol_ch08 = 5
  INTEGER, PARAMETER ::  pol_ch09 = 5
  INTEGER, PARAMETER ::  pol_ch10 = 5
  INTEGER, PARAMETER ::  pol_ch11 = 5
  INTEGER, PARAMETER ::  pol_ch12 = 5
  INTEGER, PARAMETER ::  pol_ch13 = 4
  INTEGER, PARAMETER ::  pol_ch14 = 4
  INTEGER, PARAMETER ::  pol_ch15 = 5
  INTEGER, PARAMETER ::  pol_ch16 = 4
  INTEGER, PARAMETER ::  pol_ch17 = 4
  INTEGER, PARAMETER ::  pol_ch18 = 5
  INTEGER, PARAMETER ::  pol_ch19 = 2
  INTEGER, PARAMETER ::  pol_ch20 = 2
  INTEGER, PARAMETER ::  pol_ch21 = 2
  INTEGER, PARAMETER ::  pol_ch22 = 2
  INTEGER, PARAMETER ::  pol_ch23 = 2
  INTEGER, PARAMETER ::  pol_ch24 = 2
  
  REAL, DIMENSION(6)    :: cfr_img 
  REAL, DIMENSION(5)    :: cfr_env 
  REAL, DIMENSION(8)    :: cfr_las 
  REAL, DIMENSION(5)    :: cfr_uas
  
  INTEGER, DIMENSION(6) :: pol_img 
  INTEGER, DIMENSION(5) :: pol_env 
  INTEGER, DIMENSION(8) :: pol_las 
  INTEGER, DIMENSION(5) :: pol_uas
 
  DATA cfr_img/cfr_ch08, cfr_ch09, cfr_ch10, cfr_ch11, cfr_ch17, cfr_ch18/
  DATA pol_img/pol_ch08, pol_ch09, pol_ch10, pol_ch11, pol_ch17, pol_ch18/
  DATA cfr_env/cfr_ch12, cfr_ch13, cfr_ch14, cfr_ch15, cfr_ch16/
  DATA pol_env/pol_ch12, pol_ch13, pol_ch14, pol_ch15, pol_ch16/
  DATA cfr_las/cfr_ch01, cfr_ch02, cfr_ch03, cfr_ch04, cfr_ch05, cfr_ch06, cfr_ch07, cfr_ch24/
  DATA pol_las/pol_ch01, pol_ch02, pol_ch03, pol_ch04, pol_ch05, pol_ch06, pol_ch07, pol_ch24/
  DATA cfr_uas/cfr_ch19, cfr_ch20, cfr_ch21, cfr_ch22, cfr_ch23/
  DATA pol_uas/pol_ch19, pol_ch20, pol_ch21, pol_ch22, pol_ch23/

  !---- Namelist data
  CHARACTER(LEN=250)    :: rdrfileList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=250)    :: pathTDR=DEFAULT_VALUE_STR4
  CHARACTER(LEN=16)     :: accessStr=DEFAULT_VALUE_STR4
  CHARACTER(LEN=16)     :: formStr=DEFAULT_VALUE_STR4
  NAMELIST /ContrlRDR2TDR/rdrfileList,pathTDR,accessStr,formStr
  
  !-------------------------------------------------------------------------------------
  READ(*,NML=ContrlRDR2TDR)
  
  !---Read the file names of SSMIS data and build IMG/ENV/LAS/UAS TDR files names
  call ReadList(iu_list,trim(rdrfileList),rdrFiles,nFiles,imgFiles,pathTDR,'TDR_IMG')
  DEALLOCATE(rdrFiles)
  call ReadList(iu_list,trim(rdrfileList),rdrFiles,nFiles,envFiles,pathTDR,'TDR_ENV')
  DEALLOCATE(rdrFiles)
  call ReadList(iu_list,trim(rdrfileList),rdrFiles,nFiles,lasFiles,pathTDR,'TDR_LAS')
  DEALLOCATE(rdrFiles)
  
  call ReadList(iu_list,trim(rdrfileList),rdrFiles,nFiles,uasFiles,pathTDR,'TDR_UAS')
  
  IF (nFiles .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'')

  FILE_LOOPS: DO IFILE=1,nFiles
  
    !ssmisTdrFile='NPR.TDRN.SA.D08075.S0403.E0541.B2273637.NS.CALIB'
    ssmisTdrFile=trim(rdrFiles(ifile))
    !write(*,*)trim(ssmisTdrFile) 
    call ReadCalibratedSsmisTdr(22,ssmisTdrFile,rev_hdr,year,jday,hour,minute,nscans,&
        scan_hdr,ephem,img,env,las,uas,aux,accessStr,formStr)
    
    lat1 = ichar(img(1,1)%lat(1))*256 + ichar(img(1,1)%lat(2))
    lat2 = ichar(img(2,1)%lat(1))*256 + ichar(img(2,1)%lat(2))
    
    if ( lat1 .gt. 9000  ) lat1=lat1-65536
    if ( lat2 .gt. 9000  ) lat2=lat2-65536
    
    if ( lat2 .ge. lat1 ) node = 0 ! ascending
    if ( lat2 .lt. lat1 ) node = 1 ! descending

    MeasureFile_img=trim(imgFiles(ifile))
    MeasureFile_env=trim(envFiles(ifile))
    MeasureFile_las=trim(lasFiles(ifile))
    MeasureFile_uas=trim(uasFiles(ifile))
    
    call WriteRadHdrScanLMode_ascii(MeasureFile_img,iu_img,nscans,nFovs_img,nqc,nchan_img,cfr_img,pol_img)
    call WriteRadHdrScanLMode_ascii(MeasureFile_env,iu_env,nscans,nFovs_env,nqc,nchan_env,cfr_env,pol_env)
    call WriteRadHdrScanLMode_ascii(MeasureFile_las,iu_las,nscans,nFovs_las,nqc,nchan_las,cfr_las,pol_las)
    call WriteRadHdrScanLMode_ascii(MeasureFile_uas,iu_uas,nscans,nFovs_uas,nqc,nchan_uas,cfr_uas,pol_uas)

    !---- loop over the scan lines
    SCAN_LOOPS: do i=1, nscans
      
      if(i.ge.2) then      
        lat1 = ichar(img(i-1,1)%lat(1))*256 + ichar(img(i-1,1)%lat(2))
        lat2 = ichar(img(i,1)%lat(1))*256   + ichar(img(i,1)%lat(2))

        if ( lat1 .gt. 9000  ) lat1=lat1-65536
        if ( lat2 .gt. 9000  ) lat2=lat2-65536

        if ( lat2 .ge. lat1 ) node = 0 ! ascending
        if ( lat2 .lt. lat1 ) node = 1 ! descending
      endif
      
      scanYear = ichar(scan_hdr(i)%year(1))*256*256*256 &
               + ichar(scan_hdr(i)%year(2))*256*256 &
               + ichar(scan_hdr(i)%year(3))*256 &
               + ichar(scan_hdr(i)%year(4))
      
      scanDay = ichar(scan_hdr(i)%jday(1))*256 &
              + ichar(scan_hdr(i)%jday(2))
      
      scanUTC = ichar(scan_hdr(i)%st_time(1))*256*256*256 &
              + ichar(scan_hdr(i)%st_time(2))*256*256 &
              + ichar(scan_hdr(i)%st_time(3))*256 &
              + ichar(scan_hdr(i)%st_time(4))

      !-- 2-bytes integer might have overflow, the first bit might be 1
      !-- so we need adjust this by minus 65536
      !-- TB saved in raw SSMIS range from -19000 to 6000, scale factor is 100
      !-- namely, -190C to 60C
      
      !---- IMG scan ---------------------------------------------------------
      
      do j=1,NFOVS_IMG

        tb_img(j,1)  = ichar(img(i,j)%ch8(1))*256  + ichar(img(i,j)%ch8(2))
        tb_img(j,2)  = ichar(img(i,j)%ch9(1))*256  + ichar(img(i,j)%ch9(2))
        tb_img(j,3)  = ichar(img(i,j)%ch10(1))*256 + ichar(img(i,j)%ch10(2))
        tb_img(j,4)  = ichar(img(i,j)%ch11(1))*256 + ichar(img(i,j)%ch11(2))
        tb_img(j,5)  = ichar(img(i,j)%ch17(1))*256 + ichar(img(i,j)%ch17(2))
        tb_img(j,6)  = ichar(img(i,j)%ch18(1))*256 + ichar(img(i,j)%ch18(2))

        if ( tb_img(j,1) .gt. 6000  ) tb_img(j,1)=tb_img(j,1)-65536
        if ( tb_img(j,2) .gt. 6000  ) tb_img(j,2)=tb_img(j,2)-65536
        if ( tb_img(j,3) .gt. 6000  ) tb_img(j,3)=tb_img(j,3)-65536
        if ( tb_img(j,4) .gt. 6000  ) tb_img(j,4)=tb_img(j,4)-65536
        if ( tb_img(j,5) .gt. 6000  ) tb_img(j,5)=tb_img(j,5)-65536
        if ( tb_img(j,6) .gt. 6000  ) tb_img(j,6)=tb_img(j,6)-65536

        tb_img(j,1)=tb_img(j,1)*0.01+CTOK
        tb_img(j,2)=tb_img(j,2)*0.01+CTOK
        tb_img(j,3)=tb_img(j,3)*0.01+CTOK
        tb_img(j,4)=tb_img(j,4)*0.01+CTOK
        tb_img(j,5)=tb_img(j,5)*0.01+CTOK
        tb_img(j,6)=tb_img(j,6)*0.01+CTOK

        angle_img(j) = 53.

        lat_img(j) = ichar(img(i,j)%lat(1))*256 + ichar(img(i,j)%lat(2))
        lon_img(j) = ichar(img(i,j)%lon(1))*256 + ichar(img(i,j)%lon(2))

        if ( lat_img(j) .gt. 9000  ) lat_img(j)=lat_img(j)-65536
        if ( lon_img(j) .gt. 18000 ) lon_img(j)=lon_img(j)-65536

        lat_img(j) = lat_img(j) * 0.01
        lon_img(j) = lon_img(j) * 0.01

        RelAziAngle_img(j) = -999.
        SolZenAngle_img(j) = -999.

      enddo


      !---- ENV scan ---------------------------------------------------------

      do j=1,NFOVS_ENV 

        tb_env(j,1)  = ichar(env(i,j)%ch12(1))*256 + ichar(env(i,j)%ch12(2))
        tb_env(j,2)  = ichar(env(i,j)%ch13(1))*256 + ichar(env(i,j)%ch13(2))
        tb_env(j,3)  = ichar(env(i,j)%ch14(1))*256 + ichar(env(i,j)%ch14(2))
        tb_env(j,4)  = ichar(env(i,j)%ch15(1))*256 + ichar(env(i,j)%ch15(2))
        tb_env(j,5)  = ichar(env(i,j)%ch16(1))*256 + ichar(env(i,j)%ch16(2))

        if ( tb_env(j,1) .gt. 6000  ) tb_env(j,1)=tb_env(j,1)-65536
        if ( tb_env(j,2) .gt. 6000  ) tb_env(j,2)=tb_env(j,2)-65536
        if ( tb_env(j,3) .gt. 6000  ) tb_env(j,3)=tb_env(j,3)-65536
        if ( tb_env(j,4) .gt. 6000  ) tb_env(j,4)=tb_env(j,4)-65536
        if ( tb_env(j,5) .gt. 6000  ) tb_env(j,5)=tb_env(j,5)-65536

        tb_env(j,1)=tb_env(j,1)*0.01+CTOK
        tb_env(j,2)=tb_env(j,2)*0.01+CTOK
        tb_env(j,3)=tb_env(j,3)*0.01+CTOK
        tb_env(j,4)=tb_env(j,4)*0.01+CTOK
        tb_env(j,5)=tb_env(j,5)*0.01+CTOK

        angle_env(j) = 53.

        !----Select lat/lon of Channels 15/16 (instead of 12-14) because more focalized
        !lat_env(j) = ichar(env(i,j)%lat(1))*256 + ichar(env(i,j)%lat(2))
        !lon_env(j) = ichar(env(i,j)%lon(1))*256 + ichar(env(i,j)%lon(2))
        lat_env(j) = ichar(env(i,j)%lat15(1))*256 + ichar(env(i,j)%lat15(2))
        lon_env(j) = ichar(env(i,j)%lon15(1))*256 + ichar(env(i,j)%lon15(2))

        if ( lat_env(j) .gt. 9000  ) lat_env(j)=lat_env(j)-65536
        if ( lon_env(j) .gt. 18000 ) lon_env(j)=lon_env(j)-65536

        lat_env(j) = lat_env(j) * 0.01
        lon_env(j) = lon_env(j) * 0.01

        RelAziAngle_env(j) = -999.
        SolZenAngle_env(j) = -999.

      enddo


      !---- LAS scan ---------------------------------------------------------

      do j=1,NFOVS_LAS

        tb_las(j,1)  = ichar(las(i,j)%ch1(1))*256 + ichar(las(i,j)%ch1(2))
        tb_las(j,2)  = ichar(las(i,j)%ch2(1))*256 + ichar(las(i,j)%ch2(2))
        tb_las(j,3)  = ichar(las(i,j)%ch3(1))*256 + ichar(las(i,j)%ch3(2))
        tb_las(j,4)  = ichar(las(i,j)%ch4(1))*256 + ichar(las(i,j)%ch4(2))
        tb_las(j,5)  = ichar(las(i,j)%ch5(1))*256 + ichar(las(i,j)%ch5(2))
        tb_las(j,6)  = ichar(las(i,j)%ch6(1))*256 + ichar(las(i,j)%ch6(2))
        tb_las(j,7)  = ichar(las(i,j)%ch7(1))*256 + ichar(las(i,j)%ch7(2))
        tb_las(j,8)  = ichar(las(i,j)%ch24(1))*256 + ichar(las(i,j)%ch24(2))

        if ( tb_las(j,1) .gt. 6000  ) tb_las(j,1)=tb_las(j,1)-65536
        if ( tb_las(j,2) .gt. 6000  ) tb_las(j,2)=tb_las(j,2)-65536
        if ( tb_las(j,3) .gt. 6000  ) tb_las(j,3)=tb_las(j,3)-65536
        if ( tb_las(j,4) .gt. 6000  ) tb_las(j,4)=tb_las(j,4)-65536
        if ( tb_las(j,5) .gt. 6000  ) tb_las(j,5)=tb_las(j,5)-65536
        if ( tb_las(j,6) .gt. 6000  ) tb_las(j,6)=tb_las(j,6)-65536
        if ( tb_las(j,7) .gt. 6000  ) tb_las(j,7)=tb_las(j,7)-65536
        if ( tb_las(j,8) .gt. 6000  ) tb_las(j,8)=tb_las(j,8)-65536

        tb_las(j,1)=tb_las(j,1)*0.01+CTOK
        tb_las(j,2)=tb_las(j,2)*0.01+CTOK
        tb_las(j,3)=tb_las(j,3)*0.01+CTOK
        tb_las(j,4)=tb_las(j,4)*0.01+CTOK
        tb_las(j,5)=tb_las(j,5)*0.01+CTOK
        tb_las(j,6)=tb_las(j,6)*0.01+CTOK
        tb_las(j,7)=tb_las(j,7)*0.01+CTOK
        tb_las(j,8)=tb_las(j,8)*0.01+CTOK

        angle_las(j) = 53.

        lat_las(j) = ichar(las(i,j)%lat(1))*256 + ichar(las(i,j)%lat(2))
        lon_las(j) = ichar(las(i,j)%lon(1))*256 + ichar(las(i,j)%lon(2))

        if ( lat_las(j) .gt. 9000  ) lat_las(j)=lat_las(j)-65536
        if ( lon_las(j) .gt. 18000 ) lon_las(j)=lon_las(j)-65536

        lat_las(j) = lat_las(j) * 0.01
        lon_las(j) = lon_las(j) * 0.01

        RelAziAngle_las(j) = -999.
        SolZenAngle_las(j) = -999.

      enddo


      !---- UAS scan ---------------------------------------------------------

      do j=1,NFOVS_UAS

        tb_uas(j,1)  = ichar(uas(i,j)%ch19(1))*256 + ichar(uas(i,j)%ch19(2))
        tb_uas(j,2)  = ichar(uas(i,j)%ch20(1))*256 + ichar(uas(i,j)%ch20(2))
        tb_uas(j,3)  = ichar(uas(i,j)%ch21(1))*256 + ichar(uas(i,j)%ch21(2))
        tb_uas(j,4)  = ichar(uas(i,j)%ch22(1))*256 + ichar(uas(i,j)%ch22(2))
        tb_uas(j,5)  = ichar(uas(i,j)%ch23(1))*256 + ichar(uas(i,j)%ch23(2))

        if ( tb_uas(j,1) .gt. 6000  ) tb_uas(j,1)=tb_uas(j,1)-65536
        if ( tb_uas(j,2) .gt. 6000  ) tb_uas(j,2)=tb_uas(j,2)-65536
        if ( tb_uas(j,3) .gt. 6000  ) tb_uas(j,3)=tb_uas(j,3)-65536
        if ( tb_uas(j,4) .gt. 6000  ) tb_uas(j,4)=tb_uas(j,4)-65536
        if ( tb_uas(j,5) .gt. 6000  ) tb_uas(j,5)=tb_uas(j,5)-65536

        tb_uas(j,1)=tb_uas(j,1)*0.01+CTOK
        tb_uas(j,2)=tb_uas(j,2)*0.01+CTOK
        tb_uas(j,3)=tb_uas(j,3)*0.01+CTOK
        tb_uas(j,4)=tb_uas(j,4)*0.01+CTOK
        tb_uas(j,5)=tb_uas(j,5)*0.01+CTOK

        angle_uas(j) = 53.

        lat_uas(j) = ichar(uas(i,j)%lat(1))*256 + ichar(uas(i,j)%lat(2))
        lon_uas(j) = ichar(uas(i,j)%lon(1))*256 + ichar(uas(i,j)%lon(2))

        if ( lat_uas(j) .gt. 9000  ) lat_uas(j)=lat_uas(j)-65536
        if ( lon_uas(j) .gt. 18000 ) lon_uas(j)=lon_uas(j)-65536

        lat_uas(j) = lat_uas(j) * 0.01
        lon_uas(j) = lon_uas(j) * 0.01

        RelAziAngle_uas(j) = -999.
        SolZenAngle_uas(j) = -999.

      enddo


      !---- write scan ---------------------------------------------------------

      call WriteRadMeasScanLMode_ascii(iu_img,nqc,qc,nchan_img,nFovs_img,angle_img,tb_img,lat_img,lon_img,node,&
        scanUTC,scanDAY,scanYear,RelAziAngle_img,SolZenAngle_img)

      call WriteRadMeasScanLMode_ascii(iu_env,nqc,qc,nchan_env,nFovs_env,angle_env,tb_env,lat_env,lon_env,node,&
        scanUTC,scanDAY,scanYear,RelAziAngle_env,SolZenAngle_env)

      call WriteRadMeasScanLMode_ascii(iu_las,nqc,qc,nchan_las,nFovs_las,angle_las,tb_las,lat_las,lon_las,node,&
        scanUTC,scanDAY,scanYear,RelAziAngle_las,SolZenAngle_las)
     
      call WriteRadMeasScanLMode_ascii(iu_uas,nqc,qc,nchan_uas,nFovs_uas,angle_uas,tb_uas,lat_uas,lon_uas,node,&
        scanUTC,scanDAY,scanYear,RelAziAngle_uas,SolZenAngle_uas)
     
    
    end do SCAN_LOOPS
    !---- end loop over the scan lines
  
  END DO FILE_LOOPS
  
  !---- release memory
  DEALLOCATE(rdrFiles)
  DEALLOCATE(imgFiles)
  DEALLOCATE(envFiles)
  DEALLOCATE(lasFiles)
  DEALLOCATE(uasFiles)
  

END PROGRAM rdr2tdr_f16_ssmis


