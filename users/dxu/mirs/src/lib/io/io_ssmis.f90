!$Id: io_ssmis.f90 975 2008-04-29 18:54:05Z sidb $
!-----------------------------------------------------------------------------------------------
! Name:        io_ssmis
! 
! Type:        F90 module
!
! Description:
!        Module that contains the necessary routine to read calibrated SSMIS TDR
!
! Modules needed:
!       - none
!
! Subroutines contained:
!       - ReadCalibratedSsmisTdr
!
! Data type included:
!       - rev_structure
!       - scan_structure
!       - ephem__structure
!       - aux_structure
!       - img_structure
!       - env_structure
!       - las_structure
!       - uas_structure
! 
! History:
!       2008     Wanchun Chen PSGS Inc. @ NOAA/NESDIS/STAR
!
!-----------------------------------------------------------------------------------------------

MODULE io_ssmis
  
IMPLICIT NONE
!  Array Parameters
INTEGER(2), PARAMETER :: NSCENES_IMG=180, NSCENES_ENV=90, NSCENES_LAS=60, NSCENES_UAS=30
INTEGER(2), PARAMETER :: NBASE_PTS=28, NCAL_PTS=24
INTEGER(2), PARAMETER :: MAX_NPOINTS=9000
REAL, PARAMETER       :: CTOK=273.15

!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! rev_hdr structure
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TYPE :: rev_structure
  INTEGER(4) :: finfo
  INTEGER(4) :: rev_no
  CHARACTER(1),DIMENSION(4) :: year
  CHARACTER(1),DIMENSION(2) :: jul_day
  INTEGER(1) :: hour
  INTEGER(1) :: minute
  INTEGER(2) :: sat_id
  CHARACTER(1),DIMENSION(2) :: num_scans
  INTEGER(4), DIMENSION(5) :: spare
END TYPE rev_structure

!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! scan_hdr structure
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TYPE :: scan_structure
  CHARACTER(1),DIMENSION(4) :: year
  CHARACTER(1),DIMENSION(2) :: jday
  INTEGER(1) :: hour
  INTEGER(1) :: minute
  INTEGER(2) :: spare1
  INTEGER(2) :: scan_no
  CHARACTER(1),DIMENSION(4) :: st_time
  INTEGER(4), DIMENSION(5) :: spare2
END TYPE scan_structure

!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! ephem structure
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TYPE :: ephem_structure
  INTEGER(4) :: lat
  INTEGER(4) :: lon
  INTEGER(4) :: alt
  INTEGER(4) :: jd
  INTEGER(4) :: sec
END TYPE ephem_structure

!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! IMG structure
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TYPE :: img_structure
  CHARACTER(1),DIMENSION(2) :: lat
  CHARACTER(1),DIMENSION(2) :: lon
  INTEGER(2) :: scene
  INTEGER(1) :: surf
  INTEGER(1) :: rain
  CHARACTER(1),DIMENSION(2) :: ch8
  CHARACTER(1),DIMENSION(2) :: ch9
  CHARACTER(1),DIMENSION(2) :: ch10
  CHARACTER(1),DIMENSION(2) :: ch11
  CHARACTER(1),DIMENSION(2) :: lat17
  CHARACTER(1),DIMENSION(2) :: lon17
  CHARACTER(1),DIMENSION(2) :: ch17
  CHARACTER(1),DIMENSION(2) :: ch18
END TYPE img_structure

!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! ENV structure
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TYPE :: env_structure
  CHARACTER(1),DIMENSION(2) :: lat
  CHARACTER(1),DIMENSION(2) :: lon
  INTEGER(1) :: scene
  INTEGER(1) :: surf
  CHARACTER(1),DIMENSION(2) :: ch12
  CHARACTER(1),DIMENSION(2) :: ch13
  CHARACTER(1),DIMENSION(2) :: ch14
  CHARACTER(1),DIMENSION(2) :: lat15
  CHARACTER(1),DIMENSION(2) :: lon15
  CHARACTER(1),DIMENSION(2) :: ch15
  CHARACTER(1),DIMENSION(2) :: ch16
END TYPE env_structure

!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! LAS structure
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TYPE :: las_structure
  CHARACTER(1),DIMENSION(2) :: lat
  CHARACTER(1),DIMENSION(2) :: lon
  INTEGER(2) :: scene
  INTEGER(2) :: surf
  CHARACTER(1),DIMENSION(2) :: ch1
  CHARACTER(1),DIMENSION(2) :: ch2
  CHARACTER(1),DIMENSION(2) :: ch3
  CHARACTER(1),DIMENSION(2) :: ch4
  CHARACTER(1),DIMENSION(2) :: ch5
  CHARACTER(1),DIMENSION(2) :: ch6
  CHARACTER(1),DIMENSION(2) :: ch7
  CHARACTER(1),DIMENSION(2) :: ch24
END TYPE las_structure

!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! UAS structure
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TYPE :: uas_structure
  CHARACTER(1),DIMENSION(2) :: lat
  CHARACTER(1),DIMENSION(2) :: lon
  INTEGER(2) :: scene
  CHARACTER(1),DIMENSION(2) :: ch19
  CHARACTER(1),DIMENSION(2) :: ch20
  CHARACTER(1),DIMENSION(2) :: ch21
  CHARACTER(1),DIMENSION(2) :: ch22
  CHARACTER(1),DIMENSION(2) :: ch23
END TYPE uas_structure

!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
! aux structure
!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TYPE :: aux_structure
  CHARACTER(1),DIMENSION(2, NCAL_PTS) :: warm_cal
  CHARACTER(1),DIMENSION(2, NCAL_PTS) :: cold_cal
  CHARACTER(1),DIMENSION(2, 3) :: warm_load
  CHARACTER(1),DIMENSION(2) :: subfr_id
  CHARACTER(1),DIMENSION(2, 4) :: mux_data
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lat_k
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lon_k
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_eia_k
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_azm_k
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lat_uv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lon_uv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_eia_uv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_azm_uv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lat_w
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lon_w
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_eia_w
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_azm_w
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lat_g
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lon_g
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_eia_g
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_azm_g
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lat_lv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lon_lv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_eia_lv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_azm_lv
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lat_ka
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_lon_ka
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_eia_ka
  INTEGER(2), DIMENSION(NBASE_PTS) :: bp_azm_ka
END TYPE aux_structure

!---INTRINSIC functions used in this module
INTRINSIC :: ICHAR,TRIM


CONTAINS
  
SUBROUTINE ReadCalibratedSsmisTdr(iu,inputFile,  rev_hdr, year, jday, hour, minute, nscans, &
      scan_hdr,ephem,img,env,las,uas,aux, accessStr, formStr)

  INTEGER             :: iu, i, j, ierror
  CHARACTER(len=250)  :: InputFile
  TYPE(rev_structure) :: rev_hdr
  INTEGER             :: year, jday, hour, minute, nscans
  
  character(len=16)   :: accessStr, formStr
  
  TYPE (scan_structure),  DIMENSION(MAX_NPOINTS) :: scan_hdr
  TYPE (ephem_structure), DIMENSION(MAX_NPOINTS,3) :: ephem
  TYPE (img_structure),   DIMENSION(MAX_NPOINTS,NSCENES_IMG) :: img
  TYPE (env_structure),   DIMENSION(MAX_NPOINTS,NSCENES_ENV) :: env
  TYPE (las_structure),   DIMENSION(MAX_NPOINTS,NSCENES_LAS) :: las
  TYPE (uas_structure),   DIMENSION(MAX_NPOINTS,NSCENES_UAS) :: uas
  TYPE (aux_structure),   DIMENSION(MAX_NPOINTS) :: aux
  
  !OPEN(iu,file=InputFile, STATUS='OLD', ACTION='READ', FORM='BINARY', IOSTAT=ierror)
  !OPEN(iu,file=InputFile, STATUS='OLD', ACTION='READ', ACCESS='STREAM', FORM='UNFORMATTED', IOSTAT=ierror)
  OPEN(iu,file=InputFile, STATUS='OLD', ACTION='READ', ACCESS=trim(accessStr), FORM=trim(formStr), IOSTAT=ierror)
  
  READ(iu)rev_hdr
    
  year = ICHAR(rev_hdr%year(1))*256**3+ICHAR(rev_hdr%year(2))*256**2 &
        +ICHAR(rev_hdr%year(3))*256+ICHAR(rev_hdr%year(4))
  jday = ICHAR(rev_hdr%jul_day(1))*256+ICHAR(rev_hdr%jul_day(2))
  hour = rev_hdr%hour
  minute = rev_hdr%minute
  nscans = ICHAR(rev_hdr%num_scans(1))*256+ICHAR(rev_hdr%num_scans(2))
  
  !write(*,*)year, jday, hour, minute, nscans  
  
  SCAN_LOOPS: do i=1,nscans
    
    read(iu) scan_hdr(i)
    
    do j=1,3
        read(iu)ephem(i,j)
    enddo
    
    do j=1,NSCENES_IMG
        read(iu)img(i,j)
    enddo

    do j=1,NSCENES_ENV
        read(iu)env(i,j)
    enddo
    
    do j=1,NSCENES_LAS
        read(iu)las(i,j)
    enddo
    
    do j=1,NSCENES_UAS
        read(iu)uas(i,j)
    enddo
    
    read(iu) aux(i)
  
  end do SCAN_LOOPS
    
  close(iu)  
    
  RETURN
    
END SUBROUTINE ReadCalibratedSsmisTdr

END MODULE io_ssmis
