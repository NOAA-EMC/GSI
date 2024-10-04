Subroutine  nc_get_latlon(ncfile,mscNlon,mscNlat,mscValueLAT,mscValueLON)
!
!  Author: Ming Hu, ESRL/GSD
!  
!  First written: 12/16/2007.
!
!  IN:
!     mscNlon
!     mscNlan
!     NCID
!  out:
!     mscValueLAT
!     mscValueLON
!
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER*120    ncfile

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data

  INTEGER ::  NCID, STATUS, MSLATID,MSLONID

  INTEGER ::   NDIMS
  PARAMETER (NDIMS=3)                  ! number of dimensions
  INTEGER START(NDIMS), COUNT(NDIMS)

  REAL ::   mscValueLAT(mscNlon,mscNlat,1)
  REAL ::   mscValueLON(mscNlon,mscNlat,1)
  INTEGER :: i,j

  START(1)=1
  START(2)=1
  START(3)=1
  COUNT(1)=mscNlon
  COUNT(2)=mscNlat
  COUNT(3)=1
  STATUS = NF_OPEN(trim(ncfile), 0, NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)

  STATUS = NF_INQ_VARID (NCID, 'XLAT', MSLATID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)
  STATUS = NF_GET_VARA_REAL (NCID, MSLATID, START, COUNT, mscValueLAT)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)

  STATUS = NF_INQ_VARID (NCID, 'XLONG', MSLONID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)
  STATUS = NF_GET_VARA_REAL (NCID, MSLONID, START, COUNT, mscValueLON)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)

  STATUS = NF_CLOSE(NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS) 

end subroutine nc_get_latlon

Subroutine  nc_get_dim(ncfile,LONLEN,LATLEN)
!
!  
!  First written: 12/16/2007.
!
!
!  IN:
!     ncfile : name of mosaic file
!  OUT
!     LONLEN
!     LATLEN
!
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER*120    ncfile

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data

  INTEGER ::  NCID, STATUS
  INTEGER ::  LONID, LATID
  INTEGER ::  LONLEN, LATLEN

  STATUS = NF_OPEN(trim(ncfile), 0, NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)

  STATUS = NF_INQ_DIMID(NCID, 'west_east', LONID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)
  STATUS = NF_INQ_DIMID(NCID, 'south_north', LATID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)

  STATUS = NF_INQ_DIMLEN(NCID, LONID, LONLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)
  STATUS = NF_INQ_DIMLEN(NCID, LATID, LATLEN)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS)

  STATUS = NF_CLOSE(NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_geo(STATUS) 

END SUBROUTINE nc_get_dim

SUBROUTINE HANDLE_ERR_geo(STATUS)
     INCLUDE 'netcdf.inc'
     INTEGER STATUS
     IF (STATUS .NE. NF_NOERR) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
     ENDIF
END SUBROUTINE HANDLE_ERR_geo
