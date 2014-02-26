!--------------------------------------------------------------------------------
!M+
! NAME:
!    onelyr.f90 
!
! PURPOSE:
!   Retrieve surface emissivity and ice water path from AMSU observations at window channels
!
! CONTAINS:
!
! EXTERNALS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Banghua Yan and Fuzhong Weng, NOAA/NESDIS/STAR 
!  beta version : 2006
!  version 1    : 2007/03/30
!M-
!--------------------------------------------------------------------------------

  SUBROUTINE onelyr(load_indx,CLOUD_OPT_LUT_NAME,tbo,b89,alza,blza,emiss,ts,tc,iwp,tpw,de)

  USE CLOUD_OPT_CAL
  USE GET_EMISS_1DALG

  CHARACTER( * ), INTENT( IN ) :: CLOUD_OPT_LUT_NAME 
  INTEGER(2), PARAMETER :: nch1=6
! INTEGER(2) :: len1, len2, len3
  INTEGER(2) :: load_indx 
  REAL(4), DIMENSION(nch1) :: tbo,emiss
! REAL(4), DIMENSION(len2) :: tbo,emiss
  REAL(4) :: b89,alza,blza,ts,tc,iwp,tpw,de

  !print *,'get into onelyr!!!!!!!!!!!!!!!!!!!'

!(a) LOAD LUT DATA OF GRAUPEL OPTICAL PROPERTIES
  if(load_indx .eq. 0) then
    CALL LOAD_CLOUD_OPTICS(CLOUD_OPT_LUT_NAME,error_status)
!    print *, 'err=',error_status
  endif


!(b) Estimate surface emissivity and ice water path

    CALL EMISS_1DVAR_ALG(tbo,b89,alza,blza,emiss,ts,tc,iwp,tpw,de)
!    print *,'iwp after call:',tbo,b89,alza,blza,emiss,ts,tc,iwp,tpw,de

  END SUBROUTINE onelyr
