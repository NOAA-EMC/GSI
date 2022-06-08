!$$$ module documentation block
!           .      .    .                                       .
! module:   da_adBuffer
!  prgmmr: 
!
! abstract: Utility routines for adjoint code
!           Implemented as a collection of subroutines, functions, common blocks, and block data
!           (not an f90 module)
!
! program history log:
!
! subroutines included:
!   sub PUSHCHARACTER
!   sub LOOKCHARACTER
!   sub POPCHARACTER
!   sub PUSHBOOLEAN
!   sub LOOKBOOLEAN
!   sub POPBOOLEAN
!   sub PUSHINTEGER4
!   sub LOOKINTEGER4
!   sub POPINTEGER4
!   sub PUSHINTEGER8
!   sub LOOKINTEGER8
!   sub POPINTEGER8
!   sub PUSHREAL4
!   sub LOOKREAL4
!   sub POPREAL4
!   sub PUSHREAL8
!   sub LOOKREAL8
!   sub POPREAL8
!   sub PUSHREAL16
!   sub LOOKREAL16
!   sub POPREAL16
!   sub PUSHCOMPLEX8
!   sub LOOKCOMPLEX8
!   sub POPCOMPLEX8
!   sub PUSHCOMPLEX16
!   sub LOOKCOMPLEX16
!   sub POPCOMPLEX16
!
! variable definition:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

      BLOCK DATA CHARACTERS
      CHARACTER ads1buf(512), ads1lbuf(512)
      INTEGER(i_short) ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf,                                &
     &       ads1ibuf,ads1ilbuf,ads1inlbuf
      DATA ads1ibuf/1/
      DATA ads1ilbuf/-1/
      DATA ads1inlbuf/.FALSE./
      END

      SUBROUTINE PUSHCHARACTER(x)
      CHARACTER x, ads1buf(512), ads1lbuf(512)
      INTEGER(i_short) ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf,                                &
     &       ads1ibuf,ads1ilbuf,ads1inlbuf
      IF (ads1ilbuf /= -1) THEN
         ads1ilbuf = -1
         ads1inlbuf = .FALSE.
      ENDIF
      IF (ads1ibuf >= 512) THEN
         ads1buf(512) = x
         CALL PUSHCHARACTERARRAY(ads1buf, 512)
         ads1ibuf = 1
      ELSE
         ads1buf(ads1ibuf) = x
         ads1ibuf = ads1ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKCHARACTER(x)
      CHARACTER x, ads1buf(512), ads1lbuf(512)
      INTEGER(i_short) ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf,                                &
     &       ads1ibuf,ads1ilbuf,ads1inlbuf
      IF (ads1ilbuf == -1) THEN
         ads1ilbuf=ads1ibuf
         call RESETADLOOKSTACK()
      ENDIF
      IF (ads1ilbuf <= 1) THEN
         CALL LOOKCHARACTERARRAY(ads1lbuf, 512)
         ads1inlbuf = .TRUE.
         ads1ilbuf = 512
         x = ads1lbuf(512)
      ELSE
         ads1ilbuf = ads1ilbuf-1
         if (ads1inlbuf) THEN
            x = ads1lbuf(ads1ilbuf)
         ELSE
            x = ads1buf(ads1ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPCHARACTER(x)
      CHARACTER x, ads1buf(512), ads1lbuf(512)
      INTEGER(i_short) ads1ibuf,ads1ilbuf
      LOGICAL ads1inlbuf
      COMMON /ads1fbuf/ads1buf,ads1lbuf,                                &
     &       ads1ibuf,ads1ilbuf,ads1inlbuf
      IF (ads1ilbuf /= -1) THEN
         ads1ilbuf = -1
         ads1inlbuf = .FALSE.
      ENDIF
      IF (ads1ibuf <= 1) THEN
         CALL POPCHARACTERARRAY(ads1buf, 512)
         ads1ibuf = 512
         x = ads1buf(512)
      ELSE
         ads1ibuf = ads1ibuf-1
         x = ads1buf(ads1ibuf)
      ENDIF
      END

      BLOCK DATA BOOLEANS
      LOGICAL adl4buf(512), adl4lbuf(512)
      INTEGER(i_short) adl4ibuf,adl4ilbuf
      LOGICAL adl4inlbuf
      COMMON /adl4fbuf/adl4buf,adl4lbuf,                                &
     &       adl4ibuf,adl4ilbuf,adl4inlbuf
      DATA adl4ibuf/1/
      DATA adl4ilbuf/-1/
      DATA adl4inlbuf/.FALSE./
      END

      SUBROUTINE PUSHBOOLEAN(x)
      LOGICAL x, adl4buf(512), adl4lbuf(512)
      INTEGER(i_short) adl4ibuf,adl4ilbuf
      LOGICAL adl4inlbuf
      COMMON /adl4fbuf/adl4buf,adl4lbuf,                                &
     &       adl4ibuf,adl4ilbuf,adl4inlbuf
      IF (adl4ilbuf /= -1) THEN
         adl4ilbuf = -1
         adl4inlbuf = .FALSE.
      ENDIF
      IF (adl4ibuf >= 512) THEN
         adl4buf(512) = x
         CALL PUSHBOOLEANARRAY(adl4buf, 512)
         adl4ibuf = 1
      ELSE
         adl4buf(adl4ibuf) = x
         adl4ibuf = adl4ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKBOOLEAN(x)
      LOGICAL x, adl4buf(512), adl4lbuf(512)
      INTEGER(i_short) adl4ibuf,adl4ilbuf
      LOGICAL adl4inlbuf
      COMMON /adl4fbuf/adl4buf,adl4lbuf,                                &
     &       adl4ibuf,adl4ilbuf,adl4inlbuf
      IF (adl4ilbuf == -1) THEN
         adl4ilbuf=adl4ibuf
         call RESETADLOOKSTACK()
      ENDIF
      IF (adl4ilbuf <= 1) THEN
         CALL LOOKBOOLEANARRAY(adl4lbuf, 512)
         adl4inlbuf = .TRUE.
         adl4ilbuf = 512
         x = adl4lbuf(512)
      ELSE
         adl4ilbuf = adl4ilbuf-1
         if (adl4inlbuf) THEN
            x = adl4lbuf(adl4ilbuf)
         ELSE
            x = adl4buf(adl4ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPBOOLEAN(x)
      LOGICAL x, adl4buf(512), adl4lbuf(512)
      INTEGER(i_short) adl4ibuf,adl4ilbuf
      LOGICAL adl4inlbuf
      COMMON /adl4fbuf/adl4buf,adl4lbuf,                                &
     &       adl4ibuf,adl4ilbuf,adl4inlbuf
      IF (adl4ilbuf /= -1) THEN
         adl4ilbuf = -1
         adl4inlbuf = .FALSE.
      ENDIF
      IF (adl4ibuf <= 1) THEN
         CALL POPBOOLEANARRAY(adl4buf, 512)
         adl4ibuf = 512
         x = adl4buf(512)
      ELSE
         adl4ibuf = adl4ibuf-1
         x = adl4buf(adl4ibuf)
      ENDIF
      END

      BLOCK DATA INTEGERS4
      INTEGER(I_SHORT) adi4buf(512), adi4lbuf(512)
      INTEGER(i_short) adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf,                                &
     &       adi4ibuf,adi4ilbuf,adi4inlbuf
      DATA adi4ibuf/1/
      DATA adi4ilbuf/-1/
      DATA adi4inlbuf/.FALSE./
      END

      SUBROUTINE PUSHINTEGER4(x)
      INTEGER(I_SHORT) x, adi4buf(512), adi4lbuf(512)
      INTEGER(i_short) adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf,                                &
     &       adi4ibuf,adi4ilbuf,adi4inlbuf
      IF (adi4ilbuf /= -1) THEN
         adi4ilbuf = -1
         adi4inlbuf = .FALSE.
      ENDIF
      IF (adi4ibuf >= 512) THEN
         adi4buf(512) = x
         CALL PUSHINTEGER4ARRAY(adi4buf, 512)
         adi4ibuf = 1
      ELSE
         adi4buf(adi4ibuf) = x
         adi4ibuf = adi4ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKINTEGER4(x)
      INTEGER(I_SHORT) x, adi4buf(512), adi4lbuf(512)
      INTEGER(i_short) adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf,                                &
     &       adi4ibuf,adi4ilbuf,adi4inlbuf
      IF (adi4ilbuf == -1) THEN
         adi4ilbuf=adi4ibuf
         call RESETADLOOKSTACK()
      ENDIF
      IF (adi4ilbuf <= 1) THEN
         CALL LOOKINTEGER4ARRAY(adi4lbuf, 512)
         adi4inlbuf = .TRUE.
         adi4ilbuf = 512
         x = adi4lbuf(512)
      ELSE
         adi4ilbuf = adi4ilbuf-1
         if (adi4inlbuf) THEN
            x = adi4lbuf(adi4ilbuf)
         ELSE
            x = adi4buf(adi4ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPINTEGER4(x)
      INTEGER(I_SHORT) x, adi4buf(512), adi4lbuf(512)
      INTEGER(i_short) adi4ibuf,adi4ilbuf
      LOGICAL adi4inlbuf
      COMMON /adi4fbuf/adi4buf,adi4lbuf,                                &
     &       adi4ibuf,adi4ilbuf,adi4inlbuf
      IF (adi4ilbuf /= -1) THEN
         adi4ilbuf = -1
         adi4inlbuf = .FALSE.
      ENDIF
      IF (adi4ibuf <= 1) THEN
         CALL POPINTEGER4ARRAY(adi4buf, 512)
         adi4ibuf = 512
         x = adi4buf(512)
      ELSE
         adi4ibuf = adi4ibuf-1
         x = adi4buf(adi4ibuf)
      ENDIF
      END

      BLOCK DATA INTEGERS8
      INTEGER(I_LONG) adi8buf(512), adi8lbuf(512)
      INTEGER(i_short) adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf,                                &
     &       adi8ibuf,adi8ilbuf,adi8inlbuf
      DATA adi8ibuf/1/
      DATA adi8ilbuf/-1/
      DATA adi8inlbuf/.FALSE./
      END

      SUBROUTINE PUSHINTEGER8(x)
      INTEGER(I_LONG) x, adi8buf(512), adi8lbuf(512)
      INTEGER(i_short) adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf,                                &
     &       adi8ibuf,adi8ilbuf,adi8inlbuf
      IF (adi8ilbuf /= -1) THEN
         adi8ilbuf = -1
         adi8inlbuf = .FALSE.
      ENDIF
      IF (adi8ibuf >= 512) THEN
         adi8buf(512) = x
         CALL PUSHINTEGER8ARRAY(adi8buf, 512)
         adi8ibuf = 1
      ELSE
         adi8buf(adi8ibuf) = x
         adi8ibuf = adi8ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKINTEGER8(x)
      INTEGER(I_LONG) x, adi8buf(512), adi8lbuf(512)
      INTEGER(i_short) adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf,                                &
     &       adi8ibuf,adi8ilbuf,adi8inlbuf
      IF (adi8ilbuf == -1) THEN
         adi8ilbuf=adi8ibuf
         call RESETADLOOKSTACK()
      ENDIF
      IF (adi8ilbuf <= 1) THEN
         CALL LOOKINTEGER8ARRAY(adi8lbuf, 512)
         adi8inlbuf = .TRUE.
         adi8ilbuf = 512
         x = adi8lbuf(512)
      ELSE
         adi8ilbuf = adi8ilbuf-1
         if (adi8inlbuf) THEN
            x = adi8lbuf(adi8ilbuf)
         ELSE
            x = adi8buf(adi8ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPINTEGER8(x)
      INTEGER(I_LONG) x, adi8buf(512), adi8lbuf(512)
      INTEGER(i_short) adi8ibuf,adi8ilbuf
      LOGICAL adi8inlbuf
      COMMON /adi8fbuf/adi8buf,adi8lbuf,                                &
     &       adi8ibuf,adi8ilbuf,adi8inlbuf
      IF (adi8ilbuf /= -1) THEN
         adi8ilbuf = -1
         adi8inlbuf = .FALSE.
      ENDIF
      IF (adi8ibuf <= 1) THEN
         CALL POPINTEGER8ARRAY(adi8buf, 512)
         adi8ibuf = 512
         x = adi8buf(512)
      ELSE
         adi8ibuf = adi8ibuf-1
         x = adi8buf(adi8ibuf)
      ENDIF
      END

      BLOCK DATA REALS4
      REAL(R_SINGLE) adr4buf(512), adr4lbuf(512)
      INTEGER(i_short) adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf,                                &
     &       adr4ibuf,adr4ilbuf,adr4inlbuf
      DATA adr4ibuf/1/
      DATA adr4ilbuf/-1/
      DATA adr4inlbuf/.FALSE./
      END

      SUBROUTINE PUSHREAL4(x)
      REAL(R_SINGLE) x, adr4buf(512), adr4lbuf(512)
      INTEGER(i_short) adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf,                                &
     &       adr4ibuf,adr4ilbuf,adr4inlbuf
      IF (adr4ilbuf /= -1) THEN
         adr4ilbuf = -1
         adr4inlbuf = .FALSE.
      ENDIF
      IF (adr4ibuf >= 512) THEN
         adr4buf(512) = x
         CALL PUSHREAL4ARRAY(adr4buf, 512)
         adr4ibuf = 1
      ELSE
         adr4buf(adr4ibuf) = x
         adr4ibuf = adr4ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKREAL4(x)
      REAL(R_SINGLE) x, adr4buf(512), adr4lbuf(512)
      INTEGER(i_short) adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf,                                &
     &       adr4ibuf,adr4ilbuf,adr4inlbuf
      IF (adr4ilbuf == -1) THEN
         adr4ilbuf=adr4ibuf
         call RESETADLOOKSTACK()
      ENDIF
      IF (adr4ilbuf <= 1) THEN
         CALL LOOKREAL4ARRAY(adr4lbuf, 512)
         adr4inlbuf = .TRUE.
         adr4ilbuf = 512
         x = adr4lbuf(512)
      ELSE
         adr4ilbuf = adr4ilbuf-1
         if (adr4inlbuf) THEN
            x = adr4lbuf(adr4ilbuf)
         ELSE
            x = adr4buf(adr4ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPREAL4(x)
      REAL(R_SINGLE) x, adr4buf(512), adr4lbuf(512)
      INTEGER(i_short) adr4ibuf,adr4ilbuf
      LOGICAL adr4inlbuf
      COMMON /adr4fbuf/adr4buf,adr4lbuf,                                &
     &       adr4ibuf,adr4ilbuf,adr4inlbuf
      IF (adr4ilbuf /= -1) THEN
         adr4ilbuf = -1
         adr4inlbuf = .FALSE.
      ENDIF
      IF (adr4ibuf <= 1) THEN
         CALL POPREAL4ARRAY(adr4buf, 512)
         adr4ibuf = 512
         x = adr4buf(512)
      ELSE
         adr4ibuf = adr4ibuf-1
         x = adr4buf(adr4ibuf)
      ENDIF
      END

      BLOCK DATA REALS8
      REAL(r_kind) adr8buf(512), adr8lbuf(512)
      INTEGER(i_short) adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf,                                &
     &       adr8ibuf,adr8ilbuf,adr8inlbuf
      DATA adr8ibuf/1/
      DATA adr8ilbuf/-1/
      DATA adr8inlbuf/.FALSE./
      END

      SUBROUTINE PUSHREAL8(x)
      REAL(r_kind) x, adr8buf(512), adr8lbuf(512)
      INTEGER(i_short) adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf,                                &
     &       adr8ibuf,adr8ilbuf,adr8inlbuf
      IF (adr8ilbuf /= -1) THEN
         adr8ilbuf = -1
         adr8inlbuf = .FALSE.
      ENDIF
      IF (adr8ibuf >= 512) THEN
         adr8buf(512) = x
         CALL PUSHREAL8ARRAY(adr8buf, 512)
         adr8ibuf = 1
      ELSE
         adr8buf(adr8ibuf) = x
         adr8ibuf = adr8ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKREAL8(x)
      REAL(r_kind) x, adr8buf(512), adr8lbuf(512)
      INTEGER(i_short) adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf,                                &
     &       adr8ibuf,adr8ilbuf,adr8inlbuf
      IF (adr8ilbuf == -1) THEN
         adr8ilbuf=adr8ibuf
         call RESETADLOOKSTACK()
      ENDIF
      IF (adr8ilbuf <= 1) THEN
         CALL LOOKREAL8ARRAY(adr8lbuf, 512)
         adr8inlbuf = .TRUE.
         adr8ilbuf = 512
         x = adr8lbuf(512)
      ELSE
         adr8ilbuf = adr8ilbuf-1
         if (adr8inlbuf) THEN
            x = adr8lbuf(adr8ilbuf)
         ELSE
            x = adr8buf(adr8ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPREAL8(x)
      REAL(r_kind) x, adr8buf(512), adr8lbuf(512)
      INTEGER(i_short) adr8ibuf,adr8ilbuf
      LOGICAL adr8inlbuf
      COMMON /adr8fbuf/adr8buf,adr8lbuf,                                &
     &       adr8ibuf,adr8ilbuf,adr8inlbuf
      IF (adr8ilbuf /= -1) THEN
         adr8ilbuf = -1
         adr8inlbuf = .FALSE.
      ENDIF
      IF (adr8ibuf <= 1) THEN
         CALL POPREAL8ARRAY(adr8buf, 512)
         adr8ibuf = 512
         x = adr8buf(512)
      ELSE
         adr8ibuf = adr8ibuf-1
         x = adr8buf(adr8ibuf)
      ENDIF
      END

      BLOCK DATA REALS16
      DOUBLE PRECISION adr16buf(512), adr16lbuf(512)
      INTEGER(i_short) adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf,                             &
     &       adr16ibuf,adr16ilbuf,adr16inlbuf
      DATA adr16ibuf/1/
      DATA adr16ilbuf/-1/
      DATA adr16inlbuf/.FALSE./
      END

      SUBROUTINE PUSHREAL16(x)
      DOUBLE PRECISION x, adr16buf(512), adr16lbuf(512)
      INTEGER(i_short) adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf,                             &
     &       adr16ibuf,adr16ilbuf,adr16inlbuf
      IF (adr16ilbuf /= -1) THEN
         adr16ilbuf = -1
         adr16inlbuf = .FALSE.
      ENDIF
      IF (adr16ibuf >= 512) THEN
         adr16buf(512) = x
         CALL PUSHREAL16ARRAY(adr16buf, 512)
         adr16ibuf = 1
      ELSE
         adr16buf(adr16ibuf) = x
         adr16ibuf = adr16ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKREAL16(x)
      DOUBLE PRECISION x, adr16buf(512), adr16lbuf(512)
      INTEGER(i_short) adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf,                             &
     &       adr16ibuf,adr16ilbuf,adr16inlbuf
      IF (adr16ilbuf == -1) THEN
         adr16ilbuf=adr16ibuf
         call RESETADLOOKSTACK()
      ENDIF
      IF (adr16ilbuf <= 1) THEN
         CALL LOOKREAL16ARRAY(adr16lbuf, 512)
         adr16inlbuf = .TRUE.
         adr16ilbuf = 512
         x = adr16lbuf(512)
      ELSE
         adr16ilbuf = adr16ilbuf-1
         if (adr16inlbuf) THEN
            x = adr16lbuf(adr16ilbuf)
         ELSE
            x = adr16buf(adr16ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPREAL16(x)
      DOUBLE PRECISION x, adr16buf(512), adr16lbuf(512)
      INTEGER(i_short) adr16ibuf,adr16ilbuf
      LOGICAL adr16inlbuf
      COMMON /adr16fbuf/adr16buf,adr16lbuf,                             &
     &       adr16ibuf,adr16ilbuf,adr16inlbuf
      IF (adr16ilbuf /= -1) THEN
         adr16ilbuf = -1
         adr16inlbuf = .FALSE.
      ENDIF
      IF (adr16ibuf <= 1) THEN
         CALL POPREAL16ARRAY(adr16buf, 512)
         adr16ibuf = 512
         x = adr16buf(512)
      ELSE
         adr16ibuf = adr16ibuf-1
         x = adr16buf(adr16ibuf)
      ENDIF
      END

      BLOCK DATA COMPLEXS8
      COMPLEX*8 adc8buf(512), adc8lbuf(512)
      INTEGER(i_short) adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf,                                &
     &       adc8ibuf,adc8ilbuf,adc8inlbuf
      DATA adc8ibuf/1/
      DATA adc8ilbuf/-1/
      DATA adc8inlbuf/.FALSE./
      END

      SUBROUTINE PUSHCOMPLEX8(x)
      COMPLEX*8 x, adc8buf(512), adc8lbuf(512)
      INTEGER(i_short) adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf,                                &
     &       adc8ibuf,adc8ilbuf,adc8inlbuf
      IF (adc8ilbuf /= -1) THEN
         adc8ilbuf = -1
         adc8inlbuf = .FALSE.
      ENDIF
      IF (adc8ibuf >= 512) THEN
         adc8buf(512) = x
         CALL PUSHCOMPLEX8ARRAY(adc8buf, 512)
         adc8ibuf = 1
      ELSE
         adc8buf(adc8ibuf) = x
         adc8ibuf = adc8ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKCOMPLEX8(x)
      COMPLEX*8 x, adc8buf(512), adc8lbuf(512)
      INTEGER(i_short) adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf,                                &
     &       adc8ibuf,adc8ilbuf,adc8inlbuf
      IF (adc8ilbuf == -1) THEN
         adc8ilbuf=adc8ibuf
         call RESETADLOOKSTACK()
      ENDIF
      IF (adc8ilbuf <= 1) THEN
         CALL LOOKCOMPLEX8ARRAY(adc8lbuf, 512)
         adc8inlbuf = .TRUE.
         adc8ilbuf = 512
         x = adc8lbuf(512)
      ELSE
         adc8ilbuf = adc8ilbuf-1
         if (adc8inlbuf) THEN
            x = adc8lbuf(adc8ilbuf)
         ELSE
            x = adc8buf(adc8ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPCOMPLEX8(x)
      COMPLEX*8 x, adc8buf(512), adc8lbuf(512)
      INTEGER(i_short) adc8ibuf,adc8ilbuf
      LOGICAL adc8inlbuf
      COMMON /adc8fbuf/adc8buf,adc8lbuf,                                &
     &       adc8ibuf,adc8ilbuf,adc8inlbuf
      IF (adc8ilbuf /= -1) THEN
         adc8ilbuf = -1
         adc8inlbuf = .FALSE.
      ENDIF
      IF (adc8ibuf <= 1) THEN
         CALL POPCOMPLEX8ARRAY(adc8buf, 512)
         adc8ibuf = 512
         x = adc8buf(512)
      ELSE
         adc8ibuf = adc8ibuf-1
         x = adc8buf(adc8ibuf)
      ENDIF
      END

      BLOCK DATA COMPLEXS16
      COMPLEX*16 adc16buf(512), adc16lbuf(512)
      INTEGER(i_short) adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf,                             &
     &       adc16ibuf,adc16ilbuf,adc16inlbuf
      DATA adc16ibuf/1/
      DATA adc16ilbuf/-1/
      DATA adc16inlbuf/.FALSE./
      END

      SUBROUTINE PUSHCOMPLEX16(x)
      COMPLEX*16 x, adc16buf(512), adc16lbuf(512)
      INTEGER(i_short) adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf,                             &
     &       adc16ibuf,adc16ilbuf,adc16inlbuf
      IF (adc16ilbuf /= -1) THEN
         adc16ilbuf = -1
         adc16inlbuf = .FALSE.
      ENDIF
      IF (adc16ibuf >= 512) THEN
         adc16buf(512) = x
         CALL PUSHCOMPLEX16ARRAY(adc16buf, 512)
         adc16ibuf = 1
      ELSE
         adc16buf(adc16ibuf) = x
         adc16ibuf = adc16ibuf+1
      ENDIF
      END

      SUBROUTINE LOOKCOMPLEX16(x)
      COMPLEX*16 x, adc16buf(512), adc16lbuf(512)
      INTEGER(i_short) adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf,                             &
     &       adc16ibuf,adc16ilbuf,adc16inlbuf
      IF (adc16ilbuf == -1) THEN
         adc16ilbuf=adc16ibuf
         call RESETADLOOKSTACK()
      ENDIF
      IF (adc16ilbuf <= 1) THEN
         CALL LOOKCOMPLEX16ARRAY(adc16lbuf, 512)
         adc16inlbuf = .TRUE.
         adc16ilbuf = 512
         x = adc16lbuf(512)
      ELSE
         adc16ilbuf = adc16ilbuf-1
         if (adc16inlbuf) THEN
            x = adc16lbuf(adc16ilbuf)
         ELSE
            x = adc16buf(adc16ilbuf)
         ENDIF
      ENDIF
      END

      SUBROUTINE POPCOMPLEX16(x)
      COMPLEX*16 x, adc16buf(512), adc16lbuf(512)
      INTEGER(i_short) adc16ibuf,adc16ilbuf
      LOGICAL adc16inlbuf
      COMMON /adc16fbuf/adc16buf,adc16lbuf,                             &
     &       adc16ibuf,adc16ilbuf,adc16inlbuf
      IF (adc16ilbuf /= -1) THEN
         adc16ilbuf = -1
         adc16inlbuf = .FALSE.
      ENDIF
      IF (adc16ibuf <= 1) THEN
         CALL POPCOMPLEX16ARRAY(adc16buf, 512)
         adc16ibuf = 512
         x = adc16buf(512)
      ELSE
         adc16ibuf = adc16ibuf-1
         x = adc16buf(adc16ibuf)
      ENDIF
      END

