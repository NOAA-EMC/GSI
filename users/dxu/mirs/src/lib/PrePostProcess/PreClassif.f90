!$Id: PreClassif.f90 3387 2014-02-04 21:06:12Z chrisg $
!-----------------------------------------------------------------------------------------------
! Name:         Preclassif
! 
! Type:         F90 module
!
! Description:
!       Module containing subroutines related to the pre-classification process.
!       Note that this module could be sensor-specific, as every sensor would
!       have its own pre-classification algorithm.
!
! Modules needed:
!       - misc
!       - Consts
!
! Subroutines contained:
!       - preClassAtm
!       - preClassSfc
!
! Data type included:
!       - none
! 
! History:
!      2006   S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 
!
!-----------------------------------------------------------------------------------------------

MODULE Preclassif
  USE misc
  USE Consts
  IMPLICIT NONE
  PRIVATE
  !----Publicly available subroutine(s)
  PUBLIC :: preClassAtm,preClassSfc
  PUBLIC :: applySeaIceClimo
  !----INTRINSIC functions used in this module
  INTRINSIC :: ABS
CONTAINS

!===============================================================
! Name:         preClassAtm
!
!
! Type:         Subroutine
!
!
! Description:  Performs prec-classification (Atmospheric part)
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Year               I             Year
!       - Julday             I             Julian day
!       - NCHAN              I             Number of channels
!       - FREQ               I             Central frequencies
!       - xlat               I             Latitude
!       - xlon               I             Longitude
!       - stypeSfc           I             Ocean/Land flag
!       - SENSOR_ID          I             Sensor ID
!       - Y                  I             Brightness Temperature vector
!       - AtmClass           O             Atmospheric class determined
!
!
! Modules needed:
!       - SURFACE_CLASSIFICATION_ALG
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE preClassAtm(Year, Julday, NCHAN, FREQ, xlat, xlon, stypeSfc,SENSOR_ID,Y,AtmClass,TskPreclass)
    INTEGER               :: NCHAN
    INTEGER               :: sTypeSfc,AtmClass,SENSOR_ID,LAND
    INTEGER               :: Year,Julday
    REAL,    DIMENSION(:) :: Y,FREQ
    REAL                  :: xlat,xlon,TskPreclass
    IF (stypeSfc .ge. 0) THEN
       IF (stypeSfc .eq. OC_TYP .or. stypeSfc .eq. SEAICE_TYP ) THEN
          LAND=0
       ELSE
          LAND=1
       ENDIF
       CALL SURFACE_CLASSIFICATION_ALG(Year,Julday,SENSOR_ID,NCHAN,FREQ,Y,xlat,xlon,LAND,AtmClass,TskPreclass)
    ELSE
       AtmClass=DEFAULT_VALUE_INT
    ENDIF
    RETURN
  END SUBROUTINE preClassAtm


!===============================================================
! Name:         preClassSfc
!
!
! Type:         Subroutine
!
!
! Description:  Performs prec-classification (Surface part)
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Year               I             Year
!       - Julday             I             Julian day
!       - NCHAN              I             Number of channels
!       - FREQ               I             Central frequencies
!       - xlat               I             Latitude
!       - xlon               I             Longitude
!       - stypeSfc           I             Ocean/Land flag
!       - SENSOR_ID          I             Sensor ID
!       - Y                  I             Brightness Temperature vector
!       - SfcClass           O             Surface class determined
!
!
! Modules needed:
!       - SURFACE_CLASSIFICATION_ALG
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE preClassSfc(Year, Julday, NCHAN, FREQ, xlat,xlon,stypeSfc,SENSOR_ID,Y,SfcClass,TskPreclass)
    INTEGER               :: NCHAN
    INTEGER               :: SfcClass,stypeSfc,SENSOR_ID,LAND
    INTEGER               :: Year,Julday
    REAL,    DIMENSION(:) :: Y,FREQ
    REAL                  :: xlat,xlon,TskPreclass
    IF (stypeSfc .ge. 0) THEN
       IF (stypeSfc .eq. OC_TYP .or. stypeSfc .eq. SEAICE_TYP ) THEN
          LAND=0
       ELSE
          LAND=1
       ENDIF
       CALL SURFACE_CLASSIFICATION_ALG(Year,Julday,SENSOR_ID,NCHAN,FREQ,Y,xlat,xlon,LAND,SfcClass,TskPreclass)
    ELSE
       SfcClass=DEFAULT_VALUE_INT
    ENDIF
    RETURN
  END SUBROUTINE preClassSfc





  SUBROUTINE SURFACE_CLASSIFICATION_ALG(Year,Julday,SENSOR_ID,NCHAN,FREQ,TA,LATITUDE,LONGITUDE,LAND_INDEX,&
             NSURFACE_TYPE,TskPreclass)
    !-----------------------------------------------------------------------------------------------
    !M+
    ! NAME:
    !       SURFACE_CLASSIFICATION_ALG
    !
    ! PURPOSE:
    !       Distinguish four surface types (open ocean, sea ice, snow-free land, snow)
    !
    ! from land index and satellite-based microwave measurements of antenna/brightness
    !
    ! temperatures at window channels.
    !
    !
    ! INPUT VARIABLES:
    !
    !   Year         : Year
    !   Julday       : Julian day
    !   SENSOR_ID    : sensor ID,
    !
    !   e.g.,
    !
    !       SENSOR_ID = See Consts.f90 Module for sensor IDs
    !.....
    !
    !   NCHAN        : CHANNEL NUMBER FOR A CERTAIN SENSOR
    !
    !   FREQ(NCHAN)  : CENTRAL FREQUENCY LIST CORRESPONDING TO EACH CHANNEL AT A CERTAIN SENSOR
    !
    !   TA(1:NCHAN)  : ANTENNA TEMPEATURES AT ALL CHHANELS
    !
    !   LATITUDE     : LATITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   LATITUDE     : LONGITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   LAND_INDEX   : LAND/OCEAN INDEX (0: ocean, non-0 : land)
    !
    !
    ! OUTPUT VARIABLES:
    !
    !   NSURFACE_TYPE   : 0 (OPEN OCEAN)
    !                   : 1 (SEA ICE)
    !                   : 2 (SNOW-FREE LAND)
    !                   : 3 (SNOW)
    !
    ! LANGUAGE:
    !
    !       Fortran-95
    !
    ! CALLING SEQUENCE:
    !
    !
    ! CONTAINS:
    !
    !       N18_AMSUA_MHS_ALG      : Subroutine to distinguish four surface types from N18 AMSUA-MHS
    !
    !       F16_SSMIS_ALG          : Subroutine to distinguish four surface types from SSMIS
    !
    !       N15_AMSUAB_ALG         : Subroutine to distinguish four surface types from N15 AMSUAB
    !
    !       N16_AMSUAB_ALG         : Subroutine to distinguish four surface types from N16 AMSUAB
    !
    !       N17_AMSUAB_ALG         : Subroutine to distinguish four surface types from N17 AMSUAB
    !
    !       F15_SSMI_ALG           : Subroutine to distinguish four surface types from F15 SSMI
    !
    !       AMSRE_ALG              : Subroutine to distinguish four surface types from AMSRE
    !
    !       etc.
    !
    ! INCLUDE FILES:
    !       None.
    !
    ! EXTERNALS:
    !       None.
    !
    ! COMMON BLOCKS:
    !       None.
    !
    ! FILES ACCESSED:
    !       None.
    !
    ! CREATION HISTORY:
    !       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (03-21-2006)
    !
    !
    !       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
    !
    !   01/09/2007    :     Add descriptions of the selection of TA at window channels
    !
    !   11/23/2007    :     S.-A. Boukabara. Added first guess tskin as input, to help reduce false alarms, and
    !                       misclassifications
    !
    !  Copyright (C) 2006 Fuzhong Weng and Banghua Yan
    !
    !  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
    !  General Public License as published by the Free Software Foundation; either version 2 of the License,
    !  or (at your option) any later version.
    !
    !  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
    !  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
    !  License for more details.
    !
    !  You should have received a copy of the GNU General Public License along with this program; if not, write
    !  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
    !M-
    !--------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER, PARAMETER    :: NOCEAN = 0
    INTEGER               :: LAND_INDEX, SENSOR_ID,NSURFACE_TYPE,NCHAN
    INTEGER               :: Year, Julday
    REAL,    DIMENSION(:) :: TA ,FREQ
    REAL                  :: LATITUDE,LONGITUDE,TskPreclass

    !---INITIALIZATION
    IF (LAND_INDEX == NOCEAN) THEN
       NSURFACE_TYPE = OC_TYP
    ELSE
       NSURFACE_TYPE = LD_TYP
    ENDIF
    !---UPDATE SURFACE TYPE USING SATELLITE-MEASURED ANTENNA OR BRIGHTNESS TEMPERATURES AT WINDOW CHANNELS
    IF (SENSOR_ID == sensor_id_n18)     &
        CALL N18_AMSUA_MHS_ALG(Year,Julday,NCHAN,FREQ,LATITUDE,LONGITUDE,LAND_INDEX,TA,NSURFACE_TYPE,TskPreclass)
    IF (SENSOR_ID == sensor_id_n19)     &
        CALL N18_AMSUA_MHS_ALG(Year,Julday,NCHAN,FREQ,LATITUDE,LONGITUDE,LAND_INDEX,TA,NSURFACE_TYPE,TskPreclass)
    IF (SENSOR_ID == sensor_id_metopA)  &
        CALL N18_AMSUA_MHS_ALG(Year,Julday,NCHAN,FREQ,LATITUDE,LONGITUDE,LAND_INDEX,TA,NSURFACE_TYPE,TskPreclass)
    IF (SENSOR_ID == sensor_id_metopB)  &
        CALL N18_AMSUA_MHS_ALG(Year,Julday,NCHAN,FREQ,LATITUDE,LONGITUDE,LAND_INDEX,TA,NSURFACE_TYPE,TskPreclass)
    IF (SENSOR_ID == sensor_id_f16)     &
        CALL F16_SSMIS_ALG3(Year,Julday,NCHAN,FREQ,LATITUDE,LONGITUDE,LAND_INDEX,TA,NSURFACE_TYPE,TskPreclass)
    IF (SENSOR_ID == sensor_id_f17)     &
        CALL F16_SSMIS_ALG3(Year,Julday,NCHAN,FREQ,LATITUDE,LONGITUDE,LAND_INDEX,TA,NSURFACE_TYPE,TskPreclass)
    IF (SENSOR_ID == sensor_id_f18)     &
        CALL F16_SSMIS_ALG3(Year,Julday,NCHAN,FREQ,LATITUDE,LONGITUDE,LAND_INDEX,TA,NSURFACE_TYPE,TskPreclass)
    IF (SENSOR_ID == sensor_id_amsre)   &
        CALL AQUA_AMSRE_ALG(Year,Julday,NCHAN,FREQ,LATITUDE,LONGITUDE,LAND_INDEX,TA,NSURFACE_TYPE,TskPreclass)
    IF (SENSOR_ID == sensor_id_gcomw1)   &
        CALL GCOMW1_AMSR2_ALG2(Year,Julday,NCHAN,FREQ,LATITUDE,LONGITUDE,LAND_INDEX,TA,NSURFACE_TYPE,TskPreclass)
    IF (SENSOR_ID == sensor_id_fy3ri)   CALL FY3RI_MWRI_ALG(NCHAN,FREQ,LATITUDE,LAND_INDEX,TA,NSURFACE_TYPE,TskPreclass)
    IF (SENSOR_ID == sensor_id_npp)     CALL NPP_ATMS_ALG(Year,Julday,NCHAN,FREQ,LATITUDE,LONGITUDE,LAND_INDEX,TA,&
                                             NSURFACE_TYPE,TskPreclass)
    IF (SENSOR_ID == sensor_id_trmm)     &
        CALL TRMM_TMI_ALG(Year,Julday,NCHAN,FREQ,LATITUDE,LONGITUDE,LAND_INDEX,TA,NSURFACE_TYPE,TskPreclass)
    IF (SENSOR_ID == sensor_id_mtma)     &
        CALL MTMA_MADRAS_ALG(Year,Julday,NCHAN,FREQ,LATITUDE,LONGITUDE,LAND_INDEX,TA,NSURFACE_TYPE,TskPreclass)
    IF (SENSOR_ID == sensor_id_mtsa)     &
        CALL MTSA_SAPHIR_ALG(Year,Julday,NCHAN,FREQ,LATITUDE,LONGITUDE,LAND_INDEX,TA,NSURFACE_TYPE,TskPreclass)
  END SUBROUTINE SURFACE_CLASSIFICATION_ALG


!===============================================================
! Name:         N18_AMSUA_MHS_ALG
!
!
! Type:         Subroutine
!
!
! Description:
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - year
!       - julday
!       - nchan
!       - freq
!       - lat
!       - lon
!       - landindex
!       - ta
!       - surface_type
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!
!===============================================================

  SUBROUTINE N18_AMSUA_MHS_ALG(Year,Julday,nchan,freq,lat,lon,landindex,ta,surface_type,TskPreclass)
    !--------------------------------------------------------------------------------
    !M+
    ! NAME:
    !       N18_AMSUA_MHS_ALG
    !
    ! PURPOSE:
    !       Distinguish four surface types (open ocean, sea ice, snow-free land, snow)
    !
    ! from land index and N18 AMSUA & MHS antenna/brightness temperatures at window channels.
    !
    !
    ! INPUT VARIABLES:
    !
    !   year        : Year
    !
    !   Julday      : Julian day
    !
    !   nchan       : CHANNEL NUMBER FOR A CERTAIN SENSOR
    !
    !   freq(nchan) : CENTRAL FREQUENCY LIST CORRESPONDING TO EACH CHANNEL AT A CERTAIN SENSOR
    !
    !   lat         : LATITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   lon         : LONGITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   landindex   : LAND/OCEAN INDEX (0: ocean, 1: land)
    !
    !   ta          : antenna temperatures at all channels
    !
    ! INTERNAL VARIABLES:
    !
    !   taw          : N18 AMSUA & MHS antenna temperatures at five window channels
    !
    !   taw(1)       : antenna temperature at 23.8 GHz
    !   taw(2)       : antenna temperature at 31.4 GHz
    !   taw(3)       : antenna temperature at 50.3 GHz
    !   taw(4)       : antenna temperature at 89 GHz
    !   taw(5)       : antenna temperature at 157 GHz
    !
    !   freqw        : N18 AMSUA & MHS frequencies at five window channels
    !
    ! OUTPUT VARIABLES:
    !
    !   surface_type    : 0 (OPEN OCEAN)
    !                   : 1 (SEA ICE)
    !                   : 2 (SNOW-FREE LAND)
    !                   : 3 (SNOW)
    !
    !
    !
    ! LANGUAGE:
    !
    !       Fortran-95
    !
    ! INCLUDE FILES:
    !       None.
    !
    ! EXTERNALS:
    !       None.
    !
    ! COMMON BLOCKS:
    !       None.
    !
    ! FILES ACCESSED:
    !       None.
    !
    ! CREATION HISTORY:
    !       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (03-20-2006)
    !
    !
    !       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
    !
    !
    !  Copyright (C) 2006 Fuzhong Weng and Banghua Yan
    !
    !  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
    !  General Public License as published by the Free Software Foundation; either version 2 of the License,
    !  or (at your option) any later version.
    !
    !  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
    !  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
    !  License for more details.
    !
    !  You should have received a copy of the GNU General Public License along with this program; if not, write
    !  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
    !M-
    !--------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER,  PARAMETER      :: NOCEAN = 0,ncoe = 7,nchanw = 5
    REAL,     PARAMETER      :: SILOW = 5.0, SIHIGH = 10.0, LATTH1 = 30.0,LATTH2 = 40.0,LATTH3 = 50.0
    INTEGER                  :: nchan,wchan_index,landindex, surface_type,k,ich,jch,nd
    INTEGER                  :: Year,Julday
    REAL                     :: lat,lon,TA92_SICE,TA157_SICE,TA92_SNOW,TA157_SNOW,TA_ICE(2),TA_SNOW(2),SI,TskPreclass
    REAL,     DIMENSION(:)      :: ta,freq
    REAL,     DIMENSION(nchanw) :: taw,freqw
    REAL,     DIMENSION(2,ncoe) :: coe_oc,coe_land

    !---Fitting coefficients to predict ta92 over open ocean
    data (coe_oc(1,k),k=1,ncoe)/6.76185e+002,  2.55301e+000,  2.44504e-001, -6.88612e+000,   &
         -5.01409e-003, -1.41372e-003,  1.59245e-002/
    !---Fitting coefficients to predict ta157 over open ocean
    data (coe_oc(2,k),k=1,ncoe)/ 5.14546e+002,  6.06543e+000, -6.09327e+000, -2.81614e+000,   &
         -1.35415e-002,  1.29683e-002 , 7.69443e-003/
    !---Fitting coefficients to predict ta92 over snow-free land
    data (coe_land(1,k),k=1,ncoe)/-3.09009e+002,  1.74746e+000, -2.01890e+000,  3.43417e+000, &
         -2.85680e-003,  3.53140e-003, -4.39255e-003/
    !---Fitting coefficients to predict ta157 over snow-free land
    data (coe_land(2,k),k=1,ncoe)/-1.01014e+001,  3.97994e+000, -4.11268e+000,  1.26658e+000, &
         -9.52526e-003,  8.75558e-003,  4.77981e-004/
    !---Central Frequencies ct five window channel
    data freqw/23.8, 31.4, 50.3, 89.0, 157.0/

    ! SELECT TA AT WINDOWN CHANNELS
    DO ich = 1, nchanw
       wchan_index = 1
       DO jch = 1, NCHAN
          IF(abs(freqw(ich)-freq(jch)) <= 0.5) THEN
             wchan_index = jch
             EXIT
          ENDIF
       ENDDO
       taw(ich) = ta(wchan_index)
       IF (ich == 1 .and. wchan_index /= 1)  PRINT *,'CHANNEL ORDER IS INCONSISTENT TO OFFICIAL WAY'
       IF (ich == 2 .and. wchan_index /= 2)  PRINT *,'CHANNEL ORDER IS INCONSISTENT TO OFFICIAL WAY'
       IF (ich == 3 .and. wchan_index /= 3)  PRINT *,'CHANNEL ORDER IS INCONSISTENT TO OFFICIAL WAY'
       IF (ich == 4 .and. wchan_index /= 15) PRINT *,'CHANNEL ORDER IS INCONSISTENT TO OFFICIAL WAY'
       IF (ich == 5 .and. wchan_index /= 17) PRINT *,'CHANNEL ORDER IS INCONSISTENT TO OFFICIAL WAY'
    ENDDO

    ! START TO IDENTIFY SURFACE TYPE
    IF (landindex == NOCEAN) THEN
       surface_type = OC_TYP
    ELSE
       surface_type = LD_TYP
    ENDIF
    !---Predict SEA ICE TA92 and TA157 from TA23 ~ TA50 using open ocean fitting coeffs.
    DO nd =1, 2
       TA_ICE(nd)= coe_oc(nd,1)
       DO ich=1,3
          TA_ICE(nd) = TA_ICE(nd) + coe_oc(nd,ich+1)*taw(ich)
       ENDDO
       DO ich=1,3
          TA_ICE(nd) = TA_ICE(nd) + coe_oc(nd,ich+4)*taw(ich)*taw(ich)
       ENDDO
    ENDDO
    TA92_SICE  = TA_ICE(1)
    TA157_SICE = TA_ICE(2)
    !---Predict SEA ICE TA92 and TA157 from TA23 ~ TA50 using open ocean fitting coeffs.
    DO nd =1, 2
       TA_SNOW(nd)= coe_land(nd,1)
       DO ich=1,3
          TA_SNOW(nd) = TA_SNOW(nd) + coe_land(nd,ich+1)*taw(ich)
       ENDDO
       DO ich=1,3
          TA_SNOW(nd) = TA_SNOW(nd) + coe_land(nd,ich+4)*taw(ich)*taw(ich)
       ENDDO
    ENDDO
    TA92_SNOW  = TA_SNOW(1)
    TA157_SNOW = TA_SNOW(2)
    !---COMPUTE SI = TA23 - TA92
    SI = taw(1)-taw(4)
    !---Predict surface types
    if (landindex == NOCEAN) then    ! over ocean conditions
       surface_type = OC_TYP
       if (TA92_SICE - taw(4) >= 10.0 .and. abs(lat) >= LATTH2)  surface_type = SEAICE_TYP
       if (abs(lat) >= LATTH2 .and. SI >= SIHIGH)                surface_type = SEAICE_TYP
       if (abs(lat) >= LATTH3 .and. SI >= SILOW)                 surface_type = SEAICE_TYP
       if (abs(lat) >= LATTH3 .and. taw(1) >= 235.0)             surface_type = SEAICE_TYP
       !---Get help from the tskin temperature
       if (TskPreclass >= 280.)                                  surface_type = OC_TYP 
       if (TskPreclass <= 265. .and. TskPreclass >=0.)           surface_type = SEAICE_TYP 
       !---Get help from the latitude
!       if (abs(lat) <= 50. )                                     surface_type = OC_TYP
       !---Get help from the latitude and longitude 
       call applySeaIceClimo(Year, Julday, lat, lon, landindex, surface_type)
    else                             ! over land conditions
       surface_type = LD_TYP
       if(TA92_SNOW-taw(4)>=10.0.and.taw(1).le.260.0 .and. abs(lat) >= LATTH1) surface_type = SNOW_TYP
       if(abs(lat) >= LATTH2 .and. SI >= SIHIGH .and. taw(1).le.260.0)         surface_type = SNOW_TYP
       if(abs(lat) >= LATTH3 .and. SI >= SILOW .and. taw(1).le.260.0)          surface_type = SNOW_TYP
       if(taw(1) <= 210.0 .and. taw(3) <= 225.0 .and. abs(lat) >= LATTH3)      surface_type = SNOW_TYP
       !---Get help from the tskin temperature
       if (TskPreclass >= 280.)                                                surface_type = LD_TYP
       if (TskPreclass <= 265. .and. TskPreclass >=0.)                         surface_type = SNOW_TYP 
    endif
  END SUBROUTINE N18_AMSUA_MHS_ALG


 SUBROUTINE F16_SSMIS_ALG(Year,Julday,nchan,freq,lat,lon,landindex,ta,surface_type,TskPreclass)
    !--------------------------------------------------------------------------------
    !M+
    ! NAME:
    !       F16_SSMIS_ALG
    !
    ! PURPOSE:
    !       Distinguish five surface types (open ocean, sea ice, desert, snow-free and non-desert land, snow)
    !
    ! from land index and N18 AMSUA & MHS antenna/brightness temperatures at window channels.
    !
    !
    ! INPUT VARIABLES:
    !
    !   year        : Year
    !
    !   Julday      : Julian day
    !
    !   nchan       : CHANNEL NUMBER FOR A CERTAIN SENSOR
    !
    !   freq(nchan) : CENTRAL FREQUENCY LIST CORRESPONDING TO EACH CHANNEL AT A CERTAIN SENSOR
    !
    !   lat         : LATITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   lon         : LONGITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   landindex   : LAND/OCEAN INDEX (0: ocean, 1: land)
    !
    !   ta          : antenna temperatures at all channels
    !
    ! INTERNAL VARIABLES:
    !
    !   taw          : F16 SSMIS antenna temperatures at seven window channels
    !
    !   taw(1)       : antenna temperature at 19.35 VPOL GHz
    !   taw(2)       : antenna temperature at 19.35 HPOL GHz
    !   taw(3)       : antenna temperature at 22.235 VPOL GHz
    !   taw(4)       : antenna temperature at 37 VPOL GHz
    !   taw(5)       : antenna temperature at 37 HPOL GHz
    !   taw(6)       : antenna temperature at 91.65 VPOL GHz
    !   taw(7)       : antenna temperature at 91.65 HPOL GHz
    !
    !   freqw        : F16 SSMIS frequencies at seven window channels
    !
    ! OUTPUT VARIABLES:
    !
    !   surface_type    : OC_TYP (OPEN OCEAN)
    !                   : SEAICE_TYP (SEA ICE)
    !                   : Desert_TYP (DESERT)
    !                   : LD_TYP (SNOW-FREE AND NON-DESERT LAND)
    !                   : SNOW_TYP (SNOW)
    !
    !
    !
    ! LANGUAGE:
    !
    !       Fortran-95
    !
    ! INCLUDE FILES:
    !       None.
    !
    ! EXTERNALS:
    !       None.
    !
    ! COMMON BLOCKS:
    !       None.
    !
    ! FILES ACCESSED:
    !       None.
    !
    ! CREATION HISTORY:
    !       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (05-29-2007)
    !
    !
    !       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
    !
    !
    !  Copyright (C) 2007 Fuzhong Weng and Banghua Yan
    !
    !  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
    !  General Public License as published by the Free Software Foundation; either version 2 of the License,
    !  or (at your option) any later version.
    !
    !  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
    !  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
    !  License for more details.
    !
    !  You should have received a copy of the GNU General Public License along with this program; if not, write
    !  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
    !M-
    !--------------------------------------------------------------------------------
    INTEGER,  PARAMETER         :: NOCEAN = 0, ncoe = 11,nchanw = 7
    REAL,     PARAMETER         :: SILOW = 5.0, SIHIGH = 10.0, LATTH1 = 30.0,LATTH3 = 50.0
    INTEGER                     :: surface_type,ich
    INTEGER, INTENT(IN)         :: nchan,landindex
    INTEGER, INTENT(IN)         :: Year,Julday
    INTEGER,DIMENSION(nchanw)   :: wchan_index
    REAL                        :: SCH92,SCH37,SCV92,SCV37,lat,lon,TS_ALG,TH92_ALG,TH92_OBS,&
                                   DV19,TskPreclass,PR19,PR37,GRH37,SCVX92
    REAL, INTENT(IN), DIMENSION(nchan)  :: ta,freq
    REAL,     DIMENSION(nchanw) :: taw,freqw
    REAL,     DIMENSION(ncoe)   :: coeh_land,coe_ts
    ! Fitting coefficients to predict ta92 over snow
    data coeh_land/9.60083e+001,-6.05660e-002, 3.51944e-001,-6.52072e-001,-3.79843e-001,   &
                   9.74554e-001,-9.93822e-004, 1.82148e-003,-1.19589e-003,-4.43338e-004,   &
                   2.32471e-003/
    ! Fitting coefficients to predict ts over snow and non-desert land surfaces
    data coe_ts/2.63055e+002,  2.24818e+000, -1.75953e+000, -2.46555e+000, -9.57587e-001,  &
                2.22300e+000, -6.05612e-003,  3.70943e-003,  8.40393e-003,  1.88572e-003,  &
               -5.06110e-003/
    ! Seven window channel indices used in the algorithm
    data wchan_index/13,12,14,16,15,17,18/
    !---Central Frequencies ct five window channel
    data freqw/19.35, 19.35, 22.235, 37.0, 37.0, 91.655, 91.655/

    ! SELECT TA AT WINDOWN CHANNELS
    DO ich = 1, nchanw  !19V,19H,22V,37V,37H,92V,92H
       taw(ich) = ta(wchan_index(ich))
    ENDDO

    ! SELECT TA AT WINDOWN CHANNELS
    DO ich = 1, nchanw
       IF(abs(freqw(ich)-freq(wchan_index(ich))) >= epsilonLoose) THEN
          print *, 'Frequencies do not match in F16 SSMIS surface pre-classifier:',&
               ich,wchan_index(ich),freqw(ich),freq(wchan_index(ich))
       ENDIF
    ENDDO

    ! START TO IDENTIFY SURFACE TYPE
    IF (landindex == NOCEAN) THEN
       surface_type = OC_TYP
    ELSE
       surface_type = LD_TYP
    ENDIF
    ! Calculate polarization difference and indices at 19 and 37 GHz
    DV19 = taw(1)-taw(2)   !TV19-TH19
    
    PR19 = (taw(1)-taw(2)) / (taw(1)+taw(2))
    PR37 = (taw(4)-taw(5)) / (taw(4)+taw(5))

     ! Calculate scattering differences at 37 and 92 GHz with respect to 19 GHz
    SCV37 = taw(1) - taw(4)
    SCH37 = taw(2) - taw(5)
    SCV92 = taw(1) - taw(6)
    SCVX92 = taw(3) - taw(6)
    SCH92 = taw(2) - taw(7)


     ! Calculate scattering index at 37 GHz horizontal polarization
    GRH37 = (taw(2) - taw(5)) / (taw(2) + taw(5))
    !---Predict surface types
    if (landindex == NOCEAN) then    ! over ocean conditions
       surface_type = OC_TYP
       if (PR19 <= 0.13 .and. GRH37 >= -0.07  .and. abs(lat) >= LATTH3) surface_type = SEAICE_TYP
       !---Get help from the latitude and longitude 
       call applySeaIceClimo(Year, Julday, lat, lon, landindex, surface_type)
    else                             ! over land conditions
!       if (DV19 >= 20.0 .and. taw(2) >= 210.0) then
!           surface_type = Desert_TYP
!       endif
       !---Predict SNOW TH92 from TA19 ~ TA37 using snow-free land fitting coeffs.
       TH92_ALG = coeh_land(1)
       DO ich=1,5
          TH92_ALG = TH92_ALG + coeh_land(ich+1)*taw(ich)
       ENDDO
       DO ich=6,10
          TH92_ALG = TH92_ALG + coeh_land(ich+1)*taw(ich-5)*taw(ich-5)
       ENDDO
       TH92_OBS = taw(7)
       ! PREDICT TS
       TS_ALG = coe_ts(1)
       DO ich=1,5
           TS_ALG = TS_ALG + coe_ts(ich+1)*taw(ich)
       ENDDO
       DO ich=6,10
           TS_ALG = TS_ALG + coe_ts(ich+1)*taw(ich-5)*taw(ich-5)
       ENDDO
       ! DEFAULT TYPE
       surface_type = LD_TYP
       !UPDATE LAND TYPE
       if (abs(lat) >= 60.0) then
          if( TH92_OBS - TH92_ALG >= SILOW) surface_type = SNOW_TYP
          if( SCH37 >= SILOW) surface_type = SNOW_TYP
       else
          !--- Additional snow screens
           if ( (abs(lat) >= LATTH1) .and. (SCH37 >= SIHIGH)  .and. TS_ALG < 276.0) &
                surface_type = SNOW_TYP
           !--- Use 2 levels of detection with TS_ALG threshold adjusted depending on TB22v92v gradient
           if ( (abs(lat) >= LATTH1) .and. (SCVX92 >= 20.) .and. TS_ALG < 278.0) &
                surface_type = SNOW_TYP
           if ( (abs(lat) >= LATTH1) .and. (SCVX92 >= 5.) .and. TS_ALG < 276.0) &
                surface_type = SNOW_TYP
       endif

!      Glacial snow
       if(abs(lat) >= 60.0 .and. taw(2) .le. 215.0 .and. DV19 >= 23.0) surface_type = SNOW_TYP
 
!     endif  ! non desert
   endif    ! non ocean
  END SUBROUTINE F16_SSMIS_ALG

 SUBROUTINE F16_SSMIS_ALG2(Year,Julday,nchan,freq,lat,lon,landindex,ta,surface_type,TskPreclass)
    !--------------------------------------------------------------------------------
    !M+
    ! NAME:
    !       F16_SSMIS_ALG
    !
    ! PURPOSE:
    !       Distinguish five surface types (open ocean, sea ice, desert, snow-free and non-desert land, snow)
    !
    ! from land index and N18 AMSUA & MHS antenna/brightness temperatures at window channels.
    !
    !
    ! INPUT VARIABLES:
    !
    !   year        : Year
    !
    !   Julday      : Julian day
    !
    !   nchan       : CHANNEL NUMBER FOR A CERTAIN SENSOR
    !
    !   freq(nchan) : CENTRAL FREQUENCY LIST CORRESPONDING TO EACH CHANNEL AT A CERTAIN SENSOR
    !
    !   lat         : LATITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   lon         : LONGITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   landindex   : LAND/OCEAN INDEX (0: ocean, 1: land)
    !
    !   ta          : antenna temperatures at all channels
    !
    ! INTERNAL VARIABLES:
    !
    !   taw          : F16 SSMIS antenna temperatures at seven window channels
    !
    !   taw(1)       : antenna temperature at 19.35 VPOL GHz
    !   taw(2)       : antenna temperature at 19.35 HPOL GHz
    !   taw(3)       : antenna temperature at 22.235 VPOL GHz
    !   taw(4)       : antenna temperature at 37 VPOL GHz
    !   taw(5)       : antenna temperature at 37 HPOL GHz
    !   taw(6)       : antenna temperature at 91.65 VPOL GHz
    !   taw(7)       : antenna temperature at 91.65 HPOL GHz
    !
    !   freqw        : F16 SSMIS frequencies at seven window channels
    !
    ! OUTPUT VARIABLES:
    !
    !   surface_type    : OC_TYP (OPEN OCEAN)
    !                   : SEAICE_TYP (SEA ICE)
    !                   : Desert_TYP (DESERT)
    !                   : LD_TYP (SNOW-FREE AND NON-DESERT LAND)
    !                   : SNOW_TYP (SNOW)
    !
    !
    !
    ! LANGUAGE:
    !
    !       Fortran-95
    !
    ! INCLUDE FILES:
    !       None.
    !
    ! EXTERNALS:
    !       None.
    !
    ! COMMON BLOCKS:
    !       None.
    !
    ! FILES ACCESSED:
    !       None.
    !
    ! CREATION HISTORY:
    !       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (05-29-2007)
    !
    !
    !       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
    !
    !
    !  Copyright (C) 2007 Fuzhong Weng and Banghua Yan
    !
    !  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
    !  General Public License as published by the Free Software Foundation; either version 2 of the License,
    !  or (at your option) any later version.
    !
    !  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
    !  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
    !  License for more details.
    !
    !  You should have received a copy of the GNU General Public License along with this program; if not, write
    !  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
    !M-
    !--------------------------------------------------------------------------------
    INTEGER,  PARAMETER         :: NOCEAN = 0, ncoe = 11,nchanw = 7
    REAL,     PARAMETER         :: SILOW = 5.0, SIHIGH = 10.0, LATTH1 = 30.0,LATTH3 = 50.0
    INTEGER                     :: surface_type,ich
    INTEGER, INTENT(IN)         :: nchan,landindex
    INTEGER, INTENT(IN)         :: Year,Julday
    INTEGER,DIMENSION(nchanw)   :: wchan_index
    REAL                        :: SCH92,SCH37,SCV92,SCV37,lat,lon,TS_ALG,TH92_ALG,TH92_OBS,&
                                   DV19,TskPreclass,PR19,PR37,GRH37,SCVX92
    REAL, INTENT(IN), DIMENSION(nchan)  :: ta,freq
    REAL,     DIMENSION(nchanw) :: taw,freqw
    REAL,     DIMENSION(ncoe)   :: coeh_land,coe_ts
    ! Fitting coefficients to predict ta92 over snow
    data coeh_land/9.60083e+001,-6.05660e-002, 3.51944e-001,-6.52072e-001,-3.79843e-001,   &
                   9.74554e-001,-9.93822e-004, 1.82148e-003,-1.19589e-003,-4.43338e-004,   &
                   2.32471e-003/
    ! Fitting coefficients to predict ts over snow and non-desert land surfaces
    data coe_ts/2.63055e+002,  2.24818e+000, -1.75953e+000, -2.46555e+000, -9.57587e-001,  &
                2.22300e+000, -6.05612e-003,  3.70943e-003,  8.40393e-003,  1.88572e-003,  &
               -5.06110e-003/
    ! Seven window channel indices used in the algorithm
    data wchan_index/13,12,14,16,15,17,18/
    !---Central Frequencies ct five window channel
    data freqw/19.35, 19.35, 22.235, 37.0, 37.0, 91.655, 91.655/

    ! SELECT TA AT WINDOWN CHANNELS
    DO ich = 1, nchanw  !19V,19H,22V,37V,37H,92V,92H
       taw(ich) = ta(wchan_index(ich))
    ENDDO

    ! SELECT TA AT WINDOWN CHANNELS
    DO ich = 1, nchanw
       IF(abs(freqw(ich)-freq(wchan_index(ich))) >= epsilonLoose) THEN
          print *, 'Frequencies do not match in F16 SSMIS surface pre-classifier:',&
               ich,wchan_index(ich),freqw(ich),freq(wchan_index(ich))
       ENDIF
    ENDDO

    ! START TO IDENTIFY SURFACE TYPE
    IF (landindex == NOCEAN) THEN
       surface_type = OC_TYP
    ELSE
       surface_type = LD_TYP
    ENDIF
    ! Calculate polarization difference and indices at 19 and 37 GHz
    DV19 = taw(1)-taw(2)   !TV19-TH19
    
    PR19 = (taw(1)-taw(2)) / (taw(1)+taw(2))
    PR37 = (taw(4)-taw(5)) / (taw(4)+taw(5))

     ! Calculate scattering differences at 37 and 92 GHz with respect to 19 GHz
    SCV37 = taw(1) - taw(4)
    SCH37 = taw(2) - taw(5)
    SCV92 = taw(1) - taw(6)
    SCVX92 = taw(3) - taw(6)
    SCH92 = taw(2) - taw(7)


     ! Calculate scattering index at 37 GHz horizontal polarization
    GRH37 = (taw(2) - taw(5)) / (taw(2) + taw(5))
    !---Predict surface types
    if (landindex == NOCEAN) then    ! over ocean conditions
       surface_type = OC_TYP
       if (PR19 <= 0.13 .and. GRH37 >= -0.07  .and. abs(lat) >= LATTH3) surface_type = SEAICE_TYP
       !---Get help from the latitude and longitude 
       call applySeaIceClimo(Year, Julday, lat, lon, landindex, surface_type)
    else                             ! over land conditions
!       if (DV19 >= 20.0 .and. taw(2) >= 210.0) then
!           surface_type = Desert_TYP
!       endif
       !---Predict SNOW TH92 from TA19 ~ TA37 using snow-free land fitting coeffs.
       TH92_ALG = coeh_land(1)
       DO ich=1,5
          TH92_ALG = TH92_ALG + coeh_land(ich+1)*taw(ich)
       ENDDO
       DO ich=6,10
          TH92_ALG = TH92_ALG + coeh_land(ich+1)*taw(ich-5)*taw(ich-5)
       ENDDO
       TH92_OBS = taw(7)
       ! PREDICT TS
       TS_ALG = coe_ts(1)
       DO ich=1,5
           TS_ALG = TS_ALG + coe_ts(ich+1)*taw(ich)
       ENDDO
       DO ich=6,10
           TS_ALG = TS_ALG + coe_ts(ich+1)*taw(ich-5)*taw(ich-5)
       ENDDO
       ! DEFAULT TYPE
       surface_type = LD_TYP
       !UPDATE LAND TYPE: test modified version official AMSR-E algorithm

       if(TskPreclass .gt. 0.)then
          if(taw(5) .lt. 245. .and. taw(4) .lt. 255.)then
             if(SCV37 .gt. 0. .or. SCH37 .gt. 0.)then
                surface_type = SNOW_TYP
             else
                if(taw(7) .le. 255. .and. &
                     taw(6) .le. 265. .and. &
                     SCVX92 .gt. 0. .and. &
                     TskPreclass .lt. 267.)then
                   surface_type = SNOW_TYP
                endif
             endif
          endif
       else
          if(taw(5) .lt. 245. .and. taw(4) .lt. 255.)then
             if(SCV37 .gt. 0. .or. SCH37 .gt. 0.)then
                surface_type = SNOW_TYP
             else
                if(taw(7) .le. 255. .and. &
                     taw(6) .le. 265. .and. &
                     SCVX92 .gt. 0.)then
                   surface_type = SNOW_TYP
                endif
             endif
          endif
       endif

!      Glacial snow
       if(abs(lat) >= 60.0 .and. taw(2) .le. 215.0 .and. DV19 >= 23.0) surface_type = SNOW_TYP
 
!     endif  ! non desert
   endif    ! non ocean
 END SUBROUTINE F16_SSMIS_ALG2

 SUBROUTINE F16_SSMIS_ALG3(Year,Julday,nchan,freq,lat,lon,landindex,ta,surface_type,TskPreclass)
    !--------------------------------------------------------------------------------
    !M+
    ! NAME:
    !       F16_SSMIS_ALG
    !
    ! PURPOSE:
    !       Distinguish five surface types (open ocean, sea ice, desert, snow-free and non-desert land, snow)
    !
    ! from land index and N18 AMSUA & MHS antenna/brightness temperatures at window channels.
    !
    !
    ! INPUT VARIABLES:
    !
    !   year        : Year
    !
    !   Julday      : Julian day
    !
    !   nchan       : CHANNEL NUMBER FOR A CERTAIN SENSOR
    !
    !   freq(nchan) : CENTRAL FREQUENCY LIST CORRESPONDING TO EACH CHANNEL AT A CERTAIN SENSOR
    !
    !   lat         : LATITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   lon         : LONGITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   landindex   : LAND/OCEAN INDEX (0: ocean, 1: land)
    !
    !   ta          : antenna temperatures at all channels
    !
    ! INTERNAL VARIABLES:
    !
    !   taw          : F16 SSMIS antenna temperatures at seven window channels
    !
    !   taw(1)       : antenna temperature at 19.35 VPOL GHz
    !   taw(2)       : antenna temperature at 19.35 HPOL GHz
    !   taw(3)       : antenna temperature at 22.235 VPOL GHz
    !   taw(4)       : antenna temperature at 37 VPOL GHz
    !   taw(5)       : antenna temperature at 37 HPOL GHz
    !   taw(6)       : antenna temperature at 91.65 VPOL GHz
    !   taw(7)       : antenna temperature at 91.65 HPOL GHz
    !
    !   freqw        : F16 SSMIS frequencies at seven window channels
    !
    ! OUTPUT VARIABLES:
    !
    !   surface_type    : OC_TYP (OPEN OCEAN)
    !                   : SEAICE_TYP (SEA ICE)
    !                   : Desert_TYP (DESERT)
    !                   : LD_TYP (SNOW-FREE AND NON-DESERT LAND)
    !                   : SNOW_TYP (SNOW)
    !
    !
    !
    ! LANGUAGE:
    !
    !       Fortran-95
    !
    ! INCLUDE FILES:
    !       None.
    !
    ! EXTERNALS:
    !       None.
    !
    ! COMMON BLOCKS:
    !       None.
    !
    ! FILES ACCESSED:
    !       None.
    !
    ! CREATION HISTORY:
    !       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (05-29-2007)
    !
    !
    !       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
    !
    !
    !  Copyright (C) 2007 Fuzhong Weng and Banghua Yan
    !
    !  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
    !  General Public License as published by the Free Software Foundation; either version 2 of the License,
    !  or (at your option) any later version.
    !
    !  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
    !  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
    !  License for more details.
    !
    !  You should have received a copy of the GNU General Public License along with this program; if not, write
    !  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
    !M-
    !--------------------------------------------------------------------------------
    INTEGER,  PARAMETER         :: NOCEAN = 0, ncoe = 11,nchanw = 7
    REAL,     PARAMETER         :: SILOW = 5.0, SIHIGH = 10.0, LATTH1 = 30.0,LATTH3 = 50.0
    INTEGER                     :: surface_type,ich
    INTEGER, INTENT(IN)         :: nchan,landindex
    INTEGER, INTENT(IN)         :: Year,Julday
    INTEGER,DIMENSION(nchanw)   :: wchan_index
    REAL                        :: SCH92,SCH37,SCV92,SCV37,lat,lon,TS_ALG,TH92_ALG,TH92_OBS,&
                                   DV19,TskPreclass,PR19,PR37,GRH37,SCVX92
    REAL, INTENT(IN), DIMENSION(nchan)  :: ta,freq
    REAL,     DIMENSION(nchanw) :: taw,freqw
    REAL,     DIMENSION(ncoe)   :: coeh_land,coe_ts
    ! Fitting coefficients to predict ta92 over snow
    data coeh_land/9.60083e+001,-6.05660e-002, 3.51944e-001,-6.52072e-001,-3.79843e-001,   &
                   9.74554e-001,-9.93822e-004, 1.82148e-003,-1.19589e-003,-4.43338e-004,   &
                   2.32471e-003/
    ! Fitting coefficients to predict ts over snow and non-desert land surfaces
    data coe_ts/2.63055e+002,  2.24818e+000, -1.75953e+000, -2.46555e+000, -9.57587e-001,  &
                2.22300e+000, -6.05612e-003,  3.70943e-003,  8.40393e-003,  1.88572e-003,  &
               -5.06110e-003/
    ! Seven window channel indices used in the algorithm
    data wchan_index/13,12,14,16,15,17,18/
    !---Central Frequencies ct five window channel
    data freqw/19.35, 19.35, 22.235, 37.0, 37.0, 91.655, 91.655/

    ! SELECT TA AT WINDOWN CHANNELS
    DO ich = 1, nchanw  !19V,19H,22V,37V,37H,92V,92H
       taw(ich) = ta(wchan_index(ich))
    ENDDO

    ! SELECT TA AT WINDOWN CHANNELS
    DO ich = 1, nchanw
       IF(abs(freqw(ich)-freq(wchan_index(ich))) >= epsilonLoose) THEN
          print *, 'Frequencies do not match in F16 SSMIS surface pre-classifier:',&
               ich,wchan_index(ich),freqw(ich),freq(wchan_index(ich))
       ENDIF
    ENDDO

    ! START TO IDENTIFY SURFACE TYPE
    IF (landindex == NOCEAN) THEN
       surface_type = OC_TYP
    ELSE
       surface_type = LD_TYP
    ENDIF
    ! Calculate polarization difference and indices at 19 and 37 GHz
    DV19 = taw(1)-taw(2)   !TV19-TH19
    
    PR19 = (taw(1)-taw(2)) / (taw(1)+taw(2))
    PR37 = (taw(4)-taw(5)) / (taw(4)+taw(5))

     ! Calculate scattering differences at 37 and 92 GHz with respect to 19 GHz
    SCV37 = taw(1) - taw(4)
    SCH37 = taw(2) - taw(5)
    SCV92 = taw(1) - taw(6)
    SCVX92 = taw(3) - taw(6)
    SCH92 = taw(2) - taw(7)


     ! Calculate scattering index at 37 GHz horizontal polarization
    GRH37 = (taw(2) - taw(5)) / (taw(2) + taw(5))
    !---Predict surface types
    if (landindex == NOCEAN) then    ! over ocean conditions
       surface_type = OC_TYP
       if (PR19 <= 0.13 .and. GRH37 >= -0.07  .and. abs(lat) >= LATTH3) surface_type = SEAICE_TYP
       !---Get help from the latitude and longitude 
       call applySeaIceClimo(Year, Julday, lat, lon, landindex, surface_type)
    else                             ! over land conditions
!       if (DV19 >= 20.0 .and. taw(2) >= 210.0) then
!           surface_type = Desert_TYP
!       endif
       !---Predict SNOW TH92 from TA19 ~ TA37 using snow-free land fitting coeffs.
       TH92_ALG = coeh_land(1)
       DO ich=1,5
          TH92_ALG = TH92_ALG + coeh_land(ich+1)*taw(ich)
       ENDDO
       DO ich=6,10
          TH92_ALG = TH92_ALG + coeh_land(ich+1)*taw(ich-5)*taw(ich-5)
       ENDDO
       TH92_OBS = taw(7)
       ! PREDICT TS
       TS_ALG = coe_ts(1)
       DO ich=1,5
           TS_ALG = TS_ALG + coe_ts(ich+1)*taw(ich)
       ENDDO
       DO ich=6,10
           TS_ALG = TS_ALG + coe_ts(ich+1)*taw(ich-5)*taw(ich-5)
       ENDDO
       ! DEFAULT TYPE
       surface_type = LD_TYP
       !UPDATE LAND TYPE: test modified version official AMSR-E algorithm

       if(TskPreclass .gt. 0.)then
          if(taw(5) .lt. 245. .and. taw(4) .lt. 255.)then
             if(SCV37 .gt. 0. .or. SCH37 .gt. 0.)then
                surface_type = SNOW_TYP
             else
                if(taw(7) .le. 255. .and. &
                     taw(6) .le. 265. .and. &
                     SCVX92 .gt. 0. .and. &
                     TskPreclass .lt. 267.)then
                   surface_type = SNOW_TYP
                endif
             endif
          endif
       else
          if(taw(5) .lt. 245. .and. taw(4) .lt. 255.)then
             if(SCV37 .gt. 0. .or. SCH37 .gt. 0.)then
                surface_type = SNOW_TYP
             else
                if(taw(7) .le. 255. .and. &
                     taw(6) .le. 265. .and. &
                     SCVX92 .gt. 0.)then
                   surface_type = SNOW_TYP
                endif
             endif
          endif
       endif

!      Add original algorithm to detect snow
       if (abs(lat) >= 60.0) then
          if( TH92_OBS - TH92_ALG >= SILOW) surface_type = SNOW_TYP
          if( SCH37 >= SILOW) surface_type = SNOW_TYP
       else
          !--- Additional snow screens
           if ( (abs(lat) >= LATTH1) .and. (SCH37 >= SIHIGH)  .and. TS_ALG < 276.0) &
                surface_type = SNOW_TYP
           !--- Use 2 levels of detection with TS_ALG threshold adjusted depending on TB22v92v gradient
           if ( (abs(lat) >= LATTH1) .and. (SCVX92 >= 20.) .and. TS_ALG < 278.0) &
                surface_type = SNOW_TYP
           if ( (abs(lat) >= LATTH1) .and. (SCVX92 >= 5.) .and. TS_ALG < 276.0) &
                surface_type = SNOW_TYP
       endif

!      Glacial snow
       if(abs(lat) >= 60.0 .and. taw(2) .le. 215.0 .and. DV19 >= 23.0) surface_type = SNOW_TYP
 
!     endif  ! non desert
   endif    ! non ocean
 END SUBROUTINE F16_SSMIS_ALG3

SUBROUTINE AQUA_AMSRE_ALG(Year,Julday,nchan,freq,lat,lon,landindex,ta,surface_type,TskPreclass)
    !--------------------------------------------------------------------------------
    !M+
    ! NAME:
    !       AQUA_AMSRE_ALG
    !
    ! PURPOSE:
    !       Distinguish five surface types (open ocean, sea ice, desert, snow-free and non-desert land, snow)
    !       (Adapted from F16_SSMIS_ALG)    !
    ! 
    !
    !
    ! INPUT VARIABLES:
    !
    !   year        : Year
    !
    !   Julday      : Julian day
    !
    !   nchan       : CHANNEL NUMBER FOR A CERTAIN SENSOR
    !
    !   freq(nchan) : CENTRAL FREQUENCY LIST CORRESPONDING TO EACH CHANNEL AT A CERTAIN SENSOR
    !
    !   lat         : LATITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   lon         : LONGITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   landindex   : LAND/OCEAN INDEX (0: ocean, 1: land)
    !
    !   ta          : antenna temperatures at all channels
    !
    ! INTERNAL VARIABLES:
    !
    !   taw          : AQUA AMSRE antenna temperatures at 12 window channels
    !
    !   taw(1)       : antenna temperature at 6.925 VPOL GHz
    !   taw(2)       : antenna temperature at 6.925 HPOL GHz
    !   taw(3)       : antenna temperature at 10.65 VPOL GHz
    !   taw(4)       : antenna temperature at 10.65 HPOL GHz
    !   taw(5)       : antenna temperature at 18.70 VPOL GHz
    !   taw(6)       : antenna temperature at 18.70 HPOL GHz
    !   taw(7)       : antenna temperature at 23.80 VPOL GHz
    !   taw(8)       : antenna temperature at 23.80 HPOL GHz
    !   taw(9)       : antenna temperature at 36.50 VPOL GHz
    !   taw(10)      : antenna temperature at 36.50 VPOL GHz
    !   taw(11)      : antenna temperature at 89.00 VPOL GHz
    !   taw(12)      : antenna temperature at 89.00 HPOL GHz
    !
    !   freqw        : AQUA AMSRE frequencies at 12 window channels
    !
    ! OUTPUT VARIABLES:
    !
    !   surface_type    : OC_TYP (OPEN OCEAN)
    !                   : SEAICE_TYP (SEA ICE)
    !                   : Desert_TYP (DESERT)
    !                   : LD_TYP (SNOW-FREE AND NON-DESERT LAND)
    !                   : SNOW_TYP (SNOW)
    !
    !
    !
    ! LANGUAGE:
    !
    !       Fortran-95
    !
    ! INCLUDE FILES:
    !       None.
    !
    ! EXTERNALS:
    !       None.
    !
    ! COMMON BLOCKS:
    !       None.
    !
    ! FILES ACCESSED:
    !       None.
    !
    ! CREATION HISTORY:
    !       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (05-29-2007)
    !
    !
    !       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
    !
    !
    !  Copyright (C) 2007 Fuzhong Weng and Banghua Yan
    !
    !  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
    !  General Public License as published by the Free Software Foundation; either version 2 of the License,
    !  or (at your option) any later version.
    !
    !  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
    !  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
    !  License for more details.
    !
    !  You should have received a copy of the GNU General Public License along with this program; if not, write
    !  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
    !M-
    !--------------------------------------------------------------------------------
    INTEGER,  PARAMETER         :: NOCEAN = 0, ncoe = 11,nchanw = 7
    REAL,     PARAMETER         :: SILOW = 5.0, SIHIGH = 10.0, LATTH1 = 30.0,LATTH3 = 50.0
    INTEGER                     :: surface_type,ich
    INTEGER, INTENT(IN)         :: nchan,landindex
    INTEGER, INTENT(IN)         :: Year,Julday
    INTEGER,DIMENSION(nchanw)   :: wchan_index
    REAL                        :: SCH92,SCH37,SCV92,SCV37,lat,lon,TS_ALG,TH92_ALG,TH92_OBS,&
                                   DV19,TskPreclass,PR19,PR37,GRH37,SCVX92
    REAL, INTENT(IN), DIMENSION(nchan)  :: ta,freq
    REAL,     DIMENSION(nchanw) :: taw,freqw
    REAL,     DIMENSION(ncoe)   :: coeh_land,coe_ts
    ! Fitting coefficients to predict ta92 over snow
    data coeh_land/9.60083e+001,-6.05660e-002, 3.51944e-001,-6.52072e-001,-3.79843e-001,   &
                   9.74554e-001,-9.93822e-004, 1.82148e-003,-1.19589e-003,-4.43338e-004,   &
                   2.32471e-003/
    ! Fitting coefficients to predict ts over snow and non-desert land surfaces
    data coe_ts/2.63055e+002,  2.24818e+000, -1.75953e+000, -2.46555e+000, -9.57587e-001,  &
                2.22300e+000, -6.05612e-003,  3.70943e-003,  8.40393e-003,  1.88572e-003,  &
               -5.06110e-003/
    ! Seven window channel indices used in the algorithm
    data wchan_index/5,6,7,9,10,11,12/
    !---Central Frequencies ct five window channel
    data freqw/18.7, 18.7, 23.8, 36.5, 36.5, 89.0, 89.0/

    ! SELECT TA AT WINDOWN CHANNELS
    DO ich = 1, nchanw  !19V,19H,22V,37V,37H,92V,92H
       taw(ich) = ta(wchan_index(ich))
    ENDDO

    ! SELECT TA AT WINDOWN CHANNELS
    DO ich = 1, nchanw
       IF(abs(freqw(ich)-freq(wchan_index(ich))) >= epsilonLoose) THEN
          print *, 'Frequencies do not match in AQUA AMSRE surface pre-classifier:',&
               ich,wchan_index(ich),freqw(ich),freq(wchan_index(ich))
       ENDIF
    ENDDO

    ! START TO IDENTIFY SURFACE TYPE
    IF (landindex == NOCEAN) THEN
       surface_type = OC_TYP
    ELSE
       surface_type = LD_TYP
    ENDIF

    ! Calculate polarization difference and indices at 19 and 37 GHz
    DV19 = taw(1)-taw(2)   !TV19-TH19
    
    PR19 = (taw(1)-taw(2)) / (taw(1)+taw(2))
    PR37 = (taw(4)-taw(5)) / (taw(4)+taw(5))

     ! Calculate scattering differences at 37 and 92 GHz with respect to 19 GHz
    SCV37 = taw(1) - taw(4)
    SCH37 = taw(2) - taw(5)
    SCV92 = taw(1) - taw(6)
    SCVX92 = taw(3) - taw(6)
    SCH92 = taw(2) - taw(7)


     ! Calculate scattering index at 37 GHz horizontal polarization
    GRH37 = (taw(2) - taw(5)) / (taw(2) + taw(5))
    !---Predict surface types
    if (landindex == NOCEAN) then    ! over ocean conditions
       surface_type = OC_TYP
       if (PR19 <= 0.13 .and. GRH37 >= -0.07  .and. abs(lat) >= LATTH3) surface_type = SEAICE_TYP

       ! From legacy placeholder code
       IF (ta(1) > 190.) surface_type = SEAICE_TYP

       !---Get help from the latitude and longitude 
       call applySeaIceClimo(Year, Julday, lat, lon, landindex, surface_type)
    else                             ! over land conditions
!       if (DV19 >= 20.0 .and. taw(2) >= 210.0) then
!           surface_type = Desert_TYP
!       endif
       !---Predict SNOW TH92 from TA19 ~ TA37 using snow-free land fitting coeffs.
       TH92_ALG = coeh_land(1)
       DO ich=1,5
          TH92_ALG = TH92_ALG + coeh_land(ich+1)*taw(ich)
       ENDDO
       DO ich=6,10
          TH92_ALG = TH92_ALG + coeh_land(ich+1)*taw(ich-5)*taw(ich-5)
       ENDDO
       TH92_OBS = taw(7)
       ! PREDICT TS
       TS_ALG = coe_ts(1)
       DO ich=1,5
           TS_ALG = TS_ALG + coe_ts(ich+1)*taw(ich)
       ENDDO
       DO ich=6,10
           TS_ALG = TS_ALG + coe_ts(ich+1)*taw(ich-5)*taw(ich-5)
       ENDDO
       ! DEFAULT TYPE
       surface_type = LD_TYP
       !UPDATE LAND TYPE
       if (abs(lat) >= 60.0) then
          if( TH92_OBS - TH92_ALG >= SILOW) surface_type = SNOW_TYP
          if( SCH37 >= SILOW) surface_type = SNOW_TYP
       else
          !--- Additional snow screens
           if ( (abs(lat) >= LATTH1) .and. (SCH37 >= SIHIGH)  .and. TS_ALG < 276.0) &
                surface_type = SNOW_TYP
           !--- Use 2 levels of detection with TS_ALG threshold adjusted depending on TB22v92v gradient
           if ( (abs(lat) >= LATTH1) .and. (SCVX92 >= 20.) .and. TS_ALG < 278.0) &
                surface_type = SNOW_TYP
           if ( (abs(lat) >= LATTH1) .and. (SCVX92 >= 5.) .and. TS_ALG < 276.0) &
                surface_type = SNOW_TYP
       endif

!      Glacial snow
       if(abs(lat) >= 60.0 .and. taw(2) .le. 215.0 .and. DV19 >= 23.0) surface_type = SNOW_TYP
 
!     endif  ! non desert
   endif    ! non ocean
 END SUBROUTINE AQUA_AMSRE_ALG

SUBROUTINE GCOMW1_AMSR2_ALG2(Year,Julday,nchan,freq,lat,lon,landindex,ta,surface_type,TskPreclass)
    !--------------------------------------------------------------------------------
    !M+
    ! NAME:
    !       GCOMW1_AMSR2_ALG
    !
    ! PURPOSE:
    !       Distinguish five surface types (open ocean, sea ice, desert, snow-free and non-desert land, snow)
    !       (Adapted from F16_SSMIS_ALG)    !
    ! 
    !
    !
    ! INPUT VARIABLES:
    !
    !   year        : Year
    !
    !   Julday      : Julian day
    !
    !   nchan       : CHANNEL NUMBER FOR A CERTAIN SENSOR
    !
    !   freq(nchan) : CENTRAL FREQUENCY LIST CORRESPONDING TO EACH CHANNEL AT A CERTAIN SENSOR
    !
    !   lat         : LATITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   lon         : LONGITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   landindex   : LAND/OCEAN INDEX (0: ocean, 1: land)
    !
    !   ta          : antenna temperatures at all channels
    !
    ! INTERNAL VARIABLES:
    !
    !   taw          : AQUA AMSRE antenna temperatures at 9 window channels
    !
    !   taw(1)       : antenna temperature at 10.65 VPOL GHz
    !   taw(2)       : antenna temperature at 10.65 HPOL GHz
    !   taw(3)       : antenna temperature at 18.70 VPOL GHz
    !   taw(4)       : antenna temperature at 18.70 HPOL GHz
    !   taw(5)       : antenna temperature at 23.80 VPOL GHz
    !   taw(6)       : antenna temperature at 36.50 VPOL GHz
    !   taw(7)       : antenna temperature at 36.50 VPOL GHz
    !   taw(8)       : antenna temperature at 89.00 VPOL GHz
    !   taw(9)       : antenna temperature at 89.00 HPOL GHz
    !
    !   freqw        : GCOMW1 AMSR2 frequencies at 14 window channels
    !
    ! OUTPUT VARIABLES:
    !
    !   surface_type    : OC_TYP (OPEN OCEAN)
    !                   : SEAICE_TYP (SEA ICE)
    !                   : Desert_TYP (DESERT)
    !                   : LD_TYP (SNOW-FREE AND NON-DESERT LAND)
    !                   : SNOW_TYP (SNOW)
    !
    !
    !
    ! LANGUAGE:
    !
    !       Fortran-95
    !
    ! INCLUDE FILES:
    !       None.
    !
    ! EXTERNALS:
    !       None.
    !
    ! COMMON BLOCKS:
    !       None.
    !
    ! FILES ACCESSED:
    !       None.
    !
    ! CREATION HISTORY:
    !       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (05-29-2007)
    !
    !
    !       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
    !
    !
    !  Copyright (C) 2007 Fuzhong Weng and Banghua Yan
    !
    !  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
    !  General Public License as published by the Free Software Foundation; either version 2 of the License,
    !  or (at your option) any later version.
    !
    !  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
    !  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
    !  License for more details.
    !
    !  You should have received a copy of the GNU General Public License along with this program; if not, write
    !  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
    !M-
    !--------------------------------------------------------------------------------
    INTEGER,  PARAMETER         :: NOCEAN = 0, ncoe = 11,nchanw = 9
    REAL,     PARAMETER         :: SILOW = 5.0, SIHIGH = 10.0, LATTH1 = 30.0,LATTH3 = 50.0
    INTEGER                     :: surface_type,ich
    INTEGER, INTENT(IN)         :: nchan,landindex
    INTEGER, INTENT(IN)         :: Year,Julday
    INTEGER,DIMENSION(nchanw)   :: wchan_index
    REAL                        :: SCH92,SCH37,SCV92,SCV37,lat,lon,TS_ALG,TH92_ALG,TH92_OBS,&
                                   DV19,TskPreclass,PR19,PR37,GRH37,SCVX92,SCVY92,&
                                   SCVX37,SCHX37
    REAL, INTENT(IN), DIMENSION(nchan)  :: ta,freq
    REAL,     DIMENSION(nchanw) :: taw,freqw
    REAL,     DIMENSION(ncoe)   :: coeh_land,coe_ts
    ! Fitting coefficients to predict ta92 over snow
    data coeh_land/9.60083e+001,-6.05660e-002, 3.51944e-001,-6.52072e-001,-3.79843e-001,   &
                   9.74554e-001,-9.93822e-004, 1.82148e-003,-1.19589e-003,-4.43338e-004,   &
                   2.32471e-003/
    ! Fitting coefficients to predict ts over snow and non-desert land surfaces
    data coe_ts/2.63055e+002,  2.24818e+000, -1.75953e+000, -2.46555e+000, -9.57587e-001,  &
                2.22300e+000, -6.05612e-003,  3.70943e-003,  8.40393e-003,  1.88572e-003,  &
               -5.06110e-003/
    ! Seven window channel indices used in the algorithm
    data wchan_index/5,6,7,8,9,11,12,13,14/
    !---Central Frequencies ct five window channel
    data freqw/10.65,10.65,18.7, 18.7, 23.8, 36.5, 36.5, 89.0, 89.0/

    ! SELECT TA AT WINDOWN CHANNELS
    DO ich = 1, nchanw  !19V,19H,22V,37V,37H,92V,92H
       taw(ich) = ta(wchan_index(ich))
    ENDDO

    ! SELECT TA AT WINDOWN CHANNELS
    DO ich = 1, nchanw
       IF(abs(freqw(ich)-freq(wchan_index(ich))) >= epsilonLoose) THEN
          print *, 'Frequencies do not match in GCOMW1 AMSR2 surface pre-classifier:',&
               ich,wchan_index(ich),freqw(ich),freq(wchan_index(ich))
       ENDIF
    ENDDO

    ! START TO IDENTIFY SURFACE TYPE
    IF (landindex == NOCEAN) THEN
       surface_type = OC_TYP
    ELSE
       surface_type = LD_TYP
    ENDIF

    ! Calculate polarization difference and indices at 19 and 37 GHz
    DV19 = taw(3)-taw(4)   !TV19-TH19
    
    PR19 = (taw(3)-taw(4)) / (taw(3)+taw(4))
    PR37 = (taw(6)-taw(7)) / (taw(6)+taw(7))

     ! Calculate scattering differences at 37 and 92 GHz with respect to 19 GHz
    SCV37 = taw(3) - taw(6)
    SCH37 = taw(4) - taw(7)
    SCV92 = taw(3) - taw(8)
    SCVX92 = taw(5) - taw(8)
    SCH92 = taw(4) - taw(9)
    SCVX37 = taw(1) - taw(6)
    SCHX37 = taw(2) - taw(7)


     ! Calculate scattering index at 37 GHz horizontal polarization
    GRH37 = (taw(4) - taw(7)) / (taw(4) + taw(7))
    !---Predict surface types
    if (landindex == NOCEAN) then    ! over ocean conditions
       surface_type = OC_TYP
       if (PR19 <= 0.13 .and. GRH37 >= -0.07  .and. abs(lat) >= LATTH3) surface_type = SEAICE_TYP

       ! From legacy placeholder code
       IF (ta(1+2) > 190.) surface_type = SEAICE_TYP

       !---Get help from the latitude and longitude 
       call applySeaIceClimo(Year, Julday, lat, lon, landindex, surface_type)
    else                             ! over land conditions
!       if (DV19 >= 20.0 .and. taw(2) >= 210.0) then
!           surface_type = Desert_TYP
!       endif
       !---Predict SNOW TH92 from TA19 ~ TA37 using snow-free land fitting coeffs.
       TH92_ALG = coeh_land(1)
       DO ich=1,5
          TH92_ALG = TH92_ALG + coeh_land(ich+1)*taw(ich+2)
       ENDDO
       DO ich=6,10
          TH92_ALG = TH92_ALG + coeh_land(ich+1)*taw(ich-5+2)*taw(ich-5+2)
       ENDDO
       TH92_OBS = taw(7)
       ! PREDICT TS
       TS_ALG = coe_ts(1)
       DO ich=1,5
           TS_ALG = TS_ALG + coe_ts(ich+1)*taw(ich+2)
       ENDDO
       DO ich=6,10
           TS_ALG = TS_ALG + coe_ts(ich+1)*taw(ich-5+2)*taw(ich-5+2)
       ENDDO
       ! DEFAULT TYPE
       surface_type = LD_TYP
       !UPDATE LAND TYPE: Modified version of AMSR-E official algorithm
       if(TskPreclass .gt. 0.)then
          if(taw(7) .lt. 245. .and. taw(6) .lt. 255.)then
             if(SCVX37 .gt. 0. .or. SCHX37 .gt. 0.)then
                surface_type = SNOW_TYP
             else
                if(taw(9) .le. 255. .and. &
                     taw(8) .le. 265. .and. &
                     SCVX92 .gt. 0. .and. &
                     TskPreclass .lt. 267.)then
                   surface_type = SNOW_TYP
                endif
             endif
          endif
       else
          if(taw(7) .lt. 245. .and. taw(6) .lt. 255.)then
             if(SCVX37 .gt. 0. .or. SCHX37 .gt. 0.)then
                surface_type = SNOW_TYP
             else
                if(taw(8) .le. 255. .and. &
                     taw(9) .le. 265. .and. &
                     SCVX92 .gt. 0.)then
                   surface_type = SNOW_TYP
                endif
             endif
          endif
       endif
!      Glacial snow
     if(abs(lat) >= 60.0 .and. taw(4) .le. 215.0 .and. DV19 >= 23.0) surface_type = SNOW_TYP
 
!     endif  ! non desert
  endif    ! non ocean
END SUBROUTINE GCOMW1_AMSR2_ALG2


  SUBROUTINE AQUA_AMSRE_ALG_OLD(Year,Julday,nchan,freq,lat,lon,landindex,ta,surface_type,TskPreclass)
    !--------------------------------------------------------------------------------
    !M+
    ! NAME:
    !       AQUA_AMSRE_ALG
    !
    ! PURPOSE:
    !       placeholder for AMSRE preclassifier. For now simply returns the topography.
    !
    !
    ! INPUT VARIABLES:
    !
    !   year        : Year
    !
    !   Julday      : Julian day
    !
    !   nchan       : CHANNEL NUMBER FOR A CERTAIN SENSOR
    !
    !   freq(nchan) : CENTRAL FREQUENCY LIST CORRESPONDING TO EACH CHANNEL AT A CERTAIN SENSOR
    !
    !   lat         : LATITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   lon         : LONGITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   landindex   : LAND/OCEAN INDEX (0: ocean, 1: land)
    !
    !   ta          : antenna temperatures at all channels
    !
    !
    ! OUTPUT VARIABLES:
    !
    !   surface_type    : OC_TYP (OPEN OCEAN)
    !                   : SEAICE_TYP (SEA ICE)
    !                   : Desert_TYP (DESERT)
    !                   : LD_TYP (SNOW-FREE AND NON-DESERT LAND)
    !                   : SNOW_TYP (SNOW)
    !
    !
    !
    ! Sid Boukabara, NOAA/NESDIS
    !
    !
    !  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
    !  General Public License as published by the Free Software Foundation; either version 2 of the License,
    !  or (at your option) any later version.
    !
    !  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
    !  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
    !  License for more details.
    !
    !  You should have received a copy of the GNU General Public License along with this program; if not, write
    !  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
    !--------------------------------------------------------------------------------
    INTEGER,  PARAMETER         :: NOCEAN = 0
    INTEGER                     :: surface_type
    INTEGER, INTENT(IN)         :: nchan,landindex
    REAL                        :: lat,lon,TskPreclass
    REAL, INTENT(IN), DIMENSION(nchan)  :: ta,freq
    INTEGER, INTENT(IN)         :: Year,Julday
    IF (landindex == NOCEAN) THEN
       surface_type = OC_TYP
    ELSE
       surface_type = LD_TYP
    ENDIF

    IF (surface_type == OC_TYP) THEN
       IF (ta(1) > 190.) surface_type = SEAICE_TYP
       !---Get help from the latitude and longitude 
       call applySeaIceClimo(Year, Julday, lat, lon, landindex, surface_type)
       
    ENDIF
    
  END SUBROUTINE AQUA_AMSRE_ALG_OLD

  SUBROUTINE TRMM_TMI_ALG(Year,Julday,nchan,freq,lat,lon,landindex,ta,surface_type,TskPreclass)
    !--------------------------------------------------------------------------------
    !M+
    ! NAME:
    !       TRMM_TMI_ALG (Adapted from F16_SSMIS_ALG)
    !
    ! PURPOSE:
    !       Distinguish five surface types (open ocean, sea ice, desert, snow-free and non-desert land, snow)
    !
    ! 
    !
    !
    ! INPUT VARIABLES:
    !
    !   year        : Year
    !
    !   Julday      : Julian day
    !
    !   nchan       : CHANNEL NUMBER FOR A CERTAIN SENSOR
    !
    !   freq(nchan) : CENTRAL FREQUENCY LIST CORRESPONDING TO EACH CHANNEL AT A CERTAIN SENSOR
    !
    !   lat         : LATITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   lon         : LONGITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   landindex   : LAND/OCEAN INDEX (0: ocean, 1: land)
    !
    !   ta          : antenna temperatures at all channels
    !
    ! INTERNAL VARIABLES:
    !
    !   taw          : TRMM TMI antenna temperatures at seven window channels
    !
    !   taw(1)       : antenna temperature at 11 VPOL GHz
    !   taw(2)       : antenna temperature at 11 HPOL GHz
    !   taw(3)       : antenna temperature at 19.35 VPOL GHz
    !   taw(4)       : antenna temperature at 19.35 HPOL GHz
    !   taw(5)       : antenna temperature at 22.235 VPOL GHz
    !   taw(6)       : antenna temperature at 37 VPOL GHz
    !   taw(7)       : antenna temperature at 37 HPOL GHz
    !   taw(8)       : antenna temperature at 91.65 VPOL GHz
    !   taw(9)       : antenna temperature at 91.65 HPOL GHz
    !
    !   freqw        : TRMM TMI frequencies at nine window channels
    !
    ! OUTPUT VARIABLES:
    !
    !   surface_type    : OC_TYP (OPEN OCEAN)
    !                   : SEAICE_TYP (SEA ICE)
    !                   : Desert_TYP (DESERT)
    !                   : LD_TYP (SNOW-FREE AND NON-DESERT LAND)
    !                   : SNOW_TYP (SNOW)
    !
    !
    !
    ! LANGUAGE:
    !
    !       Fortran-95
    !
    ! INCLUDE FILES:
    !       None.
    !
    ! EXTERNALS:
    !       None.
    !
    ! COMMON BLOCKS:
    !       None.
    !
    ! FILES ACCESSED:
    !       None.
    !
    ! CREATION HISTORY:
    !       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (05-29-2007)
    !
    !
    !       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
    !
    !
    !  Copyright (C) 2007 Fuzhong Weng and Banghua Yan
    !
    !--------------------------------------------------------------------------------
    INTEGER,  PARAMETER         :: NOCEAN = 0, ncoe = 11,nchanw = 7
    REAL,     PARAMETER         :: SILOW = 5.0, SIHIGH = 10.0, LATTH1 = 30.0,LATTH3 = 50.0
    INTEGER                     :: surface_type,ich
    INTEGER, INTENT(IN)         :: nchan,landindex
    INTEGER, INTENT(IN)         :: Year,Julday
    INTEGER,DIMENSION(nchanw)   :: wchan_index
    REAL                        :: SCH92,SCH37,SCV92,SCV37,lat,lon,TS_ALG,TH92_ALG,TH92_OBS,&
                                   DV19,TskPreclass,PR19,PR37,GRH37,SCVX92
    REAL, INTENT(IN), DIMENSION(nchan)  :: ta,freq
    REAL,     DIMENSION(nchanw) :: taw,freqw
    REAL,     DIMENSION(ncoe)   :: coeh_land,coe_ts
    ! Fitting coefficients to predict ta92 over snow
    data coeh_land/9.60083e+001,-6.05660e-002, 3.51944e-001,-6.52072e-001,-3.79843e-001,   &
                   9.74554e-001,-9.93822e-004, 1.82148e-003,-1.19589e-003,-4.43338e-004,   &
                   2.32471e-003/
    ! Fitting coefficients to predict ts over snow and non-desert land surfaces
    data coe_ts/2.63055e+002,  2.24818e+000, -1.75953e+000, -2.46555e+000, -9.57587e-001,  &
                2.22300e+000, -6.05612e-003,  3.70943e-003,  8.40393e-003,  1.88572e-003,  &
               -5.06110e-003/
    ! Seven window channel indices used in the algorithm
    data wchan_index/3,4,5,6,7,8,9/
    !---Central Frequencies ct five window channel
    data freqw/19.35, 19.35, 21.3, 37.0, 37.0, 85.5, 85.5/


!    print *,'Testing: TRMM_TMI_ALG'
    ! SELECT TA AT WINDOWN CHANNELS
    DO ich = 1, nchanw  !19V,19H,22V,37V,37H,92V,92H
       taw(ich) = ta(wchan_index(ich))
    ENDDO

    ! SELECT TA AT WINDOW CHANNELS
    DO ich = 1, nchanw
       IF(abs(freqw(ich)-freq(wchan_index(ich))) >= epsilonLoose) THEN
          print *, 'Frequencies do not match in TRMM TMI surface pre-classifier:',&
               ich,wchan_index(ich),freqw(ich),freq(wchan_index(ich))
       ENDIF
    ENDDO

    ! START TO IDENTIFY SURFACE TYPE
    IF (landindex == NOCEAN) THEN
       surface_type = OC_TYP
    ELSE
       surface_type = LD_TYP
    ENDIF
    ! Calculate polarization difference and indices at 19 and 37 GHz
    DV19 = taw(1)-taw(2)   !TV19-TH19
    
    PR19 = (taw(1)-taw(2)) / (taw(1)+taw(2))
    PR37 = (taw(4)-taw(5)) / (taw(4)+taw(5))

     ! Calculate scattering differences at 37 and 92 GHz with respect to 19 GHz
    SCV37 = taw(1) - taw(4)
    SCH37 = taw(2) - taw(5)
    SCV92 = taw(1) - taw(6)
    SCVX92 = taw(3) - taw(6)
    SCH92 = taw(2) - taw(7)


     ! Calculate scattering index at 37 GHz horizontal polarization
    GRH37 = (taw(2) - taw(5)) / (taw(2) + taw(5))
    !---Predict surface types
    if (landindex == NOCEAN) then    ! over ocean conditions
       surface_type = OC_TYP
       if (PR19 <= 0.13 .and. GRH37 >= -0.07  .and. abs(lat) >= LATTH3) surface_type = SEAICE_TYP
       !---Get help from the latitude and longitude 
       call applySeaIceClimo(Year, Julday, lat, lon, landindex, surface_type)
    else                             ! over land conditions
!       if (DV19 >= 20.0 .and. taw(2) >= 210.0) then
!           surface_type = Desert_TYP
!       endif
       !---Predict SNOW TH92 from TA19 ~ TA37 using snow-free land fitting coeffs.
       TH92_ALG = coeh_land(1)
       DO ich=1,5
          TH92_ALG = TH92_ALG + coeh_land(ich+1)*taw(ich)
       ENDDO
       DO ich=6,10
          TH92_ALG = TH92_ALG + coeh_land(ich+1)*taw(ich-5)*taw(ich-5)
       ENDDO
       TH92_OBS = taw(7)
       ! PREDICT TS
       TS_ALG = coeh_land(1)
       DO ich=1,5
           TS_ALG = TS_ALG + coe_ts(ich+1)*taw(ich)
       ENDDO
       DO ich=6,10
           TS_ALG = TS_ALG + coe_ts(ich+1)*taw(ich-5)*taw(ich-5)
       ENDDO
       ! DEFAULT TYPE
       surface_type = LD_TYP
       !UPDATE LAND TYPE
       if (abs(lat) >= 60.0) then
          if( TH92_OBS - TH92_ALG >= SILOW) surface_type = SNOW_TYP
          if( SCH37 >= SILOW) surface_type = SNOW_TYP
       else
           if ( (abs(lat) >= LATTH1) .and. (TH92_OBS - TH92_ALG >= SIHIGH)) &
                surface_type = SNOW_TYP
           if ( (abs(lat) >= LATTH1) .and. (SCH37 >= SIHIGH)) &
                surface_type = SNOW_TYP
       endif
       if(  (SCVX92 <= 6.0 .or. SCV37 <=6) .and. DV19 >= 8.0) surface_type = LD_TYP
       if (TS_ALG >= 273.0 .or. taw(3) >=  258.0) surface_type = LD_TYP
       
!      Glacial snow
       if(abs(lat) >= 60.0 .and. taw(2) .le. 215.0 .and. DV19 >= 23.0) surface_type = SNOW_TYP
 
!     endif  ! non desert
   endif    ! non ocean
 END SUBROUTINE TRMM_TMI_ALG


  SUBROUTINE MTMA_MADRAS_ALG(Year,Julday,nchan,freq,lat,lon,landindex,ta,surface_type,TskPreclass)
    !--------------------------------------------------------------------------------
    !M+
    ! NAME:
    !       MTMA_MADRAS_ALG (Simplified preclassifier)
    !
    ! PURPOSE:
    !       Distinguish five surface types (open ocean, sea ice, desert, snow-free and non-desert land, snow)
    ! NB: Simplified preclassifier only returns ocean, land surface types
    !
    ! 
    !
    !
    ! INPUT VARIABLES:
    !
    !   year        : Year
    !
    !   Julday      : Julian day
    !
    !   nchan       : CHANNEL NUMBER FOR A CERTAIN SENSOR
    !
    !   freq(nchan) : CENTRAL FREQUENCY LIST CORRESPONDING TO EACH CHANNEL AT A CERTAIN SENSOR
    !
    !   lat         : LATITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   lon         : LONGITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   landindex   : LAND/OCEAN INDEX (0: ocean, 1: land)
    !
    !   ta          : antenna temperatures at all channels
    !
    ! INTERNAL VARIABLES:
    !
    !   taw          : Megha-Tropiques MADRASantenna temperatures at seven window channels
    !
    !   taw(1)       : antenna temperature at 18.7 VPOL GHz
    !   taw(2)       : antenna temperature at 18.7 HPOL GHz
    !   taw(3)       : antenna temperature at 23.8 VPOL GHz
    !   taw(4)       : antenna temperature at 37.5 VPOL GHz
    !   taw(5)       : antenna temperature at 37.5 HPOL GHz
    !   taw(6)       : antenna temperature at 89   VPOL GHz
    !   taw(7)       : antenna temperature at 89   HPOL GHz
    !   taw(8)       : antenna temperature at 157  VPOL GHz
    !   taw(9)       : antenna temperature at 157  HPOL GHz
    !   freqw        : MT MADRAS frequencies at nine window channels
    !
    !
    ! OUTPUT VARIABLES:
    !
    !   surface_type    : OC_TYP (OPEN OCEAN)
    !                   : SEAICE_TYP (SEA ICE)
    !                   : Desert_TYP (DESERT)
    !                   : LD_TYP (SNOW-FREE AND NON-DESERT LAND)
    !                   : SNOW_TYP (SNOW)
    !
    !
    !
    ! LANGUAGE:
    !
    !       Fortran-95
    !
    ! INCLUDE FILES:
    !       None.
    !
    ! EXTERNALS:
    !       None.
    !
    ! COMMON BLOCKS:
    !       None.
    !
    ! FILES ACCESSED:
    !       None.
    !
    ! CREATION HISTORY:
    !
    !--------------------------------------------------------------------------------
    INTEGER,  PARAMETER         :: NOCEAN = 0
    INTEGER, INTENT(IN)         :: nchan,landindex
    INTEGER, INTENT(IN)         :: Year,Julday
    REAL,    INTENT(IN), DIMENSION(nchan)  :: ta,freq
    REAL,    INTENT(IN)         :: lat,lon
    REAL,    INTENT(IN)         :: TskPreclass
    INTEGER, INTENT(OUT)         :: surface_type

!    print *,'Testing: MTMA_MADRAS_ALG'

    ! START TO IDENTIFY SURFACE TYPE
    IF (landindex == NOCEAN) THEN
       surface_type = OC_TYP
    ELSE
       surface_type = LD_TYP
    ENDIF
  END SUBROUTINE MTMA_MADRAS_ALG


  SUBROUTINE MTSA_SAPHIR_ALG(Year,Julday,nchan,freq,lat,lon,landindex,ta,surface_type,TskPreclass)
    !--------------------------------------------------------------------------------
    !M+
    ! NAME:
    !       MTSA_SAPHIR_ALG (Simplified preclassifier)
    !
    ! PURPOSE:
    !       Distinguish five surface types (open ocean, sea ice, desert, snow-free and non-desert land, snow)
    ! NB: Simplified preclassifier only returns ocean, land surface types
    !
    ! 
    !
    !
    ! INPUT VARIABLES:
    !
    !   year        : Year
    !
    !   Julday      : Julian day
    !
    !   nchan       : CHANNEL NUMBER FOR A CERTAIN SENSOR
    !
    !   freq(nchan) : CENTRAL FREQUENCY LIST CORRESPONDING TO EACH CHANNEL AT A CERTAIN SENSOR
    !
    !   lat         : LATITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   lon         : LONGITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   landindex   : LAND/OCEAN INDEX (0: ocean, 1: land)
    !
    !   ta          : antenna temperatures at all channels
    !
    ! INTERNAL VARIABLES:
    !
    !   taw          : Megha-Tropiques SAPHIR antenna temperatures at seven window channels
    !
    !   taw(1)       : antenna temperature at 183 +/- 0.2 Mixed HPOL GHz
    !   taw(2)       : antenna temperature at 183 +/- 1.1 Mixed HPOL GHz
    !   taw(3)       : antenna temperature at 183 +/- 2.8 Mixed HPOL GHz
    !   taw(4)       : antenna temperature at 183 +/- 4.2 Mixed HPOL GHz
    !   taw(5)       : antenna temperature at 183 +/- 6.8 Mixed HPOL GHz
    !   taw(6)       : antenna temperature at 183 +/- 11.0 Mixed HPOL GHz
    !   freqw        : MT SAPHIR frequencies at nine window channels
    !
    ! OUTPUT VARIABLES:
    !
    !   surface_type    : OC_TYP (OPEN OCEAN)
    !                   : SEAICE_TYP (SEA ICE)
    !                   : Desert_TYP (DESERT)
    !                   : LD_TYP (SNOW-FREE AND NON-DESERT LAND)
    !                   : SNOW_TYP (SNOW)
    !
    !
    !
    ! LANGUAGE:
    !
    !       Fortran-95
    !
    ! INCLUDE FILES:
    !       None.
    !
    ! EXTERNALS:
    !       None.
    !
    ! COMMON BLOCKS:
    !       None.
    !
    ! FILES ACCESSED:
    !       None.
    !
    ! CREATION HISTORY:
    !
    !--------------------------------------------------------------------------------
    INTEGER,  PARAMETER         :: NOCEAN = 0
    INTEGER, INTENT(IN)         :: nchan,landindex
    INTEGER, INTENT(IN)         :: Year,Julday
    REAL,    INTENT(IN), DIMENSION(nchan)  :: ta,freq
    REAL,    INTENT(IN)         :: lat,lon
    REAL,    INTENT(IN)         :: TskPreclass
    INTEGER, INTENT(OUT)         :: surface_type

!    print *,'Testing: MTSA_SAPHIR_ALG'

    ! START TO IDENTIFY SURFACE TYPE
    IF (landindex == NOCEAN) THEN
       surface_type = OC_TYP
    ELSE
       surface_type = LD_TYP
    ENDIF
  END SUBROUTINE MTSA_SAPHIR_ALG


!===============================================================
! Name:         applySeaIceClimo
!
!
! Type:         Subroutine
!
!
! Description:  Applies sea ice climatology to constrain equatorward extent 
!               of scenes classified as sea ice

! Source: National Snow and Ice Data Center
!         Ice persistence climatology based on combined SMMR and SSMI observations from 1979-2000
! http://nsidc.org/data/nsidc-0192.html
! ftp://sidads.colorado.edu/pub/DATASETS/seaice/polar-stereo/trends-climatologies/ice-persistence
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - Year               I             Year
!       - Julday             I             Julian day
!       - xlat               I             Latitude
!       - xlon               I             Longitude
!       - landindex          I             Ocean/Land flag
!       - surface_type       I/O           Classified surface type (possibly modified from input value)
!
!
! Modules needed:
!       - misc
!
!
! History:
!       02-16-2009      C. Grassotti, IMSG Inc @ NOAA/NESDIS/STAR
!
!===============================================================

  SUBROUTINE applySeaIceClimo(Year, Julday, xlat, xlon, landindex, surface_type)
    INTEGER,INTENT(IN)    :: landindex
    INTEGER,INTENT(IN)    :: Year,Julday
    REAL,INTENT(IN)       :: xlat,xlon
    INTEGER,INTENT(INOUT) :: surface_type
    integer :: surface_type_orig

    INTEGER, PARAMETER    :: NOCEAN = 0
    INTEGER, PARAMETER :: nLonBins=36,nTimeBins=12
    INTEGER :: iLonBin,month,day
    REAL :: WidthLonBin=10. ! width of longitude bins for storing latitude thresholds (degrees)

    !---Data for latitude thresholds (varying with longitude and season)    
    !---Northern Hem. values
    REAL,    DIMENSION(nlonBins,nTimeBins) :: threshLatNH
    REAL,    DIMENSION(nlonBins,nTimeBins) :: threshLatSH

    !---Data for latitude thresholds (varying with longitude and season)    
    !---Note: data for Feb and Aug were derived directly from data (tie points);
    !---remaining months obtained by linear interpolation

    !---Northern Hem. values
    data threshLatNH / &

    72.3,59.5,59.5,65.3,66.0,66.2,65.0,65.0,65.0,65.0,65.0,65.0,65.0,45.8,45.8,49.1,52.5,56.7, &
    52.5,52.5,56.7,56.7,56.7,50.0,50.0,50.0,50.0,50.0,50.0,45.8,48.3,58.0,65.0,67.0,67.0,69.3, & ! Jan

    72.0,57.0,57.0,64.0,64.0,65.0,65.0,65.0,65.0,65.0,65.0,65.0,65.0,42.0,42.0,46.0,50.0,55.0, &
    50.0,50.0,55.0,55.0,55.0,50.0,50.0,50.0,50.0,50.0,50.0,45.0,48.0,58.0,65.0,67.0,67.0,69.0, & ! Feb (tie point)

    72.3,59.5,59.5,65.3,66.0,66.2,65.0,65.0,65.0,65.0,65.0,65.0,65.0,45.8,45.8,49.1,52.5,56.7, &
    52.5,52.5,56.7,56.7,56.7,50.0,50.0,50.0,50.0,50.0,50.0,45.8,48.3,58.0,65.0,67.0,67.0,69.3, & ! Mar

    72.7,62.0,62.0,66.6,68.0,67.3,65.0,65.0,65.0,65.0,65.0,65.0,65.0,49.6,49.6,52.3,55.0,58.3, &
    55.0,55.0,58.3,58.3,58.3,50.0,50.0,50.0,50.0,50.0,50.0,46.7,48.7,58.0,65.0,67.0,67.0,69.7, & ! Apr

    73.0,64.5,64.5,68.0,70.0,68.5,65.0,65.0,65.0,65.0,65.0,65.0,65.0,53.5,53.5,55.5,57.5,60.0, &
    57.5,57.5,60.0,60.0,60.0,50.0,50.0,50.0,50.0,50.0,50.0,47.5,49.0,58.0,65.0,67.0,67.0,70.0, & ! May

    73.3,67.0,67.0,69.3,72.0,69.7,65.0,65.0,65.0,65.0,65.0,65.0,65.0,53.5,57.3,58.6,60.0,61.7, &
    60.0,60.0,61.7,61.7,61.7,50.0,50.0,50.0,50.0,50.0,50.0,48.3,49.3,58.0,65.0,67.0,67.0,70.3, & ! Jun

    73.7,69.5,69.5,70.6,74.0,70.8,65.0,65.0,65.0,65.0,65.0,65.0,65.0,53.5,61.1,61.8,62.5,63.3, &
    62.5,62.5,63.3,63.3,63.3,50.0,50.0,50.0,50.0,50.0,50.0,49.2,49.7,58.0,65.0,67.0,67.0,70.7, & ! Jul

    74.0,72.0,72.0,72.0,76.0,72.0,65.0,65.0,65.0,65.0,65.0,65.0,65.0,65.0,65.0,65.0,65.0,65.0, &
    65.0,65.0,65.0,65.0,65.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0,58.0,65.0,67.0,67.0,71.0, & ! Aug (tie point)

    73.7,69.5,69.5,70.6,74.0,70.8,65.0,65.0,65.0,65.0,65.0,65.0,65.0,53.5,61.1,61.8,62.5,63.3, &
    62.5,62.5,63.3,63.3,63.3,50.0,50.0,50.0,50.0,50.0,50.0,49.2,49.7,58.0,65.0,67.0,67.0,70.7, & ! Sep

    73.3,67.0,67.0,69.3,72.0,69.7,65.0,65.0,65.0,65.0,65.0,65.0,65.0,53.5,57.3,58.6,60.0,61.7, &
    60.0,60.0,61.7,61.7,61.7,50.0,50.0,50.0,50.0,50.0,50.0,48.3,49.3,58.0,65.0,67.0,67.0,70.3, & ! Oct

    73.0,64.5,64.5,68.0,70.0,68.5,65.0,65.0,65.0,65.0,65.0,65.0,65.0,53.5,53.5,55.5,57.5,60.0, &
    57.5,57.5,60.0,60.0,60.0,50.0,50.0,50.0,50.0,50.0,50.0,47.5,49.0,58.0,65.0,67.0,67.0,70.0, & ! Nov

    72.7,62.0,62.0,66.6,68.0,67.3,65.0,65.0,65.0,65.0,65.0,65.0,65.0,49.6,49.6,52.3,55.0,58.3, &
    55.0,55.0,58.3,58.3,58.3,50.0,50.0,50.0,50.0,50.0,50.0,46.7,48.7,58.0,65.0,67.0,67.0,69.7/   ! Dec

    !---Southern Hem. values (note: these are absolute values of latitude)
    data threshLatSH / &

    62.5,62.8,63.3,60.8,60.0,59.2,59.2,59.2,59.2,59.2,59.2,59.2,59.2,59.2,59.2,59.2,62.3,62.3, &
    62.3,62.3,62.3,62.3,62.3,62.3,63.3,63.3,61.7,60.0,58.8,58.8,57.7,57.5,58.6,58.6,62.5,62.5, & ! Jan

    65.0,65.0,65.0,62.0,61.0,60.0,60.0,60.0,60.0,60.0,60.0,60.0,60.0,60.0,60.0,60.0,64.0,64.0, &
    64.0,64.0,64.0,64.0,64.0,64.0,64.0,64.0,62.0,60.0,59.0,59.0,58.0,58.0,60.0,62.0,65.0,65.0, & ! Feb (tie point)

    62.5,62.8,63.3,60.8,60.0,59.2,59.2,59.2,59.2,59.2,59.2,59.2,59.2,59.2,59.2,59.2,62.3,62.3, &
    62.3,62.3,62.3,62.3,62.3,62.3,63.3,63.3,61.7,60.0,58.8,58.8,57.7,57.5,58.6,58.6,62.5,62.5, & ! Mar

    60.0,60.6,61.7,59.7,59.0,58.3,58.3,58.3,58.3,58.3,58.3,58.3,58.3,58.3,58.3,58.3,60.7,60.7, &
    60.7,60.7,60.7,60.7,60.7,60.7,62.7,62.7,61.3,60.0,58.7,58.7,57.3,57.0,57.3,57.3,60.0,60.0, & ! Apr

    57.5,58.5,60.0,58.5,58.0,57.5,57.5,57.5,57.5,57.5,57.5,57.5,57.5,57.5,57.5,57.5,59.0,59.0, &
    59.0,59.0,59.0,59.0,59.0,59.0,62.0,62.0,61.0,60.0,58.5,58.5,57.0,56.5,56.0,56.0,57.5,57.5, & ! May

    55.0,56.3,58.3,57.3,57.0,56.7,56.7,56.7,56.7,56.7,56.7,56.7,56.7,56.7,56.7,56.7,57.3,57.3, &
    57.3,57.3,57.3,57.3,57.3,57.3,61.3,61.3,56.1,60.0,58.3,58.3,56.7,56.0,54.6,54.0,55.0,55.0, & ! Jun

    52.5,54.1,56.7,56.2,56.0,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.7,55.7, &
    55.7,55.7,55.7,55.7,55.7,55.7,57.5,57.5,55.6,60.0,58.2,58.2,56.3,55.5,53.3,52.0,52.5,52.5, & ! Jul

    50.0,52.0,55.0,55.0,55.0,55.0,55.0,55.0,55.0,55.0,55.0,55.0,55.0,55.0,55.0,55.0,54.0,54.0, &
    54.0,54.0,54.0,54.0,54.0,54.0,60.0,60.0,60.0,60.0,58.0,58.0,56.0,55.0,52.0,50.0,50.0,50.0, & ! Aug (tie point)

    52.5,54.1,56.7,56.2,56.0,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.8,55.7,55.7, &
    55.7,55.7,55.7,55.7,55.7,55.7,57.5,57.5,55.6,60.0,58.2,58.2,56.3,55.5,53.3,52.0,52.5,52.5, & ! Sep

    55.0,56.3,58.3,57.3,57.0,56.7,56.7,56.7,56.7,56.7,56.7,56.7,56.7,56.7,56.7,56.7,57.3,57.3, &
    57.3,57.3,57.3,57.3,57.3,57.3,61.3,61.3,56.1,60.0,58.3,58.3,56.7,56.0,54.6,54.0,55.0,55.0, & ! Oct

    57.5,58.5,60.0,58.5,58.0,57.5,57.5,57.5,57.5,57.5,57.5,57.5,57.5,57.5,57.5,57.5,59.0,59.0, &
    59.0,59.0,59.0,59.0,59.0,59.0,62.0,62.0,61.0,60.0,58.5,58.5,57.0,56.5,56.0,56.0,57.5,57.5, & ! Nov

    60.0,60.6,61.7,59.7,59.0,58.3,58.3,58.3,58.3,58.3,58.3,58.3,58.3,58.3,58.3,58.3,60.7,60.7, &
    60.7,60.7,60.7,60.7,60.7,60.7,62.7,62.7,61.3,60.0,58.7,58.7,57.3,57.0,57.3,57.3,60.0,60.0/   ! Dec

    threshLatNH = reshape(threshLatNH,(/nlonBins,nTimeBins/))
    threshLatSH = reshape(threshLatSH,(/nlonBins,nTimeBins/))
    surface_type_orig=surface_type


    !---Only apply if landindex=0 (ocean flag from database)
    IF (landindex == NOCEAN) THEN
    !---Convert year and julian day to calendar month and day 
    !---Month needed to select proper element from climatology
       call day_month(Year,month,day,Julday)
       
    !---Convert longitude to index (account for lons -180 to +180)
    !---Index used to select proper element from climatology
       if(xlon .lt. 0.)then
          iLonBin=int((xlon+360.)/widthLonBin)+1
       else
          iLonBin=int(xlon/widthLonBin)+1
    endif
       if(ilonBin .gt. nLonBins)ilonBin=nlonBins

    !---Check hemisphere, and use appropriate climatology
       if(xlat .ge. 0.)then
          if(abs(xlat) .lt. threshLatNH(ilonBin,month)) surface_type=OC_TYP
       else
          if(abs(xlat) .lt. threshLatSH(ilonBin,month)) surface_type=OC_TYP
       endif
    ELSE
       RETURN
    ENDIF

! debug
!    if(xlat .ge. 40.)then
!    if(xlat .ge. 75. .and. xlat .le. 80. .and. xlon .ge. -80. .and. xlon .le. -70.)then
!       write(*,*)'applySeaIceClimo: ',year,julday,month,day,xlat,xlon,ilonBin,threshLatNH(ilonBin,month),surface_type_orig,surface_type
!    endif

    RETURN

  END SUBROUTINE applySeaIceClimo


  SUBROUTINE FY3RI_MWRI_ALG(nchan,freq,lat,landindex,ta,surface_type,TskPreclass)
    !--------------------------------------------------------------------------------
    !M+
    ! NAME:
    !       AQUA_MWRI_ALG
    !
    ! PURPOSE:
    !       placeholder for AMSRE preclassifier. For now simply returns the topography.
    !
    !
    ! INPUT VARIABLES:
    !
    !   nchan       : CHANNEL NUMBER FOR A CERTAIN SENSOR
    !
    !   freq(nchan) : CENTRAL FREQUENCY LIST CORRESPONDING TO EACH CHANNEL AT A CERTAIN SENSOR
    !
    !   lat         : LATITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   landindex   : LAND/OCEAN INDEX (0: ocean, 1: land)
    !
    !   ta          : antenna temperatures at all channels
    !
    !
    ! OUTPUT VARIABLES:
    !
    !   surface_type    : OC_TYP (OPEN OCEAN)
    !                   : SEAICE_TYP (SEA ICE)
    !                   : Desert_TYP (DESERT)
    !                   : LD_TYP (SNOW-FREE AND NON-DESERT LAND)
    !                   : SNOW_TYP (SNOW)
    !
    !
    !
    ! Sid Boukabara, NOAA/NESDIS
    !
    !-------------------------------------------------------------------------------------------------------
    !...using the 10v channel of Fy3-mwri to justify the seaice. if tb10v>230K, the surface type is seaice
    !...by Tiger.Yang
    !...02/25/2009
    !------------------------------------------------------------------------------------------------------ 
    !  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
    !  General Public License as published by the Free Software Foundation; either version 2 of the License,
    !  or (at your option) any later version.
    !
    !  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
    !  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
    !  License for more details.
    !
    !  You should have received a copy of the GNU General Public License along with this program; if not, write
    !  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
    !
    !--------------------------------------------------------------------------------
    INTEGER,  PARAMETER         :: NOCEAN = 0
    INTEGER                     :: surface_type
    INTEGER, INTENT(IN)         :: nchan,landindex
    REAL                        :: lat,TskPreclass
    REAL, INTENT(IN), DIMENSION(nchan)  :: ta,freq
    IF (landindex == NOCEAN) THEN
       surface_type = OC_TYP
    ELSE
       surface_type = LD_TYP
    ENDIF

    IF (surface_type == OC_TYP) THEN
       IF (ta(1) > 230.) surface_type = SEAICE_TYP
       
    ENDIF
    
  END SUBROUTINE FY3RI_MWRI_ALG

  SUBROUTINE NPP_ATMS_ALG(Year,Julday,nchan,freq,lat,lon,landindex,ta,surface_type,TskPreclass)
    !--------------------------------------------------------------------------------
    !M+
    ! NAME:
    !       NPP_ATMS_ALG
    !
    ! PURPOSE:
    !       Distinguish four surface types (open ocean, sea ice, snow-free land, snow)
    !
    ! from land index and NPP ATMS antenna/brightness temperatures at window channels.
    !
    !
    ! INPUT VARIABLES:
    !
    !   year        : Year
    !
    !   Julday      : Julian day
    !
    !   nchan       : CHANNEL NUMBER FOR A CERTAIN SENSOR
    !
    !   freq(nchan) : CENTRAL FREQUENCY LIST CORRESPONDING TO EACH CHANNEL AT A CERTAIN SENSOR
    !
    !   lat         : LATITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   lon         : LONGITUDE OF OBSERVED PIXEL IN DEGREE
    !
    !   landindex   : LAND/OCEAN INDEX (0: ocean, 1: land)
    !
    !   ta          : antenna temperatures at all channels
    !
    ! INTERNAL VARIABLES:
    !
    !   taw          : NPP ATMS antenna temperatures at five window channels
    !
    !   taw(1)       : antenna temperature at 23.8 GHz
    !   taw(2)       : antenna temperature at 31.4 GHz
    !   taw(3)       : antenna temperature at 50.3 GHz
    !   taw(4)       : antenna temperature at 88.2 GHz
    !   taw(5)       : antenna temperature at 165.5 GHz
    !
    !   freqw        : NPP ATMS frequencies at five window channels
    !
    ! OUTPUT VARIABLES:
    !
    !   surface_type    : 0 (OPEN OCEAN)
    !                   : 1 (SEA ICE)
    !                   : 2 (SNOW-FREE LAND)
    !                   : 3 (SNOW)
    !
    !
    !
    ! LANGUAGE:
    !
    !       Fortran-95
    !
    ! INCLUDE FILES:
    !       None.
    !
    ! EXTERNALS:
    !       None.
    !
    ! COMMON BLOCKS:
    !       None.
    !
    ! FILES ACCESSED:
    !       None.
    !
    ! CREATION HISTORY:
    !       Written by:     Kevin Garrett (slightly modified from N18_AMSU_MHS_ALG)
    !                       2009-06-16
    !
    !
    !M-
    !--------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER,  PARAMETER      :: NOCEAN = 0,ncoe = 7,nchanw = 5
    REAL,     PARAMETER      :: SILOW = 5.0, SIHIGH = 10.0, LATTH1 = 30.0,LATTH2 = 40.0,LATTH3 = 50.0
    INTEGER                  :: nchan,wchan_index,landindex, surface_type,k,ich,jch,nd
    INTEGER                  :: Year,Julday
    REAL                     :: lat,lon,TA92_SICE,TA157_SICE,TA92_SNOW,TA157_SNOW,TA_ICE(2),TA_SNOW(2),SI,TskPreclass
    REAL,     DIMENSION(:)      :: ta,freq
    REAL,     DIMENSION(nchanw) :: taw,freqw
    REAL,     DIMENSION(2,ncoe) :: coe_oc,coe_land

    !---Fitting coefficients to predict ta92 over open ocean
    data (coe_oc(1,k),k=1,ncoe)/6.76185e+002,  2.55301e+000,  2.44504e-001, -6.88612e+000,   &
         -5.01409e-003, -1.41372e-003,  1.59245e-002/
    !---Fitting coefficients to predict ta157 over open ocean
    data (coe_oc(2,k),k=1,ncoe)/ 5.14546e+002,  6.06543e+000, -6.09327e+000, -2.81614e+000,   &
         -1.35415e-002,  1.29683e-002 , 7.69443e-003/
    !---Fitting coefficients to predict ta92 over snow-free land
    data (coe_land(1,k),k=1,ncoe)/-3.09009e+002,  1.74746e+000, -2.01890e+000,  3.43417e+000, &
         -2.85680e-003,  3.53140e-003, -4.39255e-003/
    !---Fitting coefficients to predict ta157 over snow-free land
    data (coe_land(2,k),k=1,ncoe)/-1.01014e+001,  3.97994e+000, -4.11268e+000,  1.26658e+000, &
         -9.52526e-003,  8.75558e-003,  4.77981e-004/
    !---Central Frequencies ct five window channel
    data freqw/23.8, 31.4, 50.3, 88.2, 165.5/

    ! SELECT TA AT WINDOWN CHANNELS
    DO ich = 1, nchanw
       wchan_index = 1
       DO jch = 1, NCHAN
          IF(abs(freqw(ich)-freq(jch)) <= 0.5) THEN
             wchan_index = jch
             EXIT
          ENDIF
       ENDDO
       taw(ich) = ta(wchan_index)
       IF (ich == 1 .and. wchan_index /= 1)  PRINT *,'CHANNEL ORDER IS INCONSISTENT TO OFFICIAL WAY'
       IF (ich == 2 .and. wchan_index /= 2)  PRINT *,'CHANNEL ORDER IS INCONSISTENT TO OFFICIAL WAY'
       IF (ich == 3 .and. wchan_index /= 3)  PRINT *,'CHANNEL ORDER IS INCONSISTENT TO OFFICIAL WAY'
       IF (ich == 4 .and. wchan_index /= 16) PRINT *,'CHANNEL ORDER IS INCONSISTENT TO OFFICIAL WAY'
       IF (ich == 5 .and. wchan_index /= 17) PRINT *,'CHANNEL ORDER IS INCONSISTENT TO OFFICIAL WAY'
    ENDDO

    ! START TO IDENTIFY SURFACE TYPE
    IF (landindex == NOCEAN) THEN
       surface_type = OC_TYP
    ELSE
       surface_type = LD_TYP
    ENDIF
    !---Predict SEA ICE TA92 and TA157 from TA23 ~ TA50 using open ocean fitting coeffs.
    DO nd =1, 2
       TA_ICE(nd)= coe_oc(nd,1)
       DO ich=1,3
          TA_ICE(nd) = TA_ICE(nd) + coe_oc(nd,ich+1)*taw(ich)
       ENDDO
       DO ich=1,3
          TA_ICE(nd) = TA_ICE(nd) + coe_oc(nd,ich+4)*taw(ich)*taw(ich)
       ENDDO
    ENDDO
    TA92_SICE  = TA_ICE(1)
    TA157_SICE = TA_ICE(2)
    !---Predict SEA ICE TA92 and TA157 from TA23 ~ TA50 using open ocean fitting coeffs.
    DO nd =1, 2
       TA_SNOW(nd)= coe_land(nd,1)
       DO ich=1,3
          TA_SNOW(nd) = TA_SNOW(nd) + coe_land(nd,ich+1)*taw(ich)
       ENDDO
       DO ich=1,3
          TA_SNOW(nd) = TA_SNOW(nd) + coe_land(nd,ich+4)*taw(ich)*taw(ich)
       ENDDO
    ENDDO
    TA92_SNOW  = TA_SNOW(1)
    TA157_SNOW = TA_SNOW(2)
    !---COMPUTE SI = TA23 - TA92
    SI = taw(1)-taw(4)
    !---Predict surface types
    if (landindex == NOCEAN) then    ! over ocean conditions
       surface_type = OC_TYP
       if (TA92_SICE - taw(4) >= 10.0 .and. abs(lat) >= LATTH2)  surface_type = SEAICE_TYP
       if (abs(lat) >= LATTH2 .and. SI >= SIHIGH)                surface_type = SEAICE_TYP
       if (abs(lat) >= LATTH3 .and. SI >= SILOW)                 surface_type = SEAICE_TYP
       if (abs(lat) >= LATTH3 .and. taw(1) >= 235.0)             surface_type = SEAICE_TYP
       !---Get help from the tskin temperature
       if (TskPreclass >= 280.)                                  surface_type = OC_TYP 
       if (TskPreclass <= 265. .and. TskPreclass >=0.)           surface_type = SEAICE_TYP 
       !---Get help from the latitude
!       if (abs(lat) <= 50. )                                     surface_type = OC_TYP
       !---Get help from the latitude and longitude 
       call applySeaIceClimo(Year, Julday, lat, lon, landindex, surface_type)
    else                             ! over land conditions
       surface_type = LD_TYP
       if(TA92_SNOW-taw(4)>=10.0.and.taw(1).le.260.0 .and. abs(lat) >= LATTH1) surface_type = SNOW_TYP
       if(abs(lat) >= LATTH2 .and. SI >= SIHIGH .and. taw(1).le.260.0)         surface_type = SNOW_TYP
       if(abs(lat) >= LATTH3 .and. SI >= SILOW .and. taw(1).le.260.0)          surface_type = SNOW_TYP
       if(taw(1) <= 210.0 .and. taw(3) <= 225.0 .and. abs(lat) >= LATTH3)      surface_type = SNOW_TYP
       !---Get help from the tskin temperature
       if (TskPreclass >= 280.)                                                surface_type = LD_TYP
       if (TskPreclass <= 265. .and. TskPreclass >=0.)                         surface_type = SNOW_TYP 
    endif
  END SUBROUTINE NPP_ATMS_ALG


END MODULE Preclassif
