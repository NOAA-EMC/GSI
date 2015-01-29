 module calc_latlons
 
 use program_setup, only       : domain_type, &
                                 imdl,        &
                                 jmdl

 implicit none

 real, allocatable            :: lat_mdl(:,:)        ! mass pts
 real, allocatable            :: lon_mdl(:,:)        ! mass pts
 real, allocatable            :: lat_vpnts_mdl(:,:)  ! velocity pts
 real, allocatable            :: lon_vpnts_mdl(:,:)  ! velocity pts

 contains

!-----------------------------------------------------------------------
! calculate lat/lons on the gaussian grid.
!
! note: for staggered grids, the static surface fields are only
!       calculated at the mass points.  the velocity points are only
!       output for use in later programs.
!-----------------------------------------------------------------------

 subroutine calc_latlons_mdl

 implicit none

 print*,'- CALCULATE LAT/LONS ON MODEL GRID'

 allocate (lat_mdl(imdl,jmdl))
 allocate (lon_mdl(imdl,jmdl))

 if (trim(domain_type) == 'gaussian') then
   
   call calc_latlons_gaussian

 elseif (trim(domain_type) == 'egrid') then
   
   allocate(lat_vpnts_mdl(imdl,jmdl))  ! velocity points
   allocate(lon_vpnts_mdl(imdl,jmdl))  ! velocity points

   call calc_latlons_egrid(lat_mdl, lon_mdl, lat_vpnts_mdl, lon_vpnts_mdl)

 elseif (trim(domain_type) == 'latlon') then

   call calc_latlons_latlon

 elseif (trim(domain_type) == 'lambconf') then

   call calc_latlons_lambconf

 endif

 return

 end subroutine calc_latlons_mdl

!------------------------------------------------------------------
! calculate lat/lons on a lambert conformal grid.
!------------------------------------------------------------------

 subroutine calc_latlons_lambconf

 use program_setup, only       : lat_11_mdl,      &
                                 lon_11_mdl,      &
                                 dx_mdl,          &
                                 orient_lon_mdl,  &
                                 tangent_lat_mdl

 implicit none

 integer                      :: i, j
 integer                      :: iret

 real                         :: dx_in_meters

 dx_in_meters = dx_mdl * 111.0 * 1000.0

 do j = 1, jmdl
   do i = 1, imdl

     call w3fb12(float(i),float(j),lat_11_mdl,lon_11_mdl,  &
                 dx_in_meters, orient_lon_mdl, tangent_lat_mdl,  &
                 lat_mdl(i,j),lon_mdl(i,j), iret)

     if (iret /= 0) then
       print*,"BAD POINT IN LAMBERT CONFORMAL GRID CALCULATION: ",i,j
       call abort
     end if

   enddo
 enddo

 end subroutine calc_latlons_lambconf

!------------------------------------------------------------------
! calculate lat/lons on the gaussian grid
!------------------------------------------------------------------

 subroutine calc_latlons_gaussian

 implicit none

 integer                      :: i
 integer                      :: j

 real                         :: deltalon
 real                         :: lon
 real, allocatable            :: slat(:), wlat(:)

 deltalon = 360.0 / float(imdl)

 do i = 1, imdl
   lon = (i-1)*deltalon
   if (lon > 180.0) lon = lon - 360.0
   lon_mdl(i,:) = lon
 enddo

 allocate(slat(jmdl))
 allocate(wlat(jmdl))

 call splat(4, jmdl, slat, wlat)

 do j = 1, jmdl
   lat_mdl(:,j) = 90.0 - (acos(slat(j))* 180.0 / (4.*atan(1.)))
 enddo

 deallocate (slat)
 deallocate (wlat)

 return

 end subroutine calc_latlons_gaussian

!------------------------------------------------------------------
! calc lat/lons on a regular lat/lon grid.  indices increase
! from north to south and from greenwich.
!------------------------------------------------------------------

 subroutine calc_latlons_latlon

 use program_setup, only     : dx_mdl,   &
                               dy_mdl

 implicit none

 integer                    :: i, j

 do j = 1, jmdl
   do i = 1, imdl
     lat_mdl(i,j) = 90.0 - ((float(j)-0.5) * dy_mdl)
     lon_mdl(i,j) = (float(i) - 1.0) * dx_mdl      ! start at greenwich
     if (lon_mdl(i,j) > 180.0) lon_mdl(i,j) = lon_mdl(i,j) - 360.0
   enddo
 enddo

 return

 end subroutine calc_latlons_latlon

!------------------------------------------------------------------
! calculate lat/lons on nmm arakawa e grid using the routine
! formerly known at etall.
!------------------------------------------------------------------

!cggg SUBROUTINE ETALL(IM,JM,TPH0D_in,TLM0D_in,DLMD_in,DPHD_in,  &
!cggg       HLAT,HLON,VLAT,VLON)
 SUBROUTINE calc_latlons_egrid(HLAT,HLON,VLAT,VLON)

 use program_setup, only       : im => imdl,   &
                                 jm => jmdl,   &
                                 tph0d_in => centlat_mdl,  &
                                 tlm0d_in => centlon_mdl,  &
                                 dlmd_in => dx_mdl, &
                                 dphd_in => dy_mdl


!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM: ETALL         COMPUTE EARTH LATITUDE & LONIGTUDE OF
!                           ETA GRID POINTS
!   PRGMMR: ROGERS          ORG: W/NP22     DATE: 90-06-13
!
! ABSTRACT: COMPUTES THE EARTH LATITUDE AND LONGITUDE OF ETA GRID
!   POINTS (BOTH H AND V POINTS)
!
! PROGRAM HISTORY LOG:
!   90-06-13  E.ROGERS
!   98-06-09  M.BALDWIN - CONVERT TO 2-D CODE
!   01-01-03  T BLACK   - MODIFIED FOR MPI
!
! USAGE:    CALL ETALL(HLAT,HLON,VLAT,VLON)
!   INPUT ARGUMENT LIST:
!     NONE
!
!   OUTPUT ARGUMENT LIST:
!     HLAT     - LATITUDE OF H GRID POINTS IN RADIANS (NEG=S)
!     HLON     - LONGITUDE OF H GRID POINTS IN RADIANS (E)
!     VLAT     - LATITUDE OF V GRID POINTS IN RADIANS (NEG=S)
!     VLON     - LONGITUDE OF V GRID POINTS IN RADIANS (E)
!
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS/6000 SP
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!cggg       INTEGER, PARAMETER :: KNUM=SELECTED_REAL_KIND(13)
!      REAL(KIND=KNUM) :: ONE=1.,R180=180.,TWO=2.
!      REAL(KIND=KNUM) :: CLMH,CLMV,CTPH,CTPH0,CTPV,D2R,DLM,DLMD,DPH,DPHD     &
!     &                  ,FACTR,PI,R2D,SB,SBD,SPHH,SPHV,STPH,STPH0,STPV   &
!     &                  ,TDLM,TLMH,TLMV,TPH0,TPHH,TPHV,WB,WBD
!      REAL(KIND=KNUM),DIMENSION(IM,JM) :: GLATH,GLATV,GLONH,GLONV
      REAL :: ONE=1.,R180=180.,TWO=2.
      REAL :: CLMH,CLMV,CTPH,CTPH0,CTPV,D2R,DLM,DLMD,DPH,DPHD     &
                       ,FACTR,PI,R2D,SB,SBD,SPHH,SPHV,STPH,STPH0,STPV   &
                       ,TDLM,TLMH,TLMV,TPH0,TPHH,TPHV,WB,WBD
      REAL,DIMENSION(IM,JM) :: GLATH,GLATV,GLONH,GLONV
      REAL,DIMENSION(IM,JM) :: HLAT,HLON,VLAT,VLON
!cggg
      integer :: i,j
      real    :: dtr, tdph, facth, factv, tlm0d, tph0d

!-----------------------------------------------------------------------
!--------------DERIVED GEOMETRICAL CONSTANTS----------------------------
!----------------------------------------------------------------------

        TPH0D=TPH0D_in
        TLM0D=TLM0D_in
        DPHD=DPHD_in
        DLMD=DLMD_in

      WBD=-(IM-ONE)*DLMD
      SBD=-(JM-1)/2*DPHD
      PI=ACOS(-ONE)
      DTR = PI / R180
      TPH0 = TPH0D * DTR
      WB = WBD * DTR
      SB = SBD * DTR
      DLM = DLMD * DTR
      DPH = DPHD * DTR

        write(6,*) 'TPH0,WB,SB,DLM,DPH,DTR: ', TPH0,WB,SB,DLM,DPH,DTR

      TDLM = DLM + DLM
      TDPH = DPH + DPH
!
      STPH0 = SIN(TPH0)
      CTPH0 = COS(TPH0)

!-----------------------------------------------------------------------
!---COMPUTE GEOGRAPHIC LAT AND LONG OF ETA GRID POINTS (H & V POINTS)---
!-----------------------------------------------------------------------
      DO 200 J = 1,JM
!
         TLMH = WB - TDLM + MOD(J+1,2) * DLM
         TPHH = SB+(J-1)*DPH
         TLMV = WB - TDLM + MOD(J,2) * DLM
         TPHV = TPHH
         STPH = SIN(TPHH)
         CTPH = COS(TPHH)
         STPV = SIN(TPHV)
         CTPV = COS(TPHV)
!----------------------------------------------------------------------
!---------- COMPUTE EARTH LATITUDE/LONGITUDE OF H POINTS --------------
!----------------------------------------------------------------------
         DO 201 I = 1,IM
           TLMH = TLMH + TDLM
           SPHH = CTPH0 * STPH + STPH0 * CTPH * COS(TLMH)
!cggg got problems near pole.
           if (sphh .gt. 1.) sphh = 1.
           GLATH(I,J) = ASIN(SPHH)
           CLMH = CTPH * COS(TLMH) / (COS(GLATH(I,J)) * CTPH0)    &
                     - TAN(GLATH(I,J)) * TAN(TPH0)
           IF(CLMH .GT. ONE) CLMH = ONE
           IF(CLMH .LT. -ONE) CLMH = -ONE
           FACTH = ONE
           IF(TLMH .GT. 0.) FACTH = -ONE
           GLONH(I,J) = -TLM0D * DTR + FACTH * ACOS(CLMH)

           HLAT(I,J) = GLATH(I,J) / DTR
           HLON(I,J)= -GLONH(I,J)/DTR
           IF(HLON(I,J) .GT. 180.) HLON(I,J) = HLON(I,J) - 360.
           IF(HLON(I,J) .LT. -180.) HLON(I,J) = HLON(I,J) + 360.
  201    CONTINUE


!----------------------------------------------------------------------
!---------- COMPUTE EARTH LATITUDE/LONGITUDE OF V POINTS --------------
!----------------------------------------------------------------------
         DO 202 I = 1,IM
           TLMV = TLMV + TDLM
           SPHV = CTPH0 * STPV + STPH0 * CTPV * COS(TLMV)
!cggg got problems near pole.
           if (sphv .gt. 1.) sphv = 1.
           GLATV(I,J) = ASIN(SPHV)
           CLMV = CTPV * COS(TLMV) / (COS(GLATV(I,J)) * CTPH0)   &
                - TAN(GLATV(I,J)) * TAN(TPH0)
           IF(CLMV .GT. 1.) CLMV = 1.
           IF(CLMV .LT. -1.) CLMV = -1.
           FACTV = 1.
           IF(TLMV .GT. 0.) FACTV = -1.
           GLONV(I,J) = -TLM0D * DTR + FACTV * ACOS(CLMV)
!
!    CONVERT INTO DEGREES AND EAST LONGITUDE
!
           VLAT(I,J) = GLATV(I,J) / DTR
           VLON(I,J) = -GLONV(I,J) / DTR
           IF(VLON(I,J) .GT. 180.) VLON(I,J) = VLON(I,J) - 360.
           IF(VLON(I,J) .LT. -180.) VLON(I,J) = VLON(I,J) + 360.

        if (mod(I,10) .eq. 0 .and. mod(J,10) .eq. 0) then
!       if (mod(I,1) .eq. 0 .and. mod(J,1) .eq. 0) then
        write(6,*) 'I,J,HLAT,HLON,VLAT,VLON: ', I,J,HLAT(I,J),HLON(I,J) &
                                                   ,VLAT(I,J),VLON(I,J)
        endif
  202    CONTINUE
  200 CONTINUE

 RETURN
 END subroutine calc_latlons_egrid

!-----------------------------------------------------------------------
! free up memory
!-----------------------------------------------------------------------

 subroutine calc_latlons_cleanup

 if (allocated(lat_mdl))       deallocate (lat_mdl)
 if (allocated(lon_mdl))       deallocate (lon_mdl)
 if (allocated(lat_vpnts_mdl)) deallocate (lat_vpnts_mdl)
 if (allocated(lon_vpnts_mdl)) deallocate (lon_vpnts_mdl)
 
 end subroutine calc_latlons_cleanup

 end module calc_latlons
