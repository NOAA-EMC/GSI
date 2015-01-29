 module ll2xy_utils
!$$$  module documentation block
!
! module:    ll2xy_utils
!   prgmmr: gayno         org: w/np2     date: 2005-05-20
!
! abstract: collection of routines that go from lat/lon
!           to x/y space on various grids.
!
! program history log:
!   2005-05-20  gayno   - initial version
!
! usage: use ll2xy_utils
!
! remarks: none.
!
! attributes:
!   language: fortran 90
!   machine:  ibm sp
!
!$$$

 contains

 subroutine ll2xy_egrid(glatd, glond, im, jm, &
                        tph0d, tlm0d, dlmd, dphd, &
                        im_output, jm_output, ii, jj)
!$$$  subprogram documentation block
!
! subprogram:   ll2xy_egrid
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: for a given array of lat/lons, find the nearest
!   neighbor points on an nmm e-grid.
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2006-08-09  gayno    - improved calculation of transformed lat/lon
!
! usage: call ll2xy_egrid(glatd, glond, im, jm,
!                         tph0d, tlm0d, dlmd, dphd,
!                         im_output, jm_output, ii, jj)
!
!   input argument list:
!     dlmd           - delta x of egrid
!     dphd           - delta y of egrid
!     glatd          - array of latitudes
!     glond          - array of longitudes
!     im             - i dimension of egrid
!     im_output      - i dimension of output arrays
!     jm             - j dimension of egrid
!     jm_output      - j dimension of output arrays
!     tlm0d          - center lon of egrid
!     tph0d          - center lat of egrid
!
!   output argument list:
!     ii             - array of nearest i points
!     jj             - array of nearest j points
!
! remarks: for better results, compile with 8 byte floats.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer                       :: i, j
 integer, intent(out)          :: ii(im_output,jm_output)
 integer, intent(in)           :: im
 integer, intent(in)           :: im_output
 integer                       :: imt
 integer, intent(in)           :: jm
 integer, intent(in)           :: jm_output
 integer                       :: jmt
 integer, intent(out)          :: jj(im_output,jm_output)
 integer                       :: k
 integer                       :: krows
 integer                       :: ncol
 integer                       :: nrow

 real                          :: col
 real                          :: d1, d2
 real                          :: dlm
 real, intent(in)              :: dlmd
 real                          :: dlm1
 real                          :: dlm2
 real                          :: dph
 real, intent(in)              :: dphd
 real                          :: glat
 real, intent(in)              :: glatd(im_output,jm_output)
 real                          :: glon
 real, intent(in)              :: glond(im_output,jm_output)
 real                          :: row
 real                          :: tlat
 real                          :: tlat1
 real                          :: tlat2
 real                          :: tlm0
 real, intent(in)              :: tlm0d
 real                          :: tlon
 real                          :: tlon1
 real                          :: tlon2
 real, intent(in)              :: tph0d
 real                          :: tph0

 real :: STPH0, CTPH0, RELMI, SRLMI, CRLMI, SPH
 real :: CPH, CC, ANUM, DENOM
!
 real, PARAMETER               :: D2R=1.74532925E-2
 real, parameter               :: R2D=1./D2R
!-----------------------------------------------------------------
!***
!***  CONVERT FROM GEODETIC TO TRANSFORMED COORDINATES (DEGREES)
!***

 IMT=2*IM-1
 JMT=JM/2+1
 DPH=DPHD*D2R
 DLM=DLMD*D2R
 TPH0=TPH0D*D2R
 TLM0=TLM0D*D2R

 do j = 1, jm_output
 do i = 1, im_output

 GLAT=GLATD(i,j)*D2R
 GLON=GLOND(i,j)*D2R
 STPH0=SIN(TPH0)
 CTPH0=COS(TPH0)
 RELMI=GLON-TLM0
 SRLMI=SIN(RELMI)
 CRLMI=COS(RELMI)
 SPH=SIN(GLAT)
 CPH=COS(GLAT)
 CC=CPH*CRLMI
 ANUM=CPH*SRLMI
 DENOM=CTPH0*CC+STPH0*SPH
 TLON=-ATAN2(ANUM,DENOM)*R2D
 TLAT=ASIN(CTPH0*SPH-STPH0*CC)*R2D
!
!      WRITE(6,50)TLAT,TLON
! 50 FORMAT(' TRANSFORMED LATITUDE IS',F8.3, &
!                  4X,'LONGITUDE IS',F8.3)
!***
!***  FIND THE K VALUE OF THE NEAREST H POINT
!***
 ROW=TLAT/DPHD+JMT
 COL=TLON/DLMD+IM
 NROW=INT(ROW)
 NCOL=INT(COL)
 TLAT=TLAT*D2R
 TLON=TLON*D2R
!***
!***  FIRST CONSIDER THE SITUATION WHERE THE POINT X IS AT
!***
!***              V      H
!***
!***
!***                 X
!***              H      V
!***
 IF(MOD(NROW,2).EQ.1.AND.MOD(NCOL,2).EQ.1.OR.    &
    MOD(NROW,2).EQ.0.AND.MOD(NCOL,2).EQ.0)THEN
   TLAT1=(NROW-JMT)*DPH
   TLAT2=TLAT1+DPH
   TLON1=(NCOL-IM)*DLM
   TLON2=TLON1+DLM
   DLM1=TLON-TLON1
   DLM2=TLON-TLON2
   D1=ACOS(COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
   D2=ACOS(COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
   IF(D1.GT.D2)THEN
     NROW=NROW+1
     NCOL=NCOL+1
   ENDIF
!***
!***  NOW CONSIDER THE SITUATION WHERE THE POINT X IS AT
!***
!***              H      V
!***
!***
!***                 X
!***              V      H
!***
 ELSE
   TLAT1=(NROW+1-JMT)*DPH
   TLAT2=TLAT1-DPH
   TLON1=(NCOL-IM)*DLM
   TLON2=TLON1+DLM
   DLM1=TLON-TLON1
   DLM2=TLON-TLON2
   D1=ACOS(COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
   D2=ACOS(COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
   IF(D1.LT.D2)THEN
     NROW=NROW+1
   ELSE
     NCOL=NCOL+1
   ENDIF
 ENDIF
 JJ(i,j)=NROW
 II(i,j)=NCOL/2
 IF(MOD(JJ(i,j),2).EQ.1)II(i,j)=II(i,j)+1
!***
!***  NOW WE CAN FIND THE K VALUE
!***
! KROWS=((NROW-1)/2)*IMT
! IF(MOD(NROW,2).EQ.1)THEN
!   K=KROWS+(NCOL+1)/2
! ELSE
!   K=KROWS+IM+NCOL/2
! ENDIF
!      WRITE(6,100)K,II(i,j),JJ(i,j)
! 100 FORMAT(' NEAREST HEIGHT POINT AT K=',I6,' I=',i4,' J=',i4)
!
 enddo
 enddo

 return

 END subroutine ll2xy_egrid

 subroutine ll2xy_gaussian(kgds_input, imdl_output, jmdl_output, &
                           lats_output, lons_output, xpts, ypts)
!$$$  subprogram documentation block
!
! subprogram:   ll2xy_gaussian
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  given the lat/lons on a gaussian grid, find the
!   corresponding x/y values on another gaussian grid.
! 
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage:  call ll2xy_gaussian(kgds_input, imdl_output, jmdl_output,
!                             lats_output, lons_output, xpts, ypts)
!
!   input argument list:
!     imdl_output    - i-dimension of output grid
!     jmdl_output    - j-dimension of output grid
!     kgds_input     - grib gds info of input grid
!     lats_output    - latitudes of output grid
!     lons_output    - longitudes of output grid
!
!   output argument list:
!     xpts  - x index with respect to the input grid
!     ypts  - y index with respect to the input grid
!   
! remarks: works on 2-d arrays.  (want to get rid of this one)
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$
 
 implicit none

 integer, intent(in)          :: imdl_output
 integer                      :: ijmdl_output
 integer, intent(in)          :: jmdl_output
 integer, intent(in)          :: kgds_input(200)
 integer                      :: lmap
 integer                      :: lrot
 integer                      :: nret

 real, allocatable            :: area(:,:)
 real, allocatable            :: crot(:,:)
 real, intent(in)             :: lats_output(imdl_output,jmdl_output)
 real, intent(in)             :: lons_output(imdl_output,jmdl_output)
 real, allocatable            :: srot(:,:)
 real, allocatable            :: xlat(:,:)
 real, allocatable            :: xlon(:,:)
 real, allocatable            :: ylat(:,:)
 real, allocatable            :: ylon(:,:)
 real, intent(out)            :: xpts(imdl_output,jmdl_output)
 real, intent(out)            :: ypts(imdl_output,jmdl_output)

 lmap = 0
 lrot = 0

 xpts = -999.9
 ypts = -999.9

 allocate(crot(imdl_output,jmdl_output)) ! dummy argument when lrot /=1
 allocate(srot(imdl_output,jmdl_output)) ! dummy argument when lrot /=1
 allocate(xlon(imdl_output,jmdl_output)) ! dummy argument when lmap /=1
 allocate(xlat(imdl_output,jmdl_output)) ! dummy argument when lmap /=1
 allocate(ylon(imdl_output,jmdl_output)) ! dummy argument when lmap /=1
 allocate(ylat(imdl_output,jmdl_output)) ! dummy argument when lmap /=1
 allocate(area(imdl_output,jmdl_output)) ! dummy argument when lmap /=1

!-----------------------------------------------------------------------
! call gdswzd to convert the lat/lon of the input grid points to the 
! corresponding x/y coordinates of the output grid.
!
! the "-1" argument tells routine to return x/y coordinates.
! the "-999.9" is a flag value for invalid data.
! the x/y coordinates (xpts,ypts) are floating point.
!-----------------------------------------------------------------------

 ijmdl_output = imdl_output * jmdl_output

 call gdswzd(kgds_input, -1, ijmdl_output, -999.9, xpts, ypts,        &
             lons_output, lats_output, nret, lrot, crot, srot, lmap,  &
             xlon, xlat, ylon, ylat, area )

 deallocate(crot)
 deallocate(srot)
 deallocate(xlon)
 deallocate(xlat)
 deallocate(ylon)
 deallocate(ylat)
 deallocate(area)

 return

 end subroutine ll2xy_gaussian

 subroutine ll2xy_gaussian_1d(kgds_input, ijmdl_output,    &
                              lats_output, lons_output,    &
                              xindx_wrt_input_grid,        &
                              yindx_wrt_input_grid)
!$$$  subprogram documentation block
!
! subprogram:   ll2xy_gaussian_1d
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  given the lat/lons on a gaussian grid, find the
!   corresponding x/y values on another gaussian grid.
! 
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage:  ll2xy_gaussian_1d(kgds_input, ijmdl_output,   
!                           lats_output, lons_output,   
!                           xindx_wrt_input_grid,      
!                           yindx_wrt_input_grid)
!
!   input argument list:
!     ijmdl_output   - dimension of output grid
!     kgds_input     - grib gds info of input grid
!     lats_output    - latitudes of output grid
!     lons_output    - longitudes of output grid
!
!   output argument list:
!     xindx_wrt_input_grid - x index with respect to the 
!                            input grid
!     yindx_wrt_input_grid - y index with respect to the
!                            input grid
!   
! remarks: works on 1-d arrays.  (want to keep this one)
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$
 
 implicit none

 integer, intent(in)          :: ijmdl_output
 integer, intent(in)          :: kgds_input(200) 
 integer                      :: lmap
 integer                      :: lrot
 integer                      :: nret

 real, allocatable            :: area(:)
 real, allocatable            :: crot(:)
 real, intent(in)             :: lats_output(ijmdl_output)
 real, intent(in)             :: lons_output(ijmdl_output)
 real, allocatable            :: srot(:)
 real, allocatable            :: xlat(:)
 real, allocatable            :: xlon(:)
 real, allocatable            :: ylat(:)
 real, allocatable            :: ylon(:)
 real, intent(out)            :: xindx_wrt_input_grid(ijmdl_output)
 real, intent(out)            :: yindx_wrt_input_grid(ijmdl_output)

 lmap = 0
 lrot = 0

 xindx_wrt_input_grid = -999.9
 yindx_wrt_input_grid = -999.9

 allocate(crot(ijmdl_output)) ! dummy argument when lrot /=1
 allocate(srot(ijmdl_output)) ! dummy argument when lrot /=1
 allocate(xlon(ijmdl_output)) ! dummy argument when lmap /=1
 allocate(xlat(ijmdl_output)) ! dummy argument when lmap /=1
 allocate(ylon(ijmdl_output)) ! dummy argument when lmap /=1
 allocate(ylat(ijmdl_output)) ! dummy argument when lmap /=1
 allocate(area(ijmdl_output)) ! dummy argument when lmap /=1

!-----------------------------------------------------------------------
! call gdswzd to convert the lat/lon of the input grid points to the 
! corresponding x/y coordinates of the output grid.
!
! the "-1" argument tells routine to return x/y coordinates.
! the "-999.9" is a flag value for invalid data.
! the x/y coordinates (xindx_wrt_input_grid,yindx_wrt_input_grid) are floating point.
!-----------------------------------------------------------------------

 call gdswzd(kgds_input, -1, ijmdl_output, -999.9, xindx_wrt_input_grid, yindx_wrt_input_grid,        &
             lons_output, lats_output, nret, lrot, crot, srot, lmap,  &
             xlon, xlat, ylon, ylat, area )

 deallocate(crot)
 deallocate(srot)
 deallocate(xlon)
 deallocate(xlat)
 deallocate(ylon)
 deallocate(ylat)
 deallocate(area)

 return

 end subroutine ll2xy_gaussian_1d

 subroutine ll2xy_nesdis (hemi, mesh, lat, lon, gridi, gridj)
!$$$  subprogram documentation block
!
! subprogram:   ll2xy_nesdis
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: for a given lat/lon, find the corresponding x/y 
!   on the nesdis/afwa polar stereographic grid
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage:  ll2xy_nesdis (hemi, mesh, lat, lon, gridi, gridj)
!
!   input argument list:
!     hemi           - hemisphere flag (1-nh,2-sh)
!     mesh           - another name for bedient
!                      ex: afwa data is 8th mesh
!     lat            - latitude
!     lon            - longitude
!
!   output argument list:
!     gridi          - x point
!     gridj          - y point
!
! remarks: for better results, compile with 8 byte floats.
!          NOT applicable for nesdis 4km polar stereographic
!          grid.  instead use routine ll2xy_nesdis_4km.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer, intent(in)        :: hemi
 integer                    :: idim
 integer                    :: jdim
 integer, intent(in)        :: mesh

 real, parameter            :: dtr = 3.141592654 / 180.0
 real, intent(out)          :: gridi
 real                       :: gridint
 real, intent(out)          :: gridj
 real, intent(in)           :: lat
 real, intent(in)           :: lon
 real                       :: rad
 real                       :: rm
 real, parameter            :: rotang  = 10.E0
 real                       :: xpole
 real                       :: ypole

 idim = mesh * 64
 jdim = idim

 GRIDINT = FLOAT(MESH) * 31.2043316E0 ! # grid intervals between pole and equator

 XPOLE   = idim / 2 + 1  ! x location of pole
 YPOLE   = jdim / 2      ! y location of pole

 RM    = GRIDINT * COS(lat * DTR) /  &
        (1.E0 - ((-1)**hemi) * SIN(lat * DTR) )
 RAD   = (lon - ROTANG ) * DTR
 GRIDI = XPOLE + RM * COS(RAD)
 GRIDJ = YPOLE - ( ((-1)**hemi) * RM * SIN(RAD) )

 return

 end subroutine ll2xy_nesdis

 subroutine ll2xy_nesdis_4km (alat_in,along_in,x,y)
!$$$  subprogram documentation block
!
! subprogram:   ll2xy_nesdis_4km
!   prgmmr: gayno          org: w/np2     date: 2005-06-30
!
! abstract: for a given lat/lon, find the corresponding x/y 
!   on the nesdis 4km polar stereographic grid
!
! program history log:
! 2005-06-30  gayno    - initial version
!
! usage:  ll2xy_nesdis_4km (alat_in, along_in, x, y)
!
!   input argument list:
!     alat_in          - latitude in degrees
!     along_in         - longitude in degrees
!
!   output argument list:
!     x                - corresponding x point on nesdis grid
!     y                - corresponding y point on nesdis grid
!
! remarks: this subroutine converts from geodetic latitude and 
!          longitude to Polar Stereographic (X,Y) coordinates. The equations  
!          are from Snyder, J. P., 1982,  Map Projections Used by the U.S. 
!          Geological Survey, Geological Survey Bulletin 1532, U.S. Government
!          Printing Office.  See JPL Technical Memorandum 3349-85-101
!          for further details.                         
!                                                        
!          NOTE! the nesdis 4km grid is NOT 1/96th bedient.  rather it
!          has a grid spacing of exactly 4 km.  it also differs from
!          the 1/16th bedient grid in that is assumes an oblate spheroid
!          earth (wgs-84 datum).
!                                                           
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$
!
      implicit none
!
      real, parameter   :: grid_spacing = 4.0   ! in km
      real, parameter   :: midpt_x = 3072.5  ! location of pole point.
      real, parameter   :: midpt_y = 3072.5  ! location of pole point.
      real, parameter   :: pi = 3.141592654
      real, parameter   :: pi_over_2 = pi / 2.0
      real, parameter   :: pi_over_4 = pi / 4.0
      real, parameter   :: deg_2_rad = pi / 180.0
      real, parameter   :: sgn    = 1.0   ! northern hemisphere
      real, parameter   :: slat   = 60.0  ! standard latitude
      real, parameter   :: orient = 80.0  ! orientation angle, 80W
      real, parameter   :: RE = 6378.137  ! radius of earth
      real, parameter   :: E2 = .00669437999013  ! eccentricity squared
                                                 ! wgs-84 datum
      real, parameter   :: E = sqrt(E2)
!
      real, intent(in)  :: alat_in, along_in
      real, intent(out) :: x, y
!
      real              :: alat, along, MC,T,TC,SL,RHO
!
!     Compute X and Y in grid coordinates.
!
      alat  = alat_in * deg_2_rad
      if ( (along_in+orient) < 0.0) then
        along = (along_in+orient+360.0) * deg_2_rad
      else
        along = (along_in+orient) * deg_2_rad
      end if
      IF (ABS(ALAT).LT.pi_over_2) GOTO 250
      X=midpt_x
      Y=midpt_y
      GOTO 999
  250 CONTINUE
      T=TAN(pi_over_4-ALAT/2.)/((1.-E*SIN(ALAT))/(1.+E*SIN(ALAT)))**(E/2.)
      IF (ABS(90.-SLAT).LT.1.E-5) THEN
      RHO=2.*RE*T/((1.+E)**(1.+E)*(1.-E)**(1.-E))**(1/2.)
      ELSE
      SL=SLAT*deg_2_rad
      TC=TAN(pi_over_4-SL/2.)/((1.-E*SIN(SL))/(1.+E*SIN(SL)))**(E/2.)
      MC=COS(SL)/SQRT(1.0-E2*(SIN(SL)**2))
      RHO=RE*MC*T/TC
      END IF
      y= midpt_y -RHO*SGN*cos(SGN*ALONG) / grid_spacing
      x= midpt_x +RHO*SGN*sin(SGN*ALONG) / grid_spacing
  999 CONTINUE
 end subroutine ll2xy_nesdis_4km

 subroutine xy2ll_nesdis_4km(ii,jj,lat,lon)
!$$$  subprogram documentation block
!
! subprogram:   xy2ll_nesdis_4km
!   prgmmr: gayno          org: w/np2     date: 2005-08-23
!
! abstract: for a given i/j, find the corresponding lat/lon
!   on the nesdis 4km polar stereographic grid
!
! program history log:
! 2005-08-23  gayno    - initial version
!
! usage:  xy2ll_nesdis_4km (ii, jj, lat, lon)
!
!   input argument list:
!     ii           - i point on nesdis grid
!     jj           - j point on nesdis grid
!
!   output argument list:
!     lat          - latitude in degrees
!     lon          - longitude in degrees
!
! remarks: this subroutine converts from polar stereographic (i/j)
!          coordinates to latitude/longitude.   The equations
!          are from Snyder, J. P., 1982,  Map Projections Used by the U.S.
!          Geological Survey, Geological Survey Bulletin 1532, U.S. Government
!          Printing Office.
!
!          NOTE! the nesdis 4km grid is NOT 1/96th bedient.  rather it
!          has a grid spacing of exactly 4 km.  it also differs from
!          the 1/16th bedient grid in that is assumes an oblate spheroid
!          earth (wgs-84 datum).  therefore, you can't use this
!          routine for the 23 km nesdis grid or the afwa 47 km grid.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$
 implicit none

 integer               :: iter
 integer, intent(in)   :: ii, jj

 real, parameter   :: RE = 6378.137        ! radius of earth in km
 real, parameter   :: grid_spacing = 4.0   ! in km
 real, parameter   :: midpt_x = 3072.5     ! location of pole point.
 real, parameter   :: midpt_y = 3072.5     ! location of pole point.
 real, parameter   :: pi = 3.141592654
 real, parameter   :: deg_2_rad = pi / 180.0
 real, parameter   :: rad_2_deg = 180.0 / pi
 real, parameter   :: slat = 60.0*deg_2_rad  ! standard latitude
 real, parameter   :: slat_over_2 = slat * 0.5
 real, parameter   :: E2 = .00669437999013   ! eccentricity squared
                                             ! wgs-84 datum
 real, parameter   :: E = sqrt(E2)
 real, parameter   :: E_over_2 = E * 0.5
 real, parameter   :: pi_over_2 = pi / 2.0
 real, parameter   :: pi_over_4 = pi / 4.0
 real, parameter   :: orient = -80.0  ! orientation angle, 80W

 real :: mc, tc, x, y, rho, lat1, t, diff

 real, intent(out)  :: lat, lon

 x = (float(ii)-midpt_x)*grid_spacing
 y = (float(jj)-midpt_y)*grid_spacing

 MC=COS(SLAT)/SQRT(1.0-E2*(SIN(SLAT)**2))
 TC=TAN(pi_over_4-SLAT_OVER_2) /  &
    ((1.-E*SIN(SLAT))/(1.+E*SIN(SLAT)))**E_OVER_2
 rho = sqrt(x*x + y*y)
 T = (RHO*TC)/(RE*MC)

 if (abs(y) < 0.01) then
   if (x > 0.0)  lon = orient + 90.0
   if (x <= 0.0) lon = orient - 90.0
 else
   lon = orient + atan(x/(-y))*rad_2_deg
   if (y > 0) lon = lon+180.
 end if

 lat1 = pi_over_2 - 2.0*atan(T)
 do iter = 1, 10
   lat  = pi_over_2 -  &
          2.0*atan(T*(((1.0-e*sin(lat1))/(1.0+e*sin(lat1)))**(E_OVER_2)))
   diff = abs(lat-lat1) * rad_2_deg
   if (diff < 0.000001) exit
   lat1=lat
 enddo

 lat = lat * rad_2_deg

 return

 end subroutine xy2ll_nesdis_4km

 end module ll2xy_utils
