module da_um_routines

  use da_control, only : es_alpha, es_beta, rd_over_rv, rd_over_rv1, &
      es_gamma,kappa,t_kelvin, gravity,gas_constant,cp, filename_len, radian, earth_omega

implicit none

!----------------------------------------------------------------------
! UM grid definition
! ************************  2x2 UM Grid   *****************************
!
!   V/P(1,3)                  V/P(2,3)                  V/P(3,3)             
!
!               MASS(1,2)     U-WD(1,2)    MASS(2,2)    U-WD(2,2)                                  
!
!   V/P(1,2)    V-WD(1,1)     V/P(2,2)     V-WD(2,1)    V/P(3,2)  
!
!               MASS(1,1)     U-WD(1,1)    MASS(2,1)    U-WD(2,1)  
!
!   V/P(1,1)                  V/P(2,1)                  V/P(3,1)
!
!----------------------------------------------------------------------

contains

subroutine da_rotlatlon( mlat, mlon, ulat, ulon, vlat, vlon, plat, plon, &
      dim1, dim2, um_lat1, um_lon1, um_dx_deg )

!---------------------------------------------------------------------- 
! Purpose: Compute lat-lon on rotated earth for mass, u, v, and 
!          vort/psi grid points.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 15/11/10 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!----------------------------------------------------------------------

   implicit none

   real, intent(out)  :: mlat(1:dim1,1:dim2)          ! mass points latitude
   real, intent(out)  :: mlon(1:dim1,1:dim2)          ! mass points longitude
   real, intent(out)  :: ulat(1:dim1,1:dim2)          ! u points latitude
   real, intent(out)  :: ulon(1:dim1,1:dim2)          ! u points longitude
   real, intent(out)  :: vlat(1:dim1,1:dim2-1)        ! v points latitude
   real, intent(out)  :: vlon(1:dim1,1:dim2-1)        ! v points longitude
   real, intent(out)  :: plat(1:dim1+1,1:dim2+1)      ! v points latitude
   real, intent(out)  :: plon(1:dim1+1,1:dim2+1)      ! v points longitude

   integer, intent(in):: dim1, dim2                   ! Dimensions.
   real, intent(in)   :: um_lat1                      ! lower left latitude
   real, intent(in)   :: um_lon1                      ! lower left longitude
   real, intent(in)   :: um_dx_deg                    ! grid resolution in degree

   integer            :: i, j                         ! Loop counters.
   integer            :: its, ite, jts, jte           ! UM dims (dummies for now).

!  mass points:
   its = 1
   ite = dim1
   jts = 1
   jte = dim2

   do j = jts, jte
      do i = its, ite
         mlat(i,j) =  um_lat1 + (float(j-1)*um_dx_deg)
         mlon(i,j) =  um_lon1 + (float(i-1)*um_dx_deg)
      end do
   end do

!  u-wind points:
   its = 1
   ite = dim1
   jts = 1
   jte = dim2

   do j = jts, jte
      do i = its, ite
         ulat(i,j) =  um_lat1 + (float(j-1)*um_dx_deg)
         ulon(i,j) =  (um_lon1+(um_dx_deg/2.0)) + (float(i-1)*um_dx_deg)
      end do
   end do

!  v-wind points:
   its = 1
   ite = dim1
   jts = 1
   jte = dim2-1

   do j = jts, jte
      do i = its, ite
         vlat(i,j) =  (um_lat1+(um_dx_deg/2.0)) + (float(j-1)*um_dx_deg)
         vlon(i,j) =  um_lon1 + (float(i-1)*um_dx_deg)
      end do
   end do

!  vort/psi points:
   its = 1
   ite = dim1+1
   jts = 1
   jte = dim2+1

   do j = jts, jte
      do i = its, ite
         plat(i,j) =  (um_lat1-(um_dx_deg/2.0)) + (float(j-1)*um_dx_deg)
         plon(i,j) =  (um_lon1-(um_dx_deg/2.0)) + (float(i-1)*um_dx_deg)
      end do
   end do

end subroutine da_rotlatlon

!------------------------------------------------------------------------------

SUBROUTINE EQTOLL(PHI_EQ,LAMBDA_EQ,PHI,LAMBDA,&
      PHI_POLE,LAMBDA_POLE,POINTS)

! ******************************COPYRIGHT******************************
! (c) CROWN COPYRIGHT 1995, METEOROLOGICAL OFFICE, All Rights Reserved.
!
! Use, duplication or disclosure of this code is subject to the
! restrictions as set forth in the contract.
!
!                Meteorological Office
!                London Road
!                BRACKNELL
!                Berkshire UK
!                RG12 2SZ
!
! If no contract has been raised with this copy of the code, the use,
! duplication or disclosure of it is strictly prohibited.  Permission
! to do so must first be obtained in writing from the Head of Numerical
! Modelling at the above address.
! ******************************COPYRIGHT******************************
!
!LL  Subroutine EQTOLL-------------------------------------------------
!LL
!LL  Purpose:  Calculates latitude and longitude on standard grid
!LL            from input arrays of latitude and longitude on
!LL            equatorial latitude-longitude (eq) grid used
!LL            in regional models. Both input and output latitudes
!LL            and longitudes are in degrees.
!LL
!LL  Written by A. Dickinson
!LL
!LL  Model            Modification history from model version 3.0:
!LL version  Date
!LL
!LL  Documentation: The transformation formulae are described in
!LL                 unified model on-line documentation paper S1.
!LL
!LL Logical components covered : S131
!LL
!LL Project task :
!LL
!LL External documentation:
!LL
!LLEND-----------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, intent(in) :: POINTS            !IN  Number of points to be processed

      REAL, intent(out)   :: PHI(POINTS)       !OUT Latitude
      REAL, intent(out)   :: LAMBDA(POINTS)    !OUT Longitude (0 =< LON < 360)
      REAL, intent(in)    :: LAMBDA_EQ(POINTS) !IN  Longitude in equatorial lat-lon coords
      REAL, intent(in)    :: PHI_EQ(POINTS)    !IN  Latitude in equatorial lat-lon coords
      REAL, intent(in)    :: PHI_POLE          !IN  Latitude of equatorial lat-lon pole
      REAL, intent(in)    :: LAMBDA_POLE       !IN  Longitude of equatorial lat-lon pole

! Workspace usage:-----------------------------------------------------
! None
!----------------------------------------------------------------------
! External subroutines called:-----------------------------------------
! None
!*---------------------------------------------------------------------
! Local varables:------------------------------------------------------
      REAL E_LAMBDA,E_PHI,A_LAMBDA,ARG,A_PHI,SIN_PHI_POLE,COS_PHI_POLE
      REAL TERM1,TERM2,SMALL,LAMBDA_ZERO
      INTEGER I
      PARAMETER(SMALL=1.0E-6)
!----------------------------------------------------------------------
! Constants from comdecks:---------------------------------------------
!*L------------------COMDECK C_PI---------------------------------------
!LL
!LL 4.0 19/09/95  New value for PI. Old value incorrect
!LL               from 12th decimal place. D. Robinson
!LL
      REAL PI,PI_OVER_180,RECIP_PI_OVER_180

      PARAMETER( PI=3.14159265358979323846,  & ! Pi
                 PI_OVER_180=PI/180.0,       & ! Conversion factor degrees to radians
                 RECIP_PI_OVER_180=180.0/PI )  ! Conversion factor radians to
                                               ! degrees
!*----------------------------------------------------------------------
!----------------------------------------------------------------------

!L 1. Initialise local constants
!
! Latitude of zeroth meridian
      LAMBDA_ZERO=LAMBDA_POLE+180.
! Sine and cosine of latitude of eq pole
      SIN_PHI_POLE=SIN(PI_OVER_180*PHI_POLE)
      COS_PHI_POLE=COS(PI_OVER_180*PHI_POLE)

!L 2. Transform from equatorial to standard latitude-longitude

      DO 200 I= 1,POINTS

! Scale eq longitude to range -180 to +180 degs

      E_LAMBDA=LAMBDA_EQ(I)
      IF(E_LAMBDA.GT. 180.0) E_LAMBDA=E_LAMBDA-360.0
      IF(E_LAMBDA.LT.-180.0) E_LAMBDA=E_LAMBDA+360.0

! Convert eq latitude & longitude to radians

      E_LAMBDA=PI_OVER_180*E_LAMBDA
      E_PHI=PI_OVER_180*PHI_EQ(I)

! Compute latitude using equation (4.7)

      ARG=COS_PHI_POLE*COS(E_LAMBDA)*COS(E_PHI) &
                         +SIN(E_PHI)*SIN_PHI_POLE
      ARG=MIN(ARG, 1.0)
      ARG=MAX(ARG,-1.0)
      A_PHI=ASIN(ARG)
      PHI(I)=RECIP_PI_OVER_180*A_PHI

! Compute longitude using equation (4.8)

      TERM1 =(COS(E_PHI)*COS(E_LAMBDA)*SIN_PHI_POLE &
             -SIN(E_PHI)*COS_PHI_POLE)
      TERM2=COS(A_PHI)
      IF(TERM2.LT.SMALL) THEN
        A_LAMBDA=0.0
      ELSE
        ARG=TERM1/TERM2
        ARG=MIN(ARG, 1.0)
        ARG=MAX(ARG,-1.0)
        A_LAMBDA=RECIP_PI_OVER_180*ACOS(ARG)
        A_LAMBDA=SIGN(A_LAMBDA,E_LAMBDA)
        A_LAMBDA=A_LAMBDA+LAMBDA_ZERO
      END IF

! Scale longitude to range 0 to 360 degs

      IF(A_LAMBDA.GE.360.0) A_LAMBDA=A_LAMBDA-360.0
      IF(A_LAMBDA.LT.0.0) A_LAMBDA=A_LAMBDA+360.0
      LAMBDA(I)=A_LAMBDA

200   CONTINUE

      RETURN
      END SUBROUTINE EQTOLL

!------------------------------------------------------------------------------

subroutine da_distance( dx, dy, lat, lon, dim1, dim2 )

!----------------------------------------------------------------------
! Purpose: Calculate grid point distance in x and y directions using the 
!          latitude and longitude at every grid points and the Haversine
!          formula assuming a spherical earth.  
!
! Note   : dx(i,j) = distance between (i,j) and (i+1,j)
!          dy(i,j) = distance between (i,j) and (i,j+1)
!
! History:
! Date     Author & Comment
! -------- ----------------
! 23/11/10 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!----------------------------------------------------------------------

   implicit none

   real, intent(out)  :: dx(1:dim1,1:dim2)            ! x-distance (meter)
   real, intent(out)  :: dy(1:dim1,1:dim2)            ! y-distance (meter)
   real, intent(in)   :: lat(1:dim1,1:dim2)           ! latitude
   real, intent(in)   :: lon(1:dim1,1:dim2)           ! longitude

   integer, intent(in):: dim1, dim2                   ! Dimensions.

   integer            :: i, j                         ! Loop counters.
   integer            :: its, ite, jts, jte           ! UM dims (dummies for now).

   real               :: dlat, dlon, lat1, lat2, a, c, R, npi

   npi=3.14159265358979323846
   R = 6.371229E6                   ! UM radius of the earth

   its = 1
   ite = dim1
   jts = 1
   jte = dim2

   do j = jts, jte
      do i = its, ite

         ! x distance
         if (i.ne.ite) then
            dlon = abs(lon(i+1,j) - lon(i,j))
            dlat = abs(lat(i+1,j) - lat(i,j))
            lat1 = lat(i,j)
            lat2 = lat(i+1,j)
         else
            dlon = abs(lon(i,j) - lon(i-1,j))
            dlat = abs(lat(i,j) - lat(i-1,j))
            lat1 = lat(i,j)
            lat2 = lat(i-1,j)
         endif

         a = (sin(dlat/2.0)*npi/180.0)**2 + &
         cos(lat1*npi/180.0)* cos(lat2*npi/180.0)* &
         (sin(dlon/2.0*npi/180.0))**2
         c = 2.0 * atan2(sqrt(a),sqrt(1-a))
         dx(i,j)= R * c

         ! y distance
         if (j.ne.jte) then
            dlat = abs(lat(i,j+1) - lat(i,j))
            dlon = abs(lon(i,j+1) - lon(i,j))
            lat1 = lat(i,j)
            lat2 = lat(i,j+1)
         else
            dlat = abs(lat(i,j) - lat(i,j-1))
            dlon = abs(lon(i,j) - lon(i,j-1))
            lat1 = lat(i,j)
            lat2 = lat(i,j-1)
         endif
         
         a = (sin(dlat/2.0*npi/180.0))**2 + &
         cos(lat1*npi/180.0)*               &
         cos(lat2*npi/180.0)*               &
         (sin(dlon/2.0*npi/180.0))**2
         c = 2.0 * atan2(sqrt(a),sqrt(1-a))
         dy(i,j)= R * c

      end do
   end do

end subroutine da_distance

!------------------------------------------------------------------------------

subroutine da_deg2m( dx_m, dx_deg, lat)

!---------------------------------------------------------------------- 
! Purpose: Calculate grid point distance in m from a distance in degree 
!          using the the Haversine formula assuming a spherical earth.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 01/04/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!---------------------------------------------------------------------

   implicit none

   real, intent(out)  :: dx_m       ! x-distance (meter)
   real, intent(in)   :: dx_deg     ! y-distance (meter)
   real, intent(in)   :: lat        ! latitude

   real               :: dlat, dlon, lat1, lat2, a, c, R, npi

   npi=3.14159265358979323846
   R = 6.371229E6                   ! UM radius of the earth

   dlon = dx_deg
   dlat = 0.0
   lat1 = 0.0
   lat2 = 0.0

   a = (sin(dlat/2.0)*npi/180.0)**2 + &
       cos(lat1*npi/180.0)* cos(lat2*npi/180.0)* &
       (sin(dlon/2.0*npi/180.0))**2
   c = 2.0 * atan2(sqrt(a),sqrt(1-a))
   dx_m= R * c

   print*," um_dx_m      = ", dx_m
   print*

end subroutine da_deg2m
subroutine da_uv_to_div_umc( dim1, dim2, dx_u, dy_v, u, v, div )
   
!----------------------------------------------------------------------
! Purpose: Calculate divergence on a co-ordinate surface, given an input
!          wind field on an Arakawa C-grid.
!  
! Note   : Zero gradient divergence boundary conditions for
!          Western, Southern and Northern boudaries
!
! History:
! Date     Author & Comment
! -------- ----------------
! 23/11/10 J.-F. Caron (Met Office)
!          Adaptation of Dale Barker's WRF version
! -------- End History
!
!                   du      dv
!           Vor = [----  + ---- ]
!                  dx_u    dy_v
!
!----------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                ! Dimensions.
   real, intent(in)   :: dx_u(1:dim1,1:dim2)       ! Map factor - u points.
   real, intent(in)   :: dy_v(1:dim1,1:dim2-1)     ! Map factor - u points.
   real, intent(in)   :: u(1:dim1,1:dim2)          ! v wind.
   real, intent(in)   :: v(1:dim1,1:dim2-1)        ! v wind.

   real, intent(out)  :: div(1:dim1,1:dim2)           ! Divergence.

   integer            :: i, j                         ! Loop counters.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------
   div(:,:) = 0.0

!------------------------------------------------------------------------------
!  [2] Calculate divergence field:
!------------------------------------------------------------------------------
   do j = 2, dim2-1
      do i = 2, dim1
         div(i,j) = ( (u(i,j) - u(i-1,j)) / dx_u(i-1,j) ) +  &
                    ( (v(i,j) - v(i,j-1)) / dy_v(i,j-1) )

       end do
   end do

!  Boundary values (zero gradient):
   div(1,2:dim2-1)    = div(2,2:dim2-1)    ! West
   div(1:dim1,1)      = div(1:dim1,2)      ! South
   div(1:dim1,dim2)   = div(1:dim1,dim2-1) ! North

end subroutine da_uv_to_div_umc

!------------------------------------------------------------------------------

subroutine da_uv_to_vor_umc( dim1, dim2, dy_u, dx_v, u, v, vor )

!----------------------------------------------------------------------
! Purpose: Calculate vorticity on a co-ordinate surface, given an input
!          wind field on an Arakawa C-grid.
!
! Note   : Zero gradient vorticity boundary conditions.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 15/11/10 J.-F. Caron (Met Office)
!          Adaptation of Dale Barker's WRF version
! -------- End History
!
!                   dv      du
!           Vor = [----  - ---- ]
!                  dx_v    dy_u
!
!----------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                ! Dimensions.
   real, intent(in)   :: dy_u(1:dim1,1:dim2)       ! Map factor - u points.
   real, intent(in)   :: dx_v(1:dim1,1:dim2-1)     ! Map factor - u points.
   real, intent(in)   :: u(1:dim1,1:dim2)          ! v wind.
   real, intent(in)   :: v(1:dim1,1:dim2-1)        ! v wind.

   real, intent(out)  :: vor(1:dim1+1,1:dim2+1)    ! Vorticity.

   integer            :: i, j                      ! Loop counters.

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------
   vor(:,:) = 0.0

!------------------------------------------------------------------------------
!  [2] Calculate vorticity field:
!------------------------------------------------------------------------------
   do j = 2, dim2
      do i = 2, dim1
         vor(i,j) = ( (v(i,j-1) - v(i-1,j-1)) / dx_v(i-1,j-1) ) -  &
                    ( (u(i-1,j) - u(i-1,j-1)) / dy_u(i-1,j-1) )
          
      end do
   end do

!  Boundary values (zero gradient):
   vor(1,2:dim2)        = vor(2,2:dim2)      ! West
   vor(dim1+1,2:dim2)   = vor(dim1,2:dim2)   ! East
   vor(1:dim1+1,1)      = vor(1:dim1+1,2)    ! South
   vor(1:dim1+1,dim2+1) = vor(1:dim1+1,dim2) ! North

end subroutine da_uv_to_vor_umc

!------------------------------------------------------------------------------

subroutine da_psichi_to_uv_umc( dim1, dim2, dx_m, dy_m, dx_p, dy_p, &
                                psi, chi, u, v )

!----------------------------------------------------------------------
! Purpose: Calculate u and v wind components on an Arakawa C-grid.
!  
! History:
! Date     Author & Comment
! -------- ----------------
! 08/09/05 Dale Barker
!          Creation of F90 version.
! 15/11/10 J.-F. Caron (Met Office)
!          Adaptation to UM
! -------- End History
!
!----------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2                   ! Dimensions.

   real, intent(in)   :: dx_m(1:dim1,1:dim2)          ! x distance - mass points
   real, intent(in)   :: dy_m(1:dim1,1:dim2)          ! y distance - mass points
   real, intent(in)   :: dx_p(1:dim1+1,1:dim2+1)      ! x distance - vort/psi pts
   real, intent(in)   :: dy_p(1:dim1+1,1:dim2+1)      ! y distance - vort/psi pts
   real, intent(in)   :: psi(1:dim1+1,1:dim2+1)       ! Streamfunction. 
   real, intent(in)   :: chi(1:dim1,1:dim2)           ! Velcoity potential.

   real, intent(out)  :: u(1:dim1,1:dim2)             ! v wind.
   real, intent(out)  :: v(1:dim1,1:dim2-1)           ! v wind.

   integer            :: i, j                         ! Loop counters.
   integer            :: its, ite, jts, jte           ! WRF dims (dummies for now).

!  u-wind component:
   its = 1
   ite = dim1-1
   jts = 1
   jte = dim2

   do j = jts, jte
      do i = its, ite
         u(i,j) = -1.0*( psi(i+1,j+1) - psi(i+1,j) ) / dy_p(i+1,j) + &
                  ( chi(i+1,j) - chi(i,j) ) / dx_m(i,j)
      end do
   end do

!  Remaining points on E boundaries (extrapolation):
   u(dim1,jts:jte) = 2.0 * u(dim1-1,jts:jte) - u(dim1-2,jts:jte)

!  v-wind component:
   its = 1
   ite = dim1
   jts = 1
   jte = dim2-1

   do j = jts, jte
      do i = its, ite
         v(i,j) =  ( psi(i+1,j+1) - psi(i,j+1) ) / dx_p(i,j+1) &
                 + ( chi(i,j+1) - chi(i,j) ) / dy_m(i,j)
      end do
   end do

end subroutine da_psichi_to_uv_umc
subroutine da_get_rh_um( input_file, dim1, dim2, dim3, k, pheight, theight, rh )

!---------------------------------------------------------------------
! Purpose: Calculates RH on THETA levels.
!
! History:
! Date     Author & Comment
! -------- ----------------
! 23/11/10 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!---------------------------------------------------------------------

   implicit none

   character(len=200), intent(in)  :: input_file         ! 1 file name.

   integer,            intent(in)  :: dim1, dim2, dim3   ! Dimensions.
   integer,            intent(in)  :: k                  ! Model level.

   real,               intent(in) :: pheight(1:dim3+1)   ! pressure height
   real,               intent(in) :: theight(1:dim3)     ! temperature height

   real,               intent(out) :: rh(1:dim1,1:dim2)  ! Relative humidity.

   character(len=12) :: var                       ! Variable to search for. var = "T"
   integer           :: i, j                      ! Loop counters.

   real              :: theta(1:dim1,1:dim2)      ! Potential temperature.
   real              :: p_below(1:dim1,1:dim2)    ! pressure below theta level
   real              :: p_above(1:dim1,1:dim2)    ! pressure above theta level
   real              :: q(1:dim1,1:dim2)          ! Specific humidity.
   real              :: temp(1:dim1,1:dim2)       ! Temperature.

   real              :: p                         ! Pressure.
   real              :: t_c                       ! Temp(Celsius).
   real              :: es                        ! Saturation vapor pressure.
   real              :: qs                        ! Saturation specific humidity.

   var = "theta" ! Potential temperature
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, theta)

   var = "p" ! pressure on rho levels.
   call da_get_field( input_file, var, 3, dim1, dim2, dim3+1, k+1, p_above)
   call da_get_field( input_file, var, 3, dim1, dim2, dim3+1, k, p_below)

   var = "q"  ! specific humidity
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, q)

   do j = 1, dim2
      do i = 1, dim1

         ! Interpolate pressure at rho level to theta level
         p = p_below(i,j) + ( (p_above(i,j) - p_below(i,j)) * &
                              (theight(k)-pheight(k)) /      &
                              (pheight(k+1)-pheight(k)) )

         temp(i,j) = theta(i,j) *( p/1.0E5)**kappa ! Theta to T.

         ! Calculate relative humidity:
         t_c = temp(i,j) - t_kelvin
         es = es_alpha * exp( es_beta * t_c /( t_c + es_gamma))
         qs = rd_over_rv * es /( p - rd_over_rv1 * es)
         rh(i,j) = q(i,j) / qs

      end do
   end do

end subroutine da_get_rh_um

!---------------------------------------------------------------------------------------------

subroutine da_get_trhol_um( input_file, dim1, dim2, dim3, k, pheight, theight, temp)

!---------------------------------------------------------------------
! Purpose: Calculates T on rho level
!
! History:
! Date     Author & Comment
! -------- ----------------
! 23/11/10 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!---------------------------------------------------------------------

   implicit none

   character(len=200), intent(in)  :: input_file           ! 1 file name.

   integer,            intent(in)  :: dim1, dim2, dim3     ! Dimensions.
   integer,            intent(in)  :: k                    ! Model level.

   real,               intent(in) :: pheight(1:dim3+1)     ! rho height
   real,               intent(in) :: theight(1:dim3)       ! theta height

   real,               intent(out) :: temp(1:dim1,1:dim2)  ! Temperature.

   character(len=12) :: var                       ! Variable to search for. var = "T"
   integer           :: i, j                      ! Loop counters.

   real              :: rho(1:dim1,1:dim2)        ! density.
   real              :: q_below(1:dim1,1:dim2)    ! q below theta level
   real              :: q_above(1:dim1,1:dim2)    ! q above theta level
   real              :: p(1:dim1,1:dim2)          ! pressure

   real              :: q                         ! specific humidity at rho levels
   real              :: R                         ! radius of the earth

   R = 6.371229E6       ! UM value

   var = "p" ! pressure on rho levels
   call da_get_field( input_file, var, 3, dim1, dim2, dim3+1, k, p)

   var = "unspecified" ! rho * R * R
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, rho)

   var = "q" ! ! specific humidity
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, q_above)
   if (k .ne. 1) then
      call da_get_field( input_file, var, 3, dim1, dim2, dim3, k-1, q_below)
   endif

   do j = 1, dim2
      do i = 1, dim1

         rho(i,j)=rho(i,j)/(R**2)   ! Convert rho*R*R to rho

         ! Interpolate specific humidity at theta level to rho level
         if (k .ne. 1) then
            q = q_below(i,j) + ( (q_above(i,j) - q_below(i,j)) * &
                               (pheight(k)-theight(k-1)) /      &
                               (theight(k)-theight(k-1)) )
         else
            q = q_above(i,j)
         endif

         temp(i,j) = p(i,j) / ( rho(i,j) * gas_constant * (1.0+0.608*q) )

      end do
   end do

end subroutine da_get_trhol_um

!---------------------------------------------------------------------------------------------

subroutine da_hydro_p_pert_um( hp_p, pres_p, pres, theta_p, theta, q_p, q, &
                               pheight, dim1, dim2, dim3)

!---------------------------------------------------------------------
! Purpose: Calculates hydrostatic pressure perturbations
!
! Note   : theta is assumed to be on theta levels and pressure on rho levels
!
! History:
! Date     Author & Comment
! -------- ----------------
! 23/11/10 J.-F. Caron (Met Office)
!          Initial version
! 25/05/11 J.-F. Caron (Met Office)
!          Use 1D working arrays instead of 3D arrays to save memory space
! -------- End History
!
!---------------------------------------------------------------------

   implicit none

   integer,            intent(in) :: dim1, dim2, dim3              ! Dimensions.

   real,               intent(in) :: pheight(1:dim3+1)             ! rho height

   real,               intent(in) :: pres_p(1:dim1,1:dim2,1:dim3)  ! Pressure perts.
   real,               intent(in) :: pres(1:dim1,1:dim2,1:dim3)    ! Bkgd presure.
   real,               intent(in) :: theta_p(1:dim1,1:dim2,1:dim3) ! Theta perts.
   real,               intent(in) :: theta(1:dim1,1:dim2,1:dim3)   ! Bkgd theta.
   real,               intent(in) :: q_p(1:dim1,1:dim2,1:dim3)     ! q perts.
   real,               intent(in) :: q(1:dim1,1:dim2,1:dim3)       ! Bkgd q.

   real,               intent(out) :: hp_p(1:dim1,1:dim2,1:dim3)   ! Hydrostatic pres perts.

   integer           :: i, j, k                           ! Loop counters.

   real              :: thetav_p(1:dim3)    ! Virtual pot temp perts.
   real              :: thetav(1:dim3)      ! Bkgd virtual pot temp.
   real              :: exnh_p(1:dim3)      ! Hydro Exner function perts.
   real              :: exnh(1:dim3)        ! Bkgd hydro exner function.
   real              :: hp(1:dim3)          ! Bkgd hydrostatic pressure.
   
   real              :: pref,d, cv

   pref=1000.0*100.0  ! reference pressure in Pa
   cv=0.608

   do j = 1, dim2
      do i = 1, dim1

         ! Compute background virtual temperature
         do k = 1, dim3
            thetav(k) = theta(i,j,k) * ( 1.0 + cv * q(i,j,k) )
         end do
         
         ! Compute background hydrostatic pressure
         do k = 1, dim3
            if (k.eq.1) then
               exnh(k) =  ( pres(i,j,k) / pref)**kappa
            else
               d = pheight(k) - pheight(k-1)
               exnh(k) =  exnh(k-1) - ( gravity * d ) / ( cp * thetav(k-1) )
            endif
            hp(k) = pref * ( exnh(k)**(1.0/kappa) )
         end do

         ! Compute linearized virtual temperature perturbations
         do k = 1, dim3
            thetav_p(k) = theta_p(i,j,k) * ( 1.0 + cv * q(i,j,k) ) + &
                              theta(i,j,k) * cv * q_p(i,j,k)
         end do

         ! Compute linearized hydrostatic pressure perturbations
         do k = 1, dim3
            if (k.eq.1) then
               exnh_p(k) =  kappa * exnh(k) * ( pres_p(i,j,k) / pres(i,j,k) )
            else
               d = pheight(k) - pheight(k-1)
               exnh_p(k) =  exnh_p(k-1) + ( gravity * d * thetav_p(k-1)) &
                                                / ( cp * thetav(k-1)**2 )
            endif
            hp_p(i,j,k) = ( exnh_p(k) * hp(k) ) /  &
                          ( kappa * exnh(k) )
         end do

      end do
   end do

end subroutine da_hydro_p_pert_um

!------------------------------------------------------------------------------

subroutine da_linbal_um( lbpres_p, psi_p, rho, &
                               lat, dim1, dim2, dim3)

!---------------------------------------------------------------------- 
! Purpose: Calculates linearized balance pressure perturbations
!
! Note   : Linear balance : lbpres_p = rho * f * psi_p
!          All data assumed to be on the same levels
!
! History:
! Date     Author & Comment
! -------- ----------------
! 01/12/10 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!---------------------------------------------------------------------

   implicit none

   integer,            intent(in) :: dim1, dim2, dim3             ! Dimensions.

   real,               intent(in) :: psi_p(1:dim1,1:dim2,1:dim3)  ! Psi perts.
   real,               intent(in) :: rho(1:dim1,1:dim2,1:dim3)    ! Bkgd density..
   real,               intent(in) :: lat(1:dim1,1:dim2)           ! Latitude.

   real,               intent(out) :: lbpres_p(1:dim1,1:dim2,1:dim3) ! balance pres perts.

   integer           :: i, j, k                           ! Loop counters.

   real              :: coriol(1:dim1,1:dim2)    ! Coriolis.

   ! Compute coriolis parameter
   do j = 1, dim2
      do i = 1, dim1
         coriol(i,j) = 2.0 * earth_omega * sin( lat(i,j)*radian )
      end do
   end do

   ! Compute simplified linear balance
   do k = 1, dim3
      do j = 1, dim2
         do i = 1, dim1
            lbpres_p(i,j,k) = rho(i,j,k) * coriol(i,j) * psi_p(i,j,k) 
         end do
      end do
   end do

end subroutine da_linbal_um

!----------------------------------------------------------------------------------------

subroutine da_get_um_height( pheight, theight, fileplev, filetlev, dim3, &
                             iunit)

!---------------------------------------------------------------------- 
! Purpose: Read height of UM levels and check ordering
!
! History:
! Date     Author & Comment
! -------- ----------------
! 15/03/11 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!---------------------------------------------------------------------
   
   implicit none
   
   character(len=filename_len), intent(in)  :: fileplev   ! file name.
   character(len=filename_len), intent(in)  :: filetlev   ! file name.
   integer,            intent(in)  :: iunit               ! Input unit.
   integer,            intent(in)  :: dim3                ! Dimensions.
   real,               intent(out) :: pheight(1:dim3+1)   ! Height.
   real,               intent(out) :: theight(1:dim3)     ! Height.
 

   character (len=filename_len)   :: input_file           ! Input file.
   integer           :: k                                 ! Loop counter.

   input_file = trim(fileplev)
   open (iunit, file = input_file)
   do k = 1, dim3+1
      read(iunit,*)pheight(k)
      print*,k,pheight(k)
   end do
   close(iunit)

   input_file = trim(filetlev)
   open (iunit, file = input_file)
   do k = 1, dim3
      read(iunit,*)theight(k)
      print*,k,theight(k)
   end do
   close(iunit)

   ! test #1
   do k = 1, dim3
      if (pheight(k).ge.pheight(k+1)) then
         print*
         print*,"Error in pressure heights, h(k) >= h(k+1)", k, pheight(k), pheight(k+1)
         print*,"Aborting !!!!"
         stop
      endif
   end do

   ! test #2
   do k = 1, dim3-1
      if (theight(k).ge.theight(k+1)) then
         print*
         print*,"Error in temperature heights, h(k) >= h(k+1)", k, theight(k), theight(k+1)
         print*,"Aborting !!!!"
         stop
      endif
   end do

   ! test #3   
   do k = 1, dim3
      if ( (theight(k).le.pheight(k)) .or. (theight(k).ge.pheight(k+1)) ) then
         print*
         print*,"T height is not between P heights", k, pheight(k), theight(k), pheight(k+1)
         print*,"Aborting !!!!"
         stop
      endif
   end do

end subroutine da_get_um_height

!---------------------------------------------------------------------------------------------

subroutine da_pres_rho2theta_1d( pres_theta, pres_rho, pheight, theight, dim3 )

!---------------------------------------------------------------------- 
! Purpose: Interpolate pressure from RHO levels to THETA levels
!
! History:
! Date     Author & Comment
! -------- ----------------
! 01/12/10 J.-F. Caron (Met Office)
!          Initial version
! -------- End History
!
!---------------------------------------------------------------------

   implicit none

   integer,            intent(in)  :: dim3                ! Dimensions.

   real,               intent(in) :: pheight(1:dim3+1)    ! pressure height
   real,               intent(in) :: theight(1:dim3)      ! temperature height

   real,               intent(in) :: pres_rho(1:dim3+1)   ! field on rho levels
   real,               intent(out) :: pres_theta(0:dim3)  ! field on theta level.

   integer           :: k                      ! Loop counters.

   do k = 1, dim3
      pres_theta(k) = pres_rho(k) + ( (pres_rho(k+1) - pres_rho(k) ) * &
                              (theight(k)-pheight(k)) /                  &
                              (pheight(k+1)-pheight(k)) )
   end do

   ! extrapolation  below theta surface assuming :
   !                         theight(0) = pheight(1) - (theight(1)-pheight(1)) 
   pres_theta(0) = ( (pres_theta(1)-pres_rho(1)) *           &
                    2.0 * (pheight(1)-theight(1)) /                   &
                    (theight(1)-pheight(1)) ) + pres_theta(1)

end subroutine da_pres_rho2theta_1d

!----------------------------------------------------------------------------------------


end module da_um_routines
