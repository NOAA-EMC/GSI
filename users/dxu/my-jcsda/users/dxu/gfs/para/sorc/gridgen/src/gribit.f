 module gribit

 use program_setup, only           : domain_name, &
                                     domain_type, &
                                     imdl,        &
                                     jmdl

 use calc_latlons, only            : lat_mdl,  &
                                     lon_mdl


 integer, public                  :: kpds_mdl(200)
 integer, public                  :: kgds_mdl(200)

 contains

!-----------------------------------------------------------------------
! initialize grib pds and gds sections for the model grid.
!-----------------------------------------------------------------------

 subroutine init_pds_gds

 implicit none

!-----------------------------------------------------------------------
! product definition section.
!-----------------------------------------------------------------------

 kpds_mdl = 0

 kpds_mdl(1) = 7    ! oct 5 - id of center
 kpds_mdl(2) = 96   ! oct 6 - generating process id
 select case (trim(domain_name)) ! oct 7 - grid definition 
   case("t382", "T382")
     kpds_mdl(3) = 255     ! ask nco what new number is 
   case("t254", "T254")
     kpds_mdl(3) = 127 
   case("t126", "T126")
     kpds_mdl(3) = 126
   case("t62", "T62")
     kpds_mdl(3) = 98
   case ("t170", "T170")
     kpds_mdl(3) = 170
   case ("t190", "T190")
     kpds_mdl(3) = 255
   case("centnmm")
     kpds_mdl(3) = 91
   case("eta12km")
     kpds_mdl(3) = 96
   case("eta32km")
     kpds_mdl(3) = 192
   case("eastnmm")
     kpds_mdl(3) = 90
   case("westnmm")
     kpds_mdl(3) = 92
   case("aknmm")
     kpds_mdl(3) = 93
   case("ak_dgex")
     kpds_mdl(3) = 186
   case("conus_dgex")
     kpds_mdl(3) = 185
   case("prnmm")
     kpds_mdl(3) = 97
   case("hinmm")
     kpds_mdl(3) = 194
   case default
     kpds_mdl(3) = 255
 end select
 kpds_mdl(4) = 192 ! oct 8 - gds/bms flag
 kpds_mdl(5) = 1   ! oct 9 - parameter indicator
 kpds_mdl(6) = 1   ! oct 10 - type of level
 kpds_mdl(7) = 0   ! oct 11-12 - height/pressure of level
 kpds_mdl(8) = 100   ! oct 13 - year
 kpds_mdl(9) = 1   ! oct 14 - month
 kpds_mdl(10) = 1  ! oct 15 - day
 kpds_mdl(11) = 0  ! oct 16 - hour
 kpds_mdl(12) = 0  ! oct 17 - minute
 kpds_mdl(13) = 1  ! oct 18 - fcst time unit
 kpds_mdl(14) = 0  ! oct 19 - time range 1
 kpds_mdl(15) = 1  ! oct 20 - time range 2
 kpds_mdl(16) = 1  ! oct 21 - time range flag
 kpds_mdl(17) = 0  ! oct 22-23 - number included in average
 kpds_mdl(18) = 1  ! version nr of grib specification
 kpds_mdl(19) = 130 ! version nr of parameter table for land sfc modeling
 kpds_mdl(20) = 0  ! oct 24 - nr missing from average/accumulation
 kpds_mdl(21) = 19 ! oct 25 - century
 kpds_mdl(22) = 0  ! oct 27-28 - decimal scale factor
 kpds_mdl(23) = 4  ! oct 26 - subcenter                  
 kpds_mdl(24) = 0  ! oct 29 - reserved
 kpds_mdl(25) = 0  ! oct 30 - not used

 if (trim(domain_type) == 'gaussian') then

   call init_gds_gaussian

 else if (trim(domain_type) == 'egrid') then

   call init_gds_egrid

 else if (trim(domain_type) == 'latlon') then

   call init_gds_latlon

 else if (trim(domain_type) == 'lambconf') then

   call init_gds_lambconf

 end if

 end subroutine init_pds_gds

!-----------------------------------------------------------------------
! grid definition section for gaussian grid.
!-----------------------------------------------------------------------

 subroutine init_gds_gaussian

 implicit none

 kgds_mdl = 0

 kgds_mdl(1) = 4     ! oct 6 - type of grid (gaussian)
 kgds_mdl(2) = imdl  ! oct 7-8 - # pts on latitude circle
 kgds_mdl(3) = jmdl  ! oct 9-10 - # pts on longitude circle
 kgds_mdl(4) = nint(lat_mdl(1,1)*1000.0)  ! oct 11-13 - lat of origin
 kgds_mdl(5)  = 0    ! oct 14-16 - lon of origin
 kgds_mdl(6)  = 128  ! oct 17 - resolution flag
 kgds_mdl(7)  = nint(lat_mdl(imdl,jmdl)*1000.) ! oct 18-20 - lat of extreme point 
 kgds_mdl(8)  = nint(lon_mdl(imdl,jmdl)*1000.) ! oct 21-23 - lon of extreme point
 kgds_mdl(9)  = nint((360.0 / float(imdl))*1000.0)  ! oct 24-25 - longitudinal increment
 kgds_mdl(10) = jmdl / 2    ! oct 26-27 - number of circles pole to equator
 kgds_mdl(11) = 0    ! oct 28 - scanning mode flag
 kgds_mdl(12) = 255  ! oct 29 - reserved
 kgds_mdl(19) = 0    ! oct 4  - # vert coordinate parameters
 kgds_mdl(20) = 255  ! oct 5  - not used, set to 255

 return

 end subroutine init_gds_gaussian

!-----------------------------------------------------------------------
! grid definition section for regular lat/lon grid.
!-----------------------------------------------------------------------

 subroutine init_gds_latlon

 use program_setup, only       : dx_mdl,   &
                                 dy_mdl

 implicit none

 kgds_mdl = 0

 kgds_mdl(1)  = 0     ! oct 6 - type of grid (lat/lon)
 kgds_mdl(2)  = imdl  ! oct 7-8 - # pts on latitude circle
 kgds_mdl(3)  = jmdl  ! oct 9-10 - # pts on longitude circle
 kgds_mdl(4)  = nint(lat_mdl(1,1)*1000.0)  ! oct 11-13 - lat of origin
 kgds_mdl(5)  = nint(lon_mdl(1,1)*1000.0)  ! oct 14-16 - lon of origin
 kgds_mdl(6)  = 128  ! oct 17 - resolution flag
 kgds_mdl(7)  = nint(lat_mdl(imdl,jmdl)*1000.) ! oct 18-20 - lat of last point 
 kgds_mdl(8)  = nint(lon_mdl(imdl,jmdl)*1000.) ! oct 21-23 - lon of last point
 kgds_mdl(9)  = nint(dx_mdl*1000.0)  ! oct 24-25 - longitudinal increment
 kgds_mdl(10) = nint(dy_mdl*1000.0)
 kgds_mdl(11) = 0    ! oct 28 - scanning mode flag
 kgds_mdl(12) = 255  ! oct 29 - reserved
 kgds_mdl(19) = 0    ! oct 4  - # vert coordinate parameters
 kgds_mdl(20) = 255  ! oct 5  - not used, set to 255

 return

 end subroutine init_gds_latlon

!-----------------------------------------------------------------------
! set up gds for arakawa e-grid
!-----------------------------------------------------------------------

 subroutine init_gds_egrid

 use program_setup, only      : dx_mdl,      &
                                dy_mdl,      &
                                centlat_mdl, &
                                centlon_mdl

 implicit none

 real                         :: lat11, lon11  ! corner point of grid

 kgds_mdl = 0

 kgds_mdl( 1) = 203        ! oct 6 - type of grid, arakawa staggered e-grid
 kgds_mdl( 2) = imdl       ! octs 7-8 - i dimension of grid
 kgds_mdl( 3) = jmdl       ! octs 9-10 - j dimension of grid                   
 kgds_mdl( 4) = nint(lat_mdl(1,1)*1000.) ! latitude of first grid point, octs 11-13
 kgds_mdl( 5) = nint(lon_mdl(1,1)*1000.) ! longitude of first grid point, octs 14-16
 kgds_mdl( 6) = 136        ! oct 17 - resolution and component flag
 kgds_mdl( 7) = nint(centlat_mdl*1000.)  
                           ! octs 18-20 - # mass points along southmost row
 kgds_mdl( 8) = nint(centlon_mdl*1000.)  
                           ! octs 21-23 - # rows in each column
 kgds_mdl( 9) = nint(dx_mdl*1000.)  ! octs 24-25 - long direction increment
 kgds_mdl(10) = nint(dy_mdl*1000.)  ! octs 26-27 - lat direction increment
 kgds_mdl(11) = 64         ! oct 28 - scanning mode flag
 kgds_mdl(19) = 0          ! oct 4  - # vert coordinate parameters
 kgds_mdl(20) = 255        ! oct 5  - not used, set to 255
   
 return

 end subroutine init_gds_egrid

!-----------------------------------------------------------------------
! set up gds for a lambert conformal grid
!-----------------------------------------------------------------------

 subroutine init_gds_lambconf

 use program_setup, only      : dx_mdl,            &
                                dy_mdl,            &
                                tangent_lat_mdl,   &
                                orient_lon_mdl

 implicit none

 kgds_mdl = 0

 kgds_mdl( 1) = 3          ! oct 6 - type of grid, lambert conf.
 kgds_mdl( 2) = imdl       ! octs 7-8 - i dimension of grid
 kgds_mdl( 3) = jmdl       ! octs 9-10 - j dimension of grid                   
 kgds_mdl( 4) = nint(lat_mdl(1,1)*1000.) ! latitude of first grid point, octs 11-13
 kgds_mdl( 5) = nint(lon_mdl(1,1)*1000.) ! longitude of first grid point, octs 14-16
 kgds_mdl( 6) = 136        ! oct 17 - resolution and component flag
 kgds_mdl( 7) = nint(orient_lon_mdl*1000.)  
                           ! octs 18-20 - # mass points along southmost row
 kgds_mdl( 8) = nint(dx_mdl*111.*1000.)  ! octs 21-23 - x-dir grid length meters
 kgds_mdl( 9) = nint(dy_mdl*111.*1000.)  ! octs 24-26 - y-dir grid length meters
 kgds_mdl(10) = 0          ! oct 27 - projection center flag
 kgds_mdl(11) = 64         ! oct 28 - scanning mode flag
 kgds_mdl(12) = nint(tangent_lat_mdl*1000.) ! oct 29-31 - first lat from pole
                                            ! at which cone cuts earth.
 kgds_mdl(13) = nint(tangent_lat_mdl*1000.) ! oct 32-34 - second lat from pole
                                            ! at which cone cuts earth.
 kgds_mdl(19) = 0          ! oct 4  - # vert coordinate parameters
 kgds_mdl(20) = 255        ! oct 5  - not used, set to 255
   
 return

 end subroutine init_gds_lambconf

 end module gribit
