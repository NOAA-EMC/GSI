 module program_setup

 use consts, only       : lonsperlat_t382,   &
                          lonsperlat_t254,   &
                          lonsperlat_t190,   &
                          lonsperlat_t170,   &
                          lonsperlat_t126,   &
                          lonsperlat_t62
 
 implicit none

 character*20          :: domain_name
 character*20          :: domain_type
 character*150         :: glacier_file
 character*150         :: gfrac_file
 character*150         :: leaf_area_idx_file
 character*150         :: lsmask_file     ! used to override lsmask calculation
 character*150         :: mxsnow_alb_file
 character*150         :: orog_file       ! used to override orog calculation
 character*150         :: roughness_file
 character*150         :: seaice_file
 character*150         :: slopetype_file
 character*150         :: snow_climo_file
 character*150         :: snowfree_albedo_file
 character*150         :: substrate_temp_file
 character*150         :: soilm_file
 character*150         :: soiltype_notile_file
 character*150         :: soiltype_tile_file
 character*150         :: sst_climo_file
 character*150         :: vegtype_notile_file
 character*150         :: vegtype_tile_file

 integer               :: default_soil_category
 integer               :: default_veg_category
 integer               :: imdl
 integer               :: jmdl
 integer, allocatable  :: lonsperlat_mdl (:)
 integer               :: max_orog_tiles
 integer               :: num_soil_groups
 integer               :: num_veg_groups
 integer               :: max_soil_tiles
 integer               :: max_total_land_tiles
 integer               :: max_veg_tiles
 integer, allocatable  :: remaining_tot_tiles(:,:)
 integer, parameter    :: max_num_categories = 50
 integer               :: num_smooth_passes, smooth
 integer               :: soil_groups(max_num_categories)
 integer               :: veg_groups(max_num_categories)

 logical               :: lsmask_tiles
 logical               :: thinned        ! for global grid only, to run
                                         ! on a thinned grid

 real                  :: centlat_mdl
 real                  :: centlon_mdl
 real                  :: dx_mdl         ! x-dir resolution in degrees
 real                  :: dy_mdl         ! y-dir resolution in degrees
 real                  :: lat_11_mdl
 real                  :: lon_11_mdl
 real                  :: lsmask_tile_threshold
 real                  :: orient_lon_mdl 
 real                  :: orog_bin_width
 real                  :: orog_tile_threshold
 real                  :: resol_mdl      ! resolution in degrees
 real                  :: soil_tile_threshold
 real                  :: tangent_lat_mdl 
 real                  :: veg_tile_threshold

 contains

!-----------------------------------------------------------------------
! read configuration namelist and get some model grid info.
!-----------------------------------------------------------------------

 subroutine setup

 implicit none

 integer                   :: istat
 integer                   :: jcap

!-----------------------------------------------------------------------
! for the global model, set to true to run on the thinned grid
! (# grid points decreases toward the poles).  otherwise, all
! global model points will be processed. 
!-----------------------------------------------------------------------

 logical, parameter        :: run_global_thinned = .false.

 namelist /grid/ domain_name, imdl, jmdl

 namelist /tiling/ max_total_land_tiles

 namelist /veg_tiling/ max_veg_tiles, veg_tile_threshold, &
                       default_veg_category, num_veg_groups, &
                       veg_groups

 namelist /soil_tiling/ max_soil_tiles, soil_tile_threshold, &
                        default_soil_category, num_soil_groups, &
                        soil_groups 

 namelist /lsmask_orog_tiling/ max_orog_tiles,         &
                               orog_bin_width,         &
                               orog_tile_threshold,    &
                               lsmask_tiles,           &
                               lsmask_tile_threshold,  &
                               smooth,                 &
                               num_smooth_passes

 namelist /input_data/ leaf_area_idx_file,               &
                       glacier_file,                     &
                       gfrac_file, mxsnow_alb_file,      &
                       roughness_file,                   &
                       seaice_file, slopetype_file,      &
                       snow_climo_file,                  &
                       snowfree_albedo_file,             &
                       soilm_file,                       &
                       soiltype_tile_file,               &
                       soiltype_notile_file,             &
                       sst_climo_file,                   &
                       substrate_temp_file,              &
                       vegtype_tile_file,                &
                       vegtype_notile_file,              &
                       lsmask_file,                      &
                       orog_file

 print*,"- READ CONFIGURATION NAMELIST"

 open(81, iostat=istat, err=900)

 read(81, nml=grid,               iostat=istat, err=910)
 read(81, nml=tiling,             iostat=istat, err=910)
 read(81, nml=veg_tiling,         iostat=istat, err=910)
 read(81, nml=soil_tiling,        iostat=istat, err=910)
 read(81, nml=lsmask_orog_tiling, iostat=istat, err=910)
 read(81, nml=input_data,         iostat=istat, err=910)

 close(81)

!-----------------------------------------------------------------------
! for now, assume a domain that starts with T/t[0-9] is a gaussian
! grid.
!-----------------------------------------------------------------------

 thinned = .false.  ! flag to tell rest of program to handle a
                    ! global thinned grid.

 if ( (domain_name(1:1) == 't' .or. domain_name(1:1) == 'T') .and.  &
      (domain_name(2:2) >= '0' .and. domain_name(2:2) <= '9') ) then

   domain_type = 'gaussian'

   read(domain_name(2:5),fmt='(i4)') jcap

!-----------------------------------------------------------------------
!  approximate model resolution in degrees.
!-----------------------------------------------------------------------

   resol_mdl = 360.0 / imdl
   dx_mdl    = resol_mdl     
   dy_mdl    = resol_mdl   

   centlat_mdl = 0.0
   centlon_mdl = 180.0

   if (run_global_thinned) then

     print*,"- WILL RUN ON A THINNED GRID."

     thinned = .true.

     allocate (lonsperlat_mdl(jmdl/2))

     select case (trim(domain_name))
       case("t382", "T382")
         lonsperlat_mdl = lonsperlat_t382
       case("t254", "T254")
         lonsperlat_mdl = lonsperlat_t254
       case("t190", "T190")
         lonsperlat_mdl = lonsperlat_t190
       case("t170", "T170")
         lonsperlat_mdl = lonsperlat_t170
       case("t126", "T126")
         lonsperlat_mdl = lonsperlat_t126
       case("t62", "T62")
         lonsperlat_mdl = lonsperlat_t62
     end select

   end if

 elseif (trim(domain_name) == "regll") then

   domain_type = "latlon"

   imdl = 3600
   jmdl = 1800

   dx_mdl = 0.1
   dy_mdl = 0.1

   resol_mdl = 0.1

   centlat_mdl = 0.0
   centlon_mdl = 180.0

 elseif (trim(domain_name) == "ndfd") then  ! lambert conf grid

   imdl = 1073
   jmdl = 689

   centlat_mdl = -999.  ! not used
   centlon_mdl = -999.  ! not used

   dx_mdl = 5.08 / 111.0  ! degrees (approx.)
   dy_mdl = 5.08 / 111.0  ! degrees (approx.)

   resol_mdl = 5.08 / 111.0   ! degrees (approx.)

   lat_11_mdl = 20.192
   lon_11_mdl = 238.446

   orient_lon_mdl = 265.0

   tangent_lat_mdl = 25.0

   domain_type = "lambconf"
 
 else

!-----------------------------------------------------------------------
! an nmm arakawa e-grid.
!-----------------------------------------------------------------------

   select case(trim(domain_name))
     case("centnmm")
       imdl           = 360
       jmdl           = 809
       dx_mdl         = .033147632
       dy_mdl         = .03259901
       centlat_mdl    =  37.0
       centlon_mdl    = -98.0
     case("eta12km")
       imdl           = 606
       jmdl           = 1067
       dx_mdl         = 53.0/605.0
       dy_mdl         = 40.0/533.0
       centlat_mdl    =  50.0
       centlon_mdl    = -111.0
     case("nam12km")
       imdl           = 669
       jmdl           = 1165
       dx_mdl         = 60.0/668.0
       dy_mdl         = 45.0/582.0
       centlat_mdl    =  54.0
       centlon_mdl    = -106.0
     case("nmm16km")
       imdl           = 375
       jmdl           = 563
       dx_mdl         = 0.108
       dy_mdl         = 0.1025
       centlat_mdl    =  50.0
       centlon_mdl    = -111.0
     case("eta32km")
       imdl           = 237
       jmdl           = 387
       dx_mdl         = 53.0/236.0
       dy_mdl         = 40.0/193.0
       centlat_mdl    =  50.0
       centlon_mdl    = -111.0
     case("nmm12km")
       imdl           = 450
       jmdl           = 767
       dx_mdl         = 53.0/605.0
       dy_mdl         = 40.0/533.0
       centlat_mdl    =  50.0
       centlon_mdl    = -111.0
     case("launcher")
       imdl           = 500
       jmdl           = 799
       dx_mdl         = 53.0/605.0
       dy_mdl         = 40.0/533.0
       centlat_mdl    =  50.0
       centlon_mdl    = -111.0
     case("eastnmm")
       imdl           = 360
       jmdl           = 809
       dx_mdl         = .033147632
       dy_mdl         = .03259901
       centlat_mdl    =  37.0
       centlon_mdl    = -80.0
     case("westnmm")
       imdl           = 223
       jmdl           = 501
       dx_mdl         = 24.0/449.0
       dy_mdl         = 1.0/19.0
       centlat_mdl    =  40.0
       centlon_mdl    = -115.0
     case("aknmm")
       imdl           = 223
       jmdl           = 501
       dx_mdl         = 1.0/15.0
       dy_mdl         = 5.0/76.0
       centlat_mdl    =  63.0
       centlon_mdl    = -150.0
     case("ak_dgex")
       imdl           = 233
       jmdl           = 331
       dx_mdl         = 53.0/605.0
       dy_mdl         = 40.0/533.0
       centlat_mdl    =  61.0
       centlon_mdl    = -157.0
     case("conus_dgex")
       imdl           = 303
       jmdl           = 429
       dx_mdl         = 53.0/605.0
       dy_mdl         = 40.0/533.0
       centlat_mdl    =  40.0
       centlon_mdl    = -98.0
     case("prnmm")
       imdl           = 89
       jmdl           = 143
       dx_mdl         = 24.0/449.0
       dy_mdl         = 1.0/19.0
       centlat_mdl    =  18.0
       centlon_mdl    = -66.5
     case("hinmm")
       imdl           = 89
       jmdl           = 143
       dx_mdl         = 24.0/449.0
       dy_mdl         = 1.0/19.0
       centlat_mdl    =  20.25
       centlon_mdl    = -157.35
     case("nmmtest")
       imdl           = 89
       jmdl           = 143
       dx_mdl         = 24.0/449.0
       dy_mdl         = 1.0/19.0
       centlat_mdl    =  0.00
       centlon_mdl    = +10.00
     case default
       print*,"- INVALID DOMAIN SPECIFIED ", trim(domain_name)
       stop
   end select

   resol_mdl = sqrt((dx_mdl**2) + (dy_mdl**2))

   domain_type = "egrid"

 end if

 print*
 print*,"- CREATING SURFACE FIELDS FOR DOMAIN ", trim(domain_name)
 print*,"- I/J GRID DIMENSIONS ARE: ",imdl, jmdl
 print*,"- CENTER LAT/LON ARE: ", centlat_mdl, centlon_mdl
 print*,"- MODEL RESOLUTION IN DEGREES IS: ", resol_mdl

!-----------------------------------------------------------------------
! this software allows for the tiling of soil type, veg type and 
! orography.  the sum of the tiles for all fields can not exceed 
! what the user chooses (max_tot_tiles).   this array will keep a
! running total of the number of tiles available. 
!-----------------------------------------------------------------------

 allocate(remaining_tot_tiles(imdl,jmdl))

 remaining_tot_tiles = max_total_land_tiles

 return

900 print*,"- ERROR OPENING CONFIG NAMELIST. ISTAT IS ", istat
    stop

910 print*,"- ERROR READING CONFIG NAMELIST. ISTAT IS ", istat
    stop

 end subroutine setup

!-----------------------------------------------------------------------
! clear memory
!-----------------------------------------------------------------------

 subroutine setup_cleanup

 implicit none

 if (allocated(remaining_tot_tiles)) deallocate (remaining_tot_tiles)

 return

 end subroutine setup_cleanup

 end module program_setup
