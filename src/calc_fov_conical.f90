 module calc_fov_conical
!$$$   module documentation block
!                .      .    .                                       .
! module:    calc_fov_conical
!
! abstract: contains all routines necessary for fov calculations
!           for conical scanners.
!
! program history log:
!   2008-11-03  kleespies/gayno   initial version
!
! subroutines included:
!   sub fov_ellipse_conical    - calc lat/lon of fov polygon
!   sub fovconicalanglessizes  - compute cross/along track angles/fov sizes
!   sub instrument_init        - initialize variables for a specific instrument
!   sub inside_fov_conical     - determine antenna power at pt within fov
!
! Variable Definitions:
!   def  alongtrackangle       - along track angle
!   def  alongtrackfovsize     - along track fov size
!   def  crosstrackangle       - cross track angle
!   def  crosstrackfovsize     - cross track fov size
!   def  rmax                  - major axis of ellipse
!   def  rmin                  - minor axis of ellipse
!   def  eccen                 - fov eccentricity
!   def  maxinstr              - maximum number of instruments
!   def  maxfov                - maximum number of fovs for each insturment
!   def  npoly                 - number of vertices in the fov polygon
!   def  radius                - radius of earth in km
!   def  nchan                 - number of channels
!   def  instrumentrange       - instruments processed by this code
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

 use kinds, only : i_kind, r_kind
 use calc_fov_crosstrk, only : npoly
 implicit none

 private

 integer(i_kind), parameter, private :: nchan = 24 
 integer(i_kind), parameter, private :: maxinstr = 2
 integer(i_kind), parameter, private :: instrumentrange(maxinstr) = (/26 , 27/)
 real(r_kind), parameter, private    :: radius   = 6371.22_r_kind

 real(r_kind), private :: alongtrackangle(nchan)
 real(r_kind), private :: crosstrackangle(nchan)
 real(r_kind), private :: alongtrackfovsize(nchan)
 real(r_kind), private :: crosstrackfovsize(nchan)
 real(r_kind), private :: rmax(nchan)
 real(r_kind), private :: rmin(nchan)
 real(r_kind), private :: eccen(nchan)
 real(r_kind), private :: psi(npoly)

 public instrument_init
 public inside_fov_conical
 public fov_ellipse_conical

 contains
subroutine instrument_init(instr,satid,expansion)
!                .      .    .                                       .
! subprogram:    instrument_init          initalize instrument fields
!
!   prgmmr: kleespies           org: nesdis              date: 2008-08-09
!
! abstract: initialize variables required by this module.
!   
! program history log:
!   2008-08-09  kleespies
!   2008-11-06  gayno - modified for gsi software standards
!
! input argument list:
!   instr        - instrument number
!                  - 26 is ssmi/s on f16
!                  - 27 is ssmi/s on f17
!   satid        - satellite id
!   expansion    - expansion factor.  must be 1.0 for accurate rendering, 
!                  > 1.0 makes bigger ellipses, < 1.0 makes smaller ellipses.
!                  do not make bigger than 3.0.
!  
! output argument list:
!   n/a
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 use calc_fov_crosstrk, only : get_sat_height
 use constants, only  : pi, one, two, half

 implicit none

! Declare passed variables.
 character(len=*), intent(in)       :: satid
 integer(i_kind), intent(in)        :: instr
 real(r_kind), intent(in)           :: expansion

! Declare local variables.
 integer(i_kind)                    :: i, jchan
 real(r_kind)                       :: ata, cta, atf, ctf, ratio, height

 if ((instr < instrumentrange(1)) .or. (instr > instrumentrange(maxinstr))) then 
   write(6,*) "INSTRUMENT_INIT: INSTRUMENT NUMBER OF: ",instr," OUT OF RANGE." 
   write(6,*) "VALID VALUES ARE: ", instrumentrange(1)," TO ",instrumentrange(maxinstr)
   call stop2(101)
 endif

! fov is polygon.
 do i = 1 , npoly
   psi(i) = two*pi*(i-1)/(npoly-1)  ! will connect npoly points
 enddo

 call get_sat_height(satid, height)

 do jchan = 1 , nchan
! For accurate representation of fov, this computation should go with
! the height from the 1B
   call fovconicalanglessizes(instr,jchan,height,ata,cta,atf,ctf)
   alongtrackangle(jchan)   = ata
   crosstrackangle(jchan)   = cta
   alongtrackfovsize(jchan) = atf
   crosstrackfovsize(jchan) = ctf
   rmax(jchan) = half*crosstrackangle(jchan)* expansion ! remember, these are semiaxes
   rmin(jchan) = half*alongtrackangle(jchan)* expansion
   ratio = rmin(jchan)**2/rmax(jchan)**2
   if(ratio > one) ratio = one  !  this takes care of some precision issues
   eccen(jchan) = sqrt(one - ratio)
 enddo

 return

end subroutine instrument_init
subroutine fovconicalanglessizes(instr,chan,height,alongtrackangle, &
                                 crosstrackangle,alongtrackfovsize, &
                                 crosstrackfovsize)
!                .      .    .                                       .
! subprogram:    fovconicalanglessizes    calc conical angle size
!
!   prgmmr: kleespies           org: nesdis              date: 2008-08-09
!
! abstract: computes the cross track and along track angles of a 
!   conical instrument FOV as viewed from the center of the earth;
!   and cross track and along track FOV size in km. presumes a
!   spherical earth.
!   
! program history log:
!   2008-08-09  kleespies
!   2008-11-06  gayno - modified for gsi software standards
!
! input argument list:
!   instr        - insturment number
!                  - 26 is ssmi/s on f16
!                  - 27 is ssmi/s on f17
!   chan         - channel number (ghz)
!                       1  - 50.3 v
!                       2  - 52.8 v
!                       3  - 53.596 v
!                       4  - 54.4 v
!                       5  - 55.5 v
!                       6  - 57.29 rcp
!                       7  - 59.4 rcp
!                       8  - 150 h
!                       9  - 183.31+-6.6 h
!                       10 - 183.31+-3 h
!                       11 - 183.31+-1 h
!                       12 - 19.35 h
!                       13 - 19.35 v
!                       14 - 22.235 v
!                       15 - 37 h
!                       16 - 37 v
!                       17 - 91.655 v
!                       18 - 91.655 h
!                       19 - 63.283248+-0.285271 rcp
!                       20 - 60.792668+-0.357892 rcp
!                       21 - 60.792668+-0.357892+-0.002 rcp
!                       22 - 60.792668+-0.357892+-0.0055 rcp
!                       23 - 60.792668+-0.357892+-0.016  rcp
!                       24 - 60.792668+-0.357892+-0.05 rcp
!   height       - satellite height in km
!  
! output argument list:
!   alongtrackangle       - along track angle
!   alongtrackfovsize     - along track fov size
!   crosstrackangle       - cross track angle
!   crosstrackfovsize     - cross track fov size
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 use constants, only : half, two, deg2rad, rad2deg, zero

 implicit none

! Declare passed variables.
 integer(i_kind), intent(in)      :: instr
 integer(i_kind), intent(in)      :: chan
 real(r_kind), intent(in)         :: height
 real(r_kind), intent(out)        :: alongtrackangle,crosstrackangle
 real(r_kind), intent(out)        :: alongtrackfovsize,crosstrackfovsize

! Declare local parameters.
 real(r_kind), dimension(instrumentrange(1):instrumentrange(maxinstr)) &
                                  :: conicalangle = (/45._r_kind,45._r_kind/)

! These SSMIS values are the average of the 0 and 90 deg cuts as determined by
! plot_test_conical_fov.f90
 real(r_kind), dimension(nchan,instrumentrange(1):instrumentrange(maxinstr)) &
                                  :: fovangle  =  reshape(                   &
 (/ 0.795_r_kind , 0.795_r_kind,  0.835_r_kind,  0.800_r_kind, &
    0.7725_r_kind, 0.700_r_kind,  0.715_r_kind,  0.405_r_kind, &
    0.365_r_kind , 0.365_r_kind,  0.365_r_kind,  1.945_r_kind, &
    2.0975_r_kind, 1.855_r_kind,  1.210_r_kind,  1.2275_r_kind, &
    0.4125_r_kind, 0.400_r_kind,  0.660_r_kind,  0.6925_r_kind, &
    0.6925_r_kind, 0.6925_r_kind, 0.6925_r_kind, 0.6925_r_kind, &
    0.790_r_kind , 0.780_r_kind,  0.785_r_kind , 0.790_r_kind , &
    0.775_r_kind , 0.715_r_kind , 0.700_r_kind,  0.4225_r_kind, &
    0.3825_r_kind, 0.3825_r_kind, 0.3825_r_kind, 1.930_r_kind , &
    1.9125_r_kind, 1.855_r_kind,  1.2025_r_kind, 1.215_r_kind , &
    0.4175_r_kind, 0.410_r_kind , 0.695_r_kind,  0.6675_r_kind, &
    0.6675_r_kind, 0.6675_r_kind, 0.6675_r_kind, 0.6675_r_kind /) , &
  (/nchan,maxinstr/) )

! Declare local variables.
 real(r_kind)                     :: nadirangle
 real(r_kind)                     :: nadirangle_m
 real(r_kind)                     :: nadirangle_p
 real(r_kind)                     :: compzacenter
 real(r_kind)                     :: compza_m
 real(r_kind)                     :: compza_p
 real(r_kind)                     :: distancetofov

! Nadir angles of center and crosstrack extremes of fov
 nadirangle   = conicalangle(instr)
 nadirangle_m = nadirangle - fovangle(chan,instr)*half 
 nadirangle_p = nadirangle + fovangle(chan,instr)*half 

! Complement of zenith angle for center and crosstrack extremes of fov
 compzacenter = 180._r_kind-asin((radius+height)/radius * sin(nadirangle  /rad2deg))*rad2deg
 compza_m     = 180._r_kind-asin((radius+height)/radius * sin(nadirangle_m/rad2deg))*rad2deg
 compza_p     = 180._r_kind-asin((radius+height)/radius * sin(nadirangle_p/rad2deg))*rad2deg

! cross track angle of the fov as viewed from center of the earth
 crosstrackangle = abs(nadirangle_p + compza_p - nadirangle_m - compza_m)

! cross track fov size in km
 crosstrackfovsize = abs(crosstrackangle*deg2rad*radius)

! distance from satellite to the center of the fov in km
 distancetofov = (radius+height)* &
                  sin( (180._r_kind-nadirangle-compzacenter)/rad2deg)/sin((compzacenter)/rad2deg)
 if(distancetofov < zero) distancetofov = height ! for nadir fov
  
! along track fov size in km. the following is an approximation, but it is close. 
! it underestimates the FOV by a smidge.
 alongtrackfovsize = two*distancetofov*tan(fovangle(chan,instr)*half/rad2deg)

! along track angle of the fov as viewed from center of the earth
 alongtrackangle = rad2deg*alongtrackfovsize/(radius)

end subroutine fovconicalanglessizes
subroutine fov_ellipse_conical(ichan,satellite_azimuth,lat,lon,elats,elons)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fov_ellipse_conical     computes fov ellipses
!
!   prgmmr: kleespies           org: nesdis              date: 2008-08-09
!
! abstract: computes fov ellipses in latitude/longitude coordinates of
!   a conically scanning instrument. 
!   
! program history log:
!   2008-08-09  kleespies
!   2008-11-06  gayno - modified for gsi software standards
!
! input argument list:
!   ichan             - channel number (ghz)
!                       1  - 50.3 v
!                       2  - 52.8 v
!                       3  - 53.596 v
!                       4  - 54.4 v
!                       5  - 55.5 v
!                       6  - 57.29 rcp
!                       7  - 59.4 rcp
!                       8  - 150 h
!                       9  - 183.31+-6.6 h
!                       10 - 183.31+-3 h
!                       11 - 183.31+-1 h
!                       12 - 19.35 h
!                       13 - 19.35 v
!                       14 - 22.235 v
!                       15 - 37 h
!                       16 - 37 v
!                       17 - 91.655 v
!                       18 - 91.655 h
!                       19 - 63.283248+-0.285271 rcp
!                       20 - 60.792668+-0.357892 rcp
!                       21 - 60.792668+-0.357892+-0.002 rcp
!                       22 - 60.792668+-0.357892+-0.0055 rcp
!                       23 - 60.792668+-0.357892+-0.016  rcp
!                       24 - 60.792668+-0.357892+-0.05 rcp
!   satellite_azimuth - satellite azimuth angle (degrees)  
!   lat               - latitude of fov center 
!   lon               - longitude of fov center
!  
! output argument list:
!   elats             - ellipse latitudes  centered about lat,lon
!   elons             - ellipse longitudes centered about lat,lon
!
! remarks:
!
!  There are several engineering checks to handle things like 
!  arcsin( x>1.0 or x<-1.0).  These things can happen because POES navigation
!  uses an oblate spheroid earth, while here we are using a spherical earth,
!  so there is a small inconsistency in computing arc angles.
!
!  No provisions are made for spacecraft roll-pitch-yaw errors, which
!  are presumed to be small (SC attitude is usually held to within 0.1 deg)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 use constants, only : one, rad2deg

 implicit none

! Declare passed variables
 integer(i_kind), intent(in) :: ichan
 real(r_kind), intent(in)    :: lat
 real(r_kind), intent(in)    :: lon
 real(r_kind), intent(in)    :: satellite_azimuth
 real(r_kind), intent(out)   :: elats(npoly)
 real(r_kind), intent(out)   :: elons(npoly)

! Declare local variables
 integer(i_kind)                :: i ! loop counters
 real(r_kind)                   :: pos_ang ! rotation angle of the ellipse
 real(r_kind), dimension(npoly) :: psip
 real(r_kind), dimension(npoly) :: r
 real(r_kind), dimension(npoly) :: cosc
 real(r_kind), dimension(npoly) :: c
 real(r_kind), dimension(npoly) :: sinb
 real(r_kind), dimension(npoly) :: b

 pos_ang = satellite_azimuth 
 psip    = psi + pos_ang/rad2deg 
 r       = rmax(ichan) * sqrt( (one - eccen(ichan)**2)/(one - eccen(ichan)**2 *cos(psi)**2) )
 cosc    = cos((90._r_kind-lat)/rad2deg)*cos(r/rad2deg) + &
           sin((90._r_kind-lat)/rad2deg)*sin(r/rad2deg)*cos(psip)
 c       = acos(cosc)*rad2deg

 elats(1:npoly) = 90._r_kind - c
 sinb = sin(r/rad2deg)*sin(psip)/sin(c/rad2deg)

! handle numeric imprecision 
 do i = 1 , npoly
   if(sinb(i) >  one) sinb(i) =  one
   if(sinb(i) < -one) sinb(i) = -one
 enddo

 b = asin(sinb)*rad2deg
 elons(1:npoly) = lon + b
 
 return
end subroutine fov_ellipse_conical
subroutine inside_fov_conical(instr,ichan,satellite_azimuth,lat,lon, &
                              testlat,testlon,expansion,inside)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inside_fov_conical     inside satellite fov
!
!   prgmmr: kleespies           org: nesdis              date: 2008-08-09
!
! abstract: determines whether a test point is inside a fov
!   for a conically scanning instrument
!   
! program history log:
!   2008-08-09  kleespies
!   2008-11-06  gayno - modified for gsi software standards
!
! input argument list:
!   ichan             - channel number (ghz)
!                       1  - 50.3 v
!                       2  - 52.8 v
!                       3  - 53.596 v
!                       4  - 54.4 v
!                       5  - 55.5 v
!                       6  - 57.29 rcp
!                       7  - 59.4 rcp
!                       8  - 150 h
!                       9  - 183.31+-6.6 h
!                       10 - 183.31+-3 h
!                       11 - 183.31+-1 h
!                       12 - 19.35 h
!                       13 - 19.35 v
!                       14 - 22.235 v
!                       15 - 37 h
!                       16 - 37 v
!                       17 - 91.655 v
!                       18 - 91.655 h
!                       19 - 63.283248+-0.285271 rcp
!                       20 - 60.792668+-0.357892 rcp
!                       21 - 60.792668+-0.357892+-0.002 rcp
!                       22 - 60.792668+-0.357892+-0.0055 rcp
!                       23 - 60.792668+-0.357892+-0.016  rcp
!                       24 - 60.792668+-0.357892+-0.05 rcp
!   instr             - instrument number
!                        - 26 is ssmi/s on f16
!                        - 27 is ssmi/s on f17
!   expansion         - expansion factor.  must be 1.0 for accurate rendering, 
!                       > 1.0 makes bigger ellipses, < 1.0 makes smaller ellipses.
!   satellite_azimuth - satellite azimuth angle (degrees)  
!   lat               - latitude of fov center 
!   lon               - longitude of fov center
!   testlat           - latitude of test point 
!   testlon           - longitude of test point
!  
! output argument list:
!   inside            - 0.0-1.0 relative antenna power if [testLat,testLon] is 
!                       inside the FOV, 0 if outside.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 use constants, only : one, half, two, zero, deg2rad, rad2deg, pi, one_tenth

 implicit none

! Declare passed variables
 integer(i_kind), intent(in) :: instr             
 integer(i_kind), intent(in) :: ichan             
 real(r_kind), intent(in)    :: satellite_azimuth 
 real(r_kind), intent(in)    :: lat               
 real(r_kind), intent(in)    :: lon               
 real(r_kind), intent(in)    :: testlat           
 real(r_kind), intent(in)    :: testlon           
 real(r_kind), intent(in)    :: expansion         
 real(r_kind), intent(out)   :: inside         

! Declare local parameters
 real(r_kind), parameter  :: r1 = one ! Equatorial radius. Work in angular distance, 
                                      ! not km (otherwise r1=6371)
 real(r_kind), parameter  :: r2 = r1  ! assume spherical earth (otherwise r2 = polar radius)

! These are the average of the alongtrack and crosstrack half power beam widths
 real(r_kind) , dimension(nchan,instrumentrange(1):instrumentrange(maxinstr)) :: fovangle = &
 reshape((/  .7900_r_kind, .7800_r_kind, .7850_r_kind, .7900_r_kind, &
             .7750_r_kind, .7150_r_kind, .6925_r_kind, .4225_r_kind, &
             .3825_r_kind, .3825_r_kind, .3825_r_kind,1.9300_r_kind, &
            1.9125_r_kind,1.8550_r_kind,1.2025_r_kind,1.2150_r_kind, &
             .4175_r_kind, .4100_r_kind, .6950_r_kind, .6775_r_kind, &
             .6775_r_kind, .6775_r_kind, .6775_r_kind, .6775_r_kind, &
             .7950_r_kind, .7950_r_kind, .8350_r_kind, .8000_r_kind, &
             .7750_r_kind, .7000_r_kind, .7150_r_kind, .4050_r_kind, &
             .3650_r_kind, .3650_r_kind, .3650_r_kind,1.9450_r_kind, &
            2.0950_r_kind,1.8550_r_kind,1.2100_r_kind,1.2150_r_kind, &
             .4125_r_kind, .4000_r_kind, .6550_r_kind, .6925_r_kind, &
             .6925_r_kind, .6925_r_kind, .6925_r_kind, .6925_r_kind  /), &
         (/ nchan,maxinstr /) )  

! Declare local variables
 real(r_kind)  :: satellite_azimuth_rot ! relative azimuth from NOAA level 1B file
 real(r_kind)  :: dellon ! longitude difference from fov center to test location
 real(r_kind)  :: dellat ! latitude  difference from fov center to test location

! These two are what gets compared to determine if test location is within fov
 real(r_kind)  :: d      ! angular distance from fov center to test location
 real(r_kind)  :: r      ! angular distance from fov center to ellipse along d

 real(r_kind)  :: psi    ! angle from 3:00 on fov to line from fov center to test location
 real(r_kind)  :: psip   ! angle from latitude parallel through fov center
                         ! to line from fov center to test location
 
! These are the flat earth variables
 real(r_kind)  :: distance_north  ! north angular distance from fov center to test location
 real(r_kind)  :: distance_east   ! east  angular distance from fov center to test location
 real(r_kind)  :: bearing_to_test ! same as psip
 real(r_kind)  :: bearing_to_test_deg ! in degrees
 real(r_kind)  :: fovanglesize 
 real(r_kind)  :: x,y,px,py,p,rat
 
 real(r_kind), dimension(0:7,2,nchan) :: ssmiscoeff = reshape( (/                                                                                  &
   0.0000000e+000_r_kind,  -7.2329921e-001_r_kind,  -1.8478422e+001_r_kind,  &
   2.3212047e+000_r_kind,  -1.9556830e+000_r_kind,  -1.1117725e+001_r_kind,  &
  -7.8994775e+000_r_kind,  -2.0754254e+000_r_kind,  &
   0.0000000e+000_r_kind,   8.7811184e-001_r_kind,  -1.9674448e+001_r_kind,  &
  -9.6181625e-001_r_kind,  -6.4224739e+000_r_kind,   4.2248802e+000_r_kind,  &
   1.4424351e+000_r_kind,  -1.3362759e+000_r_kind,  &
   0.0000000e+000_r_kind,   3.5308849e-002_r_kind,  -1.3663235e+001_r_kind,  &
  -4.3558588e+000_r_kind,  -2.7994190e+001_r_kind,  -1.2074537e+001_r_kind,  &
   1.9154778e+001_r_kind,   1.0992817e+001_r_kind,  &
   0.0000000e+000_r_kind,   6.2549269e-001_r_kind,  -1.9263287e+001_r_kind,  &
  -3.2831177e-001_r_kind,  -7.3179111e+000_r_kind,   1.7426276e+000_r_kind,  &
   1.8361624e+000_r_kind,   3.9407963e-001_r_kind,  &
   0.0000000e+000_r_kind,  -7.9663861e-001_r_kind,  -1.8490774e+001_r_kind,  &
   1.2832975e-001_r_kind,  -7.8638358e+000_r_kind,  -1.1461545e+001_r_kind,  &
  -2.2350330e+000_r_kind,   6.4889443e-001_r_kind,  &
   0.0000000e+000_r_kind,   3.6404046e-001_r_kind,  -1.8611902e+001_r_kind,  &
   1.4742138e+000_r_kind,  -7.9531112e+000_r_kind,  -1.8630337e+000_r_kind,  &
   2.3114800e+000_r_kind,   1.7586489e+000_r_kind,  &
   0.0000000e+000_r_kind,  -6.9045693e-001_r_kind,  -2.0003733e+001_r_kind,  &
  -8.7531793e-001_r_kind,  -6.7654867e+000_r_kind,  -1.1082837e+001_r_kind,  &
  -3.9005733e+000_r_kind,  -1.9698878e-001_r_kind,  &
   0.0000000e+000_r_kind,   8.6433291e-001_r_kind,  -1.6531332e+001_r_kind,  &
  -1.0390099e+000_r_kind,  -1.1836189e+001_r_kind,   3.9595103e+000_r_kind,  &
   4.1392865e+000_r_kind,  -1.3473827e+000_r_kind,  &
   0.0000000e+000_r_kind,  -8.2256210e-001_r_kind,  -2.0885773e+001_r_kind,  &
  -6.6913253e-001_r_kind,  -5.7795787e+000_r_kind,  -1.2344604e+001_r_kind,  &
  -5.3577003e+000_r_kind,  -4.4378832e-001_r_kind,  &
   0.0000000e+000_r_kind,   3.5819715e-001_r_kind,  -1.8299393e+001_r_kind,  &
  -3.7717700e-001_r_kind,  -7.9926181e+000_r_kind,   3.5663846e+000_r_kind,  &
   2.5146661e+000_r_kind,  -1.4314207e+000_r_kind,  &
   0.0000000e+000_r_kind,   2.5967506e-001_r_kind,  -3.0314299e+001_r_kind,  &
   2.2098860e+001_r_kind,   8.0225205e-001_r_kind,  -2.1946568e+001_r_kind,  &
   5.9498363e+000_r_kind,   4.2685871e+000_r_kind,  &
   0.0000000e+000_r_kind,   2.2720766e+000_r_kind,  -2.4673639e+001_r_kind,  &
   2.4124540e-001_r_kind,  -4.0763054e+000_r_kind,   3.9966695e+000_r_kind,  &
   8.8790596e-001_r_kind,  -1.6553838e+000_r_kind,  &
   0.0000000e+000_r_kind,  -4.0487466e+000_r_kind,  -2.3861103e+001_r_kind,  &
   1.7349501e+001_r_kind,  -2.1052830e+001_r_kind,  -2.6700981e+000_r_kind,  &
   1.8992153e+001_r_kind,  -7.0507226e+000_r_kind,  &
   0.0000000e+000_r_kind,  -1.6305245e+000_r_kind,  -2.3506573e+001_r_kind,  &
  -4.7227297e+000_r_kind,  -8.7765541e+000_r_kind,   5.4034901e+000_r_kind,  &
   2.8823271e+000_r_kind,  -3.3243134e+000_r_kind,  &
   0.0000000e+000_r_kind,   9.4644480e+000_r_kind,  -8.3094887e+001_r_kind,  &
  -1.2976602e+000_r_kind,  -4.7322140e+001_r_kind,  -3.1127609e+002_r_kind,  &
   2.2231352e+002_r_kind,   5.7508954e+002_r_kind,  &
   0.0000000e+000_r_kind,   5.8333130e+000_r_kind,  -1.0824286e+002_r_kind,  &
   1.2486229e+002_r_kind,   2.9551187e+002_r_kind,  -7.6531335e+002_r_kind,  &
  -6.8249127e+002_r_kind,   1.2905453e+003_r_kind,  &
   0.0000000e+000_r_kind,   7.9034691e+000_r_kind,  -9.7452660e+001_r_kind,  &
  -4.6270566e+000_r_kind,   3.8495872e+000_r_kind,  -2.5075090e+002_r_kind,  &
   1.7962137e+002_r_kind,   4.4105771e+002_r_kind,  &
   0.0000000e+000_r_kind,   6.0860925e+000_r_kind,  -6.9839317e+001_r_kind,  &
  -3.1335958e+001_r_kind,   8.7344557e-002_r_kind,   2.4287263e-004_r_kind,  &
  -3.5353769e-006_r_kind,   1.9087382e-008_r_kind,  &
   0.0000000e+000_r_kind,   7.9034691e+000_r_kind,  -9.7452660e+001_r_kind,  &
  -4.6270566e+000_r_kind,   3.8495872e+000_r_kind,  -2.5075090e+002_r_kind,  &
   1.7962137e+002_r_kind,   4.4105771e+002_r_kind,  &
   0.0000000e+000_r_kind,   6.0860925e+000_r_kind,  -6.9839317e+001_r_kind,  &
  -3.1335958e+001_r_kind,   8.7344557e-002_r_kind,   2.4287263e-004_r_kind,  &
  -3.5353769e-006_r_kind,   1.9087382e-008_r_kind,  &
   0.0000000e+000_r_kind,   7.9034691e+000_r_kind,  -9.7452660e+001_r_kind,  &
  -4.6270566e+000_r_kind,   3.8495872e+000_r_kind,  -2.5075090e+002_r_kind,  &
   1.7962137e+002_r_kind,   4.4105771e+002_r_kind,  &
   0.0000000e+000_r_kind,   6.0860925e+000_r_kind,  -6.9839317e+001_r_kind,  &
  -3.1335958e+001_r_kind,   8.7344557e-002_r_kind,   2.4287263e-004_r_kind,  &
  -3.5353769e-006_r_kind,   1.9087382e-008_r_kind,  &
   0.0000000e+000_r_kind,  -7.3582727e-001_r_kind,  -3.7477145e+000_r_kind,  &
   1.1095083e+000_r_kind,  -1.2551503e-001_r_kind,  -1.5246950e-001_r_kind,  &
   2.4320448e-002_r_kind,   4.4045732e-003_r_kind,  &
   0.0000000e+000_r_kind,   2.4585837e-001_r_kind,  -2.8314576e+000_r_kind,  &
  -1.5220909e-001_r_kind,  -2.8791896e-001_r_kind,   7.9443365e-002_r_kind,  &
   1.8362561e-002_r_kind,  -9.4546024e-003_r_kind,  &
   0.0000000e+000_r_kind,  -7.7728200e-001_r_kind,  -3.6286652e+000_r_kind,  &
   1.0751594e+000_r_kind,  -1.9386607e-001_r_kind,  -1.2000034e-001_r_kind,  &
   3.0559521e-002_r_kind,   1.5031564e-003_r_kind,  &
   0.0000000e+000_r_kind,   1.6124040e-001_r_kind,  -3.2177579e+000_r_kind,  &
  -4.0324433e-003_r_kind,  -1.3595626e-001_r_kind,   1.5122382e-002_r_kind,  &
  -8.3842678e-003_r_kind,  -9.0431923e-004_r_kind,  &
   0.0000000e+000_r_kind,   4.5644647e-001_r_kind,  -3.5724955e+000_r_kind,  &
   2.3994593e-002_r_kind,   1.7960340e-002_r_kind,   8.8682011e-002_r_kind,  &
  -5.4145303e-002_r_kind,   9.5899152e-003_r_kind,  &
   0.0000000e+000_r_kind,   7.2329707e-002_r_kind,  -3.4088185e+000_r_kind,  &
   5.7849020e-002_r_kind,  -1.0772277e-001_r_kind,   1.0372316e-002_r_kind,  &
  -1.7166324e-003_r_kind,  -1.0316035e-003_r_kind,  &
   0.0000000e+000_r_kind,   6.0870802e-001_r_kind,  -8.4153900e+000_r_kind,  &
  -3.7284335e-001_r_kind,   4.0136233e-001_r_kind,  -3.1463799e-001_r_kind,  &
  -2.7650723e-001_r_kind,  -5.3687301e-002_r_kind,  &
   0.0000000e+000_r_kind,   7.4301755e-001_r_kind,  -8.1024418e+000_r_kind,  &
  -1.7737685e-001_r_kind,  -8.2376891e-001_r_kind,   1.0515687e-001_r_kind,  &
   1.3707381e-001_r_kind,  -3.1215623e-002_r_kind,  &
   0.0000000e+000_r_kind,   3.6598334e-001_r_kind,  -7.9595804e+000_r_kind,  &
  -5.4379743e-001_r_kind,   2.0795539e-001_r_kind,  -2.3518072e-001_r_kind,  &
  -3.3832863e-001_r_kind,  -1.0193684e-001_r_kind,  &
   0.0000000e+000_r_kind,   4.6996954e-001_r_kind,  -8.3342657e+000_r_kind,  &
  -1.4640558e-001_r_kind,  -5.2858168e-001_r_kind,  -2.4176065e-002_r_kind,  &
   7.5972915e-002_r_kind,   1.8081643e-002_r_kind,  &
   0.0000000e+000_r_kind,  -3.5891154e+000_r_kind,  -6.0961403e+001_r_kind,  &
  -9.9227448e+001_r_kind,  -1.2077326e+002_r_kind,   8.7933252e+002_r_kind,  &
   4.1147614e+001_r_kind,  -1.6901060e+003_r_kind,  &
   0.0000000e+000_r_kind,  -2.8644288e+000_r_kind,  -6.8392441e+001_r_kind,  &
  -2.3870790e+000_r_kind,  -1.3848622e+002_r_kind,  -1.8050613e+002_r_kind,  &
   1.3202374e+002_r_kind,   1.3246623e+002_r_kind,  &
   0.0000000e+000_r_kind,   6.3815022e+000_r_kind,  -5.1539516e+001_r_kind,  &
   7.8090148e+000_r_kind,  -4.6514099e+002_r_kind,   5.3498145e+002_r_kind,  &
   1.1642684e+003_r_kind,  -1.6503534e+003_r_kind,  &
   0.0000000e+000_r_kind,  -1.8209287e+000_r_kind,  -7.1066109e+001_r_kind,  &
  -9.9367409e+000_r_kind,  -6.8998085e+001_r_kind,  -5.9232510e+001_r_kind,  &
  -4.0851124e+001_r_kind,  -7.0612854e+001_r_kind,  &
   0.0000000e+000_r_kind,  -1.6817180e+000_r_kind,  -2.4404591e+001_r_kind,  &
  -2.1817307e-001_r_kind,   1.9598949e-001_r_kind,   1.2614151e+001_r_kind,  &
  -1.1097395e+001_r_kind,   2.3721972e+000_r_kind,  &
   0.0000000e+000_r_kind,   2.3969443e+000_r_kind,  -2.4861862e+001_r_kind,  &
   1.7981834e+000_r_kind,  -1.5985849e+001_r_kind,  -1.9211575e-001_r_kind,  &
   1.0514291e+001_r_kind,  -1.6616911e+000_r_kind,  &
   0.0000000e+000_r_kind,  -1.4215437e-001_r_kind,  -2.7673775e+001_r_kind,  &
   2.2236590e+000_r_kind,   3.4517300e+000_r_kind,   6.7635159e+000_r_kind,  &
  -1.3662134e+001_r_kind,   5.8717651e+000_r_kind,  &
   0.0000000e+000_r_kind,  -2.0684712e+000_r_kind,  -2.4435213e+001_r_kind,  &
  -6.7334828e+000_r_kind,  -1.9548767e+001_r_kind,   1.7209103e+001_r_kind,  &
   1.4496602e+001_r_kind,  -1.1115800e+001_r_kind,  &
   0.0000000e+000_r_kind,  -1.4215437e-001_r_kind,  -2.7673775e+001_r_kind,  &
   2.2236590e+000_r_kind,   3.4517300e+000_r_kind,   6.7635159e+000_r_kind,  &
  -1.3662134e+001_r_kind,   5.8717651e+000_r_kind,  &
   0.0000000e+000_r_kind,  -2.0684712e+000_r_kind,  -2.4435213e+001_r_kind,  &
  -6.7334828e+000_r_kind,  -1.9548767e+001_r_kind,   1.7209103e+001_r_kind,  &
   1.4496602e+001_r_kind,  -1.1115800e+001_r_kind,  &
   0.0000000e+000_r_kind,  -1.4215437e-001_r_kind,  -2.7673775e+001_r_kind,  &
   2.2236590e+000_r_kind,   3.4517300e+000_r_kind,   6.7635159e+000_r_kind,  &
  -1.3662134e+001_r_kind,   5.8717651e+000_r_kind,  &
   0.0000000e+000_r_kind,  -2.0684712e+000_r_kind,  -2.4435213e+001_r_kind,  &
  -6.7334828e+000_r_kind,  -1.9548767e+001_r_kind,   1.7209103e+001_r_kind,  &
   1.4496602e+001_r_kind,  -1.1115800e+001_r_kind,  &
   0.0000000e+000_r_kind,  -1.4215437e-001_r_kind,  -2.7673775e+001_r_kind,  &
   2.2236590e+000_r_kind,   3.4517300e+000_r_kind,   6.7635159e+000_r_kind,  &
  -1.3662134e+001_r_kind,   5.8717651e+000_r_kind,  &
   0.0000000e+000_r_kind,  -2.0684712e+000_r_kind,  -2.4435213e+001_r_kind,  &
  -6.7334828e+000_r_kind,  -1.9548767e+001_r_kind,   1.7209103e+001_r_kind,  &
   1.4496602e+001_r_kind,  -1.1115800e+001_r_kind,  &
   0.0000000e+000_r_kind,  -1.4215437e-001_r_kind,  -2.7673775e+001_r_kind,  &
   2.2236590e+000_r_kind,   3.4517300e+000_r_kind,   6.7635159e+000_r_kind,  &
  -1.3662134e+001_r_kind,   5.8717651e+000_r_kind,  &
   0.0000000e+000_r_kind,  -2.0684712e+000_r_kind,  -2.4435213e+001_r_kind,  &
  -6.7334828e+000_r_kind,  -1.9548767e+001_r_kind,   1.7209103e+001_r_kind,  &
   1.4496602e+001_r_kind,  -1.1115800e+001_r_kind /),  &
       (/ 8,2,nchan /) )

! Get satellite az to where we want it. 1st, convert +- to 0-360
  satellite_azimuth_rot = satellite_azimuth
  if(satellite_azimuth_rot < zero) satellite_azimuth_rot = 360.0_r_kind + satellite_azimuth_rot
! 2nd, shift rotation direction
  satellite_azimuth_rot = mod((450.0_r_kind-satellite_azimuth_rot),360.0_r_kind)

  dellat = (testlat - lat)*deg2rad
  dellon = testlon - lon
  if (dellon > 180._r_kind) dellon = dellon - 360._r_kind
  dellon = dellon*deg2rad

! Distance north and east in degrees
  distance_north =  r1*dellat
  distance_east  =  r2*cos(lat*deg2rad)*dellon

! Angle to the test point
  bearing_to_test = mod(atan2(distance_north,distance_east),two*pi)
  bearing_to_test_deg = bearing_to_test*rad2deg ! convert to degrees

! This is the arc distance to the test point
  d=two*asin(sqrt((sind((testlat-lat)/two))**2 +     & 
                 cosd(testlat)*cosd(lat)*(sind((testlon-lon)/two))**2))
  d = d*rad2deg  ! convert to degrees

  psip = bearing_to_test_deg

  psi = (psip  - satellite_azimuth_rot)
  psi = psi*deg2rad ! convert to radians

! r is the angular distance from the ichan center to the edge of the ellipse in degrees
  r = rmax(ichan)*sqrt( (one - eccen(ichan)**2)/(one - eccen(ichan)**2 *cos(psi)**2) )

  inside = zero

  fovanglesize = fovangle(ichan,instr)

  if (d<r) then

    rat = d / r * expansion * fovanglesize * half

    x = rat * cos(psi)
    y = rat * sin(psi)
 
    px = ssmiscoeff(0,1,ichan) + ssmiscoeff(1,1,ichan)*x    + ssmiscoeff(2,1,ichan)*x**2 & 
                               + ssmiscoeff(3,1,ichan)*x**3 + ssmiscoeff(4,1,ichan)*x**4 &
                               + ssmiscoeff(5,1,ichan)*x**5 + ssmiscoeff(6,1,ichan)*x**6 &
		               + ssmiscoeff(7,1,ichan)*x**7   

    py = ssmiscoeff(0,2,ichan) + ssmiscoeff(1,2,ichan)*y    + ssmiscoeff(2,2,ichan)*y**2 &
                               + ssmiscoeff(3,2,ichan)*y**3 + ssmiscoeff(4,2,ichan)*y**4 &
                               + ssmiscoeff(5,2,ichan)*y**5 + ssmiscoeff(6,2,ichan)*y**6 &
	                       + ssmiscoeff(7,2,ichan)*y**7   
   
    p = -(px+py) ! power in dB (positive)

   ! convert to fraction of max power

    p = 10._r_kind**(-p*one_tenth)

    inside = p  
    if(inside > one) inside = one
   
  endif

 return
 end subroutine inside_fov_conical
 end module calc_fov_conical
