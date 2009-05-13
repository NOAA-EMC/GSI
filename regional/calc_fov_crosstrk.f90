 module calc_fov_crosstrk
!$$$   module documentation block
!                .      .    .                                       .
! module:    calc_fov_crosstrk
!
! abstract: contains all routines necessary for fov calculations
!   for cross track scanning instruments
!
! program history log:
!   2007-08-13  kleespies/gayno   initial version
!
! subroutines included:
!   sub fov_ellipse_crosstrk   - calc lat/lon of fov polygon
!   sub fovanglessizes         - compute cross/along track angles/fov sizes
!   sub get_sat_height         - get satellite height
!   sub fov_cleanup            - free up memory
!   sub fov_check              - ensure fov number is valid
!   sub instrument_init        - initialize instrument fields
!   sub inside_fov_crosstrk    - determine if point inside fov
!
! Variable Definitions:
!   def  alongtrackangle       - along track angle
!   def  alongtrackfovsize     - along track fov size
!   def  crosstrackangle       - cross track angle
!   def  crosstrackfovsize     - cross track fov size
!   def  eccen                 - fov eccentricity
!   def  maxinstr              - maximum number of instruments
!   def  maxfov                - maximum number of fovs for each insturment
!   def  npoly                 - number of vertices in the fov polygon
!   def  psi                   - angle of each vertex of fov polygon
!   def  rmax                  - major axis of ellipse
!   def  rmin                  - minor axis of ellipse
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

 use kinds, only : i_kind, r_kind

 implicit none

 private

 integer(i_kind) , parameter, public    :: npoly = 30
 integer(i_kind) , parameter, private   :: maxinstr = 16
 integer(i_kind) , dimension(maxinstr), private:: maxfov = (/ 2048,2048,2048,56,56,56,56,56, &
                                                              8,11,30,90,90,96,96,96 /)

 real(r_kind) , dimension(:), allocatable, private :: alongtrackangle
 real(r_kind) , dimension(:), allocatable, private :: crosstrackangle
 real(r_kind) , dimension(:), allocatable, private :: alongtrackfovsize
 real(r_kind) , dimension(:), allocatable, private :: crosstrackfovsize
 real(r_kind) , dimension(:), allocatable, private :: rmax
 real(r_kind) , dimension(:), allocatable, private :: rmin
 real(r_kind) , dimension(:), allocatable, private :: eccen
 real(r_kind) , private                            :: psi(npoly)

 public get_sat_height
 public instrument_init
 public fov_check
 public fov_cleanup
 public inside_fov_crosstrk
 public fov_ellipse_crosstrk

 contains

 subroutine instrument_init(instr, satid, expansion)
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
!   instr           - Instrument number
!                      1 = AVHRR-2 LAC/HRPT
!                      2 = AVHRR-3 LAC/HRPT
!		       3 = AVHRR-3 LAC/HRPT on NOAA-16
!                      4 = HIRS-2
!		       5 = HIRS-2I
!		       6 = HIRS-3 NOAA-K
!                      7 = HIRS-3 NOAA-L,M
!		       8 = HIRS-4
!		       9 = SSU
!		      10 = MSU
!		      11 = AMSU-A
!		      12 = AMSU-B, AIRS, HSB
!		      13 = MHS
!                     14 = ATMS 5.2 DEG
!		      15 = ATMS 2.2 DEG
!                     16 = ATMS 1.1 DEG
!   satid            - satellite id
!   expansion        - expansion factor.  Must be 1.0 for accurate renderine, 
!                      > 1.0 makes bigger ellipses, < 1.0 makes smaller ellipses.
!                      do not make bigger than 3.0.  
!
! output argument list:
!   n/a
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 use constants, only                 : pi, one, two

 implicit none

! Declare passed variables.
 character(len=*), intent(in)       :: satid
 integer(i_kind), intent(in)        :: instr
 real(r_kind), intent(in)           :: expansion

! Declare local variables.
 integer(i_kind)                    :: i, ifov
 real(r_kind)                       :: ata, cta, atf, ctf, ratio, height    

 if (instr < 1 .or. instr > maxinstr) then
   write(6,*) "INSTURMENT_INIT: INSTRUMENT NUMBER OF: ", instr, " IS OUT OF RANGE."
   call stop2(100)
 end if

 call get_sat_height(satid, height)

 call fov_cleanup

 allocate (alongtrackangle(1:maxfov(instr)))
 allocate (crosstrackangle(1:maxfov(instr)))
 allocate (alongtrackfovsize(1:maxfov(instr)))
 allocate (crosstrackfovsize(1:maxfov(instr)))
 allocate (rmax(1:maxfov(instr)))
 allocate (rmin(1:maxfov(instr)))
 allocate (eccen(1:maxfov(instr)))
 
 do i = 1, npoly
   psi(i) = two*pi*float(i-1)/float(npoly-1) ! Will connect Npoly points
 enddo
 
! Precompute angles and sizes for speed. For accurate representation of fov, 
! this computation should go with the height from the 1B.

 do ifov = 1 , maxfov(instr)  
   call fovanglessizes(instr,height,ifov,ata,cta,atf,ctf)
   alongtrackangle(ifov)   = ata
   crosstrackangle(ifov)   = cta
   alongtrackfovsize(ifov) = atf
   crosstrackfovsize(ifov) = ctf
 enddo  ! ifov

 rmax = 0.5_r_kind * crosstrackangle * expansion ! remember, these are semiaxes
 rmin = 0.5_r_kind * alongtrackangle * expansion

 do i = 1 , maxfov(instr)
   ratio = rmin(i)**2/rmax(i)**2
   if(ratio > one ) ratio = one  !  this takes care of some precision issues
   eccen(i) = sqrt(one - ratio)
 enddo

 end subroutine instrument_init
 subroutine fov_cleanup
!                .      .    .                                       .
! subprogram:    fov cleanup        deallocate module arrays
!
!   prgmmr: gayno               org: emc                 date: 2008-08-09
!
! abstract: free up memory by deallocating module arrays
!
! program history log:
!   2008-08-09  gayno
!
! input argument list:
!   n/a
!
! output argument list:
!   n/a
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 implicit none

 if(allocated(alongtrackangle))   deallocate (alongtrackangle)
 if(allocated(crosstrackangle))   deallocate (crosstrackangle)
 if(allocated(alongtrackfovsize)) deallocate (alongtrackfovsize)
 if(allocated(crosstrackfovsize)) deallocate (crosstrackfovsize)
 if(allocated(rmax))              deallocate (rmax)
 if(allocated(rmin))              deallocate (rmin)
 if(allocated(eccen))             deallocate (eccen)

 end subroutine fov_cleanup
 subroutine fov_ellipse_crosstrk (fov, instr, satellite_azimuth,  &
                                  lat, lon, elats, elons )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fov_ellipse_crosstrk    computes fov ellipses
!
!   prgmmr: kleespies           org: nesdis              date: 2007-08-09
!
! abstract: computes FOV ellipses in latitude/longitude coordinates
!   for a scan line of a cross track scanning instrument.
!
! program history log:
!   2007-08-09  kleespies
!   2007-08-13  gayno - modified for gsi software standards
!
! input argument list:
!   fov             - fov number  min = 1, max as below
!		       2048 = AVHRR-2 LAC/HRPT
!		       2048 = AVHRR-3 LAC/HRPT
!		       2048 = AVHRR-3 LAC/HRPT on NOAA-16
!	               56 = HIRS-2
!		       56 = HIRS-2I
!		       56 = HIRS-3
!		       56 = HIRS-4
!		        8 = SSU
!		       11 = MSU
!		       30 = AMSU-A
!		       90 = AMSU-B
!		       90 = AIRS
!                      96 = ATMS
!   instr           - instrument number
!                      1 = AVHRR-2 LAC/HRPT
!                      2 = AVHRR-3 LAC/HRPT
!		       3 = AVHRR-3 LAC/HRPT on NOAA-16
!                      4 = HIRS-2
!		       5 = HIRS-2I
!		       6 = HIRS-3 NOAA-K
!                      7 = HIRS-3 NOAA-L,M
!		       8 = HIRS-4
!		       9 = SSU
!		      10 = MSU
!		      11 = AMSU-A
!		      12 = AMSU-B, AIRS, HSB
!		      13 = MHS
!                     14 = ATMS 5.2 DEG
!		      15 = ATMS 2.2 DEG
!                     16 = ATMS 1.1 DEG
!   satellite_azimuth - satellite azimuth angle
!   lat               - latitude of center of fov 
!   lon               - longitude of center of fov
!  
! output argument list:
!   elats            - ellipse latitudes  centered about lat,lon
!   elons            - ellipse longitudes centered about lat,lon
!
! remarks:
!
!   Set npoly for the order of the polygon that represents the ellipse. 
!
!   elats and elon must be dimensioned to npoly in the calling routine.
!
!   There are several engineering checks to handle things like 
!   arcsin( x>1.0 or x<-1.0). These things can happen because POES navigation
!   uses an oblate spheroid earth, while here we are using a spherical
!   earth, so there is a small inconsistency in computing arc angles.
!
!   No provisions are made for spacecraft roll-pitch-yaw errors,
!   which are presumed to be small. (SC attitude is usually held to within 0.1 deg)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 use constants, only : rad2deg, one

 implicit none

! Declare passed variables
 integer(i_kind), intent(in):: fov
 integer(i_kind), intent(in):: instr
 real(r_kind), intent(in):: satellite_azimuth
 real(r_kind), intent(in):: lat
 real(r_kind), intent(in):: lon
 real(r_kind), intent(out):: elats(npoly)
 real(r_kind), intent(out):: elons(npoly)

! Declare local variables
 integer(i_kind):: i ! loop counters
 real(r_kind):: pos_ang ! rotation angle of the ellipse
 real(r_kind):: psip(npoly), r(npoly), cosc(npoly), c(npoly), sinb(npoly), b(npoly)

 pos_ang = satellite_azimuth
 if(pos_ang > 180._r_kind)  pos_ang = pos_ang-360._r_kind
 if(pos_ang < -180._r_kind) pos_ang = 360._r_kind + pos_ang

 psip    = psi + pos_ang/rad2deg
 r       = rmax(fov) * sqrt( (one - eccen(fov)**2)/(one - eccen(fov)**2 *cos(psi)**2) ) 
 cosc    = cos((90._r_kind-lat)/rad2deg)*cos(r/rad2deg) + &
           sin((90._r_kind-lat)/rad2deg)*sin(r/rad2deg)*cos(psip)
 c       = acos(cosc)*rad2deg

 elats(1:npoly) = 90._r_kind - c

 sinb = sin(r/rad2deg)*sin(psip)/sin(c/rad2deg)
 do i = 1 , npoly  ! handle numeric imprecision
   if(sinb(i) >  one) sinb(i) =  one
   if(sinb(i) < -(one)) sinb(i) = -(one)
 enddo

 b              = asin(sinb)*rad2deg
 elons(1:npoly) = lon + b
 
 end subroutine fov_ellipse_crosstrk
 subroutine fovanglessizes(instr,height,fov,alongtrackangle,crosstrackangle,&
                           alongtrackfovsize,crosstrackfovsize)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fovanglessizes   return cross and along track angles/sizes.
!
!   prgmmr: kleespies           org: nesdis              date: 2007-08-09
!
! abstract: computes the cross track and along track angles of an 
!      instrument FOV as viewed from the center of the earth, and 
!      cross track and along track FOV size in km. presumes a spherical earth.
!
! program history log:
!   2007-08-09  kleespies
!   2007-08-13  gayno - modified for gsi software standards
!
! input argument list:
!   instr         - Instrument number
!                     1 = AVHRR-2 LAC/HRPT
!                     2 = AVHRR-3 LAC/HRPT
!		      3 = AVHRR-3 LAC/HRPT on NOAA-16
!                     4 = HIRS-2
!		      5 = HIRS-2I
!		      6 = HIRS-3 NOAA-K
!                     7 = HIRS-3 NOAA-L,M
!		      8 = HIRS-4
!		      9 = SSU
!		     10 = MSU
!		     11 = AMSU-A
!		     12 = AMSU-B, AIRS, HSB
!		     13 = MHS
!                    14 = ATMS 5.2 DEG
!		     15 = ATMS 2.2 DEG
!		     16 = ATMS 1.1 DEG
!   height        - height of satellite in km
!   fov           - fov number  min = 1, max as below
!		     2048 = AVHRR-2 LAC/HRPT
!		     2048 = AVHRR-3 LAC/HRPT
!		     2048 = AVHRR-3 LAC/HRPT on NOAA-16
!	             56 = HIRS-2
!		     56 = HIRS-2I
!		     56 = HIRS-3
!		     56 = HIRS-4
!		      8 = SSU
!		     11 = MSU
!		     30 = AMSU-A
!		     90 = AMSU-B
!		     90 = AIRS
!                    96 = ATMS
!
!  
! output argument list:
!   AlongTrackAngle    - along track angle of a fov
!   CrossTrackAngle    - cross track angle of a fov
!   AlongTrackFOVSize  - along track fov size in km
!   CrossTrackFOVSize  - cross track fov size in km
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 use constants, only  :  pi, rad2deg, two

 implicit none

! Declare passed variables
 integer(i_kind), intent(in):: fov, instr
 real(r_kind), intent(in) :: height
 real(r_kind), intent(out):: alongtrackangle, crosstrackangle
 real(r_kind), intent(out):: alongtrackfovsize, crosstrackfovsize

! Declare local parameters
 real(r_kind), parameter:: radius = 6371.22_r_kind

! assymetry   degrees (+-) that the scanner is assymetric about nadir.  
!             This thusfar is important only for NOAA-16&17 HIRS.
! degscan     FOV step angle
! fovangle    angular dimension of the FOV
! halfscan    degrees from nadir to the center of outer earth FOV

! i think AIRS is same as AMSU-B... anyway, have put MHS into old AIRS slot
! noaa-n parameters presently set to klm.  may need different numbers for 
! AQUA AMSU-A and HSB amsub=1.1, mhs=10/9.

 real(r_kind) , dimension(maxinstr) :: degscan   = (/ 0.0541e0_r_kind, 0.0541e0_r_kind, 0.053981436e0_r_kind, &
                                                      1.8e0_r_kind, 1.8e0_r_kind, 1.8e0_r_kind, &
                                                      1.8e0_r_kind, 1.8e0_r_kind, 10.0e0_r_kind, &
                                                      9.4737e0_r_kind, 3.e0_r_kind+1._r_kind/3._r_kind, &
                                                      1.1e0_r_kind , 10.e0_r_kind/9.e0_r_kind , 1.1e0_r_kind, &
                                                      1.1e0_r_kind, 1.1e0_r_kind /)
 real(r_kind) , dimension(maxinstr) :: fovangle  = (/ 0.0745_r_kind,   0.0745_r_kind,  0.0745_r_kind,  &
                                                      1.22_r_kind,  1.40_r_kind,  1.40_r_kind,  &
                                                      1.40_r_kind,  0.70_r_kind,  10.0_r_kind,  &
                                                      7.5_r_kind,  3.3_r_kind,    1.1_r_kind,  &
                                                      1.1_r_kind ,   5.2_r_kind,   2.2_r_kind,   1.1_r_kind /)
 real(r_kind) , dimension(maxinstr) :: halfscan  = (/ 55.37_r_kind,  55.37_r_kind,  55.25_r_kind,  49.5_r_kind, &
                                                      49.5_r_kind,  49.5_r_kind,  49.5_r_kind,  49.5_r_kind, &
                                                      35.0_r_kind,  47.3685_r_kind,  &
                                                      48._r_kind+1._r_kind/3._r_kind,  48.95_r_kind, &
                                                      48.95_r_kind, 52.73_r_kind, 52.73_r_kind, 52.73_r_kind /)
 real(r_kind) , dimension(maxinstr) :: assymetry = (/ 0.0_r_kind,   0.0_r_kind,   0.0_r_kind,   0.0_r_kind,  &
                                                      0.0_r_kind,   0.0_r_kind,  -1.8_r_kind,   0.0_r_kind,  &
                                                      0.0_r_kind,   0.0_r_kind,   0.0_r_kind,   0.0_r_kind,  &
                                                      0.0_r_kind,   0.0_r_kind,   0.0_r_kind,   0.0_r_kind /)

! declare local variables
 real(r_kind) nadirangle, nadirangle_m, nadirangle_p
 real(r_kind) compzacenter, compza_m, compza_p, distancetofov

! initialize to bad value
 alongtrackangle = -999._r_kind
 crosstrackangle = -999._r_kind

!Nadir angles of center and crosstrack extremes of fov
 nadirangle   = halfscan(instr) - (fov-1)*degscan(instr) + assymetry(instr)
 nadirangle_m = nadirangle - fovangle(instr)*0.5_r_kind 
 nadirangle_p = nadirangle + fovangle(Instr)*0.5_r_kind 

!Complement of zenith angle for center and crosstrack extremes of fov
 compzacenter = 180._r_kind - asin((radius+height)/radius * sin(nadirangle  /rad2deg))*rad2deg
 compza_m     = 180._r_kind - asin((radius+height)/radius * sin(nadirangle_m/rad2deg))*rad2deg
 compza_p     = 180._r_kind - asin((radius+height)/radius * sin(nadirangle_p/rad2deg))*rad2deg

!cross track angle of the fov as viewed from center of the earth
 crosstrackangle = abs(nadirangle_p + compza_p - nadirangle_m - compza_m)

!cross track fov size in km
 crosstrackfovsize = abs(crosstrackangle*two*pi*radius/360._r_kind)

!distance from satellite to the center of the fov in km
 distancetofov = (radius+height) * &
                 sin( (180._r_kind-nadirangle-compzacenter)/rad2deg)/sin((compzacenter)/rad2deg)
 
!along track fov size in km
! the following is an approximation, but it is close.  It underestimates the FOV by a smidge
 alongtrackfovsize = two*distancetofov*tan(fovangle(instr)*0.5_r_kind/rad2deg)

!along track angle of the fov as viewed from center of the earth
 alongtrackangle = 360._r_kind * alongtrackfovsize/(two*pi*radius)

 end subroutine fovanglessizes
 subroutine get_sat_height(satid, height)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_sat_height   return height of satellite in km
!   prgmmr: gayno           org: np23                date: 2007-08-09
!
! abstract: return height of desired satellite in km. 
!           information from:
!           1) www.itc.nl/research/products/sensordb
!           2) a nasa/noaa booklet entitled, "Advanced TIROS-N
!              (ATN): NOAA J"  TL798.M4A38 (1993)
!
! program history log:
!   2007-08-09  gayno
!
! input argument list:
!   satid         - satellite id
!  
! output argument list:
!   height        - height of satellite in km
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 implicit none

! Declare passed variables
 character(len=*), intent(in)    :: satid
 real(r_kind), intent(out)       :: height

 select case (trim(satid))
   case('tirosn')
     height=870._r_kind
   case('dmsp', 'f13', 'f14', 'f15', 'f16', 'f17')
     height=830._r_kind
   case('trmm')
     height=350._r_kind
   case('n05')
     height=1511._r_kind
   case('n07', 'n09', 'n11', 'n14', 'n16', 'n18', 'n19')
     height=870._r_kind
   case('n06', 'n08', 'n10', 'n12', 'n15', 'n17')
     height=833._r_kind
   case('aura', 'aqua')
     height=705._r_kind
   case('metop-a')
     height=817._r_kind
   case default
     write(6,*) 'GET_SAT_HEIGHT: ERROR, unrecognized satellite id: ', trim(satid)
     call stop2(100)
 end select

 end subroutine get_sat_height
 subroutine inside_fov_crosstrk(instr, fov, satellite_azimuth,  &
                                lat, lon, testlat, testlon, &
		                expansion, inside)
!                .      .    .                                       .
! subprogram:  inside_fov_crosstrk         determine antenna power
!
!   prgmmr: kleespies           org: nesdis              date: 2008-08-09
!
! abstract: determine if a point is inside a fov.  if it is, return
!   the relative antenna power.
!
! program history log:
!   2008-08-09  kleespies
!   2008-11-06  gayno - modified for gsi software standards
!
! input argument list:
!   instr                 - instrument number
!                           1 = AVHRR-2 LAC/HRPT
!                           2 = AVHRR-3 LAC/HRPT
!		            3 = AVHRR-3 LAC/HRPT on NOAA-16
!                           4 = HIRS-2
!		            5 = HIRS-2I
!		            6 = HIRS-3 NOAA-K
!                           7 = HIRS-3 NOAA-L,M
!		            8 = HIRS-4
!		            9 = SSU
!		           10 = MSU
!		           11 = AMSU-A
!		           12 = AMSU-B, AIRS, HSB
!		           13 = MHS
!                          14 = ATMS 5.2 DEG
!		           15 = ATMS 2.2 DEG
!                          16 = ATMS 1.1 DEG
!   fov                   - fov number
!   satellite_azimuth     - satellite azimuth angle (degrees)
!   lat                   - latitude of fov center
!   lon                   - longitude of fov center
!   testlat               - latitude of point to be tested
!   testlon               - longitude of point to be tested
!   expansion             - expansion factor.  must be 1.0 for accurate renderine,
!                           > 1.0 makes bigger ellipses, < 1.0 makes smaller ellipses.
!                           do not exceed 3.0.
!
! output argument list:
!   inside                - 0.0-1.0 relative antenna power if [testLat,testLon] is
!                           inside the FOV, 0 if outside.
!
! remarks:
!   there are several engineering checks to handle things like arcsin( x>1.0 or x<-1.0)
!   these things can happen because POES navigation uses an oblate spheroid earth, while here we are
!   using a spherical earth, so there is a small inconsistency in computing arc angles.
!
!   no provisions are made for spacecraft roll-pitch-yaw errors, which are presumed to be small 
!   (SC attitude is usually held to within 0.1 deg)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 use constants, only  : one, zero, half, two, pi, deg2rad, rad2deg

 implicit none

! Declare passed variables.
 integer(i_kind),intent(in)      :: instr    
 integer(i_kind),intent(in)      :: fov      
 real(r_kind),intent(in)         :: lat       
 real(r_kind),intent(in)         :: lon        
 real(r_kind),intent(in)         :: testlat    
 real(r_kind),intent(in)         :: testlon    
 real(r_kind),intent(in)         :: expansion  
 real(r_kind),intent(in)         :: satellite_azimuth
 real(r_kind),intent(out)        :: inside 

! Declare local parameters.
 real(r_kind), parameter  :: r1 = one ! equatorial radius. work in angular distance, not km (otherwise r1=6371)
 real(r_kind), parameter  :: r2 = r1  ! assume spherical earth (otherwise r2 = polar radius)
 real(r_kind), dimension(maxinstr) :: fovangle  = (/ 0.0745_r_kind, 0.0745_r_kind, 0.0745_r_kind, &
                                                     1.22_r_kind,   1.40_r_kind,   1.40_r_kind, &
                                                     1.40_r_kind,   0.70_r_kind,   10.0_r_kind, &
                                                     7.5_r_kind,    3.3_r_kind,    1.1_r_kind,  &
                                                     1.1_r_kind,    5.2_r_kind,    2.2_r_kind, 1.1_r_kind/)
! Variables for AMSUA power determination
 real(r_kind), dimension(0:7,2) :: coeff=reshape(   &
      (/ -0.0266875_r_kind,   0.0503659_r_kind,  -1.004460_r_kind,     0.0108736_r_kind, &
         -0.00291774_r_kind, -0.00157707_r_kind, -0.000538326_r_kind,  6.86365e-005_r_kind,   &
         -0.0455019_r_kind,  -0.0811913_r_kind,  -0.903818_r_kind,     0.0451496_r_kind, &
         -0.0276690_r_kind,   0.00156701_r_kind,  0.000986008_r_kind,  -9.50111e-005_r_kind /), &
       (/ 8,2 /) )

! Declare local variables.
 integer(i_kind) :: i   ! loop counters
 real(r_kind)  :: dellon ! longitude difference from fov center to test location
 real(r_kind)  :: dellat ! latitude difference from fov center to test location
 real(r_kind)  :: fovanglesize
! these two are what gets compared to determine if test location is within fov.
 real(r_kind)  :: d      ! angular distance from fov center to test location
 real(r_kind)  :: r      ! angular distance from fov center to ellipse along d
 real(r_kind)  :: psi    ! angle from 3:00 on fov to line from fov center to test location
 real(r_kind)  :: psip   ! angle from latitude parallel through fov center to 
                         ! line from fov center to test location
! these are the flat earth variables
 real(r_kind)  :: distance_north  ! north angular distance from fov center to test location
 real(r_kind)  :: distance_east   ! east  angular distance from fov center to test location
 real(r_kind)  :: bearing_to_test ! same as psip
 real(r_kind)  :: bearing_to_test_deg ! in degrees
 real(r_kind)  :: x,y,px,py,p,rat
 real(r_kind)  :: sataz   ! satellite azimuth, used in computing psi

 fovanglesize = fovangle(instr)

! Get satellite az where we want it.
! 1st, convert +- to 0-360
 sataz = satellite_azimuth
 if(sataz < zero) sataz = 360.0_r_kind + sataz
! 2nd, Shift rotation direction
 sataz = mod((450.0_r_kind-sataz),360.0_r_kind)

! delta lat and lon
 dellat = (testlat - lat)*deg2rad
 dellon = testlon - lon
 if (dellon > 180._r_kind) dellon = dellon - 360._r_kind
 dellon = dellon*deg2rad

! distance north and east in degrees
 distance_north =  r1*dellat
 distance_east  =  r2*cos(lat*deg2rad)*dellon

! angle to the test point
 bearing_to_test = mod(atan2(distance_north,distance_east),2*pi )
 bearing_to_test_deg = bearing_to_test*rad2deg ! convert to degrees

! this is the arc distance to the test point
 d=two*asin(sqrt((sind((testlat-lat)/two))**2 +     &
                 cosd(testlat)*cosd(lat)*(sind((testlon-lon)/two))**2))
 d = d*rad2deg  ! convert to degrees

 psip = bearing_to_test_deg

 psi = (psip  - sataz)
 psi = psi*deg2rad ! convert to radians

! r is the angular distance from the fov center to the edge of the ellipse in degrees
 r = rmax(fov) * sqrt( (one - eccen(fov)**2)/(one - eccen(fov)**2 *cos(psi)**2) )

 inside = zero
 if(d<r)then 
   if(instr == 11)then  ! compute amsua power
    rat    = d / r * expansion * fovanglesize * half
    x      = rat * cos(psi)
    y      = rat * sin(psi)
    px     = coeff(0,1) + coeff(1,1)*x + coeff(2,1)*x**2 + coeff(3,1)*x**3 + coeff(4,1)*x**4 &
                                       + coeff(5,1)*x**5 + coeff(6,1)*x**6 + coeff(7,1)*x**7   
    py     = coeff(0,2) + coeff(1,2)*y + coeff(2,2)*y**2 + coeff(3,2)*y**3 + coeff(4,2)*y**4 &
                                       + coeff(5,2)*y**5 + coeff(6,2)*y**6 + coeff(7,2)*y**7   
    p      = -(px+py) ! power in dB (positive)
    p      = 10._r_kind**(-p*0.1_r_kind)  ! convert to fraction of max power
    inside = p  
   else
    inside = one
   endif
  endif

  return

 end subroutine inside_fov_crosstrk
 subroutine fov_check(fov,instr,valid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fov_check        ensure fov number is valid      
!   prgmmr: gayno           org: np23                date: 2009-01-05
!
! abstract: ensure fov number is valid for desired instrument
!
! program history log:
!   2009-01-05  gayno
!
! input argument list:
!   fov           - fov number   
!   instr         - instrument number
!  
! output argument list:
!   valid         - true if valid fov number
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
 implicit none

! Declare passed variables
 integer(i_kind), intent(in)     :: fov
 integer(i_kind), intent(in)     :: instr
 logical, intent(out)            :: valid

! test for fov in range
 valid=.true.
 if (fov <  1) then 
   write(6,*) "FOV_CHECK: ERROR, FOV NUMBER LESS THAN ONE "
   valid=.false.
   return
 endif

 if (fov > maxfov(instr) ) then 
   write(6,*) "FOV_CHECK: ERROR, FOV GREATER THAN ",maxfov(instr), "FOR INSTRUMENT ", instr
   valid=.false.
   return
 endif

 return
 end subroutine fov_check
 end module calc_fov_crosstrk
