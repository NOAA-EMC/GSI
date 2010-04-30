   module ncepgfs_ghg
!$$$ module documentation block
!
! module ncepgfs_ghg
!   prgmmr: hou                              date: 2010-3-31
!
! abstract: reading ncep gfs/cfs green house gases and/or other
!           types of radiative active rare gases distributions.
!
! program history log:
!   2010-03-31 hou
!   2010-04-26 kistler - simplified co2 input file for ico2=2
!
! subroutines included:
!   sub read_gfsco2        - read ncep gfs historical/prescribed co2
!                            gas data and map it to grid
!
!
!   unit used for radiative active gases:
!      co2   : volume mixing ratio (ppm)
!      n2o   : volume mixing ratio (ppm)
!      ch4   : volume mixing ratio (ppm)
!      o2    : volume mixing ratio (ppm)
!      co    : volume mixing ratio (ppm)
!      cfc11 : volume mixing ratio (ppm)
!      cfc12 : volume mixing ratio (ppm)
!      cfc22 : volume mixing ratio (ppm)
!      ccl4  : volume mixing ratio (ppm)
!      cfc113: volume mixing ratio (ppm)
!
! atributes:
!   language: f90
!   machine:  ibm
!
!$$$ end document block

   use kinds,      only : r_kind, i_kind
   use constants,  only : pi, rad2deg, one

   implicit   none

   private

!  ---  default parameter constants
   integer(i_kind), parameter :: nmxlon_def = 24   ! default input co2 data lon points
   integer(i_kind), parameter :: nmxlat_def = 12   ! default input co2 data lat points
   integer(i_kind), parameter :: minyear    = 1957 ! earlist year 2-d co2 data available

   real(r_kind), parameter :: resco2_def = 15.0_r_kind    ! horiz res in degree
   real(r_kind), parameter :: prsco2     = 78.8_r_kind    ! pres lim for 2-d co2 (cb)

!  ---  parameter constants for gas volume mixing ratioes in ppm (1.e-6 p/p)
!  real(r_kind), parameter :: co2vmr_def = 350.0_r_kind
   real(r_kind), parameter :: co2vmr_def = 390.0_r_kind
	public:: co2vmr_def
!  real(r_kind), parameter :: n2ovmr_def = 0.31_r_kind
!  real(r_kind), parameter :: ch4vmr_def = 1.50_r_kind
!  real(r_kind), parameter :: o2vmr_def  = 209.0e3_r_kind
!  real(r_kind), parameter :: covmr_def  = 1.50e-2_r_kind
!  real(r_kind), parameter :: f11vmr_def = 3.520e-4_r_kind
!  real(r_kind), parameter :: f12vmr_def = 6.358e-4_r_kind
!  real(r_kind), parameter :: f22vmr_def = 1.500e-4_r_kind
!  real(r_kind), parameter :: cl4vmr_def = 1.397e-4_r_kind
!  real(r_kind), parameter :: f113vmr_def= 8.2000e-5_r_kind

!  ---  co2 2-d monthly data and global mean from observed data
   real(r_kind), allocatable :: co2vmr_sav(:,:)
   real(r_kind), save        :: co2_glb = co2vmr_def

   integer(i_kind), save :: kyrsav  = 0       ! year of data saved 
   integer(i_kind), save :: kmonsav = 0       ! month of data saved
   integer(i_kind), save :: nmxlon  = nmxlon_def
   integer(i_kind), save :: nmxlat  = nmxlat_def
   integer(i_kind), save :: resco2  = resco2_def

!  ---  public invokable subroutines

   public  read_gfsco2                 ! read global co2 data


! ===========
   contains
! ===========

   subroutine read_gfsco2  &
!  ---  inputs:
     ( iyear, month, ico2, rlat, prsi, nlat, nlon, nlev, mype,  &
!  ---  outputs:
       atmco2 )

!$$$  subprogram documentation block
!
! subprogram:    read_gfsco2       read from gfs historical co2 data
!                                  set, convert to model grid
!
!   prgmmr:  hou                                     date: 2010-03-31
!
! abstract: set up green house gase co2 profile by reading global
!           historical co2 monthly 2-d data files, convert to grid
!
! program history log:
!   2003-05-xx  hou     created the original gfs version
!   2010-03-31  hou     modified for gsi application
!   2010-04-26  kistler - simplified co2 input file for ico2=2
!
!   input argument list:
!     iyear     - integer, year for the requested data
!     month     - integer, month of the year
!     ico2      - integer, input data source control flag
!                 =0: use prescribed global mean co2 value
!                 =1: use observed co2 yearly global annual mean value
!                 =2: use observed co2 monthly data with 2-d variation
!     rlat(nlat)- real, grid latitude in radians
!     prsi(nlat,nlon,nlev+1)
!               - real, grid pressure interface in kPa (cb)
!     nlat      - integer, number of latitude points
!     nlon      - integer, number of longitude points
!     nlev      - integer, number of vertical layers
!     mype      - integer, mpi task id
!
!   output argument list:
!     atmco2(nlat,nlon,nlev)
!               - real, co2 volume mixing ratio in ppm
!
!$$$

!  ---  declare passed variables - input:
   integer(i_kind),                      intent(in   ) :: iyear
   integer(i_kind),                      intent(in   ) :: month
   integer(i_kind),                      intent(in   ) :: ico2
   integer(i_kind),                      intent(in   ) :: nlat
   integer(i_kind),                      intent(in   ) :: nlon
   integer(i_kind),                      intent(in   ) :: nlev
   integer(i_kind),                      intent(in   ) :: mype

   real(r_kind), dimension(:),           intent(in   ) :: rlat
   real(r_kind), dimension(:,:,:),       intent(in   ) :: prsi

!  ---  declare passed variables - output:
   real(r_kind), dimension(:,:,:),       intent(out  ) :: atmco2

!  ---  declare local variables:
   real(r_kind)              :: co2g1, co2g2, dlon, dlat, rres

   integer(i_kind) :: iyr, imo, iyr1, iyr2, jyr, ierr
   integer(i_kind) :: i, j, k, ilon, ilat, ires
   integer(i_kind) :: luco2 = 43         ! data file unit, may be as an input param

   logical         :: file_exist
   character(len=100):: cline = ' '
   character(len=8)  :: cform = '(24f7.2)'      ! data format (nmxlon*f7.2)
   Character(len=20) :: cfile  = 'global_co2_data.txt'

!===>  ...  begin here

   if ( ico2 < 0 .or. ico2 > 2 ) then
      write(6,*) ' ERROR!!! ICO2 value out of range, ICO2 =',ico2
      write(6,*) ' Stopped in subprogram read_gfsco2'
      call stop2(332) 
   endif

   if ( ico2 == 0 ) then
!  --- ...  use prescribed global mean co2 data

      do k = 1, nlev
         do j = 1, nlon
            do i = 1, nlat
               atmco2(i,j,k) = co2vmr_def
            enddo
         enddo
      enddo

      if ( mype == 0 ) then
         write(6,*) ' - Using prescribed co2 global mean value=',co2vmr_def
      endif

      return

!  --- ...  auto select co2 data table for required month and year

   else if ( ico2 == 1 ) then
		if ( mype == 0 ) then
			write(6,*) '  ico2 == 1 not valid '
			write(6,*) '   *** Stopped in subroutine read_gfsco2 !!'
		endif
      call stop2(332) 

   else if ( ico2 == 2 ) then
   
!  --- ...  set up input data file name

      if ( mype == 0 ) then
         write(6,*) ' - Using  Co2 Data file ',cfile
      endif

!  --- ... check to see if requested co2 data file existed

      inquire (file=cfile, exist=file_exist)
      if ( .not. file_exist ) then
			if ( mype == 0 ) then
				write(6,*) '   Can not find co2 data source file'
				write(6,*) ' *** Stopped in subroutine read_gfsco2 !!'
			endif
      	call stop2(332) 

      endif   ! end if_file_exist_block

!  --- ...  read in co2 2-d data for the requested month

      open (luco2,file=cfile,form='formatted',status='old',iostat=ierr)
		if (ierr /= 0) then
			if ( mype == 0 ) then
				write(6,*) '   error opening file = '//cfile
				write(6,*) '   *** Stopped in subroutine read_gfsco2 !!'
			endif
      	call stop2(332) 
		endif
      rewind luco2
      read (luco2, 36,iostat=ierr) iyr, nmxlon, nmxlat, ires, co2g1
 36   format(i4,t25,2i4,t58,i3,t99,f7.2)
		if (ierr /= 0) then
			if ( mype == 0 ) then
				write(6,*) '   error reading  file = '//cfile
				write(6,*) '   *** Stopped in subroutine read_gfsco2 !!'
			endif
			call stop2(332)
		endif

      resco2 = ires
      co2_glb = co2g1
      if ( mype == 0 ) then
         write(6,*) '   Opened co2 data file: ',cfile
         write(6,*) '    YEAR=',iyr,' NLON, NLAT=',nmxlon,nmxlat,  &
                    ' RES=',ires,' Global annual mean=',co2_glb
      endif

      if ( .not. allocated(co2vmr_sav) ) then
         allocate ( co2vmr_sav(nmxlon,nmxlat) )
      endif

!  --- ...  set up input data format
      write(cform(2:3),'(i2)') nmxlon

      do imo = 1, month
         read (luco2,cform) co2vmr_sav
      enddo

      do j = 1, nmxlat
         do i = 1, nmxlon
            co2vmr_sav(i,j) = co2vmr_sav(i,j)
         enddo
      enddo

      if ( mype == 0 ) then
         write(6,*) '   CHECK: Sample of selected months of CO2 ',  &
                    'data used for year, month:',iyear,month
         write(6,*) co2vmr_sav(1,:)
      endif

      close ( luco2 )

!  --- ...  put data to grid and vertical layers

      rres = one / resco2
      do j = 1, nlon
         dlon = 360.0_r_kind * float(j-1)/float(nlon)
         ilon = min( nmxlon, int(dlon*rres) + 1 )

         do i = 1, nlat
            dlat = 90.0_r_kind - rlat(i)*rad2deg
            ilat = min( nmxlat, int(dlat*rres) + 1 )

            do k = 1, nlev
               if ( prsi(i,j,k+1) >= prsco2 ) then
                  atmco2(i,j,k) = co2vmr_sav(ilon,ilat)
               else
                  atmco2(i,j,k) = co2_glb
               endif
            enddo
         enddo
      enddo

   endif  ! end if_ico2_block

   return
   end subroutine read_gfsco2

   end module ncepgfs_ghg
