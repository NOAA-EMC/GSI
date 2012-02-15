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
!   real(r_kind), parameter :: co2vmr_def = 350.0_r_kind
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
     ( iyear, month, idd,ico2, xlats,lat2, lon2, nlev, mype,  &
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
!   2003-05-xx  hou      created the original gfs version
!   2010-03-31  hou      modified for gsi application
!   2010-04-26  kistler  simplified co2 input file for ico2=2
!   2011-03-15  yang     modify to use a timp dependent monthly zonal mean co2 input
!   2011-12-12  yang     modify interpolation loop order 
 
!
!   input argument list:
!     iyear     - integer, year for the requested data
!     month     - integer, month of the year
!     idd       - integer, day of the month 
!     ico2      - integer, input data source control flag
!                 =0: use prescribed global mean co2 value
!                 =1: use observed co2 yearly global annual mean value
!                 =2: use observed co2 monthly data with 2-d variation
!     xlats(lat2)- real, grid latitude in radians
!     lat2      - integer, number of latitude points in subdomain
!     lon2      - integer, number of longitude points in subdomain
!     nlev      - integer, number of vertical layers
!     mype      - integer, mpi task id
!
!   output argument list:
!     atmco2(lat2,lon2,nlev)
!               - real, co2 volume mixing ratio in ppm
!
!$$$

!  ---  declare passed variables - input:
   integer(i_kind),                      intent(in   ) :: iyear
   integer(i_kind),                      intent(in   ) :: month
   integer(i_kind),                      intent(in   ) :: idd
   integer(i_kind),                      intent(in   ) :: ico2
   integer(i_kind),                      intent(in   ) :: lat2
   integer(i_kind),                      intent(in   ) :: lon2
   integer(i_kind),                      intent(in   ) :: nlev
   integer(i_kind),                      intent(in   ) :: mype
   real(r_kind), dimension(:),           intent(in   ) :: xlats

!  ---  declare passed variables - output:
   real(r_kind), dimension(:,:,:),       intent(out  ) :: atmco2

!  ---  declare local variables:
   real(r_kind), allocatable, dimension(:)             :: xlatsdeg
   real(r_kind), allocatable, dimension(:,:,:)         :: co2_Tintrp
   real(r_kind), allocatable, dimension(:,:,:)         :: co2_sav1
   real(r_kind), allocatable, dimension(:,:,:)         :: co2_sav2
!  ---  latitudes in degree of input co2 data
   real(r_kind), allocatable, dimension(:)             :: rlats_co2
   real(r_kind)              :: co2diff
   real(r_kind)              :: co2rate
   real(r_kind)              :: co2g1, co2g2, dlon, dlat
   real(r_kind)              :: dydn, dyup, dyall
   integer(i_kind) :: iyr, imo, iyr1, iyr2, jyr, ierr
   integer(i_kind) :: i, j, k, ilon, ilat, ires
   integer(i_kind) :: ii,jj,kk
   integer(i_kind) :: luco2 = 43         ! data file unit, may be as an input param
   integer(i_kind) ::            ndpm(12)
   integer(i_kind) ::            ndmax
   logical         :: file_exist
   character(len=100):: cline = ' '
   character(len=8)  :: cform = '(24f7.2)'      ! data format (nmxlon*f7.2)
   Character(len=20) :: cfile  = 'global_co2_data.txt'

   data ndpm /31_i_kind, 28_i_kind, 31_i_kind, 30_i_kind, 31_i_kind, 30_i_kind, &
              31_i_kind, 31_i_kind, 30_i_kind, 31_i_kind, 30_i_kind, 31_i_kind/


! --- ... begin 

   allocate(xlatsdeg(lat2))

   if ( ico2 < 0 .or. ico2 > 2 ) then
      write(6,*) ' ERROR!!! ICO2 value out of range, ICO2 =',ico2
      write(6,*) ' Stopped in subprogram read_gfsco2'
      call stop2(332) 
   endif

   if ( ico2 == 0 ) then
!  --- ...  use prescribed global mean co2 data

      do k = 1, nlev
         do j = 1, lon2
            do i = 1, lat2
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

      if ( .not. allocated(rlats_co2) ) then
         allocate ( rlats_co2(nmxlat) )
      endif
      if ( .not. allocated(co2_sav1) ) then
         allocate ( co2_sav1(nmxlon,nmxlat,nlev) )
      endif
      if ( .not. allocated(co2_sav2) ) then
         allocate ( co2_sav2(nmxlon,nmxlat,nlev) )
      endif
      if ( .not. allocated(co2_Tintrp) ) then
         allocate ( co2_Tintrp(nmxlon,nmxlat,nlev) )
      endif
! --- ...
! --- ...  rlats: latitudes array  of input co2 data (in degree)

      read (luco2,37) (rlats_co2(j), j=1,nmxlat)
37    format (12f7.2)

!  --- ...  set up input data format
      write(cform(2:3),'(i2)') nmxlon

!  --- ...  put data to grid and vertical layers

      
! --- ...  convert xlats into degree
      do i = 1, lat2
         xlatsdeg(i)=xlats(i)*rad2deg
      enddo

! --- ... read 3-d data field starting from January of the year
      do imo = 1, month
         do k = 1, nlev
            do j=1,nmxlat
               read (luco2,cform) (co2_sav1(i,j,k), i=1,nmxlon)
            enddo
         enddo
      enddo
! --- ... save the next month data for interpolation
      do k = 1, nlev
         do j=1, nmxlat
            read (luco2,cform) (co2_sav2(i,j,k), i=1,nmxlon)
         enddo
         if ( mype == 0 ) then
            write(6,*) '   CHECK: at Month+1 CO2 data ',  &
                    'data used for year, month, level:',iyear,month,k
            write(6,*) co2_sav2(1,:,k)
         endif
      enddo
! To do linear interperlation from the two month data into a daily value
      ndmax=ndpm(month)
! For leap year February: ndmax=29
      if (month ==2 ) then
         if( mod(iyear,4) == 0_i_kind .and. iyear >= 1900_i_kind) ndmax= 29
      endif
      do k=1,nlev
         do j=1,nmxlat
            do i=1,nmxlon
               co2diff= co2_sav2(i,j,k)-co2_sav1(i,j,k)
               co2rate= co2diff/ndmax
               co2_Tintrp(i,j,k)= co2_sav1(i,j,k)+ co2rate*float(idd-1)
            enddo
         enddo
      enddo

! To interpolate the co2_Tintrp data into the subdomains
      do i = 1, lat2
! --- ... If the subdomain indexes are out of the coverage of the input co2
! --- ... or fall at the same lats, atmco2(i,j,k) is assinged by the value of 
! --- ... the nearest point of the input co2
         if (xlatsdeg(i) < rlats_co2(1)) then
            do k = 1, nlev
               do j=1,lon2
                  atmco2(i,j,k)= co2_Tintrp(1,1,k)
               enddo
            enddo
         endif
         if (xlatsdeg(i) >=  rlats_co2(nmxlat)) then
            do k = 1, nlev
               do j=1,lon2
                  atmco2(i,j,k)= co2_Tintrp(1,nmxlat,k)
               enddo
            enddo
         endif 
         do ii = 1, nmxlat-1
            if (xlatsdeg(i) >= rlats_co2(ii) .and. xlatsdeg(i) < rlats_co2(ii+1)) then
               dydn= xlatsdeg(i) - rlats_co2(ii) 
               dyup= rlats_co2(ii+1)-xlatsdeg(i)
               dyall=rlats_co2(ii+1)-rlats_co2(ii)
               dydn=dydn /dyall 
               dyup=1.0-dydn
               do k=1,nlev
                  do j=1,lon2
                     atmco2(i,j,k)= dydn*co2_Tintrp(1,ii+1,k)+ dyup*co2_Tintrp(1,ii,k)
                  enddo
               enddo
            endif
         enddo     ! end loop for ii
      enddo    ! end loop for nlev
      close (luco2 )
      if (allocated(xlatsdeg)) deallocate (xlatsdeg)
      if (allocated(rlats_co2)) deallocate (rlats_co2)
      if (allocated(co2_sav1)) deallocate (co2_sav1)
      if (allocated(co2_sav2)) deallocate (co2_sav2)
      if (allocated(co2_Tintrp)) deallocate (co2_Tintrp)
   endif  ! end if_ico2_block
   return
   end subroutine read_gfsco2

   end module ncepgfs_ghg
