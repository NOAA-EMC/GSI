subroutine read_aerosol(nread,ndata,nodata,jsatid,infile,gstime,lunout, &
           obstype,twind,sis,ithin,rmesh)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_aerosol                    read aerosol data
!   prgmmr: hchuang     org: np23                date: 2009-01-26
!
! abstract:  This routine reads MODIS aerosol total column AOD observations.
!            ONLY total column values are read in.  The routine has
!            the ability to read both IEEE and BUFR format MODIS
!            aerosol data files.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2009-04-08  Huang   - modified from read_ozone to read in MODIS AEROSOL data
!
!   input argument list:
!     obstype  - observation type to process
!     jsatid   - satellite id to read
!     infile   - unit from which to read aerosol data
!     gstime   - analysis time in minutes from reference date
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!
!   output argument list:
!     nread    - number of modis aerosol observations read
!     ndata    - number of modis aerosol profiles retained for further processing
!     nodata   - number of modis aerosol observations retained for further processing
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  IBM AIX Cirrus
!
!$$$
  use kinds,     only: r_kind, r_double, i_kind
  use gridmod,   only: nlat, nlon, regional, tll2xy, rlats, rlons
  use constants, only: deg2rad, zero, rad2deg, r60inv
  use obsmod,    only: iadate, nlaero
  use gsi_4dvar, only: l4dvar,iwinbgn,winlen
  implicit none
!
! Declare local parameters
  real(r_kind), parameter :: r6   = 6.0_r_kind
  real(r_kind), parameter :: r360 = 360.0_r_kind
!
! Declare passed variables
!
  character(10),   intent(in)    :: obstype, infile, jsatid
  character(20),   intent(in)    :: sis
  integer(i_kind), intent(in)    :: lunout, ithin
  integer(i_kind), intent(inout) :: nread
  integer(i_kind), intent(inout) :: ndata, nodata
  real(r_kind),    intent(in)    :: gstime, twind, rmesh
!
! Declare local variables
!
  logical :: outside, iuse
  
  character (len= 8) :: subset
  character (len=10) :: date

  integer(i_kind) :: maxobs, naerodat
  integer(i_kind) :: idate, jdate, ksatid, kk, iy, iret, im, ihh, idd
  integer(i_kind) :: lunin = 10
  integer(i_kind) :: nmind, i
  integer(i_kind) :: imin, isec
  integer(i_kind) :: nmrecs, k, ilat, ilon, nreal, nchanl
  integer(i_kind) :: kidsat
  integer(i_kind) :: JULIAN, IDAYYR, IDAYWK
  integer(i_kind), dimension(5) :: idate5
!
!| NC008041 | SAID    AEROSOL  CLONH   CLATH YYMMDD  HHMMSS  SOZA  SOLAZI       |
!| NC008041 | SCATTA  OPTD  AEROTP                                              |
!
!| YYMMDD   | YEAR    MNTH    DAYS                                              |
!|          |                                                                   |
!| HHMMSS   | HOUR    MINU    SECO                                              |
!
!    SAID    Satellite identifier code table (eg, 783 == 'TERRA')
!    AEROSOL Aerosol Optical Depth (AOD) source code table (eg, 5 == 'AATSR' )
!    YEAR    Year                               
!    MNTH    Month                              
!    DAYS    Day                                
!    HOUR    Hour                               
!    MINU    Minute                             
!    SECO    Second                             
!    CLATH   Latitude (high accuracy)     degree (5 decimal precision)
!    CLONH   Longitude (high accuracy)    degree (5 decimal precision)
!    SOLAZI  Solar azimuth                degree (2 decimal precision)
!    SOZA    Solar zenith angle           degree (2 decimal precision)
!    OPTD    Optical depth                numeric
!    SCATTA  Scattering angle             degree (2 decimal precsion)
!    AEROTP  Aerosol type land            code table (eg, 1 == 'DUST', 2 == 'SULFATE')
!
!    0-15-195 - AEROTP (Aerosol land type)
!
!    CODE  DESCRIPTION
!    ====  ===========
!    0     Mixed
!    1     Dust
!    2     Sulfate
!    3     Smoke
!    4     Heavy absorbing smoke
!    5-14  Reserved
!    15    Missing value
!
  character (len= 4) :: aerostr  = 'OPTD'
  character (len=53) :: aerogstr = &
      'SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI'

  integer(i_kind) :: itx, itt

  real(r_kind) :: tdiff, sstime, slons, slats, dlon, dlat, t4dv, toq, poq, timedif, crit1, dist1
  real(r_kind) :: slons0, slats0, rsat, toto3, solzen, azimuth, dlat_earth, dlon_earth

  real(r_kind), allocatable, dimension(:)   :: paero
  real(r_kind), allocatable, dimension(:,:) :: aeroout
  real(r_double), dimension( 10) :: hdraerog
  real(r_double)                 :: totaod

!**************************************************************************
! Set constants.  Initialize variables
  rsat=999._r_kind
  maxobs=1e6
  ! output position of LON and LAT
  ilon=3
  ilat=4

  if ( obstype == 'modis' ) then
!
     open(lunin,file=infile,form='unformatted')
     nmrecs=0
     call openbf(lunin,'IN',lunin)
     call datelen(10)
     call readmg(lunin,subset,idate,iret)

     if ( iret == 0 ) then
!
        if (subset == 'NC008041') then
           write(6,*)'READ_AEROSOL: MODIS data type, subset = ',subset
           !          Set dependent variables and allocate arrays
           nreal=9
           nlaero=0
           nchanl=1
           naerodat=nreal+nchanl
           allocate (aeroout(naerodat,maxobs))
           allocate (paero(nlaero+1))

           iy = 0
           im = 0
           idd= 0
           ihh= 0
           write(date,'( i10)') idate
           read (date,'(i4,3i2)') iy,im,idd,ihh
           write(6,'(''READ_AEROSOL: aerosol bufr file '',a,''  date is '',i4,4i2.2,a)')trim(infile),iy,im,idd,ihh

           read_modis: do
              call readsb(lunin,iret)
              if (iret/=0) then
                 call readmg(lunin,subset,jdate,iret)
                 if (iret/=0) exit read_modis
                 cycle read_modis
              endif
     
              !    extract header information
              call ufbint(lunin,hdraerog,10,1,iret,aerogstr)
              rsat = hdraerog(1); ksatid=rsat

              if ( jsatid == 'terra' ) kidsat = 783
              if ( jsatid == 'aqua'  ) kidsat = 784

              if ( ksatid /= kidsat  ) cycle read_modis

              nmrecs=nmrecs+nlaero+1
    
              !    Convert observation location to radians
              slats0= hdraerog(2)
              slons0= hdraerog(3)
              if(slons0< zero) slons0=slons0+r360
              if(slons0>=r360) slons0=slons0-r360
              dlat_earth = slats0 * deg2rad
              dlon_earth = slons0 * deg2rad

              if(regional)then
                 call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
                 if(outside) cycle read_modis
              else
                 dlat = dlat_earth
                 dlon = dlon_earth
                 call grdcrd(dlat,1,rlats,nlat,1)
                 call grdcrd(dlon,1,rlons,nlon,1)
              endif

              solzen  = hdraerog(9)
              azimuth = hdraerog(10)

              !    Convert observation time to relative time
              idate5(1) = hdraerog(4)  !year
              idate5(2) = hdraerog(5)  !month
              idate5(3) = hdraerog(6)  !day
              idate5(4) = hdraerog(7)  !hour
              idate5(5) = hdraerog(8)  !minute

              !    extract total column aod 1 value 'OPTD' as defined in aerostr
              call ufbint(lunin,totaod,1,1,iret,aerostr)

              call w3fs21(idate5,nmind)
              t4dv=real((nmind-iwinbgn),r_kind)*r60inv
              if (l4dvar) then
                 if(t4dv<zero .OR. t4dv>winlen) cycle read_modis
              else
                 sstime=real(nmind,r_kind)
                 tdiff=(sstime-gstime)*r60inv
                 if ( abs(tdiff) > twind ) cycle read_modis
              end if

              if ( totaod > 1.0e+10_r_double ) cycle read_modis
!
              toq=zero

              ndata=min(ndata+1,maxobs)
              nodata=nodata + 1                    ! only total AOD

              aeroout( 1,ndata) = rsat
              aeroout( 2,ndata) = tdiff
              aeroout( 3,ndata) = dlon               ! grid relative longitude
              aeroout( 4,ndata) = dlat               ! grid relative latitude
              aeroout( 5,ndata) = dlon_earth*rad2deg ! earth relative longitude (degrees)
              aeroout( 6,ndata) = dlat_earth*rad2deg ! earth relative latitude (degrees)
              aeroout( 7,ndata) = toq                ! total column AOD error flag
              aeroout( 8,ndata) = solzen             ! solar zenith angle
              aeroout( 9,ndata) = azimuth            ! solar azimuth angle
              aeroout(10,ndata) = totaod
       
           end do read_modis

           ! Write header record and data to output file for further processing
           write(lunout) obstype, sis, nreal, nchanl, ilat, ilon
           write(lunout) ((aeroout(k,i), k=1,naerodat), i=1,ndata)

           nread = nmrecs

           ! Deallocate local arrays
           deallocate(aeroout,paero)

           ! End of MODIS bufr block
           call closbf(lunin)
           close(lunin)
        else       ! subset /= NC008041
           write(6,*)'READ_AEROSOL:  *** WARNING: unknown aerosol data type, subset=',subset
           write(6,*)' infile=',infile, ', lunin=',lunin, ', obstype=',obstype,', jsatid=',jsatid
           write(6,*)' SKIP PROCESSING OF THIS MODIS FILE'
        endif
     
     else          ! read subset iret /= 0
        write(6,*)'READ_AEROSOL:  *** WARNING: read subset error, obstype=',obstype,', iret=',iret
     end if
  else             ! obstype /= 'modis'
     write(6,*)'READ_AEROSOL:  *** WARNING: unknown aerosol input type, obstype=',obstype
  endif

end subroutine read_aerosol
