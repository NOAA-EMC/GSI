!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! module init_io 
!!        contains subroutines for initializing background 
!!        and analysis files IO
!! Original: 2019-09-18   martin   - original module
!!           2019-09-27   martin   - added support for netCDF IO
!!           2019-10-24   martin   - support NEMSIO analysis write
!!           2020-01-21   martin   - parallel IO support added
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module init_io
  use nemsio_module
  implicit none

  private

  public :: init_read_bg
  public :: init_write_anl

contains
  subroutine init_read_bg
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine read_bg
  !            read first guess file
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcst_file, fcstncfile, &
                                  idate6, nlon, nlat, nlev, &
                                  use_nemsio_anl, lats, lons, vcoord, &
                                  mype, npes, levpe
    use module_ncio, only: Dimension, open_dataset, get_dim,&
                                  get_idate_from_time_units, &
                                  read_vardata, read_attribute
    implicit none
    ! variables local to this subroutine
    integer ::k,kk,ilev
    type(Dimension) :: ncdim    
    real, allocatable, dimension(:) :: ak, bk
    real, allocatable, dimension(:,:) :: tmp2d

    ! open the background file
    fcstncfile = open_dataset(fcst_file, paropen=.true.)
    ! get dimensions
    ncdim = get_dim(fcstncfile,'grid_xt'); nlon=ncdim%len
    ncdim = get_dim(fcstncfile,'grid_yt'); nlat=ncdim%len
    ncdim = get_dim(fcstncfile,'pfull'); nlev=ncdim%len
    ! get valid time
    idate6 = get_idate_from_time_units(fcstncfile)
    if (mype==0) then
      write(6,*) 'Background initialization date=', idate6
      write(6,*) 'nlon=', nlon
      write(6,*) 'nlat=', nlat
      write(6,*) 'nlev=', nlev
    end if
    ! determine which PEs will be used for each model level
    ilev = 0
    allocate(levpe(nlev))
    do k=1,nlev
      levpe(k) = ilev
      ilev = ilev + 1
      if (ilev == npes) ilev = 0
    end do

    ! need to extract lat, lon, vcoord info
    if (use_nemsio_anl) then
      allocate(lats(nlon*nlat),lons(nlon*nlat))
      call read_vardata(fcstncfile, 'lon', tmp2d)
      lons = reshape(tmp2d,(/nlon*nlat/))
      call read_vardata(fcstncfile, 'lat', tmp2d)
      lats = reshape(tmp2d,(/nlon*nlat/))
      call read_attribute(fcstncfile, 'bk', bk)
      call read_attribute(fcstncfile, 'ak', ak)
      allocate(vcoord(nlev+1,3,2))
      do k=1,nlev+1
        kk = nlev+2-k
        vcoord(k,1,1) = ak(kk)
        vcoord(k,2,1) = bk(kk)
        vcoord(k,3,1) = 0
      end do
    end if

  end subroutine init_read_bg

  subroutine init_write_anl
    use vars_calc_analysis, only: anal_file, anlfile, jdate, idate6, jdate6,&
                                  fhr, nfhour, nfminute, nfsecondn, nfsecondd,&
                                  use_nemsio_anl, anlncfile, fcstncfile,&
                                  nlon, nlat, nlev, lats, lons, vcoord, mype
    use module_ncio, only: create_dataset, get_time_units_from_idate,&
                                  write_vardata, write_attribute
    use netcdf, only: nf90_max_name
    use nemsio_module
    implicit none
    ! Declare externals
    external :: w3movdat
    ! variables local to this subroutine
    integer :: iret, nrecs
    real, dimension(5) :: fha
    integer, dimension(8) :: ida, jda 
    character(len=nf90_max_name) :: time_units
    real,allocatable,dimension(:) :: fhour
    ! nemsio variables needed
    character(nemsio_charkind), allocatable :: recname(:), reclevtyp(:), variname(:)
    character(nemsio_charkind), allocatable :: varcname(:), varcval(:)
    integer(nemsio_intkind), allocatable :: reclev(:), varival(:)
    character(len=7) :: iovars_recs(16)
    character(len=9) ::  iovars_levs(16)
    data iovars_recs /  'ugrd   ', 'vgrd   ', 'dzdt   ', 'delz   ', 'tmp    ',&
                        'dpres  ', 'spfh   ', 'clwmr  ', 'rwmr   ', 'icmr   ',&
                        'snmr   ', 'grle   ', 'o3mr   ', 'cld_amt', 'pres   ',&
                        'hgt    '/
    data iovars_levs /  'mid layer', 'mid layer', 'mid layer', 'mid layer',&
                        'mid layer', 'mid layer', 'mid layer', 'mid layer',&
                        'mid layer', 'mid layer', 'mid layer', 'mid layer',&
                        'mid layer', 'mid layer', 'sfc      ', 'sfc      '/
    integer :: irec, ii 

    ! modify dates for analysis file
    ida(:) = 0
    jda(:) = 0
    fha(:) = 0
    fha(2) = fhr  

    ida(1)=idate6(1)
    ida(2)=idate6(2)
    ida(3)=idate6(3)
    ida(4)=0
    ida(5)=idate6(4)
    ida(6)=idate6(5)
    call w3movdat(fha,ida,jda)    
    nfhour=0
    nfminute=0
    nfsecondn=0
    nfsecondd=1
    jdate6(1)=jda(1)
    jdate6(2)=jda(2)
    jdate6(3)=jda(3)
    jdate6(4)=jda(5)
    jdate6(5)=idate6(5)
    jdate6(6)=0
    if (mype==0) write(6,*) 'Analysis valid date=', jdate6
    jdate(1:6)=jdate6
    jdate(7)=1

    if (use_nemsio_anl) then
       ! compute nrecs
       nrecs = (nlev*14) + 2 
       ! get recnames, etc.
       allocate(recname(nrecs),reclevtyp(nrecs),reclev(nrecs))
       ii=1
       do irec=1,nrecs-2
         recname(irec) = iovars_recs(ii)
         reclevtyp(irec) = iovars_levs(ii)
         reclev(irec) = modulo(irec,nlev)
         if (modulo(irec,nlev)==0) then
           reclev(irec) = nlev
           ii=ii+1 
         end if
       end do 
       recname(nrecs-1) = iovars_recs(15)
       reclevtyp(nrecs-1) = iovars_levs(15)
       reclev(nrecs-1) = 1
       recname(nrecs) = iovars_recs(16)
       reclevtyp(nrecs) = iovars_levs(16)
       reclev(nrecs) = 1
       
       ! more metadata
       allocate(variname(3), varival(3), varcname(5), varcval(5))
       variname(1) = 'ncnsto'
       variname(2) = 'im'
       variname(3) = 'jm'
       varival(1) = 9
       varival(2) = nlon
       varival(3) = nlat
       varcname(1) = 'hydrostatic'
       varcname(2) = 'source'
       varcname(3) = 'grid'
       varcname(4) = 'y-direction'
       varcname(5) = 'z-direction'
       varcval(1) = 'non-hydrostatic'
       varcval(2) = 'FV3GFS'
       varcval(3) = 'gaussian'
       varcval(4) = 'north2south'
       varcval(5) = 'bottom2top'
       ! open the NEMSIO output file for writing
       call nemsio_init(iret)
       if (iret /= 0) then
         write(*,*) 'Error with NEMSIO Init, iret=',iret
         stop
       end if
       call nemsio_open(anlfile, trim(anal_file), 'write', iret=iret, &
                       modelname="FV3GFS", gdatatype="bin4", &
                       idate=jdate, nfhour=nfhour, nfminute=nfminute, &
                       nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
                       dimx=nlon,dimy=nlat,dimz=nlev,nrec=nrecs, &
                       nmeta=8, ntrac=9, ncldt=5, idvc=2, idsl=1, & ! see below
                       idvm=1, idrt=4, nsoil=4, jcap=-9999,& ! hard coded to match FV3GFS history files
                       nmetavari=3, nmetavarr=0, nmetavarl=0, nmetavarc=5, & ! more hard coded vars
                       nmetaaryi=0, nmetaaryr=0, nmetaaryl=0, nmetaaryc=0, &
                       extrameta=.true., varival=varival, variname=variname, &
                       varcname=varcname, varcval=varcval,&
                       nframe=0, recname=recname, reclevtyp=reclevtyp, &
                       reclev=reclev, lat=lats, lon=lons, vcoord=vcoord)
       if (iret /= 0) then
         write(*,*) 'Error with NEMSIO Write Open, iret=',iret
         stop
       end if
    else
       ! open the netCDF file for writing and copy everything
       anlncfile = create_dataset(anal_file, fcstncfile, paropen=.true.) 
       ! update the valid time info
       allocate(fhour(1))
       fhour = 0
       call write_vardata(anlncfile, 'time', fhour)
       time_units = get_time_units_from_idate(jdate6)
       call write_attribute(anlncfile, 'units', time_units, 'time')
    end if

  end subroutine init_write_anl

end module init_io
