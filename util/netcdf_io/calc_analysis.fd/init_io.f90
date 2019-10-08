!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! module init_io 
!!        contains subroutines for initializing background 
!!        and analysis files IO
!! Original: 2019-09-18   martin   - original module
!!           2019-09-27   martin   - added support for netCDF IO
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
    use vars_calc_analysis, only: fcst_file, &
                                  fcstfile, anlfile, fcstncfile, &
                                  idate, idate6, nrec, nfday, nfhour, nfminute, nfsecondn, &
                                  nfsecondd, nlon, nlat, nlev, &
                                  recname, reclevtyp, &
                                  reclev, recname, reclevtyp, use_nemsio
    use module_fv3gfs_ncio, only: Dimension, open_dataset, get_dim,&
                                  get_idate_from_time_units
    implicit none
    ! variables local to this subroutine
    integer :: iret
    type(Dimension) :: ncdim    

    if (use_nemsio) then ! NEMSIO
       ! initialize the NEMSIO library
       call nemsio_init(iret=iret)  
       if (iret /= 0) then
         write(*,*) 'Error with NEMSIO Read Init, iret=',iret
         stop
       end if
       ! open the NEMSIO input file
       call nemsio_open(fcstfile, trim(fcst_file), 'read', iret=iret)
       if (iret /= 0) then
         write(*,*) 'Error with NEMSIO Read Open, iret=',iret
         stop
       end if
       ! get dimensions, etc so that arrays can be allocated
       call nemsio_getfilehead(fcstfile, iret=iret, idate=idate, nrec=nrec, nfday=nfday,&
                               nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn,&
                               nfsecondd=nfsecondd, dimx=nlon, dimy=nlat, dimz=nlev)

       write(6,*) 'Background initialization date=', idate
       write(6,*) 'nlon=', nlon
       write(6,*) 'nlat=', nlat
       write(6,*) 'nlev=', nlev

       !! get varnames
       allocate(recname(nrec), reclevtyp(nrec), reclev(nrec))
       call nemsio_getfilehead(fcstfile, iret=iret, recname=recname,&
                               reclevtyp=reclevtyp, reclev=reclev)

       ! copy input header info to output header info
       anlfile = fcstfile
    else ! netCDF
       ! open the background file
       fcstncfile = open_dataset(fcst_file)
       ! get dimensions
       ncdim = get_dim(fcstncfile,'grid_xt'); nlon=ncdim%len
       ncdim = get_dim(fcstncfile,'grid_yt'); nlat=ncdim%len
       ncdim = get_dim(fcstncfile,'pfull'); nlev=ncdim%len
       ! get valid time
       idate6 = get_idate_from_time_units(fcstncfile)
       write(6,*) 'Background initialization date=', idate6
       write(6,*) 'nlon=', nlon
       write(6,*) 'nlat=', nlat
       write(6,*) 'nlev=', nlev
       
    end if

  end subroutine init_read_bg

  subroutine init_write_anl
    use vars_calc_analysis, only: anal_file, anlfile, idate, jdate, idate6, jdate6,&
                                  nhr_assim, nfhour, nfminute, nfsecondn, nfsecondd,&
                                  use_nemsio, anlncfile, fcstncfile
    use module_fv3gfs_ncio, only: create_dataset, get_time_units_from_idate,&
                                  write_vardata, write_attribute
    use netcdf, only: nf90_max_name
    implicit none
    ! variables local to this subroutine
    integer :: iret
    real, dimension(5) :: fha
    integer, dimension(8) :: ida, jda 
    character(len=nf90_max_name) :: time_units
    real,allocatable,dimension(:) :: fhour

    ! modify dates for analysis file
    ida(:) = 0
    jda(:) = 0
    fha(:) = 0
    fha(2) = nhr_assim  

    if (use_nemsio) then
       ida(1)=idate(1)
       ida(2)=idate(2)
       ida(3)=idate(3)
       ida(4)=0
       ida(5)=idate(4)
       ida(6)=idate(5)
       call w3movdat(fha,ida,jda)    
       jdate(1)=jda(1)
       jdate(2)=jda(2)
       jdate(3)=jda(3)
       jdate(4)=jda(5)
       jdate(5)=idate(5)
       jdate(6)=0
       jdate(7)=idate(7)
       nfhour=0
       nfminute=0
       nfsecondn=0
       nfsecondd=100
       write(6,*) 'Analysis valid date=', jdate
    else
       ida(1)=idate6(1)
       ida(2)=idate6(2)
       ida(3)=idate6(3)
       ida(4)=0
       ida(5)=idate6(4)
       ida(6)=idate6(5)
       call w3movdat(fha,ida,jda)    
       jdate6(1)=jda(1)
       jdate6(2)=jda(2)
       jdate6(3)=jda(3)
       jdate6(4)=jda(5)
       jdate6(5)=idate6(5)
       jdate6(6)=0
       write(6,*) 'Analysis valid date=', jdate6
    end if

    if (use_nemsio) then
       ! open the NEMSIO output file for writing
       call nemsio_open(anlfile, trim(anal_file), 'write', iret=iret, &
                       idate=jdate, nfhour=nfhour, nfminute=nfminute, &
                       nfsecondn=nfsecondn, nfsecondd=nfsecondd)
       if (iret /= 0) then
         write(*,*) 'Error with NEMSIO Write Open, iret=',iret
         stop
       end if
    else
       ! open the netCDF file for writing and copy everything
       anlncfile = create_dataset(anal_file, fcstncfile) 
       ! update the valid time info
       allocate(fhour(1))
       fhour = 0
       call write_vardata(anlncfile, 'time', fhour)
       time_units = get_time_units_from_idate(jdate6)
       call write_attribute(anlncfile, 'units', time_units, 'time')
    end if

  end subroutine init_write_anl

end module init_io
