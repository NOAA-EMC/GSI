!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! module nemsio_interface
!!        contains subroutines for reading NEMSIO background file
!!        and for writing out NEMSIO analysis files
!! Original: 2019-09-16   martin   - original module
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module nemsio_interface
  use nemsio_module
  implicit none
contains
  subroutine init_read_bg
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine read_bg
  !            read NEMSIO first guess file
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcst_file, &
                                  gesfile, anlfile, &
                                  idate, nrec, nfday, nfhour, nfminute, nfsecondn, &
                                  nfsecondd, nlon, nlat, nlev, nframe, nsoil, ntrac, &
                                  lats, lons, recname, reclevtyp, &
                                  reclev, recname, reclevtyp
    implicit none
    ! variables local to this subroutine
    integer :: iret
    
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
                            dimx=nlon, dimy=nlat, dimz=nlev)
    ! get lat/lon
    allocate(lats(nlon*nlat), lons(nlon*nlat))
    call nemsio_getfilehead(fcstfile, iret=iret, lat=lats, lon=lons)

    ! get varnames
    allocate(recname(nrec), reclevtyp(nrec), reclev(nrec))
    call nemsio_getfilehead(fcstfile, iret=iret, recname=recname,&
                            reclevtyp=reclevtyp, reclev=reclev)

    ! copy input header info to output header info
    anlfile = fcstfile



  end subroutine init_read_bg

  subroutine init_write_anl
    use vars_calc_analysis, only: anal_file, anlfile
    implicit none
    ! variables local to this subroutine
    integer :: iret

    ! initialize the NEMSIO library
    call nemsio_init(iret=iret)  
    if (iret /= 0) then
      write(*,*) 'Error with NEMSIO Write Init, iret=',iret
      stop
    end if
    ! open the NEMSIO output file
    call nemsio_open(anlfile, trim(anal_file), 'write', iret=iret)
    if (iret /= 0) then
      write(*,*) 'Error with NEMSIO Write Open, iret=',iret
      stop
    end if

  end subroutine init_write_anl

end module nemsio_interface
