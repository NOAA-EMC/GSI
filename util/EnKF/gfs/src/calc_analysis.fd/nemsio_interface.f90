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
  subroutine read_bg
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine read_bg
  !            read NEMSIO first guess file
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcst_file, &
                                  nrec, nfday, nfhour, nfminute, nfsecondn, &
                                  nlon, nlat, nlev, nframe, nsoil, ntrac
    implicit none
    ! variables local to this subroutine
    integer :: iret
    type(nemsio_gfile) :: fcstfile
    
    ! initialize the NEMSIO library
    call nemsio_init(iret=iret)  
    if (iret /= 0) then
      write(*,*) 'Error with NEMSIO Init, iret=',iret
      stop
    end if
    ! open the NEMSIO input file
    call nemsio_open(fcstfile, trim(fcst_file), 'READ', iret=iret)
    if (iret /= 0) then
      write(*,*) 'Error with NEMSIO Open, iret=',iret
      stop
    end if
    ! get dimensions, etc so that arrays can be allocated

  end subroutine read_bg

  subroutine write_anl
  end subroutine write_anl

end module nemsio_interface
