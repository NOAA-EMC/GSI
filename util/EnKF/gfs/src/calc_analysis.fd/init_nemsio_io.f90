!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! module nemsio_interface
!!        contains subroutines for reading NEMSIO background file
!!        and for writing out NEMSIO analysis files
!! Original: 2019-09-18   martin   - original module
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module init_nemsio_io
  use nemsio_module
  implicit none

  private

  public :: init_read_bg
  public :: init_write_anl

contains
  subroutine init_read_bg
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine read_bg
  !            read NEMSIO first guess file
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcst_file, &
                                  fcstfile, anlfile, &
                                  idate, nrec, nfday, nfhour, nfminute, nfsecondn, &
                                  nfsecondd, nlon, nlat, nlev, &
                                  recname, reclevtyp, &
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

  end subroutine init_read_bg

  subroutine init_write_anl
    use vars_calc_analysis, only: anal_file, anlfile, idate, jdate, &
                                  nhr_assim, nfhour, nfminute, nfsecondn, nfsecondd
    implicit none
    ! variables local to this subroutine
    integer :: iret
    real, dimension(5) :: fha
    integer, dimension(8) :: ida, jda 

    ! modify dates for analysis file
    ida(:) = 0
    jda(:) = 0
    fha(:) = 0
    fha(2) = nhr_assim  
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

    ! open the NEMSIO output file for writing
    call nemsio_open(anlfile, trim(anal_file), 'write', iret=iret, &
                    idate=jdate, nfhour=nfhour, nfminute=nfminute, &
                    nfsecondn=nfsecondn, nfsecondd=nfsecondd)
    if (iret /= 0) then
      write(*,*) 'Error with NEMSIO Write Open, iret=',iret
      stop
    end if

  end subroutine init_write_anl

end module init_nemsio_io
