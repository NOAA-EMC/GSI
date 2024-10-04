! combine_met_aero
! combine GSI Berror stats standard files
! with that from the aerosol NMC code
! because GSI needs certain fields to run properly
! cory.r.martin@noaa.gov - 2019/06/12
! ifort combine_met_aero.f90 -o combine_met_aero.x -convert big_endian 
! needs metfile.bin and aerfile.bin symbolically linked as input files

program combine_met_aero
  implicit none
  character(11) :: metfile, aerfile, outfile
  integer :: nsig, nlat, nlon
  integer :: nsiga, nlata, nlona
  integer :: i, nsigi
  real, allocatable, dimension(:,:,:) :: stdev3, hscale3, vscale3 
  real, allocatable, dimension(:,:,:) :: stdeva, hscalea, vscalea 
  real, allocatable, dimension(:,:,:) :: tcon 
  real, allocatable, dimension(:,:) :: stdevsst, hscalesst, rh 
  real, allocatable, dimension(:,:) :: pscon, vpcon 
  real, allocatable, dimension(:) :: stdevps, hscaleps
  integer, parameter :: nvarsa=14
  integer, parameter :: nvars3=6
  integer, parameter :: nvars2=2
  character(5), dimension(nvars3+nvars2+nvarsa) :: var

  metfile = 'metfile.bin'
  aerfile = 'aerfile.bin'
  outfile = 'outfile.bin'

  print *, 'Combine B error stats from meteorology and aerosol species'

  ! open the files
  open(unit=45, form='unformatted', status='old', file=metfile)
  open(unit=46, form='unformatted', status='old', file=aerfile)
  open(unit=47, form='unformatted', file=outfile)

  read(45) nsig, nlat, nlon
  read(46) nsiga, nlata, nlona

  ! fail if the dimensions are different between the met and aer files
  if (nsig /= nsiga .or. nlat /= nlata .or. nlon /= nlona) then
    write(*,*) 'ERROR: input files have different dimensions!'
    write(*,*) 'Filename:     nsig, nlat, nlon'
    write(*,*) 'metfile.bin:',nsig,nlat,nlon
    write(*,*) 'aerfile.bin:',nsiga,nlata,nlona
    write(*,*) 'Terminating program'
    stop
  end if

  ! allocate arrays
  allocate(stdev3(nlat,nsig,nvars3),hscale3(nlat,nsig,nvars3),vscale3(nlat,nsig,nvars3))
  allocate(stdevsst(nlat,nlon),hscalesst(nlat,nlon))
  allocate(rh(nlat,nsig))
  allocate(stdevps(nlat),hscaleps(nlat))
  allocate(stdeva(nlat,nsig,nvarsa),hscalea(nlat,nsig,nvarsa),vscalea(nlat,nsig,nvarsa))
  allocate(tcon(nlat,nsig,nsig),pscon(nlat,nsig),vpcon(nlat,nsig))

  ! read in met vars 
  write(*,*) 'Reading in met vars from metfile.bin'
  read(45) tcon,vpcon,pscon
  do i=1,nvars3
    read(45) var(i), nsigi
    print *, var(i), nsigi
    if (i==4) then
      read(45) stdev3(:,:,i), rh
    else
      read(45) stdev3(:,:,i)
    end if
    read(45) hscale3(:,:,i)
    read(45) vscale3(:,:,i)
  end do
  read(45) var(1+nvars3), nsigi
  print *, var(1+nvars3), nsigi
  read(45) stdevps
  read(45) hscaleps
  read(45) var(2+nvars3), nsigi
  print *, var(2+nvars3), nsigi
  read(45) stdevsst
  read(45) hscalesst
  ! read in aerosol vars
  write(*,*) 'Reading in aero vars from aerfile.bin'
  do i=1,nvarsa
    read(46) var(i+nvars3+nvars2), nsigi
    print *, var(i+nvars3+nvars2), nsigi
    read(46) stdeva(:,:,i)
    read(46) hscalea(:,:,i)
    read(46) vscalea(:,:,i)
  end do
  ! write out concatenated file
  write(*,*) 'Writing out to outfile.bin'
  write(47) nsig, nlat, nlon
  write(47) tcon, vpcon, pscon
  do i=1,nvars3
    write(47) var(i), nsigi
    print *, var(i), nsigi
    if (i==4) then
      write(47) stdev3(:,:,i), rh
    else
      write(47) stdev3(:,:,i)
      print *, stdev3(:,:,i)
    end if
    write(47) hscale3(:,:,i)
    write(47) vscale3(:,:,i)
  end do
  write(47) var(1+nvars3), 1
  print *, var(1+nvars3), 1
  write(47) stdevps
  write(47) hscaleps
  write(47) var(2+nvars3), 1
  print *, var(2+nvars3), 1
  write(47) stdevsst
  write(47) hscalesst
  do i=1,nvarsa
    write(47) var(i+nvars3+nvars2), nsigi
    print *, var(i+nvars3+nvars2), nsigi
    print *, stdeva(:,:,i)
    write(47) stdeva(:,:,i)
    write(47) hscalea(:,:,i)
    write(47) vscalea(:,:,i)
  end do
  write(*,*) 'Program Complete!'

end program combine_met_aero
