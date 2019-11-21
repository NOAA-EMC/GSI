!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! module init_calc_analysis
!!        contains subroutines for reading namelist
!!        for calc_analysis utility
!! Original: 2019-09-18   martin   - original module
!!           2019-09-25   martin   - update to allow for netCDF I/O
!!           2019-10-24   martin   - update to support nemsio output
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module init_calc_analysis
  implicit none
contains
  subroutine read_nml(mype)
    !! read in namelist parameters from
    !! calc_analysis.nml file in same directory
    !! as executable
    use vars_calc_analysis, only: anal_file, fcst_file, incr_file, nhrs_assim, use_nemsio_anl, fhrs_pe, fhr 
    implicit none
    integer, intent(in) :: mype
    ! local variables to this subroutine
    character(len=500) :: datapath = './'
    character(len=500) :: analysis_filename = 'atmanl.nc'
    character(len=500) :: firstguess_filename = 'atmges.nc'
    character(len=500) :: increment_filename = 'atminc.nc'
    character(len=2) :: hrstr
    integer, parameter :: lunit = 10
    logical :: lexist = .false.
    namelist /setup/ datapath, analysis_filename, firstguess_filename, increment_filename, nhrs_assim, use_nemsio_anl

    fhrs_pe = (/6,3,9,4,5,7,8/)
    nhrs_assim = 1 ! default to 6 hour cycle only
    use_nemsio_anl = .false. ! default to using netCDF for background and analysis

    ! read in the namelist
    inquire(file='calc_analysis.nml', exist=lexist)
    if ( lexist ) then
      open(file='calc_analysis.nml', unit=lunit, status='old', &
           form='formatted', action='read', access='sequential')
      read(lunit,nml=setup)
      close(lunit)
    else
      write(6,*) 'calc_analysis.nml does not exist and should, ABORT!'
      stop 99
    end if

    ! combine strings to get full paths
    if (mype < nhrs_assim) then
      fhr = fhrs_pe(mype+1)
    else
      fhr = 0
    end if
    write(hrstr,'(I0.2)') fhr 
    anal_file = trim(adjustl(datapath)) // '/' // trim(adjustl(analysis_filename)) // '.' // hrstr 
    fcst_file = trim(adjustl(datapath)) // '/' // trim(adjustl(firstguess_filename)) // '.' // hrstr 
    incr_file = trim(adjustl(datapath)) // '/' // trim(adjustl(increment_filename)) // '.' // hrstr

    if (mype < nhrs_assim) then
      write(6,*) 'Analysis File    = ', trim(anal_file)
      write(6,*) 'First Guess File = ', trim(fcst_file)
      write(6,*) 'Increment File   = ', trim(incr_file) 
      write(6,*) 'Forecast Hour    = ', fhr
      write(6,*) 'input guess file and increment file should be in netCDF format'
      if (use_nemsio_anl) then
        write(6,*) 'writing analysis in NEMSIO format'
      else
        write(6,*) 'writing analysis in netCDF format'
      end if
    end if
    
  end subroutine read_nml
end module init_calc_analysis
