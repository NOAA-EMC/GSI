!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! module init_calc_analysis
!!        contains subroutines for reading namelist
!!        for calc_analysis utility
!! Original: 2019-09-16   martin   - original module
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module init_calc_analysis
  implicit none
contains
  subroutine read_nml
    !! read in namelist parameters from
    !! calc_analysis.nml file in same directory
    !! as executable
    use vars_calc_analysis, only: anal_file, fcst_file, incr_file 
    implicit none
    ! local variables to this subroutine
    character(len=500) :: datapath = './'
    character(len=500) :: analysis_filename = 'atmanl.nemsio'
    character(len=500) :: firstguess_filename = 'atmbkg.nemsio'
    character(len=500) :: increment_filename = 'atminc.nc'
    namelist /setup/ datapath, analysis_filename, firstguess_filename, increment_filename
    integer, parameter = lunit = 10
    logical :: lexist = .false.

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
    anal_file = trim(adjustl(datapath)) // trim(adjustl(analysis_filename)) 
    fcst_file = trim(adjustl(datapath)) // trim(adjustl(firstguess_filename)) 
    incr_file = trim(adjustl(datapath)) // trim(adjustl(increment_filename)) 

    write(6,*) 'Analysis File    = ', trim(anal_file)
    write(6,*) 'First Guess File = ', trim(fcst_file)
    write(6,*) 'Increment File   = ', trim(incr_file) 
    
  end subroutine read_nml
end module init_calc_analysis
