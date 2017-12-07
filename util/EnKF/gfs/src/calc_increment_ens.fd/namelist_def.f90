module namelist_def

  implicit none

  private

  public :: max_vars, nens
  public :: analysis_filename, firstguess_filename, increment_filename
  public :: datapath
  public :: debug
  public :: incvars_to_zero
  public :: read_namelist

  ! Define global variables

  integer, parameter :: max_vars = 99
  character(len=500) :: datapath            = './'
  character(len=500) :: analysis_filename   = 'atmanl.nemsio'
  character(len=500) :: firstguess_filename = 'atmbkg.nemsio'
  character(len=500) :: increment_filename  = 'fv3_increment.nc'
  integer            :: nens                = 1
  logical            :: debug               = .false.
  character(len=10)  :: incvars_to_zero(max_vars) = 'NONE'

  namelist /setup/ datapath, analysis_filename, firstguess_filename, increment_filename, &
                   nens, debug
  namelist /zeroinc/ incvars_to_zero

contains

subroutine read_namelist

  implicit none

  integer, parameter :: lunit = 10
  logical :: lexist = .false.

  inquire(file='calc_increment.nml', exist=lexist)
  if ( lexist ) then

    open(file='calc_increment.nml', unit=lunit, status='old', &
         form='formatted', action='read', access='sequential')
    read(lunit,nml=setup)
    read(lunit,nml=zeroinc)
    close(lunit)

  else
    write(6,*) 'calc_increment.nml does not exist and should, ABORT!'
    stop 99
  endif

  return
end subroutine read_namelist

end module namelist_def
