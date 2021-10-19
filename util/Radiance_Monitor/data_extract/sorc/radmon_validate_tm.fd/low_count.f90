!$$$ subprogram documentation block
!               .      .    .
! subprogram:	low_count		build low_count.txt file
!     prgmmr:   safford			date:  2009-11
!
! abstract:	This module contains code to build the low_count.txt file.
!		The low_count.txt file reports the satellite and channel for
!		which an unexpectedly low number of observations were found
!               in the assimilated radiance data.  
!
! program history log:
!	2009-11-23 safford - initial coding
!
! contains:
!
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

module low_count

  implicit none
  
  private

  public :: open_low_count_file
  public :: write_low_count
  public :: close_low_count_file

  integer, parameter       :: funit = 10

  contains


    !-------------------------------------------------------------
    !  create the low_count file
    !-------------------------------------------------------------

    subroutine open_low_count_file( date, cycle, fios )

      !--- interface
      character(8), intent( in )	:: date
      character(8), intent( in )	:: cycle
      integer, intent( out )		:: fios     
    
      !--- variables
      logical 				:: lexist = .FALSE. 
      character(60)                     :: fname


      write(*,*) '--> open_low_count_file, date, cycle = ', date, cycle
      !--- build the file name
      fname = 'low_count.' // trim(date) // trim(cycle)

      !--- open file and write the header
      inquire(file=fname, exist=lexist)
      if( lexist .eqv. .FALSE. ) then
         write(*,*) ' opening new low_count file'
         open( UNIT=funit, FILE=fname, STATUS='NEW', IOSTAT=fios )
      else
         write(*,*) ' opening existing low_count.txt file'
         open( UNIT=funit, FILE=fname, STATUS='OLD', POSITION='APPEND', IOSTAT=fios )
      end if
    
    end subroutine open_low_count_file


    subroutine write_low_count( satname, channel, region, num_obs, avg_cnt )

      !--- interface
      character(20), intent( in )       :: satname
      integer, intent( in )		:: channel
      integer, intent( in )             :: region
      real, intent( in )		:: num_obs
      real, intent( in )                :: avg_cnt
      !--- 
      character(len=50)                 :: fmt

      fmt = "(A16,A10,I5,A9,I1,A10,I7,A10,F9.2)"

      write(funit, fmt) satname, ' channel= ', channel, &
                        ' region= ', region, ' num_obs= ', &
                        INT(num_obs), ' avg_obs= ', avg_cnt

    end subroutine write_low_count


    subroutine close_low_count_file( )
      write(*,*) '--> close_low_count_file'
      close( funit ) 
    end subroutine close_low_count_file

end module low_count
