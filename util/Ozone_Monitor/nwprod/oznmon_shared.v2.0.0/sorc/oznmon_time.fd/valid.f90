!$$$ subprogram documentation block
!               .      .    .
! subprogram:	valid			validate the obs and penalty values
!     prgmmr:   safford			date:  2009-12
!
! abstract:	This module contains code to read a given satellite's 
!               base file and then validate new obs(count) and penalty
!               values by comparing them to the baseline values.  
!
! program history log:
!	2009-12-07 safford - initial coding
!
! contains:
!
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

module valid

  implicit none
  
  private

! --- module routines
  public :: load_base
  public :: validate_count
  public :: validate_penalty 

! --- module parameters
  integer, parameter  :: funit = 17
  real,parameter :: rmiss      = -999.0

! --- module vars
  logical :: base_loaded = .FALSE.
  integer :: nlevel, nregion, j, k, dummy

  real,allocatable,dimension(:,:)::  avg_count, sdv_count 
  real,allocatable,dimension(:,:)::  min_count, max_count 
  real,allocatable,dimension(:,:)::  avg_penalty, sdv_penalty 
  real,allocatable,dimension(:,:)::  min_penalty, max_penalty 

  integer,allocatable,dimension(:):: nu_chan
 
  contains

    !-------------------------------------------------------------
    !  load the base file for the given satellite
    !-------------------------------------------------------------

    subroutine load_base( satname, iret )

      !--- interface
      character(20), intent( in )	:: satname
      integer, intent( out )		:: iret     

      !--- variables
      character(20) fname
      character(40) test_satname
      character(10) base_date
      character(20) dum1, dum2, dum3, dum4, dum5, dum6, dum7, dum8, dum9, dum10, dum11
      character(20) dum
      integer fios
      integer level, region

      logical fexist 


      !--- initialization
      iret   = -1
      fname  = trim(satname) // '.base'
      fexist = .FALSE.
 
      write(*,*) '--> begin load_base'
      write(*,*) '--> valid, satname, fname = ', satname, fname

      !--- verify file exists and open the file
      inquire( file = fname, exist = fexist )
      if( fexist .eqv. .FALSE. ) then
         fios = -1 
      else 
         open( UNIT=funit, FILE=fname, IOSTAT=fios )
      end if
      write(*,*) ' fios from inquire = ', fios
  
      if( fios == 0 ) then
         !--- read the file header
         read(funit,*) test_satname, nlevel, nregion

         write(*,*) ' test_satname, nlevel, nregion = ', test_satname, nlevel, nregion
         !--- line 2 of header file
!         read(funit,*) dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,dum9,dum10,dum11
         read(funit,*) dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,dum9,dum10,dum11
         write(*,*) 'line 2, dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8 = ', &
                       dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,dum9,dum10,dum11

         allocate( avg_count(nlevel,nregion), sdv_count(nlevel,nregion), &
              avg_penalty(nlevel,nregion), sdv_penalty(nlevel,nregion), &
              min_count(nlevel,nregion), max_count(nlevel,nregion), &
              min_penalty(nlevel,nregion), max_penalty(nlevel,nregion) )
         write(*,*) 'allocation complete'

         ! --- set all missing
         do k=1,nregion
            do j=1,nlevel
               avg_count(j,k) = rmiss
               sdv_count(j,k) = rmiss
               min_count(j,k) = rmiss
               max_count(j,k) = rmiss
               avg_penalty(j,k) = rmiss
               sdv_penalty(j,k) = rmiss
               min_penalty(j,k) = rmiss
               max_penalty(j,k) = rmiss
            end do
         end do

         write(*,*) 'nregion, nlevel = ', nregion, nlevel

         do k=1,nregion
            do j=1,nlevel
               read(funit,*) level, region, &
                    avg_count(j,k), sdv_count(j,k), &
                    min_count(j,k), max_count(j,k), &
                    avg_penalty(j,k), sdv_penalty(j,k), &
                    min_penalty(j,k), max_penalty(j,k)

!               read(funit,*) region, level, &
!                    avg_count(j,k), sdv_count(j,k), &
!                    min_count(j,k), max_count(j,k), &
!                    avg_penalty(j,k), sdv_penalty(j,k), &
!                    min_penalty(j,k), max_penalty(j,k)

               write(*,*) 'finished read, level, region :', j, k
               write(*,*) '    avg_count,sdv_count, min_count, max_count = ', &
                               avg_count(j,k),sdv_count(j,k), min_count(j,k), max_count(j,k)
               write(*,*) '    avg_pen,sdv_pen, min_pen, max_pen = ', &
                               avg_penalty(j,k),sdv_penalty(j,k), min_penalty(j,k), max_penalty(j,k)

            end do
         end do

         write(*,*) 'LOADED values:'
         k=1
         do j=1,nlevel
            write(*,*) 'j, k,avg_count,avg_penalty = ', j, k,avg_count(j,k),avg_penalty(j,k)
         end do

         iret = 0 
         base_loaded = .TRUE.
      else
         write(*,*) 'unable to load fname for data error checking'
      end if

      write(*,*) '<-- end load_base'

    end subroutine load_base


    !---------------------------------------------------------------
    !  validate a count
    !     given a count value for a level and region, determine
    !     if the count is within +/- 2*sdv 
    !
    !     iret         0 = normal
    !                 -1 = invalid level
    !                 -2 = invalid region
    !                  1 = base file wasn't loaded, unable to validate
    !---------------------------------------------------------------
    subroutine validate_count( level, region, count, valid, bound, iret )

      !--- interface
      integer, intent( in )		:: level
      integer, intent( in )		:: region
      real, intent( in )                :: count
      logical, intent( out )            :: valid
      real, intent( out )               :: bound
      integer, intent( out )		:: iret

      !--- vars
      real cnt, hi, lo, sdv2

      write(*,*) '--> validate_count, level, region, count ', level, region, count
      !--- initialize vars
      iret = 0 
      cnt = count
      valid = .FALSE.

      if( base_loaded .eqv. .TRUE. ) then
         if( level < 1 .OR. level > nlevel ) then
            iret = -1
            write(*,*) 'Warning:  In validate_count attempt to validate level out of range', level
            valid = .TRUE.
         else if( region < 1 .OR. region > nregion ) then
            iret = -2
            write(*,*) 'Warning:  In validate_count attempt to validate region out of range', region
            valid = .TRUE.
         else
            ! 
            !  all unassimilated level in the base files will have an rmiss
            !  value and are considered valid for verification purposes
            !
            if( avg_count(level,region) < 0.0 ) then
               valid = .TRUE.
            else
               sdv2 = 2 * sdv_count( level, region )
               hi = avg_count(level,region) + sdv2
               lo = avg_count(level,region) - sdv2
               bound = lo

               !
               !  Consider any count valid if
               !    cnt is 2 sdv from avg 
               !
               if( cnt >= lo ) then
                  valid = .TRUE.
               end if 

            end if

         end if

         if ( valid .eqv. .FALSE. ) then
            write(*,*) ' avg_count(level,region), sdv2, hi, lo = ', avg_count(level,region), sdv2, hi, lo
         end if
         write (*,*) '<-- valid, iret=', valid, iret
      else 
         !--- base file was not loaded, so return a warning that validation isn't possible
         iret = 1 
      end if 
    end subroutine validate_count


    !-------------------------------------------------------------
    !  validate a penalty value
    !     given a penalty value for a level and region, determine
    !     if the penalty is within +/- 2*sdv 
    !
    !     iret         0 = normal
    !                 -1 = invalid level
    !                 -2 = invalid region
    !-------------------------------------------------------------
    subroutine validate_penalty( level, region, penalty, valid, bound, iret )

      !--- interface
      integer, intent( in )		:: level
      integer, intent( in )		:: region
      real, intent( in )                :: penalty
      logical, intent( out )            :: valid
      real, intent( out )               :: bound
      integer, intent( out )		:: iret

      !--- vars
      real sdv2

      write(*,*) '--> validate_penalty, level, region, penalty ', level, region, penalty

      !--- initialize vars
      iret = 0 
      valid = .FALSE.
      bound = rmiss

      if( base_loaded .eqv. .TRUE. .AND. nlevel > 1 ) then
         if( level < 1 .OR. level > nlevel ) then
            iret = -1
            write(*,*) 'Warning:  In validate_penalty attempt to validate level out of range', level
            valid = .TRUE.
         else if( region < 1 .OR. region > nregion ) then
            iret = -2
            write(*,*) 'Warning:  In validate_penalty attempt to validate region out of range', region
            valid = .TRUE.
         else
            !
            !  all unassimilated level in the base files will have an rmiss
            !  value and are considered valid for verification purposes
            !
!            sdv2 = 2 * sdv_penalty( level, region )

!            bound = avg_penalty(level,region) + sdv2
            bound = max_penalty(level,region) * 1.2

!            write(6,*)'check pen vs bound = ', penalty, bound

            if( avg_penalty(level,region) < 0.0 ) then
               valid = .TRUE.
            else

               !
               !  Penalty value less than bound is valid
               !
               if( penalty <= bound ) then
                  valid = .TRUE.
               end if
  
            end if
         end if

         if ( valid .eqv. .FALSE. ) then
            write(*,*) ' BAD:  penalty, max_penalty(level,region), bound = ', penalty, max_penalty(level,region), bound
         end if
         write (*,*) '<-- valid, iret=', valid, iret
      else 
         !--- base file was not loaded, or nlevel was 0 so return 
         !--- a warning that validation isn't possible
         write (*,*) 'Warning:  base file not loaded or nlevel < 1, nlevel= ', nlevel
         iret = 1 
      end if 
    end subroutine validate_penalty

    
end module valid
