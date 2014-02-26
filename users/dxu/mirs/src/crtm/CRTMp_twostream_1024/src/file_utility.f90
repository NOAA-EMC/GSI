!------------------------------------------------------------------------------
!M+
! NAME:
!       file_utility
!
! PURPOSE:
!       Module containing generic file utility routines
!
! CATEGORY:
!       Utility
!
! CALLING SEQUENCE:
!       USE file_utility
!
! OUTPUTS:
!       None
!
! MODULES:
!       None.
!
! CONTAINS:
!       get_lun:     PUBLIC function to return a free logical unit number for
!                    file access.
!
!       file_exists: PUBLIC function to determine if a named file exists.
!
!       file_open:   PUBLIC function to determine if a file is open.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Jul-2000
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!------------------------------------------------------------------------------

MODULE file_utility


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: get_lun
  PUBLIC :: file_exists
  PUBLIC :: file_open

  ! ... and replaced with this due to a bug in the 
  ! pgf90 compiler (to be fixed by Q4 2001)
!  PRIVATE :: file_unit_exists
!  PRIVATE :: file_name_exists
!  PRIVATE :: file_open_by_unit
!  PRIVATE :: file_open_by_name


  ! --------------------
  ! Function overloading
  ! --------------------

  INTERFACE file_exists
    MODULE PROCEDURE file_unit_exists
    MODULE PROCEDURE file_name_exists
  END INTERFACE ! file_exists

  INTERFACE file_open
    MODULE PROCEDURE file_open_by_unit
    MODULE PROCEDURE file_open_by_name
  END INTERFACE ! file_open

CONTAINS


!------------------------------------------------------------------------------
!S+
! NAME:
!       get_lun
!
! PURPOSE:
!       PUBLIC function to obtain a free logical unit number for file access
!
! CALLING SEQUENCE:
!       result = get_lun()
!
! INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns a default integer.
!
!       If result > 0 it can be used as a logical unit number to open and
!                     access a file.
!          result < 0 a non-existant logical unit number was reached during
!                     the search
!
! CALLS:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The search for a free logical unit number begins at 10. The logical
!       unit number is first tested to see if it exists. If it does not, the
!       result is set to -1. If the file unit does exist, it is tested to see
!       if it is connected to an open file. If so, it is incremented by 1.
!       This is repeated until a free (or invalid) logical unit number is found.
!S-
!------------------------------------------------------------------------------

  FUNCTION get_lun() RESULT( lun )


    ! -----------------
    ! Type declarations
    ! -----------------
 
    INTEGER :: lun


    ! ------------------------------
    ! Initialise logical unit number
    ! ------------------------------

    lun = 9


    ! ------------------------------
    ! Start open loop for lun search
    ! ------------------------------

    lun_search: DO

      ! -- Increment logical unit number
      lun = lun + 1

      ! -- If file unit does not exist, set to -1 and exit
      IF ( .NOT. file_exists( lun ) ) THEN
        lun = -1
        EXIT lun_search
      END IF

      ! -- If the file is not open, we're done.
      IF ( .NOT. file_open( lun ) ) EXIT lun_search

    END DO lun_search

  END FUNCTION get_lun



!------------------------------------------------------------------------------
!S+
! NAME:
!       file_exists
!
! PURPOSE:
!       PUBLIC function to determine if a file unit or a file exists.
!
! CALLING SEQUENCE:
!       result = file_exists( file_id/file_name )
!
! INPUT ARGUMENTS:
!       Specify one of:
!
!         file_id:    The logical unit number for which the existence
!                     is to be determined.
!                     UNITS:      None
!                     TYPE:       Integer
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!       or
!
!         file_name:  Name of the file the existence of which is to be determined.
!                     UNITS:      None
!                     TYPE:       Character
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns a logical result.
!
!       result = .TRUE.  => file unit/file exists
!              = .FALSE. => file unit/file does not exist
!
! CALLS:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION file_unit_exists( file_id ) RESULT ( existence )


    ! -----------------
    ! Type declarations
    ! -----------------
 
    INTEGER, INTENT( IN ) :: file_id
    LOGICAL :: existence


    ! ---------------
    ! Inquire by unit
    ! ---------------

    INQUIRE( UNIT = file_id, EXIST = existence )

  END FUNCTION file_unit_exists



  FUNCTION file_name_exists( file_name ) RESULT ( existence )


    ! -----------------
    ! Type declarations
    ! -----------------
 
    CHARACTER( * ), INTENT( IN ) :: file_name
    LOGICAL :: existence


    ! ---------------
    ! Inquire by name
    ! ---------------

    INQUIRE( FILE = file_name, EXIST = existence )

  END FUNCTION file_name_exists





!------------------------------------------------------------------------------
!S+
! NAME:
!       file_open
!
! PURPOSE:
!       PUBLIC function to determine if a file is open for I/O.
!
! CALLING SEQUENCE:
!       result = file_open( file_id/file_name )
!
! INPUT ARGUMENTS:
!       Specify one of:
!
!         file_id:    The logical unit number of the file.
!                     UNITS:      None
!                     TYPE:       Integer
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!       or
!
!         file_name:  The name of the file.
!                     UNITS:      None
!                     TYPE:       Character
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Function returns a logical result.
!
!       result = .TRUE.  => file is open
!              = .FALSE. => file is not open.
!
! CALLS:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       It is assumed the file unit or name exists.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION file_open_by_unit( file_id ) RESULT ( is_open )


    ! -----------------
    ! Type declarations
    ! -----------------
 
    INTEGER, INTENT( IN ) :: file_id
    LOGICAL :: is_open


    ! ---------------
    ! Inquire by unit
    ! ---------------

    INQUIRE( UNIT = file_id, OPENED = is_open )

  END FUNCTION file_open_by_unit



  FUNCTION file_open_by_name( file_name ) RESULT ( is_open )


    ! -----------------
    ! Type declarations
    ! -----------------
 
    CHARACTER( * ), INTENT( IN ) :: file_name
    LOGICAL :: is_open


    ! ---------------
    ! Inquire by name
    ! ---------------

    INQUIRE( FILE = file_name, OPENED = is_open )

  END FUNCTION file_open_by_name

END MODULE file_utility


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: file_utility.f90,v 1.11 2002/05/15 17:59:54 paulv Exp $
!
! $Date: 2002/05/15 17:59:54 $
!
! $Revision: 1.11 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: file_utility.f90,v $
! Revision 1.11  2002/05/15 17:59:54  paulv
! - Overloaded FILE_EXISTS() functions from FILE_UNITS_EXISTS() and FILE_NAME_EXISTS()
!   functions.
! - Added test for file unit existence to the GET_LUN() function.
!
! Revision 1.10  2001/10/24 17:36:18  paulv
! - Changed the way in which module subprograms are declared PUBLIC or PRIVATE
!   so code would compile using pgf90 3.2-4a. The compiler has a bug, dammit.
!
! Revision 1.9  2001/09/28 19:33:36  paulv
! - Updated FILE_OPEN subprogram header documentation.
!
! Revision 1.8  2001/09/24 02:54:21  paulv
! - Overloaded FILE_OPEN function to allow inquiry by unit or file name.
!
! Revision 1.7  2001/09/23 19:49:54  paulv
! - Removed file_open logical variable from GET_LUN function. Argh.
!
! Revision 1.6  2001/09/23 19:38:17  paulv
! - Added CVS "Name" to modification history keyword list.
!
! Revision 1.5  2001/09/23 19:29:14  paulv
! - Corrected bug in FILE_OPEN argument type specification
! - Use FILE_OPEN() function in GET_LUN()
! - Updated header documentation
!
! Revision 1.4  2001/09/17 20:11:09  paulv
! - Module now resides in the UTILITY module directory.
! - Added FILE_OPEN function.
!
! Revision 1.3  2000/08/31 19:36:32  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 1.2  2000/08/24 15:33:42  paulv
! - In the GET_LUN subprogram, the loop to search for a free unit number
!   was changed from:
!
!     DO WHILE ( file_open )
!       ...search
!     END DO
!
!   to
!
!     lun_search: DO
!       ...search
!       IF ( .NOT. file_open ) EXIT lun_search
!     END DO lun_search
!
!   The earlier version is a deprecated use of the DO with WHILE.
!
! - The subprogram FILE_EXISTS was added. Note that the INQUIRE statement
!   required the FILE =  keyword to work. Simply using the file name in
!   the INQUIRE returned an error (compiler assumed it was an inquire by
!   unit number?)
! - Updated module and subprogram documentation.
!
! Revision 1.1  2000/07/12 16:08:10  paulv
! Initial checked in version
!
!
!

