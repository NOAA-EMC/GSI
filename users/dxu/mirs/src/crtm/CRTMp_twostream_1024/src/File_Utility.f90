!------------------------------------------------------------------------------
!M+
! NAME:
!       File_Utility
!
! PURPOSE:
!       Module containing generic file utility routines
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE File_Utility
!
! MODULES:
!       None.
!
! CONTAINS:
!       Get_Lun:      Function to return a free logical unit number for
!                     file access.
!
!       File_Exists:  Function to determine if a named file exists.
!
!       File_Open:    Function to determine if a file is open.
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
!  Copyright (C) 2000, 2004 Paul van Delst
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

MODULE File_Utility


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Get_Lun
  PUBLIC :: File_Exists
  PUBLIC :: File_Open


  ! --------------------
  ! Function overloading
  ! --------------------

  INTERFACE File_Exists
    MODULE PROCEDURE File_Unit_Exists
    MODULE PROCEDURE File_Name_Exists
  END INTERFACE File_Exists

  INTERFACE File_Open
    MODULE PROCEDURE File_Open_by_Unit
    MODULE PROCEDURE File_Open_by_Name
  END INTERFACE File_Open


CONTAINS


!------------------------------------------------------------------------------
!S+
! NAME:
!       Get_Lun
!
! PURPOSE:
!       Function to obtain a free logical unit number for file access
!
! CALLING SEQUENCE:
!       Lun = Get_Lun()
!
! INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Lun:          Logical unit number that may be used for file access.
!                     If Lun > 0 it can be used as a logical unit number to open
!                                and access a file.
!                        Lun < 0 a non-existant logical unit number was reached
!                                during the search.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
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
!       result is set to a negative value. If the file unit does exist, it is
!       tested to see if it is connected to an open file. If so, it is
!       incremented by 1. This is repeated until a free (or invalid) logical
!       unit number is found.
!S-
!------------------------------------------------------------------------------

  FUNCTION Get_Lun() RESULT( Lun )


    ! -----------------
    ! Type declarations
    ! -----------------
 
    INTEGER :: Lun


    ! ------------------------------
    ! Initialise logical unit number
    ! ------------------------------

    Lun = 9


    ! ------------------------------
    ! Start open loop for Lun Search
    ! ------------------------------

    Lun_Search: DO

      ! -- Increment logical unit number
      Lun = Lun + 1

      ! -- If file unit does not exist, set to -1 and exit
      IF ( .NOT. File_Exists( Lun ) ) THEN
        Lun = -1
        EXIT Lun_Search
      END IF

      ! -- If the file is not open, we're done.
      IF ( .NOT. File_Open( Lun ) ) EXIT Lun_Search

    END DO Lun_Search

  END FUNCTION Get_Lun



!------------------------------------------------------------------------------
!S+
! NAME:
!       File_Exists
!
! PURPOSE:
!       PUBLIC function to determine if a file unit or a file exists.
!
! CALLING SEQUENCE:
!       Result = File_Exists( FileID/Filename )
!
! INPUT ARGUMENTS:
!       Specify one of:
!
!         FileID:     The logical unit number for which the existence
!                     is to be determined.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!       or
!
!         Filename:   Name of the file the existence of which is to
!                     be determined.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Result:       The return value is a logical result.
!                     If .TRUE.  the file unit/file exists.
!                        .FALSE. the file unit/file does not exist.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!
! CALLS:
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

  FUNCTION File_Unit_Exists( FileID ) RESULT ( Existence )


    ! -----------------
    ! Type declarations
    ! -----------------
 
    INTEGER, INTENT( IN ) :: FileID
    LOGICAL :: Existence


    ! ---------------
    ! Inquire by unit
    ! ---------------

    INQUIRE( UNIT = FileID, EXIST = Existence )

  END FUNCTION File_Unit_Exists


  FUNCTION File_Name_Exists( Filename ) RESULT ( Existence )


    ! -----------------
    ! Type declarations
    ! -----------------
 
    CHARACTER( * ), INTENT( IN ) :: Filename
    LOGICAL :: Existence


    ! ---------------
    ! Inquire by name
    ! ---------------

    INQUIRE( FILE = Filename, EXIST = Existence )

  END FUNCTION File_Name_Exists





!------------------------------------------------------------------------------
!S+
! NAME:
!       File_Open
!
! PURPOSE:
!       Function to determine if a file is open for I/O.
!
! CALLING SEQUENCE:
!       Result = File_Open( FileID/Filename )
!
! INPUT ARGUMENTS:
!       Specify one of:
!
!         FileID:     The logical unit number of the file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!       or
!
!         Filename:   The name of the file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Result:       The return value is a logical result.
!                     If .TRUE.  the file is open.
!                        .FALSE. the file is not open
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!
! CALLS:
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

  FUNCTION File_Open_by_Unit( FileID ) RESULT ( Is_Open )


    ! -----------------
    ! Type declarations
    ! -----------------
 
    INTEGER, INTENT( IN ) :: FileID
    LOGICAL :: Is_Open


    ! ---------------
    ! Inquire by unit
    ! ---------------

    INQUIRE( UNIT = FileID, OPENED = Is_Open )

  END FUNCTION File_Open_by_Unit


  FUNCTION File_Open_by_Name( Filename ) RESULT ( Is_Open )


    ! -----------------
    ! Type declarations
    ! -----------------
 
    CHARACTER( * ), INTENT( IN ) :: Filename
    LOGICAL :: Is_Open


    ! ---------------
    ! Inquire by name
    ! ---------------

    INQUIRE( FILE = Filename, OPENED = Is_Open )

  END FUNCTION File_Open_by_Name

END MODULE File_Utility


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: File_Utility.f90,v 1.13 2005/04/01 15:20:51 paulv Exp $
!
! $Date: 2005/04/01 15:20:51 $
!
! $Revision: 1.13 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: File_Utility.f90,v $
! Revision 1.13  2005/04/01 15:20:51  paulv
! - Uncommented END INTERFACE names.
!
! Revision 1.12  2004/08/11 20:34:41  paulv
! - Updated.
!
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

