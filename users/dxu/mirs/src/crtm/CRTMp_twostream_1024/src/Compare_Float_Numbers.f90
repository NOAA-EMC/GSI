!------------------------------------------------------------------------------
!M+
! NAME:
!       Compare_Float_Numbers
!
! PURPOSE:
!       Module containing routines to perform equality and relational
!       comparisons on floating point numbers.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Compare_Numbers
!
! MODULES:
!       Type_Kinds:         Module containing definitions for kinds
!                           of variable types.
!
! CONTAINS:
!       .EqualTo.           Relational operator to test the equality of
!                           floating point numbers.
!
!       .GreaterThan.       Relational operator to test if one operand
!                           is greater than another.
!
!       .LessThan.          Relational operator to test if one operand
!                           is less than another.
!
!       Compare_Float:      Function to compare floating point scalars
!                           and arrays with adjustible precision tolerance.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2003 Paul van Delst
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

MODULE Compare_Float_Numbers


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Compare_Float
  PUBLIC :: OPERATOR (.EqualTo.)
  PUBLIC :: OPERATOR (.GreaterThan.)
  PUBLIC :: OPERATOR (.LessThan.)


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Compare_Float
    MODULE PROCEDURE Compare_Float_Single
    MODULE PROCEDURE Compare_Float_Double
  END INTERFACE Compare_Float

  INTERFACE OPERATOR (.EqualTo.)
    MODULE PROCEDURE Is_Equal_To_Single
    MODULE PROCEDURE Is_Equal_To_Double
  END INTERFACE 

  INTERFACE OPERATOR (.GreaterThan.)
    MODULE PROCEDURE Is_Greater_Than_Single
    MODULE PROCEDURE Is_Greater_Than_Double
  END INTERFACE 

  INTERFACE OPERATOR (.LessThan.)
    MODULE PROCEDURE Is_Less_Than_Single
    MODULE PROCEDURE Is_Less_Than_Double
  END INTERFACE 


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: Compare_Float_Numbers.f90,v 2.3 2004/10/06 19:00:23 paulv Exp $'


CONTAINS


!----------------------------------------------------------------------------------
!S+
! NAME:
!       .EqualTo.
!
! PURPOSE:
!       Relational operator to test the equality of floating point numbers.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       IF ( x .EqualTo. y ) THEN
!         .....
!       END IF
!
! OPERANDS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL( Single )   [ == default real]
!                                  OR
!                                REAL( Double )
!                    DIMENSION:  Scalar, or any allowed rank array.
!
! OPERATOR RESULT:
!       (x .EqualTo. y)    The result is a logical value indicating whether
!                          the operands are equal to within numerical precision
!                          UNITS:      N/A
!                          TYPE:       LOGICAL
!                          DIMENSION:  Same as operands.
!
! PROCEDURE:
!       The test performed is
!
!         ABS( x - y ) < SPACING( MAX(ABS(x),ABS(y)) )
!
!       If the result is .TRUE., the numbers are considered equal.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 30-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Is_Equal_To_Single( x, y ) RESULT( Equal_To )
    REAL( Single ), INTENT( IN )  :: x, y
    LOGICAL :: Equal_To

    Equal_To = ABS( x - y ) < SPACING( MAX(ABS(x),ABS(y)) )

  END FUNCTION Is_Equal_To_Single

  ELEMENTAL FUNCTION Is_Equal_To_Double( x, y ) RESULT( Equal_To )
    REAL( Double ), INTENT( IN )  :: x, y
    LOGICAL :: Equal_To

    Equal_To = ABS( x - y ) < SPACING( MAX(ABS(x),ABS(y)) )

  END FUNCTION Is_Equal_To_Double





!----------------------------------------------------------------------------------
!S+
! NAME:
!       .GreaterThan.
!
! PURPOSE:
!       Relational operator to test if one operand is greater than another.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       IF ( x .GreaterThan. y ) THEN
!         .....
!       END IF
!
! OPERANDS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL( Single )   [ == default real]
!                                  OR
!                                REAL( Double )
!                    DIMENSION:  Scalar, or any allowed rank array.
!
! OPERATOR RESULT:
!       (x .GreaterThan. y)    The result is a logical value indicating whether
!                              the operand x is greater than y by more than
!                              the spacing between representable floating point
!                              numbers.
!                              UNITS:      N/A
!                              TYPE:       LOGICAL
!                              DIMENSION:  Same as operands.
!
! PROCEDURE:
!       The test performed is
!
!         ( x - y ) >= SPACING( MAX(ABS(x),ABS(y)) )
!
!       If the result is .TRUE., x is considered greater than y.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 30-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Is_Greater_Than_Single( x, y ) RESULT ( Greater_Than )
    REAL( Single ), INTENT( IN ) :: x, y
    LOGICAL :: Greater_Than

    IF ( (x - y) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Greater_Than = .TRUE.
    ELSE
      Greater_Than = .FALSE.
    END IF
  END FUNCTION Is_Greater_Than_Single


  ELEMENTAL FUNCTION Is_Greater_Than_Double( x, y ) RESULT ( Greater_Than )
    REAL( Double ), INTENT( IN ) :: x, y
    LOGICAL :: Greater_Than

    IF ( (x - y) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Greater_Than = .TRUE.
    ELSE
      Greater_Than = .FALSE.
    END IF
  END FUNCTION Is_Greater_Than_Double





!----------------------------------------------------------------------------------
!S+
! NAME:
!       .LessThan.
!
! PURPOSE:
!       Relational operator to test if one operand is less than another.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       IF ( x .LessThan. y ) THEN
!         .....
!       END IF
!
! OPERANDS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL( Single )   [ == default real]
!                                  OR
!                                REAL( Double )
!                    DIMENSION:  Scalar, or any allowed rank array.
!
! OPERATOR RESULT:
!       (x .LessThan. y)    The result is a logical value indicating whether
!                           the operand x is less than y by more than the
!                           spacing between representable floating point
!                           numbers.
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Same as operands.
!
! PROCEDURE:
!       The test performed is
!
!         ( y - x ) >= SPACING( MAX(ABS(x),ABS(y)) )
!
!       If the result is .TRUE., x is considered less than y.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 30-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Is_Less_Than_Single( x, y ) RESULT ( Less_Than )
    REAL( Single ), INTENT( IN ) :: x, y
    LOGICAL :: Less_Than

    IF ( (y - x) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Less_Than = .TRUE.
    ELSE
      Less_Than = .FALSE.
    END IF
  END FUNCTION Is_Less_Than_Single


  ELEMENTAL FUNCTION Is_Less_Than_Double( x, y ) RESULT ( Less_Than )
    REAL( Double ), INTENT( IN ) :: x, y
    LOGICAL :: Less_Than

    IF ( (y - x) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Less_Than = .TRUE.
    ELSE
      Less_Than = .FALSE.
    END IF
  END FUNCTION Is_Less_Than_Double





!----------------------------------------------------------------------------------
!S+
! NAME:
!       Compare_Float
!
! PURPOSE:
!       Function to compare floating point scalars and arrays with adjustible
!       precision tolerance.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Result = Compare_Float( x, y,     &  ! Input
!                               ULP = ULP )  ! Optional input
!
! INPUT ARGUMENTS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL( Single )   [ == default real]
!                                  OR
!                                REAL( Double )
!                    DIMENSION:  Scalar, or any allowed rank array.
!                    ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP:         Unit of data precision. The acronym stands for "unit in
!                    the last place," the smallest possible increment or decrement
!                    that can be made using a machine's floating point arithmetic.
!                    A 0.5 ulp maximum error is the best you could hope for, since
!                    this corresponds to always rounding to the nearest representable
!                    floating-point number. Value must be positive - if a negative
!                    value is supplied, the absolute value is used.
!                    If not specified, the default value is 1.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: OPTIONAL, INTENT( IN )
!                  
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Result:      The return value is a logical value indicating whether
!                    the inputs are equal (to within the required precision)
!                    .TRUE.  - if the floating point numbers are equal to
!                              within the specified tolerance. 
!                    .FALSE. - if the floating point numbers are different.
!                    UNITS:      N/A
!                    TYPE:       LOGICAL
!                    DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The test performed is
!
!         ABS( x - y ) < ( ULP * SPACING( MAX(ABS(x),ABS(y)) ) )
!
!       If the result is .TRUE., the numbers are considered equal.
!
!       The intrinsic function SPACING(x) returns the absolute spacing of numbers
!       near the value of x,
!
!                      {     EXPONENT(x)-DIGITS(x)
!                      {  2.0                        for x /= 0
!         SPACING(x) = {
!                      {  
!                      {  TINY(x)                    for x == 0
!
!       The ULP optional argument scales the comparison.
!
!       James Van Buskirk and James Giles suggested this method for floating
!       point comparisons in the comp.lang.fortran newsgroup.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!----------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Compare_Float_Single( x, y, ulp ) RESULT( Compare )
    REAL( Single ),           INTENT( IN )  :: x
    REAL( Single ),           INTENT( IN )  :: y
    INTEGER,        OPTIONAL, INTENT( IN )  :: ulp
    LOGICAL :: Compare

    REAL( Single ) :: Rel

    Rel = 1.0_Single
    IF ( PRESENT( ulp ) ) THEN
      Rel = REAL( ABS(ulp), Single )
    END IF

    Compare = ABS( x - y ) < ( Rel * SPACING( MAX(ABS(x),ABS(y)) ) )

  END FUNCTION Compare_Float_Single


  ELEMENTAL FUNCTION Compare_Float_Double( x, y, ulp ) RESULT( Compare )
    REAL( Double ),           INTENT( IN )  :: x
    REAL( Double ),           INTENT( IN )  :: y
    INTEGER,        OPTIONAL, INTENT( IN )  :: ulp
    LOGICAL :: Compare

    REAL( Double ) :: Rel

    Rel = 1.0_Double
    IF ( PRESENT( ulp ) ) THEN
      Rel = REAL( ABS(ulp), Double )
    END IF

    Compare = ABS( x - y ) < ( Rel * SPACING( MAX(ABS(x),ABS(y)) ) )

  END FUNCTION Compare_Float_Double

END MODULE Compare_Float_Numbers


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Compare_Float_Numbers.f90,v 2.3 2004/10/06 19:00:23 paulv Exp $
!
! $Date: 2004/10/06 19:00:23 $
!
! $Revision: 2.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Compare_Float_Numbers.f90,v $
! Revision 2.3  2004/10/06 19:00:23  paulv
! - Cosmetic change only.
!
! Revision 2.2  2004/08/31 19:59:30  paulv
! - Added .EqualTo., .GreaterThan., and .LessThan. relational operators for
!   Single and Double floating point data objects.
!
! Revision 2.1  2004/08/16 16:25:41  paulv
! - Updated header documentation.
!
! Revision 2.0  2004/08/13 20:25:48  paulv
! - New version using elemental functions for Compare_Float.
!
! Revision 1.1  2003/04/02 14:35:30  paulv
! Initial checkin.
!
!
!
