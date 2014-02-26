!------------------------------------------------------------------------------
!M+
! NAME:
!       Type_Kinds
!
! PURPOSE:
!       Module to hold specification kinds for variable declaration.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Type_Kinds
!
! OUTPUTS:
!       Integer Kind Types
!       ------------------
!
!       Byte:             Kind type for byte (1-byte) integer variable
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       Short:            Kind type for short (2-byte) integer variable
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       Long:             Kind type for long (4-byte) integer variable
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       LLong:            Kind type for double long (8-byte) integer variable
!                         If this kind type is not supported by a compiler, the
!                         value defaults to Long.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       IP_Kind:          Kind type for a user specified default integer.
!                         The actual kind type this value corresponds
!                         to is determined by the PRIVATE IIP index.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!
!       Floating point Kind Types
!       -------------------------
!
!       Single:           Kind type for single precision (4-byte) real variable
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       Double:           Kind type for double precision (8-byte) real variable
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       Quad:             Kind type for quad precision (16-byte) real variable
!                         If this kind type is not supported by a compiler, the
!                         value defaults to Double.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       FP_Kind:          Kind for for a user specified default floating point
!                         variable. The actual kind type this value corresponds
!                         to is determined by the PRIVATE IFP index.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!
!       Integer Byte Sizes
!       ------------------
!
!       n_Bytes_Byte:     The expected size of a Byte kind integer in units
!                         of 8-bit bytes.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       n_Bytes_Short:    The expected size of a Short kind integer in units
!                         of 8-bit bytes.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       n_Bytes_Long:     The expected size of a Long kind integer in units
!                         of 8-bit bytes.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       n_Bytes_LLong:    The expected size of a LLong kind integer in units
!                         of 8-bit bytes.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       n_Bytes_IP_kind:  The expected size of the user specified default
!                         integer kind in units of 8-bit bytes. The actual
!                         kind type size this value corresponds to is
!                         determined by the PRIVATE IIP index.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!
!
!       Floating point Byte Sizes
!       -------------------------
!
!       n_Bytes_Single:   The expected size of a Single kind real variable
!                         in units of 8-bit bytes.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       n_Bytes_Double:   The expected size of a Double kind real variable
!                         in units of 8-bit bytes.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       n_Bytes_Quad:     The expected size of a Quad kind real variable
!                         in units of 8-bit bytes.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       n_Bytes_FP_kind:  The expected size of the user specified default
!                         real kind variable in units of 8-bit bytes. The
!                         actual kind type size this value corresponds to 
!                         is determined by the PRIVATE IFP index.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
! MODULES:
!       None
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       If the LLong or Quad kind types are not available they default to the
!       Long and Double kind specifications.
!
! RESTRICTIONS:
!       None
!
! EXAMPLE:
!       USE Type_Kinds
!       INTEGER( Long ) :: i, j
!       REAL( Single )  :: x, y
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Jun-2000
!                       paul.vandelst@ssec.wisc.edu
!
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

MODULE Type_Kinds


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibility
  ! ------------------

  PRIVATE


  ! -------------------------------------------------------------------
  ! THE DEFAULT FLOATING POINT INDEX. Change the value of IFP for the
  ! required floating point kind. The following chart details the
  ! correspondence:
  !
  !    IFP     REAL( fp_kind )
  !  ==============================
  !     1       Single (4  bytes)
  !     2       Double (8  bytes)
  !     3       Quad   (16 bytes)  **IF AVAILABLE, Double OTHERWISE**
  !
  ! -------------------------------------------------------------------

  INTEGER, PARAMETER, PRIVATE :: IFP = 2  ! 1=Single, 2=Double, 3=Quad


  ! -------------------------------------------------------------------
  ! THE DEFAULT INTEGER INDEX. Change the value of IIP for the required
  ! integer kind. The following chart details the correspondence:
  !
  !    IIP     INTEGER( ip_kind )
  !  ==============================
  !     1        Byte 
  !     2       Short (2 bytes)
  !     3        Long (4 bytes)
  !     4       LLong (8 bytes)  **IF AVAILABLE, Long OTHERWISE**
  !
  ! -------------------------------------------------------------------

  INTEGER, PARAMETER, PRIVATE :: IIP = 3  ! 1=bByte, 2=Short, 3=Long, 4=LLong



  ! -------------------
  ! Integer definitions
  ! -------------------

  ! -- Integer types
  INTEGER, PARAMETER, PUBLIC  :: bByte    = SELECTED_INT_KIND(1)   ! Byte  integer
  INTEGER, PARAMETER, PUBLIC  :: Short   = SELECTED_INT_KIND(4)   ! Short integer
  INTEGER, PARAMETER, PUBLIC  :: Long    = SELECTED_INT_KIND(8)   ! Long  integer
  INTEGER, PARAMETER, PRIVATE :: LLong_t = SELECTED_INT_KIND(16)  ! LLong integer
  INTEGER, PARAMETER, PUBLIC  :: LLong   = ( ( ( 1 + SIGN( 1, LLong_t ) ) / 2 ) * LLong_t ) + &
                                           ( ( ( 1 - SIGN( 1, LLong_t ) ) / 2 ) * Long    )

  ! -- Expected 8-bit byte sizes of the integer kinds
  INTEGER, PARAMETER, PUBLIC :: n_Bytes_Byte  = 1
  INTEGER, PARAMETER, PUBLIC :: n_Bytes_Short = 2
  INTEGER, PARAMETER, PUBLIC :: n_Bytes_Long  = 4
  INTEGER, PARAMETER, PUBLIC :: n_Bytes_LLong = 8

  ! -- Define arrays for default definition
  INTEGER, PARAMETER, PRIVATE :: N_IP_KINDS = 4
  INTEGER, PARAMETER, DIMENSION( N_IP_KINDS ), PRIVATE :: IP_KIND_TYPES = (/ bByte,  &
                                                                             Short, &
                                                                             Long,  &
                                                                             LLong  /) 
  INTEGER, PARAMETER, DIMENSION( N_IP_KINDS ), PRIVATE :: IP_BYTE_SIZES = (/ n_Bytes_Byte,  &
                                                                             n_Bytes_Short, &
                                                                             n_Bytes_Long,  &
                                                                             n_Bytes_LLong  /)

  ! -- Default values
  INTEGER, PARAMETER, PUBLIC  :: IP_Kind         = IP_KIND_TYPES( IIP )
  INTEGER, PARAMETER, PUBLIC  :: n_Bytes_IP_Kind = IP_BYTE_SIZES( IIP )


  ! --------------------------
  ! Floating point definitions
  ! --------------------------

  ! -- Floating point types
  INTEGER, PARAMETER, PUBLIC  :: Single = SELECTED_REAL_KIND(6)  ! Single precision
  INTEGER, PARAMETER, PUBLIC  :: Double = SELECTED_REAL_KIND(15) ! Double precision
  INTEGER, PARAMETER, PRIVATE :: Quad_t = SELECTED_REAL_KIND(20) ! Quad precision
  INTEGER, PARAMETER, PUBLIC  :: Quad   = ( ( ( 1 + SIGN( 1, Quad_t ) ) / 2 ) * Quad_t ) + &
                                          ( ( ( 1 - SIGN( 1, Quad_t ) ) / 2 ) * Double )

  ! -- Expected 8-bit byte sizes of the floating point kinds
  INTEGER, PARAMETER, PUBLIC :: n_Bytes_Single = 4
  INTEGER, PARAMETER, PUBLIC :: n_Bytes_Double = 8
  INTEGER, PARAMETER, PUBLIC :: n_Bytes_Quad   = 16

  ! -- Define arrays for default definition
  INTEGER, PARAMETER, PRIVATE :: N_FP_KINDS = 3
  INTEGER, PARAMETER, DIMENSION( N_FP_KINDS ), PRIVATE :: FP_KIND_TYPES = (/ Single, &
                                                                             Double, &
                                                                             Quad    /) 
  INTEGER, PARAMETER, DIMENSION( N_FP_KINDS ), PRIVATE :: FP_BYTE_SIZES = (/ n_Bytes_Single, &
                                                                             n_Bytes_Double, &
                                                                             n_Bytes_Quad    /)

  ! -- Default values
  INTEGER, PARAMETER, PUBLIC  :: FP_Kind         = FP_KIND_TYPES( IFP )
  INTEGER, PARAMETER, PUBLIC  :: n_Bytes_FP_Kind = FP_BYTE_SIZES( IFP )

END MODULE Type_Kinds


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Type_Kinds.f90,v 2.13 2004/11/30 20:37:36 paulv Exp $
!
! $Date: 2004/11/30 20:37:36 $
!
! $Revision: 2.13 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Type_Kinds.f90,v $
! Revision 2.13  2004/11/30 20:37:36  paulv
! - Updated header documentation.
!
! Revision 2.12  2004/09/10 19:05:03  paulv
! - Removed the temporary definition of the LLong and Quad kinds. I'm sick and
!   tired of waiting for the PGI compiler to work on standard code.
!
! Revision 2.11  2004/08/13 20:28:21  paulv
! - Renamed the byte-length parameters.
! - "Undid" the temporary LLong_t and Quad_t selection since PGI *still*
!   has a bug wrt to initialization expression in parameter statements.
!
! Revision 2.10  2004/08/11 20:34:41  paulv
! - Updated.
!
! Revision 2.9  2003/08/01 13:44:19  paulv
! - Removed type_sizes() function.
!
! Revision 2.8  2001/10/24 17:33:27  paulv
! - Added "Name" to RCS keyword list.
!
! Revision 2.7  2001/09/17 19:55:57  paulv
! - Module now resides in the GENERAL module directory.
!
! Revision 2.6  2001/08/31 20:47:00  paulv
! - Updated definitions such that when the default type definition is changed
!   so is the assumed byte size of the result.
! - Commented out correct definition of type for LLong and Quad. PGI compiler
!   has a bug in it that does not allow elemental intrinsic functions to be
!   used in parameter initialisation expressions.
! - Function TYPE_SIZE altered to optional return the "expected" byte size
!   of specified type. This allows the user to check if the requested kind
!   type is supported by the compiler, i.e. if the actual and expected sizes
!   do not agree, then the kind type is unsupported.
!
! Revision 2.5  2001/07/12 16:43:16  paulv
! - Replaced possible LLong (8-byte integer) kind definition from
!     LLong   = ( ( ABS( LLong_t ) + LLong_t ) * LLong_t + &
!                 ( ABS( LLong_t ) - LLong_t ) * Long ) / &
!               ( 2 * ABS( LLong_t ) )
!   to
!     LLong   = MAX( LLong_t, Long )
! - Replaced possible Quad (16-byte floating point) kind definition from
!     Quad   = ( ( ABS( Quad_t ) + Quad_t ) * Quad_t + &
!                ( ABS( Quad_t ) - Quad_t ) * Double ) / &
!              ( 2 * ABS( Quad_t ) )
!   to
!     Quad   = MAX( Quad_t, Double )
! - Added commented definition for Quad precision kind.
! - Added comment in TYPE_SIZE function header explaining reliance on
!   1 character = 8 bits for function to return storage.
! - Removed LEN intrinsic from character definitions.
!
! Revision 2.4  2001/03/26 22:35:18  paulv
! - Renamed ip_precision and fp_precision parameters with ip_kind and
!   fp_kind.
! - Initialisation of ip_kind and fp_kind are now defined from the definitions
!   of the "fundamental" types. Currently ip_kind = Long and fp_kind = Double.
!   This was changed from the default kind typing using KIND( 0 ) and KIND( 0.0 )
!   respectively to allow the user to easily and explicitly redefine some sort
!   of default integer and floating point kind.
!
! Revision 2.3  2000/08/31 19:36:34  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 2.2  2000/08/31 15:55:34  paulv
! - Added documentation delimiters.
! - Added documentation for type_size function.
! - Changed default module visibility from PUBLIC to PRIVATE.
! - Added true default integer and floating point types.
!
! Revision 2.1  2000/08/08 17:08:55  paulv
! - Added definitions of 64-bit integers and reals. Definitions default to
!   the largest integer and real available on a system if not available.
! - Added type_size function to return the number of bytes used by a defined
!   data type - both integer and real.
!
! Revision 1.1  2000/07/12 16:08:11  paulv
! Initial checked in version
!
!
!
