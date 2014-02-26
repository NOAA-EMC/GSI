!--------------------------------------------------------------------------------
!M+
! NAME:
!       SpcCoeff_Define
!
! PURPOSE:
!       Module defining the SpcCoeff_Sensor and SpcCoeff_Spectral data structures
!       and containing routines to manipulate them.
!       
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE SpcCoeff_Define
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Error_Handler:          Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       Compare_Float_Numbers:  Module containing routines to perform equality
!                               and relational comparisons on floating point
!                               numbers.
!                               USEs: TYPE_KINDS module
!
! CONTAINS:
!       Associated_SpcCoeff:          Function to test the association status
!                                     of the pointer members of a SpcCoeff
!                                     structure.
!
!       Destroy_SpcCoeff:             Function to re-initialize an SpcCoeff
!                                     structure.
!
!       Allocate_SpcCoeff:            Function to allocate the pointer members
!                                     of an SpcCoeff structure.
!
!       Assign_SpcCoeff:              Function to copy an SpcCoeff structure.
!
!       Concatenate_SpcCoeff:         Function to concatenate two SpcCoeff
!                                     structures along the CHANNEL dimension.
!
!       Equal_SpcCoeff:               Function to test if two SpcCoeff
!                                     structures are equal.
!
!       Check_SpcCoeff_Release:       Function to check the SpcCoeff Release value.
!
!       Count_SpcCoeff_Sensors:       Subroutine to count the number of
!                                     different satellites/sensors in the
!                                     SpcCoeff data structure.
!
!       Version_SpcCoeff:             Subroutine to return a string containing
!                                     version and dimension information about
!                                     the SpcCoeff data structure.
!
! DERIVED TYPES:
!       SpcCoeff_Sensor_type:   Definition of the sensor SpcCoeff data structure.
!       ====================    Fields are...
!
!         Release:                    Sensor coefficient data release.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Scalar
!
!         Version:                    Sensor coefficient data version.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Scalar
!
!         n_Channels:                 Total number of sensor channels.
!                                     "L" dimension.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Scalar
!
!         n_Sensors:                  Number of different satellite/sensors in the
!                                     data structure.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Scalar
!
!         Sensor_Descriptor:          String variable containing a short text
!                                     description of the sensor and satellite.
!                                     Descriptors are taken from the SensorInfo
!                                     file prefix member. Examples are:
!                                       - hirs3_n17
!                                       - airs_aqua
!                                       - ssmis_f16... etc
!                                     UNITS:      N/A
!                                     TYPE:       CHARACTER( 20 )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Sensor_Type:                Sensor channel type flag. Valid parameter values
!                                     are:
!                                       MICROWAVE_SENSOR
!                                       INFRARED_SENSOR
!                                       VISIBLE_SENSOR
!                                     This replaces the "Is_Microwave_Channel" flag.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         NCEP_Sensor_ID:             An "in-house" value used at NOAA/NCEP/EMC 
!                                     to identify a satellite/sensor combination.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         WMO_Satellite_ID:           The WMO code for identifying satellite
!                                     platforms. Taken from the WMO common
!                                     code tables at:
!                                       http://www.wmo.ch/web/ddbs/Code-tables.html
!                                     The Satellite ID is from Common Code
!                                     table C-5, or code table 0 01 007 in BUFR
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         WMO_Sensor_ID:              The WMO code for identifying a satelite
!                                     sensor. Taken from the WMO common
!                                     code tables at:
!                                       http://www.wmo.ch/web/ddbs/Code-tables.html
!                                     The Sensor ID is from Common Code
!                                     table C-8, or code table 0 02 019 in BUFR
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Sensor_Channel:             This is the sensor channel number associated
!                                     with the data in the coefficient file. Helps
!                                     in identifying channels where the numbers are
!                                     not contiguous (e.g. AIRS).
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Frequency:                  Channel central frequencies in gigahertz.
!                                     UNITS:      Gigahertz (GHz)
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Wavenumber:                 Channel central frequencies in inverse
!                                     centimetres.
!                                     UNITS:      inverse centimetres (cm^-1)
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Planck_C1:                  First Planck function value for each
!                                     channel.
!                                     UNITS:      mW/(m^2.sr.cm^-1)
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Planck_C2:                  Second Planck function value for each
!                                     channel.
!                                     UNITS:      K
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Band_C1:                    Channel polychromaticity correction
!                                     offsets.
!                                     UNITS:      K
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Band_C2:                    Channel polychromaticity correction
!                                     slopes.
!                                     UNITS:      K/K
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Is_Microwave_Channel:       Array of flags indicating if a channel
!                                     is microwave.
!                                     OBSOLETE. Use the SENSOR_TYPE component.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER( Long )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Polarization:               Channel polarization flag. Valid values are
!                                       UNPOLARIZED             : Unpolarized
!                                       INTENSITY               : Intensity
!                                       FIRST_STOKES_COMPONENT  : First Stokes component (I)
!                                       SECOND_STOKES_COMPONENT : Second Stokes component (Q)
!                                       THIRD_STOKES_COMPONENT  : Third Stokes component (U)
!                                       FOURTH_STOKES_COMPONENT : Fourth Stokes component (V)
!                                       VL_POLARIZATION         : Vertical linear polarization
!                                       HL_POLARIZATION         : Horizontal linear polarization
!                                       plus45L_POLARIZATION    : +45deg. linear polarization
!                                       minus45L_POLARIZATION   : -45deg. linear polarization
!                                       VL_MIXED_POLARIZATION   : Vertical polarization at nadir; mixed off nadir
!                                       HL_MIXED_POLARIZATION   : Horizontal polarization at nadir; mixed off nadir
!                                       RC_POLARIZATION         : Right circular polarization
!                                       LC_POLARIZATION         : Left circular polarization
!                                     Note that UNPOLARIZED, INTENSITY, and
!                                     FIRST_STOKES_COMPONENT refer to the same
!                                     state.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Cosmic_Background_Radiance: Cosmic background radiance.
!                                     UNITS:      mW/(m^2.sr.cm^-1)
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Is_Solar_Channel:           Array of flags indicating if a channel
!                                     is sensitive to solar contribution.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER( Long )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Solar_Irradiance:           Kurucz solar irradiance source function.
!                                     UNITS:      mW/(m^2.cm^-1)
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!
!       SpcCoeff_Spectral_type:   Definition of the spectral SpcCoeff data structure.
!       ======================    Fields are...
!
!         Release:                    Spectral coefficient data release.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Scalar
!
!         Version:                    Spectral coefficient data version.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Scalar
!
!         n_Channels:                 Total number of sensor channels.
!                                     "L" dimension.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Scalar
!
!         n_Nodes:                    Total number of monochromatic nodes.
!                                     "N" dimension.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Scalar
!
!         Max_Channels_pr_Node:       The maximum number of sensor channels that
!                                     use a particular node.
!                                     "LN" dimension.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Scalar
!
!         n_Sensors:                  Number of different satellite/sensors in the
!                                     data structure.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Scalar
!
!         Channel data arrays
!         -------------------
!
!         Sensor_Descriptor:          String variable containing a short text
!                                     description of the sensor and satellite.
!                                     Descriptors are taken from the SensorInfo
!                                     file prefix member. Examples are:
!                                       - hirs3_n17
!                                       - airs_aqua
!                                       - ssmis_f16... etc
!                                     UNITS:      N/A
!                                     TYPE:       CHARACTER( 20 )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         NCEP_Sensor_ID:             An "in-house" value used at NOAA/NCEP/EMC 
!                                     to identify a satellite/sensor combination.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         WMO_Satellite_ID:           The WMO code for identifying satellite
!                                     platforms. Taken from the WMO common
!                                     code tables at:
!                                       http://www.wmo.ch/web/ddbs/Code-tables.html
!                                     The Satellite ID is from Common Code
!                                     table C-5, or code table 0 01 007 in BUFR
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         WMO_Sensor_ID:              The WMO code for identifying a satelite
!                                     sensor. Taken from the WMO common
!                                     code tables at:
!                                       http://www.wmo.ch/web/ddbs/Code-tables.html
!                                     The Sensor ID is from Common Code
!                                     table C-8, or code table 0 02 019 in BUFR
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Sensor_Channel:             This is the sensor channel number associated
!                                     with the data in the coefficient file. Helps
!                                     in identifying channels where the numbers are
!                                     not contiguous (e.g. AIRS).
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Band_C1:                    Channel polychromaticity correction
!                                     offsets.
!                                     UNITS:      K
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Band_C2:                    Channel polychromaticity correction
!                                     slopes.
!                                     UNITS:      K/K
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Is_Solar_Channel:           Array of flags indicating if a channel
!                                     is sensitive to solar contribution.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER( Long )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Solar_Irradiance:           Kurucz solar irradiance source function.
!                                     UNITS:      mW/(m^2.cm^-1)
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Channels)
!                                     ATTRIBUTES: POINTER
!
!         Node data arrays
!         ----------------
!
!         Sensor_Type:                Sensor channel type flag. Valid parameter values
!                                     are:
!                                       MICROWAVE_SENSOR
!                                       INFRARED_SENSOR
!                                       VISIBLE_SENSOR
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Rank-1 (n_Nodes)
!                                     ATTRIBUTES: POINTER
!
!         Polarization:               Node polarization flag. Valid values are
!                                       UNPOLARIZED             : Unpolarized
!                                       INTENSITY               : Intensity
!                                       FIRST_STOKES_COMPONENT  : First Stokes component (I)
!                                       SECOND_STOKES_COMPONENT : Second Stokes component (Q)
!                                       THIRD_STOKES_COMPONENT  : Third Stokes component (U)
!                                       FOURTH_STOKES_COMPONENT : Fourth Stokes component (V)
!                                       VL_POLARIZATION         : Vertical linear polarization
!                                       HL_POLARIZATION         : Horizontal linear polarization
!                                       plus45L_POLARIZATION    : +45deg. linear polarization
!                                       minus45L_POLARIZATION   : -45deg. linear polarization
!                                       VL_MIXED_POLARIZATION   : Vertical polarization at nadir; mixed off nadir
!                                       HL_MIXED_POLARIZATION   : Horizontal polarization at nadir; mixed off nadir
!                                       RC_POLARIZATION         : Right circular polarization
!                                       LC_POLARIZATION         : Left circular polarization
!                                     Note that UNPOLARIZED, INTENSITY, and
!                                     FIRST_STOKES_COMPONENT refer to the same
!                                     state.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Rank-1 (n_Nodes)
!                                     ATTRIBUTES: POINTER
!
!         n_Channels_per_Node:        The number of sensor channels that use
!                                     each node.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Rank-1 (n_Nodes)
!                                     ATTRIBUTES: POINTER
!
!         Channel_Node_Map:           The index array to map channels to nodes.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Rank-2 (Max_Channels_per_Node x n_Nodes)
!                                     ATTRIBUTES: POINTER
!
!         MW_and_IR_Node_Index:       The index array containing the combined microwave
!                                     and infrared node indices. Each is mapped separately
!                                     (i.e. 1 to n_MW/IR_Nodes) as the algorithms that
!                                     use this information to compute the monochromatic
!                                     gas absorption are slightly different for the
!                                     microwave and infrared.
!                                     UNITS:      N/A
!                                     TYPE:       INTEGER
!                                     DIMENSION:  Rank-1 (n_Nodes)
!                                     ATTRIBUTES: POINTER
!
!         Frequency:                  Node frequencies in gigahertz.
!                                     UNITS:      Gigahertz (GHz)
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Nodes)
!                                     ATTRIBUTES: POINTER
!
!         Wavenumber:                 Node frequencies in inverse
!                                     centimetres.
!                                     UNITS:      inverse centimetres (cm^-1)
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Nodes)
!                                     ATTRIBUTES: POINTER
!
!         Planck_C1:                  First Planck function value for each
!                                     node.
!                                     UNITS:      mW/(m^2.sr.cm^-1)
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Nodes)
!                                     ATTRIBUTES: POINTER
!
!         Planck_C2:                  Second Planck function value for each
!                                     node.
!                                     UNITS:      K
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Nodes)
!                                     ATTRIBUTES: POINTER
!
!         Cosmic_Background_Radiance: Cosmic background radiance for each node.
!                                     UNITS:      mW/(m^2.sr.cm^-1)
!                                     TYPE:       REAL( Double )
!                                     DIMENSION:  Rank-1 (n_Nodes)
!                                     ATTRIBUTES: POINTER
!
!
!       *!IMPORTANT!*
!       -------------
!       Note the SpcCoeff data types are PUBLIC and their members are not
!       encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structures and their data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structures. *But*, it is recommended that the user
!       destroy, allocate, assign, and concatenate the structures
!       using only the routines in this module where possible to
!       eliminate -- or at least minimise -- the possibility of 
!       memory leakage since most of the structure members are
!       pointers.
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
! FILES ACCESSED:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
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
!--------------------------------------------------------------------------------

MODULE SpcCoeff_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Error_Handler
  USE Compare_Float_Numbers


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Public procedures to manipulate the SpcCoeff structure
  PUBLIC :: Associated_SpcCoeff
  PUBLIC :: Destroy_SpcCoeff
  PUBLIC :: Allocate_SpcCoeff
  PUBLIC :: Assign_SpcCoeff
  PUBLIC :: Concatenate_SpcCoeff
  PUBLIC :: Equal_SpcCoeff
  PUBLIC :: Check_SpcCoeff_Release
  PUBLIC :: Count_SpcCoeff_Sensors
  PUBLIC :: Version_SpcCoeff


  ! -------------------
  ! Procedure overloads
  ! -------------------

  ! -- PRIVATE procedures
  INTERFACE Clear_SpcCoeff
    MODULE PROCEDURE Clear_Spectral
    MODULE PROCEDURE Clear_Sensor
  END INTERFACE Clear_SpcCoeff

  ! -- PUBLIC procedures
  INTERFACE Associated_SpcCoeff
    MODULE PROCEDURE Associated_Spectral
    MODULE PROCEDURE Associated_Sensor
  END INTERFACE Associated_SpcCoeff

  INTERFACE Destroy_SpcCoeff
    MODULE PROCEDURE Destroy_Spectral
    MODULE PROCEDURE Destroy_Sensor
  END INTERFACE Destroy_SpcCoeff

  INTERFACE Allocate_SpcCoeff
    MODULE PROCEDURE Allocate_Spectral
    MODULE PROCEDURE Allocate_Sensor
  END INTERFACE Allocate_SpcCoeff

  INTERFACE Assign_SpcCoeff
    MODULE PROCEDURE Assign_Spectral
    MODULE PROCEDURE Assign_Sensor
  END INTERFACE Assign_SpcCoeff

  INTERFACE Concatenate_SpcCoeff
    MODULE PROCEDURE Concatenate_Spectral
    MODULE PROCEDURE Concatenate_Sensor
  END INTERFACE Concatenate_SpcCoeff

  INTERFACE Equal_SpcCoeff
    MODULE PROCEDURE Equal_Spectral
    MODULE PROCEDURE Equal_Sensor
  END INTERFACE Equal_SpcCoeff

  INTERFACE Check_SpcCoeff_Release
    MODULE PROCEDURE Check_Spectral
    MODULE PROCEDURE Check_Sensor
  END INTERFACE Check_SpcCoeff_Release

  INTERFACE Count_SpcCoeff_Sensors
    MODULE PROCEDURE Count_Spectral
    MODULE PROCEDURE Count_Sensor
  END INTERFACE Count_SpcCoeff_Sensors

  INTERFACE Version_SpcCoeff
    MODULE PROCEDURE Version_Spectral
    MODULE PROCEDURE Version_Sensor
  END INTERFACE Version_SpcCoeff


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: SpcCoeff_Define.f90,v 6.3 2005/07/05 23:54:51 paulv Exp $'

  ! -- SpcCoeff scalar member invalid value
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Sensor descriptor component string length
  INTEGER, PRIVATE, PARAMETER :: DL = 20

  ! -- Current valid release and version numbers
  INTEGER, PRIVATE, PARAMETER :: SPCCOEFF_SENSOR_RELEASE = 6  ! This determines structure and file formats.
  INTEGER, PRIVATE, PARAMETER :: SPCCOEFF_SENSOR_VERSION = 1  ! This is just the data version.

  INTEGER, PRIVATE, PARAMETER :: SPCCOEFF_SPECTRAL_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PRIVATE, PARAMETER :: SPCCOEFF_SPECTRAL_VERSION = 1  ! This is just the data version.

  ! -- Data type identifiers
  INTEGER( Long ), PRIVATE, PARAMETER :: STRING_TYPE = 7_Long
  INTEGER( Long ), PRIVATE, PARAMETER :: DOUBLE_TYPE = 5_Long
  INTEGER( Long ), PRIVATE, PARAMETER :: SINGLE_TYPE = 4_Long
  INTEGER( Long ), PRIVATE, PARAMETER :: LONG_TYPE   = 3_Long

  ! -- ASCII codes for Version routine
  INTEGER, PRIVATE, PARAMETER :: CARRIAGE_RETURN = 13
  INTEGER, PRIVATE, PARAMETER :: LINEFEED = 10


  ! ----------------------------------------
  ! PUBLIC Module parameters for SENSOR type
  ! ----------------------------------------

  ! -- Number of SpcCoeff pointer data items
  INTEGER( Long ), PUBLIC, PARAMETER :: N_SPCCOEFF_SENSOR_ITEMS = 16_Long

  ! -- Data types of the SpcCoeff pointer data
  INTEGER( Long ), PUBLIC, PARAMETER, DIMENSION( N_SPCCOEFF_SENSOR_ITEMS ) :: &
    SPCCOEFF_SENSOR_DATA_TYPE = (/ STRING_TYPE, &  ! Sensor_Descriptor
                                   LONG_TYPE,   &  ! Sensor_Type
                                   LONG_TYPE,   &  ! NCEP_Sensor_ID
                                   LONG_TYPE,   &  ! WMO_Satellite_ID
                                   LONG_TYPE,   &  ! WMO_Sensor_ID
                                   LONG_TYPE,   &  ! Sensor_Channel
                                   DOUBLE_TYPE, &  ! Frequency
                                   DOUBLE_TYPE, &  ! Wavenumber
                                   DOUBLE_TYPE, &  ! Planck_C1
                                   DOUBLE_TYPE, &  ! Planck_C2
                                   DOUBLE_TYPE, &  ! Band_C1
                                   DOUBLE_TYPE, &  ! Band_C2
                                   LONG_TYPE,   &  ! Polarization
                                   DOUBLE_TYPE, &  ! Cosmic_Background_Radiance
                                   LONG_TYPE,   &  ! Is_Solar_Channel
                                   DOUBLE_TYPE /)  ! Solar_Irradiance

  ! -- Names of the pointer data items (for error processing)
  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( N_SPCCOEFF_SENSOR_ITEMS ) :: &
    SPCCOEFF_SENSOR_DATA_NAME = (/ 'Sensor_Descriptor         ', &
                                   'Sensor Type               ', & 
                                   'NCEP_Sensor_ID            ', & 
                                   'WMO_Satellite_ID          ', & 
                                   'WMO_Sensor_id             ', & 
                                   'Sensor_Channel            ', & 
                                   'Frequency                 ', & 
                                   'Wavenumber                ', & 
                                   'Planck_C1                 ', & 
                                   'Planck_C2                 ', & 
                                   'Band_C1                   ', & 
                                   'Band_C2                   ', & 
                                   'Polarization              ', & 
                                   'Cosmic_Background_Radiance', & 
                                   'Is_Solar_Channel          ', & 
                                   'Solar_Irradiance          ' /) 


  ! ----------------------------------------
  ! PUBLIC Module parameters for SENSOR type
  ! ----------------------------------------

  INTEGER( Long ), PUBLIC, PARAMETER :: N_SPCCOEFF_SPECTRAL_ITEMS = 24_Long

  ! -- Data types of the SpcCoeff pointer data
  INTEGER( Long ), PUBLIC, PARAMETER, DIMENSION( N_SPCCOEFF_SPECTRAL_ITEMS ) :: &
    SPCCOEFF_SPECTRAL_DATA_TYPE = (/ STRING_TYPE, &  ! Sensor_Descriptor         
                                     LONG_TYPE,   &  ! Sensor_Type               
                                     LONG_TYPE,   &  ! NCEP_Sensor_ID            
                                     LONG_TYPE,   &  ! WMO_Satellite_ID          
                                     LONG_TYPE,   &  ! WMO_Sensor_ID             
                                     LONG_TYPE,   &  ! Sensor_Channel            
                                     DOUBLE_TYPE, &  ! Frequency                 
                                     DOUBLE_TYPE, &  ! Wavenumber                
                                     DOUBLE_TYPE, &  ! Planck_C1                 
                                     DOUBLE_TYPE, &  ! Planck_C2                 
                                     DOUBLE_TYPE, &  ! Band_C1                   
                                     DOUBLE_TYPE, &  ! Band_C2                   
                                     LONG_TYPE,   &  ! Polarization              
                                     LONG_TYPE,   &  ! Is_Solar_Channel          
                                     DOUBLE_TYPE, &  ! Solar_Irradiance          
                                     LONG_TYPE,   &  ! MW_and_IR_Channel_Index   
                                     LONG_TYPE,   &  ! n_Channels_per_Node       
                                     LONG_TYPE,   &  ! Channel_Node_Map          
                                     LONG_TYPE,   &  ! MW_and_IR_Node_Index      
                                     DOUBLE_TYPE, &  ! Node_Frequency            
                                     DOUBLE_TYPE, &  ! Node_Wavenumber           
                                     DOUBLE_TYPE, &  ! Node_Planck_C1            
                                     DOUBLE_TYPE, &  ! Node_Planck_C2            
                                     DOUBLE_TYPE /)  ! Cosmic_Background_Radiance


  ! -- Names of the pointer data items (for error processing)
  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( N_SPCCOEFF_SPECTRAL_ITEMS ) :: &
    SPCCOEFF_SPECTRAL_DATA_NAME = (/ 'Sensor_Descriptor         ', &
                                     'Sensor_Type               ', &
                                     'NCEP_Sensor_ID            ', &
                                     'WMO_Satellite_ID          ', &
                                     'WMO_Sensor_ID             ', &
                                     'Sensor_Channel            ', &
                                     'Frequency                 ', &
                                     'Wavenumber                ', &
                                     'Planck_C1                 ', &
                                     'Planck_C2                 ', &
                                     'Band_C1                   ', &
                                     'Band_C2                   ', &
                                     'Polarization              ', &
                                     'Is_Solar_Channel          ', &
                                     'Solar_Irradiance          ', &
                                     'MW_and_IR_Channel_Index   ', &
                                     'n_Channels_per_Node       ', &
                                     'Channel_Node_Map          ', &
                                     'MW_and_IR_Node_Index      ', &
                                     'Node_Frequency            ', &
                                     'Node_Wavenumber           ', &
                                     'Node_Planck_C1            ', &
                                     'Node_Planck_C2            ', &
                                     'Cosmic_Background_Radiance' /)



  ! ------------------------------
  ! Other PUBLIC Module parameters
  ! ------------------------------

  ! -- SpcCoeff file type IDs
  INTEGER, PUBLIC, PARAMETER ::  INVALID_FILE_TYPE = 0
  INTEGER, PUBLIC, PARAMETER ::   SENSOR_FILE_TYPE = 1
  INTEGER, PUBLIC, PARAMETER :: SPECTRAL_FILE_TYPE = 2


  ! -- Invalid sensor ids
  INTEGER, PUBLIC, PARAMETER :: INVALID_NCEP_SENSOR_ID   = -1
  INTEGER, PUBLIC, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PUBLIC, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047

  ! -- The instrument types
  INTEGER, PUBLIC, PARAMETER :: N_SENSOR_TYPES = 3

  INTEGER, PUBLIC, PARAMETER :: INVALID_SENSOR   = 0
  INTEGER, PUBLIC, PARAMETER :: MICROWAVE_SENSOR = 1
  INTEGER, PUBLIC, PARAMETER :: INFRARED_SENSOR  = 2
  INTEGER, PUBLIC, PARAMETER :: VISIBLE_SENSOR   = 3

  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_SENSOR_TYPES ) :: &
    SENSOR_TYPE_NAME = (/ 'Invalid  ', &
                          'Microwave', &
                          'Infrared ', &
                          'Visible  ' /)
 
  ! -- The polarisation flags
  INTEGER, PUBLIC, PARAMETER :: N_POLARIZATION_TYPES = 12

  INTEGER, PUBLIC, PARAMETER :: INVALID_POLARIZATION    = 0
  INTEGER, PUBLIC, PARAMETER :: UNPOLARIZED             = 1
  INTEGER, PUBLIC, PARAMETER :: INTENSITY               = UNPOLARIZED
  INTEGER, PUBLIC, PARAMETER :: FIRST_STOKES_COMPONENT  = UNPOLARIZED
  INTEGER, PUBLIC, PARAMETER :: SECOND_STOKES_COMPONENT = 2
  INTEGER, PUBLIC, PARAMETER :: THIRD_STOKES_COMPONENT  = 3
  INTEGER, PUBLIC, PARAMETER :: FOURTH_STOKES_COMPONENT = 4
  INTEGER, PUBLIC, PARAMETER :: VL_POLARIZATION         = 5
  INTEGER, PUBLIC, PARAMETER :: HL_POLARIZATION         = 6
  INTEGER, PUBLIC, PARAMETER :: plus45L_POLARIZATION    = 7
  INTEGER, PUBLIC, PARAMETER :: minus45L_POLARIZATION   = 8
  INTEGER, PUBLIC, PARAMETER :: VL_MIXED_POLARIZATION   = 9
  INTEGER, PUBLIC, PARAMETER :: HL_MIXED_POLARIZATION   = 10
  INTEGER, PUBLIC, PARAMETER :: RC_POLARIZATION         = 11
  INTEGER, PUBLIC, PARAMETER :: LC_POLARIZATION         = 12

  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_POLARIZATION_TYPES ) :: &
    POLARIZATION_TYPE_NAME = (/ 'Invalid                                          ', &
                                'Unpolarized/Intensity/First Stokes component (I) ', &
                                'Second Stokes component (Q)                      ', &
                                'Third Stokes component (U)                       ', &
                                'Fourth Stokes component (V)                      ', &
                                'Vertical linear polarization                     ', &
                                'Horizontal linear polarization                   ', &
                                '+45deg. linear polarization                      ', &
                                '-45deg. linear polarization                      ', &
                                'Vertical polarization at nadir; mixed off nadir  ', &
                                'Horizontal polarization at nadir; mixed off nadir', &
                                'Right circular polarization                      ', &
                                'Left circular polarization                       ' /)
 

  ! ------------------------------------
  ! SpcCoeff_Sensor data type definition
  ! ------------------------------------

  TYPE, PUBLIC :: SpcCoeff_Sensor_type
    INTEGER :: n_Allocates = 0

    INTEGER( Long ) :: Release = SPCCOEFF_SENSOR_RELEASE
    INTEGER( Long ) :: Version = SPCCOEFF_SENSOR_VERSION

    INTEGER( Long ) :: Sensor_Descriptor_StrLen = DL

    ! -- Dimensions
    INTEGER( Long ) :: n_Channels = 0  ! L  dimension

    ! -- Number of represented sensors
    INTEGER( Long ) :: n_Sensors = 0

    ! -- Channel data arrays
    CHARACTER( DL ), POINTER, DIMENSION( : ) :: Sensor_Descriptor          => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Sensor_Type                => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : ) :: NCEP_Sensor_ID             => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : ) :: WMO_Satellite_ID           => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : ) :: WMO_Sensor_ID              => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Sensor_Channel             => NULL()  ! L
    REAL( Double ),  POINTER, DIMENSION( : ) :: Frequency                  => NULL()  ! L
    REAL( Double ),  POINTER, DIMENSION( : ) :: Wavenumber                 => NULL()  ! L
    REAL( Double ),  POINTER, DIMENSION( : ) :: Planck_C1                  => NULL()  ! L
    REAL( Double ),  POINTER, DIMENSION( : ) :: Planck_C2                  => NULL()  ! L
    REAL( Double ),  POINTER, DIMENSION( : ) :: Band_C1                    => NULL()  ! L
    REAL( Double ),  POINTER, DIMENSION( : ) :: Band_C2                    => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Is_Microwave_Channel       => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Polarization               => NULL()  ! L
    REAL( Double ),  POINTER, DIMENSION( : ) :: Cosmic_Background_Radiance => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : ) :: Is_Solar_Channel           => NULL()  ! L
    REAL( Double ),  POINTER, DIMENSION( : ) :: Solar_Irradiance           => NULL()  ! L
  END TYPE SpcCoeff_Sensor_type


  ! --------------------------------------
  ! SpcCoeff_Spectral data type definition
  ! --------------------------------------

  TYPE, PUBLIC :: SpcCoeff_Spectral_type
    INTEGER :: n_Allocates = 0

    INTEGER( Long ) :: Release = SPCCOEFF_SPECTRAL_RELEASE
    INTEGER( Long ) :: Version = SPCCOEFF_SPECTRAL_VERSION

    INTEGER( Long ) :: Sensor_Descriptor_StrLen = DL

    ! -- Dimensions
    INTEGER( Long ) :: n_Channels            = 0  ! L  dimension
    INTEGER( Long ) :: n_Nodes               = 0  ! N  dimension
    INTEGER( Long ) :: Max_Channels_per_Node = 0  ! LN dimension

    ! -- Number of represented sensors
    INTEGER( Long ) :: n_Sensors = 0

    ! -- Channel data arrays
    CHARACTER( DL ), POINTER, DIMENSION( : )    :: Sensor_Descriptor          => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : )    :: Sensor_Type                => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : )    :: NCEP_Sensor_ID             => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : )    :: WMO_Satellite_ID           => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : )    :: WMO_Sensor_ID              => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : )    :: Sensor_Channel             => NULL()  ! L
    REAL( Double ),  POINTER, DIMENSION( : )    :: Frequency                  => NULL()  ! L
    REAL( Double ),  POINTER, DIMENSION( : )    :: Wavenumber                 => NULL()  ! L
    REAL( Double ),  POINTER, DIMENSION( : )    :: Planck_C1                  => NULL()  ! L
    REAL( Double ),  POINTER, DIMENSION( : )    :: Planck_C2                  => NULL()  ! L
    REAL( Double ),  POINTER, DIMENSION( : )    :: Band_C1                    => NULL()  ! L
    REAL( Double ),  POINTER, DIMENSION( : )    :: Band_C2                    => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : )    :: Polarization               => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : )    :: Is_Solar_Channel           => NULL()  ! L
    REAL( Double ),  POINTER, DIMENSION( : )    :: Solar_Irradiance           => NULL()  ! L
    INTEGER( Long ), POINTER, DIMENSION( : )    :: MW_and_IR_Channel_Index    => NULL()  ! L

    ! -- Node data arrays
    INTEGER( Long ), POINTER, DIMENSION( : )    :: n_Channels_per_Node        => NULL()  ! N
    INTEGER( Long ), POINTER, DIMENSION( :, : ) :: Channel_Node_Map           => NULL()  ! LN x N
    INTEGER( Long ), POINTER, DIMENSION( : )    :: MW_and_IR_Node_Index       => NULL()  ! N
    REAL( Double ),  POINTER, DIMENSION( : )    :: Node_Frequency             => NULL()  ! N
    REAL( Double ),  POINTER, DIMENSION( : )    :: Node_Wavenumber            => NULL()  ! N
    REAL( Double ),  POINTER, DIMENSION( : )    :: Node_Planck_C1             => NULL()  ! N
    REAL( Double ),  POINTER, DIMENSION( : )    :: Node_Planck_C2             => NULL()  ! N
    REAL( Double ),  POINTER, DIMENSION( : )    :: Cosmic_Background_Radiance => NULL()  ! N

  END TYPE SpcCoeff_Spectral_type

CONTAINS




!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       Clear_SpcCoeff
!
! PURPOSE:
!       Subroutine to clear the scalar members of an SpcCoeff structure.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_SpcCoeff( SpcCoeff) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       SpcCoeff:    SpcCoeff structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       SpcCoeff_Sensor_type
!                                  OR
!                                SpcCoeff_Spectral_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
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
! COMMENTS:
!       Note the INTENT on the output SpcCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_Sensor( SpcCoeff )
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN OUT ) :: SpcCoeff

    SpcCoeff%Sensor_Descriptor_StrLen = DL
    SpcCoeff%n_Channels = 0
    SpcCoeff%n_Sensors  = 0
  END SUBROUTINE Clear_Sensor


  SUBROUTINE Clear_Spectral( SpcCoeff )
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN OUT ) :: SpcCoeff

    SpcCoeff%Sensor_Descriptor_StrLen = DL
    SpcCoeff%n_Channels            = 0
    SpcCoeff%n_Nodes               = 0
    SpcCoeff%Max_Channels_per_Node = 0
    SpcCoeff%n_Sensors             = 0
  END SUBROUTINE Clear_Spectral





!----------------------------------------------------------------------------------
!
! NAME:
!       Construct_Message
!
! PURPOSE:
!       Function to to construct a standard message when a floating point
!       number comparison fails in the Equal_SpcCoeff() functions.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Message = Construct_Message( Label, i, x1, x2, Node )
!
! INPUT ARGUMENTS:
!       Label:       Character string containing the name of the data
!                    that was compared.
!                    UNITS:      N/A
!                    TYPE:       CHARACTER( * )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!       i:           The index (channel or node) in the SpcCoeff structure
!                    at which the floating point comparison failed.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!       x1, x2:      The numbers for which the floating point comparison
!                    failed. These are used to determine the best floating
!                    point format with which to output the numbers in the
!                    message.
!                    UNITS:      N/A
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Node:        Optional flag to indicate whether the numbers were
!                    channel or node-based.
!                    If == 0,             channel-based (default)
!                       == SET parameter, node-based
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Message:     Character string containing the constructed error
!                    that is passed into the Display_Message subroutine.
!                    UNITS:      N/A
!                    TYPE:       CHARACTER( * )
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Sep-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  FUNCTION Construct_Message( Label, i, x1, x2, Node ) RESULT( Message )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    CHARACTER( * ),    INTENT( IN ) :: Label
    INTEGER,           INTENT( IN ) :: i
    REAL( fp_kind ),   INTENT( IN ) :: x1, x2
    INTEGER, OPTIONAL, INTENT( IN ) :: Node


    ! ---------------
    ! Function result
    ! ---------------

    CHARACTER( 256 ) :: Message


    ! ----------------
    ! Local parameters
    ! ----------------

    INTEGER,         PARAMETER :: WIDTH = 20
    REAL( fp_kind ), PARAMETER :: HUNDRED = 100.0_fp_kind


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 6 ) :: Real_Format
    CHARACTER( 7 ) :: Base_Type
    CHARACTER( LEN(SPCCOEFF_SENSOR_DATA_NAME) ) :: Data_Name
    CHARACTER( 256 ) :: Format_String
    INTEGER :: n
    REAL( fp_kind ) :: dx



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! -- Determine if index is for a channel or node
    Base_Type = 'channel'
    IF ( PRESENT( Node ) ) THEN
      IF ( Node == SET ) Base_Type = 'node'
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CONSTRUCT MESSAGE --                          #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------------
    ! Determine the print format for the floating
    ! point numbers based on their magnitudes
    ! -------------------------------------------

    n = WIDTH - INT( MAX( LOG10( x1 ), LOG10( x2 ), 0.0_fp_kind ) ) - 2
    WRITE( Real_Format, FMT = '( "f", i2.2, ".", i2.2 )' ) WIDTH, n


    ! ------------------------------------------------------
    ! Construct the format string used to create the message
    ! ------------------------------------------------------

    Data_Name = TRIM( Label )
    Format_String = '( "'//Data_Name//' difference, ", '//Real_Format//&
                    ', " vs. ", '//Real_Format//', "(", es9.2, "% diff), '//&
                    TRIM( Base_Type )//' index # ", i4 )'


    ! ------------------
    ! Create the message
    ! ------------------

    dx = HUNDRED * ( x1 - x2 ) / x1
    WRITE( Message, FMT = TRIM( Format_String ) ) x1, x2, dx, i

  END FUNCTION Construct_Message





!----------------------------------------------------------------------------------
!
! NAME:
!       Count_Using_NCEP_ID
!
! PURPOSE:
!       Function to count the number of sensors in an SpcCoeff structure
!       (either sensor or spectral) based on the NCEP Sensor ID.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       n_Sensors = Count_Using_NCEP_ID( NCEP_Sensor_Id )
!
! INPUT ARGUMENTS:
!       NCEP_Sensor_ID:  The list of NCEP Sensor IDs from the SpcCoeff
!                        structure.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       n_Sensors:       The number of unique sensors represented in the
!                        NCEP sensor ID list.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Jul-2005
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  FUNCTION Count_Using_NCEP_ID( NCEP_Sensor_Id ) RESULT( n_Sensors )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    INTEGER, DIMENSION( : ), INTENT( IN ) :: NCEP_Sensor_ID


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: n_Sensors


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: l



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! -- Check that ALL the values are valid 
    IF ( ANY( NCEP_Sensor_ID == INVALID ) ) THEN
      n_Sensors = INVALID
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- COUNT THE SENSORS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Initialise the sensor count
    ! ---------------------------

    n_Sensors = 1


    ! ------------------
    ! Loop over channels
    ! ------------------

    DO l = 2, SIZE( NCEP_Sensor_ID )

      ! -- Only increment sensor count if the current channel's
      ! -- value has not been previously encountered
      IF ( ALL( NCEP_Sensor_ID(1:l-1) /= NCEP_Sensor_ID(l) ) ) THEN
        n_Sensors = n_Sensors + 1
      END IF

    END DO

  END FUNCTION Count_Using_NCEP_ID





!----------------------------------------------------------------------------------
!
! NAME:
!       Count_Using_WMO_ID
!
! PURPOSE:
!       Function to count the number of sensors in an SpcCoeff structure
!       (either sensor or spectral) based on the WMO satelltie and
!       sensor IDs.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       n_Sensors = Count_Using_WMO_ID( WMO_Satellite_ID, WMO_Sensor_Id )
!
! INPUT ARGUMENTS:
!       WMO_Satellite_ID:  The list of WMO Satellite IDs from the SpcCoeff
!                          structure.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Rank-1
!                          ATTRIBUTES: INTENT( IN )
!
!       WMO_Sensor_ID:     The list of WMO Sensor IDs from the SpcCoeff
!                          structure.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Same as WMO_Satellite_ID
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       n_Sensors:         The number of unique sensors represented in
!                          the WMO ID lists.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Jul-2005
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  FUNCTION Count_Using_WMO_ID( WMO_Satellite_ID, WMO_Sensor_Id ) RESULT( n_Sensors )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    INTEGER, DIMENSION( : ), INTENT( IN ) :: WMO_Satellite_ID
    INTEGER, DIMENSION( : ), INTENT( IN ) :: WMO_Sensor_ID


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: n_Sensors


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: l, j, n
    INTEGER, DIMENSION( SIZE( WMO_Satellite_ID ) ) :: Idx



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! -- Check that ALL the values are valid 
    IF ( ANY( WMO_Satellite_ID == INVALID_WMO_SATELLITE_ID ) .OR. &
         ANY( WMO_Sensor_ID    == INVALID_WMO_SENSOR_ID    )      ) THEN
      n_Sensors = INVALID
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- COUNT THE SENSORS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Initialise the sensor count
    ! ---------------------------

    n_Sensors = 1


    ! ------------------
    ! Loop over channels
    ! ------------------

    DO l = 2, SIZE( WMO_Satellite_ID )

      ! -- Count the number of channels with the SAME
      ! -- WMO SENSOR ID as the current channel
      n = COUNT( WMO_Sensor_ID(1:l-1) == WMO_Sensor_ID(l) )

      ! -- How many channels have the same WMO SENSOR ID?
      IF ( n == 0 ) THEN

        ! -- None. Increment the sensor count
        n_Sensors = n_Sensors + 1

      ELSE

        ! -- Some channels have the same SENSOR ID.
        ! -- Now get those corresponding array indices
        Idx(1:n) = PACK( (/ ( j, j=1,l-1 ) /), &
                         WMO_Sensor_ID(1:l-1) == WMO_Sensor_ID(l) )

        ! -- If ALL of the previous channels' SATELLITE ID
        ! -- values are different from the current channel,
        ! -- then we have a different sensor so increment
        ! -- the sensor count.
        IF ( ALL( WMO_Satellite_ID(Idx(1:n)) /= WMO_Satellite_ID(l) ) ) THEN
          n_Sensors = n_Sensors + 1
        END IF

      END IF

    END DO

  END FUNCTION Count_Using_WMO_ID





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!S+
! NAME:
!       Associated_SpcCoeff
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       SpcCoeff structure.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_SpcCoeff( SpcCoeff,           &  ! Input
!                                                 ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       SpcCoeff:            SpcCoeff structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       SpcCoeff_Sensor_type
!                                          OR
!                                        SpcCoeff_Spectral_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            SpcCoeff structure pointer members are associated.
!                            The default is to test if ALL the pointer members
!                            are associated.
!                            If ANY_Test = 0, test if ALL the pointer members
!                                             are associated.  (DEFAULT)
!                               ANY_Test = 1, test if ANY of the pointer members
!                                             are associated.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the SpcCoeff pointer members.
!                            .TRUE.  - if ALL the SpcCoeff pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the SpcCoeff pointer
!                                      members are associated.
!                            .FALSE. - some or all of the SpcCoeff pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Mar-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_Sensor( SpcCoeff,  & ! Input
                              ANY_Test ) & ! Optional input
                            RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN ) :: SpcCoeff

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN ) :: ANY_Test


    ! ---------------
    ! Function result
    ! ---------------

    LOGICAL :: Association_Status


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: ALL_Test



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! -- Default is to test ALL the pointer members
    ! -- for a true association status....
    ALL_Test = .TRUE.

    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( SpcCoeff%Sensor_Descriptor          ) .AND. &
           ASSOCIATED( SpcCoeff%Sensor_Type                ) .AND. &
           ASSOCIATED( SpcCoeff%NCEP_Sensor_ID             ) .AND. &
           ASSOCIATED( SpcCoeff%WMO_Satellite_ID           ) .AND. &
           ASSOCIATED( SpcCoeff%WMO_Sensor_ID              ) .AND. &
           ASSOCIATED( SpcCoeff%Sensor_Channel             ) .AND. &
           ASSOCIATED( SpcCoeff%Frequency                  ) .AND. &
           ASSOCIATED( SpcCoeff%Wavenumber                 ) .AND. &
           ASSOCIATED( SpcCoeff%Planck_C1                  ) .AND. &
           ASSOCIATED( SpcCoeff%Planck_C2                  ) .AND. &
           ASSOCIATED( SpcCoeff%Band_C1                    ) .AND. &
           ASSOCIATED( SpcCoeff%Band_C2                    ) .AND. &
           ASSOCIATED( SpcCoeff%Is_Microwave_Channel       ) .AND. &
           ASSOCIATED( SpcCoeff%Polarization               ) .AND. &
           ASSOCIATED( SpcCoeff%Cosmic_Background_Radiance ) .AND. &
           ASSOCIATED( SpcCoeff%Is_Solar_Channel           ) .AND. &
           ASSOCIATED( SpcCoeff%Solar_Irradiance           )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( SpcCoeff%Sensor_Descriptor          ) .OR. &
           ASSOCIATED( SpcCoeff%Sensor_Type                ) .OR. &
           ASSOCIATED( SpcCoeff%NCEP_Sensor_ID             ) .OR. &
           ASSOCIATED( SpcCoeff%WMO_Satellite_ID           ) .OR. &
           ASSOCIATED( SpcCoeff%WMO_Sensor_ID              ) .OR. &
           ASSOCIATED( SpcCoeff%Sensor_Channel             ) .OR. &
           ASSOCIATED( SpcCoeff%Frequency                  ) .OR. &
           ASSOCIATED( SpcCoeff%Wavenumber                 ) .OR. &
           ASSOCIATED( SpcCoeff%Planck_C1                  ) .OR. &
           ASSOCIATED( SpcCoeff%Planck_C2                  ) .OR. &
           ASSOCIATED( SpcCoeff%Band_C1                    ) .OR. &
           ASSOCIATED( SpcCoeff%Band_C2                    ) .OR. &
           ASSOCIATED( SpcCoeff%Is_Microwave_Channel       ) .OR. &
           ASSOCIATED( SpcCoeff%Polarization               ) .OR. &
           ASSOCIATED( SpcCoeff%Cosmic_Background_Radiance ) .OR. &
           ASSOCIATED( SpcCoeff%Is_Solar_Channel           ) .OR. &
           ASSOCIATED( SpcCoeff%Solar_Irradiance           )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_Sensor


  FUNCTION Associated_Spectral( SpcCoeff,  & ! Input
                                ANY_Test ) & ! Optional input
                              RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN ) :: SpcCoeff

    ! -- Optional input
    INTEGER,              OPTIONAL, INTENT( IN ) :: ANY_Test


    ! ---------------
    ! Function result
    ! ---------------

    LOGICAL :: Association_Status


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: ALL_Test



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! -- Default is to test ALL the pointer members
    ! -- for a true association status....
    ALL_Test = .TRUE.

    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( SpcCoeff%Sensor_Descriptor          ) .AND. &
           ASSOCIATED( SpcCoeff%Sensor_Type                ) .AND. &
           ASSOCIATED( SpcCoeff%NCEP_Sensor_ID             ) .AND. &
           ASSOCIATED( SpcCoeff%WMO_Satellite_ID           ) .AND. &
           ASSOCIATED( SpcCoeff%WMO_Sensor_ID              ) .AND. &
           ASSOCIATED( SpcCoeff%Sensor_Channel             ) .AND. &
           ASSOCIATED( SpcCoeff%Frequency                  ) .AND. &
           ASSOCIATED( SpcCoeff%Wavenumber                 ) .AND. &
           ASSOCIATED( SpcCoeff%Planck_C1                  ) .AND. &
           ASSOCIATED( SpcCoeff%Planck_C2                  ) .AND. &
           ASSOCIATED( SpcCoeff%Band_C1                    ) .AND. &
           ASSOCIATED( SpcCoeff%Band_C2                    ) .AND. &
           ASSOCIATED( SpcCoeff%Polarization               ) .AND. &
           ASSOCIATED( SpcCoeff%Is_Solar_Channel           ) .AND. &
           ASSOCIATED( SpcCoeff%Solar_Irradiance           ) .AND. &
           ASSOCIATED( SpcCoeff%MW_and_IR_Channel_Index    ) .AND. &
           ASSOCIATED( SpcCoeff%n_Channels_per_Node        ) .AND. &
           ASSOCIATED( SpcCoeff%Channel_Node_Map           ) .AND. &
           ASSOCIATED( SpcCoeff%MW_and_IR_Node_Index       ) .AND. &
           ASSOCIATED( SpcCoeff%Node_Frequency             ) .AND. &
           ASSOCIATED( SpcCoeff%Node_Wavenumber            ) .AND. &
           ASSOCIATED( SpcCoeff%Node_Planck_C1             ) .AND. &
           ASSOCIATED( SpcCoeff%Node_Planck_C2             ) .AND. &
           ASSOCIATED( SpcCoeff%Cosmic_Background_Radiance )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( SpcCoeff%Sensor_Descriptor          ) .OR. &
           ASSOCIATED( SpcCoeff%Sensor_Type                ) .OR. &
           ASSOCIATED( SpcCoeff%NCEP_Sensor_ID             ) .OR. &
           ASSOCIATED( SpcCoeff%WMO_Satellite_ID           ) .OR. &
           ASSOCIATED( SpcCoeff%WMO_Sensor_ID              ) .OR. &
           ASSOCIATED( SpcCoeff%Sensor_Channel             ) .OR. &
           ASSOCIATED( SpcCoeff%Frequency                  ) .OR. &
           ASSOCIATED( SpcCoeff%Wavenumber                 ) .OR. &
           ASSOCIATED( SpcCoeff%Planck_C1                  ) .OR. &
           ASSOCIATED( SpcCoeff%Planck_C2                  ) .OR. &
           ASSOCIATED( SpcCoeff%Band_C1                    ) .OR. &
           ASSOCIATED( SpcCoeff%Band_C2                    ) .OR. &
           ASSOCIATED( SpcCoeff%Polarization               ) .OR. &
           ASSOCIATED( SpcCoeff%Is_Solar_Channel           ) .OR. &
           ASSOCIATED( SpcCoeff%Solar_Irradiance           ) .OR. &
           ASSOCIATED( SpcCoeff%MW_and_IR_Channel_Index    ) .OR. &
           ASSOCIATED( SpcCoeff%n_Channels_per_Node        ) .OR. &
           ASSOCIATED( SpcCoeff%Channel_Node_Map           ) .OR. &
           ASSOCIATED( SpcCoeff%MW_and_IR_Node_Index       ) .OR. &
           ASSOCIATED( SpcCoeff%Node_Frequency             ) .OR. &
           ASSOCIATED( SpcCoeff%Node_Wavenumber            ) .OR. &
           ASSOCIATED( SpcCoeff%Node_Planck_C1             ) .OR. &
           ASSOCIATED( SpcCoeff%Node_Planck_C2             ) .OR. &
           ASSOCIATED( SpcCoeff%Cosmic_Background_Radiance )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_Spectral




!--------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_SpcCoeff
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of SpcCoeff
!       data structures.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_SpcCoeff( SpcCoeff,                 &  ! Output
!                                        RCS_Id = RCS_Id,          &  ! Revision control
!                                        Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SpcCoeff:     Re-initialized SpcCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       SpcCoeff_Sensor_type
!                                   OR
!                                 SpcCoeff_Spectral_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the structure re-initialisation was successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to zero (0) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:    Subroutine to output messages
!                           SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SpcCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Destroy_Sensor( SpcCoeff,     &  ! Output
                           No_Clear,     &  ! Optional input
                           RCS_Id,       &  ! Revision control
                           Message_Log ) &  ! Error messaging
                         RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN OUT ) :: SpcCoeff

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_SpcCoeff(Sensor)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! -- Default is to clear scalar members...
    Clear = .TRUE.
    ! -- ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF


    
    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM REINITIALISATION --                     #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_SpcCoeff( SpcCoeff )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the Sensor_Descriptor
    IF ( ASSOCIATED( SpcCoeff%Sensor_Descriptor ) ) THEN

      DEALLOCATE( SpcCoeff%Sensor_Descriptor, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Sensor_Descriptor ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Sensor_Type
    IF ( ASSOCIATED( SpcCoeff%Sensor_Type ) ) THEN

      DEALLOCATE( SpcCoeff%Sensor_Type, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Sensor_Type ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the NCEP Sensor ID
    IF ( ASSOCIATED( SpcCoeff%NCEP_Sensor_ID ) ) THEN

      DEALLOCATE( SpcCoeff%NCEP_Sensor_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff NCEP_Sensor_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the WMO Satellite ID
    IF ( ASSOCIATED( SpcCoeff%WMO_Satellite_ID ) ) THEN

      DEALLOCATE( SpcCoeff%WMO_Satellite_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff WMO_Satellite_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the WMO Sensor ID
    IF ( ASSOCIATED( SpcCoeff%WMO_Sensor_ID ) ) THEN

      DEALLOCATE( SpcCoeff%WMO_Sensor_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff WMO_Sensor_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the sensor channel number array
    IF ( ASSOCIATED( SpcCoeff%Sensor_Channel ) ) THEN

      DEALLOCATE( SpcCoeff%Sensor_Channel, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Sensor_Channel ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Frequency
    IF ( ASSOCIATED( SpcCoeff%Frequency ) ) THEN

      DEALLOCATE( SpcCoeff%Frequency, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Frequency ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Wavenumber
    IF ( ASSOCIATED( SpcCoeff%Wavenumber ) ) THEN

      DEALLOCATE( SpcCoeff%Wavenumber, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Wavenumber ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Planck_C1
    IF ( ASSOCIATED( SpcCoeff%Planck_C1 ) ) THEN

      DEALLOCATE( SpcCoeff%Planck_C1, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Planck_C1 ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Planck_C2
    IF ( ASSOCIATED( SpcCoeff%Planck_C2 ) ) THEN

      DEALLOCATE( SpcCoeff%Planck_C2, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Planck_C2 ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Band_C1
    IF ( ASSOCIATED( SpcCoeff%Band_C1 ) ) THEN

      DEALLOCATE( SpcCoeff%Band_C1, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Band_C1 ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Band_C2
    IF ( ASSOCIATED( SpcCoeff%Band_C2 ) ) THEN

      DEALLOCATE( SpcCoeff%Band_C2, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Band_C2 ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Is_Microwave_Channel
    IF ( ASSOCIATED( SpcCoeff%Is_Microwave_Channel ) ) THEN

      DEALLOCATE( SpcCoeff%Is_Microwave_Channel, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Is_Microwave_Channel ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Polarization
    IF ( ASSOCIATED( SpcCoeff%Polarization ) ) THEN

      DEALLOCATE( SpcCoeff%Polarization, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Polarization ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Cosmic_Background_Radiance
    IF ( ASSOCIATED( SpcCoeff%Cosmic_Background_Radiance ) ) THEN

      DEALLOCATE( SpcCoeff%Cosmic_Background_Radiance, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Cosmic_Background_Radiance ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Is_Solar_Channel
    IF ( ASSOCIATED( SpcCoeff%Is_Solar_Channel ) ) THEN

      DEALLOCATE( SpcCoeff%Is_Solar_Channel, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Is_Solar_Channel ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Solar_Irradiance
    IF ( ASSOCIATED( SpcCoeff%Solar_Irradiance ) ) THEN

      DEALLOCATE( SpcCoeff%Solar_Irradiance, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Solar_Irradiance ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- DECREMENT AND TEST ALLOCATION COUNTER --                #
    !#--------------------------------------------------------------------------#

    SpcCoeff%n_Allocates = SpcCoeff%n_Allocates - 1

    IF ( SpcCoeff%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      SpcCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Sensor


  FUNCTION Destroy_Spectral( SpcCoeff,     &  ! Output
                             No_Clear,     &  ! Optional input
                             RCS_Id,       &  ! Revision control
                             Message_Log ) &  ! Error messaging
                           RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN OUT ) :: SpcCoeff

    ! -- Optional input
    INTEGER,              OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_SpcCoeff(Spectral)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! -- Default is to clear scalar members...
    Clear = .TRUE.
    ! -- ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF


    
    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM REINITIALISATION --                     #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_SpcCoeff( SpcCoeff )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the Sensor_Descriptor
    IF ( ASSOCIATED( SpcCoeff%Sensor_Descriptor ) ) THEN

      DEALLOCATE( SpcCoeff%Sensor_Descriptor, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Sensor_Descriptor ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Sensor_Type
    IF ( ASSOCIATED( SpcCoeff%Sensor_Type ) ) THEN

      DEALLOCATE( SpcCoeff%Sensor_Type, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Sensor_Type ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the NCEP Sensor ID
    IF ( ASSOCIATED( SpcCoeff%NCEP_Sensor_ID ) ) THEN

      DEALLOCATE( SpcCoeff%NCEP_Sensor_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff NCEP_Sensor_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the WMO Satellite ID
    IF ( ASSOCIATED( SpcCoeff%WMO_Satellite_ID ) ) THEN

      DEALLOCATE( SpcCoeff%WMO_Satellite_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff WMO_Satellite_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the WMO Sensor ID
    IF ( ASSOCIATED( SpcCoeff%WMO_Sensor_ID ) ) THEN

      DEALLOCATE( SpcCoeff%WMO_Sensor_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff WMO_Sensor_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the sensor channel number array
    IF ( ASSOCIATED( SpcCoeff%Sensor_Channel ) ) THEN

      DEALLOCATE( SpcCoeff%Sensor_Channel, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Sensor_Channel ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate channel Frequency
    IF ( ASSOCIATED( SpcCoeff%Frequency ) ) THEN

      DEALLOCATE( SpcCoeff%Frequency, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Frequency ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate channel Wavenumber
    IF ( ASSOCIATED( SpcCoeff%Wavenumber ) ) THEN

      DEALLOCATE( SpcCoeff%Wavenumber, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Wavenumber ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate channel Planck_C1
    IF ( ASSOCIATED( SpcCoeff%Planck_C1 ) ) THEN

      DEALLOCATE( SpcCoeff%Planck_C1, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Planck_C1 ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate channel Planck_C2
    IF ( ASSOCIATED( SpcCoeff%Planck_C2 ) ) THEN

      DEALLOCATE( SpcCoeff%Planck_C2, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Planck_C2 ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Band_C1
    IF ( ASSOCIATED( SpcCoeff%Band_C1 ) ) THEN

      DEALLOCATE( SpcCoeff%Band_C1, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Band_C1 ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Band_C2
    IF ( ASSOCIATED( SpcCoeff%Band_C2 ) ) THEN

      DEALLOCATE( SpcCoeff%Band_C2, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Band_C2 ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate channel Polarization
    IF ( ASSOCIATED( SpcCoeff%Polarization ) ) THEN

      DEALLOCATE( SpcCoeff%Polarization, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Polarization ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Is_Solar_Channel
    IF ( ASSOCIATED( SpcCoeff%Is_Solar_Channel ) ) THEN

      DEALLOCATE( SpcCoeff%Is_Solar_Channel, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Is_Solar_Channel ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Solar_Irradiance
    IF ( ASSOCIATED( SpcCoeff%Solar_Irradiance ) ) THEN

      DEALLOCATE( SpcCoeff%Solar_Irradiance, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Solar_Irradiance ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate MW_and_IR_Channel_Index
    IF ( ASSOCIATED( SpcCoeff%MW_and_IR_Channel_Index ) ) THEN

      DEALLOCATE( SpcCoeff%MW_and_IR_Channel_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff MW_and_IR_Channel_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate n_Channels_per_Node
    IF ( ASSOCIATED( SpcCoeff%n_Channels_per_Node ) ) THEN

      DEALLOCATE( SpcCoeff%n_Channels_per_Node, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff n_Channels_per_Node ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Channel_Node_Map
    IF ( ASSOCIATED( SpcCoeff%Channel_Node_Map ) ) THEN

      DEALLOCATE( SpcCoeff%Channel_Node_Map, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Channel_Node_Map ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate MW_and_IR_Node_Index
    IF ( ASSOCIATED( SpcCoeff%MW_and_IR_Node_Index ) ) THEN

      DEALLOCATE( SpcCoeff%MW_and_IR_Node_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff MW_and_IR_Node_Index ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Node_Frequency
    IF ( ASSOCIATED( SpcCoeff%Node_Frequency ) ) THEN

      DEALLOCATE( SpcCoeff%Node_Frequency, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Node_Frequency ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Node_Wavenumber
    IF ( ASSOCIATED( SpcCoeff%Node_Wavenumber ) ) THEN

      DEALLOCATE( SpcCoeff%Node_Wavenumber, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Node_Wavenumber ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Node_Planck_C1
    IF ( ASSOCIATED( SpcCoeff%Node_Planck_C1 ) ) THEN

      DEALLOCATE( SpcCoeff%Node_Planck_C1, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Node_Planck_C1 ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Node_Planck_C2
    IF ( ASSOCIATED( SpcCoeff%Node_Planck_C2 ) ) THEN

      DEALLOCATE( SpcCoeff%Node_Planck_C2, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Node_Planck_C2 ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate Cosmic_Background_Radiance
    IF ( ASSOCIATED( SpcCoeff%Cosmic_Background_Radiance ) ) THEN

      DEALLOCATE( SpcCoeff%Cosmic_Background_Radiance, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating SpcCoeff Cosmic_Background_Radiance ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- DECREMENT AND TEST ALLOCATION COUNTER --                #
    !#--------------------------------------------------------------------------#

    SpcCoeff%n_Allocates = SpcCoeff%n_Allocates - 1

    IF ( SpcCoeff%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      SpcCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Spectral




!--------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_SpcCoeff
! 
! PURPOSE:
!       Function to allocate the pointer members of the SpcCoeff
!       data structure.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       For sensor SpcCoeff structures:
!       -------------------------------
!       Error_Status = Allocate_SpcCoeff( n_Channels,               &  ! Input
!                                         SpcCoeff,                 &  ! Output
!                                         RCS_Id = RCS_Id,          &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
!
!       For spectral SpcCoeff structures:
!       ---------------------------------
!       Error_Status = Allocate_SpcCoeff( n_Channels,               &  ! Input
!                                         n_Nodes,                  &  ! Input
!                                         Max_Channels_per_Node,    &  ! Input
!                                         SpcCoeff,                 &  ! Output
!                                         RCS_Id = RCS_Id,          &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Channels:             Dimension of SpcCoeff structure channel
!                               based pointer members.
!                               Must be > 0.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN )
!
!       For spectral SpcCoeff structures:
!       ---------------------------------
!       n_Nodes:                Dimension of SpcCoeff structure node
!                               based pointer members.
!                               Must be > 0.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN )
!
!       Max_Channels_per_Node:  Dimension of SpcCoeff structure channel
!                               to node map pointer member.
!                               Must be > 0.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:            Character string specifying a filename in
!                               which any messages will be logged. If not
!                               specified, or if an error occurs opening the
!                               log file, the default action is to output
!                                messages to standard output.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER(*)
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SpcCoeff:               SpcCoeff structure with allocated pointer
!                               members
!                               UNITS:      N/A
!                               TYPE:       SpcCoeff_Sensor_type
!                                             OR
!                                           SpcCoeff_Spectral_type
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:                 Character string containing the Revision
!                               Control System Id field for the module.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER(*)
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:           The return value is an integer defining the
!                               error status. The error codes are defined in
!                               the ERROR_HANDLER module.
!                               If == SUCCESS the structure pointer allocations
!                                             were successful
!                                  == FAILURE - an error occurred, or
!                                             - the structure internal allocation
!                                               counter is not equal to one (1)
!                                               upon exiting this function. This
!                                               value is incremented and decre-
!                                               mented for every structure
!                                               allocation and deallocation
!                                               respectively.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!
! CALLS:
!       Associated_SpcCoeff:  Function to test the association status of the
!                             pointer members of a SpcCoeff structure.
!
!       Destroy_SpcCoeff:     Function to re-initialize the scalar and pointer
!                             members of SpcCoeff data structures.
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SpcCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Allocate_Sensor( n_Channels,   &  ! Input
                            SpcCoeff,     &  ! Output
                            RCS_Id,       &  ! Revision control
                            Message_Log ) &  ! Error messaging
                          RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                      INTENT( IN )     :: n_Channels

    ! -- Output
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN OUT ) :: SpcCoeff

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_SpcCoeff(Sensor)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Dimensions
    ! ----------

    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input N_CHANNELS must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_SpcCoeff( SpcCoeff, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_SpcCoeff( SpcCoeff, &
                                       No_Clear = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating SpcCoeff pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( SpcCoeff%Sensor_Descriptor( n_Channels ),          &
              SpcCoeff%Sensor_Type( n_Channels ),                &
              SpcCoeff%NCEP_Sensor_ID( n_Channels ),             &
              SpcCoeff%WMO_Satellite_ID ( n_Channels ),          &
              SpcCoeff%WMO_Sensor_ID( n_Channels ),              &
              SpcCoeff%Sensor_Channel( n_Channels ),             &
              SpcCoeff%Frequency( n_Channels ),                  &
              SpcCoeff%Wavenumber( n_Channels ),                 &
              SpcCoeff%Planck_C1( n_Channels ),                  &
              SpcCoeff%Planck_C2( n_Channels ),                  &
              SpcCoeff%Band_C1( n_Channels ),                    &
              SpcCoeff%Band_C2( n_Channels ),                    &
              SpcCoeff%Is_Microwave_Channel( n_Channels ),       &
              SpcCoeff%Polarization( n_Channels ),               &
              SpcCoeff%Cosmic_Background_Radiance( n_Channels ), &
              SpcCoeff%Is_Solar_Channel( n_Channels ),           &
              SpcCoeff%Solar_Irradiance( n_Channels ),           &
              STAT = Allocate_Status                             )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating SpcCoeff data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- ASSIGN SOME COMPONENTS --                        #
    !#--------------------------------------------------------------------------#

    ! --------------
    ! The dimensions
    ! --------------

    SpcCoeff%n_Channels = n_Channels
    SpcCoeff%n_Sensors  = 0


    ! --------------
    ! The array data
    ! --------------

    SpcCoeff%Sensor_Descriptor          = ' '
    SpcCoeff%Sensor_Type                = INVALID_SENSOR
    SpcCoeff%NCEP_Sensor_ID             = INVALID_NCEP_SENSOR_ID 
    SpcCoeff%WMO_Satellite_ID           = INVALID_WMO_SATELLITE_ID
    SpcCoeff%WMO_Sensor_ID              = INVALID_WMO_SENSOR_ID   
    SpcCoeff%Sensor_Channel             = INVALID
    SpcCoeff%Frequency                  = REAL( INVALID, fp_kind )
    SpcCoeff%Wavenumber                 = REAL( INVALID, fp_kind )
    SpcCoeff%Planck_C1                  = REAL( INVALID, fp_kind )
    SpcCoeff%Planck_C2                  = REAL( INVALID, fp_kind )
    SpcCoeff%Band_C1                    = REAL( INVALID, fp_kind )
    SpcCoeff%Band_C2                    = REAL( INVALID, fp_kind )
    SpcCoeff%Is_Microwave_Channel       = INVALID
    SpcCoeff%Polarization               = INVALID_POLARIZATION
    SpcCoeff%Cosmic_Background_Radiance = REAL( INVALID, fp_kind )
    SpcCoeff%Is_Solar_Channel           = INVALID
    SpcCoeff%Solar_Irradiance           = REAL( INVALID, fp_kind )



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    SpcCoeff%n_Allocates = SpcCoeff%n_Allocates + 1

    IF ( SpcCoeff%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      SpcCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Sensor


  FUNCTION Allocate_Spectral( n_Channels,            &  ! Input
                              n_Nodes,               &  ! Input
                              Max_Channels_per_Node, &  ! Input
                              SpcCoeff,              &  ! Output
                              RCS_Id,                &  ! Revision control
                              Message_Log )          &  ! Error messaging
                            RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                        INTENT( IN )     :: n_Channels
    INTEGER,                        INTENT( IN )     :: n_Nodes
    INTEGER,                        INTENT( IN )     :: Max_Channels_per_Node

    ! -- Output
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN OUT ) :: SpcCoeff

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_SpcCoeff(Sensor)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Dimensions
    ! ----------

    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input N_CHANNELS must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Nodes < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input N_NODES must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( Max_Channels_per_Node < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input MAX_CHANNELS_PER_NODE must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_SpcCoeff( SpcCoeff, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_SpcCoeff( SpcCoeff, &
                                       No_Clear = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating SpcCoeff pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( SpcCoeff%Sensor_Descriptor( n_Channels ),          &
              SpcCoeff%Sensor_Type( n_Channels ),                &
              SpcCoeff%NCEP_Sensor_ID( n_Channels ),             &
              SpcCoeff%WMO_Satellite_ID ( n_Channels ),          &
              SpcCoeff%WMO_Sensor_ID( n_Channels ),              &
              SpcCoeff%Sensor_Channel( n_Channels ),             &
              SpcCoeff%Frequency( n_Channels ),                  &
              SpcCoeff%Wavenumber( n_Channels ),                 &
              SpcCoeff%Planck_C1( n_Channels ),                  &
              SpcCoeff%Planck_C2( n_Channels ),                  &
              SpcCoeff%Band_C1( n_Channels ),                    &
              SpcCoeff%Band_C2( n_Channels ),                    &
              SpcCoeff%Polarization( n_Channels ),               &
              SpcCoeff%Is_Solar_Channel( n_Channels ),           &
              SpcCoeff%Solar_Irradiance( n_Channels ),           &
              SpcCoeff%MW_and_IR_Channel_Index( n_Channels ),    &

              SpcCoeff%n_Channels_per_Node( n_Nodes ),           &
              SpcCoeff%Channel_Node_Map( Max_Channels_per_Node,  &
                                         n_Nodes ),              &
              SpcCoeff%MW_and_IR_Node_Index( n_Nodes ),          &
              SpcCoeff%Node_Frequency( n_Nodes ),                &
              SpcCoeff%Node_Wavenumber( n_Nodes ),               &
              SpcCoeff%Node_Planck_C1( n_Nodes ),                &
              SpcCoeff%Node_Planck_C2( n_Nodes ),                &
              SpcCoeff%Cosmic_Background_Radiance( n_Nodes ),    &

              STAT = Allocate_Status                             )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating SpcCoeff data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- ASSIGN SOME COMPONENTS --                        #
    !#--------------------------------------------------------------------------#

    ! --------------
    ! The dimensions
    ! --------------

    SpcCoeff%n_Channels            = n_Channels
    SpcCoeff%n_Nodes               = n_Nodes
    SpcCoeff%Max_Channels_per_Node = Max_Channels_per_Node

    SpcCoeff%n_Sensors  = 0


    ! --------------
    ! The array data
    ! --------------

    SpcCoeff%Sensor_Descriptor          = ' '
    SpcCoeff%Sensor_Type                = INVALID_SENSOR
    SpcCoeff%NCEP_Sensor_ID             = INVALID_NCEP_SENSOR_ID 
    SpcCoeff%WMO_Satellite_ID           = INVALID_WMO_SATELLITE_ID
    SpcCoeff%WMO_Sensor_ID              = INVALID_WMO_SENSOR_ID   
    SpcCoeff%Sensor_Channel             = INVALID
    SpcCoeff%Frequency                  = REAL( INVALID, fp_kind )
    SpcCoeff%Wavenumber                 = REAL( INVALID, fp_kind )
    SpcCoeff%Planck_C1                  = REAL( INVALID, fp_kind )
    SpcCoeff%Planck_C2                  = REAL( INVALID, fp_kind )
    SpcCoeff%Polarization               = INVALID_POLARIZATION
    SpcCoeff%Band_C1                    = REAL( INVALID, fp_kind )
    SpcCoeff%Band_C2                    = REAL( INVALID, fp_kind )
    SpcCoeff%Is_Solar_Channel           = INVALID
    SpcCoeff%Solar_Irradiance           = REAL( INVALID, fp_kind )
    SpcCoeff%MW_and_IR_Channel_Index    = INVALID

    SpcCoeff%n_Channels_per_Node        = INVALID
    SpcCoeff%Channel_Node_Map           = INVALID
    SpcCoeff%MW_and_IR_Node_Index       = INVALID
    SpcCoeff%Node_Frequency             = REAL( INVALID, fp_kind )
    SpcCoeff%Node_Wavenumber            = REAL( INVALID, fp_kind )
    SpcCoeff%Node_Planck_C1             = REAL( INVALID, fp_kind )
    SpcCoeff%Node_Planck_C2             = REAL( INVALID, fp_kind )
    SpcCoeff%Cosmic_Background_Radiance = REAL( INVALID, fp_kind )



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    SpcCoeff%n_Allocates = SpcCoeff%n_Allocates + 1

    IF ( SpcCoeff%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      SpcCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Spectral




!--------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_SpcCoeff
!
! PURPOSE:
!       Function to copy valid SpcCoeff structures.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_SpcCoeff( SpcCoeff_in,              &  ! Input
!                                       SpcCoeff_out,             &  ! Output
!                                       RCS_Id = RCS_Id,          &  ! Revision control
!                                       Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SpcCoeff_in:   SpcCoeff structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_Sensor_type
!                                    OR
!                                  SpcCoeff_Spectral_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SpcCoeff_out:  Copy of the input structure, SpcCoeff_in.
!                      UNITS:      N/A
!                      TYPE:       Same as SpcCoeff_in
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the structure assignment was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_SpcCoeff:  Function to test the association status of the
!                             pointer members of a SpcCoeff structure.
!
!       Allocate_SpcCoeff:    Function to allocate the pointer members of
!                             the SpcCoeff data structure.
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SpcCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Assign_Sensor( SpcCoeff_in,  &  ! Input
                          SpcCoeff_out, &  ! Output
                          RCS_Id,       &  ! Revision control
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN )     :: SpcCoeff_in

    ! -- Output
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN OUT ) :: SpcCoeff_out

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_SpcCoeff(Sensor)'



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE ARGUMENT POINTER ASSOCIATION --          #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------
    ! ALL *input* pointers must be associated
    ! ---------------------------------------

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT SpcCoeff pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Allocate the structure
    ! ----------------------

    Error_Status = Allocate_SpcCoeff( SpcCoeff_in%n_Channels,  &
                                      SpcCoeff_out,            &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output SpcCoeff arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    SpcCoeff_out%Release = SpcCoeff_in%Release
    SpcCoeff_out%Version = SpcCoeff_in%Version

    SpcCoeff_out%n_Sensors = SpcCoeff_in%n_Sensors


    ! -----------------
    ! Assign array data
    ! -----------------

    SpcCoeff_out%Sensor_Descriptor          = SpcCoeff_in%Sensor_Descriptor
    SpcCoeff_out%Sensor_Type                = SpcCoeff_in%Sensor_Type
    SpcCoeff_out%NCEP_Sensor_ID             = SpcCoeff_in%NCEP_Sensor_ID
    SpcCoeff_out%WMO_Satellite_ID           = SpcCoeff_in%WMO_Satellite_ID
    SpcCoeff_out%WMO_Sensor_ID              = SpcCoeff_in%WMO_Sensor_ID
    SpcCoeff_out%Sensor_Channel             = SpcCoeff_in%Sensor_Channel
    SpcCoeff_out%Frequency                  = SpcCoeff_in%Frequency
    SpcCoeff_out%Wavenumber                 = SpcCoeff_in%Wavenumber
    SpcCoeff_out%Planck_C1                  = SpcCoeff_in%Planck_C1
    SpcCoeff_out%Planck_C2                  = SpcCoeff_in%Planck_C2
    SpcCoeff_out%Band_C1                    = SpcCoeff_in%Band_C1
    SpcCoeff_out%Band_C2                    = SpcCoeff_in%Band_C2
    SpcCoeff_out%Is_Microwave_Channel       = SpcCoeff_in%Is_Microwave_Channel
    SpcCoeff_out%Polarization               = SpcCoeff_in%Polarization
    SpcCoeff_out%Cosmic_Background_Radiance = SpcCoeff_in%Cosmic_Background_Radiance
    SpcCoeff_out%Is_Solar_Channel           = SpcCoeff_in%Is_Solar_Channel
    SpcCoeff_out%Solar_Irradiance           = SpcCoeff_in%Solar_Irradiance

  END FUNCTION Assign_Sensor


  FUNCTION Assign_Spectral( SpcCoeff_in,  &  ! Input
                            SpcCoeff_out, &  ! Output
                            RCS_Id,       &  ! Revision control
                            Message_Log ) &  ! Error messaging
                          RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN )     :: SpcCoeff_in

    ! -- Output
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN OUT ) :: SpcCoeff_out

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_SpcCoeff(Spectral)'



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE ARGUMENT POINTER ASSOCIATION --          #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------
    ! ALL *input* pointers must be associated
    ! ---------------------------------------

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff_In ) ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT SpcCoeff pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Allocate the structure
    ! ----------------------

    Error_Status = Allocate_SpcCoeff( SpcCoeff_in%n_Channels,            &
                                      SpcCoeff_in%n_Nodes,               &
                                      SpcCoeff_in%Max_Channels_per_Node, &
                                      SpcCoeff_out,                      &
                                      Message_Log = Message_Log          )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output SpcCoeff arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    SpcCoeff_out%Release = SpcCoeff_in%Release
    SpcCoeff_out%Version = SpcCoeff_in%Version

    SpcCoeff_out%n_Sensors = SpcCoeff_in%n_Sensors


    ! -----------------
    ! Assign array data
    ! -----------------

    SpcCoeff_out%Sensor_Descriptor          = SpcCoeff_in%Sensor_Descriptor
    SpcCoeff_out%Sensor_Type                = SpcCoeff_in%Sensor_Type
    SpcCoeff_out%NCEP_Sensor_ID             = SpcCoeff_in%NCEP_Sensor_ID
    SpcCoeff_out%WMO_Satellite_ID           = SpcCoeff_in%WMO_Satellite_ID
    SpcCoeff_out%WMO_Sensor_ID              = SpcCoeff_in%WMO_Sensor_ID
    SpcCoeff_out%Sensor_Channel             = SpcCoeff_in%Sensor_Channel
    SpcCoeff_out%Frequency                  = SpcCoeff_in%Frequency
    SpcCoeff_out%Wavenumber                 = SpcCoeff_in%Wavenumber
    SpcCoeff_out%Planck_C1                  = SpcCoeff_in%Planck_C1
    SpcCoeff_out%Planck_C2                  = SpcCoeff_in%Planck_C2
    SpcCoeff_out%Band_C1                    = SpcCoeff_in%Band_C1
    SpcCoeff_out%Band_C2                    = SpcCoeff_in%Band_C2
    SpcCoeff_out%Polarization               = SpcCoeff_in%Polarization
    SpcCoeff_out%Is_Solar_Channel           = SpcCoeff_in%Is_Solar_Channel
    SpcCoeff_out%Solar_Irradiance           = SpcCoeff_in%Solar_Irradiance
    SpcCoeff_out%MW_and_IR_Channel_Index    = SpcCoeff_in%MW_and_IR_Channel_Index

    SpcCoeff_out%n_Channels_per_Node        = SpcCoeff_in%n_Channels_per_Node
    SpcCoeff_out%Channel_Node_Map           = SpcCoeff_in%Channel_Node_Map
    SpcCoeff_out%MW_and_IR_Node_Index       = SpcCoeff_in%MW_and_IR_Node_Index
    SpcCoeff_out%Node_Frequency             = SpcCoeff_in%Node_Frequency
    SpcCoeff_out%Node_Wavenumber            = SpcCoeff_in%Node_Wavenumber
    SpcCoeff_out%Node_Planck_C1             = SpcCoeff_in%Node_Planck_C1
    SpcCoeff_out%Node_Planck_C2             = SpcCoeff_in%Node_Planck_C2
    SpcCoeff_out%Cosmic_Background_Radiance = SpcCoeff_in%Cosmic_Background_Radiance

  END FUNCTION Assign_Spectral





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Concatenate_SpcCoeff
!
! PURPOSE:
!       Function to concatenate two valid SpcCoeff structures along
!       the channel/node dimension.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Concatenate_SpcCoeff( SpcCoeff1,                &  ! Input/Output
!                                            SpcCoeff2,                &  ! Input
!                                            RCS_Id = RCS_Id,          &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SpcCoeff1:     First SpcCoeff structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_Sensor_type
!                                    OR
!                                  SpcCoeff_Spectral_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
!       SpcCoeff2:     Second SpcCoeff structure to concatenate.
!                      UNITS:      N/A
!                      TYPE:       Same as SpcCoeff1
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SpcCoeff1:     The concatenated SpcCoeff structure. The order of
!                      concatenation is SpcCoef1,SpcCoeff2.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_Sensor_type
!                                    OR
!                                  SpcCoeff_Spectral_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structure concatenation was successful
!                         == FAILURE an unrecoverable error occurred, or
!                         == WARNING the destruction of a temporary, local SpcCoeff
!                                    structure failed.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       Associated_SpcCoeff:     Function to test the association status of the
!                                pointer members of a SpcCoeff structure.
!
!       Assign_SpcCoeff:         Function to copy valid SpcCoeff data structures.
!
!       Destroy_SpcCoeff:        Function to re-initialize the scalar and pointer
!                                members of SpcCoeff data structures.
!
!       Allocate_SpcCoeff:       Function to allocate the pointer members of
!                                the SpcCoeff data structure.
!
!       Count_SpcCoeff_Sensors:  Subroutine to count the number of different
!                                satellite/sensors in the SpcCoeff structure
!                                and set the n_Sensors field.
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       The input SpcCoeff1 argument contains the concatenated structure
!       data (in character-speak: SpcCoeff1//SpcCoeff2) on output. It is
!       reallocated within this routine so if an error occurs during the
!       reallocation, the contents of the input SpcCoeff1 structure will
!       be lost.
!
!       Because of the structure reallocation there is a potential that 
!       available memory will become fragmented. Use this routine in a
!       manner that will minimise this effect (e.g. destroying structures or
!       allocatable arrays in the opposite order in which they were created). 
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 06-Feb-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Concatenate_Sensor( SpcCoeff1,     &  ! Input/Output
                               SpcCoeff2,     &  ! Input
                               RCS_Id,        &  ! Revision control
                               Message_Log )  &  ! Error messaging
                             RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input/Output
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN OUT )  :: SpcCoeff1
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN )      :: SpcCoeff2

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )     :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )      :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Concatenate_SpcCoeff(Sensor)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: n_Channels, l1, l2
    TYPE( SpcCoeff_Sensor_type ) :: SpcCoeff_Tmp



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! The first structure
    ! -------------------

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT SpcCoeff1 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------
    ! The second structure
    ! --------------------

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff2 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT SpcCoeff2 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- COMPARE THE INPUT SpcCoeff RELEASE AND VERSION --           #
    !#--------------------------------------------------------------------------#

    ! -------
    ! Release
    ! -------

    IF ( SpcCoeff1%Release /= SpcCoeff2%Release ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input SpcCoeff Release values are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------
    ! Version
    ! -------

    IF ( SpcCoeff1%Version /= SpcCoeff2%Version ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Input SpcCoeff Version values are different.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- COPY FIRST INPUT SpcCoeff STRUCTURE --                #
    !#--------------------------------------------------------------------------#

    Error_Status = Assign_SpcCoeff( SpcCoeff1, SpcCoeff_Tmp, &
                                    Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error copying SpcCoeff1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
   


    !#--------------------------------------------------------------------------#
    !#             -- REALLOCATE FIRST INPUT SpcCoeff STRUCTURE --              #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Destroy it
    ! ----------

    Error_Status = Destroy_SpcCoeff( SpcCoeff1, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying SpcCoeff1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------
    ! Re-Allocate it
    ! --------------

    ! -- Set the total number of channels
    n_Channels = SpcCoeff_Tmp%n_Channels + SpcCoeff2%n_Channels

    ! -- Perform the allocation
    Error_Status = Allocate_SpcCoeff( n_Channels, &
                                      SpcCoeff1, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reallocating SpcCoeff1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE CONCATENATION --                    #
    !#--------------------------------------------------------------------------#

    SpcCoeff1%Version = MAX( SpcCoeff_Tmp%Version, SpcCoeff2%Version )


    ! ----------------------
    ! Concatenate array data
    ! ----------------------

    ! -- The first part
    l1 = 1
    l2 = SpcCoeff_Tmp%n_Channels

    SpcCoeff1%Sensor_Descriptor(l1:l2)          = SpcCoeff_Tmp%Sensor_Descriptor
    SpcCoeff1%Sensor_Type(l1:l2)                = SpcCoeff_Tmp%Sensor_Type
    SpcCoeff1%NCEP_Sensor_ID(l1:l2)             = SpcCoeff_Tmp%NCEP_Sensor_ID
    SpcCoeff1%WMO_Satellite_ID(l1:l2)           = SpcCoeff_Tmp%WMO_Satellite_ID
    SpcCoeff1%WMO_Sensor_ID(l1:l2)              = SpcCoeff_Tmp%WMO_Sensor_ID
    SpcCoeff1%Sensor_Channel(l1:l2)             = SpcCoeff_Tmp%Sensor_Channel
    SpcCoeff1%Frequency(l1:l2)                  = SpcCoeff_Tmp%Frequency
    SpcCoeff1%Wavenumber(l1:l2)                 = SpcCoeff_Tmp%Wavenumber
    SpcCoeff1%Planck_C1(l1:l2)                  = SpcCoeff_Tmp%Planck_C1
    SpcCoeff1%Planck_C2(l1:l2)                  = SpcCoeff_Tmp%Planck_C2
    SpcCoeff1%Band_C1(l1:l2)                    = SpcCoeff_Tmp%Band_C1
    SpcCoeff1%Band_C2(l1:l2)                    = SpcCoeff_Tmp%Band_C2
    SpcCoeff1%Is_Microwave_Channel(l1:l2)       = SpcCoeff_Tmp%Is_Microwave_Channel
    SpcCoeff1%Polarization(l1:l2)               = SpcCoeff_Tmp%Polarization
    SpcCoeff1%Cosmic_Background_Radiance(l1:l2) = SpcCoeff_Tmp%Cosmic_Background_Radiance
    SpcCoeff1%Is_Solar_Channel(l1:l2)           = SpcCoeff_Tmp%Is_Solar_Channel
    SpcCoeff1%Solar_Irradiance(l1:l2)           = SpcCoeff_Tmp%Solar_Irradiance

    ! -- The second part
    l1 = l2 + 1
    l2 = n_Channels

    SpcCoeff1%Sensor_Descriptor(l1:l2)          = SpcCoeff2%Sensor_Descriptor
    SpcCoeff1%Sensor_Type(l1:l2)                = SpcCoeff2%Sensor_Type
    SpcCoeff1%NCEP_Sensor_ID(l1:l2)             = SpcCoeff2%NCEP_Sensor_ID
    SpcCoeff1%WMO_Satellite_ID(l1:l2)           = SpcCoeff2%WMO_Satellite_ID
    SpcCoeff1%WMO_Sensor_ID(l1:l2)              = SpcCoeff2%WMO_Sensor_ID
    SpcCoeff1%Sensor_Channel(l1:l2)             = SpcCoeff2%Sensor_Channel
    SpcCoeff1%Frequency(l1:l2)                  = SpcCoeff2%Frequency
    SpcCoeff1%Wavenumber(l1:l2)                 = SpcCoeff2%Wavenumber
    SpcCoeff1%Planck_C1(l1:l2)                  = SpcCoeff2%Planck_C1
    SpcCoeff1%Planck_C2(l1:l2)                  = SpcCoeff2%Planck_C2
    SpcCoeff1%Band_C1(l1:l2)                    = SpcCoeff2%Band_C1
    SpcCoeff1%Band_C2(l1:l2)                    = SpcCoeff2%Band_C2
    SpcCoeff1%Is_Microwave_Channel(l1:l2)       = SpcCoeff2%Is_Microwave_Channel
    SpcCoeff1%Polarization(l1:l2)               = SpcCoeff2%Polarization
    SpcCoeff1%Cosmic_Background_Radiance(l1:l2) = SpcCoeff2%Cosmic_Background_Radiance
    SpcCoeff1%Is_Solar_Channel(l1:l2)           = SpcCoeff2%Is_Solar_Channel
    SpcCoeff1%Solar_Irradiance(l1:l2)           = SpcCoeff2%Solar_Irradiance



    !#--------------------------------------------------------------------------#
    !#                    -- COUNT THE NUMBER OF SENSORS --                     #
    !#--------------------------------------------------------------------------#

    CALL Count_SpcCoeff_Sensors( SpcCoeff1, Use_WMO_Id = SET )



    !#--------------------------------------------------------------------------#
    !#             -- DEALLOCATE THE TEMPORARY SpcCoeff STRUCTURE --            #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_SpcCoeff( SpcCoeff_Tmp, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying SpcCoeff_Tmp structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Concatenate_Sensor


  FUNCTION Concatenate_Spectral( SpcCoeff1,     &  ! Input/Output
                                 SpcCoeff2,     &  ! Input
                                 RCS_Id,        &  ! Revision control
                                 Message_Log )  &  ! Error messaging
                               RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input/Output
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN OUT )  :: SpcCoeff1
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN )      :: SpcCoeff2

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )     :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )      :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Concatenate_SpcCoeff(Spectral)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: n_Channels,            l1,  l2 
    INTEGER :: n_Nodes,               n1,  n2 
    INTEGER :: Max_Channels_per_Node, ln1, ln2
    TYPE( SpcCoeff_Spectral_type ) :: SpcCoeff_Tmp



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! The first structure
    ! -------------------

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT SpcCoeff1 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------
    ! The second structure
    ! --------------------

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff2 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT SpcCoeff2 pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- COMPARE THE INPUT SpcCoeff RELEASE AND VERSION --           #
    !#--------------------------------------------------------------------------#

    ! -------
    ! Release
    ! -------

    IF ( SpcCoeff1%Release /= SpcCoeff2%Release ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input SpcCoeff Release values are different.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------
    ! Version
    ! -------

    IF ( SpcCoeff1%Version /= SpcCoeff2%Version ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Input SpcCoeff Version values are different.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    

    !#--------------------------------------------------------------------------#
    !#                 -- COPY FIRST INPUT SpcCoeff STRUCTURE --                #
    !#--------------------------------------------------------------------------#

    Error_Status = Assign_SpcCoeff( SpcCoeff1, SpcCoeff_Tmp, &
                                    Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error copying SpcCoeff1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
   


    !#--------------------------------------------------------------------------#
    !#             -- REALLOCATE FIRST INPUT SpcCoeff STRUCTURE --              #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Destroy it
    ! ----------

    Error_Status = Destroy_SpcCoeff( SpcCoeff1, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying SpcCoeff1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------
    ! Re-Allocate it
    ! --------------

    ! -- Compute the new dimensions
    n_Channels = SpcCoeff_Tmp%n_Channels + SpcCoeff2%n_Channels
    n_Nodes    = SpcCoeff_Tmp%n_Nodes    + SpcCoeff2%n_Nodes

    Max_Channels_per_Node = MAX( SpcCoeff_Tmp%Max_Channels_per_Node, &
                                 SpcCoeff2%Max_Channels_per_Node )

    ! -- Perform the allocation
    Error_Status = Allocate_SpcCoeff( n_Channels, &
                                      n_Nodes, &
                                      Max_Channels_per_Node, &
                                      SpcCoeff1, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reallocating SpcCoeff1 structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE CONCATENATION --                    #
    !#--------------------------------------------------------------------------#

    SpcCoeff1%Version = MAX( SpcCoeff_Tmp%Version, SpcCoeff2%Version )


    ! ------------------------------------
    ! Concatenate CHANNEL based array data
    ! ------------------------------------

    ! -- The first part
    l1 = 1
    l2 = SpcCoeff_Tmp%n_Channels

    SpcCoeff1%Sensor_Descriptor(l1:l2)       = SpcCoeff_Tmp%Sensor_Descriptor
    SpcCoeff1%Sensor_Type(l1:l2)             = SpcCoeff_Tmp%Sensor_Type
    SpcCoeff1%NCEP_Sensor_ID(l1:l2)          = SpcCoeff_Tmp%NCEP_Sensor_ID
    SpcCoeff1%WMO_Satellite_ID(l1:l2)        = SpcCoeff_Tmp%WMO_Satellite_ID
    SpcCoeff1%WMO_Sensor_ID(l1:l2)           = SpcCoeff_Tmp%WMO_Sensor_ID
    SpcCoeff1%Sensor_Channel(l1:l2)          = SpcCoeff_Tmp%Sensor_Channel
    SpcCoeff1%Frequency(l1:l2)               = SpcCoeff_Tmp%Frequency
    SpcCoeff1%Wavenumber(l1:l2)              = SpcCoeff_Tmp%Wavenumber
    SpcCoeff1%Planck_C1(l1:l2)               = SpcCoeff_Tmp%Planck_C1
    SpcCoeff1%Planck_C2(l1:l2)               = SpcCoeff_Tmp%Planck_C2
    SpcCoeff1%Band_C1(l1:l2)                 = SpcCoeff_Tmp%Band_C1
    SpcCoeff1%Band_C2(l1:l2)                 = SpcCoeff_Tmp%Band_C2
    SpcCoeff1%Is_Solar_Channel(l1:l2)        = SpcCoeff_Tmp%Is_Solar_Channel
    SpcCoeff1%Solar_Irradiance(l1:l2)        = SpcCoeff_Tmp%Solar_Irradiance
    SpcCoeff1%MW_and_IR_Channel_Index(l1:l2) = SpcCoeff_Tmp%MW_and_IR_Channel_Index

    ! -- The second part
    l1 = l2 + 1
    l2 = n_Channels

    SpcCoeff1%Sensor_Descriptor(l1:l2)       = SpcCoeff2%Sensor_Descriptor
    SpcCoeff1%Sensor_Type(l1:l2)             = SpcCoeff2%Sensor_Type
    SpcCoeff1%NCEP_Sensor_ID(l1:l2)          = SpcCoeff2%NCEP_Sensor_ID
    SpcCoeff1%WMO_Satellite_ID(l1:l2)        = SpcCoeff2%WMO_Satellite_ID
    SpcCoeff1%WMO_Sensor_ID(l1:l2)           = SpcCoeff2%WMO_Sensor_ID
    SpcCoeff1%Sensor_Channel(l1:l2)          = SpcCoeff2%Sensor_Channel
    SpcCoeff1%Frequency(l1:l2)               = SpcCoeff2%Frequency
    SpcCoeff1%Wavenumber(l1:l2)              = SpcCoeff2%Wavenumber
    SpcCoeff1%Planck_C1(l1:l2)               = SpcCoeff2%Planck_C1
    SpcCoeff1%Planck_C2(l1:l2)               = SpcCoeff2%Planck_C2
    SpcCoeff1%Band_C1(l1:l2)                 = SpcCoeff2%Band_C1
    SpcCoeff1%Band_C2(l1:l2)                 = SpcCoeff2%Band_C2
    SpcCoeff1%Is_Solar_Channel(l1:l2)        = SpcCoeff2%Is_Solar_Channel
    SpcCoeff1%Solar_Irradiance(l1:l2)        = SpcCoeff2%Solar_Irradiance
    SpcCoeff1%MW_and_IR_Channel_Index(l1:l2) = SpcCoeff2%MW_and_IR_Channel_Index


    ! ---------------------------------
    ! Concatenate NODE based array data
    ! ---------------------------------

    ! -- The first part
    n1  = 1
    n2  = SpcCoeff_Tmp%n_Nodes
    ln2 = SpcCoeff_Tmp%Max_Channels_per_Node

    SpcCoeff1%n_Channels_per_Node(n1:n2)        = SpcCoeff_Tmp%n_Channels_per_Node
    SpcCoeff1%Channel_Node_Map(1:ln2,n1:n2)     = SpcCoeff_Tmp%Channel_Node_Map
    SpcCoeff1%MW_and_IR_Node_Index(n1:n2)       = SpcCoeff_Tmp%MW_and_IR_Node_Index
    SpcCoeff1%Node_Frequency(n1:n2)             = SpcCoeff_Tmp%Node_Frequency
    SpcCoeff1%Node_Wavenumber(n1:n2)            = SpcCoeff_Tmp%Node_Wavenumber
    SpcCoeff1%Node_Planck_C1(n1:n2)             = SpcCoeff_Tmp%Node_Planck_C1
    SpcCoeff1%Node_Planck_C2(n1:n2)             = SpcCoeff_Tmp%Node_Planck_C2
    SpcCoeff1%Cosmic_Background_Radiance(n1:n2) = SpcCoeff_Tmp%Cosmic_Background_Radiance

    ! -- The second part
    n1  = n2 + 1
    n2  = n_Nodes
    ln2 = SpcCoeff2%Max_Channels_per_Node

    SpcCoeff1%n_Channels_per_Node(n1:n2)        = SpcCoeff2%n_Channels_per_Node
    SpcCoeff1%Channel_Node_Map(1:ln2,n1:n2)     = SpcCoeff2%Channel_Node_Map
    SpcCoeff1%MW_and_IR_Node_Index(n1:n2)       = SpcCoeff2%MW_and_IR_Node_Index
    SpcCoeff1%Node_Frequency(n1:n2)             = SpcCoeff2%Node_Frequency
    SpcCoeff1%Node_Wavenumber(n1:n2)            = SpcCoeff2%Node_Wavenumber
    SpcCoeff1%Node_Planck_C1(n1:n2)             = SpcCoeff2%Node_Planck_C1
    SpcCoeff1%Node_Planck_C2(n1:n2)             = SpcCoeff2%Node_Planck_C2
    SpcCoeff1%Cosmic_Background_Radiance(n1:n2) = SpcCoeff2%Cosmic_Background_Radiance



    !#--------------------------------------------------------------------------#
    !#                    -- COUNT THE NUMBER OF SENSORS --                     #
    !#--------------------------------------------------------------------------#

    CALL Count_SpcCoeff_Sensors( SpcCoeff1, Use_WMO_Id = SET )



    !#--------------------------------------------------------------------------#
    !#             -- DEALLOCATE THE TEMPORARY SpcCoeff STRUCTURE --            #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_SpcCoeff( SpcCoeff_Tmp, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying SpcCoeff_Tmp structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Concatenate_Spectral





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Equal_SpcCoeff
!
! PURPOSE:
!       Function to test if two SpcCoeff structures are equal.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Equal_SpcCoeff( SpcCoeff_LHS,             &  ! Input
!                                      SpcCoeff_RHS,             &  ! Input
!                                      ULP_Scale   = ULP_Scale,  &  ! Optional input
!                                      Check_All   = Check_All,  &  ! Optional input
!                                      RCS_Id      = RCS_Id,     &  ! Revision control
!                                      Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SpcCoeff_LHS:  SpcCoeff structure to be compared; equivalent to the
!                      left-hand side of a lexical comparison, e.g.
!                        IF ( SpcCoeff_LHS == SpcCoeff_RHS ).
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_Sensor_type
!                                    OR
!                                  SpcCoeff_Spectral_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
!       SpcCoeff_RHS:  SpcCoeff structure to be compared to; equivalent to
!                      right-hand side of a lexical comparison, e.g.
!                        IF ( SpcCoeff_LHS == SpcCoeff_RHS ).
!                      UNITS:      N/A
!                      TYPE:       Same as SpcCoeff_LHS
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:     Unit of data precision used to scale the floating
!                      point comparison. ULP stands for "Unit in the Last Place,"
!                      the smallest possible increment or decrement that can be
!                      made using a machine's floating point arithmetic.
!                      Value must be positive - if a negative value is supplied,
!                      the absolute value is used. If not specified, the default
!                      value is 1.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Check_All:     Set this argument to check ALL the floating point
!                      channel data of the SpcCoeff structures. The default
!                      action is return with a FAILURE status as soon as
!                      any difference is found. This optional argument can
!                      be used to get a listing of ALL the differences
!                      between data in SpcCoeff structures.
!                      If == 0, Return with FAILURE status as soon as
!                               ANY difference is found  *DEFAULT*
!                         == 1, Set FAILURE status if ANY difference is
!                               found, but continue to check ALL data.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structures were equal
!                         == FAILURE - an error occurred, or
!                                    - the structures were different.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       Associated_SpcCoeff:  Function to test the association status of the
!                             pointer members of a SpcCoeff structure.
!
!       Compare_Float:        Function to compare floating point scalars and
!                             arrays with adjustible precision tolerance.
!                             SOURCE: COMPARE_FLOAT_NUMBERS module
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
!
! CONTAINS:
!       Construct_Message:    Internal subprogram to construct a standard message
!                             when a floating point number comparison fails.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Equal_Sensor( SpcCoeff_LHS, &  ! Input
                         SpcCoeff_RHS, &  ! Input
                         ULP_Scale,    &  ! Optional input
                         Check_All,    &  ! Optional input
                         RCS_Id,       &  ! Revision control
                         Message_Log ) &  ! Error messaging
                       RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN )  :: SpcCoeff_LHS
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN )  :: SpcCoeff_RHS

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN )  :: ULP_Scale
    INTEGER,            OPTIONAL, INTENT( IN )  :: Check_All

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Equal_SpcCoeff(Sensor)'
    CHARACTER( * ), PARAMETER :: INT_FORMAT = 'i4'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    CHARACTER( 256 ) :: Format_String
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: l



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                   -- CHECK THE OPTIONAL ARGUMENTS --                     #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Test the ULP_Scale argument
    ! ---------------------------

    ! -- Default precision is a single unit in last place
    ULP = 1
    ! -- ... unless the ULP_Scale argument is set and positive
    IF ( PRESENT( ULP_Scale ) ) THEN
      IF ( ULP_Scale > 0 ) ULP = ULP_Scale
    END IF


    ! ---------------------------
    ! Test the Check_All argument
    ! ---------------------------

    ! -- Default action is to return on ANY difference...
    Check_Once = .TRUE.
    ! -- ...unless the Check_All argument is set
    IF ( PRESENT( Check_All ) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! The LHS structure
    ! -----------------

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SpcCoeff_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------
    ! The RHS structure
    ! -----------------

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT SpcCoeff_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- CHECK SCALAR MEMBERS --                        #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Release/Version info
    ! --------------------

    IF ( ( SpcCoeff_LHS%Release /= SpcCoeff_RHS%Release ) .OR. &
         ( SpcCoeff_LHS%Version /= SpcCoeff_RHS%Version )      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Release/Version numbers are different : ", &
                        &i2, ".", i2.2, " vs. ", i2, ".", i2.2 )' ) &
                      SpcCoeff_LHS%Release, SpcCoeff_LHS%Version, &
                      SpcCoeff_RHS%Release, SpcCoeff_RHS%Version
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------
    ! Channel dimension
    ! -----------------

    IF ( SpcCoeff_LHS%n_Channels /= SpcCoeff_RHS%n_Channels ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Channels dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      SpcCoeff_LHS%n_Channels, SpcCoeff_RHS%n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------
    ! n_Sensors value
    ! ---------------

    IF ( SpcCoeff_LHS%n_Sensors /= SpcCoeff_RHS%n_Sensors ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Sensors values are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      SpcCoeff_LHS%n_Sensors, SpcCoeff_RHS%n_Sensors
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- CHECK POINTER MEMBERS BY CHANNEL --                  #
    !#                                                                          #
    !# Each structure member is tested separately. It's a bit of a brain dead   #
    !# way to do it, but easiest to implement since the data types differ.      #
    !# Also, each channel is tested explicitly, rather than using the ANY       #
    !# or ALL intrinsic functions, since I wanted to highlight the actual       #
    !# channel index where any difference occured so it would be very easy to   #
    !# track down the location of the difference.                               #
    !#--------------------------------------------------------------------------#

    Channel_Loop: DO l = 1, SpcCoeff_RHS%n_Channels


      ! ---------------------
      ! The Sensor Descriptor
      ! ---------------------

      IF ( SpcCoeff_LHS%Sensor_Descriptor(l) /= SpcCoeff_RHS%Sensor_Descriptor(l) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Descriptor values are different, ", &
                          &a, " vs. ", a, ",  for channel index # ", i4 )' ) &
                        TRIM( SpcCoeff_LHS%Sensor_Descriptor(l) ), &
                        TRIM( SpcCoeff_RHS%Sensor_Descriptor(l) ), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! ---------------
      ! The Sensor Type
      ! ---------------

      IF ( SpcCoeff_LHS%Sensor_Type(l) /= SpcCoeff_RHS%Sensor_Type(l) ) THEN
        Error_Status = FAILURE
        IF ( SpcCoeff_LHS%Sensor_Type(l) > INVALID_SENSOR .AND. &
             SpcCoeff_LHS%Sensor_Type(l) < N_SENSOR_TYPES .AND. &
             SpcCoeff_RHS%Sensor_Type(l) > INVALID_SENSOR .AND. &
             SpcCoeff_RHS%Sensor_Type(l) < N_SENSOR_TYPES       ) THEN
          WRITE( Message, '( "Sensor types are different, ", &
                            &a, " vs. ", a, ",  for channel index # ", i4 )' ) &
                          TRIM( SENSOR_TYPE_NAME( SpcCoeff_LHS%Sensor_Type(l) ) ), &
                          TRIM( SENSOR_TYPE_NAME( SpcCoeff_RHS%Sensor_Type(l) ) ), &
                          l
        ELSE
          WRITE( Message, '( "Sensor type values are different or invalid, ", &
                            &i5, " vs. ", i5, ",  for channel index # ", i4 )' ) &
                          SpcCoeff_LHS%Sensor_Type(l), &
                          SpcCoeff_RHS%Sensor_Type(l), &
                          l
        END IF
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! ------------------
      ! The NCEP sensor ID
      ! ------------------

      IF ( SpcCoeff_LHS%NCEP_Sensor_ID(l) /= SpcCoeff_RHS%NCEP_Sensor_ID(l) ) THEN
        Error_Status = FAILURE
        Format_String = '( "NCEP_Sensor_ID values are different, ", '//INT_FORMAT//&
                        ', " vs. ", '//INT_FORMAT//', ",  for channel index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%NCEP_Sensor_ID(l), &
                        SpcCoeff_RHS%NCEP_Sensor_ID(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! --------------------
      ! The WMO Satellite ID
      ! --------------------

      IF ( SpcCoeff_LHS%WMO_Satellite_ID(l) /= SpcCoeff_RHS%WMO_Satellite_ID(l) ) THEN
        Error_Status = FAILURE
        Format_String = '( "WMO_Satellite_ID values are different, ", '//INT_FORMAT//&
                        ', " vs. ", '//INT_FORMAT//', ",  for channel index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%WMO_Satellite_ID(l), &
                        SpcCoeff_RHS%WMO_Satellite_ID(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! -----------------
      ! The WMO Sensor ID
      ! -----------------

      IF ( SpcCoeff_LHS%WMO_Sensor_ID(l) /= SpcCoeff_RHS%WMO_Sensor_ID(l) ) THEN
        Error_Status = FAILURE
        Format_String = '( "WMO_Sensor_ID values are different, ", '//INT_FORMAT//&
                        ', " vs. ", '//INT_FORMAT//', ",  for channel index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%WMO_Sensor_ID(l), &
                        SpcCoeff_RHS%WMO_Sensor_ID(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! --------------------------
      ! The sensor channel numbers
      ! --------------------------

      IF ( SpcCoeff_LHS%Sensor_Channel(l) /= SpcCoeff_RHS%Sensor_Channel(l) ) THEN
        Error_Status = FAILURE
        Format_String = '( "Sensor_Channel values are different, ", '//INT_FORMAT//&
                        ', " vs. ", '//INT_FORMAT//', ",  for channel index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%Sensor_Channel(l), &
                        SpcCoeff_RHS%Sensor_Channel(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! -----------------------------
      ! The channel central frequency
      ! -----------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Frequency(l), &
                                  SpcCoeff_RHS%Frequency(l), &
                                  ULP = ULP                  ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Frequency', l, &
                                     SpcCoeff_LHS%Frequency(l), &
                                     SpcCoeff_RHS%Frequency(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! ------------------------------
      ! The channel central wavenumber
      ! ------------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Wavenumber(l), &
                                  SpcCoeff_RHS%Wavenumber(l), &
                                  ULP = ULP                   ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Wavenumber', l, &
                                     SpcCoeff_LHS%Wavenumber(l), &
                                     SpcCoeff_RHS%Wavenumber(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! ----------------------------------
      ! The first Planck function constant
      ! ----------------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Planck_C1(l), &
                                  SpcCoeff_RHS%Planck_C1(l), &
                                  ULP = ULP                  ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Planck_C1', l, &
                                     SpcCoeff_LHS%Planck_C1(l), &
                                     SpcCoeff_RHS%Planck_C1(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! -----------------------------------
      ! The second Planck function constant
      ! -----------------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Planck_C2(l), &
                                  SpcCoeff_RHS%Planck_C2(l), &
                                  ULP = ULP                  ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Planck_C2', l, &
                                     SpcCoeff_LHS%Planck_C2(l), &
                                     SpcCoeff_RHS%Planck_C2(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! --------------------------
      ! The band correction offset
      ! --------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Band_C1(l), &
                                  SpcCoeff_RHS%Band_C1(l), &
                                  ULP = ULP                ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Band_C1', l, &
                                     SpcCoeff_LHS%Band_C1(l), &
                                     SpcCoeff_RHS%Band_C1(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! -------------------------
      ! The band correction slope
      ! -------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Band_C2(l), &
                                  SpcCoeff_RHS%Band_C2(l), &
                                  ULP = ULP                ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Band_C2', l, &
                                     SpcCoeff_LHS%Band_C2(l), &
                                     SpcCoeff_RHS%Band_C2(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! ---------------------------
      ! The microwave channel flags
      ! ---------------------------

      IF ( SpcCoeff_LHS%Is_Microwave_Channel(l) /= SpcCoeff_RHS%Is_Microwave_Channel(l) ) THEN
        Error_Status = FAILURE
        Format_String = '( "Is_Microwave_Channel values are different, ", '//INT_FORMAT//&
                        ', " vs. ", '//INT_FORMAT//', ",  for channel index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%Is_Microwave_Channel(l), &
                        SpcCoeff_RHS%Is_Microwave_Channel(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! ------------------------
      ! The channel polarization
      ! ------------------------

      IF ( SpcCoeff_LHS%Polarization(l) /= SpcCoeff_RHS%Polarization(l) ) THEN
        Error_Status = FAILURE
        Format_String = '( "Polarization flags are different, ", '//INT_FORMAT//&
                        ', " vs. ", '//INT_FORMAT//', ",  for channel index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%Polarization(l), &
                        SpcCoeff_RHS%Polarization(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! ------------------------------
      ! The cosmic background radiance
      ! ------------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Cosmic_Background_Radiance(l), &
                                  SpcCoeff_RHS%Cosmic_Background_Radiance(l), &
                                  ULP = ULP                                   ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Cosmic_Background_Radiance', l, &
                                     SpcCoeff_LHS%Cosmic_Background_Radiance(l), &
                                     SpcCoeff_RHS%Cosmic_Background_Radiance(l) )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! --------------------------------
      ! The solar affected channel flags
      ! --------------------------------

      IF ( SpcCoeff_LHS%Is_Solar_Channel(l) /= SpcCoeff_RHS%Is_Solar_Channel(l) ) THEN
        Error_Status = FAILURE
        Format_String = '( "Is_Solar_Channel values are different, ", '//INT_FORMAT//&
                        ', " vs. ", '//INT_FORMAT//', ",  for channel index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%Is_Solar_Channel(l), &
                        SpcCoeff_RHS%Is_Solar_Channel(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! -----------------------------------------------
      ! The Kurucz TOA solar irradiance source function
      ! -----------------------------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Solar_Irradiance(l), &
                                  SpcCoeff_RHS%Solar_Irradiance(l), &
                                  ULP = ULP                         ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Solar_Irradiance', l, &
                                     SpcCoeff_LHS%Solar_Irradiance(l), &
                                     SpcCoeff_RHS%Solar_Irradiance(l) )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF

    END DO Channel_Loop

  END FUNCTION Equal_Sensor


  FUNCTION Equal_Spectral( SpcCoeff_LHS, &  ! Input
                           SpcCoeff_RHS, &  ! Input
                           ULP_Scale,    &  ! Optional input
                           Check_All,    &  ! Optional input
                           RCS_Id,       &  ! Revision control
                           Message_Log ) &  ! Error messaging
                         RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN )  :: SpcCoeff_LHS
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN )  :: SpcCoeff_RHS

    ! -- Optional input
    INTEGER,              OPTIONAL, INTENT( IN )  :: ULP_Scale
    INTEGER,              OPTIONAL, INTENT( IN )  :: Check_All

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Equal_SpcCoeff(Spectral)'
    CHARACTER( * ), PARAMETER :: INT_FORMAT = 'i4'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    CHARACTER( 256 ) :: Format_String
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: l, n, ln



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                   -- CHECK THE OPTIONAL ARGUMENTS --                     #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Test the ULP_Scale argument
    ! ---------------------------

    ! -- Default precision is a single unit in last place
    ULP = 1
    ! -- ... unless the ULP_Scale argument is set and positive
    IF ( PRESENT( ULP_Scale ) ) THEN
      IF ( ULP_Scale > 0 ) ULP = ULP_Scale
    END IF


    ! ---------------------------
    ! Test the Check_All argument
    ! ---------------------------

    ! -- Default action is to return on ANY difference...
    Check_Once = .TRUE.
    ! -- ...unless the Check_All argument is set
    IF ( PRESENT( Check_All ) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! The LHS structure
    ! -----------------

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SpcCoeff_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------
    ! The RHS structure
    ! -----------------

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT SpcCoeff_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- CHECK SCALAR MEMBERS --                        #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Release/Version info
    ! --------------------

    IF ( ( SpcCoeff_LHS%Release /= SpcCoeff_RHS%Release ) .OR. &
         ( SpcCoeff_LHS%Version /= SpcCoeff_RHS%Version )      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Release/Version numbers are different : ", &
                        &i2, ".", i2.2, " vs. ", i2, ".", i2.2 )' ) &
                      SpcCoeff_LHS%Release, SpcCoeff_LHS%Version, &
                      SpcCoeff_RHS%Release, SpcCoeff_RHS%Version
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------
    ! Dimensions
    ! ----------

    ! -- The number of channels
    IF ( SpcCoeff_LHS%n_Channels /= SpcCoeff_RHS%n_Channels ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Channels dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      SpcCoeff_LHS%n_Channels, SpcCoeff_RHS%n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- The number of nodes
    IF ( SpcCoeff_LHS%n_Nodes /= SpcCoeff_RHS%n_Nodes ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Nodes dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      SpcCoeff_LHS%n_Nodes, SpcCoeff_RHS%n_Nodes
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- The maximum number of channels per node
    IF ( SpcCoeff_LHS%Max_Channels_per_Node /= &
         SpcCoeff_RHS%Max_Channels_per_Node    ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Max_Channels_per_Node dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      SpcCoeff_LHS%Max_Channels_per_Node, SpcCoeff_RHS%Max_Channels_per_Node
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------
    ! n_Sensors value
    ! ---------------

    IF ( SpcCoeff_LHS%n_Sensors /= SpcCoeff_RHS%n_Sensors ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Sensors values are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      SpcCoeff_LHS%n_Sensors, SpcCoeff_RHS%n_Sensors
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#          -- CHECK CHANNEL-BASED POINTER MEMBERS BY CHANNEL --            #
    !#                                                                          #
    !# Each structure member is tested separately. It's a bit of a brain dead   #
    !# way to do it, but easiest to implement since the data types differ.      #
    !# Also, each channel is tested explicitly, rather than using the ANY       #
    !# or ALL intrinsic functions, since I wanted to highlight the actual       #
    !# channel index where any difference occured so it would be very easy to   #
    !# track down the location of the difference.                               #
    !#--------------------------------------------------------------------------#

    Channel_Loop: DO l = 1, SpcCoeff_RHS%n_Channels


      ! ---------------------
      ! The Sensor Descriptor
      ! ---------------------

      IF ( SpcCoeff_LHS%Sensor_Descriptor(l) /= SpcCoeff_RHS%Sensor_Descriptor(l) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Descriptor values are different, ", &
                          &a, " vs. ", a, ",  for channel index # ", i4 )' ) &
                        TRIM( SpcCoeff_LHS%Sensor_Descriptor(l) ), &
                        TRIM( SpcCoeff_RHS%Sensor_Descriptor(l) ), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! ---------------
      ! The Sensor Type
      ! ---------------

      IF ( SpcCoeff_LHS%Sensor_Type(l) /= SpcCoeff_RHS%Sensor_Type(l) ) THEN
        Error_Status = FAILURE
        IF ( SpcCoeff_LHS%Sensor_Type(l) > INVALID_SENSOR .AND. &
             SpcCoeff_LHS%Sensor_Type(l) < N_SENSOR_TYPES .AND. &
             SpcCoeff_RHS%Sensor_Type(l) > INVALID_SENSOR .AND. &
             SpcCoeff_RHS%Sensor_Type(l) < N_SENSOR_TYPES       ) THEN
          WRITE( Message, '( "Sensor types are different, ", &
                            &a, " vs. ", a, ",  for channel index # ", i4 )' ) &
                          TRIM( SENSOR_TYPE_NAME( SpcCoeff_LHS%Sensor_Type(l) ) ), &
                          TRIM( SENSOR_TYPE_NAME( SpcCoeff_RHS%Sensor_Type(l) ) ), &
                          l
        ELSE
          WRITE( Message, '( "Sensor type values are different or invalid, ", &
                            &i5, " vs. ", i5, ",  for channel index # ", i4 )' ) &
                          SpcCoeff_LHS%Sensor_Type(l), &
                          SpcCoeff_RHS%Sensor_Type(l), &
                          l
        END IF
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! ------------------
      ! The NCEP sensor ID
      ! ------------------

      IF ( SpcCoeff_LHS%NCEP_Sensor_ID(l) /= SpcCoeff_RHS%NCEP_Sensor_ID(l) ) THEN
        Error_Status = FAILURE
        Format_String = '( "NCEP_Sensor_ID values are different, ", '//INT_FORMAT//&
                        ', " vs. ", '//INT_FORMAT//', ",  for channel index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%NCEP_Sensor_ID(l), &
                        SpcCoeff_RHS%NCEP_Sensor_ID(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! --------------------
      ! The WMO Satellite ID
      ! --------------------

      IF ( SpcCoeff_LHS%WMO_Satellite_ID(l) /= SpcCoeff_RHS%WMO_Satellite_ID(l) ) THEN
        Error_Status = FAILURE
        Format_String = '( "WMO_Satellite_ID values are different, ", '//INT_FORMAT//&
                        ', " vs. ", '//INT_FORMAT//', ",  for channel index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%WMO_Satellite_ID(l), &
                        SpcCoeff_RHS%WMO_Satellite_ID(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! -----------------
      ! The WMO Sensor ID
      ! -----------------

      IF ( SpcCoeff_LHS%WMO_Sensor_ID(l) /= SpcCoeff_RHS%WMO_Sensor_ID(l) ) THEN
        Error_Status = FAILURE
        Format_String = '( "WMO_Sensor_ID values are different, ", '//INT_FORMAT//&
                        ', " vs. ", '//INT_FORMAT//', ",  for channel index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%WMO_Sensor_ID(l), &
                        SpcCoeff_RHS%WMO_Sensor_ID(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! --------------------------
      ! The sensor channel numbers
      ! --------------------------

      IF ( SpcCoeff_LHS%Sensor_Channel(l) /= SpcCoeff_RHS%Sensor_Channel(l) ) THEN
        Error_Status = FAILURE
        Format_String = '( "Sensor_Channel values are different, ", '//INT_FORMAT//&
                        ', " vs. ", '//INT_FORMAT//', ",  for channel index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%Sensor_Channel(l), &
                        SpcCoeff_RHS%Sensor_Channel(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        RETURN
      END IF


      ! ---------------------
      ! The channel frequency
      ! ---------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Frequency(l), &
                                  SpcCoeff_RHS%Frequency(l), &
                                  ULP = ULP                  ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Frequency', l, &
                                     SpcCoeff_LHS%Frequency(l), &
                                     SpcCoeff_RHS%Frequency(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! ----------------------
      ! The channel wavenumber
      ! ----------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Wavenumber(l), &
                                  SpcCoeff_RHS%Wavenumber(l), &
                                  ULP = ULP                   ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Wavenumber', l, &
                                     SpcCoeff_LHS%Wavenumber(l), &
                                     SpcCoeff_RHS%Wavenumber(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! ----------------------------------
      ! The first Planck function constant
      ! ----------------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Planck_C1(l), &
                                  SpcCoeff_RHS%Planck_C1(l), &
                                  ULP = ULP                  ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Planck_C1', l, &
                                     SpcCoeff_LHS%Planck_C1(l), &
                                     SpcCoeff_RHS%Planck_C1(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! -----------------------------------
      ! The second Planck function constant
      ! -----------------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Planck_C2(l), &
                                  SpcCoeff_RHS%Planck_C2(l), &
                                  ULP = ULP                  ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Planck_C2', l, &
                                     SpcCoeff_LHS%Planck_C2(l), &
                                     SpcCoeff_RHS%Planck_C2(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF



      ! --------------------------
      ! The band correction offset
      ! --------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Band_C1(l), &
                                  SpcCoeff_RHS%Band_C1(l), &
                                  ULP = ULP                ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Band_C1', l, &
                                     SpcCoeff_LHS%Band_C1(l), &
                                     SpcCoeff_RHS%Band_C1(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! -------------------------
      ! The band correction slope
      ! -------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Band_C2(l), &
                                  SpcCoeff_RHS%Band_C2(l), &
                                  ULP = ULP                ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Band_C2', l, &
                                     SpcCoeff_LHS%Band_C2(l), &
                                     SpcCoeff_RHS%Band_C2(l)  )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! ------------------------
      ! The channel polarization
      ! ------------------------

      IF ( SpcCoeff_LHS%Polarization(l) /= SpcCoeff_RHS%Polarization(l) ) THEN
        Error_Status = FAILURE
        Format_String = '( "Polarization flags are different, ", '//INT_FORMAT//&
                        ', " vs. ", '//INT_FORMAT//', ",  for channel index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%Polarization(l), &
                        SpcCoeff_RHS%Polarization(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! --------------------------------
      ! The solar affected channel flags
      ! --------------------------------

      IF ( SpcCoeff_LHS%Is_Solar_Channel(l) /= SpcCoeff_RHS%Is_Solar_Channel(l) ) THEN
        Error_Status = FAILURE
        Format_String = '( "Is_Solar_Channel values are different, ", '//INT_FORMAT//&
                        ', " vs. ", '//INT_FORMAT//', ",  for channel index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%Is_Solar_Channel(l), &
                        SpcCoeff_RHS%Is_Solar_Channel(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                             Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! -----------------------------------------------
      ! The Kurucz TOA solar irradiance source function
      ! -----------------------------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Solar_Irradiance(l), &
                                  SpcCoeff_RHS%Solar_Irradiance(l), &
                                  ULP = ULP                         ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Solar_Irradiance', l, &
                                     SpcCoeff_LHS%Solar_Irradiance(l), &
                                     SpcCoeff_RHS%Solar_Irradiance(l) )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! ---------------------------
      ! The MW_and_IR_Channel_Index
      ! ---------------------------

      IF ( SpcCoeff_LHS%MW_and_IR_Channel_Index(l) /= SpcCoeff_RHS%MW_and_IR_Channel_Index(l) ) THEN
        Error_Status = FAILURE
        Format_String = '( "MW_and_IR_Channel_Index flags are different, ", '//INT_FORMAT//&
                        ', " vs. ", '//INT_FORMAT//', ",  for channel index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%MW_and_IR_Channel_Index(l), &
                        SpcCoeff_RHS%MW_and_IR_Channel_Index(l), &
                        l
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF

    END DO Channel_Loop



    !#--------------------------------------------------------------------------#
    !#               -- CHECK NODE-BASED POINTER MEMBERS BY NODE --             #
    !#                                                                          #
    !# Each structure member is tested separately. It's a bit of a brain dead   #
    !# way to do it, but easiest to implement since the data types differ.      #
    !# Also, each node is tested explicitly, rather than using the ANY          #
    !# or ALL intrinsic functions, since I wanted to highlight the actual       #
    !# node index where any difference occured so it would be very easy to      #
    !# track down the location of the difference.                               #
    !#--------------------------------------------------------------------------#

    Node_Loop: DO n = 1, SpcCoeff_RHS%n_Nodes


      ! ----------------------
      ! The node channel count
      ! ----------------------

      IF ( SpcCoeff_LHS%n_Channels_per_Node(n) /= &
           SpcCoeff_RHS%n_Channels_per_Node(n)    ) THEN
        Error_Status = FAILURE
        Format_String = '( "n_Channels_per_Node flags are different, ", '//&
                        INT_FORMAT//', " vs. ", '//INT_FORMAT//&
                        ', ",  for node index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%n_Channels_per_Node(n), &
                        SpcCoeff_RHS%n_Channels_per_Node(n), &
                        n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! --------------------
      ! The channel-node map
      ! --------------------

      IF ( ANY( SpcCoeff_LHS%Channel_Node_Map(:,n) /= &
                SpcCoeff_RHS%Channel_Node_Map(:,n)    ) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Channel_Node_Map values are different ", &
                          &"for node index #", i4 )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! -----------------------------------
      ! The combined MW and IR node indices
      ! -----------------------------------

      IF ( SpcCoeff_LHS%MW_and_IR_Node_Index(n) /= &
           SpcCoeff_RHS%MW_and_IR_Node_Index(n)    ) THEN
        Error_Status = FAILURE
        Format_String = '( "MW_and_IR_Node_Index flags are different, ", '//&
                        INT_FORMAT//', " vs. ", '//INT_FORMAT//&
                        ', ",  for node index # ", i4 )'
        WRITE( Message, FMT = TRIM( Format_String ) ) &
                        SpcCoeff_LHS%MW_and_IR_Node_Index(n), &
                        SpcCoeff_RHS%MW_and_IR_Node_Index(n), &
                        n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! ------------------
      ! The node frequency
      ! ------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Node_Frequency(n), &
                                  SpcCoeff_RHS%Node_Frequency(n), &
                                  ULP = ULP                  ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Node_Frequency', n, &
                                     SpcCoeff_LHS%Node_Frequency(n), &
                                     SpcCoeff_RHS%Node_Frequency(n), &
                                     Node = SET )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! -------------------
      ! The node wavenumber
      ! -------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Node_Wavenumber(n), &
                                  SpcCoeff_RHS%Node_Wavenumber(n), &
                                  ULP = ULP                   ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Node_Wavenumber', n, &
                                     SpcCoeff_LHS%Node_Wavenumber(n), &
                                     SpcCoeff_RHS%Node_Wavenumber(n), &
                                     Node = SET )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! ----------------------------------
      ! The first Planck function constant
      ! ----------------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Node_Planck_C1(n), &
                                  SpcCoeff_RHS%Node_Planck_C1(n), &
                                  ULP = ULP                  ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Node_Planck_C1', n, &
                                     SpcCoeff_LHS%Node_Planck_C1(n), &
                                     SpcCoeff_RHS%Node_Planck_C1(n), &
                                     Node = SET )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! -----------------------------------
      ! The second Planck function constant
      ! -----------------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Node_Planck_C2(n), &
                                  SpcCoeff_RHS%Node_Planck_C2(n), &
                                  ULP = ULP                  ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Node_Planck_C2', n, &
                                     SpcCoeff_LHS%Node_Planck_C2(n), &
                                     SpcCoeff_RHS%Node_Planck_C2(n), &
                                     Node = SET )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF



      ! ------------------------------
      ! The cosmic background radiance
      ! ------------------------------

      IF ( .NOT. ( Compare_Float( SpcCoeff_LHS%Cosmic_Background_Radiance(n), &
                                  SpcCoeff_RHS%Cosmic_Background_Radiance(n), &
                                  ULP = ULP                                   ) ) ) THEN
        Error_Status = FAILURE
        Message = Construct_Message( 'Cosmic_Background_Radiance', n, &
                                     SpcCoeff_LHS%Cosmic_Background_Radiance(n), &
                                     SpcCoeff_RHS%Cosmic_Background_Radiance(n), &
                                     Node = SET )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF

    END DO Node_Loop

  END FUNCTION Equal_Spectral





!----------------------------------------------------------------------------------
!S+
! NAME:
!       Check_SpcCoeff_Release
!
! PURPOSE:
!       Function to check the SpcCoeff Release value.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Check_SpcCoeff_Release( SpcCoeff,                 &  ! Input
!                                              RCS_Id      = RCS_Id,     &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SpcCoeff:      SpcCoeff structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_Sensor_type
!                                    OR
!                                  SpcCoeff_Spectral_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structure Release value is valid.
!                         == FAILURE the structure Release value is NOT valid
!                                    and either a data file file or software
!                                    update is required.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 19-Jun-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!----------------------------------------------------------------------------------

  FUNCTION Check_Sensor( SpcCoeff,     &  ! Input
                         RCS_Id,       &  ! Revision control
                         Message_Log ) &  ! Error messaging
                       RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN )  :: SpcCoeff

    ! -- Optional output
    CHARACTER( * ),     OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Check_SpcCoeff_Release(Sensor)'



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CHECK THE RELEASE --                          #
    !#--------------------------------------------------------------------------#

    Error_Status = Check_Release( SpcCoeff%Release, &
                                  SPCCOEFF_SENSOR_RELEASE, &
                                  ROUTINE_NAME, &
                                  Message_Log = Message_Log )

  END FUNCTION Check_Sensor


  FUNCTION Check_Spectral( SpcCoeff,     &  ! Input
                           RCS_Id,       &  ! Revision control
                           Message_Log ) &  ! Error messaging
                         RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN )  :: SpcCoeff

    ! -- Optional output
    CHARACTER( * ),       OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Check_SpcCoeff_Release(Spectral)'



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CHECK THE RELEASE --                          #
    !#--------------------------------------------------------------------------#

    Error_Status = Check_Release( SpcCoeff%Release, &
                                  SPCCOEFF_SPECTRAL_RELEASE, &
                                  ROUTINE_NAME, &
                                  Message_Log = Message_Log )

  END FUNCTION Check_Spectral


  FUNCTION Check_Release( Structure_Value, &
                          Valid_Value, &
                          Routine_Name, &
                          Message_Log ) &
                        RESULT( Error_Status )

    ! -- Arguments
    INTEGER,                  INTENT( IN ) :: Structure_Value
    INTEGER,                  INTENT( IN ) :: Valid_Value
    CHARACTER( * ),           INTENT( IN ) :: Routine_Name
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log

    ! -- Function result
    INTEGER :: Error_Status

    ! -- Local variables
    CHARACTER( 256 ) :: Message

    ! -- Define a successful return status
    Error_Status = SUCCESS

    ! -- Check that release is not too old
    IF ( Structure_Value < Valid_Value ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An SpcCoeff data update is needed. ", &
                        &"SpcCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      Structure_Value, Valid_Value
      CALL Display_Message( TRIM( Routine_Name ), &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Check that release is not too new
    IF ( Structure_Value > Valid_Value ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "A SpcCoeff software update is needed. ", &
                        &"SpcCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      Structure_Value, Valid_Value
      CALL Display_Message( TRIM( Routine_Name ), &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Check_Release




!--------------------------------------------------------------------------------
!S+
! NAME:
!       Count_SpcCoeff_Sensors
!
! PURPOSE:
!       Subroutine to count the number of different satellite/sensors in the
!       SpcCoeff structure and set the n_Sensors field.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Count_SpcCoeff_Sensors( SpcCoeff,                &  ! In/Output
!                                    Use_WMO_ID = Use_WMO_ID, &  ! Optional input
!                                    RCS_Id = RCS_Id          )  ! Revision control
!
! INPUT ARGUMENTS:
!       SpcCoeff:      Filled SpcCoeff structure.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_Sensor_type
!                                    OR
!                                  SpcCoeff_Spectral_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Use_WMO_ID:    Set this argument to use the WMO satellite and sensor
!                      IDs in the SpcCoeff structure to count the number of
!                      different sensors. By default, the NCEP sensor ID is
!                      used.
!                      If = 0, use NCEP sensor ID (default)
!                         = 1, use WMO satellite/sensor ID
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       The N_SENSORS field of the input SpcCoeff structure is modified.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Dec-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Count_Sensor( SpcCoeff,   &  ! In/Output
                           Use_WMO_ID, &  ! Optional input
                           RCS_Id      )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN OUT ) :: SpcCoeff

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN )     :: Use_WMO_ID

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Use_NCEP_ID



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENT --                 #
    !#--------------------------------------------------------------------------#

    Use_NCEP_ID = .TRUE.
    IF ( PRESENT( Use_WMO_ID ) ) THEN
      IF ( Use_WMO_ID == SET ) Use_NCEP_ID = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- COUNT THE DIFFERENT SENSORS --                    #
    !#--------------------------------------------------------------------------#

    IF ( Use_NCEP_ID ) THEN

      SpcCoeff%n_Sensors = Count_Using_NCEP_ID( SpcCoeff%NCEP_Sensor_ID )

    ELSE

      SpcCoeff%n_Sensors = Count_Using_WMO_ID( SpcCoeff%WMO_Satellite_ID, &
                                               SpcCoeff%WMO_Sensor_ID )

    END IF

  END SUBROUTINE Count_Sensor


  SUBROUTINE Count_Spectral( SpcCoeff,   &  ! In/Output
                             Use_WMO_ID, &  ! Optional input
                             RCS_Id      )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN OUT ) :: SpcCoeff

    ! -- Optional input
    INTEGER,              OPTIONAL, INTENT( IN )     :: Use_WMO_ID

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Use_NCEP_ID



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENT --                 #
    !#--------------------------------------------------------------------------#

    Use_NCEP_ID = .TRUE.
    IF ( PRESENT( Use_WMO_ID ) ) THEN
      IF ( Use_WMO_ID == SET ) Use_NCEP_ID = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- COUNT THE DIFFERENT SENSORS --                    #
    !#--------------------------------------------------------------------------#

    IF ( Use_NCEP_ID ) THEN

      SpcCoeff%n_Sensors = Count_Using_NCEP_ID( SpcCoeff%NCEP_Sensor_ID )

    ELSE

      SpcCoeff%n_Sensors = Count_Using_WMO_ID( SpcCoeff%WMO_Satellite_ID, &
                                               SpcCoeff%WMO_Sensor_ID )

    END IF

  END SUBROUTINE Count_Spectral





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Version_SpcCoeff
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the SpcCoeff data structure.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Version_SpcCoeff( SpcCoeff,       &  ! Input
!                              Version_Info,   &  ! Output
!                              RCS_Id = RCS_Id )  ! Revision control
!
! INPUT ARGUMENTS:
!       SpcCoeff:      Filled SpcCoeff structure.
!                      UNITS:      N/A
!                      TYPE:       SpcCoeff_Sensor_type
!                                    OR
!                                  SpcCoeff_Spectral_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Version_Info:  String containing version and dimension information
!                      about the passed SpcCoeff data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Dec-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Version_Sensor( SpcCoeff,     &  ! Input
                             Version_Info, &  ! Output
                             RCS_Id        )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN )  :: SpcCoeff

    ! -- Output
    CHARACTER( * ),               INTENT( OUT ) :: Version_Info

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT ) :: RCS_Id


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 512 ) :: Long_String



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- FILL THE VERSION INFO STRING --                   #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------------
    ! Write the required data to the local string
    ! -------------------------------------------

    WRITE( Long_String, '( a,1x,"SpcCoeff(Sensor) RELEASE.VERSION: ",i2,".",i2.2,2x,&
                           &"N_CHANNELS=",i5,2x,&
                           &"N_SENSORS=",i3 )' ) &
                        ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                        SpcCoeff%Release, SpcCoeff%Version, &
                        SpcCoeff%n_Channels, &
                        SpcCoeff%n_Sensors


    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Version_Info = Long_String( 1:MIN( LEN( Version_Info ),    &
                                       LEN_TRIM( Long_String ) ) )

  END SUBROUTINE Version_Sensor


  SUBROUTINE Version_Spectral( SpcCoeff,     &  ! Input
                               Version_Info, &  ! Output
                               RCS_Id        )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN )  :: SpcCoeff

    ! -- Output
    CHARACTER( * ),                 INTENT( OUT ) :: Version_Info

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT ) :: RCS_Id


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 512 ) :: Long_String



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- FILL THE VERSION INFO STRING --                   #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------------
    ! Write the required data to the local string
    ! -------------------------------------------

    WRITE( Long_String, '( a,1x,"SpcCoeff(Spectral) RELEASE.VERSION: ",i2,".",i2.2,2x,&
                           &"N_CHANNELS=",i5,2x,&
                           &"N_NODES=",i5,2x,&
                           &"MAX_CHANNELS_PER_NODE=",i2,2x,&
                           &"N_SENSORS=",i3 )' ) &
                        ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                        SpcCoeff%Release, SpcCoeff%Version, &
                        SpcCoeff%n_Channels, &
                        SpcCoeff%n_Nodes, &
                        SpcCoeff%Max_Channels_per_Node, &
                        SpcCoeff%n_Sensors


    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Version_Info = Long_String( 1:MIN( LEN( Version_Info ),    &
                                       LEN_TRIM( Long_String ) ) )

  END SUBROUTINE Version_Spectral

END MODULE SpcCoeff_Define


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: SpcCoeff_Define.f90,v 6.3 2005/07/05 23:54:51 paulv Exp $
!
! $Date: 2005/07/05 23:54:51 $
!
! $Revision: 6.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SpcCoeff_Define.f90,v $
! Revision 6.3  2005/07/05 23:54:51  paulv
! - Corrected bugs in Spectral Allocate() and Equal() functions.
!
! Revision 6.2  2005/07/05 22:32:18  paulv
! - Fixed some minor misnamed variable bugs.
!
! Revision 6.1  2005/07/05 21:38:04  paulv
! - Added some additional components to the Spectral SpcCoeff structure
!   definition.
! - Moved some node-based components back to being channel-based in the
!   Spectral SpcCoeff structure definition.
! - Still untested.
!
! Revision 6.0  2005/07/05 11:39:34  paulv
! - Major update. There are now two SpcCoeff data structures; a Sensor based
!   one (the old, regular SpcCoeff) and a Spectral based one (for use with the
!   monomchromatic OSS code). Definition and I/O modules have been modified
!   accordingly. Untested.
!
! Revision 5.2  2005/04/01 18:02:25  paulv
! - Corrected header documentation detailing valid parameter names for the
!   Sensor_Type structure member.
!
! Revision 5.1  2005/03/31 21:24:44  paulv
! - Corrected bug in minus45L_POLARIZATION parameter declaration.
! - Made invalid sensor ID parameters PUBLIC.
! - Updated test for invalid sensor type.
!
! Revision 5.0  2005/03/30 21:59:30  paulv
! - New Version.
! - Added Sensor_Type flag to structure
! - Modified Polarization structure component from Stokes vector type of
!   representation to a oplarization type flag.
! - Removed the Cosmic_Background_Temperature component
! - Removed the Blackbody_Irradiance component.
! - All routines modified to accomodate the above changes. UNTESTED.
!
! Revision 4.8  2005/02/18 23:13:46  paulv
! - Added invalid WMO ID parameters.
!
! Revision 4.7  2004/09/27 19:05:28  paulv
! - Concatenation now uses newest version number for result.
!
! Revision 4.6  2004/09/09 20:25:43  paulv
! - Added association test in destroy function. If no pointer components
!   are associated, the function returns without modifying the n_Allocates
!   counter. Thus, calling the destroy function on an already destroyed
!   structure want flag an error.
!
! Revision 4.5  2004/09/02 16:47:19  paulv
! - Added optional ULP_Scale argument to Equal_SpcCoeff() function.
! - Replaced .EqualTo. operator with Compare_Float() function call in
!   Equal_SpcCoeff() function to allow user to specify an ULP scale value
!   for the floating point comparisons.
! - Added internal subprogram Construct_Message() to Equal_SpcCoeff() function
!   to construct a standard message when a floating point comparison fails.
!
! Revision 4.4  2004/08/31 23:00:06  paulv
! - Replaced calls to Compare_Float() function with use of .EqualTo. operator
!   in Equal_SpcCoeff() function.
!
! Revision 4.3  2004/08/23 14:39:22  paulv
! - Removed warning message from Allocate() function when the structure argument
!   is already allocated. Now it is silently destroyed and reallocated.
!
! Revision 4.2  2004/08/18 16:16:43  paulv
! - Upgraded to Fortran95.
! - Derived type component initialisation is now done in the defintion block.
! - Init_SpcCoeff() subroutine has been removed.
! - Intent of SpcCoeff dummy argument in Clear_SpcCoeff() routine changed from
!   OUT to IN OUT to prevent memory leaks.
! - Added optional No_Clear argument to Destroy_SpcCoeff() function.
! - Intent of SpcCoeff dummy argument in Allocate_SpcCoeff() routine changed from
!   OUT to IN OUT to prevent memory leaks.
! - Call to Destroy_SpcCoeff() added to Allocate_SpcCoeff() function for the case
!   where the input SpcCoeff argument is already allocated.
! - Intent of SpcCoeff_out dummy argument in Assign_SpcCoeff() routine changed from
!   OUT to IN OUT to prevent memory leaks.
! - Using new Compare_Float_Numbers module with elemental function. This required
!   changing the comparison of the polarisation component from
!     IF ( .NOT. Compare_Float( SpcCoeff_LHS%Polarization(:,l), &
!                               SpcCoeff_RHS%Polarization(:,l)  ) ) THEN
!   to
!     IF ( ANY( .NOT. Compare_Float( SpcCoeff_LHS%Polarization(:,l), &
!                                    SpcCoeff_RHS%Polarization(:,l)  ) ) ) THEN
! - Updated header documentation.
!
! Revision 4.1  2004/06/25 22:31:32  paulv
! - Minor header documentation update.
!
! Revision 4.0  2004/06/25 19:40:18  paulv
! - Updated polarization component to a full Stokes vector.
!
! Revision 3.2  2004/06/25 17:12:07  paulv
! - Removed unused variables from type declarations.
! - Cosmetic changes.
!
! Revision 3.1  2004/06/24 18:56:44  paulv
! - Minor documentation update.
!
! Revision 3.0  2004/05/17 17:40:41  paulv
! - Added Sensor_Descriptor component to SpcCoeff structure. Modified the
!   netCDF and Binary I/O modules to handle the new component. New SpcCoeff
!   release number is 3.
!
! Revision 2.15  2004/05/12 20:48:22  paulv
! - Added assignment of optional RCS_ID argument in the Count_SpcCoeff_Sensors()
!   routine. Forgot to include it, even though the RCS_Id is an optional argument.
!
! Revision 2.14  2004/03/09 17:24:36  paulv
! - Mostly cosmetic changes. Some fixes to eliminate possibilities of
!   exceeding string lengths.
!
! Revision 2.13  2003/11/13 19:35:47  paulv
! - Updated header documentation.
!
! Revision 2.12  2003/10/24 18:18:14  paulv
! - Code category changed from
!     NCEP RTM : Coefficients : SpcCoeff
!   to
!     Instrument Information : SpcCoeff
!
! Revision 2.11  2003/10/24 18:04:07  paulv
! - Changed scalar component reinitialisation in Clear() routine to 0 from INVALID.
! - Intent of SpcCoeff structure in Destroy() routine cahnged to IN OUT from OUT.
! - Added CHECK_ALL optional argument to Equal() routine. Default action is to
!   return as soon as ANY difference is found. Setting this argument will still
!   check ALL the SpcCoeff floating point data even if differences are found.
! - Added MESSAGE_LOG optional argument to CHECK_SPCCOEFF_RELEASE() function.
!
! Revision 2.10  2003/06/19 21:32:24  paulv
! - Added SPCCOEFF_RELEASE and SPCCOEFF_VERSION parameters to assign to SpcCoeff
!   structure Release and Version members during initialisation.
! - Added optional VERSION argument to the Initialize_SpcCoeff() function to
!   allow user to override default initialisation value.
! - Added Check_SpcCoeff_Release() function.
!
! Revision 2.9  2003/04/14 19:54:38  paulv
! - Corrected bug in sensor counting routine when the WMO Ids were used to
!   identify separate sensors.
!
! Revision 2.8  2003/04/02 17:07:24  paulv
! - Added USE of Compare_Float_Numbers module. All floating point comparisons
!   in the Equal_SpcCoeff() function are now done using the Compare_Float()
!   function.
!
! Revision 2.7  2003/04/01 14:48:50  paulv
! - Minor documentation corrections.
!
! Revision 2.6  2003/03/31 16:49:59  paulv
! - Added RCS_Id optional output argument to public functions.
! - Removed Check_SpcCoeff_Status routine and replaced it with the PRIVATE
!   Associated_SpcCoeff function. All references to the former routine have been replaced
!   with the latter.
! - Updated documentation.
!
! Revision 2.5  2003/02/10 22:54:33  paulv
! - Added SpcCoeff structure concatenation function.
!
! Revision 2.4  2002/12/26 17:34:06  paulv
! - Added Equal_SpcCoeff() function to compare SpcCoeff structures.
! - Added Check_SpcCoeff_Status() function to check the association
!   status of pointer members in an SpcCoeff structure.
! - Added Version_SpcCoeff() function to generate an info string used in
!   INFORMATION message output.
!
! Revision 2.3  2002/12/24 00:13:46  paulv
! - Corrected bug in WMO_Satellite_ID variable name.
!
! Revision 2.2  2002/12/23 12:51:26  paulv
! - Completed new versions. Untested.
!
! Revision 2.1  2002/12/20 22:22:23  paulv
! - New version with more sensor channel information.
!
! Revision 1.2  2002/08/16 20:49:32  paulv
! - Corrected bug in SpcCoeff TYPE specification.
!
! Revision 1.1  2002/08/13 20:05:39  paulv
! Initial checkin
!
!
!
!
!
