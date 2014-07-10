	SUBROUTINE ST_LSTR  ( string, lens, iret )
C************************************************************************
C* ST_LSTR								*
C*									*
C* This subroutine returns the number of characters in a string 	*
C* disregarding trailing null characters, tabs and spaces.		*
C*									*
C* ST_LSTR  ( STRING, LENS, IRET )						*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String 				*
C*									*
C* Output parameters:							*
C*	LENS		INTEGER 	Length of string		*
C*	IRET		INTEGER		Return code			*
C*				 	 0 = normal return 		*
C**									*
C* Log:									*
C* J. Woytek/GSFC	 6/82 	STR_LNSTR				*
C* I. Graffman/RDS	 2/84 	Fix zero length string handling		*
C* M. desJardins/GSFC	 6/88	Rewritten				*
C* D. Kidwell/NCEP      10/96   Ported to Cray                          *
C************************************************************************
C* GEMPRM.PRM
C*
C* This include file contains parameter definitions for the GEMPAK
C* software routines in the ST_ and PR_ libraries.
C*
C* CRAY version
C**
C* Log:  
C*	Kidwell/NCEP	07/96	Adapted a subset of gemprm.prm for Cray
C************************************************************************
C!
C!	Missing data definitions
C!
 	PARAMETER	( RMISSD = -9999.0 )
C!						Missing data value
	PARAMETER	( RDIFFD =  0.1    )
C!						Missing value fuzziness
	PARAMETER	( IMISSD = -9999   )
C!						Missing integer value
	LOGICAL		  ERMISS
C!						Declare for stmt func
C!
C! 	Physical and mathematical constants
C!
	PARAMETER       ( PI = 3.14159265  )
C!                                              PI
	PARAMETER       ( DTR = PI / 180.  )
	PARAMETER       ( RTD = 180. / PI  )
C!                                              Degrees <--> Radians
	PARAMETER	( GRAVTY = 9.80616  )
C!						Acceleration of gravity
	PARAMETER	( RDGAS  = 287.04   )
	PARAMETER	( RKAP   = RDGAS / GRAVTY )
C!						Gas constant of dry air
	PARAMETER	( RKAPPA = 2. / 7. )
C!						Poisson constant
	PARAMETER	( GAMUSD = 6.5 )
C!						US std atmos lapse rate
	PARAMETER	( TMCK   = 273.15 )
C!						Centigrade -> Kelvin
C!
C!	ASCII character constants 
C!
C!	Since the Cray does not allow the use of a function (e.g.,
C!	CHAR) to define a parameter, nor does it allow a character
C!	to be defined directly as a hex (X) value, the convolutions
C!      below are necessary to define the character values for Cray.
C!
C
	CHARACTER * 1 chnull, chtab, chspac, chtlda
C
	CHARACTER * 8 c8null, c8tab, c8spac, c8tlda
C
	INTEGER       iigemc ( 4 )
C
	EQUIVALENCE ( chnull, c8null (8:8) ), ( chtab , c8tab  (8:8) ),
     +              ( chspac, c8spac (8:8) ), ( chtlda, c8tlda (8:8) ) 
C
	EQUIVALENCE ( iigemc ( 1), c8null ), ( iigemc ( 2), c8tab  ),
     +              ( iigemc ( 3), c8spac ), ( iigemc ( 4), c8tlda )
C
	DATA iigemc / X'00',    X'09',    X'20',    X'7E' /
C                     Null      Tab       Space     Tilda
C!
C*
	CHARACTER*(*)	string
C*
	CHARACTER*1	c
C*------------------------------------------------------------------------
	lens = 0
	iret = 0
C
C*	Get the actual length of the string.
C
	lens = LEN  ( string )
	IF  ( lens .eq. 0 )  RETURN
C
C*	Start at last character and loop backwards.
C
	ip = lens
	DO WHILE  ( ip .gt. 0 )
C
C*	    Get current value of string and check for space, null, tab.
c
	    c = string ( ip : ip )
	    IF  ( ( c .eq. CHSPAC ) .or. ( c .eq. CHNULL ) .or.
     +		  ( c .eq. CHTAB  ) )  THEN
		lens = lens - 1
		ip   = ip - 1
	      ELSE
		ip   = 0
	    END IF
	END DO
C*
	RETURN
	END
