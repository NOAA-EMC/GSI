	SUBROUTINE ST_RMBL  ( string, outstr, length, iret )
C************************************************************************
C* ST_RMBL								*
C*									*
C* This subroutine removes spaces and tabs from a string.  The input	*
C* and output strings may be the same variable.				*
C*									*
C* ST_RMBL  ( STRING, OUTSTR, LENGTH, IRET )				*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String				*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		String without blanks		*
C*	LENGTH		INTEGER		Length of output string		*
C*	IRET		INTEGER		Return code			*
C*				   	 0 = normal return 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/84						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/NMC	 3/92	Add temporary variable ttt		*
C* L. Sager/NCEP         2/96   Increased size of sss and ttt           *
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
	CHARACTER*(*)	string, outstr
C*
	CHARACTER	c*1, sss*160, ttt*160
C-----------------------------------------------------------------------
	iret   = 0
	length = 0
	sss    = string
	ttt    = ' '
C
C*	Get length of input string.
C
	CALL ST_LSTR  ( sss, lens, iret )
C
C*	Check each character to see if it is a blank.
C
	DO  i = 1, lens
	    c = sss (i:i)
	    IF  ( ( c .ne. CHSPAC ) .and. ( c .ne. CHTAB ) )  THEN
		length = length + 1
		ttt ( length : length ) = c
	    END IF
	END DO
C*
	outstr = ttt
C*
	RETURN
	END
