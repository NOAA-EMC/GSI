	SUBROUTINE ST_READ  ( outstr, nchars, num, iret )
C************************************************************************
C* ST_READ								*
C*									*
C* This subroutine reads a block of information from a control file.	*
C*									*
C* ST_READ  ( OUTSTR, NCHARS, NUM, IRET )				*
C*									*
C*									*
C* Output parameters:							*
C*	OUTSTR (24,NUM)	CHAR*		Output string array		*
C*      NCHARS (NUM)	INTEGER		Number of characters in strings *
C*	NUM		INTEGER		Number of strings read  	*
C*	IRET		INTEGER		Return code			*
C*				   	 0 = normal return 		*
C*					-1 = END OF FILE		*
C**									*
C* Log:									*
C* K. Brill	11/98							*
C************************************************************************
C*
	CHARACTER*(*)	outstr (*)
	INTEGER		nchars (*)
C*
	CHARACTER*256	input
	CHARACTER*128	substr (2)
C-----------------------------------------------------------------------
	iret   = 0
C
C   Note:  no adherence to format is necessary for this input.
C
        READ (5,'(A)', END=100) input
        CALL ST_CLST ( input, ' ', ' ', 2, substr, nx, ier )
	CALL ST_NUMB ( substr (1), num, ier )
	CALL ST_RMBL ( substr (2), outstr (1), nchars (1), ier )
	IF ( num .eq. 1 ) RETURN
C     
C*	Read the remaining num-1 entries.
C     
	lng = 0
	DO n = 2, num
	    READ (5,'(A)', END=100) input
	    CALL ST_RMBL ( input, outstr (n), nchars (n), ier )
	END DO
C*
	RETURN
100	iret = -1
	RETURN
	END
