	SUBROUTINE ST_READFHO  ( outstr, nchars, num, iret )
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
      INCLUDE 'parm.inc'

	CHARACTER*(*)	outstr (*)
	INTEGER		nchars (*)
C*
	CHARACTER*256	input
	CHARACTER*128	substr (mxthr+3)
        COMMON /fho/    numthr(mxvrbl), thresh(mxvrbl,mxthr)
C-----------------------------------------------------------------------
	iret   = 0
        mxstr = mxthr + 3
C
C   Note:  no adherence to format is necessary for this input.
C
        READ (5,'(A)', END=100) input
        CALL ST_CLST ( input, ' ', ' ', mxstr, substr, nx, ier )
	CALL ST_NUMB ( substr (1), num, ier )
	CALL ST_RMBL ( substr (2), outstr (1), nchars (1), ier )

C--- This part is used when only the Fortran code is involved --------
        CALL ST_NUMB ( substr (3), numthr(1), ier )
             do lthr=4,numthr(1)+3
             CALL ST_REAL ( substr(lthr), thresh(1,lthr-3), ier )
             enddo
C---------------------------------------------------------------------

C--- Readfho is a c program to read thresholds -----------------------
c       OPEN (99,FILE="thresholds")
c       WRITE (99,'(A)') input
c       CLOSE (99)
c       print*,'outstr(1)=',outstr(1)
c       call readfho(numthr(1), thresh(1,:))
        print*,'THRESHOLDS FOR ', outstr (1)(1:nchars (1)),':'
        print*,'NUM = ', numthr(1)
        print*, (thresh(1,l),l=1,numthr(1))
C---------------------------------------------------------------------

	IF ( num .eq. 1 ) RETURN
C     
C*	Read the remaining num-1 entries.
C     
	lng = 0
	DO n = 2, num
	    READ (5,'(A)', END=100) input
            CALL ST_CLST ( input, ' ', ' ', mxstr, substr, nx, ier )
	    CALL ST_RMBL ( substr (1), outstr (n), nchars (n), ier )

C--- This part is used when only the Fortran code is involved --------
C--- format E (00e+00) is not allowed for thresholds

        CALL ST_NUMB ( substr (2), numthr(n), ier )
             do lthr=3,numthr(n)+2
             CALL ST_REAL ( substr(lthr), thresh(n,lthr-2), ier )
             enddo
C---------------------------------------------------------------------

C--- Readfho is a c program to read thresholds (any format allowed) --

c       OPEN (99,FILE="thresholds")
c       print*,'n,input=',n,input
c       WRITE (99,'(I2,(A))') n, input
c       CLOSE (99)
c       print*,'before readfho'
c       call readfho(numthr(n), thresh(n,:))
c       print*,'after readfho'
        print*,'THRESHOLDS FOR ', outstr (n)(1:nchars (n)),':'
        print*,'NUM = ', numthr(n)
        print*, (thresh(n,l),l=1,numthr(n))
C---------------------------------------------------------------------
	END DO
C*
	RETURN
100	iret = -1
	RETURN
	END
