	SUBROUTINE READ_AREA(outstr,nchars,num,ptr1,ptr2,usrmrk,iret )
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
C*      PTR1   (2,NUM)  REAL            long, lat of point1 of a rectangular region* 
C*      PTR2   (2,NUM)  REAL            long, lat of point2 of a rectangular region* 
C*	NUM		INTEGER		Number of strings read  	*
C*	IRET		INTEGER		Return code			*
C*				   	 0 = normal return 		*
C*					-1 = END OF FILE		*
C**									*
C* Log:									*
C* K. Brill	     11/98							*
C* Binbin. Zhou      3/2005 to read /USR    
C************************************************************************
C*
	CHARACTER*(*)	outstr(*) 
	INTEGER		nchars (*), nchs
C*
	CHARACTER*256	input
	CHARACTER*128	substr (10)
        CHARACTER*20 p2(4,50) 
        real ptr1(2,*), ptr2(2,*)
        integer usrmrk(100)
C-----------------------------------------------------------------------
	iret   = 0
        usrmrk = 0
C
C   Note:  no adherence to format is necessary for this input.
C
        READ (5,'(A)', END=100) input
        CALL ST_CLST ( input, ' ', ' ', 10, substr, nx, ier )
	CALL ST_NUMB ( substr (1), num, ier )
CC	CALL ST_RMBL ( substr (2), outstr (1), nchars(1), ier )
        nchars(1) = len_trim(substr(2))
        outstr(1) = substr(2)(1:nchars(1))


        IF (nx.eq.2) then
	  IF ( num .eq. 1 ) RETURN
        ELSE
CC        CALL ST_RMBL ( substr (3),  p2(1,1), nchs, ier )
CC        CALL ST_RMBL ( substr (4),  p2(2,1), nchs, ier )
CC        CALL ST_RMBL ( substr (5),  p2(3,1), nchs, ier )
CC        CALL ST_RMBL ( substr (6),  p2(4,1), nchs, ier )
          nchs = len_trim(substr(3))
          p2(1,1) = substr(3)(1:nchs)
          nchs = len_trim(substr(4))
          p2(2,1) = substr(4)(1:nchs)
          nchs = len_trim(substr(5))
          p2(3,1) = substr(5)(1:nchs)
          nchs = len_trim(substr(6))
          p2(4,1) = substr(6)(1:nchs)
          usrmrk(1) = 1
        END IF
C     
C*	Read the remaining num-1 entries.
C     
	DO n = 2, num
	    READ (5,'(A)', END=100) input
            CALL ST_CLST ( input, ' ', ' ', 10, substr, nx, ier )
            IF (nx. eq. 1) then
CC	     CALL ST_RMBL ( input, outstr (n), nchars (n), ier )
             nchars(n) = len_trim(input)
             outstr(n) = input(1:nchars(n))
            else
CC           CALL ST_RMBL ( substr (1), outstr (n), nchars (n), ier )
CC           CALL ST_RMBL ( substr (2),  p2(1,n), nchs, ier )
CC           CALL ST_RMBL ( substr (3),  p2(2,n), nchs, ier )
CC           CALL ST_RMBL ( substr (4),  p2(3,n), nchs, ier )
CC           CALL ST_RMBL ( substr (5),  p2(4,n), nchs, ier )
              nchars(n) = len_trim(substr(1))
              outstr(n) = substr(1)(1:nchars(n))
              nchs = len_trim(substr(2))
              p2(1,n) = substr(2)(1:nchs)
              nchs = len_trim(substr(3))
              p2(2,n) = substr(3)(1:nchs)
              nchs = len_trim(substr(4))
              p2(3,n) = substr(4)(1:nchs)
              nchs = len_trim(substr(5))
              p2(4,n) = substr(5)(1:nchs)
             usrmrk(n) = 1
           end if  
	END DO

        write(*,*)"area, ptr1(1),ptr1(2),ptr2(1),ptr2(2)"
        do n =1, num
         ptr1(1,n)=CharToReal(p2(1,n))
         ptr1(2,n)=CharToReal(p2(2,n))
         ptr2(1,n)=CharToReal(p2(3,n))
         ptr2(2,n)=CharToReal(p2(4,n))
         write(*,*)n, ptr1(1,n),ptr1(2,n),ptr2(1,n),ptr2(2,n)
        end do       

C*
	RETURN
100	iret = -1
	RETURN
	END
