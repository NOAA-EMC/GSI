C******************************************************************
C
C  BRIEF DESCRIPTION OF PROGRAM MODULES:
C
C   dtgchk -  Check to see if a DTG is valid.
C   dtgdif -  wrapper for the dtgdif2 routine.
C   dtgdif2 - Given two DTGs, return the difference in hours (=MDTG-NDTG).
C   dtgmod -  Given base DTG and increment (+/- hours), return new DTG
C                 ( = indtg + idif ) and the status value.
C   dtgnum -  Given a DTG (YYYYMMDDHH), return integer values for
C		 year, month, day, hour, days into the year, and hours
C		 into the year.
C   dtgyrhr - Given a year and hours of the year, DTGYRHR returns
C                a DTG of format YYYYMMDDHH in NEWDTG.
C   icrdtg -  wrapper for the dtgmod routine.
C   
C******************************************************************

      subroutine icrdtg ( idtg1, idtg2, incr )
C
C
C   DESCRIPTION:
C      This is a wrapper for the dtgmod routine.
C      The purpose of the routine is to convert idtg1
C      into a 10 digit dtg prior to calling dtgmod and then
C      to convert idtg2 from 10 digits back to 8 digits after
C      returning from the dtgmod call.
C
C     DTGMOD description:
C      Given base DTG and increment (+/- hours), return new
C      DTG ( = idtg1 + incr ) and the status value.
C      Year 2000 compliant.
C
C   Programmer:  Ann Schrader  9/98
C
C   USAGE (CALLING SEQUENCE):
C      icrdtg( idtg1, idtg2, incr );
C
C  VARIABLES AND THEIR USE
C    PASSED                                                    
C      idtg1 - Base DTG in the format YYMMDDHH.               
C      incr - Difference in hours.                             
C    RETURNED                                                  
C      idtg2 - New DTG, the sum of idtg1 and incr.  If one of    
C               the errors listed below occurrs, idtg2 will     
C               be returned asterisk-filled.                   
C                                                              
C      error possibilities -  0 = OK,
C                            -1 = invalid DTG,
C                            -2 = invalid increment,
C                            -3 = dtgyrhr returned a non-zero status.
C                                                              
C ..............MAINTENANCE SECTION............................
C                                                              
C   METHOD:                                                    
C                                                              
C   LANGUAGE:  C                                               
C                                                              
C   RECORD OF CHANGES:                                         
C
      implicit none

      character*8 idtg1, idtg2
      character*10 indtg, newdtg
      integer incr, istat
      integer iyr, imo, iday, ihour

      IYR    = 0

C     Do an internal read to get the year portion of the input idtg1
      read( idtg1, '(4i2)' ) iyr, imo, iday, ihour

C     Compose the 10 digit input dtg for the dtgmod call.
C     The ATCF database contains data as far back as 1945, any data
C     with a year of less than 45 must by default be century = 20.
C     This logic, of course, will not work past the year 2044.
      if( iyr .lt. 45 ) then
         indtg = '20'//idtg1
      else
         indtg = '19'//idtg1
      endif

C     Call dtgmod, the year 2000 compliant version of icrdtg
      call dtgmod( indtg, incr, newdtg, istat )

      if( istat .eq. 0 ) then
C        Get the newdtg without the first two digits of the year.
         idtg2 = newdtg(3:10)
      else
         idtg2 = '********'
      endif
         
      return
      end


      SUBROUTINE DTGMOD ( INDTG, IDIF, NEWDTG, ISTAT )
C
C.........START PROLOGUE.........................................
C
C  SUBPROGRAM NAME:  DTGMOD
C
C  DESCRIPTION:  Given base DTG and increment (+/- hours), return new DTG
C                 ( = indtg + idif ) and the status value.
C
C  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
C
C  CURRENT PROGRAMMER (UTIL*):   S. Glassner
C
C  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
C
C  LIBRARIES OF RESIDENCE(UTIL*):
C
C  CLASSIFICATION:  UNCLAS
C
C  USAGE (CALLING SEQUENCE):
C
C    CHARACTER*10 INDTG, NEWDTG
C
C    CALL DTGMOD ( INDTG, IDIF, NEWDTG, ISTAT )
C
C  INPUT PARAMETERS:
C
C    INDTG	C*10	Base DTG in the format YYYYMMDDHH.
C    IDIF       INT     Difference in hours 
C		         (-8775216 through +4294967295).
C
C  OUTPUT PARAMETERS:
C    
C    NEWDTG	C*10	New DTG, the sum of INDTG and IDIF. If one of
C			  the errors listed below occurrs, NEWDTG will
C			  be returned asterisk-filled.
C    ISTAT      INT     Error return.
C			  ISTAT =  0, ok.
C			  ISTAT = -1, Invalid DTG.
C			  ISTAT = -2, Invalid increment.
C			  ISTAT = -3, DTGYRHR returned a non-zero
C				      status.  
C
C  CALLS:
C
C    DTGNUM		Get integer year, month, day, hour, days of the
C			  year and hours of the year from DTG.
C    DTGYRHR		Get DTG from julian date.
C
C  EXAMPLE:
C 
C    CHARACTER*10 CURDTG, NEWDTG
C    INTEGER IDIF
C
C    DATA CURDTG / '1991030612' /`
C    DATA IDIF  / 2 /
C
C    CALL DTGMOD ( CURDTG, IDIF, NEWDTG, ISTAT )
C		...
C
C    NEWDTG will contain 1991030614. 
C
C  ERROR CONDITIONS:
C
C    Invalid DTG.   ISTAT=-1 at return. NEWDTG asterisk-filled.
C    Invalid IDIF.  ISTAT=-2 at return. NEWDTG asterisk-filled.
C    Non-zero status return from DTGYRHR. ISTAT=-3 at return.
C      NEWDTG asterisk-filled.
C        
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C    BADDTG     C*10    Value returned in NEWDTG if an error occured.
C    IDX        INT     Set to 1 if the input year is a leap
C			  year. Otherwise, it will be 2, 3 or 4 
C			  (the remainder resulting from the mod 
C			  function). Used as a subscript for the
C			  IHOURS array.
C    MONTH(12,2) INT	Number of days elapsed before getting to 
C          	          this month.  There are two sets
C			  of these, one for non-leap years (second
C			  subscript = 1), one for leap years (second
C			  subscript = 2).  For example, MONTH(3,2),
C			  for March in a leap year, contains 60, the
C			  the number of days elapsed before March.
C			  The idea is that if IDAYS comes up with
C			  64 days in a leap year, we know the month
C			  should be March because it is greater than
C			  60 and less than the end of March, 91.
C     IHOURS(4)	INT	Number of hours in the year.  IHOURS(1) contains  
C			  the number of hours in a leap year.
C     IYR	INT	Year, extracted from INDTG by DTGNUM.
C     IMO	INT	Month, extracted from INDTG by DTGNUM.
C     IDAY	INT	Day, extracted from INDTG by DTGNUM.
C     IHR	INT	Hour, extracted from INDTG by DTGNUM.
C     IDAOFYR	INT	Day of the year, extracted from INDTG by DTGNUM.
C     IHROFYR	INT	Hour of year, extracted from INDTG by DTGNUM.
C     JSTAT	INT	Error return from DTGNUM. If=0, ok.
C     KSTAT	INT	Error return from DTGYRHR. If=0, ok.
C
C  METHOD
C
C     1.  Call DTGNUM to get hours of the year.
C     2.  Add IDIF to the hours of the year.
C     3.  If the new sum is negative or has too many hours for one year,
C         adjust the sum and input year until the sum is positive and within  
C         one year.
C     4.  Call DTGYRHR, passing it the year and hours of the year, to get
C         the new DTG.
C
C  LANGUAGE (UTIL*):  FORTRAN 77
C
C  RECORD OF CHANGES:
C
C <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
C                   Change to DTGMOD required to port to Cray:
C                      from IMPLICIT UNDEFINED ( A-Z )
C                      to   IMPLICIT NONE
C
C.........END PROLOGUE..................................................
C
      IMPLICIT NONE
C
      CHARACTER*10 INDTG, NEWDTG, BADDTG
C
      INTEGER IHOURS(4), IYR, IMO, IDAY, IHR, IDAOFYR, IHROFYR, NEWHRS, 
     *        IDIF, IDX, I, ISTAT, JSTAT, KSTAT
C
      DATA IHOURS / 8784, 3*8760 /
      DATA BADDTG / '**********' /
C
      ISTAT = 0
C
      CALL DTGNUM ( INDTG, IYR, IMO, IDAY, IHR, IDAOFYR, IHROFYR, 
     *              JSTAT )
      IF ( JSTAT .NE. 0 ) THEN
	 ISTAT = -1
	 NEWDTG = BADDTG
         GO TO 5000
      END IF

****************************************************************************
*        Increment (decrement) hour of the year (IHROFYR) by IDIF,
*        test for change of year, adjust if necessary, reformat to NEWDTG.
****************************************************************************

      NEWHRS = IHROFYR + IDIF

****************************************************************************
*         See if NEWHRS is negative.  If it is,  perform a loop that
*		Subtracts 1 from the year,
*		Adds a year's worth of hours to NEWHRS, the number of hours
*		  depending on whether the year is a leap year or not.
*		Sees if NEWHRS is still negative.  Leave the loop when it
*		  becomes zero or positive.
****************************************************************************

      IF ( NEWHRS .LT. 0 ) THEN
C                  
	 DO 10 I=1,1000
            IYR = IYR - 1
C
	    IF ( MOD(IYR, 100) .EQ. 0 ) THEN
	       IF ( MOD(IYR, 400) .EQ. 0 ) THEN
		  IDX = 1
	       ELSE
		  IDX = 2
	       END IF	       
            ELSE
 	       IDX = MOD(IYR, 4) + 1
	    END IF
C
    	    NEWHRS = NEWHRS + IHOURS(IDX)
            IF ( NEWHRS .GE. 0 ) GO TO 30
   10    CONTINUE
C
         GO TO 900

****************************************************************************
*         NEWHRS is positive or 0.
*         Perform a loop until NEWHRS' value is less than or equal to the
*           number of hours in a year.   
*              See if there is more than one year's worth of hours in NEWHRS.  
*              If there is, perform a loop that
*	         Subtracts a year's worth of hours from NEWHRS, the number 
*		   of hours depending on whether the year is a leap year or
*                  not.
*	         Adds 1 to the year.
***************************************************************************

      ELSE    
         DO 20 I=1,1000       
	    IF ( MOD(IYR, 100) .EQ. 0 ) THEN
	       IF ( MOD(IYR, 400) .EQ. 0 ) THEN
		  IDX = 1
	       ELSE
		  IDX = 2
	       END IF
            ELSE
 	       IDX = MOD(IYR, 4) + 1
	    END IF
C
            IF ( NEWHRS .LT. IHOURS(IDX) ) GO TO 30
            NEWHRS = NEWHRS - IHOURS(IDX)    
            IYR = IYR + 1  
   20    CONTINUE
C
          GO TO 900
      END IF     

   30 CALL DTGYRHR ( IYR, NEWHRS, NEWDTG, KSTAT )
      IF ( KSTAT .NE. 0 ) THEN
         NEWDTG = BADDTG
	 ISTAT = -3
      END IF
C 
      GO TO 5000

**************************************************************************
*        Error.
**************************************************************************

  900 NEWDTG = BADDTG
      ISTAT = -2
C
 5000 RETURN  
      END


      SUBROUTINE DTGYRHR ( IYR, IHRS, NEWDTG, ISTAT )
C
C.........START PROLOGUE.........................................
C
C  SUBPROGRAM NAME:  DTGYRHR
C
C  DESCRIPTION:  Given a year and hours of the year, DTGYRHR returns
C                a DTG of format YYYYMMDDHH in NEWDTG.
C
C  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
C
C  CURRENT PROGRAMMER (UTIL*):  S. Glassner
C
C  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS 
C
C  LIBRARIES OF RESIDENCE(UTIL*):
C
C  CLASSIFICATION:  UNCLAS
C
C  USAGE (CALLING SEQUENCE):  CALL DTGYRHR ( IYR, IHRS, NEWDTG, ISTAT )
C
C  INPUT PARAMETERS:
C
C    IYR	INT     4-digit year, e.g. 1995.
C    IHRS	INT	Hours into the year.
C
C  OUTPUT PARAMETERS:
C
C    NEWDTG	C*10	DTG of form YYYYMMDDHH, e.g. 1995041606.
C    ISTAT	INT     Status.
C			  =  0 - OK.
C			  = -1 - Invalid year.
C			  = -2 - Invalid hour-of-the-year value.
C
C  CALLED BY:  DTGMOD
C
C  CALLS:      None.
C
C  EXAMPLE:
C
C    CHARACTER NEWDTG*10
C    INTEGER IYR, IHRS, ISTAT
C
C    DATA IYR / 1991 /, IHRS / 674 /
C
C    CALL DTGYRHR ( IYR, IHRS, NEWDTG, ISTAT )
C    IF ( ISTAT .NE. 0 ) THEN
C	Error...
C
C    NEWDTG will contain 1991012902      
C  
C  ERROR CONDITIONS:
C
C    Negative year passed. Set NEWDTG to all asterisks, set ISTAT
C     to -1, and return.
C    Invalid hours value passed. Set NEWDTG to all asterisks,
C     set hours to max hours for that year, and set ISTAT to -2.
C
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C    BADDTG	  C*10  Returned in NEWDTG if an error occurred.
C    MONTH(12,2)  INT	Number of days elapsed before getting to 
C          	          this month.  There are two sets
C			  of these, one for non-leap years (second
C			  subscript = 1), one for leap years (second
C			  subscript = 2).  For example, MONTH(3,2),
C			  for March in a leap year, contains 60, the
C			  the number of days elapsed before March.
C			  The idea is that if IDAYS comes up with
C			  64 days in a leap year, we know the month
C			  should be March because it is greater than
C			  60 and less than the end of March, 91.
C
C    IDAYS	  INT	Number, of hours (IHRS) converted into whole
C			  days.
C    IHOURS       INT   Number of hours into the next day.
C
C    IDX	  INT	Subscript for month.  
C			     =1, non-leap year
C			     =2, leap year
C
C    IMO	  INT   IMO is initially set to 0.  It is incremented 
C			  by 1 as the month loop runs.  When the correct
C			  month is found, IMO will be one less than I.
C			  If no correct month is found, IMO is set to 
C			  12 and in either case becomes the month in the
C			  new DTG.
C
C    MAXHRS(2)	  INT	Maximum number of hours in one year.  Used for
C			  checking hours argument.		
C
C  METHOD:
C
C    1.  Calculate the number of the day by dividing the hours by 24
C	 and adding 1 (IDAYS).
C    2.  Find what month the day is in by finding where the day
C	 falls in the month array (see description of MONTH, above).
C    3.  The month array has two sections, one for leap years and
C	 one for non-leap years.  Get the subscript for this dimension
C	 by taking mod 4 of the year.
C    4.  Calculate the days in the retrieved month by subtracting the
C        the number of days up to the month from the days of the year
C	 (IDAYS).
C    5.  Calculate the hours in the retrieved day by taking mod 24 of
C	 the hours input argument (IHRS).	     				
C    6.  Do an internal write of the given year and derived month,
C	 day and hour into the output DTG.
C
C  LANGUAGE (UTIL*):  FORTRAN 77
C
C  RECORD OF CHANGES:
C
C <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
C                   Changes to DTGYRHR required to port to Cray:
C                      from IMPLICIT UNDEFINED ( A-Z )
C                      to   IMPLICIT NONE
C
C.........END PROLOGUE..................................................
C 
      IMPLICIT NONE
C
      CHARACTER*10 NEWDTG, BADDTG
C
      INTEGER IYR, IHRS, ISTAT, IDX, MOD, IDAYS, IMO, I, IDAY, IHOURS
      INTEGER MONTH(12,2), MAXHRS(2)
C
      DATA MONTH/0,31,59,90,120,151,181,212,243,273,304,334,
     *           0,31,60,91,121,152,182,213,244,274,305,335/
      DATA MAXHRS / 8759, 8783 /
      DATA BADDTG / '**********' /
C     
      ISTAT = 0   

************************************************************************
*        Set the leap year index, IDX, to 1 if non-leap year and 2
*	   if leap year.
*	 If year is a century, it's a leap year if it's evenly divisible
*	   by 400. If it's not a century, it's a leap year if it's
*	   evenly divisible by 4.
************************************************************************

      IF ( MOD ( IYR, 100 ) . EQ. 0 ) THEN
	 IF ( MOD ( IYR, 400 ) .EQ. 0 ) THEN
	    IDX = 2
	 ELSE
	    IDX = 1
	 END IF

      ELSEIF (MOD ( IYR, 4 ) .EQ. 0 ) THEN
	 IDX = 2

      ELSE
	 IDX = 1
      END IF

***********************************************************************
*        Calculate number of whole days in IHRS.
*        Validate the hours argument.
***********************************************************************

      IF ( IYR .LT. 0 ) THEN
         ISTAT = -1
	 NEWDTG = BADDTG
         GO TO 5000
      END IF
C
      IF ( (IHRS .LT. 0) .OR. (IHRS .GT. MAXHRS(IDX)) ) THEN
         ISTAT = -2
         IHRS = MAXHRS(IDX)
 	 NEWDTG = BADDTG
      END IF
C
      IDAYS = IHRS/24+1

***********************************************************************
*        Find the proper month by determining where this number of
*        days falls in the MONTH array.
***********************************************************************
 
      IMO = 0
      DO 2 I = 2,12
         IMO = IMO + 1
         IF ( IDAYS .LE. MONTH(I,IDX) ) GO TO 3
 2    CONTINUE
      IMO = IMO + 1
C
 3    IHOURS = MOD(IHRS,24)
      IDAY = IDAYS - MONTH(IMO,IDX)
      WRITE(NEWDTG, '(I4.4, 3I2.2)') IYR, IMO, IDAY, IHOURS
C
 5000 RETURN
      END


      SUBROUTINE DTGNUM ( INDTG, IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS,
     *                    ISTAT )
C
C.........START PROLOGUE.........................................
C
C  SUBPROGRAM NAME:  DTGNUM 
C
C  DESCRIPTION:  Given a DTG (YYYYMMDDHH), return integer values for
C		 year, month, day, hour, days into the year, and hours
C		 into the year. 	  		
C
C  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
C
C  CURRENT PROGRAMMER (UTIL*):   S. Glassner
C  
C  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
C
C  LIBRARIES OF RESIDENCE(UTIL*):
C
C  CLASSIFICATION:  UNCLAS
C
C  USAGE (CALLING SEQUENCE):
C
C    CALL DTGNUM ( INDTG, IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS, ISTAT )
C
C  INPUT PARAMETERS:
C
C    INDTG	C*10	    Day-time group, yyyymmddhh
C
C  OUTPUT PARAMETERS:
C
C    IYR	INT	    Year.
C    IMO	INT	    Month.
C    IDAY	INT         Day.
C    IHOUR	INT	    Hour.
C    IYRDAY	INT	    Days of the year.
C    IYRHRS	INT	    Hours of the year.
C    ISTAT	INT	    Error status return, = 0, OK; = -1,
C			     bad input DTG.
C
C  CALLS:  
C
C    DTGCHK	A function that validates DTGs.  Output is a 10-character
C		  string that is blank if the DTG is valid.  It will
C		  contain asterisks corresponding to where the DTG is invalid
C		  otherwise.
C
C  ERRORS CONDITIONS:
C
C    Invalid DTG.  All integers will return with zero. ISTAT will be
C     set to -1.
C
C  EXAMPLE:
C
C    CHARACTER*10 INDTG
C
C    INTEGER IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS, ISTAT
C    DATA INDTG / '1990120312' /
C
C    CALL DTGNUM ( INDTG, IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS,
C   *              ISTAT )
C
C    		...
C
C    Values returned will be:
C	IYR     - 1990
C	IMO     -   12
C	IDAY    -   03
C	IHOUR	-   12
C	IYRDAY  -  337
C 	IYRHRS  - 8076
C       ISTAT   -    0
C
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C    DTGERR	 C*10	Holds error return from DTGCHK.
C    IDX         INT    Set to 1 if the input year is not a leap
C			  year. If it is a leap year, it will be
C			  set to 2. Used as a subscript for the
C			  month array.
C    MONTH(12,2) INT	Number of days elapsed before getting to 
C          	          this month.  There are two sets
C			  of these, one for non-leap years (second
C			  subscript = 1), one for leap years (second
C			  subscript = 2).  For example, MONTH(3,2),
C			  for March in a leap year, contains 60, the
C			  the number of days elapsed before March.
C			  The idea is that if IDAYS comes up with
C			  64 days in a leap year, we know the month
C			  should be March because it is greater than
C			  60 and less than the end of March, 91.
C
C  METHOD:
C
C    1.  Do an internal read of the input DTG to get year, month,
C	 day, hour.  Call DTGCHK to make sure it's valid first.
C	 If it is invalid, set ISTAT to 0 and return.
C    2.  Set leap year index, IDX.
C    3.  Calculate number of days in the year by adding IDAY to the
C	 number of days elapsed before the first of the month, IMO.
C	 Days elapsed is in the MONTH array.
C    4.  Calculate number of hours into the year by multiplying
C        whole days of the year (IYRDAY-1) by 24 hours and adding
C        IHOUR.     	     
C
C  LANGUAGE (UTIL*):  FORTRAN 77
C
C  RECORD OF CHANGES:
C
C <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
C                   Changes to DTGNUM required to port to Cray:
C                      from IMPLICIT UNDEFINED ( A-Z )
C                      to   IMPLICIT NONE
C
C.........END PROLOGUE..................................................

C
      IMPLICIT NONE

      CHARACTER*10 INDTG, DTGERR, DTGCHK
C
      INTEGER IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS, ISTAT, IDX, MOD
      INTEGER MONTH(12,2) 
C
      DATA MONTH /0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334,
     *            0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335
     *           /
C
      DATA DTGERR /'          '/
C
      IYR    = 0
      IMO    = 0
      IDAY   = 0
      IHOUR  = 0
      IYRDAY = 0
      IYRHRS = 0
      ISTAT  = 0

************************************************************************
*	 Call DTGCHK to validate the DTG.
*	 If it's ok, do an internal read of the integer date parts.
************************************************************************

      DTGERR = DTGCHK ( INDTG )
      IF ( DTGERR .NE. '          ' ) THEN
	 ISTAT = -1
         GO TO 5000
      ELSE   
         READ ( INDTG, '(I4, 3I2)' ) IYR, IMO, IDAY, IHOUR
      END IF

************************************************************************
*        Set the leap year index, IDX, to 1 if non-leap year and 2
*	   if leap year.
*	 If year is a century, it's a leap year if it's evenly divisible
*	   by 400. If it's not a century, it's a leap year if it's
*	   evenly divisible by 4.
************************************************************************

      IF ( MOD ( IYR, 100 ) . EQ. 0 ) THEN
	 IF ( MOD ( IYR, 400 ) .EQ. 0 ) THEN
	    IDX = 2
	 ELSE
	    IDX = 1
	 END IF

      ELSEIF (MOD ( IYR, 4 ) .EQ. 0 ) THEN
	 IDX = 2

      ELSE
	 IDX = 1
      END IF
C
      IYRDAY = MONTH(IMO,IDX) + IDAY
      IYRHRS = ( IYRDAY-1 ) * 24 + IHOUR
C
 5000 RETURN
      END


      FUNCTION DTGCHK ( DTG )
C
C.........START PROLOGUE.........................................
C
C  SUBPROGRAM NAME:  DTGCHK
C
C  DESCRIPTION:  Check to see if a DTG is valid.
C
C  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
C
C  CURRENT PROGRAMMER (UTIL*):  S. Glassner 
C  
C  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
C
C  LIBRARIES OF RESIDENCE(UTIL*):  
C
C  CLASSIFICATION:  UNCLAS
C
C  USAGE (CALLING SEQUENCE):  
C
C    CHARACTER*10 DTG, ERRDTG
C    ERRDTG = DTGCHK ( DTG )
C            or
C    CHARACTER*12 DTG, ERRDTG
C    ERRDTG = DTGCHK ( DTG ) 
C
C  INPUT PARAMETERS:
C
C    DTG           	  Date-time group in one of two formats:
C		C*10        YYYYMMDDHH
C		C*12	    YYYYMMDDHHmm
C
C  OUTPUT PARAMETERS:
C
C    ERRDTG	C*10/12   Error designator.  All blank if DTG was valid.	   
C			    Characters 1-4   = **** if year was bad.
C		            Characters 5-6   = **   if month was bad.
C			    Characters 7-8   = **   if day was bad.
C			    Characters 9-10  = **   if hour was bad.
C			    Characters 11-12 = **   if minutes were bad.
C
C  CALLS:
C
C    LEN	Returns the length of a string.
C
C  EXAMPLE:
C
C    1)  CHARACTER*10 DTGCHK, DTG, ERRDTG
C	 
C	 DATA DTG / '1995070312' /	
C
C        ERRDTG = DTGCHK ( DTG )
C	 IF ( ERRDTG .EQ. ' ' ) THEN
C	    No error...
C	 ELSE
C	    Error...
C	 END IF
C
C        ERRDTG will be blank.
C
C    2)  CHARACTER*12 DTGCHK, DTG, ERRDTG
C	 
C	 DATA DTG / '198607031245' /	
C
C        ERRDTG = DTGCHK ( DTG )
C	 IF ( ERRDTG .EQ. ' ' ) THEN
C	    No error...
C	 ELSE
C	    Error...
C	 END IF
C
C        ERRDTG will be blank.
C
C    3)  CHARACTER*12 DTGCHK, DTG, ERRDTG
C	 
C	 DATA DTG / '198677031290' /	
C
C        ERRDTG = DTGCHK ( DTG )
C	 IF ( ERRDTG .EQ. ' ' ) THEN
C	    No error...
C	 ELSE
C	    Error...
C	 END IF
C
C        ERRDTG will look like this, where a period denotes a blank:
C	   ....**....**	 meaning that the month and minutes are invalid.
C
C  RESTRICTIONS:
C
C    The DTG may not contain any blanks.
C    The DTG must be either 10 or 12 digits long.
C    DTGCHK rejects DTG years that are outside the range 1800-2799.  
C
C  ERROR CONDITIONS:  
C
C    Non-numeric DTG. DTGCHK will be asterisk-filled.
C
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C    IDAY	INT	Integer day from the input DTG.	
C    IHOUR	INT	Integer hour from the input DTG.
C    IMO	INT	Integer month from the input DTG.
C    IYR	INT	Integer year from the input DTG.
C    LENGTH	INT	Gets the return from LEN, i.e. the length
C			  of the argument string.
C    MIN	INT	Integer minutes from the input DTG.	
C    MONTH(12)	INT	An array containing the number of days in
C			  each month.
C
C  METHOD:
C
C    Check that each value is numeric and within the ranges:
C	Year:  1800-2799.
C	Month: 1-12.
C	Day:   1-31 for Jan, Mar, May, July, Aug, Oct and Dec.
C	       1-30 for Apr, June, Sep, Nov.
C	       28 or 29 for Feb, depending on whether year is a leap year.
C		  Note that 1900 is not a leap year and 2000 is.	
C	Hour:  0-23
C	Min:   0-59 	
C    Whenever a value is outside its range, set the corresponding characters
C	of DTGCHK to asterisks.		
C
C  LANGUAGE (UTIL*):  FORTRAN 77
C
C  RECORD OF CHANGES:
C
C <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
C                   Change to DTGCHK required to port to Cray:
C                      from IMPLICIT UNDEFINED ( A-Z )
C                      to   IMPLICIT NONE
C
C.........END PROLOGUE..................................................
C
      IMPLICIT NONE
C
      CHARACTER DTGCHK*(*), DTG*(*)
C
      INTEGER MONTH(12), IYR, IMO, IDAY, IHOUR, MIN, LENGTH, LEN, I
C
      DATA MONTH / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
C
      LENGTH = LEN ( DTG )
      IF ( LENGTH .NE. 10 .AND. LENGTH .NE. 12 ) THEN
	 DTGCHK = '************'
	 GO TO 5000
      END IF

****************************************************************************
*        Make sure every character in the DTG is a number.
****************************************************************************

      DO 10 I=1,LENGTH
	 IF ( DTG(I:I) .LT. '0' .OR. DTG(I:I) .GT. '9' ) THEN
C
	    IF (LENGTH .EQ. 10 ) THEN 
               DTGCHK = '**********'
	    ELSE
  	       DTGCHK = '************'
	    END IF
C
	    GO TO 5000
         END IF
   10 CONTINUE

****************************************************************************
*        MONTH(2) will be changed to 29 later if the year is a leap year.
****************************************************************************

      MONTH(2) = 28

****************************************************************************
*        Get the integer versions of the year, month, day, and hour.
****************************************************************************

      READ ( DTG, '(I4,3I2)' ) IYR, IMO, IDAY, IHOUR

****************************************************************************
*       Get minutes if they're there.
****************************************************************************

      DTGCHK(1:LENGTH) = ' '
C
      IF ( LENGTH .EQ. 12 ) THEN
	 IF ( (DTG(11:11) .GE. '0') .AND. (DTG(11:11) .LE. '9') .AND.
     *        (DTG(12:12) .GE. '0') .AND. (DTG(12:12) .LE. '9') ) THEN
  	    READ (DTG, '(10X, I2)' ) MIN
	 ELSE
	    MIN = 99
	 END IF
      END IF

****************************************************************************
*        Check out the DTG.
*          Year...
*        Allow iyr == 0 for util_julday(), ajs  09/2002
****************************************************************************

      IF ( ((IYR .LT. 1800) .OR. (IYR .GT. 2799)) .AND. IYR .NE. 0 ) 
     *   DTGCHK(1:4) = '****'

****************************************************************************
*          Month...
****************************************************************************

      IF ( (IMO .LT. 1) .OR. (IMO .GT. 12) ) DTGCHK(5:6) = '**'
         		
****************************************************************************
*          Day...
*	     Set February days to 29 if this is a leap year.
*
*	     If DTGCHK(1:6) (year/month) already has asterisks, it was 
*              invalid, so don't do the day check.  If the year was invalid,
*	       we can't see if it was a leap year.  If the month was 
*	       invalid, we can't see if it was February.
*	     See if this is a leap year, i.e. evenly divisible by four or,
*	       if it's a century (dtg(3:4) = '00'), evenly divisible
*	       by 400.	
****************************************************************************

      IF ( DTGCHK(1:6) .EQ. '  ' ) THEN
         IF ( (DTG(3:4) .NE. '00') .AND. (MOD(IYR, 4) .EQ. 0) .OR.
     *      ( (DTG(3:4) .EQ. '00') .AND. (MOD(IYR, 400) .EQ. 0 ) )  )
     * 	       MONTH(2) = 29
C
         IF ( (IDAY .LT. 1) .OR. (IDAY .GT. MONTH(IMO)) )
     *      DTGCHK(7:8) = '**' 	
      END IF

****************************************************************************
*          Hour...
****************************************************************************

      IF ( (IHOUR .LT. 0) .OR. (IHOUR .GT. 23) ) DTGCHK(9:10) = '**'

****************************************************************************
*	   Minutes...
****************************************************************************

      IF ( LENGTH .EQ. 12 ) THEN
	 IF ( (MIN .LT. 0) .OR. (MIN .GT. 59) ) DTGCHK(11:12) = '**'
      END IF
C
 5000 RETURN
      END


      subroutine dtgdif ( dtg1, dtg2, idif )
C
C
C   DESCRIPTION:
C      This is a wrapper for the dtgdif2 routine.
C      The purpose of the routine is to convert dtg1 and
C      dtg2 into 10 digit dtg's prior to calling dtgdif2.
C
C     DTGDIF2 description:
C      Given two DTGs, return the difference in hours (=dtg2 - dtg1).
C      Year 2000 compliant.
C
C   Programmer:  Ann Schrader  9/98
C
C   USAGE (CALLING SEQUENCE):
C      dtgdif( dtg1, dtg2, idif );
C
C  VARIABLES AND THEIR USE
C    PASSED                                                    
C      dtg1 - a dtg of format YYMMDDHH
C      dtg2 - a dtg of format YYMMDDHH
C    RETURNED                                                  
C      idif - difference in hours (dtg2 - dtg1)
C                                                              
C ..............MAINTENANCE SECTION............................
C                                                              
C   METHOD:                                                    
C                                                              
C   LANGUAGE:  C                                               
C                                                              
C   RECORD OF CHANGES:                                         
C
      implicit none

      character*8 dtg1, dtg2
      character*10 ndtg, mdtg
      integer idif, istat
      integer iyr, imo, iday, ihour

      IYR    = 0
C     Do an internal read to get the year portion of dtg1
      read( dtg1, '(4i2)' ) iyr, imo, iday, ihour

C     Compose the 10 digit, ndtg, for the dtgdif2 call.
C     The ATCF database contains data as far back as 1945, any data
C     with a year of less than 45 must by default be century = 20.
C     This logic, of course, will not work past the year 2044.
      if( iyr .lt. 45 ) then
         ndtg = '20'//dtg1
      else
         ndtg = '19'//dtg1
      endif

      IYR    = 0
C     Do an internal read to get the year portion of dtg2
      read( dtg2, '(4i2)' ) iyr, imo, iday, ihour

C     Compose the 10 digit, mdtg, for the dtgdif2 call.
      if( iyr .lt. 45 ) then
         mdtg = '20'//dtg2
      else
         mdtg = '19'//dtg2
      endif

C     Call dtgdif2, the year 2000 compliant version of dtgdif
      call dtgdif2( ndtg, mdtg, idif, istat )

C      if( istat .eq. 0 ) then
C      else
C      endif
         
      return
      end


      SUBROUTINE DTGDIF2 ( NDTG, MDTG, IHRS, ISTAT )
C
C.........START PROLOGUE.........................................
C
C  SUBPROGRAM NAME:  DTGDIF2
C
C  DESCRIPTION:
C
C     Given two DTGs, return the difference in hours (=MDTG-NDTG).
C
C  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
C
C  CURRENT PROGRAMMER (UTIL*):   S. Glassner 
C  
C  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
C
C  LIBRARIES OF RESIDENCE(UTIL*):  
C
C  CLASSIFICATION:  UNCLAS
C
C  USAGE (CALLING SEQUENCE):
C
C     CALL DTGDIF2 ( NDTG, MDTG, IHRS, ISTAT )
C
C  INPUT PARAMETERS:
C
C     NDTG	C*10	A DTG of format YYYYMMDDHH.
C     MDTG	C*10    A DTG of format YYYYMMDDHH.
C
C     Note that NDTG can be greater than MDTG. The result will be a
C     negative difference.
C
C  OUTPUT PARAMETERS:
C
C     IHRS	INT	Difference in hours (MDTG-NDTG)
C     ISTAT	INT	Status variable.
C			  ISTAT =  0, ok
C			  ISTAT = -1, invalid DTG.
C
C  CALLS:
C
C     DTGNUM		Get integer year, month, day, hour, hours
C			  into the year, days into the year from DTG.
C
C  EXAMPLE:
C
C     CHARACTER*10 CUTDTG, REPDTG
C
C     INTEGER IDIFF, IHRS, ISTAT
C
C     DATA CUTDTG / '1991022800' /
C     DATA REPDTG / '1991030112' /
C
C     CALL DTGDIF2 ( CUTDTG, REPDTG, IHRS, ISTAT )
C     IF ( ISTAT .NE. 0 ) THEN
C	 Error...
C     ELSE
C	 Okay...
C     END IF
C	 	 :
C                :
C     		 
C     The resulting value returned in IDIFF will be 36.
C     For the call CALL DTGDIF2 ( REPDTG, CUTDTG, IHRS, ISTAT ),
C	the value returned will be -36.
C
C  RESTRICTIONS:
C
C     DTGDIF2 handles DTGs in the range 1800 through 2799.
C
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C     IDIFF		The difference between the two DTG's years.
C     IDUM		A dummy variable to use for the call to
C			  DTGNUM.  We don't need most of the
C			  values returned by DTGNUM.
C     IHOURS(4)		Total hours in the year.  IHOURS(1) is for
C			  leap years, and the other three are for
C			  non-leap years.
C     IHRYRA/B		Number of hours in each DTG's year. They start 
C		  	  out containing the values of MDTG and NDTG,
C			  respectively, but the values may be swapped
C			  later on.	
C     ITEMP		Temporary variable used for swapping the 
C			  IYEARA/B and IHRYRA/B values.     
C     IYEARA/B		Integer year for each DTG. They start out
C		  	  containing the values of MDTG and NDTG,
C			  respectively, but the values may be swapped
C			  later on.
C     JSTAT	  	Error return variable from DTGNUM. If JSTAT =
C			  =0, ok.
C     LEAPCT		Counts the number of leap years between IYEARA
C			  and IYEARB.
C     LEAPYR		A logical variable that is true if the year is
C			  a leap year.		
C     SWAP        	A logical variable that is .TRUE. if the
C			  values of the IHRYR and IYEAR variables
C			  are swapped. 
C			
C  METHOD:
C
C     1.  Call DTGNUM to get the integer years and hours into the
C	  year for both input DTGs.
C     2.  If the two years are different:
C	  a.  If NDTG < MDTG
C	      1)  Set swap flag on
C	      2)  Put MDTG's value in NDTG and vice versa.  			
C         b.  END IF
C         c.  Figure out whether this is a leap year or not and
C	      set IDX, the leap year subscript.
C	  d.  Add the remaining hours of MDTG to the hours of NDTG
C             and put the result in IHRS.
C	  e.  If there is more than one year between the two DTGs,
C	      add in those years' hours.		
C	  f.  If the two values were swapped, change the sign of
C	      IHRS.
C     3.  If the two years are the same, use the difference between
C	  their hours-of-the-year.
C
C  LANGUAGE (UTIL*):  FORTRAN 77
C
C  RECORD OF CHANGES:
C
C <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
C                   Changes to DTGNUM required to port to Cray:
C                   1) from IMPLICIT UNDEFINED ( A-Z )
C                      to   IMPLICIT NONE
C                   2) initialize ISTAT = 0
cx                  sampson, nrl 5/25/98   Change name to dtgdif2 for use in ATCF
C
C.........END PROLOGUE..................................................
C
      IMPLICIT NONE
C
      CHARACTER*10 NDTG, MDTG      
C
      INTEGER IDIFF, IHOURS(4), IHRYRA, IHRYRB, IYEARA, 
     *        IYEARB, LEAPCT, ITEMP, IDX, IREM, I, J, K, MOD,
     *        IDUM1, IDUM2, IDUM3, IDUM4, IHRS, ISTAT, JSTAT
C
      LOGICAL SWAP, LEAPYR
C
      DATA IHOURS / 8784, 3*8760 /
C
      ISTAT = 0
      SWAP = .FALSE.
C
      CALL DTGNUM ( MDTG, IYEARA, IDUM1, IDUM2, IDUM3, IDUM4, IHRYRA,
     *              JSTAT )
      IF ( JSTAT .NE. 0 ) THEN
	 ISTAT = -1
         GO TO 5000
      END IF
C 
      CALL DTGNUM ( NDTG, IYEARB, IDUM1, IDUM2, IDUM3, IDUM4, IHRYRB,
     *              JSTAT ) 
      IF ( JSTAT .NE. 0 ) THEN
	 ISTAT = -1
         GO TO 5000
      END IF
C
      IF ( IYEARA .NE. IYEARB ) THEN
         IF ( IYEARA .LT. IYEARB ) THEN

**********************************************************************
*        The years are unequal.
*        Year A is less than year B.  Swap values so year A is
*          greater than year B.  Then use the "greater than"
*          section to do the calculation and change the sign of
*          the output hours.
**********************************************************************

            SWAP = .TRUE.
            ITEMP  = IYEARA
	    IYEARA = IYEARB
  	    IYEARB = ITEMP
c	 
            ITEMP  = IHRYRA
    	    IHRYRA = IHRYRB
	    IHRYRB = ITEMP
         END IF

**********************************************************************
*		Section that calulates the difference for
*		  years that are not the same.
*		Set leap year subscript. (If a century is not a leap
*		  year, we set it unequal to 1, arbitrarily choosing
*		  2.)  
* 		Get the hours remaining in year B by subtracting
*		  its hours-into-the-year from the total hours 
*		  in the year.
*	        Add them to the other year's hours, then change 
*	          the sign of the result if necessary.    
**********************************************************************

	 IDX = MOD (IYEARB, 4) + 1

	 IF ( MOD ( IYEARB, 100 ) . EQ. 0 ) THEN
	    IF ( MOD ( IYEARB, 400 ) .NE. 0 ) THEN
	       IDX = 2
 	    END IF
   	 ELSEIF (MOD ( IYEARB, 4 ) .EQ. 0 ) THEN
	    IDX = 1
	 END IF
C
         IREM = IHOURS(IDX) - IHRYRB
         IHRS = IREM + IHRYRA

**********************************************************************
*           The above logic will handle dates from YY010100 through
*	      123123 of the next year.  The next section handles cases
*             where the time difference is greater than that.
**********************************************************************

         IDIFF = IYEARA - IYEARB
         IF ( IDIFF .GT. 1 ) THEN
	    
**********************************************************************
*	    Count the leap years in the set of years between the two
*	    given years.
**********************************************************************
            
            LEAPCT = 0
	    J = IYEARB
	    K = IYEARA
C 		
	    DO 10 I=J+1,K-1
               LEAPYR = .FALSE.
	       IF ( MOD ( I, 100 ) . EQ. 0 ) THEN
	          IF ( MOD ( I, 400 ) .EQ. 0 ) THEN
	             LEAPYR = .TRUE.
 	          END IF
	       ELSEIF (MOD ( I, 4 ) .EQ. 0 ) THEN
	          LEAPYR = .TRUE.
	       END IF
C
	       IF ( LEAPYR ) LEAPCT = LEAPCT + 1
   10 	    CONTINUE

**********************************************************************
*	    Add the additional years' hours to the total hours (IHRS).
*	    First, add leap hours in one year (IHOURS(1)) multiplied
*	      by the number of leap years (LEAPCT).
*	    Then, add non-leap hours in one year (IHOURS(2)) multiplied
*	      by the number of years between the given years 
*	      (IDIFF-1) less the number of leap years (LEAPCT). 	
**********************************************************************
            
            IHRS = IHRS + (IHOURS(1) * LEAPCT)
	    IHRS = IHRS + (IHOURS(2) * (IDIFF-1 - LEAPCT))
	 END IF
C		  	  	   
         IF ( SWAP ) IHRS = -IHRS

      ELSE
**********************************************************************
*		Section that calculates the difference when the
*		two years are the same.
**********************************************************************

         IHRS = IHRYRA - IHRYRB
      END IF
C
 5000 RETURN
      END      
