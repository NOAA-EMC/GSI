SUBROUTINE AddOneDay(Year,Month,Day)

!  Simple code to increment the date by one day.  
!  It is assumed that a valid date is input.

IMPLICIT NONE

!Subroutine Arguments
INTEGER, INTENT(INOUT) :: Year, Month, Day

! Local Variable
LOGICAL :: LeapYear

! History
! 14th June 2010  Original version   A.Collard
!

Day = Day + 1

SELECT CASE (Month)
CASE (1, 3, 5, 7, 8, 10, 12)
  IF (Day == 32) THEN
     Day = 1
     Month = Month + 1
  END IF
CASE (4, 6, 9, 11)  
  IF (Day == 31) THEN
     Day = 1
     Month = Month + 1
  END IF
CASE (2)
  IF (Day >= 29) THEN
    LeapYear = ( (Year/4)*4 == Year .AND. &
       ((Year/100)*100 /= Year .OR. (Year/400)*400 == Year))
    IF ((LeapYear .AND. Day == 30) .OR. (.NOT.(LeapYear) .AND. Day == 29)) THEN
       Day = 1
       Month = Month + 1
    END IF
  END IF
END SELECT

IF (Month == 13) THEN
   Month = 1
   Year = Year + 1
END IF

END SUBROUTINE AddOneDay
