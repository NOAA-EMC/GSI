!*****************************************************************************
! Utilities for time
!
! Created by Y.Tahara          Aug 2000
!*****************************************************************************


module utilities_time

  implicit none

  private
  public Retrieve_NearTime
  public Create_TextTime
  public Convert_DateTime_TotalMin
  public Convert_TotalMin_DateTime
  public Convert_TotalDay_Date

  interface Retrieve_NearTime
    module procedure Retrieve_NearTime_TotalMin
    module procedure Retrieve_NearTime_DateTime
  end interface


  contains
  !========================================================================
  ! Create text of time for GrADS as [hh:mmZddmmmyyyy].
  !========================================================================
  subroutine Create_TextTime( ctime, datetime )

    !--- Interface ---
    character(len=15),intent(out) :: ctime
    integer(4),intent(in)         :: datetime(5)    ! year,month,day,hour,min

    !--- Work ---
    character(len=3),parameter :: cmonth(12) = &
                    (/ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', &
                       'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /)

    !--- Create ---
    write(ctime,'(I2.2,":",I2.2,"Z",I2.2,A,I4.4)') &
                    datetime(4), datetime(5), datetime(3), cmonth(datetime(2)), datetime(1)

  end subroutine Create_TextTime



  !========================================================================
  ! Retrieve time nearest to specified interval (<1day).
  !
  ! Ex)  Time interval 6hr.    2000/08/01 02:11  -->  2000/08/01 00:00
  !                                       13:35  -->             12:00
  !                                       22:35  -->  2000/08/02 00:00
  !========================================================================
  !--- Total min from 1801 ------------------------------------------------
  subroutine Retrieve_NearTime_TotalMin( totalmin, interval )

    !--- Interface ---
    integer(4),intent(inout) :: totalmin   ! total minitues from 1801
    integer(4),intent(in)    :: interval   ! interval (min)

    !--- Retrieve ---
    totalmin = int((totalmin + interval/2)/interval) * interval

  end subroutine Retrieve_NearTime_TotalMin


  !--- Date/Time ----------------------------------------------------------
  subroutine Retrieve_NearTime_DateTime( datetime, interval )

    !--- Interface ---
    integer(4),intent(inout) :: datetime(5) ! year,month,day,hour,min
    integer(4),intent(in)    :: interval    ! interval (min)

    !--- Work ---
    integer(4) :: totalmin

    !--- Retrieve ---
    call Convert_DateTime_TotalMin( totalmin, datetime )
    totalmin = int((totalmin + interval/2)/interval) * interval
    call Convert_TotalMin_DateTime( datetime, totalmin )

  end subroutine Retrieve_NearTime_DateTime



  !========================================================================
  ! Convert date/time to total minitues from 1801.
  !========================================================================
  subroutine Convert_DateTime_TotalMin( totalmin, datetime )

    !--- Interface ---
    integer(4),intent(in)  :: datetime(5)   ! year,month,day,hour,min
    integer(4),intent(out) :: totalmin      ! total minites from 1801

    !--- Parameter ---
    integer(4),parameter :: min_day  = 24*60, &
                            min_year = min_day * 365
    integer(4),parameter :: min_monthtop(12) = &
                        (/      0,  44640,  84960, &
                           129600, 172800, 217440, &
                           260640, 305280, 349920, &
                           393120, 437760, 480960 /)

    !--- Work ---
    integer(4) :: year, mon, uru_flag


    !--- Check Uruu ---
    year = datetime(1)
    if( (mod(year,4) == 0 .and. mod(year,100) /= 0)  .or.  mod(year,400) == 0 )then
       uru_flag = 1
    else
       uru_flag = 0
    endif

    !--- Convert ---
    year = year - 1801
    totalmin =   min_year * year &
               + min_day  * (year/4 - year/100 + (year+200)/400)

    mon = datetime(2)
    if( mon > 2 )then
      totalmin = totalmin + min_monthtop(mon) + uru_flag * min_day
    else
      totalmin = totalmin + min_monthtop(mon)
    endif

    totalmin = totalmin + (datetime(3) - 1) * min_day &
                        +  datetime(4) * 60 &
                        +  datetime(5)

  end subroutine Convert_DateTime_TotalMin



  !========================================================================
  ! Convert total minitues from 1801 to date/time.
  !========================================================================
  subroutine Convert_TotalMin_DateTime( datetime, totalmin )

    !--- Interface ---
    integer(4),intent(in)  :: totalmin      ! total minites from 1801
    integer(4),intent(out) :: datetime(5)   ! year,month,day,hour,min

    !--- Parameter ---
    integer(4),parameter :: min_day     = 24*60, &
                            min_1year   = min_day * 365, &
                            min_4year   = min_1year   *  4 + min_day, &
                            min_100year = min_4year   * 25 - min_day, &
                            min_400year = min_100year *  4 + min_day
    integer(4),parameter :: min_monthtop(12) = &
                        (/      0,  44640,  84960, &
                           129600, 172800, 217440, &
                           260640, 305280, 349920, &
                           393120, 437760, 480960 /)

    !--- Work ---
    integer(4) :: ttl
    integer(4) :: year, uru
    integer(4) :: dmy
    integer(4) :: i, j

    !--- Year ---
    ttl = totalmin + min_100year*2    ! total min from 1601

    dmy  = ttl / min_400year
    year =       dmy * 400 + 1601
    ttl  = ttl - dmy * min_400year

    dmy  = ttl / min_100year
    if( dmy > 3 )  dmy = 3
    year = year + dmy * 100
    ttl  = ttl  - dmy * min_100year

    dmy  = ttl / min_4year
    year = year + dmy * 4
    ttl  = ttl  - dmy * min_4year

    dmy  = ttl / min_1year
    if( dmy > 3 )  dmy = 3
    year = year + dmy
    ttl  = ttl  - dmy * min_1year

    datetime(1) = year

    !--- Month ---
    if( (mod(year,4) == 0  .and.  mod(year,100) /= 0)  .or.  mod(year,400) == 0 )then
      uru = min_day
    else
      uru = 0
    endif

    CheckMon: do j=1, 1
      do i=12, 3, -1
        dmy = ttl - min_monthtop(i) - uru
        if( dmy >= 0 )then
          datetime(2) = i
          ttl = dmy
          exit CheckMon
        endif
      enddo

      dmy = ttl - min_monthtop(2)
      if( dmy >= 0 )then
        datetime(2) = 2
        ttl = dmy
      else
        datetime(2) = 1
      endif
    enddo CheckMon

    !--- Day ---
    dmy = ttl / min_day
    datetime(3) = dmy + 1
    ttl = ttl - dmy * min_day

    !--- Hour, Min ---
    dmy = ttl / 60
    datetime(4) = dmy
    datetime(5) = ttl - dmy * 60

  end subroutine Convert_TotalMin_DateTime



  !========================================================================
  ! Convert total days from Jan.1 to date
  !========================================================================
  subroutine Convert_TotalDay_Date( month, day, year, totalday )

    !--- Interface ---
    integer(4),intent(out) :: month, day   ! month, day
    integer(4),intent(in)  :: year         ! year
    integer(4),intent(in)  :: totalday     ! total days from Jan.1

    !--- Work ---
    integer(4),save :: days_mon(12) = &
                    (/ 31,28,31,30,31,30, 31,31,30,31,30,31 /)

    !--- Uruu ---
    if( (mod(year,4) == 0  .and.  mod(year,100) /= 0)  .or.  mod(year,400) == 0 )then
      days_mon(2) = 29
    else
      days_mon(2) = 28
    endif

    !--- Convert ---
    day = totalday

    do month=1, 12
      if( (day - days_mon(month)) < 1 ) &
        exit

      day = day - days_mon(month)
    enddo

  end subroutine Convert_TotalDay_Date

end module utilities_time
