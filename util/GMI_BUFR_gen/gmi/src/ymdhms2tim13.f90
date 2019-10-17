    !subroutine ymdhms2tim13(tim13,yy,mo,dd,hh,mm,ss)
    subroutine ymdhms2tim13(tim13, ymdhms)
!
! Derive seconds from from 00Z00 Jan 1, 2013.

! input:
!      yy,mo,dd,hh,mm,ss
! output:
!      tim13,  seconds from 00Z00 Jan 1, 2013
!
! 2014-03-25  j.jin   debut

  implicit none
  REAl*8, intent(in)           ::  ymdhms(6)
  integer*8, intent(out)       ::  tim13
  integer*4                    ::  yy,mo,dd,hh,mm,ss
  integer*8                    ::  yrss,ddss,days
  integer*4                    ::  mlen0(12),mlen(12)
  integer*4                    ::  xyr,im
  data  mlen0/0, 31,28,31,30,31,30, &
             31,31,30,31,30/
  yy=int(ymdhms(1))  
  mo=int(ymdhms(2))  
  dd=int(ymdhms(3))  
  hh=int(ymdhms(4))  
  mm=int(ymdhms(5))  
  ss=int(ymdhms(6))  

  mlen = mlen0
  if( mod(yy,4) == 0) mlen(3) = mlen(3) + 1
  yrss = 365*24*3600
  ddss = 24*3600

  days =0
  do im = 1,mo 
    days = days + mlen(im) 
  enddo
  days = days + dd - 1
  xyr = (yy-2013)/4
  tim13 = days * ddss +  (yy-2013)*yrss +  xyr*ddss &
         + hh*3600 + mm*60 + ss

  end subroutine ymdhms2tim13
