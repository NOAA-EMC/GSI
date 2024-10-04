! 12-10-2012   J.Jin    assign scans to different 6-hr periods (max=2).
!                       so that scans can be saved in different files
!                       output format: %y4%m2%d2_t%h2z            
! 10/10/2014  Yelena    Add 'need_date,need_syn'  as Input --> to write
!                       Bufr  only for needed date,synoptic
!                       Add output  tak(2)
!                       tak(i) =  0 , do not take file "i" 
!                       tak(i) =  1 ,  take file "i"

!=======================================================================
      subroutine ymd_thhz(nscan, iScan_x1, iScan_x2, &
                         year, mon, day, hour, &
                      need_date,need_syn, &
                   ymdthrz, id_oa, id_ob,tak)
  implicit none
  INTEGER,   INTENT(IN)      :: year(nscan)
  INTEGER,   INTENT(IN)      :: mon(nscan), day(nscan),      &
                                  hour(nscan)
  integer, INTENT(IN)          :: nScan 
  integer, INTENT(IN)          :: iScan_x1,iScan_x2
  integer, INTENT(IN)          :: need_date,need_syn
  integer                      :: iof, iScan_x1b, n,ii
  character(len=13),INTENT(out):: ymdthrz(2)
  integer, INTENT(OUT)         :: id_oa(2), id_ob(2)
  character(len=8)             ::  want_date

  character(len=13)            :: need_yymmddhr, want_yymmddhr

  character(len=13)            :: yymmddhr, yymmddhr0
  integer                      :: mm,dd,mm_day(12),mm_dayb(12),hr
  integer                      :: yy,ntak(2)
  integer, INTENT(OUT)         :: tak(2)
  data mm_day  /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
  data mm_dayb /31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

        tak(1) = 0
        tak(2) = 0
        ntak(1) = 0
        ntak(2) = 0

        write(want_date,'(i8.4)') need_date


       write(need_yymmddhr,'(i8.4,''.t'',i2.2,''z'')') &
               need_date,need_syn

        write(want_yymmddhr,'(A8,''.t'',i2.2,''z'')') &
               want_date,need_syn


  ! the first  ymdthrz
       iof = 1
       n = iScan_x1
         yy = year(n)
         mm = mon(n)
         dd = day(n)

         if ( hour(n) < 3 ) then
             hr = 0
         elseif ( hour(n) < 9 ) then
             hr = 6
         elseif ( hour(n) < 15 ) then
             hr = 12
         elseif ( hour(n) < 21 ) then
             hr = 18
         else
             hr = 0
             dd = dd + 1
           if ( mod(yy,4) /= 0 ) then 
             if ( dd > mm_day(mm) ) then
               dd = 1
               mm = mm +1
               if (mm > 12) then
                 mm = 1
                 yy = yy + 1
               endif
             endif
           else
             if ( dd > mm_dayb(mm) ) then
               dd = 1
               mm = mm +1
               if (mm > 12) then
                 mm = 1
                 yy = yy + 1
               endif
             endif
           endif
         endif

         write(yymmddhr0,'(i4.4,i2.2,i2.2,''.t'',i2.2,''z'')') &
               yy,mm,dd,hr 
       
           ymdthrz(iof) =  yymmddhr0
           id_oa(iof) = iScan_x1
           id_ob(iof) = iScan_x2
           id_oa(2) = -999
           id_ob(2) = -999

          

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          iScan_x1b = iScan_x1+1
       do n = iScan_x1b, iScan_x2
         yy = year(n)
         mm = mon(n)
         dd = day(n)
         if ( hour(n) < 3 ) then
             hr = 0
         elseif ( hour(n) < 9 ) then
             hr = 6
         elseif ( hour(n) < 15 ) then
             hr = 12
         elseif ( hour(n) < 21 ) then
             hr = 18
         else
             hr = 0
             dd = dd + 1
           if ( mod(yy,4) /= 0 ) then 
             if ( dd > mm_day(mm) )then
               dd = 1
               mm = mm +1
               if (mm > 12) then
                 mm = 1
                 yy = yy + 1
               endif
             endif
           else
             if ( dd > mm_dayb(mm) ) then
               dd = 1
               mm = mm +1
               if (mm > 12) then
                 mm = 1
                 yy = yy + 1
               endif
             endif
           endif

         endif

         write(yymmddhr,'(i4.4,i2.2,i2.2,''.t'',i2.2,''z'')') &
               yy,mm,dd,hr
         if (yymmddhr .ne. yymmddhr0 ) then
            yymmddhr0 = yymmddhr
            iof = iof + 1
            if ( iof > 2 ) then
               print *, 'The obit expands over 6 hours! check the HDF file'
               print *, 'Stop at ymd_thhz.f90'
               stop
            endif
            ymdthrz(iof) =  yymmddhr
            id_ob(iof-1) = n-1
            id_oa(iof) = n
         endif
       enddo
            id_ob(iof) = iScan_x2
            ! no 2nd file
            if ( id_oa(iof) < 0 ) id_ob(iof) = id_oa(iof) - 1 


           do ii = 1,2
        if (ymdthrz(ii) .eq. want_yymmddhr) tak(ii) = 1
           enddo

           do ii = 1,2
        if (ymdthrz(ii) .eq. need_yymmddhr) ntak(ii) = 1
           enddo

            print *,' VNUTRI  tak = ', tak

            print *,' VNUTRI  ntak = ', ntak

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      end subroutine ymd_thhz
