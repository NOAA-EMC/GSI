program gatime
!************************************************************************
!
!  time.f90
!
!  Extract necessary data for time plots from *.ieee_d files (ges and anl) 
!  and export to an ascii data file for use by javascript.
!
!  In addition to the namelist, a 3 other files -- times.txt, use.txt,
!  and chan.txt must be available.  
!    times.txt --> must contain the 10 digit date(s) to be processed, 
!                  one time per line and comma separated. 
!    chan.txt  --> must contain the actual channel number for each channel,
!                  the use flag, the wavelength, and frequency, in CSV format, 
!                  one entry for a given channel per line.
!************************************************************************

!   use IFPORT
! Intel® Fortran includes functions and subroutines that ease porting of code to or from a PC, or allow you to write code on a PC that is compatible with other platforms.  The portability library is called LIBIFPORT.LIB (Windows*) or libifport.a (Linux* and macOS*). Frequently used functions are included in a portability module called IFPORT.
! What functionality from IFPORT library is being used here and why?
 
   implicit none

   character(5)  str_chan
   character(10) pdate,ndate
   character(20) stringd,str_nchanl,str_ncycle
   character(60) data_file, sat_chan_file, sat_out_file
   character(200) outstr

   character(60) cnt_out_file, pen_out_file, omgnbc_out_file, totcor_out_file
   character(60) omgbc_out_file

   character(len=10),allocatable,dimension(:)::times
   character(len=2), allocatable,dimension(:)::useflg
   character(len=5), allocatable,dimension(:)::chan_nums
  
   integer luname,ldname,lpname,lsatchan,lsatout
   integer cyc,ii,jj,iflag,j,k,res,chan,ftyp,open_status
   integer max_region

   logical exist

   real rmiss
   real chan_cnt, chan_tot, chan_omgbc, chan_omgbc2
   real tot_pen, tot_cnt

   real,allocatable,dimension(:,:,:,:):: cnt,pen,avg_pen
   real,allocatable,dimension(:,:,:,:):: tot_cor,omg_nbc,omg_bc
   real,allocatable,dimension(:,:,:,:):: tot_cor2,omg_nbc2,omg_bc2
   real,allocatable,dimension(:,:,:,:):: avg_biascr,sdv_biascr
   real,allocatable,dimension(:,:,:,:):: avg_omgnbc,sdv_omgnbc
   real,allocatable,dimension(:,:,:,:):: avg_omgbc, sdv_omgbc

   real,allocatable,dimension(:)      :: wave, freq
   real,allocatable,dimension(:,:,:)  :: chi

!************************************************************************
!  Namelist with defaults
!************************************************************************
   integer               :: nchanl               = 19
   integer               :: ncycle               = 1
   character(15)         :: satname              = 'ssmis_f18'  
   integer               :: nregion              = 5
   namelist /input/ satname, nchanl, ncycle, nregion


   data luname,ldname,lpname,lsatchan / 5, 50, 51, 52 /
   data max_region / 5 /
   data rmiss /-999./

!************************************************************************
! Read namelist input
!************************************************************************
  read(luname,input)
  write(6,input)


!************************************************************************
! Read times.txt input file, which is the dates to process in order
! from most recent to oldest.
!************************************************************************
   allocate( times (ncycle) )

   open( lpname, file='times.txt' )
    
   do ii=1,ncycle
      read(lpname, *) times(ii)
   end do
   close(lpname)


!************************************************************************
! Read chan.txt input file, which is the actual channel number for each 
! channel.
!************************************************************************
   allocate( chan_nums (nchanl) )
   allocate( useflg(nchanl), wave(nchanl), freq(nchanl) )
!   allocate( chi(2, nchanl, nregion) )
   allocate( chi(2, nchanl, max_region) )

   open( lpname, file='chan.txt' )

   do ii=1,nchanl
      read(lpname, *) chan_nums(ii), useflg(ii), wave(ii), freq(ii)
      do jj=1,max_region
         chi(1,ii,jj) = 0.00
      end do
   end do
   close(lpname)

!**************************************************
!  Allocate space for variables
!    note:  first 2 is for ges|anl, 
!           second 2 is for value, value**2
!**************************************************
   allocate ( cnt     (2,ncycle,nchanl,nregion) )
   allocate ( pen     (2,ncycle,nchanl,nregion) )
   allocate ( tot_cor (2,ncycle,nchanl,nregion) )
   allocate ( omg_nbc (2,ncycle,nchanl,nregion) )
   allocate ( omg_bc  (2,ncycle,nchanl,nregion) )
   allocate ( tot_cor2(2,ncycle,nchanl,nregion) )
   allocate ( omg_nbc2(2,ncycle,nchanl,nregion) )
   allocate ( omg_bc2 (2,ncycle,nchanl,nregion) )
   allocate ( avg_pen (2,ncycle,nchanl,nregion) )

   allocate ( avg_biascr(2,ncycle,nchanl,nregion) )  
   allocate ( sdv_biascr(2,ncycle,nchanl,nregion) )  
   allocate ( avg_omgnbc(2,ncycle,nchanl,nregion) )  
   allocate ( sdv_omgnbc(2,ncycle,nchanl,nregion) )  
   allocate ( avg_omgbc (2,ncycle,nchanl,nregion) )
   allocate ( sdv_omgbc (2,ncycle,nchanl,nregion) )


!**************************************************
!  Loop over ftyp (ges|anl), cyc, chan, region and
!    read data from ges and anl files.
!
!  NOTE:  omg_bc/nbc are used for both ges and anl
!         as a programming simplicity; the values 
!         are really omabc|omanbc for anl files.
!**************************************************
   do ftyp=1,2
      do cyc=1,ncycle
         if ( ftyp == 1 ) then
            data_file= trim(satname) // '.' // trim(times(cyc)) // '.ieee_d'
         else
            data_file= trim(satname) // '_anl.' // trim(times(cyc)) // '.ieee_d'
         end if

         inquire(file=data_file, exist=exist) 

         if ( exist .eqv. .TRUE. ) then
            open(ldname,file=data_file,form='unformatted')
            read(ldname) ((cnt(ftyp,cyc,j,k),j=1,nchanl),k=1,nregion)
            read(ldname) ((pen(ftyp,cyc,j,k),j=1,nchanl),k=1,nregion)
            read(ldname) ((omg_nbc (ftyp,cyc,j,k),j=1,nchanl),k=1,nregion)
            read(ldname) ((tot_cor (ftyp,cyc,j,k),j=1,nchanl),k=1,nregion)
            read(ldname) ((omg_bc  (ftyp,cyc,j,k),j=1,nchanl),k=1,nregion)
            read(ldname) ((omg_nbc2(ftyp,cyc,j,k),j=1,nchanl),k=1,nregion)
            read(ldname) ((tot_cor2(ftyp,cyc,j,k),j=1,nchanl),k=1,nregion)
            read(ldname) ((omg_bc2 (ftyp,cyc,j,k),j=1,nchanl),k=1,nregion)
            close(ldname)
         else
            do j=1,nchanl
               do k=1,nregion
                  cnt(ftyp,cyc,j,k)      = 0.0
                  pen(ftyp,cyc,j,k)      = 0.0  
                  omg_nbc (ftyp,cyc,j,k) = 0.0  
                  tot_cor (ftyp,cyc,j,k) = 0.0
                  omg_bc  (ftyp,cyc,j,k) = 0.0   
                  omg_nbc2(ftyp,cyc,j,k) = 0.0   
                  tot_cor2(ftyp,cyc,j,k) = 0.0
                  omg_bc2 (ftyp,cyc,j,k) = 0.0  
               end do
            end do 
         end if

      end do       ! loop over cycles (times)
   end do       ! loop over ftyp (ges|anl)

   write(6,*) 'finished reading data'

! *****************************************************************************
!  Calculate chi values
!     (pen/cnt)/cnt for each channel by region
! *****************************************************************************
   do ftyp=1,2
      do j=1,nchanl
         do k=1,nregion
             tot_pen = 0
             tot_cnt = 0
             do cyc=1,ncycle
                tot_pen = tot_pen + pen(ftyp,cyc,j,k)
                tot_cnt = tot_cnt + cnt(ftyp,cyc,j,k)
             end do

             chi(ftyp,j,k) = (tot_pen/tot_cnt)
         end do
      end do
   end do


! ********************************************************************************
!  Calculate Total Bias Correction 
!
!  Calc biascr value (tot_cor/count), by ges|anl, by cycle, by channel, by region
! ********************************************************************************
   do ftyp=1,2
      do cyc=1,ncycle
         do j=1,nchanl
            do k=1,nregion

               if( cnt( ftyp,cyc,j,k ) > 0.00 ) then
                  avg_biascr( ftyp,cyc,j,k ) = tot_cor( ftyp,cyc,j,k )
                  sdv_biascr( ftyp,cyc,j,k ) = tot_cor2( ftyp,cyc,j,k )
                  call avgsdv( cnt( ftyp,cyc,j,k ), avg_biascr( ftyp,cyc,j,k ), &
                                                sdv_biascr( ftyp,cyc,j,k ), 0.00 ) 
               else
                  avg_biascr( ftyp,cyc,j,k ) = rmiss
                  sdv_biascr( ftyp,cyc,j,k ) = rmiss
               end if

            end do
         end do
      end do
   end do
   write(6,*) 'finished calculating avg_biascr'

! ********************************************************************************
!  Calculate Observed - ges|anl without bias correction
!
!  Calc avg omgnbc value (ombnbc/count), by ges|anl, by cycle, by channel, by region
! ********************************************************************************
   do ftyp=1,2
      do cyc=1,ncycle
         do j=1,nchanl
            do k=1,nregion

               if( cnt( ftyp,cyc,j,k ) > 0.00 ) then
                  avg_omgnbc( ftyp,cyc,j,k ) = omg_nbc( ftyp,cyc,j,k )
                  sdv_omgnbc( ftyp,cyc,j,k ) = omg_nbc2( ftyp,cyc,j,k )
                  call avgsdv( cnt( ftyp,cyc,j,k ), avg_omgnbc( ftyp,cyc,j,k ), &
                                                sdv_omgnbc( ftyp,cyc,j,k ), 0.00 ) 
               else
                  avg_omgnbc( ftyp,cyc,j,k ) = rmiss
                  sdv_omgnbc( ftyp,cyc,j,k ) = rmiss
               end if

            end do
         end do
      end do
   end do
   write(6,*) 'finished calculating avg_omgnbc'


! ********************************************************************************
!  Calculate Observed - ges|anl with bias correction
!
!  Calc avg omgbc value (ombnbc/count), by ges|anl, by cycle, by channel, by region
! ********************************************************************************
   do ftyp=1,2
      do cyc=1,ncycle
         do j=1,nchanl
            do k=1,nregion

               if( cnt( ftyp,cyc,j,k ) > 0.00 ) then
                  avg_omgbc( ftyp,cyc,j,k ) = omg_bc( ftyp,cyc,j,k )
                  sdv_omgbc( ftyp,cyc,j,k ) = omg_bc2( ftyp,cyc,j,k )
                  call avgsdv( cnt( ftyp,cyc,j,k ), avg_omgbc( ftyp,cyc,j,k ), &
                                                sdv_omgbc( ftyp,cyc,j,k ), 0.00 ) 
               else
                  avg_omgbc( ftyp,cyc,j,k ) = rmiss
                  sdv_omgbc( ftyp,cyc,j,k ) = rmiss
               end if

            end do
         end do
      end do
   end do
   write(6,*) 'finished calculating avg_omgbc'


! ********************************************************************************
!  Calculate contribution to penalty 
!
!  Average penalty value (pen/count), by ges|anl, by cycle, by channel, by region
! ********************************************************************************
   do ftyp=1,2
      do cyc=1,ncycle
         do j=1,nchanl
            do k=1,nregion

               if( cnt( ftyp,cyc,j,k ) > 0.00 ) then
                  avg_pen( ftyp,cyc,j,k ) = pen( ftyp,cyc,j,k )/cnt( ftyp,cyc,j,k )
               else
                  avg_pen( ftyp,cyc,j,k ) = rmiss
               end if

               if( ftyp == 1) then
                 if( cyc == 1) then
                   if( j == 1) then
                      write( 6,*) "ftyp,cyc,j,k,pen,cnt,avg_pen = ", ftyp,cyc,j,k, &
                                        pen(ftyp,cyc,j,k), cnt(ftyp,cyc,j,k), &
                                        avg_pen(ftyp,cyc,j,k)
                   end if
                 end if
               end if

            end do
         end do
      end do
   end do
   write(6,*) 'finished calculating penalty'


!*********************************************************************
!    Write output to txt files 
!  
!    Output file [satname].chan.txt (in CSV format).
!      line 1 is a file header containing satname, nchanl, ntimes
!      lines 2..nchan+1 is a channel header containing actual channel
!            number, use flag, freq, and wavelength values
!      line nchan+2 contains the 10 chi values for the channel 
!            1-5 are the values for 5 regions of the anl data,
!            6-10 are the values for the ges data
!
!    Output file [satname].cnt.time.txt, (in CSV format).
!*********************************************************************
   70 FORMAT(A15,',',A5,',',A5)

   72 FORMAT(A5,',',A3,',',F9.3,',',F9.3,',',F9.2,',',F9.2)

   73 FORMAT(A5,',',A3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                          ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                          ,F9.2,',',F9.2)


   78 FORMAT(A5,',',A10,',',I5.1,',',I5.1)
 
   79 FORMAT(A5,',',A10,',',I5.1,',',I5.1,',',I5.1,',',I5.1,',',I5.1,',' & 
                           ,I5.1,',',I5.1,',',I5.1,',',I5.1,',',I5.1,',')
 
   80 FORMAT(A5,',',A10,',',F9.3,',',F9.3) 

   81 FORMAT(A5,',',A10,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' & 
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3) 

   82 FORMAT(A5,',',A10,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' & 
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' & 
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3)

   83 FORMAT(A5,',',A10,',',F9.3,',',F9.3,',',F9.3,',',F9.3) 


   write(6,*) 'after formats declared'
   write(6,*)' '
   write(6,*)' before sat_chan_file assigned'
   sat_chan_file=trim(satname) // '.chan.txt'
   open(lsatchan,file=sat_chan_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
   write(6,*)' after sat_chan_file opened, status:  ', open_status

!*****************************************************************************
!*****************************************************************************
!  Output is in 2 stages.  
!    First write the [satname].chan.txt file
!    Second write the [satname].[n].time.txt files where each file contains
!     the cnt,pen,omgnbc,bcr,omgbc data for a given channel

!*****************************************************************************
!  Begin [satname].chan.txt file 1
!     write file header to sat_chan_file
!
   write(str_nchanl, '(i20)') nchanl
   write(str_ncycle, '(i20)') ncycle
   write(lsatchan,70) trim(adjustl(satname)), trim(adjustl(str_nchanl)), trim(adjustl(str_ncycle))
   write(6,*) 'added line 1, file header to lsatchan'

!*****************************************************************************
!     write channel information to sat_chan_file
!
   do j=1,nchanl
!      if( nregion == 1 ) then
!         write(lsatchan,72) trim(adjustl(chan_nums(j))), trim(useflg(j)), &
!                       chi(1,j,1), chi(2,j,1), wave(j), freq(j)
!      else
         write(lsatchan,73) trim(adjustl(chan_nums(j))), trim(useflg(j)), &
                       chi(1,j,1), chi(1,j,2), chi(1,j,3), chi(1,j,4), chi(1,j,5), &
                       chi(2,j,1), chi(2,j,2), chi(2,j,3), chi(2,j,4), chi(2,j,5), &
                       wave(j), freq(j)
!      end if

   end do
   write(6,*) 'wrote all channel lines to lsatchan'
   close(lsatchan)



!*****************************************************************************
!  Second state, loop over channel and write count, penalty data for
!    every time to [satname].[chan].time.txt file
!
   do chan=1,nchanl
     write(str_nchanl, '(i20)') chan-1
     cnt_out_file    = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                     // '.cnt.time.txt'
     pen_out_file    = trim(satname) // '.' // trim(adjustl(str_nchanl)) & 
                                     // '.pen.time.txt'
     omgnbc_out_file = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                     // '.omgnbc.time.txt'
     totcor_out_file = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                     // '.totcor.time.txt'
     omgbc_out_file  = trim(satname) // '.' // trim(adjustl(str_nchanl)) &
                                     // '.omgbc.time.txt'

     lsatout = 70 + chan

     !----------------------------------------------------------------------------
     !  COUNT data is arranged:
     !     for each time step:
     !          chan, time, cnt ges rgn 1, cnt ges rgn 2, cnt ges rgn 3, 
     !                      cnt ges rgn 4, cnt ges rgn 5, cnt anl rgn 1,
     !                      cnt anl rgn 2, cnt anl rgn 3, cnt anl rgn 4,
     !                      cnt anl rgn 5
     !
     open(lsatout,file=cnt_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' cnt_out_file opened, status:  ', open_status

     do cyc=1,ncycle
        if( nregion == 1 ) then
           write(6,*) 'nregion = 1, writing cnt_out_file'
           write(lsatout,78) trim(chan_nums(chan)), trim(times(cyc)),    &
                         int(cnt(1,cyc,chan,1)), int(cnt(2,cyc,chan,1))
        else
           write(lsatout,79) trim(chan_nums(chan)), trim(times(cyc)),       &
                         int(cnt(1,cyc,chan,1)), int(cnt(1,cyc,chan,2)), &
                         int(cnt(1,cyc,chan,3)), int(cnt(1,cyc,chan,4)), &
                         int(cnt(1,cyc,chan,5)), int(cnt(2,cyc,chan,1)), &
                         int(cnt(2,cyc,chan,2)), int(cnt(2,cyc,chan,3)), &
                         int(cnt(2,cyc,chan,4)), int(cnt(2,cyc,chan,5))
        end if
     end do
     close(lsatout)


     !----------------------------------------------------------------------------
     !  PENALTY data is arranged:
     !     for each time step:
     !          chan, time, pen ges rgn 1, pen ges rgn 2, pen ges rgn 3, 
     !                      pen ges rgn 4, pen ges rgn 5, pen anl rgn 1,
     !                      pen anl rgn 2, pen anl rgn 3, pen anl rgn 4,
     !                      pen anl rgn 5
     !
     open(lsatout,file=pen_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' pen_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         if( nregion == 1 ) then
            write(lsatout,80) trim(chan_nums(chan)), trim(times(cyc)),  &
                          avg_pen(1,cyc,chan,1), avg_pen(2,cyc,chan,1)
         else
            write(lsatout,81) trim(chan_nums(chan)), trim(times(cyc)),     &
                          avg_pen(1,cyc,chan,1), avg_pen(1,cyc,chan,2), &
                          avg_pen(1,cyc,chan,3), avg_pen(1,cyc,chan,4), &
                          avg_pen(1,cyc,chan,5), avg_pen(2,cyc,chan,1), &
                          avg_pen(2,cyc,chan,2), avg_pen(2,cyc,chan,3), &
                          avg_pen(2,cyc,chan,4), avg_pen(2,cyc,chan,5)
         end if
     end do
     close(lsatout)

     !----------------------------------------------------------------------------
     !  OM[G|A]NBC data is arranged:
     !     for each time step:
     !          chan, time, avg omgnbc rgn 1, avg omgnbc rgn 2, avg ombnbc rgn 3, 
     !                      avg omgnbc rgn 4, avg omgnbc rgn 5, avg omanbc rgn 1,
     !                      avg omanbc rgn 2, avg omanbc rgn 3, avg omanbc rgn 4,
     !                      avg omanbc rgn 5, 
     !                      sdv omgnbc rgn 1, sdv omgnbc rgn 2, sdv omgnbc rgn 3,
     !                      sdv omgnbc rgn 4, sdv omgnbc rgn 5, sdv omanbc rgn 1,
     !                      sdv omanbc rgn 2, sdv omanbc rgn 3, sdv omanbc rgn 4,
     !                      sdv omanbc rgn 5
     !
     open(lsatout,file=omgnbc_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' omgnbc_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         if( nregion == 1 ) then 
            write(lsatout,83) trim(chan_nums(chan)), trim(times(cyc)),        &
                          avg_omgnbc(1,cyc,chan,1), avg_omgnbc(2,cyc,chan,1), &
                          sdv_omgnbc(1,cyc,chan,1), sdv_omgnbc(2,cyc,chan,1)
         else 
            write(lsatout,82) trim(chan_nums(chan)), trim(times(cyc)),        &
                          avg_omgnbc(1,cyc,chan,1), avg_omgnbc(1,cyc,chan,2), &
                          avg_omgnbc(1,cyc,chan,3), avg_omgnbc(1,cyc,chan,4), &
                          avg_omgnbc(1,cyc,chan,5), avg_omgnbc(2,cyc,chan,1), &
                          avg_omgnbc(2,cyc,chan,2), avg_omgnbc(2,cyc,chan,3), &
                          avg_omgnbc(2,cyc,chan,4), avg_omgnbc(2,cyc,chan,5), &
                          sdv_omgnbc(1,cyc,chan,1), sdv_omgnbc(1,cyc,chan,2), &
                          sdv_omgnbc(1,cyc,chan,3), sdv_omgnbc(1,cyc,chan,4), &
                          sdv_omgnbc(1,cyc,chan,5), sdv_omgnbc(2,cyc,chan,1), &
                          sdv_omgnbc(2,cyc,chan,2), sdv_omgnbc(2,cyc,chan,3), &
                          sdv_omgnbc(2,cyc,chan,4), sdv_omgnbc(2,cyc,chan,5)
         end if
     end do
     close(lsatout)


     !----------------------------------------------------------------------------
     !  BIASCR (total correction) data is arranged:
     !     for each time step:
     !          chan, time, 
     !          avg ges biascr rgn 1, avg ges biascr rgn 2, avg ges biascr rgn 3, 
     !          avg ges biascr rgn 4, avg ges biascr rgn 5, avg anl biascr rgn 1,
     !          avg anl biascr rgn 2, avg anl biascr rgn 3, avg anl biascr rgn 4,
     !          avg anl biascr rgn 5, 
     !          sdv ges biascr rgn 1, sdv ges biascr rgn 2, sdv ges biascr rgn 3,
     !          sdv ges biascr rgn 4, sdv ges biascr rgn 5, sdv anl biascr rgn 1,
     !          sdv anl biascr rgn 2, sdv anl biascr rgn 3, sdv anl biascr rgn 4,
     !          sdv anl biascr rgn 5
     !
     open(lsatout,file=totcor_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' totcor_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         if( nregion == 1 ) then 
            write(lsatout,83) trim(chan_nums(chan)), trim(times(cyc)),        &
                          avg_biascr(1,cyc,chan,1), avg_biascr(2,cyc,chan,1), &
                          sdv_biascr(1,cyc,chan,1), sdv_biascr(2,cyc,chan,1)
         else
            write(lsatout,82) trim(chan_nums(chan)), trim(times(cyc)),     &
                          avg_biascr(1,cyc,chan,1), avg_biascr(1,cyc,chan,2), &
                          avg_biascr(1,cyc,chan,3), avg_biascr(1,cyc,chan,4), &
                          avg_biascr(1,cyc,chan,5), avg_biascr(2,cyc,chan,1), &
                          avg_biascr(2,cyc,chan,2), avg_biascr(2,cyc,chan,3), &
                          avg_biascr(2,cyc,chan,4), avg_biascr(2,cyc,chan,5), &
                          sdv_biascr(1,cyc,chan,1), sdv_biascr(1,cyc,chan,2), &
                          sdv_biascr(1,cyc,chan,3), sdv_biascr(1,cyc,chan,4), &
                          sdv_biascr(1,cyc,chan,5), sdv_biascr(2,cyc,chan,1), &
                          sdv_biascr(2,cyc,chan,2), sdv_biascr(2,cyc,chan,3), &
                          sdv_biascr(2,cyc,chan,4), sdv_biascr(2,cyc,chan,5)
         end if
     end do
     close(lsatout)


     !----------------------------------------------------------------------------
     !  OM[G|A]BC data is arranged:
     !     for each time step:
     !          chan, time, avg omgbc rgn 1, avg omgbc rgn 2, avg ombbc rgn 3, 
     !                      avg omgbc rgn 4, avg omgbc rgn 5, avg omabc rgn 1,
     !                      avg omabc rgn 2, avg omabc rgn 3, avg omabc rgn 4,
     !                      avg omabc rgn 5, 
     !                      sdv omgbc rgn 1, sdv omgbc rgn 2, sdv omgbc rgn 3,
     !                      sdv omgbc rgn 4, sdv omgbc rgn 5, sdv omabc rgn 1,
     !                      sdv omabc rgn 2, sdv omabc rgn 3, sdv omabc rgn 4,
     !                      sdv omabc rgn 5
     !
     open(lsatout,file=omgbc_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
     write(6,*)' omgbc_out_file opened, status:  ', open_status

     do cyc=1,ncycle
         if( nregion == 1 ) then 
            write(lsatout,83) trim(chan_nums(chan)), trim(times(cyc)),        &
                          avg_omgbc(1,cyc,chan,1), avg_omgbc(2,cyc,chan,1), &
                          sdv_omgbc(1,cyc,chan,1), sdv_omgbc(2,cyc,chan,1)
         else
            write(lsatout,82) trim(chan_nums(chan)), trim(times(cyc)),     &
                          avg_omgbc(1,cyc,chan,1), avg_omgbc(1,cyc,chan,2), &
                          avg_omgbc(1,cyc,chan,3), avg_omgbc(1,cyc,chan,4), &
                          avg_omgbc(1,cyc,chan,5), avg_omgbc(2,cyc,chan,1), &
                          avg_omgbc(2,cyc,chan,2), avg_omgbc(2,cyc,chan,3), &
                          avg_omgbc(2,cyc,chan,4), avg_omgbc(2,cyc,chan,5), &
                          sdv_omgbc(1,cyc,chan,1), sdv_omgbc(1,cyc,chan,2), &
                          sdv_omgbc(1,cyc,chan,3), sdv_omgbc(1,cyc,chan,4), &
                          sdv_omgbc(1,cyc,chan,5), sdv_omgbc(2,cyc,chan,1), &
                          sdv_omgbc(2,cyc,chan,2), sdv_omgbc(2,cyc,chan,3), &
                          sdv_omgbc(2,cyc,chan,4), sdv_omgbc(2,cyc,chan,5)
         end if
     end do
     close(lsatout)

   end do   ! ncycle



   deallocate( cnt, pen, tot_cor, omg_nbc, omg_bc, tot_cor2, omg_nbc2, omg_bc2 )
   deallocate( avg_biascr, sdv_biascr, avg_omgnbc, sdv_omgnbc )
   deallocate ( avg_omgbc, sdv_omgbc, avg_pen )
   deallocate( useflg, chan_nums, chi, wave, freq )

   stop

end program gatime
