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

   use IFPORT
 
   implicit none
   integer nregion

   character(5)  str_chan
   character(10) pdate,ndate
   character(20) stringd,str_nchanl,str_ncycle
   character(60) data_file, sat_chan_file, sat_out_file
   character(200) outstr
   character(len=10),allocatable,dimension(:)::times
   character(len=2), allocatable,dimension(:)::useflg
   character(len=5), allocatable,dimension(:)::chan_nums
  
   integer luname,ldname,lpname,lsatchan,lsatout
   integer cyc,ii,jj,iflag,j,k,res,chan,ftyp,open_status

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
   namelist /input/ satname, nchanl, ncycle


   data nregion / 5 /
   data luname,ldname,lpname,lsatchan / 5, 50, 51, 52 /
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
   allocate( chi(2, nchanl, nregion) )

   open( lpname, file='chan.txt' )

   do ii=1,nchanl
      read(lpname, *) chan_nums(ii), useflg(ii), wave(ii), freq(ii)
      do jj=1,nregion
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

         if ( exist == .TRUE. ) then
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
                  pen(ftyp,cyc,j,k)      = rmiss
                  omg_nbc (ftyp,cyc,j,k) = 0.0  
                  tot_cor (ftyp,cyc,j,k) = rmiss
                  omg_bc  (ftyp,cyc,j,k) = 0.0   
                  omg_nbc2(ftyp,cyc,j,k) = 0.0   
                  tot_cor2(ftyp,cyc,j,k) = rmiss 
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
                  avg_biascr( ftyp,cyc,j,k ) = 0.00
                  sdv_biascr( ftyp,cyc,j,k ) = 0.00
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
                  avg_omgnbc( ftyp,cyc,j,k ) = 0.00
                  sdv_omgnbc( ftyp,cyc,j,k ) = 0.00
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
                  avg_omgbc( ftyp,cyc,j,k ) = 0.00
                  sdv_omgbc( ftyp,cyc,j,k ) = 0.00
               end if

            end do
         end do
      end do
   end do
   write(6,*) 'finished calculating avg_omgbc'


! *****************************************************************************
!  Observed - ges|anl with bias correction
!    omgbc, omabc 
!  Average and standard deviation by channel for 1 cycle and 30 days, 8 values
!    total for each channel (ges & anl).
! *****************************************************************************
!   do ftyp=1,2
!      do j=1,nchanl
!
!         chan_cnt    = 0.0
!         chan_omgbc  = 0.0
!         chan_omgbc2 = 0.0
!
!         do cyc=1,ncycle
!            chan_cnt    = chan_cnt + cnt(ftyp,cyc,j,1)
!            chan_omgbc  = chan_omgbc + omg_bc( ftyp,cyc,j,1 )
!            chan_omgbc2 = chan_omgbc2 + omg_bc2( ftyp,cyc,j,1 )
!
!            if( cyc == 1 ) then
!               avg_omgbc( ftyp,j,1 ) = chan_omgbc
!               sdv_omgbc( ftyp,j,1 ) = chan_omgbc2
!               call avgsdv(chan_cnt, avg_omgbc( ftyp,j,1 ),sdv_omgbc( ftyp,j,1), 0.00) 
!            else if( cyc == ncycle ) then
!               avg_omgbc( ftyp,j,2 ) = chan_omgbc
!               sdv_omgbc( ftyp,j,2 ) = chan_omgbc2
!               call avgsdv(chan_cnt, avg_omgbc( ftyp,j,2 ),sdv_omgbc( ftyp,j,2), 0.00) 
!            end if   
!         end do
!
!      end do
!   end do
!   write(6,*) 'finished omgbc/omabc'


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
                  avg_pen( ftyp,cyc,j,k ) = 0.00
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

   73 FORMAT(A5,',',A3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                          ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                          ,F9.2,',',F9.2)

   74 FORMAT(A5,',',A10,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' & 
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3) 

   75 FORMAT(A5,',',A10,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' & 
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3) 

   80 FORMAT(A5,',',A10,',',I5.1,',',I5.1,',',I5.1,',',I5.1,',',I5.1,',' & 
                           ,I5.1,',',I5.1,',',I5.1,',',I5.1,',',I5.1,',' & 
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' & 
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' & 
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' & 
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' & 
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3,',' &
                           ,F9.3,',',F9.3,',',F9.3,',',F9.3,',',F9.3) 
   
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
      write(lsatchan,73) trim(adjustl(chan_nums(j))), trim(useflg(j)), &
                       chi(1,j,1), chi(1,j,2), chi(1,j,3), chi(1,j,4), chi(1,j,5), &
                       chi(2,j,1), chi(2,j,2), chi(2,j,3), chi(2,j,4), chi(2,j,5), &
                       wave(j), freq(j)
   end do
   write(6,*) 'wrote all channel lines to lsatchan'
   close(lsatchan)



!*****************************************************************************
!  Second state, loop over channel and write count, penalty data for
!    every time to [satname].[chan].time.txt file
!
   do chan=1,nchanl
      write(str_nchanl, '(i20)') chan-1
      sat_out_file= trim(satname) // '.' // trim(adjustl(str_nchanl)) // '.time.txt'
      write(6,*)' after sat_out_file assigned:  ', sat_out_file

      lsatout = 60 + chan 
      open(lsatout,file=sat_out_file,iostat=open_status, &
                             action='write',status='new',form='formatted')
      write(6,*)' sat_out_file opened, status:  ', open_status

!     write ges and anl counts to output file 
!       format: time, 10 cnt values corresponding to 5 regions for ges | anl for
!       each given channel at the specified cycle

      do cyc=1,ncycle
         write(lsatout,80) trim(chan_nums(chan)),  trim(times(cyc)),       &
                          int(cnt(1,cyc,chan,1)), int(cnt(1,cyc,chan,2)), &
                          int(cnt(1,cyc,chan,3)), int(cnt(1,cyc,chan,4)), &
                          int(cnt(1,cyc,chan,5)), int(cnt(2,cyc,chan,1)), &
                          int(cnt(2,cyc,chan,2)), int(cnt(2,cyc,chan,3)), &
                          int(cnt(2,cyc,chan,4)), int(cnt(2,cyc,chan,5)), &
                          avg_pen(1,cyc,chan,1), avg_pen(1,cyc,chan,2), &
                          avg_pen(1,cyc,chan,3), avg_pen(1,cyc,chan,4), &
                          avg_pen(1,cyc,chan,5), avg_pen(2,cyc,chan,1), &
                          avg_pen(2,cyc,chan,2), avg_pen(2,cyc,chan,3), &
                          avg_pen(2,cyc,chan,4), avg_pen(2,cyc,chan,5), &
                          avg_omgnbc(1,cyc,chan,1), avg_omgnbc(1,cyc,chan,2), &
                          avg_omgnbc(1,cyc,chan,3), avg_omgnbc(1,cyc,chan,4), &
                          avg_omgnbc(1,cyc,chan,5), avg_omgnbc(2,cyc,chan,1), &
                          avg_omgnbc(2,cyc,chan,2), avg_omgnbc(2,cyc,chan,3), &
                          avg_omgnbc(2,cyc,chan,4), avg_omgnbc(2,cyc,chan,5), &
                          sdv_omgnbc(1,cyc,chan,1), sdv_omgnbc(1,cyc,chan,2), &
                          sdv_omgnbc(1,cyc,chan,3), sdv_omgnbc(1,cyc,chan,4), &
                          sdv_omgnbc(1,cyc,chan,5), sdv_omgnbc(2,cyc,chan,1), &
                          sdv_omgnbc(2,cyc,chan,2), sdv_omgnbc(2,cyc,chan,3), &
                          sdv_omgnbc(2,cyc,chan,4), sdv_omgnbc(2,cyc,chan,5), &
                          avg_biascr(1,cyc,chan,1), avg_biascr(1,cyc,chan,2), &
                          avg_biascr(1,cyc,chan,3), avg_biascr(1,cyc,chan,4), &
                          avg_biascr(1,cyc,chan,5), avg_biascr(2,cyc,chan,1), &
                          avg_biascr(2,cyc,chan,2), avg_biascr(2,cyc,chan,3), &
                          avg_biascr(2,cyc,chan,4), avg_biascr(2,cyc,chan,5), &
                          sdv_biascr(1,cyc,chan,1), sdv_biascr(1,cyc,chan,2), &
                          sdv_biascr(1,cyc,chan,3), sdv_biascr(1,cyc,chan,4), &
                          sdv_biascr(1,cyc,chan,5), sdv_biascr(2,cyc,chan,1), &
                          sdv_biascr(2,cyc,chan,2), sdv_biascr(2,cyc,chan,3), &
                          sdv_biascr(2,cyc,chan,4), sdv_biascr(2,cyc,chan,5), &
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
      end do
   end do

   close(lsatout)

!*****************************************************************************
!  Write penalty data to sat.pen.time file
!
!    format: time, 10 avg_pen values corresponding to 5 regions for ges | anl for
!    each given channel at the specified cycle
!   sat_pen_file= trim(satname) // '.pen.time.txt'
!   write(6,*)' after sat_pen_file assigned:  ', sat_pen_file
!   open(lsatpen,file=sat_pen_file,iostat=open_status, &
!                             action='write',status='new',form='formatted')
!   write(6,*)' after sat_pen_file opened, status:  ', open_status
!
!
!   do chan=1,nchanl
!      do cyc=1,ncycle
!         write(lsatpen,74) trim(chan_nums(chan)),  trim(times(cyc)),       &
!                          avg_pen(1,cyc,chan,1), avg_pen(1,cyc,chan,2), &
!                          avg_pen(1,cyc,chan,3), avg_pen(1,cyc,chan,4), &
!                          avg_pen(1,cyc,chan,5), avg_pen(2,cyc,chan,1), &
!                          avg_pen(2,cyc,chan,2), avg_pen(2,cyc,chan,3), &
!                          avg_pen(2,cyc,chan,4), avg_pen(2,cyc,chan,5)
!      end do
!   end do
!
!   close( lsatpen )


!*****************************************************************************
!  Write omgnbc data to sat.nbc.time file
!
!  write ges and anl avg_omgnbc and sdv_omgnbc values to output file 
!    format: time, 10 avg_omgnbc values corresponding to 5 regions for ges | anl for
!    each given channel at the specified cycle, 10 sdv_omgnbc values for same

!   sat_nbc_file= trim(satname) // '.omgnbc.time.txt'
!   write(6,*)' after sat_nbc_file assigned:  ', sat_nbc_file
!   open(lsatnbc,file=sat_nbc_file,iostat=open_status, &
!                             action='write',status='new',form='formatted')
!   write(6,*)' after sat_nbc_file opened, status:  ', open_status
!
!   do chan=1,nchanl
!      do cyc=1,ncycle
!         write(lsatnbc,75) trim(chan_nums(chan)),  trim(times(cyc)),       &
!                          avg_omgnbc(1,cyc,chan,1), avg_omgnbc(1,cyc,chan,2), &
!                          avg_omgnbc(1,cyc,chan,3), avg_omgnbc(1,cyc,chan,4), &
!                          avg_omgnbc(1,cyc,chan,5), avg_omgnbc(2,cyc,chan,1), &
!                          avg_omgnbc(2,cyc,chan,2), avg_omgnbc(2,cyc,chan,3), &
!                          avg_omgnbc(2,cyc,chan,4), avg_omgnbc(2,cyc,chan,5), &
!                          sdv_omgnbc(1,cyc,chan,1), sdv_omgnbc(1,cyc,chan,2), &
!                          sdv_omgnbc(1,cyc,chan,3), sdv_omgnbc(1,cyc,chan,4), &
!                          sdv_omgnbc(1,cyc,chan,5), sdv_omgnbc(2,cyc,chan,1), &
!                          sdv_omgnbc(2,cyc,chan,2), sdv_omgnbc(2,cyc,chan,3), &
!                          sdv_omgnbc(2,cyc,chan,4), sdv_omgnbc(2,cyc,chan,5)
!      end do
!   end do

!   close( lsatnbc )

!*****************************************************************************
!  Write omgnbc data to sat.bcr.time file
!
!  write ges and anl avg_biascr and sdv_biascr values to output file 
!    format: time, 10 avg_biascr values corresponding to 5 regions for ges | anl for
!    each given channel at the specified cycle, 10 sdv_biascr values for same

!   sat_bcr_file= trim(satname) // '.bcr.time.txt'
!   write(6,*)' after sat_bcr assigned:  ', sat_bcr_file
!   open(lsatbcr,file=sat_bcr_file,iostat=open_status, &
!                             action='write',status='new',form='formatted')
!   write(6,*)' after sat_bcr_file opened, status:  ', open_status
!   do chan=1,nchanl
!      do cyc=1,ncycle
!         write(lsatbcr,75) trim(chan_nums(chan)),  trim(times(cyc)),       &
!                          avg_biascr(1,cyc,chan,1), avg_biascr(1,cyc,chan,2), &
!                          avg_biascr(1,cyc,chan,3), avg_biascr(1,cyc,chan,4), &
!                          avg_biascr(1,cyc,chan,5), avg_biascr(2,cyc,chan,1), &
!                          avg_biascr(2,cyc,chan,2), avg_biascr(2,cyc,chan,3), &
!                          avg_biascr(2,cyc,chan,4), avg_biascr(2,cyc,chan,5), &
!                          sdv_biascr(1,cyc,chan,1), sdv_biascr(1,cyc,chan,2), &
!                          sdv_biascr(1,cyc,chan,3), sdv_biascr(1,cyc,chan,4), &
!                          sdv_biascr(1,cyc,chan,5), sdv_biascr(2,cyc,chan,1), &
!                          sdv_biascr(2,cyc,chan,2), sdv_biascr(2,cyc,chan,3), &
!                          sdv_biascr(2,cyc,chan,4), sdv_biascr(2,cyc,chan,5)
!      end do
!   end do

!   close( lsatbcr )

!*****************************************************************************
!  Write omgbc data to sat.bc.time file
!
!  write ges and anl avg_omgbc and sdv_omgbc values to output file 
!    format: time, 10 avg_omgbc values corresponding to 5 regions for ges | anl for
!    each given channel at the specified cycle, 10 sdv_omgbc values for same

!   sat_bc_file= trim(satname) // '.bc.time.txt'
!   write(6,*)' after sat_bc_file assigned:  ', sat_bc_file
!   open(lsatbc,file=sat_bc_file,iostat=open_status, &
!                             action='write',status='new',form='formatted')
!   write(6,*)' after sat_bc_file opened, status:  ', open_status
!
!   do chan=1,nchanl
!      do cyc=1,ncycle
!         write(lsatbc,75) trim(chan_nums(chan)),  trim(times(cyc)),       &
!                          avg_omgbc(1,cyc,chan,1), avg_omgbc(1,cyc,chan,2), &
!                          avg_omgbc(1,cyc,chan,3), avg_omgbc(1,cyc,chan,4), &
!                          avg_omgbc(1,cyc,chan,5), avg_omgbc(2,cyc,chan,1), &
!                          avg_omgbc(2,cyc,chan,2), avg_omgbc(2,cyc,chan,3), &
!                          avg_omgbc(2,cyc,chan,4), avg_omgbc(2,cyc,chan,5), &
!                          sdv_omgbc(1,cyc,chan,1), sdv_omgbc(1,cyc,chan,2), &
!                          sdv_omgbc(1,cyc,chan,3), sdv_omgbc(1,cyc,chan,4), &
!                          sdv_omgbc(1,cyc,chan,5), sdv_omgbc(2,cyc,chan,1), &
!                          sdv_omgbc(2,cyc,chan,2), sdv_omgbc(2,cyc,chan,3), &
!                          sdv_omgbc(2,cyc,chan,4), sdv_omgbc(2,cyc,chan,5)
!      end do
!   end do

!   write(6,*) 'finished writing sat.bc.time.txt'
!   close(lsatbc)


   deallocate( cnt, pen, tot_cor, omg_nbc, omg_bc, tot_cor2, omg_nbc2, omg_bc2 )
   deallocate( avg_biascr, sdv_biascr, avg_omgnbc, sdv_omgnbc )
   deallocate ( avg_omgbc, sdv_omgbc, avg_pen )
   deallocate( useflg, chan_nums, chi, wave, freq )

   stop

end program gatime
