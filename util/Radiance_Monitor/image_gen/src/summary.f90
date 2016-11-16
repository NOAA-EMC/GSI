program summary
!************************************************************************
!
!  summary.f90
!
!  Extract necessary data from *.ieee_d files (ges and anl) and export
!  to an ascii data file for use by javascript.
!
!  In addition to the namelist, a 2 other files -- times.txt and use.txt
!  must be available.  
!    times.txt --> must contain the 10 digit date(s) to be processed, 
!                  one time per line and comma separated. 
!    use.txt   --> must contain the iuse flag for each channel,
!                  one flag (1 | -1) per line and comma separated.
!
!************************************************************************

   use IFPORT
 
   implicit none

   character(10) pdate,ndate
   character(20) stringd,str_nchanl
   character(60) data_file,out_file
   character(200) outstr
   character(len=10),allocatable,dimension(:)::times
   character(len=2), allocatable,dimension(:)::use

   integer luname,ldname,loname,lpname
   integer cyc,ii,iflag,j,k,res,chan,ftyp,open_status

   logical exist

   real rmiss
   real chan_cnt, chan_tot, chan_omgbc, chan_omgbc2

   real,allocatable,dimension(:,:,:,:):: cnt,pen
   real,allocatable,dimension(:,:,:,:):: tot_cor,omg_nbc,omg_bc
   real,allocatable,dimension(:,:,:,:):: tot_cor2,omg_nbc2,omg_bc2
   real,allocatable,dimension(:,:,:)  :: avg_tot_cor, avg_pen
   real,allocatable,dimension(:,:,:)  :: avg_omgbc, sdv_omgbc


!************************************************************************
!  Namelist with defaults
!************************************************************************
   integer               :: nchanl               = 19
   integer               :: ncycle               = 1
   integer               :: nregion              = 5
   character(40)         :: satname              = 'ssmis_f18'  
   character(10)         :: st_time              = '2014070800'
   namelist /input/ satname, nchanl, ncycle, nregion, st_time


   data luname,lpname,ldname,loname / 5, 50, 51, 52 /
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
! Read use.txt input file, which is the use flag for each channel.
!************************************************************************
   allocate( use (nchanl) )

   open( lpname, file='use.txt' )
    
   do ii=1,nchanl
      read(lpname, *) use(ii)
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

   allocate ( avg_tot_cor  (2,nchanl,3) )  
   allocate ( avg_omgbc    (2,nchanl,2) )
   allocate ( sdv_omgbc    (2,nchanl,2) )
   allocate ( avg_pen      (2,nchanl,3) )


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
!  Total Bias Correction 
!  
!  Determine bias correction (tot_cor/count) for the latest cycle, the past 4
!  cycles (1 day) and past 120 cycles (30 days).  
!  
!  By channel, sum all tot_cor for the 3 periods and divide by the total count
!   for the same period.
! *****************************************************************************
   do ftyp=1,2
      do j=1,nchanl
         chan_cnt = 0.0
         chan_tot = 0.0

         do cyc=1,ncycle
            chan_cnt = chan_cnt + cnt(ftyp,cyc,j,1)
            chan_tot = chan_tot + tot_cor( ftyp,cyc,j,1 )

            if( cyc == 1 ) then
               if( chan_cnt > 0.0 ) then
                  avg_tot_cor( ftyp,j,1 ) = chan_tot / chan_cnt
               else
                  avg_tot_cor( ftyp,j,1 ) = 0.0
               end if
            else if( cyc == 4 ) then
               if( chan_cnt > 0.0 ) then
                  avg_tot_cor( ftyp,j,2 ) = chan_tot / chan_cnt
               else
                  avg_tot_cor( ftyp,j,2 ) = 0.0
               end if
            else if( cyc == ncycle ) then
               if( chan_cnt > 0.0 ) then
                  avg_tot_cor( ftyp,j,3 ) = chan_tot / chan_cnt
               else
                  avg_tot_cor( ftyp,j,3 ) = chan_tot / chan_cnt
               end if
            end if
         end do

      end do 
   end do
   write(6,*) 'finished bias cor '


! *****************************************************************************
!  Observed - ges|anl with bias correction
!    omgbc, omabc 
!  Average and standard deviation by channel for 1 cycle and 30 days, 8 values
!    total for each channel (ges & anl).
! *****************************************************************************
   do ftyp=1,2
      do j=1,nchanl

         chan_cnt    = 0.0
         chan_omgbc  = 0.0
         chan_omgbc2 = 0.0

         do cyc=1,ncycle
            chan_cnt    = chan_cnt + cnt(ftyp,cyc,j,1)
            chan_omgbc  = chan_omgbc + omg_bc( ftyp,cyc,j,1 )
            chan_omgbc2 = chan_omgbc2 + omg_bc2( ftyp,cyc,j,1 )

            if( cyc == 1 ) then
               avg_omgbc( ftyp,j,1 ) = chan_omgbc
               sdv_omgbc( ftyp,j,1 ) = chan_omgbc2
               call avgsdv(chan_cnt, avg_omgbc( ftyp,j,1 ),sdv_omgbc( ftyp,j,1), 0.00) 
            else if( cyc == ncycle ) then
               avg_omgbc( ftyp,j,2 ) = chan_omgbc
               sdv_omgbc( ftyp,j,2 ) = chan_omgbc2
               call avgsdv(chan_cnt, avg_omgbc( ftyp,j,2 ),sdv_omgbc( ftyp,j,2), 0.00) 
            end if   
         end do

      end do
   end do
   write(6,*) 'finished omgbc/omabc'

! *****************************************************************************
!  Contribution to penalty
!  Average, by channel, for 1 cycle, 1 day, and 30 days, 6 values
!    total for each channel (ges & anl).
! *****************************************************************************
   do ftyp=1,2
      do j=1,nchanl

         chan_tot = 0.0
         chan_cnt = 0.0

         do cyc=1,ncycle
            chan_tot = chan_tot + pen( ftyp,cyc,j,1 )
            chan_cnt = chan_cnt + cnt(ftyp,cyc,j,1)

            if( cyc == 1 ) then
                if( chan_cnt > 0.0 ) then
                   avg_pen( ftyp,j,1 ) = chan_tot / chan_cnt
                else
                   avg_pen( ftyp,j,1 ) = 0.00
                end if
            else if( cyc == 4 ) then
                if( chan_cnt > 0.0 ) then
                   avg_pen( ftyp,j,2 ) = chan_tot / chan_cnt
                else
                   avg_pen( ftyp,j,2 ) = 0.00
                end if
            else if( cyc == ncycle ) then
                if( chan_cnt > 0.0 ) then
                   avg_pen( ftyp,j,3 ) = chan_tot / chan_cnt
                else
                   avg_pen( ftyp,j,3 ) = 0.00
                end if
            end if

         end do
      end do
   end do
   write(6,*) 'finished penalty'


!*********************************************************************
!    Write output to txt file 
!  
!    Output file is named [satname].sum.cnt.txt, and in CSV format
!      line 1 is channel,use flag,cycle1,cycle2,cycle3,cycle4
!             (each cycle is 10 digit cycle time)
!      line 2-n is channel#, use flag, count1, count2, count3, count4
!*********************************************************************
   write(6,*)' '
   write(6,*)' before out_file assigned'
   out_file= trim(satname) // '.sum.txt'
   write(6,*)' after out_file assigned:  ', out_file
   open(loname,file=out_file,iostat=open_status,action='write',status='new',form='formatted')
   write(6,*)' after out_file opened, status:  ', open_status
 
   70 FORMAT(A4,',',A10,',',A10,',',A10,',',A10)
   71 FORMAT(I5.1,',',A2,',',I5.1,',',I5.1,',',I5.1,',',I5.1, &
                         ',',I5.1,',',I5.1,',',I5.1,',',I5.1)
   72 FORMAT(I5.1,',',A2,',',F7.4,',',F7.4,',',F7.4, &
                         ',',F7.4,',',F7.4,',',F7.4)
   73 FORMAT(I5.1,',',A2,',',F9.4,',',F9.4,',',F9.4,',',F9.4, &
                         ',',F9.4,',',F9.4,',',F9.4,',',F9.4)
   write(6,*) 'after formats delcared'

   write(str_nchanl, '(i20)') nchanl
   write(loname,70) trim(adjustl(str_nchanl)),trim(times(1)),trim(times(2)),&
                                     trim(times(3)),trim(times(4))
   write(6,*) 'wrote str_nchanl and 4 times'

!*****************************************************************************
!  write ges and anl counts to output file 
!    format: channel, use flage, ges values cycles 1-4, anl values cycles 1-4

   do j=1,nchanl
      write(loname,71) j, trim(use(j)), int(cnt(1,1,j,1)), int(cnt(1,2,j,1)), &
                                        int(cnt(1,3,j,1)), int(cnt(1,4,j,1)), &
                                        int(cnt(2,1,j,1)), int(cnt(2,2,j,1)), &
                                        int(cnt(2,3,j,1)), int(cnt(2,4,j,1))
   end do
   write(6,*) 'finished writing counts'

!*****************************************************************************
!  write tot_cor/count values to output file 
!    format: channel, use flage, tot_cor/cnt for 1 cycle, 4 cycles, 120 cycles
   do j=1,nchanl
      write(loname,72) j, trim(use(j)), &
            avg_tot_cor(1,j,1), avg_tot_cor(1,j,2), avg_tot_cor(1,j,3), &
            avg_tot_cor(2,j,1), avg_tot_cor(2,j,2), avg_tot_cor(2,j,3) 
   end do
   write(6,*) 'finished writing tot_cor'

!*****************************************************************************
!  write omgbc|omabc avg and sdv values by channel to output file 
!    format: channel, use flage, avg omgbc 1 cycle, sdv omgbc 1 cycle,
!                                avg omgbc 30 days, sdv omgbc 30 days,
!                                avg omabc 1 cycle, sdv omabc 1 cycle,
!                                avg omabc 30 days, sdv omabc 30 days

   do j=1,nchanl
      do ii=1,2
         if ( IsNaN(avg_omgbc(1,j,ii)) ) then
            avg_omgbc(1,j,ii) = rmiss
         end if
         if ( IsNaN(avg_omgbc(1,j,ii)) ) then
            avg_omgbc(1,j,ii) = rmiss
         end if
      end do

      write(loname,73) j, trim(use(j)), &
         avg_omgbc(1,j,1), sdv_omgbc(1,j,1), avg_omgbc(1,j,2), sdv_omgbc(1,j,2), &
         avg_omgbc(2,j,1), sdv_omgbc(2,j,1), avg_omgbc(2,j,2), sdv_omgbc(2,j,2)
   end do
   write(6,*) 'finished writing omgbc|omabc'

!*****************************************************************************
!  write avg_pen values to output file 
!    format: channel, use flage, avg_pen for 1 cycle, 4 cycles, 120 cycles
!            ges values), avg_pen for 1 cycle, 4 cycles, 120 cycles (ges values) 
   do j=1,nchanl
      write(loname,72) j, trim(use(j)), &
            avg_pen(1,j,1), avg_pen(1,j,2), avg_pen(1,j,3), &
            avg_pen(2,j,1), avg_pen(2,j,2), avg_pen(2,j,3) 
   end do
   write(6,*) 'finished writing penalty'

   close(loname)


   deallocate( cnt, pen, tot_cor, omg_nbc, omg_bc, tot_cor2, omg_nbc2, omg_bc2 )
   deallocate( avg_tot_cor )
   deallocate ( avg_omgbc, sdv_omgbc, avg_pen )

   stop

end program summary
