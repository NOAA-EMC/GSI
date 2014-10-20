program make_base
  implicit none

  integer n_chan, nregion, nfile, mregion, mfile
  real rmiss

  !
  ! note mregion (max region) in time.f90 is 25, while nregion is 5
  !
  parameter ( mregion=25, mfile=999 )
  parameter ( rmiss = -999.0 )

  character(20) satname
  real,allocatable,dimension(:,:,:):: count, penalty

  character(40) data_file, cycle_file, ctl_file, out_file
  character(40) channel_file
  integer lungrd, luncyc, j, k, ii, ierror, hr, status
  integer lunctl, lunout, lunchn, channels
  integer,allocatable,dimension(:):: iuse
  integer,allocatable,dimension(:,:):: file_ctr

  real,allocatable,dimension(:,:):: total_count, total_penalty
  real,allocatable,dimension(:,:):: avg_count, avg_penalty
  real,allocatable,dimension(:,:):: sdv_count, sdv_penalty
  real,allocatable,dimension(:,:):: min_count, max_count
  real,allocatable,dimension(:,:):: min_penalty, max_penalty

  integer num_sdv, counter
  real total_sdv, avg_sdv, diff_count, diff_total, diff_pen, temp
  
  character(10) date, new_date, cycle
  character(40) ctl_var1, ctl_var2, dummy
  logical fexist, gexist
  integer luname

  namelist /input/ satname, n_chan, nregion, nfile, date, out_file
  data luname,lunout / 5, 50 /

!
!  read namelist input
!
   read(luname, input)
   write(6, input)
   write(6,*) 'satname = ', satname

!
!  Ensure number of requested regions does not exceed specified upper limit
!
   write(6,*)' check nregion size'
   if (nregion>mregion) then
      write(6,*)'***ERROR*** too many regions specified'
      write(6,*)'   maximum allowed:  mregion=',mregion
      write(6,*)'    user requested:  nregion=',nregion
      call errexit(91)
   endif

!
!  Ensure number of input files does not exceed specified upper limit
!
   write(6,*)' check nfile too high'
   if (nfile>mfile) then
      write(6,*)'***ERROR*** too many input files specified'
      write(6,*)'   maximum allowed:  mfile=',mfile
      write(6,*)'    user requested:  nfile=',nfile
      call errexit(92)
   endif

!
! Ensure number of input files is one or greater
!
   write(6,*)' check nfile too low'
   if (nfile<=0) then
      write(6,*)'***ERROR*** too few input files specified'
      write(6,*)'   minimum allowed:       = 1'
      write(6,*)'    user requested:  nfile=',nfile
      call errexit(93)
   endif

!
!  allocate n_chan dependent arrays
!
   write(6,*)' begin allocate total_count, total_penalty'

   allocate( total_count( n_chan,nregion ), total_penalty( n_chan,nregion ) )
   allocate( avg_count( n_chan,nregion ), avg_penalty( n_chan,nregion ) )
   allocate( sdv_count( n_chan,nregion ), sdv_penalty( n_chan,nregion ) )
   allocate( min_count( n_chan,nregion ), max_count( n_chan,nregion ) )
   allocate( min_penalty( n_chan,nregion ), max_penalty( n_chan,nregion ) )
   allocate( file_ctr(n_chan, nregion) )
   allocate( iuse(n_chan) )
 
   write(6,*)' after allocate iuse'
!
!  initialize vars 
! 
   cycle_file  = 'cycle_hrs.txt'
   luncyc = 50
   lungrd = 51
   lunchn = 52


   do k=1,nregion
      do j=1,n_chan
         avg_count(j,k)     = rmiss
         sdv_count(j,k)     = rmiss
         avg_penalty(j,k)   = rmiss
         sdv_penalty(j,k)   = rmiss

         total_count(j,k)   = 0.0
         total_penalty(j,k) = 0.0
         file_ctr(j,k)      = 0

         min_count(j,k)     = rmiss
         max_count(j,k)     = rmiss
         min_penalty(j,k)   = rmiss
         max_penalty(j,k)   = rmiss
      end do
   end do
   
   do j=1,n_chan
      iuse(j) = -1
   end do

   num_sdv     = 0
   total_sdv   = 0.0

!   write(6,*)' before allocate count, penalty'

!
!  allocate and initialize reading vars
! 
   allocate( count(n_chan, nregion,nfile), penalty(n_chan, nregion,nfile) )
!  write(6,*)' after allocate total_count, total_penalty'

   count   = 0.0
   penalty = 0.0

!  write(6,*)' after initialize total_count, total_penalty'
!
!  verify the channel.txt file exists and load iuse from it
! 
   channel_file = 'channel.txt' 
   inquire( file = channel_file, exist = fexist )
   write(6,*) ' channel file ', channel_file, ' fexist = ', fexist
   if( .NOT. fexist ) then
      write(6,*) ' channel file ', channel_file, ' not found'
      call errexit( 94 )
   end if
   
   open( lunchn, file=channel_file, form='formatted',iostat=ierror )
   write(6,*) 'ierror from channel_file open = ', ierror

   do j=1,n_chan
      read( lunchn,* ) iuse(j) 
      write(6,*) ' iuse(',j,')  = ', iuse(j)
   end do
   close( lunchn )

!
!  verify cycle_hrs.txt file exists
!
   inquire( file = cycle_file, exist = fexist )
   write(6,*) ' cycle file ', cycle_file, ' fexist = ', fexist
   if( .NOT. fexist ) then
      write(6,*) ' cycle file ', cycle_file, ' not found'
      call errexit( 95 )
   end if

!
!  read a line from cycle file
!
   open( luncyc, file=cycle_file, form='formatted',iostat=ierror )
   read( luncyc,* ) cycle 

!
!  loop over the data files, 
!  read count and penalty values for each channel/region
!
   counter = 1 
   ierror = 0
   do while( ierror == 0 .AND. counter < nfile )

      data_file = trim(satname) //'.'//cycle//'.ieee_d'
      inquire( file = data_file, exist = gexist )

      if( gexist ) then
         open( lungrd, file=data_file, form='unformatted', iostat=ierror ) 

         read( lungrd ) ((count(j,k,counter),j=1,n_chan), k=1,nregion)
         read( lungrd ) ((penalty(j,k,counter),j=1,n_chan), k=1,nregion)
         close( lungrd )
      else
         write(6,*) ' file ', data_file, ' not found'
      end if

      read( luncyc, *, end=300 ) cycle 
      counter = counter + 1

   end do  ! file loop

300 continue
   close( luncyc )

!
!  accumulate sums and min/max values
!
   do ii=1,nfile
      do k=1,nregion
         do j=1,n_chan
            if( (count(j,k,ii) > 0.0) .AND. (iuse(j) > 0) ) then
               temp = penalty(j,k,ii)/count(j,k,ii)
               penalty(j,k,ii) = temp
               write(6,*)'penalty(j,k,ii),count(j,k,ii)',j,k,ii,penalty(j,k,ii), count(j,k,ii)
            end if
         end do
      end do
   end do

   write(6,*) ' number of files read = ', counter-1
   do ii=1,nfile
      do k=1,nregion
         do j=1,n_chan
            if( (count(j,k,ii) > 0.0) .AND. (iuse(j) > 0) ) then
               total_count(j,k)   = total_count(j,k) + count(j,k,ii)          
               total_penalty(j,k) = total_penalty(j,k) + penalty(j,k,ii)
               file_ctr(j,k)      = file_ctr(j,k) + 1

               if( min_count(j,k) == rmiss ) then
                  min_count(j,k) = count( j,k,ii )
               else if( (count(j,k,ii) > 0.0) .AND. (min_count(j,k) > count( j,k,ii )) ) then
                  min_count(j,k) = count( j,k,ii )
               end if

               if( max_count(j,k) == rmiss ) then
                  max_count(j,k) = count( j,k,ii )
               else if( max_count(j,k) < count( j,k,ii ) ) then
                  max_count(j,k) = count( j,k,ii )
               end if

               if( min_penalty(j,k) == rmiss ) then
                  min_penalty(j,k) = penalty( j,k,ii )
               else if( (penalty(j,k,ii) > 0.0) .AND. (min_penalty(j,k) > penalty( j,k,ii )) ) then
                  min_penalty(j,k) = penalty( j,k,ii )
               end if

               if( max_penalty(j,k) == rmiss ) then
                  max_penalty(j,k) = penalty( j,k,ii )
               else if( max_penalty(j,k) < penalty( j,k,ii ) ) then
                  max_penalty(j,k) = penalty( j,k,ii )
               end if

            end if
         end do
      end do
   end do

!
!  calculate averages and sdv
!
   do k=1,nregion
      do j=1,n_chan

          if( total_count(j,k) > 0 ) then
             avg_count(j,k) = total_count(j,k) / file_ctr(j,k)
             avg_penalty(j,k) = total_penalty(j,k) / file_ctr(j,k)
             write(6,*)'total_penalty(j,k), file_ctr(j,k), avg_penalty(j,k)= ',j,k,total_penalty(j,k),file_ctr(j,k),avg_penalty(j,k)

             diff_total = 0.0
             do ii=1,nfile 
                if( count(j,k,ii) > 0 ) then
                   diff_count = ( count(j,k,ii) - avg_count(j,k) )**2 
                   diff_total = diff_total + diff_count
                end if
             end do       
             sdv_count(j,k) = sqrt( diff_total/(file_ctr(j,k)-1) )

             diff_total = 0.0
             do ii=1,nfile
                if( count(j,k,ii) > 0 ) then
                   diff_pen = ( penalty(j,k,ii) - avg_penalty(j,k) )**2
                   diff_total = diff_total + diff_pen
                end if
             end do
             sdv_penalty(j,k) = sqrt(diff_total/file_ctr(j,k))
 
          end if   
      end do
   end do

!
!  loop over channel/region and write out the obs and penalty avg & sdv
!
   write(6,*) 'writing output to ', out_file
   open(lunout,file=out_file,form='formatted')
   write(lunout,*) satname, date, n_chan, nregion

   do k=1,nregion
      do j=1,n_chan
          write(lunout,*) j,k,avg_count(j,k),sdv_count(j,k), min_count(j,k), max_count(j,k), avg_penalty(j,k),sdv_penalty(j,k), min_penalty(j,k), max_penalty(j,k)
      end do
   end do

 deallocate( count, penalty ) 
 stop
end program make_base
