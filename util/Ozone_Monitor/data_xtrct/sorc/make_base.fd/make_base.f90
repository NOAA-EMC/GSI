program oznmon_make_base
  implicit none

!  Note that nregion is 6, but for error checking purposes we only 
!    need to consider region 1, which is "global". 
  integer nregion,nfile
  real rmiss

  
  parameter ( rmiss = -999.0, nregion=6 )

  character(20) satname

  real,allocatable,dimension(:,:,:):: count,error,use,penalty
  real,allocatable,dimension(:,:,:):: omg_cor

  character(40) cycle_file, data_file, level_file, out_file

  integer ierror,j,k,ii,counter,read_ctr
  integer,allocatable,dimension(:):: iuse
  integer,allocatable,dimension(:,:):: file_ctr

  real,allocatable,dimension(:,:):: total_count, total_penalty
  real,allocatable,dimension(:,:):: avg_count, avg_penalty
  real,allocatable,dimension(:,:):: sdv_count, sdv_penalty
  real,allocatable,dimension(:,:):: min_count, max_count
  real,allocatable,dimension(:,:):: min_penalty, max_penalty

  real total_sdv, avg_sdv, diff_count, diff_total, diff_pen, temp

  character(10) cycle

  logical fexist, gexist

  integer luncyc,lungrd,luname,lunout,lunlvl
  integer nlev

  namelist /input/ satname, nlev, nfile, out_file
  data luname,lunout,lungrd,luncyc,lunlvl / 5, 50,51,52,53 /

  write(6,*) ''
  write(6,*) 'Begin oznmon_make_base'
  write(6,*) ''
!
!  read namelist input
!
   read(luname, input)
   write(6,*) 'satname,nlev,nregion = ', satname, nlev, nregion

!   write(6,*)' begin allocate total_count, total_penalty'

   allocate( total_count( nlev,nregion ), total_penalty( nlev,nregion ) )
   allocate( avg_count( nlev,nregion ), avg_penalty( nlev,nregion ) )
   allocate( sdv_count( nlev,nregion ), sdv_penalty( nlev,nregion ) )
   allocate( min_count( nlev,nregion ), max_count( nlev,nregion ) )
   allocate( min_penalty( nlev,nregion ), max_penalty( nlev,nregion ) )
   allocate( file_ctr(nlev, nregion) )

   allocate( iuse(nlev) )
 
!   write(6,*)' after allocate iuse'

!
!  initialize vars 
! 
   cycle_file  = 'cycle_hrs.txt'


   do k=1,nregion
      do j=1,nlev
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
  
   file_ctr=0
 
   do j=1,nlev
      iuse(j) = -1
   end do

   write(6,*)' before allocate count, penalty'

!
!  allocate and initialize reading vars
! 
   allocate( count(nlev, nregion,nfile), penalty(nlev, nregion,nfile) )
   write(6,*)' after allocate total_count, total_penalty'

   count   = 0.0
   penalty = 0.0

!  write(6,*)' after initialize total_count, total_penalty'

!
!  verify the level.txt file exists and load iuse from it
! 
   level_file = 'level.txt' 
   inquire( file = level_file, exist = fexist )
   write(6,*) ' channel file ', level_file, ' fexist = ', fexist
   if( .NOT. fexist ) then
      write(6,*) ' channel file ', level_file, ' not found'
      call errexit( 94 )
   end if
   
   open( lunlvl, file=level_file, form='formatted',iostat=ierror )
   write(6,*) 'ierror from level_file open = ', ierror

   do j=1,nlev
      read( lunlvl,* ) iuse(j) 
      write(6,*) ' iuse(',j,')  = ', iuse(j)
   end do
   close( lunlvl )

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
   read_ctr = 0
   ierror = 0
   do while( ierror == 0 .AND. counter <= nfile )

      data_file = trim(satname) //'.'//cycle//'.ieee_d'
      inquire( file = data_file, exist = gexist )

      if( gexist ) then
         write(6,*) ' file ', data_file, ' is found'
         open( lungrd, file=data_file, form='unformatted', iostat=ierror ) 

         read( lungrd ) ((count(j,k,counter),j=1,nlev), k=1,nregion)
         read( lungrd ) ((penalty(j,k,counter),j=1,nlev), k=1,nregion)
         close( lungrd )
         read_ctr = read_ctr + 1
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

!   do ii=1,nfile
!      do k=1,nregion
!         do j=1,nlev
!            if( (count(j,k,ii) > 0.0) .AND. (iuse(j) > 0) ) then
!               temp = penalty(j,k,ii)/count(j,k,ii)
!               penalty(j,k,ii) = temp
!!               write(6,*)'penalty(j,k,ii),count(j,k,ii)',j,k,ii,penalty(j,k,ii), count(j,k,ii)
!            end if
!         end do
!      end do
!   end do

   write(6,*) ' number of files read = ', read_ctr
   do ii=1,nfile
      do k=1,nregion
         do j=1,nlev
            if( (count(j,k,ii) > 0.0) .AND. (iuse(j) > 0) ) then
               total_count(j,k)   = total_count(j,k) + count(j,k,ii)          
               total_penalty(j,k) = total_penalty(j,k) + penalty(j,k,ii)
               file_ctr(j,k)      = file_ctr(j,k) + 1
               write(6,*) 'file_ctr(j,k),filenum = ', j, k, file_ctr(j,k),ii

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
      do j=1,nlev

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
!  loop over level/region and write out the obs and penalty avg & sdv
!
   write(6,*) 'writing output to ', out_file
   open(lunout,file=out_file,form='formatted')
   write(lunout,*) satname, nlev, nregion
   write(lunout,*) 'fields:  level, region, avg_count, sdv_count, min_count, max_count, avg_pen, sdv_pen, min_pen, max_pen'
   
   do k=1,nregion
      do j=1,nlev
          write(lunout,*) j,k,avg_count(j,k),sdv_count(j,k), min_count(j,k), max_count(j,k), avg_penalty(j,k),sdv_penalty(j,k), min_penalty(j,k), max_penalty(j,k)
      end do
   end do

! deallocate( count, penalty ) 

 write(6,*) ''
 write(6,*) 'End oznmon_make_base'
 write(6,*) ''

 stop
end program oznmon_make_base
