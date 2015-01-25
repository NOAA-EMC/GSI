program time
!************************************************************************
!
!  validate time.f90
!
!  log
!    08/2013	safford  created from time.f90
!************************************************************************

  use bad_obs
  use bad_penalty
  use bad_chan
  use valid 

  implicit none
  integer ntype,mregion,surf_nregion
  parameter (ntype=8,mregion=25, surf_nregion=5)

  character(20) satname,stringd
  character(8) date,cycle
  character(80) data_file
  integer luname,lungrd,lunctl,lndiag,nregion
  integer iyy,imm,idd,ihh,idhh,incr,iread,iflag
  integer j,k
  integer :: ios = 0
  integer :: iret 

  real bound
  real rmiss
  real,allocatable,dimension(:,:):: count,penalty
  real,allocatable,dimension(:):: test_pen

  logical valid_penalty
  integer :: nchanl
  integer,allocatable,dimension(:) :: test_iuse,test_chan

!*************************
! Namelist definitions
!
  namelist /input/ satname,iyy,imm,idd,ihh,&
       nchanl
  namelist /iuseflg/ test_iuse
  namelist /ichannum/ test_chan

  data luname,lungrd,lunctl,lndiag / 5, 51, 52, 21 /
  data rmiss /-999./
  data stringd / '.%y4%m2%d2%h2' /


!*************************
! Read namelist input
  write(6,*) '--> validate_time'

  read(luname,input)
  write(6,input)
  write(6,*)' '

  allocate (test_iuse(nchanl), test_chan(nchanl), test_pen(nchanl))
  allocate (count(nchanl,surf_nregion), penalty(nchanl,surf_nregion))

! Read namelist iuseflg
  read(luname,iuseflg)
  write(6,iuseflg)

! Read namelist iuseflg
  read(luname,ichannum)


! Create filenames for diagnostic input, binary output file
  write(stringd,100) iyy,imm,idd,ihh
100 format('.',i4.4,3i2.2)

   data_file= 'time.' // trim(satname) // trim(stringd) // '.ieee_d'
   write(6,*)'data_file=',data_file


!    Open data_file
  
   write(6,*)' '
   open(lungrd,file=data_file,form='unformatted')
   read(lungrd) ((count(j,k),j=1,nchanl),k=1,surf_nregion)
   read(lungrd) ((penalty(j,k),j=1,nchanl),k=1,surf_nregion)

   close(lungrd)


!****************************************************************************
!  initialize bad* reporting files,  load base penalty values for validation
!    validation
!
   date = stringd(2:9)
   cycle = stringd(10:11) 
   write(6,*) 'date, cycle = ', date, cycle
   call open_bad_penalty_file( date, cycle, ios )
   write(6,*) 'open_bad_penalty_file = ', ios
   call open_bad_chan_file( date, cycle, ios )
   write(6,*) 'open_bad_chan_file = ', ios
   call load_base( satname, ios )
   write(6,*) 'load_base = ', ios


!***************************************************************************
!  validate penalty values if there is a valid count and if iuse flag=1
   k=1
   do j=1,nchanl
      if (count(j,k)>0 .AND. test_iuse(j) == 1 ) then
         test_pen(j)=penalty(j,k)/count(j,k)
!         write(6,*) 'test_pen = ', j, test_pen(j)
         call validate_penalty( j, k, test_pen(j), valid_penalty, bound, iret )
         write(6,*) ' valid_penalty, iret = ', valid_penalty, iret
         if( (iret == 0) .AND. (valid_penalty .eqv. .FALSE.) ) then
            call write_bad_penalty( satname, test_chan(j), k, test_pen(j), bound )
         endif
      end if
   end do


! Deallocate arrays
  write(6,*)' '
  write(6,*)'deallocate arrays'
  deallocate(test_iuse,test_chan,test_pen,count,penalty)


  write(6,*) '<-- validate_time'
  stop
end program time
