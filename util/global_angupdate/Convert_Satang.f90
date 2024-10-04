program convert_satang
!$$$  main program documentation block
!                .      .    .                                       .
! main program: Convert_Satang
!   PRGMMR: Collard             DATE: 2012-10-26
!
! abstract:  This program changes the format of the satang files
!            to include the number of scan positions in the header and
!            to change the number of scan positions.
!
! program history log:
!   10-26-12  collard    initial code  from global_angupdate

! usage:
!   input files:
!     lnangl   - scan angle dependent bias correction file
!
!   output files:
!     lnupdt   - updated scan angle dependent bias correction file
!
!   exit states:
!     cond =   0 - successful run
!          =  98 - inconsistent satellite/sensor identifiers
!          =  99 - problem reading input satang file
!
!
! attributes:
!   language: f90
!
!$$$

  implicit none

! Declare local parameters
  integer,parameter:: lnangl = 11
  integer,parameter:: lnupdt = 52

  real,parameter:: zero = 0.0


! Declare local variables

  character(20):: satsensor0

  integer:: i, ios, shift_atms
  integer:: nstep_in, nstep_out
  integer :: ich, jchanum0

  real:: tlap0
  real,allocatable,dimension(:):: c_ang0


!************************************************************************

  write(*,*) 'Enter number of scan positions for input file'
  write(*,*) '     (-1 if specified in file)'
  read(*,*) nstep_in
  write(*,*) 'Enter number of scan positions for output file'
  read(*,*) nstep_out
  write(*,*) 'Enter one to shift ATMS'
  read(*,*) shift_atms
  

! Read input satang file
  open(lnangl,file='satbias_ang.in',form='formatted')
  open(lnupdt,file='satbias_ang.out',form='formatted')
  if (nstep_in <= 0) read(lnangl,'(6x,i8)') nstep_in
  allocate(c_ang0(MAX(nstep_out,nstep_in)))
  c_ang0(:)=zero
  write(lnupdt,'("nscan=",i8)') nstep_out
  ios=0
  do while (ios == 0)
     read(lnangl,110,err=120,end=120,iostat=ios) ich,satsensor0,&
          jchanum0,tlap0,c_ang0(1:nstep_in)
     if (shift_atms == 1 .AND. Trim(satsensor0)=='atms_npp') then
        c_ang0(4:93)=c_ang0(1:90)
        c_ang0(1:3)=c_ang0(4)
        c_ang0(94:96)=c_ang0(93)
     end if
     write(lnupdt,110) ich,satsensor0,&
          jchanum0,tlap0,c_ang0(1:nstep_out)
  end do
110 format(I5,1x,A20,1x,I5,e15.6/100(4x,10f7.3/))

120 continue

  close(lnangl)
  close(lnupdt)


end program convert_satang



