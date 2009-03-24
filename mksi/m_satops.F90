!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_satops - Input module of satops_[*] table
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_satops
      implicit none
      private	! except

      public :: satops_select
      public :: satops_clean
      public :: satops_show

      interface satops_select; module procedure &
        select_; end interface
      interface satops_clean; module procedure &
        dealloc_; end interface
      interface satops_show; module procedure &
        show_; end interface

! !REVISION HISTORY:
! 	04Jun07	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_satops'

!!$ Usecase:
!!$
!!$   character(len=LEN_WORD),pointer,dimension(:) :: p_nm => null()
!!$   integer :: n
!!$   call satops_select(p_nm,n,from="satops-0.tbl",append=.false.)
!!$   call satops_select(p_nm,n,from="satops-1.tbl",append=.true.)
!!$   [.. p_nm(1:n) ..]
!!$   call satops_clean(p_nm,n)
!!$

contains

subroutine select_(p_nm,n,where_dt,from,dbase,bufrsize,append)
  implicit none
  character(len=*),pointer,dimension(:) :: p_nm
  integer,intent(inout) :: n
  integer,dimension(:),intent(in) :: where_dt ! select all if d=-1
  character(len=*),intent(in) :: from

  character(len=*),optional,intent(in) :: dbase    ! default=".".
  integer         ,optional,intent(in) :: bufrsize ! default=256(bytes)
  logical         ,optional,intent(in) :: append   ! default=.false.

  integer :: lbufr

  lbufr=256
  if(present(bufrsize)) lbufr=bufrsize
  if(present(dbase)) then
    call selectb_(p_nm,n,where_dt,from, &
      dbase=dbase,bufrsize=lbufr,append=append)
  else
    call selectb_(p_nm,n,where_dt,from, &
      dbase='.'  ,bufrsize=lbufr,append=append)
  endif
end subroutine select_

subroutine selectb_(p_nm,n,where_dt,from,dbase,bufrsize,append)
  use satinfo_util,only : perr,die
  use satinfo_util,only : luavail,getarec
  use satinfo_util,only : alloc,realloc
  implicit none
  character(len=*),pointer,dimension(:) :: p_nm
  integer,intent(inout) :: n
  integer,dimension(:),intent(in) :: where_dt
  character(len=*),intent(in) :: from
  character(len=*),intent(in) :: dbase
  integer         ,intent(in) :: bufrsize
  logical,optional,intent(in) :: append

  integer :: lu,ier,m,k
  integer :: iymd,ihms
  integer :: nymd,nhms
  integer :: lymd,lhms
  character(len=bufrsize) :: bufr

  character(len=*),parameter :: myname_=myname//"::selectb_"

  character(len=1) :: ch
  logical :: time_matched
  logical :: append_
  append_=.false.; if(present(append)) append_=append

  lu=luavail()
  open(lu,file=trim(dbase)//'/'//trim(from),status='old',iostat=ier)
    if(ier/=0) call die(myname_, &
      'open("'//trim(dbase)//'/'//trim(from)//'"), iostat =',ier)

  if(.not.append_) then
    call alloc(p_nm,n)
  endif

  nymd=where_dt(1)
  nhms=where_dt(2)

  call getarec(lu,bufr,ier)
  do while(ier==0)
    read(bufr,*,iostat=ier) iymd,ihms,lymd,lhms,m
      if(ier/=0) then
        call perr(myname_,'bufr="'//trim(bufr)//'"')
        call die(myname_, &
	  'stem-read("'//trim(dbase)//'/'//trim(from)//'"), iostat =',ier)
      endif

    time_matched = nymd==-1
    if(.not.time_matched) time_matched = &
       (iymd<nymd .or. (iymd==nymd.and.ihms<=nhms)) .and. &
       (nymd<lymd .or. (nymd==lymd.and.nhms<=lhms))

    if(time_matched) then

      call realloc(p_nm,n,incr=m)
      read(bufr,*,iostat=ier) iymd,ihms,lymd,lhms,k,p_nm(n+1:n+m),ch
        if(ier==0.and.ch/="#") then
          call perr(myname_,'bufr="'//trim(bufr)//'"')
          call die(myname_, &
	    'leaf-read("'//trim(dbase)//'/'//trim(from)//'"), too many entries',m+1)
	endif

      read(bufr,*,iostat=ier) iymd,ihms,lymd,lhms,k,p_nm(n+1:n+m)
        if(ier/=0) then
          call perr(myname_,'bufr="'//trim(bufr)//'"')
          call die(myname_, &
	    'leaf-read("'//trim(dbase)//'/'//trim(from)//'"), iostat =',ier)
        endif

      n=n+m
    endif

        ! potential read() errors are ignored
    call getarec(lu,bufr,ier)
  enddo

  close(lu,iostat=ier)
    if(ier/=0) call die(myname_, &
      'close("'//trim(dbase)//'/'//trim(from)//'"), iostat =',ier)

end subroutine selectb_

subroutine dealloc_(p_nm,n)
  implicit none
  character(len=*),pointer,dimension(:) :: p_nm
  integer,intent(out) :: n

  deallocate(p_nm)
  n=-1
end subroutine dealloc_

subroutine show_(p_nm,n)
  implicit none
  character(len=*),pointer,dimension(:) :: p_nm
  integer,intent(in) :: n
  integer :: i
  print*, myname,": n=",n,"; i,nm"
  do i=1,n
    print*, i,p_nm(i)
  end do
end subroutine show_
end module m_satops
