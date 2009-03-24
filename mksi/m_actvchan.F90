!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_actvchan - Input module of active-channels table
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_actvchan
      implicit none
      private	! except

      public :: actvchan_select
      public :: actvchan_clean
      public :: actvchan_show

      interface actvchan_select; module procedure &
        select_; end interface
      interface actvchan_clean; module procedure &
        dealloc_; end interface
      interface actvchan_show; module procedure &
        show_; end interface

! !REVISION HISTORY:
! 	04Jun07	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_actvchan'

!!$ Usecase:
!!$
!!$   character(len=LEN_WORD),pointer,dimension(:) :: p_nm
!!$   integer,pointer,dimension(:) :: p_ch
!!$   integer :: n
!!$   call actvchan_select(p_nm,p_ch,n, &
!!$                        where_dt=(/19990330,060000/), &
!!$                        where_nm=(/"f10","f11"/), &
!!$                        from="active_channels.tbl")

!!$   [.. p_nm(1:n),p_ch(1:n) ..]
!!$   call actvchan_clean(p_nm,p_ch,n)
!!$

contains

subroutine select_(p_nm,p_sn,p_ch,n,where_dt,where_nm,from, &
                   dbase,bufrsize,append)  ! optionals
  implicit none
  character(len=*),pointer,dimension(:) :: p_nm
  character(len=*),pointer,dimension(:) :: p_sn
  integer         ,pointer,dimension(:) :: p_ch
  integer,intent(inout) :: n
  integer         ,dimension(:),intent(in) :: where_dt ! all if d=-1
  character(len=*),dimension(:),intent(in) :: where_nm
  character(len=*),intent(in) :: from

  character(len=*),optional,intent(in) :: dbase  ! default ".".
  integer         ,optional,intent(in) :: bufrsize ! default 256 bytes
  logical         ,optional,intent(in) :: append !

  integer :: lbufr

  lbufr=256
  if(present(bufrsize)) lbufr=bufrsize
  if(present(dbase)) then
    call selectb_(p_nm,p_sn,p_ch,n,where_dt,where_nm,from, &
      dbase=dbase,bufrsize=lbufr,append=append)
  else
    call selectb_(p_nm,p_sn,p_ch,n,where_dt,where_nm,from, &
      dbase='.'  ,bufrsize=lbufr,append=append)
  endif
end subroutine select_

subroutine selectb_(p_nm,p_sn,p_ch,n,where_dt,where_nm,from, &
                    dbase,bufrsize,append)
  use satinfo_util,only : perr,die
  use satinfo_util,only : luavail,getarec
  use satinfo_util,only : alloc,realloc
  use satinfo_util,only : locate
  implicit none
  character(len=*),pointer,dimension(:) :: p_nm
  character(len=*),pointer,dimension(:) :: p_sn
  integer         ,pointer,dimension(:) :: p_ch
  integer,intent(inout) :: n
  integer,dimension(:),intent(in) :: where_dt
  character(len=*),dimension(:),intent(in) :: where_nm
  character(len=*),intent(in) :: from
  character(len=*),intent(in) :: dbase
  integer         ,intent(in) :: bufrsize
  logical,optional,intent(in) :: append

  character(len=*),parameter :: myname_=myname//"::selectb_"

  integer :: lu,ier,j,k,l,m
  integer :: iymd,ihms
  integer :: nymd,nhms
  integer :: lymd,lhms
  character(len=len(p_nm)) :: nm
  character(len=len(p_sn)) :: sn
  character(len=bufrsize) :: bufr

  logical :: time_matched
  logical :: append_
  append_=.false.; if(present(append)) append_=append

  lu=luavail()
  open(lu,file=trim(dbase)//'/'//trim(from),status='old',iostat=ier)
    if(ier/=0) call die(myname_, &
      'open("'//trim(dbase)//'/'//trim(from)//'"), iostat =',ier)

  if(.not.append_) then
    call alloc(p_nm,n)
    call alloc(p_sn,n)
    call alloc(p_ch,n)
  endif

  nymd=where_dt(1)
  nhms=where_dt(2)

  call getarec(lu,bufr,ier)
  do while(ier==0)
    read(bufr,*,iostat=ier) nm,iymd,ihms,lymd,lhms,sn,m
      if(ier/=0) then
        call perr(myname_,'bufr="'//trim(bufr)//'"')
        call die(myname_, &
	  'stem-read("'//trim(dbase)//'/'//trim(from)//'"), iostat =',ier)
      endif

    l=locate(nm,where_nm(:))
    time_matched = nymd==-1
    if(.not.time_matched) time_matched = &
       (iymd<nymd .or. (iymd==nymd.and.ihms<=nhms)) .and. &
       (nymd<lymd .or. (nymd==lymd.and.nhms<=lhms))

    if((l>0) .and. time_matched) then

      call realloc(p_nm,n,incr=m)
      call realloc(p_sn,n,incr=m)
      call realloc(p_ch,n,incr=m)

      p_nm(n+1:n+m)=nm
      p_sn(n+1:n+m)=sn
      read(bufr,*,iostat=ier) nm,iymd,ihms,lymd,lhms,sn,k, &
                              p_ch(n+1:n+m),k
        if(ier==0) then
          call perr(myname_,'bufr="'//trim(bufr)//'"')
          call die(myname_, &
	    'leaf-read("'//trim(dbase)//'/'//trim(from)//'"), too many channels',m+1)
	endif

      read(bufr,*,iostat=ier) nm,iymd,ihms,lymd,lhms,sn,k, &
                              p_ch(n+1:n+m)
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

subroutine dealloc_(p_nm,p_sn,p_ch,n)
  implicit none
  character(len=*),pointer,dimension(:) :: p_nm
  character(len=*),pointer,dimension(:) :: p_sn
  integer         ,pointer,dimension(:) :: p_ch
  integer,intent(out) :: n

  deallocate(p_nm)
  deallocate(p_sn)
  deallocate(p_ch)
  n=-1
end subroutine dealloc_

subroutine show_(p_nm,p_sn,p_ch,n)
  implicit none
  character(len=*),pointer,dimension(:) :: p_nm
  character(len=*),pointer,dimension(:) :: p_sn
  integer         ,pointer,dimension(:) :: p_ch
  integer,intent(in) :: n
  integer :: i
  !!print*, myname,": n=",n,"; i,nm,sn,ch"
  do i=1,n
    print'(a20,i8.6)', trim(p_sn(i))//' '//trim(p_nm(i)),p_ch(i)
    !!print*, i,p_nm(i),p_sn(i),p_ch(i)
  end do
end subroutine show_
end module m_actvchan
