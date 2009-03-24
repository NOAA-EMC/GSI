!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_satinst - Input module of satellite sensors table
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_satinst
      implicit none
      private	! except

      public :: satinst_select
      public :: satinst_clean
      public :: satinst_show

      interface satinst_select; module procedure &
        select_; end interface
      interface satinst_clean; module procedure &
        dealloc_; end interface
      interface satinst_show; module procedure &
        show_; end interface

! !REVISION HISTORY:
! 	04Jun07	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_satinst'

!!$ Usecase:
!!$
!!$   character(len=LEN_WORD),pointer,dimension(:) :: p_nm => null()
!!$   integer                ,pointer,dimension(:) :: p_id => null()
!!$   character(len=LEN_WORD),pointer,dimension(:) :: p_sn => null()
!!$   integer :: n
!!$   call satinst_select(p_nm,p_id,p_sn,n,from="satinst.tbl")
!!$   [.. p_nm(1:n),p_id(1:n),p_sn(1:n) ..]
!!$   call satinst_clean(p_nm,p_id,p_sn,n)
!!$

contains

subroutine select_(p_nm,p_sn,p_ch,n,where_nm,from,dbase,bufrsize,append)
  implicit none
  character(len=*),pointer,dimension(:) :: p_nm  ! satellite names
  character(len=*),pointer,dimension(:) :: p_sn  ! sensor names
  integer         ,pointer,dimension(:) :: p_ch  ! channel numbers
  integer,intent(inout) :: n
  character(len=*),dimension(:),intent(in) :: where_nm ! satellite name selections
  character(len=*),intent(in) :: from

  character(len=*),optional,intent(in) :: dbase    ! default=".".
  integer         ,optional,intent(in) :: bufrsize ! default=256(bytes)
  logical         ,optional,intent(in) :: append   ! default=.false.

  integer :: lbufr

  lbufr=256
  if(present(bufrsize)) lbufr=bufrsize
  if(present(dbase)) then
    call selectb_(p_nm,p_sn,p_ch,n,where_nm,from, &
      dbase=dbase,bufrsize=lbufr,append=append)
  else
    call selectb_(p_nm,p_sn,p_ch,n,where_nm,from, &
      dbase='.'  ,bufrsize=lbufr,append=append)
  endif
end subroutine select_

subroutine selectb_(p_nm,p_sn,p_ch,n,where_nm,from, &
                    dbase,bufrsize,append)
  use satinfo_util,only : perr,die
  use satinfo_util,only : luavail,getarec
  use satinfo_util,only : alloc,realloc
  use satinfo_util,only : locate
  implicit none
  character(len=*),pointer,dimension(:) :: p_nm  ! satellite names
  character(len=*),pointer,dimension(:) :: p_sn  ! sensor names
  integer         ,pointer,dimension(:) :: p_ch  ! channel numbers
  integer,intent(inout) :: n
  character(len=*),dimension(:),intent(in) :: where_nm ! satellite name selections
  character(len=*),intent(in) :: from

  character(len=*),intent(in) :: dbase
  integer         ,intent(in) :: bufrsize
  logical,optional,intent(in) :: append

  character(len=*),parameter :: myname_=myname//"::selectb_"

  integer :: lu,ier,j,k,l,m
  character(len=len(p_nm)) :: nm,sn
  character(len=bufrsize) :: bufr

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

  call getarec(lu,bufr,ier)
  do while(ier==0)
    read(bufr,*,iostat=ier) nm,sn,m
      if(ier/=0) then
        call perr(myname_,'bufr="'//trim(bufr)//'"')
        call die(myname_, &
	  'stem-read("'//trim(dbase)//'/'//trim(from)//'"), iostat =',ier)
      endif

    l=locate(nm,where_nm(:))
    if(l>0) then
      call realloc(p_nm,n,incr=m)
      call realloc(p_sn,n,incr=m)
      call realloc(p_ch,n,incr=m)

      p_nm(n+1:n+m)=nm
      p_sn(n+1:n+m)=sn
      read(bufr,*,iostat=ier) nm,sn,k,p_ch(n+1:n+m),k
        if(ier==0) then
          call perr(myname_,'bufr="'//trim(bufr)//'"')
          call die(myname_, &
	    'leaf-read("'//trim(dbase)//'/'//trim(from)//'"), too many channels',m+1)
        endif

      read(bufr,*,iostat=ier) nm,sn,k,p_ch(n+1:n+m)
        if(ier/=0) then
          call perr(myname_,'bufr="'//trim(bufr)//'"')
          call die(myname_, &
	    'leaf-read("'//trim(dbase)//'/'//trim(from)//'"), iostat =',ier)
        endif

      n=n+m
    endif
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
  !print*, myname,": n=",n,"; i,nm,sn,ch"
  do i=1,n
    print'(a20,i6.4)', trim(p_sn(i))//' '//trim(p_nm(i)),p_ch(i)
    !print'(i5,2(2x,a),i5)', i,p_nm(i),p_sn(i),p_ch(i)
  end do
end subroutine show_
end module m_satinst
