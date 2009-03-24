!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: satinfo_util - utilities
!
! !DESCRIPTION:
!   to be compiled with -Dsys`uname -s`
!
! !INTERFACE:

    module satinfo_util
      implicit none
      private	! except

      public :: die, perr, warn, tell, assert_
      public :: luavail
      public :: stdin, stdout, stderr
      public :: alloc, realloc, dealloc
      public :: getarec
      public :: locate

    integer,parameter :: STDIN = 5
    integer,parameter :: STDOUT= 6
#ifdef sysHP_UX
    integer,parameter :: STDERR= 7
#else
    integer,parameter :: STDERR= 0
#endif
    interface luavail; module procedure luavail_; end interface

    interface die; module procedure &
      die_chr_, &
      die_int_, &
      die_flt_, &
      die_dbl_, &
      die_; end interface
    interface perr; module procedure &
      perr_chr_, &
      perr_int_, &
      perr_flt_, &
      perr_dbl_, &
      perr_; end interface
    interface warn; module procedure &
      warn_chr_, &
      warn_int_, &
      warn_flt_, &
      warn_dbl_, &
      warn_; end interface
    interface tell; module procedure &
      tell_chr_, &
      tell_int_, &
      tell_flt_, &
      tell_dbl_, &
      tell_; end interface

    interface alloc; module procedure &
      flt_alloc_, &
      dbl_alloc_, &
      int_alloc_, &
      chr_alloc_; end interface
    interface realloc; module procedure &
      flt_realloc_, &
      dbl_realloc_, &
      int_realloc_, &
      chr_realloc_; end interface
    interface dealloc; module procedure &
      flt_dealloc_, &
      dbl_dealloc_, &
      int_dealloc_, &
      chr_dealloc_; end interface

    interface getarec; module procedure getarec_; end interface
    interface locate; module procedure &
      int_locate_, &
      chr_locate_; end interface

! !REVISION HISTORY:
! 	02May07	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname="satinfo_util"

  real*4,parameter :: R4=1.E+0
  real*8,parameter :: R8=1.D+0
  integer,parameter :: SP=kind(R4)
  integer,parameter :: DP=kind(R8)

  integer,parameter :: PAGESIZE_=64
  integer,parameter :: MAX_LUNIT=1024

#include "assert.H"

!!$ Usecases:
!!$
!!$ Usecase 1: store data into a reallocatable array.
!!$
!!$	use satinfo_util,only : alloc,realloc,dealloc
!!$	integer,pointer,dimensioni(:) :: iptr =>null()
!!$	integer :: ilen,itmp,ierr
!!$	call alloc(iptr,ilen)
!!$	read(*,*,iostat=ierr) itmp
!!$	do while(ierr==0)
!!$	  ![.. can iptr(:) store one more?  If not, realloc it ..]
!!$	  call realloc(iptr,ilen,incr=1)
!!$	  ilen=ilen+1
!!$	  iptr(ilen)=itmp
!!$	  read(*,*,iostat=ierr) itmp
!!$	end do
!!$	![.. array iptr(1:ilen) contains all values of itmp ..]
!!$	call dealloc(iptr,ilen)
!!$
!!$ Usecase 2: get a table record from an ascii file containing comments
!!$
!!$	use satinfo_util,only: getarec
!!$	character(len=MXRECLEN) :: arec
!!$	character(len=32) :: t
!!$	character(len=32),pointer,dimension(:) :: tags=>null()
!!$	call alloc(tags,ntag)	! initialize tags
!!$	call getarec(5,arec,ier)
!!$	do while(ier==0)
!!$	  read(arec,*,iostat=ier) t
!!$	    if(ier/=0) [...]
!!$	    call realloc(tags,ntag,incr=1)
!!$	    tags(ntag)=t
!!$	  call getarec(5,arec,ier)
!!$	enddo
!!$	[.. tags(1:ntag) is a list of tags ..]
!!$	call dealloc(tags,ntag)

contains
subroutine int_alloc_(ptr,nptr,nominal,page)
    ! allocate an INTEGER pointer(:), which is assumed null()
  implicit none
  integer,pointer,dimension(:) :: ptr    ! assumed null()
  integer,intent(out) :: nptr            ! content size, set to 0
  integer,optional,intent(in) :: nominal ! nominal minimum size
  integer,optional,intent(in) :: page    ! block size of this alloc.

  integer :: psz,msz

  psz=PAGESIZE_
  if(present(page)) psz=max(page,PAGESIZE_)
  msz=psz
  if(present(nominal)) msz=max(1,(nominal+psz-1)/psz)*psz
  allocate(ptr(msz))
  nptr=0
end subroutine int_alloc_

subroutine int_realloc_(ptr,nptr,incr,page)
    ! reallocate an INTEGER pointer(:), which is associated().
  implicit none
  integer,pointer,dimension(:) :: ptr ! assumed assoicated()
  integer,intent(in) :: nptr          ! content size, unchanged
  integer,intent(in) :: incr          ! required size increment
  integer,optional,intent(in) :: page ! block size of this increase.

  integer,pointer,dimension(:) :: tmp
  integer :: msz,psz

  msz=size(ptr)
    ASSERT(msz>=nptr)
  if( nptr+incr <= msz) return

  psz=PAGESIZE_
  if(present(page)) psz=page

  msz=msz+max(0,((incr+psz-1)/psz)*psz)
  tmp => ptr
  allocate(ptr(msz))
  ptr(1:nptr)=tmp(1:nptr)
  deallocate(tmp)
end subroutine int_realloc_

subroutine int_dealloc_(ptr,nptr)
    ! deallocate an INTEGER pointer(:).
  implicit none
  integer,pointer,dimension(:) :: ptr ! assumed associated()
  integer,intent(out) :: nptr         ! content size, set to -1
  deallocate(ptr)
  nptr=-1
end subroutine int_dealloc_

subroutine chr_alloc_(ptr,nptr,nominal,page)
    ! allocate a CHARACTER(LEN=*) pointer(:), assumed null().
  implicit none
  character(len=*),pointer,dimension(:) :: ptr ! assumed null()
  integer,intent(out) :: nptr                  ! content size, set to 0
  integer,optional,intent(in) :: nominal ! nominal minimum size
  integer,optional,intent(in) :: page    ! block size of this alloc.

  integer :: psz,msz

  psz=PAGESIZE_
  if(present(page)) psz=max(page,PAGESIZE_)
  msz=psz
  if(present(nominal)) msz=max(1,(nominal+psz-1)/psz)*psz
  allocate(ptr(msz))
  nptr=0
end subroutine chr_alloc_

subroutine chr_realloc_(ptr,nptr,incr,page)
    ! reallocate a CHARACTER(LEN=*) pointer(:), assumed associated().
  implicit none
  character(len=*),pointer,dimension(:) :: ptr ! assumed associated()
  integer,intent(in) :: nptr          ! content size, unchanged
  integer,intent(in) :: incr          ! required size increment
  integer,optional,intent(in) :: page ! block size of this increase.

  character(len=len(ptr)),pointer,dimension(:) :: tmp
  integer :: msz,psz

  msz=size(ptr)
    ASSERT(msz>=nptr)
  if( nptr+incr <= msz) return

  psz=PAGESIZE_
  if(present(page)) psz=page

  msz=msz+max(0,((incr+psz-1)/psz)*psz)
  tmp => ptr
  allocate(ptr(msz))
  ptr(1:nptr)=tmp(1:nptr)
  deallocate(tmp)
end subroutine chr_realloc_

subroutine chr_dealloc_(ptr,nptr)
    ! deallocate a CHARACTER(LEN=*) pointer(:)
  implicit none
  character(len=*),pointer,dimension(:) :: ptr ! assumed associated()
  integer,intent(out) :: nptr                  ! content size, set to -1
  deallocate(ptr)
  nptr=-1
end subroutine chr_dealloc_

subroutine flt_alloc_(ptr,nptr,nominal,page)
    ! allocate a REAL pointer(:), assumed null().
  implicit none
  real(SP),pointer,dimension(:) :: ptr       ! assumed null()
  integer,intent(out) :: nptr            ! content size, set to 0
  integer,optional,intent(in) :: nominal ! nominal minimum size
  integer,optional,intent(in) :: page    ! block size of this alloc.

  integer :: psz,msz

  psz=PAGESIZE_
  if(present(page)) psz=max(page,PAGESIZE_)
  msz=psz
  if(present(nominal)) msz=max(1,(nominal+psz-1)/psz)*psz
  allocate(ptr(msz))
  nptr=0
end subroutine flt_alloc_

subroutine flt_realloc_(ptr,nptr,incr,page)
    ! reallocate a REAL pointer(:), assumed associated().
  implicit none
  real(SP),pointer,dimension(:) :: ptr    ! assumed associated()
  integer,intent(in) :: nptr          ! content size, unchanged
  integer,intent(in) :: incr          ! required size increment
  integer,optional,intent(in) :: page ! block size of this increase.

  real(kind(ptr)),pointer,dimension(:) :: tmp
  integer :: msz,psz

  msz=size(ptr)
    ASSERT(msz>=nptr)
  if( nptr+incr <= msz) return

  psz=PAGESIZE_
  if(present(page)) psz=page

  msz=msz+max(0,((incr+psz-1)/psz)*psz)
  tmp => ptr
  allocate(ptr(msz))
  ptr(1:nptr)=tmp(1:nptr)
  deallocate(tmp)
end subroutine flt_realloc_

subroutine flt_dealloc_(ptr,nptr)
    ! deallocate a REAL pointer(:).
  implicit none
  real(SP),pointer,dimension(:) :: ptr    ! assumed associated()
  integer,intent(out) :: nptr         ! content size, set to -1
  deallocate(ptr)
  nptr=-1
end subroutine flt_dealloc_

subroutine dbl_alloc_(ptr,nptr,nominal,page)
    ! allocate a DOUBLE PRECISION pointer(:), assumed null().
  implicit none
  real(DP),pointer,dimension(:) :: ptr ! assumed null()
  integer,intent(out) :: nptr            ! content size, set to 0
  integer,optional,intent(in) :: nominal ! nominal minimum size
  integer,optional,intent(in) :: page    ! block size of this alloc.

  integer :: psz,msz

  psz=PAGESIZE_
  if(present(page)) psz=max(page,PAGESIZE_)
  msz=psz
  if(present(nominal)) msz=max(1,(nominal+psz-1)/psz)*psz
  allocate(ptr(msz))
  nptr=0
end subroutine dbl_alloc_

subroutine dbl_realloc_(ptr,nptr,incr,page)
    ! reallocate a DOUBLE PRECISION pointer(:), assumed assoicated().
  implicit none
  real(DP),pointer,dimension(:) :: ptr ! assumed associated()
  integer,intent(in) :: nptr          ! content size, unchanged
  integer,intent(in) :: incr          ! required size increment
  integer,optional,intent(in) :: page ! block size of this increase.

  real(kind(ptr)),pointer,dimension(:) :: tmp
  integer :: msz,psz

  msz=size(ptr)
    ASSERT(msz>=nptr)
  if( nptr+incr <= msz) return

  psz=PAGESIZE_
  if(present(page)) psz=page

  msz=msz+max(0,((incr+psz-1)/psz)*psz)
  tmp => ptr
  allocate(ptr(msz))
  ptr(1:nptr)=tmp(1:nptr)
  deallocate(tmp)
end subroutine dbl_realloc_

subroutine dbl_dealloc_(ptr,nptr)
    ! deallocate a DOUBLE PRECISION  pointer(:).
  implicit none
  real(DP),pointer,dimension(:) :: ptr ! assumed assoicated().
  integer,intent(out) :: nptr         ! content size, set to -1
  deallocate(ptr)
  nptr=-1
end subroutine dbl_dealloc_

subroutine getarec_(lu,line,ier,nrec,commchar)
  implicit none
  integer,intent(in) :: lu
  character(len=*),intent(out) :: line
  integer,intent(out) :: ier
  integer,optional,intent(out) :: nrec ! count of record readings
  character(len=*),optional,intent(in) :: commchar ! set of comment chars

character(len=1) :: c
character(len=*),parameter :: SPC=achar(32),TAB=achar(09)
character(len=*),parameter :: NUL=achar(00),COM='#'

if(present(nrec)) nrec=0

    ! Read records until a line of non-blank and any non-comment text.
    ! A pure commont text record is a record with non-block content
    ! lead by a "#".
  read(lu,'(a)',iostat=ier) line
  do while(ier==0)
    if(present(nrec)) nrec=nrec+1
    c=leadchar_(line)
    if(present(commchar)) then
      if(c/=SPC .and. c/=TAB .and. index(commchar,c)/=1) exit
    else
      if(c/=SPC .and. c/=TAB .and. c/=COM) exit
    endif
    read(lu,'(a)',iostat=ier) line
  enddo
contains
function leadchar_(line) result(c)
  implicit none
  character(len=*),intent(in) :: line
  character(len=1) :: c
  integer :: i,l
  i=0
  l=len(line)
  c=SPC
  do while(i<l)
    i=i+1
    if(line(i:i)==SPC .or. line(i:i)==TAB) cycle
    c=line(i:i)
    return
  enddo
end function leadchar_
end subroutine getarec_

function int_locate_(key,list) result(l)
  implicit none
  integer,intent(in) :: key
  integer,dimension(:),intent(in) :: list
  integer :: l
  integer :: i
  l=0
  do i=1,size(list)
    if(key/=list(i)) cycle
    l=i
    return
  enddo
end function int_locate_
function chr_locate_(key,list) result(l)
  implicit none
  character(len=*),intent(in) :: key
  character(len=*),dimension(:),intent(in) :: list
  integer :: l
  integer :: i
  l=0
  do i=1,size(list)
    if(key/=list(i)) cycle
    l=i
    return
  enddo
end function chr_locate_
function luavail_() result(lu)
  implicit none
  integer :: lu

  character(len=*),parameter :: myname_=myname//'::luavail_'
  integer ios
  logical inuse
  character*8 attr

  lu=-1
  ios=0
  inuse=.true.

  do while(ios.eq.0.and.inuse)
    lu=lu+1

	! Test #1, reserved units

    inuse = lu.eq.stdout .or. lu.eq.stdin .or. lu.eq.stderr

#ifdef sysSunOS
	! Reserved units under SunOS
    inuse = lu.eq.100 .or. lu.eq.101 .or. lu.eq.102
#endif

	! Test #2, in-use

    if(.not.inuse) inquire(unit=lu,opened=inuse,iostat=ios)

    if(lu >= MAX_LUNIT) ios=-1
  end do
  if(ios.ne.0) lu=-1
end function luavail_

subroutine perr_(who,what)
  implicit none
  character(len=*),intent(in) :: who,what
  write(stderr,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stderr,*)
  if(stderr==stdout) return
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stdout,*)
end subroutine perr_
subroutine perr_chr_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what,val
  write(stderr,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stderr,'(1x,3a)') '"',trim(val),'"'
  if(stderr==stdout) return
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stdout,'(1x,3a)') '"',trim(val),'"'
end subroutine perr_chr_
subroutine perr_int_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  integer,intent(in) :: val
  write(stderr,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stderr,*) val
  if(stderr==stdout) return
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stdout,*) val
end subroutine perr_int_
subroutine perr_flt_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(SP),intent(in) :: val
  write(stderr,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stderr,*) val
  if(stderr==stdout) return
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stdout,*) val
end subroutine perr_flt_
subroutine perr_dbl_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(DP),intent(in) :: val
  write(stderr,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stderr,*) val
  if(stderr==stdout) return
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stdout,*) val
end subroutine perr_dbl_

subroutine warn_(who,what)
  implicit none
  character(len=*),intent(in) :: who,what
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> WARNING <<< ',trim(what),' '
  write(stdout,*)
end subroutine warn_
subroutine warn_chr_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what,val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> WARNING <<< ',trim(what),' '
  write(stdout,'(1x,3a)') '"',trim(val),'"'
end subroutine warn_chr_
subroutine warn_int_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  integer,intent(in) :: val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> WARNING <<< ',trim(what),' '
  write(stdout,*) val
end subroutine warn_int_
subroutine warn_flt_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(SP),intent(in) :: val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> WARNING <<< ',trim(what),' '
  write(stdout,*) val
end subroutine warn_flt_
subroutine warn_dbl_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(DP),intent(in) :: val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> WARNING <<< ',trim(what),' '
  write(stdout,*) val
end subroutine warn_dbl_

subroutine tell_(who,what)
  implicit none
  character(len=*),intent(in) :: who,what
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): ',trim(what),' '
  write(stdout,*)
end subroutine tell_
subroutine tell_chr_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what,val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): ',trim(what),' '
  write(stdout,'(1x,3a)') '"',trim(val),'"'
end subroutine tell_chr_
subroutine tell_int_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  integer,intent(in) :: val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): ',trim(what),' '
  write(stdout,*) val
end subroutine tell_int_
subroutine tell_flt_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(SP),intent(in) :: val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): ',trim(what),' '
  write(stdout,*) val
end subroutine tell_flt_
subroutine tell_dbl_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(DP),intent(in) :: val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): ',trim(what),' '
  write(stdout,*) val
end subroutine tell_dbl_

subroutine dropdead_()
  implicit none
  call exit(2)
end subroutine dropdead_
subroutine die_(who,what)
  implicit none
  character(len=*),intent(in) :: who,what
  call perr_(who,what)
  call dropdead_()
end subroutine die_
subroutine die_chr_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what,val
  call perr_chr_(who,what,val)
  call dropdead_()
end subroutine die_chr_
subroutine die_int_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  integer,intent(in) :: val
  call perr_int_(who,what,val)
  call dropdead_()
end subroutine die_int_
subroutine die_flt_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(SP),intent(in) :: val
  call perr_flt_(who,what,val)
  call dropdead_()
end subroutine die_flt_
subroutine die_dbl_(who,what,val)
  implicit none
  character(len=*),intent(in) :: who,what
  real(DP),intent(in) :: val
  call perr_dbl_(who,what,val)
  call dropdead_()
end subroutine die_dbl_
subroutine assert_(str,from,line)
  implicit none
  character(len=*),intent(in) :: str	! a message of assert_()
  character(len=*),intent(in) :: from	! where assert_() is invoked.
  integer,         intent(in) :: line	! where assert_() is invoked.
  character(len=*),parameter :: myname_='ASSERT_'
  call perr_(myname_,'failed: "'//str//'"')
  call die(myname_,from,line)
end subroutine assert_
end module satinfo_util
