!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: mpeu_util - utilities
!
! !DESCRIPTION:
!   to be compiled with -Dsys`uname -s`
!
! !INTERFACE:

    module mpeu_util
      use kinds, only: i_kind,r_single,r_double
      use constants, only: izero,ione
      implicit none
      private	! except

      public :: die, perr, warn, tell, assert_
      public :: luavail
      public :: stdin, stdout, stderr
      public :: strTemplate

    integer(i_kind),parameter :: STDIN = 5_i_kind
    integer(i_kind),parameter :: STDOUT= 6_i_kind
#ifdef sysHP_UX
    integer(i_kind),parameter :: STDERR= 7_i_kind
#else
    integer(i_kind),parameter :: STDERR= izero
#endif
    interface luavail; module procedure luavail_; end interface

    interface die; module procedure &
      die_chr_, &
      die_int_, &
      die_flt_, &
      die_dbl_, &
      die2_,	&
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

! !REVISION HISTORY:
!	19Feb09	- Jing Guo <Jing.Guo@nasa.gov>
!		- Implemented for GSI to avoid the dependency on
!		  GMAO_mpeu/.
!		. Selected from mksi/satinfo_util.F90.
!		. Added StrTemplate() from m_StrTemplate.F90.  Format
!		  class is no limited to GrADS like.
!		. Some new %-keywords are added (%i, %j, %k, %l).
!		. Modified dropdead_() to use GSI abor1().
!		
! 	02May07	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname="mpeu_util"

  integer(i_kind),parameter :: MAX_LUNIT=1024_i_kind

contains

function luavail_() result(lu)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    luavail_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  integer(i_kind) :: lu

  character(len=*),parameter :: myname_=myname//'::luavail_'
  integer(i_kind) ios
  logical inuse

  lu=-ione
  ios=izero
  inuse=.true.

  do while(ios==izero.and.inuse)
    lu=lu+ione

	! Test #1, reserved units

    inuse = lu==stdout .or. lu==stdin .or. lu==stderr

#ifdef sysSunOS
	! Reserved units under SunOS
    inuse = lu==100_i_kind .or. lu==101_i_kind .or. lu==102_i_kind
#endif

	! Test #2, in-use

    if(.not.inuse) inquire(unit=lu,opened=inuse,iostat=ios)

    if(lu >= MAX_LUNIT) ios=-ione
  end do
  if(ios/=izero) lu=-ione
end function luavail_

subroutine perr_(who,what)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    perr_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
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
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    perr_chr_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what,val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
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
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    perr_int_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!    val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what
  integer(i_kind) ,intent(in) :: val
  write(stderr,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stderr,*) val
  if(stderr==stdout) return
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stdout,*) val
end subroutine perr_int_

subroutine perr_flt_(who,what,val)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    perr_flt_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!    val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what
  real(r_single)  ,intent(in) :: val
  write(stderr,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stderr,*) val
  if(stderr==stdout) return
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stdout,*) val
end subroutine perr_flt_

subroutine perr_dbl_(who,what,val)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    perr_dbl_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!    val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what
  real(r_double)  ,intent(in) :: val
  write(stderr,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stderr,*) val
  if(stderr==stdout) return
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> ERROR <<< ',trim(what),' '
  write(stdout,*) val
end subroutine perr_dbl_

subroutine warn_(who,what)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    warn_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> WARNING <<< ',trim(what),' '
  write(stdout,*)
end subroutine warn_

subroutine warn_chr_(who,what,val)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    warn_chr_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what,val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what,val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> WARNING <<< ',trim(what),' '
  write(stdout,'(1x,3a)') '"',trim(val),'"'
end subroutine warn_chr_

subroutine warn_int_(who,what,val)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    warn_int_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!    val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what
  integer(i_kind) ,intent(in) :: val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> WARNING <<< ',trim(what),' '
  write(stdout,*) val
end subroutine warn_int_

subroutine warn_flt_(who,what,val)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    warn_flt_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!    val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what
  real(r_single)  ,intent(in) :: val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> WARNING <<< ',trim(what),' '
  write(stdout,*) val
end subroutine warn_flt_

subroutine warn_dbl_(who,what,val)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    warn_dbl_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!    val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what
  real(r_double)  ,intent(in) :: val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): >>> WARNING <<< ',trim(what),' '
  write(stdout,*) val
end subroutine warn_dbl_

subroutine tell_(who,what)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tell_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): ',trim(what),' '
  write(stdout,*)
end subroutine tell_

subroutine tell_chr_(who,what,val)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tell_chr_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what,val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what,val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): ',trim(what),' '
  write(stdout,'(1x,3a)') '"',trim(val),'"'
end subroutine tell_chr_

subroutine tell_int_(who,what,val)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tell_int_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!    val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what
  integer(i_kind) ,intent(in) :: val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): ',trim(what),' '
  write(stdout,*) val
end subroutine tell_int_

subroutine tell_flt_(who,what,val)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tell_flt_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!    val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what
  real(r_single)  ,intent(in) :: val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): ',trim(what),' '
  write(stdout,*) val
end subroutine tell_flt_

subroutine tell_dbl_(who,what,val)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tell_dbl_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!    val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what
  real(r_double)  ,intent(in) :: val
  write(stdout,'(1x,4a)',advance='no') &
    trim(who),'(): ',trim(what),' '
  write(stdout,*) val
end subroutine tell_dbl_

subroutine dropdead_()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dropdead_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  	! this is a GSI_GridComp::abort()
  call abor1(myname//"dropdead_()")
!!  call exit(2)
end subroutine dropdead_

subroutine die_(who)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    die_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who
  call perr_(who,'presence of a fatal condition')
  call dropdead_()
end subroutine die_

subroutine die2_(who,what)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    die2_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what
  call perr_(who,what)
  call dropdead_()
end subroutine die2_

subroutine die_chr_(who,what,val)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    die_chr_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what,val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what,val
  call perr_chr_(who,what,val)
  call dropdead_()
end subroutine die_chr_

subroutine die_int_(who,what,val)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    die_int_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!    val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what
  integer(i_kind) ,intent(in) :: val
  call perr_int_(who,what,val)
  call dropdead_()
end subroutine die_int_

subroutine die_flt_(who,what,val)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    die_flt_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!    val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what
  real(r_single)  ,intent(in) :: val
  call perr_flt_(who,what,val)
  call dropdead_()
end subroutine die_flt_

subroutine die_dbl_(who,what,val)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    die_dbl_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    who,what
!    val
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: who,what
  real(r_double)  ,intent(in) :: val
  call perr_dbl_(who,what,val)
  call dropdead_()
end subroutine die_dbl_

subroutine assert_(str,from,line)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    assert_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    str
!    from
!    line
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in) :: str    ! a message of assert_()
  character(len=*),intent(in) :: from   ! where assert_() is invoked.
  integer(i_kind) ,intent(in) :: line   ! where assert_() is invoked.
  character(len=*),parameter :: myname_='ASSERT_'
  call perr_(myname_,'failed: "'//str//'"')
  call die(myname_,from,line)
end subroutine assert_

subroutine assert_GE_(m,n,who,str)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    assert_GE_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    m,n
!    who
!    str
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  integer(i_kind) ,intent(in) :: m,n
  character(len=*),intent(in) :: who    ! where assert_GE_() is invoked.
  character(len=*),intent(in) :: str    ! a message of assert_GE_()
  character(len=*),parameter :: myname_='ASSERT_GE_'
  if(.not.(m>=n)) then
    call perr(myname_,'failed: "'//str//'"')
    call perr(myname_,'operand 1 = ',m)
    call perr(myname_,'operand 2 = ',n)
    call die(myname_,who)
  endif
end subroutine assert_GE_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: strTemplate - expanding a format template to a string
!
! !DESCRIPTION:
!
!	A template resolver formatting a string with string, time, and
!   dimension variables.  The format descriptors are similar to those
!   used in the GrADS, with some extensions.
!
!	"${E}"	substitute environment variable ${E}
!	"%y4"	substitute with a 4 digit year
!	"%y2"	a 2 digit year
!	"%m1"	a 1 or 2 digit month
!	"%m2"	a 2 digit month
!	"%mc"	a 3 letter month in lower cases
!	"%Mc"	a 3 letter month with a leading letter in upper case
!	"%MC"	a 3 letter month in upper cases
!	"%d1"	a 1 or 2 digit day
!	"%d2"	a 2 digit day
!	"%h1"	a 1 or 2 digit hour
!	"%h2"	a 2 digit hour
!	"%h3"	a 3 digit hour (?)
!	"%n2"	a 2 digit minute
!	"%s"	a string variable
!	"%i"	dims(1) of dims=(/im,jm,km,lm/)
!	"%j"	dims(2) of dims=(/im,jm,km,lm/)
!	"%k"	dims(3) of dims=(/im,jm,km,lm/)
!	"%l"	dims(4) of dims=(/im,jm,km,lm/)
!	"%%"	a "%"
!
! !INTERFACE:

    subroutine strTemplate(str,tmpl,nymd,nhms,dims,xid,stat)
      !! use m_stdio, only : stderr
      !! use m_die,   only : die,perr
      implicit none

      character(len=*),intent(out) :: str	! the output

      character(len=*),intent(in ) :: tmpl	! a "format"

      integer(i_kind) ,intent(in ),optional :: nymd
                        ! yyyymmdd, substituting "%y4", "%y2", "%m1",
                        ! "%m2", "%mc", "%Mc', and "%MC"

      integer(i_kind) ,intent(in ),optional :: nhms
                        ! hhmmss, substituting "%h1", "%h2", "%h3",
                        ! and "%n2"

      integer(i_kind),dimension(:),intent(in ),optional :: dims
                        ! integers, substituing "%i", "%j", "%k", "%l"

      character(len=*),intent(in ),optional :: xid
                        ! a string substituting a "%s".  Trailing
                        ! spaces will be ignored

      integer(i_kind) ,intent(out),optional :: stat
                        ! error code

! !REVISION HISTORY:
! 	18Feb09	- Jing Guo <Jing.Guo@nasa.gov>
!		- implemented for GSI to cut the m_StrTemplate
!		  dependency on GMAO_mpeu.
!		- Extended the "%-keyword" with "%i", "%j", "%k", and
!		  "%l" to support dims=(/./) option.
! 	19Dec06	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- Merged changes between 1.1.2.6 and 1.1.2.9 to 1.2,
!		  including a fix at bug nymd==0 and environment
!		  variable ($env or ${env}) support if getenv() is
!		  available from the system.
! 	01Jun99	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::strTemplate'

  character(len=3),parameter,dimension(12) :: mon_lc =	(/  &
        'jan','feb','mar','apr','may','jun',	&
        'jul','aug','sep','oct','nov','dec' /)

  character(len=3),parameter,dimension(12) :: mon_wd =	(/  &
        'Jan','Feb','Mar','Apr','May','Jun',	&
        'Jul','Aug','Sep','Oct','Nov','Dec' /)

  character(len=3),parameter,dimension(12) :: mon_uc =	(/  &
        'JAN','FEB','MAR','APR','MAY','JUN',	&
        'JUL','AUG','SEP','OCT','NOV','DEC' /)


  integer(i_kind) :: iy4,iy2,imo,idy
  integer(i_kind) :: ihr,imn
  integer(i_kind) :: i,i1,i2,m,k
  integer(i_kind) :: ln_tmpl,ln_str
  integer(i_kind) :: istp,kstp
  integer(i_kind) :: ier

  character(len=1) :: c0,c1,c2
  character(len=8) :: sbuf
!________________________________________
! Determine iyr, imo, and idy
  iy4=-ione
  iy2=-ione
  imo=-ione
  idy=-ione
  if(present(nymd)) then
    if(nymd <= izero) then
      call perr(myname_,'nymd <= 0',nymd)
      if(.not.present(stat)) call die(myname_)
      stat=ione
      return
    endif

    i=nymd
    iy4=i/10000
    iy2=mod(iy4,100)
      i=mod(i,10000)
    imo=i/100
      i=mod(i,100)
    idy=i
  endif
!________________________________________
! Determine ihr and imn
  ihr=-ione
  imn=-ione
  if(present(nhms)) then
    if(nhms < izero) then
      call perr(myname_,'nhms < 0',nhms)
      if(.not.present(stat)) call die(myname_)
      stat=ione
      return
    endif

    i=nhms
    ihr=i/10000
      i=mod(i,10000)
    imn=i/100
  endif
!________________________________________

  ln_tmpl=len_trim(tmpl)   ! size of the format template
  ln_str =len(str)         ! size of the output string
!________________________________________

  if(present(stat)) stat=izero

  str=""

  i=izero; istp=ione
  k=ione ; kstp=ione

  do while( i+istp <= ln_tmpl )    ! A loop over all tokens in (tmpl)

    if(k>ln_Str) exit    ! truncate the output here.

    i=i+istp
    c0=tmpl(i:i)

    select case(c0)
    case ("$")
      call genv_(tmpl,ln_tmpl,i,istp,str,ln_str,k,ier)
      if(ier/=izero) then
        call perr(myname_,'genv_("'//tmpl(i:ln_tmpl)//'"',ier)
        if(.not.present(stat)) call die(myname_)
        stat=ione
        return
      endif

    case ("%")
    !________________________________________

      c1=""
      i1=i+ione
      if(i1 <= ln_Tmpl) c1=tmpl(i1:i1)
      !________________________________________

      select case(c1)

      case("s")
        if(.not.present(xid)) then
          write(stderr,'(2a)') myname_,	&
                  ': optional argument expected, "xid="'
          if(.not.present(stat)) call die(myname_)
          stat=ione
          return
        endif

        istp=2_i_kind
        m=min(k+len_trim(xid)-ione,ln_str)
        str(k:m)=xid
        k=m+ione
        cycle

      case("i":"l")    ! from "i" to "l", (i,j,k,l)
        if(.not.present(dims)) then
          write(stderr,'(2a)') myname_,	&
                  ': optional argument expected, "dims=(/./)"'
          if(.not.present(stat)) call die(myname_)
          stat=ione
          return
        endif

        m=ichar(c1)-ichar("i")+ione    ! m=1,2,3,4 for i,j,k,l

        if(m>size(dims)) then
          write(stderr,'(2a)') myname_,	&
                  ': additional "dims=(/./)" element expected'
          write(stderr,'(2a,i4)') myname_,': size(dims) = ',size(dims)
          write(stderr,'(2a,2a)') myname_,': %-keyword  = "%',c1,'"'
          if(.not.present(stat)) call die(myname_)
          stat=ione
          return
        endif
      	! If m<size(dims), any extra dims elements are ignored

        write(sbuf,'(i8)') dims(m)
        sbuf=adjustl(sbuf)    ! adjust left all digits

        istp=2_i_kind         ! size of the "%" keyword

        m=min(k+len_trim(sbuf)-ione,ln_str)
        str(k:m)=sbuf
        k=m+ione
        cycle

      case("%","$")

        istp=2_i_kind
        str(k:k)=c1
        k=k+ione    ! kstp=ione
        cycle

      case default

        c2=""
        i2=i+2_i_kind
        if(i2 <= ln_Tmpl) c2=tmpl(i2:i2)
        !________________________________________

      select case(c1//c2)

      case("y4","y2","m1","m2","mc","Mc","MC","d1","d2")
        if(.not.present(nymd)) then
          write(stderr,'(2a)') myname_,	&
                  ': optional argument expected, "nymd="'
          if(.not.present(stat)) call die(myname_)
          stat=ione
          return
        endif
        istp=3_i_kind

      case("h1","h2","h3","n2")
        if(.not.present(nhms)) then
          write(stderr,'(2a)') myname_,	&
                  ': optional argument expected, "nhms="'
          if(.not.present(stat)) call die(myname_)
          stat=ione
          return
        endif
        istp=3_i_kind

      case default

        write(stderr,'(4a)') myname_,	&
                ': invalid template entry, "',trim(tmpl(i:)),'"'
        if(.not.present(stat)) call die(myname_)
        stat=2_i_kind
        return

      end select	  ! case(c1//c2)
    end select		! case(c1)
    !________________________________________

    select case(c1)

    case("y")
      select case(c2)
      case("2")
        write(sbuf,'(i2.2)') iy2
        kstp=2_i_kind
      case("4")
        write(sbuf,'(i4.4)') iy4
        kstp=4_i_kind
      case default
        write(stderr,'(4a)') myname_,	&
                ': invalid template entry, "',trim(tmpl(i:)),'"'
        if(.not.present(stat)) call die(myname_)
        stat=2_i_kind
        return
      end select

    case("m")
      select case(c2)
      case("1")
        if(imo < 10_i_kind) then
          write(sbuf,'(i1)') imo
          kstp=ione
        else
          write(sbuf,'(i2)') imo
          kstp=2_i_kind
        endif
      case("2")
        write(sbuf,'(i2.2)') imo
        kstp=2_i_kind
      case("c")
        sbuf=mon_lc(imo)
        kstp=3_i_kind
      case default
        write(stderr,'(4a)') myname_,	&
                ': invalid template entry, "',trim(tmpl(i:)),'"'
        if(.not.present(stat)) call die(myname_)
        stat=2_i_kind
        return
      end select

    case("M")
      select case(c2)
      case("c")
        sbuf=mon_wd(imo)
        kstp=3_i_kind
      case("C")
        sbuf=mon_uc(imo)
        kstp=3_i_kind
      case default
        write(stderr,'(4a)') myname_,	&
                ': invalid template entry, "',trim(tmpl(i:)),'"'
        if(.not.present(stat)) call die(myname_)
        stat=2_i_kind
        return
      end select

    case("d")
      select case(c2)
      case("1")
        if(idy < 10_i_kind) then
          write(sbuf,'(i1)') idy
          kstp=ione
        else
          write(sbuf,'(i2)') idy
          kstp=2_i_kind
        endif
      case("2")
        write(sbuf,'(i2.2)') idy
        kstp=2_i_kind
      case default
        write(stderr,'(4a)') myname_,	&
                ': invalid template entry, "',trim(tmpl(i:)),'"'
        if(.not.present(stat)) call die(myname_)
        stat=2_i_kind
        return
      end select

    case("h")
      select case(c2)
      case("1")
        if(ihr < 10_i_kind) then
        write(sbuf,'(i1)') ihr
        kstp=ione
      else
        write(sbuf,'(i2)') ihr
        kstp=2_i_kind
      endif
      case("2")
        write(sbuf,'(i2.2)') ihr
        kstp=2_i_kind
      case("3")
        write(sbuf,'(i3.3)') ihr
        kstp=3_i_kind
      case default
        write(stderr,'(4a)') myname_,	&
                ': invalid template entry, "',trim(tmpl(i:)),'"'
        if(.not.present(stat)) call die(myname_)
        stat=2_i_kind
        return
      end select

    case("n")
      select case(c2)
      case("2")
        write(sbuf,'(i2.2)') imn
        kstp=2_i_kind
      case default
        write(stderr,'(4a)') myname_,	&
                ': invalid template entry, "',trim(tmpl(i:)),'"'
        if(.not.present(stat)) call die(myname_)
        stat=2_i_kind
        return
      end select

    case default
      write(stderr,'(4a)') myname_,	&
              ': invalid template entry, "',trim(tmpl(i:)),'"'
      if(.not.present(stat)) call die(myname_)
      stat=2_i_kind
      return
    end select	! case(c1)

    m=min(k+kstp-ione,ln_Str)
    str(k:m)=sbuf
    k=m+ione

  case default

    istp=ione
    str(k:k)=tmpl(i:i)
    k=k+ione

  end select	! case(c0)
end do

contains

subroutine genv_(tmpl,lnt,i,istp,str,lns,k,ier)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genv_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    tmpl
!    lnt
!    i
!    str
!    lns
!    k
!
!   output argument list:
!    istp
!    str
!    k
!    ier
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  character(len=*),intent(in   ) :: tmpl
  integer(i_kind) ,intent(in   ) :: lnt
  integer(i_kind) ,intent(in   ) :: i
  integer(i_kind) ,intent(  out) :: istp
  character(len=*),intent(inout) :: str
  integer(i_kind) ,intent(in   ) :: lns
  integer(i_kind) ,intent(inout) :: k
  integer(i_kind) ,intent(  out) :: ier

  integer(i_kind) :: j,jb,je
  integer(i_kind) :: l,m
  logical :: bracket,more
  character(len=256) :: env

  j=i+ione    ! skip "$"
  ier=izero

  if(j>lnt) then
    ier=ione
    return
  endif

  bracket = tmpl(j:j)=='{'
  if(bracket) j=j+ione

! There is at least one a letter (including "_") to start a
! variable name

  select case(tmpl(j:j))
  case ("A":"Z","a":"z","_")
  case default
    ier=2_i_kind
    return
  end select

  jb=j
  je=j

  if(bracket) then

    more=.true.
    do while(more)
      select case(tmpl(j:j))
      case ("A":"Z","a":"z","_","0":"9")
	je=j
        j=j+ione
      case ("}")	! End if "}" or eos
        j=j+ione
        exit
      case default
        ier=3_i_kind
        return
      end select
      more=j<=lnt
    enddo

  else

    more=.true.
    do while(more)
      select case(tmpl(j:j))
      case ("A":"Z","a":"z","_","0":"9")
        je=j
	j=j+ione
      case default
        exit
      end select
      more=j<=lnt
    enddo
  endif

  istp=j-i

  call getenv(tmpl(jb:je),env)
  l=len_trim(env)
  m=min(k+l-ione,lns)
  str(k:m)=env
  k=m+ione

end subroutine genv_
end subroutine strTemplate
end module mpeu_util
