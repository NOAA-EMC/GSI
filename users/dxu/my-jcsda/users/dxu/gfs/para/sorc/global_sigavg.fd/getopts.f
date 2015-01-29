subroutine getopts(optionstring,name,optarg,optind,exitcode)
!$$$  Subprogram Documentation Block
!
! Subprogram:  Getopts    Process command line arguments for valid options
!   Prgmmr: Iredell       Org: W/NP23        Date: 2000-08-22
!
! Abstract: This subprogram processes command-line arguments for valid options.
!           It is the Fortran equivalent of the built-in shell command getopts.
!           Options on the command line come before the positional arguments.
!           Options are preceded by a - (minus sign) or a + (plus sign).
!           Options are single case-sensitive alphanumeric characters.
!           Options either do or do not have an expected argument.
!           Options without an argument may be immediately succeeded by
!           further options without the accompanying - or + prefix.
!           Options with an argument may be separated from their argument
!           by zero or more blanks.  The argument cannot end with a blank.
!           Options end when not preceded by a - or a + or after -- or ++.
!           This subprogram processes one option per invocation.
!           This subprogram is not thread-safe.
!
! Program History Log:
!   2000-08-22  Iredell
!
! Usage:    call getopts(optionstring,name,optarg,optind,exitcode)
!
!   Input Argument List:
!     optionstring
!       character string containing a list of all valid options;
!       options succeeded by a : require an argument
!
!   Input and Output Argument List:
!     optind
!       integer index of the next argument to be processed;
!       set to 0 before initial call or to restart processing
!
!   Output Argument List:
!     name
!       character string containing the name of the next option
!       or ? if no option or an unknown option is found
!       or : if an option had a missing required argument;
!       a + is prepended to the value in name if the option begins with a +
!     optarg
!       character string containing the option argument if required
!       or null if not required or not found;
!       optarg contains the option found if name is ? or :.
!     exitcode
!       integer return code (0 if an option was found, 1 if end of options)
!     
! Subprograms Called:
!   iargc
!     Retrieve number of command-line arguments
!   getarg
!     Retrieve a command-line argument
!   index 
!     Retrieve the starting position of a substring within a string
!     
! Remarks:
!   Here is an example of how to use this subprogram.
!     implicit none
!     character*8 copt,carg,cb,cpos
!     integer ia,ib,iopt,iret,narg,npos,ipos,iargc
!     ia=0
!     ib=0
!     iopt=0
!     do
!       call getopts('ab:',copt,carg,iopt,iret)
!       if(iret.ne.0) exit
!       select case(copt)
!       case('a','+a')
!         ia=1
!       case('b','+b')
!         ib=1
!         cb=carg
!       case('?',':')
!         print *,'invalid option ',carg(1:1)
!         stop 1
!       end select
!     enddo
!     if(ia.eq.1) print *,'option a selected'
!     if(ib.eq.1) print *,'option b selected; argument=',cb
!     narg=iargc()
!     npos=narg-iopt+1
!     do ipos=1,npos
!       call getarg(ipos+iopt-1,cpos)
!       print *,'positional argument ',ipos,' is ',cpos
!     enddo
!     end
!
! Attributes:
!   Language: Fortran 90
!
!$$$
  implicit none
!  Passed data
  character*(*),intent(in):: optionstring
  character*(*),intent(out):: name
  character*(*),intent(out):: optarg
  integer,intent(inout):: optind
  integer,intent(out):: exitcode
!  Saved data
  character*256,save:: carg
  character*1,save:: cone
  integer,save:: narg,larg,lcur
!  Local data
  character*1 copt
  integer iargc,lname,lopt
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Initially set saved data.
  if(optind.le.0) then
    optind=0
    narg=iargc()
    carg=''
    cone=''
    larg=0
    lcur=1
  endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Retrieve next command-line argument if necessary;
!  exit if at end of options
  if(lcur.gt.larg) then
    optind=optind+1
    if(optind.gt.narg) then
      name='?'
      optarg=''
      exitcode=1
      return
    endif
    call getarg(optind,carg)
    cone=carg(1:1)
    larg=len_trim(carg)
    lcur=2
    if(larg.eq.1.or.(cone.ne.'-'.and.cone.ne.'+')) then
      name='?'
      optarg=''
      exitcode=1
      return
    elseif(larg.eq.2.and.carg(2:2).eq.cone) then
      optind=optind+1
      name='?'
      optarg=''
      exitcode=1
      return
    endif
  endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Find next option in the list; exit if option is unknown
  exitcode=0
  copt=carg(lcur:lcur)
  lcur=lcur+1
  lopt=index(optionstring,copt)
  if(lopt.eq.0) then
    name='?'
    optarg=copt
    return
  endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Option found; retrieve its argument if requested
    if(cone.eq.'-') then
      name=""
      lname=1
    else
      name="+"
      lname=2
    endif
  name(lname:lname)=copt
  optarg=''
  if(lopt.lt.len(optionstring).and.optionstring(lopt+1:lopt+1).eq.':') then
    if(lcur.gt.larg) then
      optind=optind+1
      if(optind.gt.narg) then
        name=':'
        optarg=copt
        return
      endif
      call getarg(optind,carg)
      larg=len_trim(carg)
      lcur=1
    endif
    optarg=carg(lcur:larg)
    lcur=larg+1
  endif
end subroutine
