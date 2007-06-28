!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: trace.H - macros for code tracing.
!
! !DESCRIPTION:
!
!   This header file contains following six macros:
!
!	_ENTRY_ and _ALLENTRY_	: used at the entry point of a routine
!	_EXIT_ and _ALLEXIT_	: used at the exit of a routine
!	_TRACE_(s) and _ALLTRACE_(s)
!				: used with a string message s=""
!	_ALWAYS_TRACE_(s) and _ALWAYS_ALLTRACE_(s)
!				: are always defined, while other
!				: macros can be turned on only after
!				: -DDEBUG_TRACE is specified during
!				: preprocessing stage of compilation.
!
! where the second macros in all groups are define to do a MPI_Barrier()
! before they write out their messages.
!
! !INTERFACE:
!
!   The interface of this header file is better represented in the
! following example:
!
!   #include "trace.H"
!   use m_mpout,only : mpout_log
!   implicit none
!   #include "mpif.h"
!	! myname_ is required parameter for all macros.
!   character(len=*),parameter :: myname_='m_this::that'
!	! ier and comm are required parameters for macros
!	! _ALLENTRY_, _ALLEXIT_, and _ALLTRACE("")
!   integer :: ier	
!   integer :: comm
!   comm=MPI_COMM_WORLD		! or any other communicator
!				! if turned on (with -DDEBUG_TRACE),
!   _ENTRY_			! produces "m_this::that: entered";
!   _TRACE("checkpoint")	! produces "m_this::that: checkpoint";
!   _EXIT_			! produces "m_this::that: exiting".
!
! !REVISION HISTORY:
! 	16Feb05	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

	! These macros are always redefined, such that one may toggle
	! between turn them on or off.

#ifdef _ALLENTRY_
#undef _ALLENTRY_
#endif
#ifdef _ENTRY_
#undef _ENTRY_
#endif
#ifdef _ALLEXIT_
#undef _ALLEXIT_
#endif
#ifdef _EXIT_
#undef _EXIT_
#endif
#ifdef _ALLTRACE_
#undef _ALLTRACE_
#endif
#ifdef _TRACE_
#undef _TRACE_
#endif

	! NOw define all of them, if trace is turned on by -DDEBUG_TRACE
#ifdef DEBUG_TRACE

#define _ALLENTRY_ call MPI_Barrier(comm,ier);call mpout_log(myname_,"entered",flush=.true.)

#define _ALLEXIT_ call MPI_Barrier(comm,ier);call mpout_log(myname_,"exiting",flush=.true.)

#define _ALLTRACE_(EX) call MPI_Barrier(comm,ier);call mpout_log(myname_,EX,flush=.true.)

#define _ENTRY_	call mpout_log(myname_,"entered",flush=.true.)
#define _EXIT_ call mpout_log(myname_,"exiting",flush=.true.)
#define _TRACE_(EX) call mpout_log(myname_,EX,flush=.true.)

	! Or define them to null, if DEBUG_TRACE is not defined.
#else
#define _ALLENTRY_ 
#define _ALLEXIT_
#define _ALLTRACE_(EX)

#define _ENTRY_
#define _EXIT_
#define _TRACE_(EX)

#endif

	! Following two macros are always defined.
#ifndef _ALWAYS_TRACE_
#define _ALWAYS_TRACE_(EX) call mpout_log(myname_,EX,flush=.true.)
#endif

#ifndef _ALWAYS_ALLTRACE_
#define _ALWAYS_ALLTRACE_(EX) call MPI_Barrier(comm,ier);call mpout_log(myname_,EX,flush=.true.)
#endif
