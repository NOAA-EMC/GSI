module m_gsiversion
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_gsiversion
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2016-02-26
!
! abstract: - an interface to report GSI version
!
! program history log:
!   2016-02-26  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

  implicit none
  private	! except
  public:: gsiversion_tell
  interface gsiversion_tell; module procedure tell_; end interface

#define EMC_TRUNK_VERSION "ncep-gsi-r#####"
#ifdef GSI_VERSION
#define _MYVERSION_ GSI_VERSION
#else
#define _MYVERSION_ EMC_TRUNK_VERSION
#endif

  character(len=*),parameter:: myVERSION   = _MYVERSION_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='m_gsiversion'

contains
subroutine tell_(root,comm)
  use mpeu_mpif, only: MPI_ikind
  use mpeu_mpif, only: MPI_comm_world
  use mpeu_util, only: die,warn,tell
  integer(kind=MPI_ikind),optional,intent(in):: root
  integer(kind=MPI_ikind),optional,intent(in):: comm

  integer(kind=MPI_ikind):: myComm_,myRoot_
  integer(kind=MPI_ikind):: myPE,ier
  logical:: initialized_

  myRoot_=0
  if(present(root)) myRoot_=root
  myComm_=MPI_comm_world
  if(present(comm)) myComm_=comm

  myPE=myRoot_
  call MPI_initialized(initialized_,ier)
  if(ier/=0) then
    call warn(myname,'MPI_initialized() error, ierror =',ier)
  elseif(initialized_) then
    call MPI_comm_rank(myComm_,myPE,ier)
        if(ier/=0) call die(myname,'MPI_comm_rank() error, ierror =',ier)
  else
    call warn(myname,'.not. MPI_initialized()')
  endif

  if(myPE==myRoot_) call tell(myname,' =',myVERSION)
end subroutine tell_

end module m_gsiversion
