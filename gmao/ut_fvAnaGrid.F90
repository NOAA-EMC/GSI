!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !ROUTINE: ut_fvAnaGrid - a unit tester of m_fvAnaGrid
!
! !DESCRIPTION:
!
! !INTERFACE:
!#include "regime.H"

    program ut_fvAnaGrid
      use m_mpif90,only : MP_comm_world
      use m_mpif90,only : MP_init
      use m_mpif90,only : MP_finalize
      use m_mpif90,only : MP_comm_rank
      use m_die,only : MP_die
      use m_mpout,only : mpout

      use m_fvAnaGrid,only : fvAnaGrid_setup
      use m_fvAnaGrid,only : fvAnaGrid_read
      use m_fvAnaGrid,only : fvAnaGrid_write
      use m_utests,only : utests_init
      use m_utests,only : utests_clean
      implicit none

! !REVISION HISTORY:
! 	23Nov04	- Jing Guo <guo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='ut_fvAnaGrid'
  integer :: jiter
  integer :: nsig,jcap
  integer :: ier,myPE,root
  integer :: iyr,imo,idy,ihr

  call MP_init(ier)
  	if(ier/=0) call MP_die(myname,'MP_init()',ier)

  call MP_comm_rank(MP_comm_world,myPE,ier)
  	if(ier/=0) call MP_die(myname,'MP_comm_rank()',ier)

  nsig=28
  jcap=62
  root= 0
  call utests_init(nsig,jcap,root,MP_comm_world)

  call fvAnaGrid_setup(06,iyr,imo,idy,ihr,MP_comm_world)
  write(mpout,'(2a,2(i2.2,a),i4.4,a,i2.2,a)')	&
  	myname,': ',imo,'/',idy,'/',iyr,', ',ihr,':00:00'

  jiter=0
  call fvAnaGrid_read(jiter,myPE,MP_comm_world)
  call fvAnaGrid_write(myPE,MP_comm_world)

  call utests_clean()

  call MP_finalize(ier)
  	if(ier/=0) call MP_die(myname,'MP_finalize()',ier)

end program ut_fvAnaGrid
