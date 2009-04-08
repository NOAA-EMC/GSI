subroutine strong_baldiag_inc(sval)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    strong_baldiag_inc    get balance diagnostics
!   prgmmr: parrish          org: np23                date: 2006-08-12
!
! abstract: get balance diagnostic statistics of increment
!
! program history log:
!   2006-08-12  parrish
!   2007-04-16  kleist   - modified to be used for diagnostics only
!   2007-07-26 cucurull  - call getprs; add xhat3dp and remove ps in calctends_tl argument list
!   2007-08-08  derber - only calculate dynamics time derivatives
!   2008-04-09  safford  - rm unused vars and uses
!   2009-01-17  todling  - per early changes from Tremolet (revisited)
!
!   input argument list:
!     sval    - current solution in state space
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: levs_id,mype
  use gridmod, only: latlon1n,latlon11,nnnn1o
  use gsi_4dvar, only: nsubwin
  use mod_vtrans,only: nvmodes_keep
  use state_vectors
  use control_vectors
  use constants, only: zero
  implicit none

! Declare passed variables
  type(state_vector),intent(inout)::sval(nsubwin)

! Declare local variables
  integer(i_kind) k,ii
  logical fullfield,tracer
  type(state_vector) dhat_dt

!************************************************************************************  
! Initialize variable

!     compute derivatives
! Determine how many vertical levels each mpi task will
! handle in computing horizontal derivatives
  call allocate_state(dhat_dt)
  call assign_scalar2state(dhat_dt,zero)

  do ii=1,nsubwin

   call calctends_tl( &
     sval(ii)%u,sval(ii)%v ,sval(ii)%t   ,               &
     sval(ii)%q,sval(ii)%oz,sval(ii)%cw  ,               &
     mype, nnnn1o,          &
     dhat_dt%u,dhat_dt%v ,dhat_dt%t,dhat_dt%p3d, &
     dhat_dt%q,dhat_dt%oz,dhat_dt%cw,sval(ii)%p3d)
   if(nvmodes_keep.gt.0) then
      fullfield=.false.
      call strong_bal_correction(dhat_dt%u,dhat_dt%v,dhat_dt%t,dhat_dt%p3d,&
                  mype,sval(ii)%u,sval(ii)%v,sval(ii)%t,sval(ii)%p,.true.,fullfield,.false.)
   end if

  enddo

  return
end subroutine strong_baldiag_inc
