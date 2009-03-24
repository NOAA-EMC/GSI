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
  use jfunc, only: noz,nq,nt,nsst,ncw,np,nst,nvp,&
       nclen,nuvlen,ntendlen,nu,nv,nut,nvt,ntt,nprst,&
       nqt,nozt,ncwt
  use gridmod, only: latlon1n,latlon11,nsig1o
  use gsi_4dvar, only: nsubwin
  use mod_vtrans,only: nvmodes_keep
  use state_vectors
  use control_vectors
  implicit none

! Declare passed variables
  type(state_vector),intent(inout)::sval(nsubwin)

! Declare local variables
  integer(i_kind) k,nnn,mm1,ii
  logical fullfield,tracer
  real(r_kind),dimension(ntendlen)::xhat_t
  real(r_kind),dimension(nclen)::xhat_x,xhat_y

!************************************************************************************  
! Initialize variable
  mm1=mype+1

!     compute derivatives
! Determine how many vertical levels each mpi task will
! handle in computing horizontal derivatives
  nnn=0
  do k=1,nsig1o
    if (levs_id(k)/=0) nnn=nnn+1
  end do

  do ii=1,nsubwin

   call get_derivatives( &
     sval(ii)%u,sval(ii)%v ,sval(ii)%t  ,sval(ii)%p ,  &
     sval(ii)%q,sval(ii)%oz,sval(ii)%sst,sval(ii)%cw, &
     xhat_x(nst),xhat_x(nvp),xhat_x(nt)  ,xhat_x(np),  &
     xhat_x(nq) ,xhat_x(noz),xhat_x(nsst),xhat_x(ncw), &
     xhat_y(nst),xhat_y(nvp),xhat_y(nt)  ,xhat_y(np),  &
     xhat_y(nq) ,xhat_y(noz),xhat_y(nsst),xhat_y(ncw), &
     nnn,mype,1)

   tracer=.false.
   call calctends_tl( &
     sval(ii)%u,sval(ii)%v ,sval(ii)%t   ,               &
     sval(ii)%q,sval(ii)%oz,sval(ii)%cw  ,               &
     xhat_x(nst),xhat_y(nst) ,xhat_x(nvp),xhat_y(nvp),   &
     xhat_x(nt) ,xhat_y(nt)  ,xhat_x(np) ,xhat_y(np),    &
     xhat_x(nq) ,xhat_y(nq)  ,xhat_x(noz),xhat_y(noz),   &
     xhat_x(ncw),xhat_y(ncw) ,mype,          &
     xhat_t(nut),xhat_t(nvt) ,xhat_t(ntt),xhat_t(nprst), &
     xhat_t(nqt),xhat_t(nozt),xhat_t(ncwt),sval(ii)%p3d,tracer)
   if(nvmodes_keep.gt.0) then
      fullfield=.false.
      call strong_bal_correction(xhat_t(nut),xhat_t(nvt),xhat_t(ntt),xhat_t(nprst),&
                  mype,sval(ii)%u,sval(ii)%v,sval(ii)%t,sval(ii)%p,.true.,fullfield,.false.)
   end if

  enddo

  return
end subroutine strong_baldiag_inc
