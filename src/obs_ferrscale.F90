module obs_ferrscale
!
! abstract: Contains variables and routines for computation of
!           forecast sensitivity to observations.
!
! program history log:
!   2008-11-17 todling 
!
! Subroutines Included:
!   init_ferr_scale  - Initialize parameters
!   get_ferr_scale   - Read in forecast error vector
!   hrm1h_ferr_scale - Scale forecast error with H'R^{-1}H
!   put_ferr_scale   - Write out forecast error vector
!
! Variable Definitions:
!   ferr - forecast error vector

! ------------------------------------------------------------------------------
use kinds, only: r_kind,i_kind
use constants, only: zero
use gsi_4dvar, only: nobs_bins, idmodel, lsqrtb
use mpimod, only: mype
use state_vectors
use bias_predictors
use obsmod, only: l_do_adjoint
#ifdef GEOS_PERT
use geos_pertmod, only: model_init, model_clean, pgcm2gsi, gsi2pgcm
#endif /* GEOS_PERT */
! ------------------------------------------------------------------------------
implicit none
save
private
public lferrscale, apply_hrm1h

logical :: lferrscale = .false.

logical :: lthis_adj
logical :: lthis_sqrt

character(len=*), parameter :: fnerri = 'ferr.eta.hdf'
character(len=*), parameter :: fnerro = 'ferr_rm1.eta'

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
subroutine init_ferr_scale
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_ferr_scale
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-07  lueken - added subprogram doc block
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

integer(i_kind) :: ierr

lthis_adj=l_do_adjoint
l_do_adjoint=.true.
lthis_sqrt=lsqrtb
lsqrtb=.false.
#ifdef GEOS_PERT
      call model_init(ierr,skiptraj=idmodel)
#endif /* GEOS_PERT */

end subroutine init_ferr_scale
subroutine clean_ferr_scale
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    clean_ferr_scale
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-07  lueken - added subprogram doc block
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

#ifdef GEOS_PERT
      call model_clean()
#endif /* GEOS_PERT */
l_do_adjoint=lthis_adj
lsqrtb=lthis_sqrt

end subroutine clean_ferr_scale
! ------------------------------------------------------------------------------
subroutine apply_hrm1h (nprt)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    apply_hrm1h
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-07  lueken - added subprogram doc block
!
!   input argument list:
!    nprt
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

integer(i_kind),intent(in) :: nprt

type(state_vector) :: ferrin
type(state_vector) :: ferrout
integer(i_kind) :: nymd, nhms

if(.not.lferrscale) return

call init_ferr_scale
call allocate_state(ferrin)
call allocate_state(ferrout)
ferrin =zero
ferrout=zero

call get_ferr_scale (ferrin,nymd,nhms)
call hrm1h_ferr_scale(ferrin,ferrout,nprt,'apply_hrm1h')
call put_ferr_scale (ferrout,nymd,nhms)

call deallocate_state(ferrout)
call deallocate_state(ferrin)
call clean_ferr_scale

end subroutine apply_hrm1h
! ------------------------------------------------------------------------------
subroutine get_ferr_scale (ferrin,nymd,nhms)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_ferr_scale
!   prgmmr:      todling
!
! abstract: Read forecast error
!
! program history log:
!   2008-11-16  todling - initial code
!   2009-08-07  lueken  - updated documentation
!
!   input argument list:
!    ferrin
!    nymd,nhms
!
!   output argument list:
!    ferrin
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none
type(state_vector),intent(inout) :: ferrin ! not yet implemented
integer(i_kind),intent(in) :: nymd,nhms

real(r_kind) :: zjx
integer(i_kind) :: ierr

if (mype==0) then
  write(6,*)'get_ferr_scale: read forecast error vector'
endif

ferrin = zero ! not yet implemented

! Read in forecast error
if (lferrscale) then
#ifdef GEOS_PERT
      call pgcm2gsi(ferrin,'tlm',ierr,nymd_in=nymd,nhms_in=nhms,filename=fnerri)
#endif /* GEOS_PERT */
  zjx=dot_product(ferrin,ferrin)
  if (mype==0) write(6,888)'get_ferr_scale: Norm ferrin=',sqrt(zjx)
endif
888 format(A,3(1X,ES24.18))

return
end subroutine get_ferr_scale
! ------------------------------------------------------------------------------
subroutine put_ferr_scale (ferrout,nymd,nhms)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_ferr_scale
!   prgmmr:      todling
!
! abstract: Write out scaled forecast error
!
! program history log:
!   2008-11-16  todling - initial code
!   2009-08-07  lueken  - updated documentation
!
!   input argument list:
!    ferrout
!    nymd,nhms
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none
type(state_vector),intent(in) :: ferrout
integer(i_kind),intent(in) :: nymd,nhms

real(r_kind) :: zjx
integer(i_kind) :: ierr

if (mype==0) then
  write(6,*)'put_ferr_scale: store scaled forecast error vector'
endif

! Write out scaled forecast error to file
if (lferrscale) then
  zjx=dot_product(ferrout,ferrout)
  if (mype==0) write(6,888)'put_ferr_scale: Norm ferrout=',sqrt(zjx)
#ifdef GEOS_PERT
      call gsi2pgcm(nymd,nhms,ferrout,'adm',ierr,filename=fnerro)
#endif /* GEOS_PERT */
endif
888 format(A,3(1X,ES24.18))

return
end subroutine put_ferr_scale
! ------------------------------------------------------------------------------
subroutine hrm1h_ferr_scale(xin,xout,nprt,calledby)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    hrm1h_ferr_scale
!   pgrmmr:      todling
!
! abstract: Apply H'R^{-1}H to xhat.
!
! program history log:
!   2007-11-17 todling - stripped off version of Tremolet's evaljgrad
!   2008-12-06 todling - intjo is now a module
!   2009-01-18 todling - quad precision passed to evaljo
!   2009-08-07 lueken  - updated documentation
!
!   input argument list:
!    xin  - state vector to be scaled
!    nprt - print level
!
!   output argument list:
!    xout - scaled state vector
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_quad
use gsi_4dvar, only: nsubwin, l4dvar
use constants, only: zero_quad
use mpimod, only: mype
use obsmod, only: yobs
use intjomod, only: intjo

implicit none

! Declare passed variables
type(state_vector), intent(in)    :: xin
type(state_vector), intent(inout) :: xout
integer(i_kind), intent(in) :: nprt
character(len=*), intent(in) :: calledby

! Declare local variables  
character(len=*), parameter :: myname='hrm1h_ferr_scale'
type(state_vector) :: sval(nobs_bins), rval(nobs_bins)
type(state_vector) :: mval(nsubwin)
type(predictors) :: sbias, rbias
real(r_quad) :: zjb,zjo,zjc,zjl
integer(i_kind) :: ii,iobs,ibin
logical :: llprt,llouter
character(len=255) :: seqcalls

!**********************************************************************

llprt=(mype==0.and.nprt>=2)
llouter=.false.
seqcalls = trim(calledby)//'::'//trim(myname)

! Allocate local variables
do ii=1,nobs_bins
  call allocate_state(sval(ii))
  call allocate_state(rval(ii))
end do
do ii=1,nsubwin
  call allocate_state(mval(ii))
end do
call allocate_preds(sbias)
call allocate_preds(rbias)

! Contribution from background term
xout = xin
zjb=dot_product(xout,xout)

! Initialize state vector for model integration
do ii=1,nsubwin
   mval(ii)=zero
enddo
call self_add(mval(1),xout)
if (nprt>=2) then
    call prt_state_norms(mval(1),'mval(ii=1)')
endif

! Run TL model to fill sval
if (l4dvar) then
  call model_tl(mval,sval,llprt)
else
  do ii=1,nobs_bins
    sval(ii)=mval(1)
  enddo
end if

if (nprt>=2) then
  do ii=1,nobs_bins
    call prt_state_norms(sval(ii),'sval')
  enddo
endif

! Zero gradient
do ii=1,nobs_bins
  rval(ii)=zero
end do
rbias=zero
do ii=1,nsubwin
  mval(ii)=zero
end do

! Compare obs to solution and transpose back to grid (H^T R^{-1} H)
do ibin=1,nobs_bins
  call intjo(yobs(ibin),rval(ibin),rbias,sval(ibin),sbias,ibin)
end do

! Evaluate Jo
call evaljo(zjo,iobs,nprt,llouter)

if (l_do_adjoint) then
! Moisture constraint
  zjl=zero_quad

  zjc=zero_quad

  if (nprt>=2) then
    do ii=1,nobs_bins
      call prt_state_norms(rval(ii),'rval')
    enddo
  endif

! Run adjoint model
  if (l4dvar) then
    call model_ad(mval,rval,llprt)
  else
    mval(1)=rval(1)
    do ii=2,nobs_bins
      call self_add(mval(1),rval(ii))
    enddo
  end if

  if (nprt>=2) then
    do ii=1,nsubwin
      call prt_state_norms(mval(ii),'mval')
    enddo
  endif

! Return result
  xout=zero
  call self_add(xout,mval(1))

! Print diagnostics
  if (nprt>=2) call prt_state_norms(xout,'xout')
  if (nprt>=1.and.mype==0) write(6,999)trim(seqcalls),': grepcost Jb,Jo,Jc,Jl=',&
                              zjb,zjo,zjc,zjl
endif

! Release memory
call deallocate_preds(rbias)
call deallocate_preds(sbias)
do ii=1,nsubwin
  call deallocate_state(mval(ii))
end do
do ii=1,nobs_bins
  call deallocate_state(rval(ii))
  call deallocate_state(sval(ii))
end do

999 format(2A,5(1X,ES24.18))

return
end subroutine hrm1h_ferr_scale
! ------------------------------------------------------------------------------
end module obs_ferrscale
