subroutine sqrtmin()

!$$$  subprogram documentation block
!
! abstract: Minimize cost function using sqrt(B) preconditioner
!
! program history log:
!   2007-05-01  tremolet - initial code
!   2007-11-23  todling  - add timers
!   2008-01-04  tremolet - forecast sensitivity option
!   2009-01-18  todling  - calc dot-prod in quad precision
!
!$$$

use kinds, only: r_kind,i_kind,r_quad
use gsi_4dvar, only: l4dvar, lsqrtb, lcongrad, lbfgsmin, ltlint, &
                     ladtest, lgrtest, lanczosave, nwrvecs
use jfunc, only: jiter,miter,niter,xhatsave,jiterstart
use qcmod, only: nlnqc_iter
use constants, only: zero, one
use mpimod, only: mype
use obs_sensitivity, only: lobsensadj, lobsensmin, lobsensfc, lobsensincr, &
                           iobsconv, fcsens, llancdone, dot_prod_obs, &
                           lsensrecompute,lobsensjb
use obsmod, only: lsaveobsens,l_do_adjoint
use qnewton, only: lbfgs
use qnewton3, only: m1qn3
use lanczos, only: congrad,setup_congrad,save_precond,congrad_ad,read_lanczos
use adjtest, only: adtest
use control_vectors
use state_vectors
use obs_ferrscale, only: lferrscale, apply_hrm1h
use timermod, only: timer_ini, timer_fnl

implicit none

character(len=*), parameter :: myname='sqrtmin'
! Declare local variables  
type(control_vector) :: xhat, gradx, gradf, xsens
real(r_kind) :: costf,eps,rdx,zeps,zy
real(r_quad) :: zf0,zg0,zff,zgf,zge,zgg         
integer(i_kind) :: nprt,itermax,ii,itest,jtermax
logical :: lsavinc, lsavev
character(len=12) :: clfile

!**********************************************************************

if (.not.lsqrtb) then
  write(6,*)'sqrtmin: lsqrtb false'
  call stop2(307)
end if

! Initialize timer
  call timer_ini('sqrtmin')

! Initialize
lsavinc=.false.
if (ltlint) nlnqc_iter=.false.

! Allocate control vectors
call allocate_cv(xhat)
call allocate_cv(gradx)

! Run tests if required
rdx=1.0e-10_r_kind
itest=8
if (lgrtest) call grtest(xhat,rdx,itest)
if (ladtest) call adtest()
zgg=dot_product(xhatsave,xhatsave,r_quad)
if (mype==0) write(6,888)trim(myname),': Norm xhatsave=',sqrt(zgg)
call prt_guess('guess')
if (lobsensfc.and.lobsensmin) lsaveobsens=.true.

! Get initial cost function and gradient
nprt=2
xhat=zero
call evaljgrad(xhat,zf0,gradx,lsavinc,nprt,myname)

zg0=dot_product(gradx,gradx,r_quad)
zg0=sqrt(zg0)
if (mype==0) then
  write(6,888)trim(myname),': Initial cost function =',zf0
  write(6,888)trim(myname),': Initial gradient norm =',zg0
endif

! Minimization
nprt=1
itermax=niter(jiter)
eps=1.0e-8_r_kind
costf=zf0

zeps=eps
jtermax=itermax

if (lbfgsmin) then
! call lbfgs  (xhat,costf,gradx,eps,itermax,nprt,nwrvecs)
  call m1qn3  (xhat,costf,gradx,eps,itermax,nprt,nwrvecs)

elseif (lcongrad) then
! Setup CONGRAD
  if (.not.ltlint) then
    write(6,*)'sqrtmin: congrad requires ltlint'
    call stop2(308)
  end if
  call setup_congrad(mype,nprt,jiter,jiterstart,itermax,nwrvecs, &
                     l4dvar,lanczosave)
  lsavev=(.not.lobsensfc).and.(jiter<miter)

  if (lobsensfc.and.lobsensadj) then
!   Get Lanczos vectors
    if ( llancdone ) then        ! Lanczos vecs already computed, read them in
      call read_lanczos(itermax)
    else                         ! Do forward min to get Lanczos vectors
      call congrad(xhat,costf,gradx,eps,itermax,iobsconv,lsavev)
    endif

!yt    if (lsensrecompute.and.lobsensjb.and.jiter==miter) then
!yt      do ii=1,fcsens%lencv
!yt        fcsens%values(ii) = xhatsave%values(ii) + xhat%values(ii)
!yt      end do
!yt    endif

!   Compute sensitivity
    zgg=dot_product(fcsens,fcsens,r_quad)
    if (mype==0) write(6,888)trim(myname),': Norm fcsens=',sqrt(zgg)
    xhat=fcsens
    call congrad_ad(xhat,itermax)
  else
!   Compute increment
    call congrad(xhat,costf,gradx,eps,itermax,iobsconv,lsavev)
  endif

! Finish CONGRAD
  call save_precond(lsavev)

else  ! plain conjugate gradient
  if (.not.ltlint) then
    write(6,*)'sqrtmin: pcgsqrt requires ltlint'
    call stop2(309)
  end if
  call pcgsqrt(xhat,costf,gradx,eps,itermax,nprt)
endif

if (mype==0) write(6,*)trim(myname),': Minimization final diagnostics'

if (lferrscale .and. jiter==miter) call apply_hrm1h(2)

! Get final cost function and gradient
! and save increment (or update guess)
if (lobsensfc) then
  lsaveobsens=.true.
  l_do_adjoint=.false.
  nprt=1
else
  lsavinc=.true.
  nprt=2
endif
call allocate_cv(gradf)
gradf=zero
call evaljgrad(xhat,zff,gradf,lsavinc,nprt,myname)

if (lobsensfc) then
! Compute <dJ/dy,d>
  if (lobsensincr) then
    call test_obsens(fcsens,xhat)
  else
    zy=dot_prod_obs()
    if (mype==0) write(6,'(A,ES25.18)')'Obs impact <dF/dy,d>= ',zy
  endif
! Save gradient for next (backwards) outer loop
  if (jiter>1) then
    do ii=1,xhat%lencv
      fcsens%values(ii) = fcsens%values(ii) - xhat%values(ii)
    enddo
    clfile='fgsens.ZZZ'
    write(clfile(8:10),'(I3.3)') jiter
    call write_cv(fcsens,clfile)
  endif
  lsaveobsens=.false.
  l_do_adjoint=.true.

else
! Update xhatsave
  do ii=1,xhat%lencv
    xhatsave%values(ii) = xhatsave%values(ii) + xhat%values(ii)
  end do

  zgg=dot_product(xhat,xhat,r_quad)
  if (mype==0) write(6,888)trim(myname),': Norm xhat=',sqrt(zgg)
  zgg=dot_product(xhatsave,xhatsave,r_quad)
  if (mype==0) write(6,888)trim(myname),': Norm xhatsave=',sqrt(zgg)

! Print diagnostics
  zgf=dot_product(gradf,gradf,r_quad)
  zgf=sqrt(zgf)
  if (mype==0) then
    write(6,888)trim(myname),': Final cost function =',real(zff,r_kind)
    write(6,888)trim(myname),': Final gradient norm =',real(zgf,r_kind)
    write(6,888)trim(myname),': Final/Initial cost function=',real(zff,r_kind)/real(zf0,r_kind)
    write(6,888)trim(myname),': Final/Initial gradient norm=',real(zgf,r_kind)/real(zg0,r_kind)
  endif

  if (l4dvar) then
    call prt_guess('increment')
  else
    call prt_guess('analysis')
  endif

  zge=dot_product(gradx,gradx,r_quad)
  zge=sqrt(zge)

  do ii=1,xhat%lencv
    gradf%values(ii) = gradf%values(ii) - gradx%values(ii)
  end do
  zgg=dot_product(gradf,gradf,r_quad)
  zgg=sqrt(zgg)
  if (mype==0) then
    write(6,888)trim(myname),': Gradient norm estimated,actual,error:',zge,zgf,zgg
  endif

  if (zgg>0.1) then
    if (mype==0) write(6,*)'*** sqrtmin: error estimated gradient ***'
!   With conjugate gradient, estimated gradient norm will differ from
!   actual gradient norm without re-orthogonalisation so don't abort.
!yt    if (lcongrad.or.lbfgsmin) then
!         write(6,*)'sqrtmin: error estimated gradient',lcongrad,lbfgsmin
!         call stop2(310)
!      end if
  endif
  
  if (ladtest) call adtest(xhat)

endif

! Release memory
call deallocate_cv(xhat)
call deallocate_cv(gradx)
call deallocate_cv(gradf)
call inquire_cv
call inquire_state

! Finalize timer
call timer_fnl('sqrtmin')

888 format(2A,3(1X,ES24.18))

return
end subroutine sqrtmin
