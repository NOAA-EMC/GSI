subroutine pcgsqrt(xhat,costf,gradx,itermax,nprt)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pcgsqrt
!   prgmmr:      tremolet
!
! abstract: solve inner loop of analysis equation using conjugate
!           gradient preconditioned with sqrt(B).
!
! program history log:
!   2007-04-27  tremolet - initial code
!   2009-01-14  todling  - zero-obs fix
!   2009-01-18  todling  - carry summations in quad precision
!
!   input argument lits:
!    xhat,gradx
!    costf
!    itermax,nprt
!
!   output argument list:
!    xhat,gradx
!    costf
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind,r_quad
use jfunc, only: iter,jiter
use constants, only: zero,zero_quad,tiny_r_kind
use mpimod, only: mype
use control_vectors
use timermod,only: timer_ini,timer_fnl

implicit none

! Declare arguments
type(control_vector), intent(inout) :: xhat,gradx
real(r_kind)        , intent(inout) :: costf
integer(i_kind)     , intent(in   ) :: itermax,nprt

! Declare local variables
character(len=*), parameter :: myname='pcgsqrt'
type(control_vector) :: dirx,xtry,grtry,grad0,gradf
real(r_quad) :: beta,alpha,zg0,zgk,zgnew,zfk,dkqk
integer(i_kind) :: ii
logical lsavinc

!**********************************************************************
call timer_ini('pcgsqrt')

! Allocate local variables
call allocate_cv(dirx)
call allocate_cv(xtry)
call allocate_cv(grtry)
call allocate_cv(grad0)
if (nprt>=1) call allocate_cv(gradf)

! Initializations
dirx=zero
beta=zero_quad
lsavinc=.false.

! Save initial cost function and gradient
grad0=gradx
zg0=dot_product(grad0,grad0,r_quad)
zgk=zg0

! Perform inner iteration
inner_iteration: do iter=1,itermax
   if (mype==0) write(6,*)trim(myname),': Minimization iteration',iter

!  Search direction
   do ii=1,dirx%lencv
      dirx%values(ii)=-gradx%values(ii)+beta*dirx%values(ii)
   end do

!  Estimate
   do ii=1,xtry%lencv
      xtry%values(ii)=xhat%values(ii)+dirx%values(ii)
   end do

!  Evaluate cost and gradient
   call evaljgrad(xtry,zfk,grtry,lsavinc,0,myname)

!  Get A q_k
   do ii=1,grtry%lencv
      grtry%values(ii)=grtry%values(ii)-grad0%values(ii)
   end do

!  Calculate stepsize
   dkqk=dot_product(dirx,grtry,r_quad)
   alpha=zero_quad
   if(abs(dkqk)>tiny_r_kind) alpha = zgk/dkqk

!  Update estimates
   do ii=1,xhat%lencv
      xhat%values(ii) =xhat%values(ii) +alpha* dirx%values(ii)
      gradx%values(ii)=gradx%values(ii)+alpha*grtry%values(ii)
   end do

   zgnew=dot_product(gradx,gradx,r_quad)
   beta=zero_quad
   if(abs(zgk)>tiny_r_kind) beta=zgnew/zgk
   zgk=zgnew

!  Evaluate cost for printout
   if (nprt>=1) call evaljgrad(xhat,zfk,gradf,lsavinc,nprt,myname)

   if (mype==0) then
      if (abs(zg0)>tiny_r_kind) then
         write(6,999)trim(myname),': grepgrad grad,reduction,step=',jiter,iter,sqrt(real(zgk,r_kind)),&
              sqrt(real(zgk,r_kind)/real(zg0,r_kind)),real(alpha,r_kind)
      else
         write(6,999)trim(myname),': grepgrad grad,reduction,step=',jiter,iter,sqrt(real(zgk,r_kind)),&
              zero,real(alpha,r_kind)
      endif
   endif

end do inner_iteration
costf=zfk

! Release memory
call deallocate_cv(dirx)
call deallocate_cv(xtry)
call deallocate_cv(grtry)
call deallocate_cv(grad0)
if (nprt>=1) call deallocate_cv(gradf)

999 format(2A,2(1X,I3),3(1X,ES24.18))

call timer_fnl('pcgsqrt')
return
end subroutine pcgsqrt
