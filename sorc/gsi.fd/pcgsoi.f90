module pcgsoimod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pcgsoimod    module for pcgsoi and its tangent linear pcgsoi_tl
!   
! abstract: module for pcgsoi and its tangent linear pcgsoi_tl
!
! program history log:
!   2005-05-17  Yanqiu zhu - wrap pcgsoi and its tangent linear pcgsoi_tl into one module
!   2005-11-21  Derber - remove interface
!

PRIVATE
PUBLIC pcgsoi
PUBLIC pcgsoi_tl

contains

subroutine pcgsoi(mype)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pcgsoi      solve inner loop of analysis equation
!   prgmmr: parrish          org: np22                date: 1991-04-02
!
! abstract: solve inner loop of analysis equation. at end update outer
!           loop variables
!
! program history log:
!   1991-04-02  parrish, d., derber,j.
!   1998-01-16  derber,j.
!   1998-07-10  yang w., mpp version
!   1999-06-28  yang w., second structure mpp version
!   1999-07-30  wu, update handling of ozone data
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   1999-12-07  wu               grid version
!   2003-12-18  derber, j. modify minimization procedures
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-10-06  kleist - separate control vector for u,v
!   2004-10-15  parrish - add nonlinear qc option
!   2005-03-09  parrish - add optional call to regional anisotropic
!                         background error
!   2005-03-28  treadon - combine hopers.f90 and update_ggrid.f90 in update_guess.f90
!   2005-04-11  treadon - add logical flag for nonlinear qc, remove calls
!                         to intall_qc and stpcalc_qc
!   2005-05-02  treadon - add call getuv for restart option
!   2005-05-17  yanqiu zhu - add 'use intallmod', 'use stpcalcmod' and 'use dprodxmod' 
!   2005-05-27  derber  - changes to minimization and output
!   2005-06-03  parrish - add horizontal derivatives
!   2005-09-29  kleist,parrish - include _t (time derivatives) array
!   2006-04-06  treadon - move bias cor. and tskin update into update_guess
!   2006-04-16  derber - change call to stpcalc - move stepsize print to stpcalc
!   2006-04-21  kleist - add calls to update Jc terms
!   2006-05-26  derber - modify to improve convergence testing
!   2006-07-28  derber - remove calls to makeobs
!   2006-08-04  parrish - add changes for strong constraint option
!                         
!
! input argument list:
!
!     mype     - mpi task id
!
!
! output argument list:      
!      none
!
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use radinfo, only: npred
  use pcpinfo, only: npredp
  use qcmod, only: nlnqc_iter
  use obsmod, only: destroyobs
  use jfunc, only: iter,jiter,jiterstart,niter,iout_iter,&
       nclen,nclen1,penorig,gnormorig,xhatsave,yhatsave,&
       xhatsave_r,yhatsave_r,nsst,iguess,nst,np,nuvlen,ntendlen,&
       niter_no_qc,switch_on_derivatives,nu,nv,nst,nvp,&
       nt,nq,noz,nsst,ncw,nut,nvt,ntt,nprst,ndivt,nagvt
  use gridmod, only: lat2,lon2,regional,twodvar_regional,latlon1n,latlon11
  use constants, only: zero,izero,one,five
  use anberror, only: anisotropic
  use mpimod, only: levs_id
  use jcmod, only: jcterm,jcdivt,update_jcterms,update_jcterms_divt
  use intallmod
  use stpcalcmod
  use dprodxmod
  use mod_inmi,only: nstrong_end

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local parameters
  real(r_kind),parameter:: start_step=1.e-4_r_kind
  real(r_kind),parameter:: small_step=1.e-5_r_kind


! Declare local variables  
  logical iout_6,restart,end_iter
  character(5) step(2)
  integer(i_kind) i,j,k,istep,nnn
! real(r_double) dprodx
  real(r_kind) stp,b,converge
  real(r_kind) gnormx,penx,penalty,pennorm
  real(r_kind),dimension(3):: gnorm
  real(r_kind),dimension(nclen):: gradx,grady,dirx,diry,xhat,ydiff
  real(r_kind),dimension(nuvlen):: xhatuv,dirxuv
  real(r_kind),dimension(nclen):: xhat_x,xhat_y,dirx_x,dirx_y
!    note that xhatt,dirxt,xhatp,dirxp are added to carry corrected grid fields
!      of t and p from implicit normal mode initialization (strong constraint option)
!     inmi generates a linear correction to t,u,v,p.  already have xhatuv which can
!      be used for the corrected wind, but nothing for t,p.  xhatt, etc are used exactly
!       like xhatuv,dirxuv.
  real(r_kind),dimension(latlon1n):: xhatt,dirxt
  real(r_kind),dimension(latlon11):: xhatp,dirxp
  real(r_kind),dimension(ntendlen):: xhat_t,dirx_t

! Step size diagnostic strings
  data step /'good', 'SMALL'/

!**********************************************************************

! Set constants.  Initialize variables.
  restart=.false.
  if (jiter==0 .and. (iguess==1 .or. iguess==2)) restart=.true.
  pennorm=10.e50_r_kind
  iout_6=.true.
  stp=start_step
  if (iout_iter==6) iout_6=.false.
  end_iter=.false.

! Convergence criterion needs to be relaxed a bit for anisotropic mode,
! because the anisotropic recursive filter, for reasons of computational
! efficiency, uses r_single (4 byte) arithmetic.  this generally triggers
! the warning about penalty increasing, but this doesn't happen until
! the gradient has been reduced by more than 9 orders of magnitude.
  converge=1.e-10_r_kind
  if(anisotropic) converge=1.e-9_r_kind
  
!   Begin calculation of gradient
!   First the contribution of observations on grid
!
  do i=1,nclen
     gradx(i)=zero
     grady(i)=zero
     dirx(i)=zero
     diry(i)=zero
     ydiff(i)=zero
     xhat(i)=zero
  end do
  if(switch_on_derivatives) then
    do i=1,nclen
       xhat_x(i)=zero
       xhat_y(i)=zero
       dirx_x(i)=zero
       dirx_y(i)=zero
    end do
    do i=1,ntendlen
       xhat_t(i)=zero
       dirx_t(i)=zero
    end do
  end if

  do i=1,nuvlen
    xhatuv(i)=zero
    dirxuv(i)=zero
  end do
  do i=1,latlon1n
    dirxt(i)=zero
    xhatt(i)=zero
  end do
  do i=1,latlon11
    dirxp(i)=zero
    xhatp(i)=zero
  end do

! Perform inner iteration
  inner_iteration: do iter=izero,niter(jiter)

     nlnqc_iter = iter >= niter_no_qc(jiter)


!    Compare obs to solution and transpose back to grid
     call intall(gradx,xhat,xhatt,xhatp,xhatuv,xhat_x,xhat_y,xhat_t,mype)


!    Add contribution from background term
     do i=1,nclen
        gradx(i)=gradx(i)+yhatsave(i)
        ydiff(i)=gradx(i)-ydiff(i)
     end do


!    Multiply by background error
     if(anisotropic) then
       if(regional) then
         call anbkerror_reg(gradx,grady,mype)
       else
     !     NOT AVAILABLE YET
     !   call anbkerror(gradx,grady,mype)
          write(6,*)'PCGSOI:  ***ERROR*** global anisotropic option new yet available'
       end if
     else
       call bkerror(gradx,grady)
     end if

!    Calculate new search direction and norm of gradients
!      gnorm(1) = gradx*grady
!      gnorm(2) = ydiff*grady
!      gnorm(3) = gradx*gradx ***note*** this term not currently used

     b=dprodx(nclen1,nclen,gradx,grady,ydiff,dirx,gnorm,iter,mype)
     if (b>five) b=zero

!    Calculate new search direction
     if (.not. restart) then
        do i=1,nclen
          ydiff(i)=gradx(i)
          dirx(i)=-grady(i)+b*dirx(i)
          diry(i)=-gradx(i)+b*diry(i)
        end do
     else
!    If previous solution available, transfer into local arrays.
!    Compute horizontal derivatives.  Always need
        do i=1,nclen
          ydiff(i)=zero
          dirx(i)=xhatsave_r(i)
          diry(i)=yhatsave_r(i)
        end do
        stp=one
        deallocate(xhatsave_r,yhatsave_r)

     endif

!    Calculate stepsize
     call stpcalc(stp,xhat,xhatt,xhatp,xhatuv,xhat_x,xhat_y,xhat_t, &
          dirx,dirxt,dirxp,dirxuv,dirx_x,dirx_y,dirx_t,diry,penalty,mype,end_iter)

!    Diagnostic calculations
     if(iter == 0 .and. jiter == jiterstart) then
         gnormorig=gnorm(1)
         penorig=penalty
     end if
     gnormx=gnorm(1)/gnormorig
     penx=penalty/penorig

     if (mype==izero) then
        istep=1
        if (stp<small_step) istep=2
        if (iout_6) write(6,110) jiter,iter,penalty,gnorm(1),stp,b
        if (iout_6) write(6,120) jiter,iter,penx,gnormx,step(istep)
        write(iout_iter,110) jiter,iter,penalty,gnorm(1),stp,b
        write(iout_iter,120) jiter,iter,penx,gnormx,step(istep)

110     format(' penalty,grad ,a,b= ',i3,i4,1x,4(e24.18,1x),2(g12.6,1x))
120     format(' pnorm,gnorm, step? ',i3,i4,1x,2(e24.18,1x),a5)
     endif

!    Check for convergence or failure of algorithm
     if(gnormx < converge .or. penalty < converge  .or.  &
        penx >= pennorm .or. end_iter)then

        if(mype == izero)then
          if(iout_6) write(6,101)
          write(iout_iter,101)

          if(gnormx < converge) then
               if(iout_6)write(6,130)gnormx,converge
               write(iout_iter,130) gnormx,converge
          end if
          if(penalty < converge) then
               if(iout_6)write(6,131)penalty,converge
               write(iout_iter,131) penalty,converge
          end if
          if(penx >= pennorm) then
             if(iout_6)write(6,100)jiter,iter,penx,pennorm
             write(iout_iter,100)jiter,iter,penx,pennorm
          end if
          if(end_iter)then
               if(stp > zero)then
                 if(iout_6)write(6,140)
                 write(iout_iter,140)
               else
                 if(iout_6)write(6,141)
                 write(iout_iter,141)
               end if
          end if
        end if
101     format(' PCGSOI: WARNING **** Stopping inner iteration ***')
100     format(' Penalty increase or constant ',I3,1x,i4,1x,2(e24.18,1x))
130     format(' gnorm ', e24.18,' less than ',e24.18)
131     format(' penalty ', e24.18,' less than ',e24.18)
140     format(' Stepsize calculation terminates inner iteration - probable convergence')
141     format(' Stepsize calculation terminates inner iteration - probable error')
        exit inner_iteration
     end if
     pennorm=penx


!    Update solution
     do i=1,nclen
        xhat(i)=xhat(i)+stp*dirx(i)
        xhatsave(i)=xhatsave(i)+stp*dirx(i)
        yhatsave(i)=yhatsave(i)+stp*diry(i)
     end do
     if(switch_on_derivatives) then
       do i=1,nclen
          xhat_x(i)=xhat_x(i)+stp*dirx_x(i)
          xhat_y(i)=xhat_y(i)+stp*dirx_y(i)
       end do
       do i=1,ntendlen
          xhat_t(i)=xhat_t(i)+stp*dirx_t(i)
       end do
     end if

!    Update separate u,v solution to be used in the int routines 
!    in next inner loop
     do i=1,nuvlen
       xhatuv(i)=xhatuv(i)+stp*dirxuv(i)
     end do
!    Similiarly, update t and p to be used in int routines in next inner loop
!     (redundant for no strong constraint correction, but necessary for
!      strong constraint correction enabled)
     do i=1,latlon1n
       xhatt(i)=xhatt(i)+stp*dirxt(i)
     end do
     do i=1,latlon11
       xhatp(i)=xhatp(i)+stp*dirxp(i)
     end do

  end do inner_iteration

! 
! Deallocate obs file
  call destroyobs

! Update contributions of incremental values from current outer loop
  if (jcterm) then
    if (jcdivt) then
      call update_jcterms_divt(xhat_t(ndivt),xhat_t(nagvt),mype)
    else
      call update_jcterms(xhat_t(nut),xhat_t(nvt),xhat_t(ntt),xhat_t(nprst),mype)
    end if
  end if 

  if(nstrong_end.gt.0) call strong_end(xhat,xhatt,xhatp,xhatuv,mype)

! Update guess (model background, bias correction) fields
!  first, replace xhat(nt...) with xhatt, and xhat(np...) with xhatp
!   (incase strong constraint correction enabled)
  do i=1,latlon1n
    xhat(nt+i-1)=xhatt(i)
  end do
  do i=1,latlon11
    xhat(np+i-1)=xhatp(i)
  end do
  call update_guess(xhat,xhatuv,mype)

! End of routine
  return
end subroutine pcgsoi


subroutine pcgsoi_tl(mype, tlm_check)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pcgsoi_tl     the tangent linear of the operator that solves 
!                             inner loop of analysis equation
!   prgmmr: yanqiu zhu          org: GMAO                date: 2005-05-17
!
! abstract: the tangent linear of the operator that solves inner loop of 
!           analysis equation. at end update outer loop variables
!
! program history log:
!   2005-05-02  treadon - add call getuv for restart option
!   2005-05-17  yanqiu zhu - tangent linear of pcgsoi
!   2005-05-27  derber  - changes to minimization and output
!   2005-06-03  parrish - add horizontal derivatives
!   2006-04-06  treadon - move bias cor. and tskin update into update_guess
!   2006-05-26  derber - modify to improve convergence testing
!                         
!
! input argument list:
!
!     mype     - mpi task id
!
!
! output argument list:      
!      none
!
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use radinfo, only: npred
  use pcpinfo, only: npredp
  use qcmod, only: nlnqc_iter
  use obsmod, only: destroyobs,obs_sen
  use obsmod_tl, only: destroyobs_tl
  use jfunc, only: iter,jiter,jiterstart,niter,iout_iter,&
       nclen,nclen1,penorig,gnormorig,xhatsave,yhatsave,&
       xhatsave_r,yhatsave_r,nsst,iguess,nst,np,nuvlen,&
       niter_no_qc,switch_on_derivatives,nu,nv,nst,nvp,&
       nt,nq,noz,nsst,ncw
  use jfunc_tl, only: xhatsave_tl,yhatsave_tl,xhatsave_r_tl, &
       yhatsave_r_tl,write_solution
  use gridmod, only: regional,twodvar_regional
  use constants, only: zero,izero,one
  use anberror, only: anisotropic
  use mpimod, only: levs_id
  use intallmod
  use stpcalcmod
  use dprodxmod

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local parameters
  real(r_kind),parameter:: start_step=1.e-4_r_kind
  real(r_kind),parameter:: small_step=1.e-5_r_kind

! Declare local variables  
  logical iout_6,restart,end_iter
  character(5) step(2)
  integer(i_kind) i,j,k,istep,nnn
  real(r_double) dprodxtmp
  real(r_double) dprodxtmp_tl
  real(r_kind) stp,b,converge
  real(r_kind) stp_tl,b_tl
  real(r_kind) gnormx,penx,penalty,pennorm
  real(r_kind) gnormx_tl,penx_tl,penalty_tl,pennorm_tl
  real(r_kind),dimension(3):: gnorm
  real(r_kind),dimension(3):: gnorm_tl
  real(r_kind),dimension(nclen):: gradx,grady,dirx,diry,xhat,ydiff
  real(r_kind),dimension(nclen):: gradx_tl,grady_tl,dirx_tl,diry_tl,xhat_tl,ydiff_tl
  real(r_kind),dimension(nuvlen):: xhatuv,dirxuv
  real(r_kind),dimension(nuvlen):: xhatuv_tl,dirxuv_tl
  real(r_kind),dimension(nclen):: xhat_x,xhat_y,dirx_x,dirx_y
  real(r_kind),dimension(nclen):: xhat_x_tl,xhat_y_tl,dirx_x_tl,dirx_y_tl

! output file name
  character*255 filename, filename_tl

! logical set for tlm check
  logical, intent(in), optional  :: tlm_check

! Step size diagnostic strings
  data step /'good', 'SMALL'/

!**********************************************************************

! Set constants.  Initialize variables.
  restart=.false.
  if (jiter==0 .and. (iguess==1 .or. iguess==2)) restart=.true.
  pennorm=10.e50_r_kind
  iout_6=.true.
  stp=start_step
  stp_tl=zero
  if (iout_iter==6) iout_6=.false.

! Convergence criterion needs to be relaxed a bit for anisotropic mode,
! because the anisotropic recursive filter, for reasons of computational
! efficiency, uses r_single (4 byte) arithmetic.  this generally triggers
! the warning about penalty increasing, but this doesn't happen until
! the gradient has been reduced by more than 9 orders of magnitude.
  converge=1.e-10_r_kind
  if(anisotropic) converge=1.e-9_r_kind

  
!   Begin calculation of gradient
!   First the contribution of observations on grid
!
  do i=1,nclen
     gradx(i)=zero
     grady(i)=zero
     dirx(i)=zero
     diry(i)=zero
     ydiff(i)=zero
     xhat(i)=zero
     gradx_tl(i)=zero
     grady_tl(i)=zero
     dirx_tl(i)=zero
     diry_tl(i)=zero
     ydiff_tl(i)=zero
     xhat_tl(i)=zero
  end do
  if(switch_on_derivatives) then
    do i=1,nclen
       xhat_x(i)=zero
       xhat_y(i)=zero
       dirx_x(i)=zero
       dirx_y(i)=zero
       xhat_x_tl(i)=zero
       xhat_y_tl(i)=zero
       dirx_x_tl(i)=zero
       dirx_y_tl(i)=zero
    end do
  end if


  do i=1,nuvlen
    xhatuv(i)=zero
    dirxuv(i)=zero
    xhatuv_tl(i)=zero
    dirxuv_tl(i)=zero
  end do


! Perform inner iteration
  inner_iteration: do iter=izero,niter(jiter)

     nlnqc_iter = iter >= niter_no_qc(jiter)


!    Compare obs to solution and transpose back to grid
     call intall_tl(gradx,xhat,xhatuv,xhat_x,xhat_y,&
          gradx_tl,xhat_tl,xhatuv_tl,xhat_x_tl,xhat_y_tl,mype)


!    Add contribution from background term
     do i=1,nclen
        gradx(i)=gradx(i)+yhatsave(i)
        ydiff(i)=gradx(i)-ydiff(i)
        gradx_tl(i)=gradx_tl(i)+yhatsave_tl(i)
        grady_tl(i)=grady_tl(i)+xhatsave_tl(i)
        ydiff_tl(i)=gradx_tl(i)-ydiff_tl(i)
     end do


!    Multiply by background error
     if(anisotropic) then
       if(regional) then
         call anbkerror_reg(gradx,grady,mype)
         call anbkerror_reg(gradx_tl,grady_tl,mype)
       else
     !     NOT AVAILABLE YET
     !   call anbkerror(gradx,grady,mype)
          write(6,*)'PCGSOI:  ***ERROR*** global anisotropic option new yet available'
       end if
     else
       call bkerror(gradx,grady)
       call bkerror(gradx_tl,grady_tl)
     end if


!    Calculate new search direction and norm of gradients
!      gnorm(1) = gradx*grady
!      gnorm(2) = ydiff*grady
!      gnorm(3) = gradx*gradx ***note*** this term not currently used

!    b=dprodx(nclen1,nclen,gradx,grady,ydiff,dirx,gnorm,iter,mype)
     call dprodx_tl(nclen1,nclen,gradx,grady,ydiff,dirx,gnorm,iter,mype,dprodxtmp, &
          gradx_tl,grady_tl,ydiff_tl,dirx_tl,gnorm_tl,dprodxtmp_tl)
     b = dprodxtmp
     b_tl = dprodxtmp_tl


!    Calculate new search direction
     if (.not. restart) then
        do i=1,nclen
          ydiff_tl(i)=gradx_tl(i)
          dirx_tl(i)=-grady_tl(i)+b_tl*dirx(i)+b*dirx_tl(i)
          diry_tl(i)=-gradx_tl(i)+b_tl*diry(i)+b*diry_tl(i)
          ydiff(i)=gradx(i)
          dirx(i)=-grady(i)+b*dirx(i)
          diry(i)=-gradx(i)+b*diry(i)
        end do
     else
        do i=1,nclen
          ydiff_tl(i)=zero
          dirx_tl(i)=xhatsave_r_tl(i)
          diry_tl(i)=yhatsave_r_tl(i)
          ydiff(i)=zero
          dirx(i)=xhatsave_r(i)
          diry(i)=yhatsave_r(i)
        end do

        stp=one
        if ( .not. present(tlm_check) ) then
           deallocate(xhatsave_r,yhatsave_r)
           deallocate(xhatsave_r_tl,yhatsave_r_tl)
        end if
     endif


!    Calculate stepsize
     call stpcalc_tl(stp,xhat,xhatuv,xhat_x,xhat_y,&
          dirx,dirxuv,dirx_x,dirx_y,diry,penalty,mype, &
          stp_tl,xhat_tl,xhatuv_tl,xhat_x_tl,xhat_y_tl,&
          dirx_tl,dirxuv_tl,dirx_x_tl,dirx_y_tl,diry_tl,penalty_tl,end_iter)

!    Diagnostic calculations
     if(iter == 0 .and. jiter == jiterstart) then
         gnormorig=gnorm(1)
         penorig=penalty
     end if
     gnormx=gnorm(1)/gnormorig
     penx=penalty/penorig

     if (mype==izero) then
        istep=1
        if (stp<small_step) istep=2
        if (iout_6) write(6,110) jiter,iter,penalty,gnorm(1),stp,b
        if (iout_6) write(6,120) jiter,iter,penx,gnormx,step(istep)
        write(iout_iter,110) jiter,iter,penalty,gnorm(1),stp,b
        write(iout_iter,120) jiter,iter,penx,gnormx,step(istep)

110     format(' penalty,grad ,a,b= ',i3,i4,1x,4(e24.18,1x),2(g12.6,1x))
120     format(' pnorm,gnorm, step? ',i3,i4,1x,2(e24.18,1x),a5)
     endif

!    Check for convergence or failure of algorithm
     if(gnormx < converge .or. penalty < converge  .or.  &
        penx >= pennorm .or. end_iter)then

        if(mype == izero)then
          if(iout_6) write(6,101)
          write(iout_iter,101)

          if(gnormx < converge) then
               if(iout_6)write(6,130)gnormx,converge
               write(iout_iter,130) gnormx,converge
          end if
          if(penalty < converge) then
               if(iout_6)write(6,131)penalty,converge
               write(iout_iter,131) penalty,converge
          end if
          if(penx >= pennorm) then
             if(iout_6)write(6,100)jiter,iter,penx,pennorm
             write(iout_iter,100)jiter,iter,penx,pennorm
          end if
          if(end_iter)then
               if(stp > zero)then
                 if(iout_6)write(6,140)
                 write(iout_iter,140)
               else
                 if(iout_6)write(6,141)
                 write(iout_iter,141)
               end if
          end if
        end if
101     format(' PCGSOI: WARNING **** Stopping inner iteration ***')
100     format(' Penalty increase or constant ',I3,1x,i4,1x,2(e24.18,1x))
130     format(' gnorm ', e24.18,' less than ',e24.18)
131     format(' penalty ', e24.18,' less than ',e24.18)
140     format(' Stepsize calculation terminates inner iteration - probable convergence')
141     format(' Stepsize calculation terminates inner iteration - probable error')
        exit inner_iteration
     end if
     pennorm=penx



!    Update solution
     do i=1,nclen
        xhat(i)=xhat(i)+stp*dirx(i)
        xhatsave(i)=xhatsave(i)+stp*dirx(i)
        yhatsave(i)=yhatsave(i)+stp*diry(i)
        xhat_tl(i)=xhat_tl(i)+stp_tl*dirx(i)+stp*dirx_tl(i)
        xhatsave_tl(i)=xhatsave_tl(i)+stp_tl*dirx(i)+stp*dirx_tl(i)
        yhatsave_tl(i)=yhatsave_tl(i)+stp_tl*diry(i)+stp*diry_tl(i)
     end do
     if(switch_on_derivatives) then
       do i=1,nclen
          xhat_x(i)=xhat_x(i)+stp*dirx_x(i)
          xhat_y(i)=xhat_y(i)+stp*dirx_y(i)
          xhat_x_tl(i)=xhat_x_tl(i)+stp_tl*dirx_x(i)+stp*dirx_x_tl(i)
          xhat_y_tl(i)=xhat_y_tl(i)+stp_tl*dirx_y(i)+stp*dirx_y_tl(i)
       end do
     end if



!    Update separate u,v solution to be used in the int routines 
!    in next inner loop
     do i=1,nuvlen
       xhatuv(i)=xhatuv(i)+stp*dirxuv(i)
       xhatuv_tl(i)=xhatuv_tl(i)+stp_tl*dirxuv(i)+stp*dirxuv_tl(i)
     end do

     
  end do inner_iteration

! End of inner iteration


! Output xhat for diagnostic study

  filename = 'xhat_file_0'
  call write_solution(xhat,xhatuv,filename,mype)
  filename_tl = 'xhat_file_tl'
  call write_solution(xhat_tl,xhatuv_tl,filename_tl,mype)


! Deallocate obs file
  if ( .not. present(tlm_check) ) then
     call destroyobs
  end if
  call destroyobs_tl

 
! ***NOTE*** JUST FOR NOW: need to fix it after interfaces with GEOS5 are ready
 if (.not. obs_sen) then

! Update guess (model background, bias correction) fields
  call update_guess(xhat,xhatuv,mype)

 endif
! ***NOTE*** JUST FOR NOW: need to fix it after interfaces are ready

! End of routine
  return
end subroutine pcgsoi_tl

end module pcgsoimod
